# =================================================================================================
# Module: MFA calculation
#
# Date of last modification: 13.07.2020
# =================================================================================================

# =================================================================================================
# 1. Introduction
# =================================================================================================

timer <- proc.time() # first timer to check how long the simulation was running
SIM <- 100000 # set number of simulation steps

# =================================================================================================
# 2. PRODUCTION
#
# Data : - Data was provided by Firmenich (min, mean, max).
#        - Triangular distribution was used to estimate the production volume
#        - Production year of 2019 
#        - Unit: ton
# =================================================================================================

Production_Country_i <- Production_volume[which(Production_volume$Country == Country_i),]

productionvolume <- triangulize_b (modalvalues = Production_Country_i$mid, min = Production_Country_i$low, max = Production_Country_i$high, N = SIM)

feprod_country_i <- productionvolume
hist(feprod_country_i)

# =================================================================================================
# 3. DEFINE TC MATRIX AND INI.INPUT VECTOR
# =================================================================================================

# Define THE TRANSFER COEFFICIENT MATRIX TC (flows from column to rows)
TC <- as.matrix(TC)

# Define uncertainty matrix
TC.uncertainty <- as.matrix(TC.uncertainty)

# Define the initial input vector, i.e. in which compartments there is an input
inp.Distr <- rbind(feprod_country_i, matrix(0,ncol(TC_template)-1,SIM)) 


# Give the vectors names (optional)
dimnames(inp.Distr)[[1]] <- dimnames(TC)[[1]]

# SOME TESTS TO INSURE CONSISTENT DATA
# stop calculation if the matrix is not square with symmetric dimnames
if(!all(colnames(TC) == rownames(TC))){
  warning("The column and row names of TC are not identical. Make sure the input is correct.")
}

# =================================================================================================
# 4. CREATE A TC MATRIX AND INPUT VECTOR FOR EVERY SIMULATION STEP
# =================================================================================================

#For other TC, an uncertainty of 50% was applied
TC.Distr <- triangulize_a(modalvalues = TC, uncertainty = TC.uncertainty, N = SIM)

#Special TC was assigned to WWTP 
TC_WWTP_Sludge <- sample(WWTP_removal_efficiency[,3],SIM,replace = TRUE)

#Assign transfer coefficient to WWTP removal efficiency, where the values were obtained from literature
TC.Distr[16,15,] <- TC_WWTP_Sludge
#Assign transfer coefficient to the percentage of FEs, which were not contained by sludge in WWTP
TC.Distr[22,15,] <- c(1-TC_WWTP_Sludge)

# make sure there are no undefined values
stopifnot(!any(is.na(TC.Distr)), !any(is.na(inp.Distr)))

# normalization step: make sure that all the flows going out of a compartment sum up to 1
TC.Norm <- normalize(Distr = TC.Distr)

# make sure there are no undefined values in the code
stopifnot(!any(is.na(TC.Norm)))

### TRANSFORM MATRIX TYPE T TO A 
TC.Norm <- matrix.transform(Distr = TC.Norm)

# =================================================================================================
# 5. SOLVE EQUATION
# =================================================================================================

# create an empty matrix to contain the mass distributions for every compartment
Mass <- matrix(NA, dim(TC)[1], SIM, dimnames = list(dimnames(TC)[[1]], NULL))
for(k in 1:SIM){
  Mass[,k] <- solve(TC.Norm[,,k],inp.Distr[,k])
}

# notify how long was needed for the whole calculation
message("Time needed for the simulation:")
print(proc.time() - timer) # second timer

# =================================================================================================
# 6. RETRIEVES VALUES
# Unit : ton
# =================================================================================================

# Flows (preparing the data)
# Function of calculating the mode number 
Mode_Y	<- function(x)
{
  dens	<- density(x)
  ind		<- which(dens$y==max(dens$y))
  dens$x[ind]
}

flows <- matrix(NA,ncol(TC_template),5)

# Distribution
distribution <-matrix(NA,ncol(TC_template),ncol(Mass))

for(i in 1:ncol(TC_template))
{
  xx			<- Mass[i,]
  
  flows[i,1]	<- i
  flows[i,2]	<- quantile(xx,0.05)
  flows[i,3]	<- Mode_Y(xx)[1]
  flows[i,4]	<- mean(xx)
  flows[i,5]	<- quantile(xx,0.95)
  
  distribution[i,] <- xx
}

# Extract Q5, Mode, Mean, Q95
measures			<- rbind(flows)
colnames(measures)	<- c("Row","Q5","Mode","Mean","Q95")
measures_table		<- as.table(measures)
measures_table[,1] <- row.names(TC) #Rename the column

# Extract flows
Flow_i <- data.frame()
Flow_i <- measures_table
Flow_i <- cbind(Flow_i, Country_i)
Flow <- rbind(Flow, Flow_i)

# Extract distribution
distribution_table <- as.table(distribution)
distribution_table[,1] <- row.names(TC)

# =================================================================================================
# 7. EXPORT RESULTS
# =================================================================================================

# Set the output file directory
setwd("C:/Users/yca/Desktop/Firmenich/Output")

# Export the mass flow table 
write.table(measures_table, paste(names(measures_table),Country_i, ".csv", sep = ""),
            sep = "\t", quote = FALSE)

# saves a pdf file of the histograms of the distributions to the working directory
pdf(paste("plot", Country_i, ".pdf", sep = ""),
    height = 7.5,
    width = 7.5,
    pointsize = 10)
par(mfrow = c(3,3), mar = c(3,3,3,1), mgp= c(1.5,0.5,0), xpd = F)
color <- c(rep("aquamarine", 3), rep("cornsilk2", 4), rep("darksalmon", 2))
for(co in 1:dim(Mass)[1]){
  hist(Mass[co,], freq = F, xlab = paste(dimnames(Mass)[[1]][co], "(t)"),
       ylab = "Probability density", main = dimnames(Mass)[[1]][co],
       col = color[co])
  box()
}
dev.off()

setwd("C:/Users/yca/Desktop/Firmenich/Model")

