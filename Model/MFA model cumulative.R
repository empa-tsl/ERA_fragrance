# =================================================================================================
# Module: MFA calculation for the year between 2010 and 2019
#
# Date of last modification: 13.04.2021
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

if(Country_i == "USA"){
  #The year between 2015-2010, an uncertainty of 5% increasing with the distance 
  #2015 5%, 2014 10%, 2013 15%, 2012 20%, 2011 25%, 2010 30% 
  
  Production_Country_i <- Production_volume[which(Production_volume$Country == Country_i),] #pick up country
  Production_Country_i_2019_Dist <- triangulize_b (modalvalues = Production_Country_i$mid, min = Production_Country_i$low, max = Production_Country_i$high, N = SIM)
  Production_Country_i_2018_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[9,5], min = Production_Country_i$low*Trend[9,5], max = Production_Country_i$high*Trend[9,5], N = SIM)
  Production_Country_i_2017_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[8,5], min = Production_Country_i$low*Trend[8,5], max = Production_Country_i$high*Trend[8,5], N = SIM)
  Production_Country_i_2016_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[7,5], min = Production_Country_i$low*Trend[7,5], max = Production_Country_i$high*Trend[7,5], N = SIM)
  Production_Country_i_2015_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[6,5], min = (Production_Country_i$low*Trend[6,5] - Production_Country_i$mid*Trend[6,5]*0.05), max = (Production_Country_i$high*Trend[6,5]+ Production_Country_i$mid*Trend[6,5]*0.05), N = SIM)
  Production_Country_i_2014_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[5,5], min = (Production_Country_i$low*Trend[5,5] - Production_Country_i$mid*Trend[5,5]*0.10), max = (Production_Country_i$high*Trend[5,5]+ Production_Country_i$mid*Trend[5,5]*0.10), N = SIM)
  Production_Country_i_2013_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[4,5], min = (Production_Country_i$low*Trend[4,5] - Production_Country_i$mid*Trend[4,5]*0.15), max = (Production_Country_i$high*Trend[4,5]+ Production_Country_i$mid*Trend[4,5]*0.15), N = SIM)
  Production_Country_i_2012_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[3,5], min = (Production_Country_i$low*Trend[3,5] - Production_Country_i$mid*Trend[3,5]*0.20), max = (Production_Country_i$high*Trend[3,5]+ Production_Country_i$mid*Trend[3,5]*0.20), N = SIM)
  Production_Country_i_2011_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[2,5], min = (Production_Country_i$low*Trend[2,5] - Production_Country_i$mid*Trend[2,5]*0.25), max = (Production_Country_i$high*Trend[2,5]+ Production_Country_i$mid*Trend[2,5]*0.25), N = SIM)
  Production_Country_i_2010_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[1,5], min = (Production_Country_i$low*Trend[1,5] - Production_Country_i$mid*Trend[1,5]*0.30), max = (Production_Country_i$high*Trend[1,5]+ Production_Country_i$mid*Trend[1,5]*0.30), N = SIM)
  Production_Country_i_sum_Dist  <- c(Production_Country_i_2010_Dist+Production_Country_i_2011_Dist+Production_Country_i_2012_Dist+Production_Country_i_2013_Dist+Production_Country_i_2014_Dist
                                      +Production_Country_i_2015_Dist+Production_Country_i_2016_Dist+Production_Country_i_2017_Dist+Production_Country_i_2018_Dist+Production_Country_i_2019_Dist)
  
}else if (Country_i == "Vietnam"){
  
  #The year between 2015-2010, an uncertainty of 5% increasing with the distance 
  #2015 5%, 2014 10%, 2013 15%, 2012 20%, 2011 25%, 2010 30% 
  Production_Country_i <- Production_volume[which(Production_volume$Country == Country_i),] #pick up country
  Production_Country_i_2019_Dist <- triangulize_b (modalvalues = Production_Country_i$mid, min = Production_Country_i$low, max = Production_Country_i$high, N = SIM)
  Production_Country_i_2018_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[9,6], min = Production_Country_i$low*Trend[9,6], max = Production_Country_i$high*Trend[9,6], N = SIM)
  Production_Country_i_2017_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[8,6], min = Production_Country_i$low*Trend[8,6], max = Production_Country_i$high*Trend[8,6], N = SIM)
  Production_Country_i_2016_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[7,6], min = Production_Country_i$low*Trend[7,6], max = Production_Country_i$high*Trend[7,6], N = SIM)
  Production_Country_i_2015_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[6,6], min = (Production_Country_i$low*Trend[6,6] - Production_Country_i$mid*Trend[6,6]*0.05), max = (Production_Country_i$high*Trend[6,6]+ Production_Country_i$mid*Trend[6,6]*0.05), N = SIM)
  Production_Country_i_2014_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[5,6], min = (Production_Country_i$low*Trend[5,6] - Production_Country_i$mid*Trend[5,6]*0.10), max = (Production_Country_i$high*Trend[5,6]+ Production_Country_i$mid*Trend[5,6]*0.10), N = SIM)
  Production_Country_i_2013_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[4,6], min = (Production_Country_i$low*Trend[4,6] - Production_Country_i$mid*Trend[4,6]*0.15), max = (Production_Country_i$high*Trend[4,6]+ Production_Country_i$mid*Trend[4,6]*0.15), N = SIM)
  Production_Country_i_2012_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[3,6], min = (Production_Country_i$low*Trend[3,6] - Production_Country_i$mid*Trend[3,6]*0.20), max = (Production_Country_i$high*Trend[3,6]+ Production_Country_i$mid*Trend[3,6]*0.20), N = SIM)
  Production_Country_i_2011_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[2,6], min = (Production_Country_i$low*Trend[2,6] - Production_Country_i$mid*Trend[2,6]*0.25), max = (Production_Country_i$high*Trend[2,6]+ Production_Country_i$mid*Trend[2,6]*0.25), N = SIM)
  Production_Country_i_2010_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[1,6], min = (Production_Country_i$low*Trend[1,6] - Production_Country_i$mid*Trend[1,6]*0.30), max = (Production_Country_i$high*Trend[1,6]+ Production_Country_i$mid*Trend[1,6]*0.30), N = SIM)
  Production_Country_i_sum_Dist  <- c(Production_Country_i_2010_Dist+Production_Country_i_2011_Dist+Production_Country_i_2012_Dist+Production_Country_i_2013_Dist+Production_Country_i_2014_Dist
                                      +Production_Country_i_2015_Dist+Production_Country_i_2016_Dist+Production_Country_i_2017_Dist+Production_Country_i_2018_Dist+Production_Country_i_2019_Dist)
  
}else if (Country_i == "Japan"){
  
  Production_Country_i <- Production_volume[which(Production_volume$Country == Country_i),] #pick up country
  Production_Country_i_2019_Dist <- triangulize_b (modalvalues = Production_Country_i$mid, min = Production_Country_i$low, max = Production_Country_i$high, N = SIM)
  Production_Country_i_2018_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[9,3], min = Production_Country_i$low*Trend[9,3], max = Production_Country_i$high*Trend[9,3], N = SIM)
  Production_Country_i_2017_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[8,3], min = Production_Country_i$low*Trend[8,3], max = Production_Country_i$high*Trend[8,3], N = SIM)
  Production_Country_i_2016_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[7,3], min = Production_Country_i$low*Trend[7,3], max = Production_Country_i$high*Trend[7,3], N = SIM)
  Production_Country_i_2015_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[6,3], min = (Production_Country_i$low*Trend[6,3] - Production_Country_i$mid*Trend[6,3]*0.05), max = (Production_Country_i$high*Trend[6,3]+ Production_Country_i$mid*Trend[6,3]*0.05), N = SIM)
  Production_Country_i_2014_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[5,3], min = (Production_Country_i$low*Trend[5,3] - Production_Country_i$mid*Trend[5,3]*0.10), max = (Production_Country_i$high*Trend[5,3]+ Production_Country_i$mid*Trend[5,3]*0.10), N = SIM)
  Production_Country_i_2013_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[4,3], min = (Production_Country_i$low*Trend[4,3] - Production_Country_i$mid*Trend[4,3]*0.15), max = (Production_Country_i$high*Trend[4,3]+ Production_Country_i$mid*Trend[4,3]*0.15), N = SIM)
  Production_Country_i_2012_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[3,3], min = (Production_Country_i$low*Trend[3,3] - Production_Country_i$mid*Trend[3,3]*0.20), max = (Production_Country_i$high*Trend[3,3]+ Production_Country_i$mid*Trend[3,3]*0.20), N = SIM)
  Production_Country_i_2011_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[2,3], min = (Production_Country_i$low*Trend[2,3] - Production_Country_i$mid*Trend[2,3]*0.25), max = (Production_Country_i$high*Trend[2,3]+ Production_Country_i$mid*Trend[2,3]*0.25), N = SIM)
  Production_Country_i_2010_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[1,3], min = (Production_Country_i$low*Trend[1,3] - Production_Country_i$mid*Trend[1,3]*0.30), max = (Production_Country_i$high*Trend[1,3]+ Production_Country_i$mid*Trend[1,3]*0.30), N = SIM)
  Production_Country_i_sum_Dist  <- c(Production_Country_i_2010_Dist+Production_Country_i_2011_Dist+Production_Country_i_2012_Dist+Production_Country_i_2013_Dist+Production_Country_i_2014_Dist
                                      +Production_Country_i_2015_Dist+Production_Country_i_2016_Dist+Production_Country_i_2017_Dist+Production_Country_i_2018_Dist+Production_Country_i_2019_Dist)
  
}else if (Country_i == "Mexico"){
  
  Production_Country_i <- Production_volume[which(Production_volume$Country == Country_i),] #pick up country
  Production_Country_i_2019_Dist <- triangulize_b (modalvalues = Production_Country_i$mid, min = Production_Country_i$low, max = Production_Country_i$high, N = SIM)
  Production_Country_i_2018_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[9,4], min = Production_Country_i$low*Trend[9,4], max = Production_Country_i$high*Trend[9,4], N = SIM)
  Production_Country_i_2017_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[8,4], min = Production_Country_i$low*Trend[8,4], max = Production_Country_i$high*Trend[8,4], N = SIM)
  Production_Country_i_2016_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[7,4], min = Production_Country_i$low*Trend[7,4], max = Production_Country_i$high*Trend[7,4], N = SIM)
  Production_Country_i_2015_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[6,4], min = (Production_Country_i$low*Trend[6,4] - Production_Country_i$mid*Trend[6,4]*0.05), max = (Production_Country_i$high*Trend[6,4]+ Production_Country_i$mid*Trend[6,4]*0.05), N = SIM)
  Production_Country_i_2014_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[5,4], min = (Production_Country_i$low*Trend[5,4] - Production_Country_i$mid*Trend[5,4]*0.10), max = (Production_Country_i$high*Trend[5,4]+ Production_Country_i$mid*Trend[5,4]*0.10), N = SIM)
  Production_Country_i_2013_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[4,4], min = (Production_Country_i$low*Trend[4,4] - Production_Country_i$mid*Trend[4,4]*0.15), max = (Production_Country_i$high*Trend[4,4]+ Production_Country_i$mid*Trend[4,4]*0.15), N = SIM)
  Production_Country_i_2012_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[3,4], min = (Production_Country_i$low*Trend[3,4] - Production_Country_i$mid*Trend[3,4]*0.20), max = (Production_Country_i$high*Trend[3,4]+ Production_Country_i$mid*Trend[3,4]*0.20), N = SIM)
  Production_Country_i_2011_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[2,4], min = (Production_Country_i$low*Trend[2,4] - Production_Country_i$mid*Trend[2,4]*0.25), max = (Production_Country_i$high*Trend[2,4]+ Production_Country_i$mid*Trend[2,4]*0.25), N = SIM)
  Production_Country_i_2010_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[1,4], min = (Production_Country_i$low*Trend[1,4] - Production_Country_i$mid*Trend[1,4]*0.30), max = (Production_Country_i$high*Trend[1,4]+ Production_Country_i$mid*Trend[1,4]*0.30), N = SIM)
  Production_Country_i_sum_Dist  <- c(Production_Country_i_2010_Dist+Production_Country_i_2011_Dist+Production_Country_i_2012_Dist+Production_Country_i_2013_Dist+Production_Country_i_2014_Dist
                                      +Production_Country_i_2015_Dist+Production_Country_i_2016_Dist+Production_Country_i_2017_Dist+Production_Country_i_2018_Dist+Production_Country_i_2019_Dist)
  
}else{
  
  Production_Country_i <- Production_volume[which(Production_volume$Country == Country_i),] #pick up country
  Production_Country_i_2019_Dist <- triangulize_b (modalvalues = Production_Country_i$mid, min = Production_Country_i$low, max = Production_Country_i$high, N = SIM)
  Production_Country_i_2018_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[9,2], min = Production_Country_i$low*Trend[9,2], max = Production_Country_i$high*Trend[9,2], N = SIM)
  Production_Country_i_2017_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[8,2], min = Production_Country_i$low*Trend[8,2], max = Production_Country_i$high*Trend[8,2], N = SIM)
  Production_Country_i_2016_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[7,2], min = Production_Country_i$low*Trend[7,2], max = Production_Country_i$high*Trend[7,2], N = SIM)
  Production_Country_i_2015_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[6,2], min = (Production_Country_i$low*Trend[6,2] - Production_Country_i$mid*Trend[6,2]*0.05), max = (Production_Country_i$high*Trend[6,2]+ Production_Country_i$mid*Trend[6,2]*0.05), N = SIM)
  Production_Country_i_2014_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[5,2], min = (Production_Country_i$low*Trend[5,2] - Production_Country_i$mid*Trend[5,2]*0.10), max = (Production_Country_i$high*Trend[5,2]+ Production_Country_i$mid*Trend[5,2]*0.10), N = SIM)
  Production_Country_i_2013_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[4,2], min = (Production_Country_i$low*Trend[4,2] - Production_Country_i$mid*Trend[4,2]*0.15), max = (Production_Country_i$high*Trend[4,2]+ Production_Country_i$mid*Trend[4,2]*0.15), N = SIM)
  Production_Country_i_2012_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[3,2], min = (Production_Country_i$low*Trend[3,2] - Production_Country_i$mid*Trend[3,2]*0.20), max = (Production_Country_i$high*Trend[3,2]+ Production_Country_i$mid*Trend[3,2]*0.20), N = SIM)
  Production_Country_i_2011_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[2,2], min = (Production_Country_i$low*Trend[2,2] - Production_Country_i$mid*Trend[2,2]*0.25), max = (Production_Country_i$high*Trend[2,2]+ Production_Country_i$mid*Trend[2,2]*0.25), N = SIM)
  Production_Country_i_2010_Dist <- triangulize_b (modalvalues = Production_Country_i$mid*Trend[1,2], min = (Production_Country_i$low*Trend[1,2] - Production_Country_i$mid*Trend[1,2]*0.30), max = (Production_Country_i$high*Trend[1,2]+ Production_Country_i$mid*Trend[1,2]*0.30), N = SIM)
  Production_Country_i_sum_Dist  <- c(Production_Country_i_2010_Dist+Production_Country_i_2011_Dist+Production_Country_i_2012_Dist+Production_Country_i_2013_Dist+Production_Country_i_2014_Dist
                                      +Production_Country_i_2015_Dist+Production_Country_i_2016_Dist+Production_Country_i_2017_Dist+Production_Country_i_2018_Dist+Production_Country_i_2019_Dist)
}

feprod_country_i <- Production_Country_i_sum_Dist
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



setwd("C:/Users/yca/Desktop/Firmenich/Model")

