# Script that performs RQ calculatation
# =================================================================================================
#                                 
# Required input: - Distribution_Cumulative.csv: a dataset including the distribution of PECs 
#                     
# Output:   - RQ_HONEC : Q5, median, Mean, Q95 of RQs
#           - RQ_distribution : full distribution of RQs 
#           - RQ_af1 :RQ calculated based on the first set of afs(freshwater: 1000; sediment: 100; soil: 100)
#           - RQ_distribution : full distribution of RQ_af1 
#           - RQ_af2 :RQ calculated based on the second set of afs(freshwater: 1000; sediment: 100; soil: 50)
#           - RQ_distribution : full distribution of RQ_af2 
#
# Date of last modification: 13.04.2021
# =================================================================================================
# library

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(forcats)
library(data.table)

# =================================================================================================
# 1. Data preparation 
# =================================================================================================

#Risk calculation, read the PEC distribution file.
PEC <- fread("C:/Users/yca/Desktop/Firmenich/Output/Distribution_Cumulative.csv")

# =================================================================================================
# 2. Compared with HONEC
#   Freshwater: 2.67 mg/L
#   Sediment: 5.35 mg/kg
#   Soil: 9.1 mg/kg
# =================================================================================================

RQ_freshwater_HONEC <- PEC[which(PEC$Row == "Surfacewater"),] #unit mg/L
RQ_sediment_HONEC <-PEC[which(PEC$Row == "Sediment"),] #unit mg/kg
RQ_soil_HONEC <-PEC[which(PEC$Row == "STSoil"),] #unit mg/kg

RQ_freshwater_HONEC[,2:100001] <- RQ_freshwater_HONEC[,2:100001]/2.67 #mg/L vs mg/L 
RQ_sediment_HONEC[,2:100001] <- RQ_sediment_HONEC[,2:100001]/5.35 #mg/kg vs mg/kg
RQ_soil_HONEC[,2:100001] <- RQ_soil_HONEC[,2:100001]/9.1 #mg/kg vs mg/kg

# =================================================================================================
# 3. Compared with PNEC
#   Freshwater: 2.67 mg/L / 1000 (AF = 1000)
#   Sediment: 5.35 mg/kg / 100 (AF = 100)
#   Soil: 91 mg/kg / 100 (AF = 100)
# =================================================================================================

RQ_freshwater_af1 <- PEC[which(PEC$Row == "Surfacewater"),] #unit mg/L
RQ_sediment_af1 <- PEC[which(PEC$Row == "Sediment"),] #unit mg/kg
RQ_soil_af1 <- PEC[which(PEC$Row == "STSoil"),] #unit mg/kg

RQ_freshwater_af1[,2:100001] <- RQ_freshwater_af1[,2:100001]/2.67*1000 #mg/L vs mg/L 
RQ_sediment_af1[,2:100001] <- RQ_sediment_af1[,2:100001]/5.35*100 #mg/kg vs mg/kg
RQ_soil_af1[,2:100001] <- RQ_soil_af1[,2:100001]/9.1*100 #mg/kg vs mg/kg

# =================================================================================================
# 4. Compared with PNEC 
#   Freshwater: 2.67 mg/L / 1000 (AF = 1000)
#   Sediment: 53.5 mg/kg / 100 (AF = 100)
#   Soil: 91 mg/kg / 50 (AF = 50)
# =================================================================================================

RQ_soil_af2 <- PEC[which(PEC$Row == "STSoil"),] #unit mg/kg

RQ_soil_af2[,2:100001] <- RQ_soil_af2[,2:100001]/9.1*50 #mg vs mg

# =================================================================================================
# 5. Perform calculation
# =================================================================================================

RQ_HONEC <- data.frame()
RQ_distribution_HONEC <- data.frame()

RQ_af1 <- data.frame()
RQ_distribution_af1 <- data.frame()

RQ_af2 <- data.frame()
RQ_distribution_af2 <- data.frame()


for (i in 1:34) {
  
  Country_i <- RQ_freshwater_HONEC[i,]$Country
  
  #########################################################################
  #5.1 Compared with HONEC
  #########################################################################
  
  freshwater_HONEC <- as.numeric(RQ_freshwater_HONEC[i,2:100001])
  
  freshwater_distribution_HONEC_i <- data.frame(Country =Country_i, Compartment = "Freshwater",RQ = freshwater_HONEC, AF = "HONEC")
  
  freshwater_HONEC_i <- data.frame(Country =Country_i, Compartment = "Freshwater",
                                   RQ_min = min(freshwater_HONEC),RQ_Q5 = quantile(freshwater_HONEC, c(0.05)),
                                   RQ_median = median(freshwater_HONEC), RQ_mean = mean(freshwater_HONEC),RQ_Q95 = quantile(freshwater_HONEC,c(0.95)),RQ_max = max(freshwater_HONEC))
  
  sediment_HONEC <- as.numeric(RQ_sediment_HONEC[i,2:100001])
  sediment_distribution_HONEC_i <- data.frame(Country =Country_i, Compartment = "Sediment",RQ = sediment_HONEC, AF = "HONEC")
  sediment_HONEC_i <- data.frame(Country =Country_i, Compartment = "Sediment",
                           RQ_min = min(sediment_HONEC),RQ_Q5 = quantile(sediment_HONEC, c(0.05)),
                           RQ_median = median(sediment_HONEC),RQ_mean = mean(sediment_HONEC),RQ_Q95 = quantile(sediment_HONEC,c(0.95)),RQ_max = max(sediment_HONEC))
  
  soil_HONEC <- as.numeric(RQ_soil_HONEC[i,2:100001])
  soil_distribution_HONEC_i <- data.frame(Country =Country_i, Compartment = "Soil",RQ = soil_HONEC, AF = "HONEC")
  soil_HONEC_i <- data.frame(Country =Country_i, Compartment = "Soil",
                       RQ_min = min(soil_HONEC),RQ_Q5 = quantile(soil_HONEC, c(0.05)),
                       RQ_median = median(soil_HONEC),RQ_mean = mean(soil_HONEC),RQ_Q95 = quantile(soil_HONEC,c(0.95)),RQ_max = max(soil_HONEC))
  
  #bind statistic 
  RQ_HONEC <- rbind(RQ_HONEC,freshwater_HONEC_i,sediment_HONEC_i, soil_HONEC_i)
  
  #bind distribution
  RQ_distribution_HONEC <- rbind(RQ_distribution_HONEC,  freshwater_distribution_HONEC_i , sediment_distribution_HONEC_i, soil_distribution_HONEC_i)
  
  #########################################################################
  #5.2 Compared with PNEC
  #########################################################################
  
  freshwater_af1 <- as.numeric(RQ_freshwater_af1[i,2:100001])
  
  freshwater_distribution_af1_i <- data.frame(Country =Country_i, Compartment = "Freshwater",RQ = freshwater_af1, AF = "af1")
  
  freshwater_af1_i <- data.frame(Country =Country_i, Compartment = "Freshwater",
                                 RQ_min = min(freshwater_af1),RQ_Q5 = quantile(freshwater_af1, c(0.05)),
                                 RQ_median = median(freshwater_af1),RQ_mean = mean(freshwater_af1),RQ_Q95 = quantile(freshwater_af1,c(0.95)),RQ_max = max(freshwater_af1))
  
  
  sediment_af1 <- as.numeric(RQ_sediment_af1[i,2:100001])
  sediment_distribution_af1_i <- data.frame(Country =Country_i, Compartment = "Sediment",RQ = sediment_af1, AF = "af1")
  sediment_af1_i <- data.frame(Country =Country_i, Compartment = "Sediment",
                               RQ_min = min(sediment_af1),RQ_Q5 = quantile(sediment_af1, c(0.05)),
                               RQ_median = median(sediment_af1),RQ_mean = mean(sediment_af1),RQ_Q95 = quantile(sediment_af1,c(0.95)),RQ_max = max(sediment_af1))
  
  soil_af1 <- as.numeric(RQ_soil_af1[i,2:100001])
  soil_distribution_af1_i <- data.frame(Country =Country_i, Compartment = "Soil",RQ = soil_af1, AF = "af1")
  soil_af1_i <- data.frame(Country =Country_i, Compartment = "Soil",
                           RQ_min = min(soil_af1),RQ_Q5 = quantile(soil_af1, c(0.05)),
                           RQ_median = median(soil_af1),RQ_mean = mean(soil_af1),RQ_Q95 = quantile(soil_af1,c(0.95)),RQ_max = max(soil_af1))
  
  #bind statistic 
  RQ_af1 <- rbind(RQ_af1,freshwater_af1_i,sediment_af1_i, soil_af1_i)
  
  #bind distribution
  RQ_distribution_af1 <- rbind(RQ_distribution_af1,  freshwater_distribution_af1_i , sediment_distribution_af1_i, soil_distribution_af1_i)
  
  #########################################################################
  #5.3 Compared with PNEC, af for soil 50
  #########################################################################
  
  soil_af2 <- as.numeric(RQ_soil_af2[i,2:100001])
  soil_distribution_af2_i <- data.frame(Country =Country_i, Compartment = "Soil",RQ = soil_af2, AF = "af2")
  soil_af2_i <- data.frame(Country =Country_i, Compartment = "Soil",
                        RQ_min = min(soil_af2),RQ_Q5 = quantile(soil_af2, c(0.05)),
                        RQ_median = median(soil_af2),RQ_mean = mean(soil_af2),RQ_Q95 = quantile(soil_af2,c(0.95)),RQ_max = max(soil_af2))
  
  #bind statistic 
  RQ_af2 <- rbind(RQ_af2,freshwater_af1_i,sediment_af1_i, soil_af2_i)
  
  #bind distribution
  RQ_distribution_af2 <- rbind(RQ_distribution_af2,  freshwater_distribution_af1_i , sediment_distribution_af1_i, soil_distribution_af2_i)
  
  i=i+1
  print(i)
  
}

# =================================================================================================
# 6. Export the data
# =================================================================================================

write.csv(RQ_HONEC,"C:/Users/yca/Desktop/Firmenich/Output/RQ_HONEC.csv")
write.csv(RQ_distribution_HONEC,"C:/Users/yca/Desktop/Firmenich/Output/RQ_distribution_HONEC.csv")

write.csv(RQ_af1,"C:/Users/yca/Desktop/Firmenich/Output/RQ_af1.csv")
write.csv(RQ_distribution_af1,"C:/Users/yca/Desktop/Firmenich/Output/RQ_distribution_af1.csv")

write.csv(RQ_af2,"C:/Users/yca/Desktop/Firmenich/Output/RQ_af2.csv")
write.csv(RQ_distribution_af2,"C:/Users/yca/Desktop/Firmenich/Output/RQ_distribution_af2.csv")

