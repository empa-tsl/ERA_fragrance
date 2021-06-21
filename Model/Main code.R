# Script that performs an MFA Calculation and calculates PECs
# =================================================================================================
# 
# The model was developed based on the previous model by Rajkovic et al (2020) https://doi.org/10.1016/j.wasman.2020.05.032
#                                 
# Required input:     - Country : a list of investigated countries 
#                     - Production_volume : production/consumption/release data 
#                     - TC_world : a table containing all the modal transfer coefficients for different countries
#                     - UC_world : a table containing the uncertainty values for transfer coefficients
#                     - TC_template : a matrix template containing a list of different compartments
#                     - WWTP removal efficiency: a list of WWTP removal efficiency of microplastics
#
# Required scripts:   - Function.R
#                     - MFA model.R
#                     - PEC calculation.R
#                     - MFA model cumulative.R
#
#                     
# Output:   - Flow_2019 : table containing the masses of the substances in each compartment for the year of 2019
#           - Plot : bar plot containing the distribution of masses in each compartment for the year of 2019
#           - Distribution_2019 : table containing PEC distribution for freshwater, soil and sediment for the year of 2019
#           - PEC_2019 : table containing PEC (Q5, Mode, Mean, Q95)
#           - Distribution_cumulative : table containing PEC distribution for freshwater, soil and sediment for the years between 2010-2019
#           - PEC_cumulative : table containing PEC (Q5, Mode, Mean, Q95) for the years between 2010-2019
#                           
# Date of last modification: 12.04.2021
# =================================================================================================

library(mc2d)
library(mvtnorm)

# =================================================================================================
# 1. Load input data
# =================================================================================================

setwd("C:/Users/yca/Desktop/Firmenich/Input")

Country <- read.csv("Country list.csv")
Production_volume <-read.csv("Production volume.csv")

#TC and UC for MFA model
TC_world <- read.table (file = "TC.csv", header = TRUE, sep = ",")
TC_world <- replace(TC_world, TC_world == 0, 10^-50) 
UC_world <- read.table (file = "UC.csv", header = TRUE, sep = ",")

TC_template <- read.table(file = "TC template.csv", header = TRUE, sep = ",")
row.names(TC_template)<- TC_template[0:(ncol(TC_template)-1),1]
TC_template <- TC_template[,2:ncol(TC_template)]
TC_template[1:ncol(TC_template),1:ncol(TC_template)] <- 0

# area of surface water and volume of sludge applied on soil for PEC calculation
Area_surfacewater <-read.csv("Environment compartment.csv")
Volume_sludge <-read.csv("Sludge volume.csv")
WWTP_removal_efficiency <- read.csv("WWTP removal efficiency.csv")

#Trend of production volume between the year 2010 and 2019
Trend <- read.csv("Trend.csv",sep = ",")

# =================================================================================================
# 2. Perform MFA analysis and calculate PECs for each country for the year of 2019
# =================================================================================================
# =================================================================================================
# 2.1 Perform the calculation 
# =================================================================================================

Flow <- data.frame()
Concentration <- data.frame()
Distribution <- data.frame()

for (i in 1:nrow(Country)){
  
  Country_i = as.character(Country[i,2])
  
  TC_Country_i <- TC_world[which(TC_world$Country == Country_i),]
  TC_i <- TC_template
  
  # Generate TC matrix 
  TC_i["Softener","Production"] <- TC_Country_i$Production_Softener
  TC_i["Washing","Softener"] <- TC_Country_i$Softener_Washing
  TC_i["SW","Washing"] <- TC_Country_i$Washing_SW
  TC_i["Wastewater","Washing"] <- TC_Country_i$Washing_WW
  
  TC_i["SWLandfill","SW"] <- TC_Country_i$SW_SWLandfill
  TC_i["SWIncineration","SW"] <- TC_Country_i$SW_SWIncineration
  TC_i["SWRecycling","SW"] <- TC_Country_i$SW_SWRecycling
  
  TC_i["Landfill","SWLandfill"] <- TC_Country_i$SWLandfill_Landfill
  TC_i["Incineration","SWIncineration"] <- TC_Country_i$SWIncineration_Incineration
  TC_i["Wastewater","SWRecycling"] <- TC_Country_i$SWRecyling_WW
  
  TC_i["Sewage","Wastewater"] <- TC_Country_i$WW_Sewage
  TC_i["Onsite","Wastewater"] <- TC_Country_i$WW_OnSite
  TC_i["WWSurfacewater","Wastewater"] <- TC_Country_i$WW_WWSurfacewater
  TC_i["Surfacewater","WWSurfacewater"] <- TC_Country_i$WWSurfacewater_Surfacewater
  
  TC_i["Subsurface","Onsite"] <- TC_Country_i$OnSite_Subsurface
  
  TC_i["WWTP","Sewage"] <- TC_Country_i$Sewage_WWTP
  TC_i["SewageOverflow","Sewage"] <- TC_Country_i$Sewage_Overflow
  TC_i["SewageNoTreatment","Sewage"] <- TC_Country_i$Sewage_NoTreatment
  TC_i["SewageExfiltration","Sewage"] <- TC_Country_i$Sewage_Exfiltration
  
  TC_i["Surfacewater","SewageOverflow"] <- TC_Country_i$Overflow_Surfacewater
  TC_i["Surfacewater","SewageNoTreatment"] <- TC_Country_i$NoTreatment_Surfacewater
  TC_i["Subsurface","SewageExfiltration"] <- TC_Country_i$Exfiltration_Subsurface
  
  TC_i["WWTPSludge","WWTP"] <- TC_Country_i$WWTP_Sludge
  TC_i["Surfacewater","WWTP"] <- TC_Country_i$WWTP_Surfacewater
  
  TC_i["SludgeLandfill","WWTPSludge"] <- TC_Country_i$Sludge_SLandfill
  TC_i["SludgeIncineration","WWTPSludge"] <- TC_Country_i$Sludge_SIncineration
  TC_i["STSoil","WWTPSludge"] <- TC_Country_i$Sludge_STSoil
  
  TC_i["Landfill","SludgeLandfill"] <- TC_Country_i$SLandfill_Landfill
  TC_i["Incineration","SludgeIncineration"] <- TC_Country_i$SIncineration_Incineration
  
  TC <- TC_i
  
  # Generate UC matrix 
  UC_Country_i <- UC_world[which(UC_world$Country == Country_i),]
  UC_i <- TC_template
  
  UC_i["Softener","Production"] <- UC_Country_i$Production_Softener
  UC_i["Washing","Softener"] <- UC_Country_i$Softener_Washing
  UC_i["SW","Washing"] <- UC_Country_i$Washing_SW
  UC_i["Wastewater","Washing"] <- UC_Country_i$Washing_WW
  
  UC_i["SWLandfill","SW"] <- UC_Country_i$SW_SWLandfill
  UC_i["SWIncineration","SW"] <- UC_Country_i$SW_SWIncineration
  UC_i["SWRecycling","SW"] <- UC_Country_i$SW_SWRecycling
  
  UC_i["Landfill","SWLandfill"] <- UC_Country_i$SWLandfill_Landfill
  UC_i["Incineration","SWIncineration"] <- UC_Country_i$SWIncineration_Incineration
  UC_i["Wastewater","SWRecycling"] <- UC_Country_i$SWRecyling_WW
  
  UC_i["Sewage","Wastewater"] <- UC_Country_i$WW_Sewage
  UC_i["Onsite","Wastewater"] <- UC_Country_i$WW_OnSite
  UC_i["WWSurfacewater","Wastewater"] <- UC_Country_i$WW_WWSurfacewater
  UC_i["Surfacewater","WWSurfacewater"] <- UC_Country_i$WWSurfacewater_Surfacewater
  
  UC_i["Subsurface","Onsite"] <- UC_Country_i$OnSite_Subsurface
  
  UC_i["WWTP","Sewage"] <- UC_Country_i$Sewage_WWTP
  UC_i["SewageOverflow","Sewage"] <- UC_Country_i$Sewage_Overflow
  UC_i["SewageNoTreatment","Sewage"] <- UC_Country_i$Sewage_NoTreatment
  UC_i["SewageExfiltration","Sewage"] <- UC_Country_i$Sewage_Exfiltration
  
  UC_i["Surfacewater","SewageOverflow"] <- UC_Country_i$Overflow_Surfacewater
  UC_i["Surfacewater","SewageNoTreatment"] <- UC_Country_i$NoTreatment_Surfacewater
  UC_i["Subsurface","SewageExfiltration"] <- UC_Country_i$Exfiltration_Subsurface
  
  UC_i["WWTPSludge","WWTP"] <- UC_Country_i$WWTP_Sludge
  UC_i["Surfacewater","WWTP"] <- UC_Country_i$WWTP_Surfacewater
  
  UC_i["SludgeLandfill","WWTPSludge"] <- UC_Country_i$Sludge_SLandfill
  UC_i["SludgeIncineration","WWTPSludge"] <- UC_Country_i$Sludge_SIncineration
  UC_i["STSoil","WWTPSludge"] <- UC_Country_i$Sludge_STSoil
  
  UC_i["Landfill","SludgeLandfill"] <- UC_Country_i$SLandfill_Landfill
  UC_i["Incineration","SludgeIncineration"] <- UC_Country_i$SIncineration_Incineration
  
  TC.uncertainty <- UC_i
  
  setwd("C:/Users/yca/Desktop/Firmenich/Model")
  source("Function.R")
  source("MFA model.R")
  source("PEC calculation.R")
  
  i = i+1
  
  print(Country_i)
}

PEC_2019 <- Concentration 
Distribution_2019 <- Distribution


# =================================================================================================
# 2.2 Export the file
#
# Output: - Flow : a summary of the flows for all the countries 
#         - PEC: PEC (Q5, Mode, Mean, Q95) for the year of 2019
#         - Distribution : full distribution of PECs for the year of 2019
# =================================================================================================

setwd("C:/Users/yca/Desktop/Firmenich/Output")

write.table(Flow,file = "Flow_2019.csv" )
write.table(Concentration,file = "PEC_2019.csv" )
write.table(Distribution,file = "Distribution_2019.csv" )

# =================================================================================================
# 3. Perform MFA analysis and calculate cumulative PECs for each country for the year between 2010 and 2019
# =================================================================================================
# =================================================================================================
# 3.1 Perform the calculation 
# =================================================================================================

Flow <- data.frame()
Concentration <- data.frame()
Distribution <- data.frame()

for (i in 1:nrow(Country)){
  
  Country_i = as.character(Country[i,2])
  
  TC_Country_i <- TC_world[which(TC_world$Country == Country_i),]
  TC_i <- TC_template
  
  # Generate TC matrix 
  TC_i["Softener","Production"] <- TC_Country_i$Production_Softener
  TC_i["Washing","Softener"] <- TC_Country_i$Softener_Washing
  TC_i["SW","Washing"] <- TC_Country_i$Washing_SW
  TC_i["Wastewater","Washing"] <- TC_Country_i$Washing_WW
  
  TC_i["SWLandfill","SW"] <- TC_Country_i$SW_SWLandfill
  TC_i["SWIncineration","SW"] <- TC_Country_i$SW_SWIncineration
  TC_i["SWRecycling","SW"] <- TC_Country_i$SW_SWRecycling
  
  TC_i["Landfill","SWLandfill"] <- TC_Country_i$SWLandfill_Landfill
  TC_i["Incineration","SWIncineration"] <- TC_Country_i$SWIncineration_Incineration
  TC_i["Wastewater","SWRecycling"] <- TC_Country_i$SWRecyling_WW
  
  TC_i["Sewage","Wastewater"] <- TC_Country_i$WW_Sewage
  TC_i["Onsite","Wastewater"] <- TC_Country_i$WW_OnSite
  TC_i["WWSurfacewater","Wastewater"] <- TC_Country_i$WW_WWSurfacewater
  TC_i["Surfacewater","WWSurfacewater"] <- TC_Country_i$WWSurfacewater_Surfacewater
  
  TC_i["Subsurface","Onsite"] <- TC_Country_i$OnSite_Subsurface
  
  TC_i["WWTP","Sewage"] <- TC_Country_i$Sewage_WWTP
  TC_i["SewageOverflow","Sewage"] <- TC_Country_i$Sewage_Overflow
  TC_i["SewageNoTreatment","Sewage"] <- TC_Country_i$Sewage_NoTreatment
  TC_i["SewageExfiltration","Sewage"] <- TC_Country_i$Sewage_Exfiltration
  
  TC_i["Surfacewater","SewageOverflow"] <- TC_Country_i$Overflow_Surfacewater
  TC_i["Surfacewater","SewageNoTreatment"] <- TC_Country_i$NoTreatment_Surfacewater
  TC_i["Subsurface","SewageExfiltration"] <- TC_Country_i$Exfiltration_Subsurface
  
  TC_i["WWTPSludge","WWTP"] <- TC_Country_i$WWTP_Sludge
  TC_i["Surfacewater","WWTP"] <- TC_Country_i$WWTP_Surfacewater
  
  TC_i["SludgeLandfill","WWTPSludge"] <- TC_Country_i$Sludge_SLandfill
  TC_i["SludgeIncineration","WWTPSludge"] <- TC_Country_i$Sludge_SIncineration
  TC_i["STSoil","WWTPSludge"] <- TC_Country_i$Sludge_STSoil
  
  TC_i["Landfill","SludgeLandfill"] <- TC_Country_i$SLandfill_Landfill
  TC_i["Incineration","SludgeIncineration"] <- TC_Country_i$SIncineration_Incineration
  
  TC <- TC_i
  
  # Generate UC matrix 
  UC_Country_i <- UC_world[which(UC_world$Country == Country_i),]
  UC_i <- TC_template
  
  UC_i["Softener","Production"] <- UC_Country_i$Production_Softener
  UC_i["Washing","Softener"] <- UC_Country_i$Softener_Washing
  UC_i["SW","Washing"] <- UC_Country_i$Washing_SW
  UC_i["Wastewater","Washing"] <- UC_Country_i$Washing_WW
  
  UC_i["SWLandfill","SW"] <- UC_Country_i$SW_SWLandfill
  UC_i["SWIncineration","SW"] <- UC_Country_i$SW_SWIncineration
  UC_i["SWRecycling","SW"] <- UC_Country_i$SW_SWRecycling
  
  UC_i["Landfill","SWLandfill"] <- UC_Country_i$SWLandfill_Landfill
  UC_i["Incineration","SWIncineration"] <- UC_Country_i$SWIncineration_Incineration
  UC_i["Wastewater","SWRecycling"] <- UC_Country_i$SWRecyling_WW
  
  UC_i["Sewage","Wastewater"] <- UC_Country_i$WW_Sewage
  UC_i["Onsite","Wastewater"] <- UC_Country_i$WW_OnSite
  UC_i["WWSurfacewater","Wastewater"] <- UC_Country_i$WW_WWSurfacewater
  UC_i["Surfacewater","WWSurfacewater"] <- UC_Country_i$WWSurfacewater_Surfacewater
  
  UC_i["Subsurface","Onsite"] <- UC_Country_i$OnSite_Subsurface
  
  UC_i["WWTP","Sewage"] <- UC_Country_i$Sewage_WWTP
  UC_i["SewageOverflow","Sewage"] <- UC_Country_i$Sewage_Overflow
  UC_i["SewageNoTreatment","Sewage"] <- UC_Country_i$Sewage_NoTreatment
  UC_i["SewageExfiltration","Sewage"] <- UC_Country_i$Sewage_Exfiltration
  
  UC_i["Surfacewater","SewageOverflow"] <- UC_Country_i$Overflow_Surfacewater
  UC_i["Surfacewater","SewageNoTreatment"] <- UC_Country_i$NoTreatment_Surfacewater
  UC_i["Subsurface","SewageExfiltration"] <- UC_Country_i$Exfiltration_Subsurface
  
  UC_i["WWTPSludge","WWTP"] <- UC_Country_i$WWTP_Sludge
  UC_i["Surfacewater","WWTP"] <- UC_Country_i$WWTP_Surfacewater
  
  UC_i["SludgeLandfill","WWTPSludge"] <- UC_Country_i$Sludge_SLandfill
  UC_i["SludgeIncineration","WWTPSludge"] <- UC_Country_i$Sludge_SIncineration
  UC_i["STSoil","WWTPSludge"] <- UC_Country_i$Sludge_STSoil
  
  UC_i["Landfill","SludgeLandfill"] <- UC_Country_i$SLandfill_Landfill
  UC_i["Incineration","SludgeIncineration"] <- UC_Country_i$SIncineration_Incineration
  
  TC.uncertainty <- UC_i
  
  setwd("C:/Users/yca/Desktop/Firmenich/Model")
  source("Function.R")
  source("MFA model cumulative.R")
  source("PEC calculation.R")
  
  i = i+1
  
  print(Country_i)
}

PEC_Cumulative <- Concentration 
Distribution_Cumulative_temp <- Distribution


# =================================================================================================
# 3.2 Export
#
# Output: - PEC: cumulative PEC (Q5, Mode, Mean, Q95) for the year between 2010 and 2019
#         - Distribution : full distribution of PECs for the year between 2010 and 2019
# =================================================================================================
setwd("C:/Users/yca/Desktop/Firmenich/Output")

PEC_2019_freshwater <- PEC_2019[which(PEC_2019$Row == "Surfacewater"),]
PEC_Cumulative_Sediment_STSoil <- PEC_Cumulative[which(PEC_Cumulative$Row != "Surfacewater"),]
PEC_Cumulative <- rbind(PEC_2019_freshwater, PEC_Cumulative_Sediment_STSoil)

Distribution_2019_freshwater <- Distribution_2019[which(Distribution_2019$Row == "Surfacewater"),]
Distribution_Cumulative_temp <- Distribution_Cumulative_temp[which(Distribution_Cumulative_temp$Row != "Surfacewater"),]

Distribution_Cumulative <- rbind(Distribution_2019_freshwater, Distribution_Cumulative_temp)

write.table(PEC_Cumulative,file = "PEC_Cumulative.csv" )
write.table(Distribution_Cumulative,file = "Distribution_Cumulative.csv" )

