# =================================================================================================
# Module: PEC calculation
#
# Date of last modification: 13.04.2021
# =================================================================================================

# =================================================================================================
# 1. PEC_freshwater
#
# Unit conversion : ton = 1000 kg= 10^6 g =10^9 mg
#                   m3 = 1000 L
# Freshwater volume - Area : FAO (Unit: 1000 ha = 1000x10000 m2 = 10^7 m2 )
#                   - Depth : ECHA (3 m)
# =================================================================================================

Concentration_i <- data.frame()
Distribution_i <- data.frame()

#Unit conversion: ton= 1000kg= 10^6g =10^9 mg
index = ncol(TC_template)-1 # surface water is No in the measurestable/template 
Release_mass <- as.data.frame(t(measures[index,2:5]*10^9))  #unit mg

Distribution_mass <- as.data.frame(t(distribution[index,0:SIM]*10^9))  #unit mg

#Surfacewater_volume (m3)
#Unit: 1000 ha = 1000x10000 m2=10^7 m2, depth 3m (ECHA)
Depth_surfacewater <- 3
Area_surfacewater_i <- as.data.frame(Area_surfacewater)
Area_surfacewater_i <- Area_surfacewater[which(Area_surfacewater$Country == Country_i),]$Surface_water*10^7 #unit m2
Surfacewater_volume <- Area_surfacewater_i*Depth_surfacewater*1000 # m3 to Liter

# Unit mg/liter
Concentration_freshwater <- Release_mass[,1:4]*((40/365))/Surfacewater_volume #40 days are the water retention time
Concentration_freshwater <- cbind(Concentration_freshwater, Row = rownames(TC)[index],Country = Country_i, Unit = "mg/L") 

Concentration_i <- rbind(Concentration_i,Concentration_freshwater)

# Distribution
Distribution_freshwater <- Distribution_mass[,0:SIM]*((40/365))/Surfacewater_volume # unit mg/liter
Distribution_freshwater <- cbind(Distribution_freshwater, Row = rownames(TC)[index],Country = Country_i, Unit = "mg/L") 

Distribution_i <- rbind(Distribution_i,Distribution_freshwater)

# =================================================================================================
# 2. PEC_sediment
#
# Sediment mass - Depth : ECHA (0.03 m)
#               - Area : Area_surfacewater (m2)
#               - Density : 260 #kg/m3 (Sun et al 2016) https://doi.org/10.1021/acs.est.5b05828
#                            
# =================================================================================================

#Parameters(ECHA):Sediment depth = 0.03 m, Solid fraction 
Depth_sediment <- 0.03
Density_sediment <- 260 #kg/m3 #SUN 260 (1300 ECHA substract the water content= 260 kg/m3)

Concentration_sediment <-Release_mass[,1:4]/(Area_surfacewater_i*Depth_sediment *Density_sediment) #unit mg/kg
Concentration_sediment <- cbind(Concentration_sediment, Row = "Sediment", Country = Country_i,Unit = "mg/kg") 

Concentration_i <- rbind(Concentration_i,Concentration_sediment)

#distribution
Distribution_sediment <-Distribution_mass[,0:SIM]/(Area_surfacewater_i*Depth_sediment *Density_sediment) #unit mg/kg
Distribution_sediment <- cbind(Distribution_sediment, Row = "Sediment", Country = Country_i,Unit = "mg/kg") 

Distribution_i <- rbind(Distribution_i,Distribution_sediment)

# =================================================================================================
# 3. PEC_soil
#
# Soil mass - Depth : ECHA (0.2 m)
#           - Application of sludge : 5000 kg/ha = 5 ton / 10^4 m2 dry weight per year (ECHA)
#           - Volume of sludge : country specific, unit 1000 ton (Eurostat)
#           - Density : 1500 kg/m3 dry soil (Sun et al 2016) https://doi.org/10.1021/acs.est.5b05828
# 
# =================================================================================================

#Release to agricultral soil, unit: ton dry weight sludge
index = ncol(TC_template)-2 # Sludge
Release_mass <- as.data.frame(t(measures[index,2:5]*10^9))  #Unit:from ton to mg

#Distribusion
Distribution_mass <- as.data.frame(t(distribution[index,0:SIM]*10^9))  #Unit:from ton to mg
Distribution_mass[1,1]
Release_mass[1,1]

Depth_soil <- 0.2
Application_rate <- 5/10^4 # 5 tons/ha = 5/10000 m2 = tons/m2
Volume_sludge_i <- (Volume_sludge[which(Volume_sludge$Country == Country_i),]$Agricultral_application)*1000 #unit: thousand tons = 1000 tons

Area_soil <- Volume_sludge_i/Application_rate #tons/(tons/m2) = m2

Concentration_soil<- Release_mass[,1:4]/(Area_soil*Depth_soil*1500) #mg/kg 
Concentration_soil <- cbind(Concentration_soil, Row = rownames(TC)[index], Country = Country_i,Unit = "mg/kg") 

Concentration_i <- rbind(Concentration_i,Concentration_soil)
Concentration <- rbind(Concentration,Concentration_i)

#Distribution
Distribution_soil<- Distribution_mass[,0:SIM]/(Area_soil*Depth_soil*1500) #mg/kg , from Gottschalk
Distribution_soil <- cbind(Distribution_soil, Row = rownames(TC)[index], Country = Country_i,Unit = "mg/kg") 

Distribution_i <- rbind(Distribution_i,Distribution_soil)
Distribution <- rbind(Distribution,Distribution_i)

