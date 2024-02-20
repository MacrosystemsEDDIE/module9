# Title: Load EDI data for Module 9
# Author: Mary Lofton
# Date: 20FEB23

# Purpose: Load most recent EDI packages for Module 9 data updates

library(tidyverse)
library(lubridate)

# BVR catwalk ----

# Package ID: edi.725.3 Cataloging System:https://pasta.edirepository.org.
# Data set title: Time series of high-frequency sensor data measuring water temperature, dissolved oxygen, conductivity, specific conductance, total dissolved solids, chlorophyll a, phycocyanin, fluorescent dissolved organic matter, and turbidity at discrete depths in Beaverdam Reservoir, Virginia, USA in 2016-2022.
# Data set creator:  Cayelan Carey - Virginia Tech 
# Data set creator:  Adrienne Breef-Pilz - Virginia Tech 
# Data set creator:  Bethany Bookout - Virginia Tech 
# Data set creator:  Ryan McClure - Virginia Tech 
# Data set creator:  Jacob Wynne - Virginia Tech 
# Contact:  Cayelan Carey -  Virginia Tech  - Cayelan@vt.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/725/3/a9a7ff6fe8dc20f7a8f89447d4dc2038" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Reservoir",     
                 "Site",     
                 "DateTime",     
                 "ThermistorTemp_C_1",     
                 "ThermistorTemp_C_2",     
                 "ThermistorTemp_C_3",     
                 "ThermistorTemp_C_4",     
                 "ThermistorTemp_C_5",     
                 "ThermistorTemp_C_6",     
                 "ThermistorTemp_C_7",     
                 "ThermistorTemp_C_8",     
                 "ThermistorTemp_C_9",     
                 "ThermistorTemp_C_10",     
                 "ThermistorTemp_C_11",     
                 "ThermistorTemp_C_12",     
                 "ThermistorTemp_C_13",     
                 "RDO_mgL_6",     
                 "RDOsat_percent_6",     
                 "RDOTemp_C_6",     
                 "RDO_mgL_13",     
                 "RDOsat_percent_13",     
                 "RDOTemp_C_13",     
                 "EXOTemp_C_1.5",     
                 "EXOCond_uScm_1.5",     
                 "EXOSpCond_uScm_1.5",     
                 "EXOTDS_mgL_1.5",     
                 "EXODOsat_percent_1.5",     
                 "EXODO_mgL_1.5",     
                 "EXOChla_RFU_1.5",     
                 "EXOChla_ugL_1.5",     
                 "EXOBGAPC_RFU_1.5",     
                 "EXOBGAPC_ugL_1.5",     
                 "EXOfDOM_RFU_1.5",     
                 "EXOfDOM_QSU_1.5",     
                 "EXOTurbidity_FNU_1.5",     
                 "EXOPressure_psi",     
                 "EXODepth_m",     
                 "EXOBattery_V",     
                 "EXOCablepower_V",     
                 "EXOWiper_V",     
                 "LvlPressure_psi_13",     
                 "Depth_m_13",     
                 "LvlTemp_C_13",     
                 "RECORD",     
                 "CR6Battery_V",     
                 "CR6Panel_Temp_C",     
                 "Flag_ThermistorTemp_C_1",     
                 "Flag_ThermistorTemp_C_2",     
                 "Flag_ThermistorTemp_C_3",     
                 "Flag_ThermistorTemp_C_4",     
                 "Flag_ThermistorTemp_C_5",     
                 "Flag_ThermistorTemp_C_6",     
                 "Flag_ThermistorTemp_C_7",     
                 "Flag_ThermistorTemp_C_8",     
                 "Flag_ThermistorTemp_C_9",     
                 "Flag_ThermistorTemp_C_10",     
                 "Flag_ThermistorTemp_C_11",     
                 "Flag_ThermistorTemp_C_12",     
                 "Flag_ThermistorTemp_C_13",     
                 "Flag_RDO_mgL_6",     
                 "Flag_RDOsat_percent_6",     
                 "Flag_RDOTemp_C_6",     
                 "Flag_RDO_mgL_13",     
                 "Flag_RDOsat_percent_13",     
                 "Flag_RDOTemp_C_13",     
                 "Flag_EXOTemp_C_1.5",     
                 "Flag_EXOCond_uScm_1.5",     
                 "Flag_EXOSpCond_uScm_1.5",     
                 "Flag_EXOTDS_mgL_1.5",     
                 "Flag_EXODOsat_percent_1.5",     
                 "Flag_EXODO_mgL_1.5",     
                 "Flag_EXOChla_RFU_1.5",     
                 "Flag_EXOChla_ugL_1.5",     
                 "Flag_EXOBGAPC_RFU_1.5",     
                 "Flag_EXOBGAPC_ugL_1.5",     
                 "Flag_EXOfDOM_RFU_1.5",     
                 "Flag_EXOfDOM_QSU_1.5",     
                 "Flag_EXOTurbidity_FNU_1.5",     
                 "Flag_EXOPressure_psi",     
                 "Flag_EXODepth_m",     
                 "Flag_EXOBattery_V",     
                 "Flag_EXOCablepower_V",     
                 "Flag_EXOWiper_V",     
                 "Flag_LvlPressure_psi_13",     
                 "Flag_LvlTemp_C_13"    ), check.names=TRUE) 

unlink(infile1)

mycols <- c("Reservoir",     
            "Site",     
            "DateTime",     
            "ThermistorTemp_C_1",     
            "ThermistorTemp_C_2",     
            "ThermistorTemp_C_3",     
            "ThermistorTemp_C_4",     
            "ThermistorTemp_C_5",     
            "ThermistorTemp_C_6",     
            "ThermistorTemp_C_7",     
            "ThermistorTemp_C_8",     
            "ThermistorTemp_C_9",     
            "ThermistorTemp_C_10",     
            "ThermistorTemp_C_11",     
            "ThermistorTemp_C_12",     
            "ThermistorTemp_C_13", 
            "RDO_mgL_6",     
            "RDO_mgL_13", 
            "EXODO_mgL_1.5",     
            "EXOChla_RFU_1.5",     
            "EXOChla_ugL_1.5"     
            )

bvr <- dt1 %>%
  filter(year(DateTime) == 2022) %>%
  select(any_of(mycols)) %>%
  mutate(Date = date(DateTime)) %>%
  select(-DateTime) %>%
  group_by(Reservoir, Site, Date) %>%
  summarise_all(list(mean), na.rm = TRUE) %>%
  add_column(site_id = "bvre")

write.csv(bvr, "./data/BVR_platform_data.csv", row.names = FALSE)

# FCR catwalk ----

# Package ID: edi.271.8 Cataloging System:https://pasta.edirepository.org.
# Data set title: Time series of high-frequency sensor data measuring water temperature, dissolved oxygen, pressure, conductivity, specific conductance, total dissolved solids, chlorophyll a, phycocyanin, fluorescent dissolved organic matter, and turbidity at discrete depths in Falling Creek Reservoir, Virginia, USA in 2018-2023.
# Data set creator:  Cayelan Carey - Virginia Tech 
# Data set creator:  Adrienne Breef-Pilz - Virginia Tech 
# Data set creator:  Vahid Daneshmand - University of Florida 
# Data set creator:  Austin Delany - Virginia Tech 
# Data set creator:  R. Thomas - Virginia Tech 
# Contact:  Cayelan Carey -  Virginia Tech  - cayelan@vt.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/271/8/fbb8c7a0230f4587f1c6e11417fe9dce" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Reservoir",     
                 "Site",     
                 "DateTime",     
                 "ThermistorTemp_C_surface",     
                 "ThermistorTemp_C_1",     
                 "ThermistorTemp_C_2",     
                 "ThermistorTemp_C_3",     
                 "ThermistorTemp_C_4",     
                 "ThermistorTemp_C_5",     
                 "ThermistorTemp_C_6",     
                 "ThermistorTemp_C_7",     
                 "ThermistorTemp_C_8",     
                 "ThermistorTemp_C_9",     
                 "RDO_mgL_5",     
                 "RDOsat_percent_5",     
                 "RDO_mgL_5_adjusted",     
                 "RDOsat_percent_5_adjusted",     
                 "RDOTemp_C_5",     
                 "RDO_mgL_9",     
                 "RDOsat_percent_9",     
                 "RDO_mgL_9_adjusted",     
                 "RDOsat_percent_9_adjusted",     
                 "RDOTemp_C_9",     
                 "EXOTemp_C_1",     
                 "EXOCond_uScm_1",     
                 "EXOSpCond_uScm_1",     
                 "EXOTDS_mgL_1",     
                 "EXODOsat_percent_1",     
                 "EXODO_mgL_1",     
                 "EXOChla_RFU_1",     
                 "EXOChla_ugL_1",     
                 "EXOBGAPC_RFU_1",     
                 "EXOBGAPC_ugL_1",     
                 "EXOfDOM_RFU_1",     
                 "EXOfDOM_QSU_1",     
                 "EXOTurbidity_FNU_1",     
                 "EXOPressure_psi",     
                 "EXODepth_m",     
                 "EXOBattery_V",     
                 "EXOCablepower_V",     
                 "EXOWiper_V",     
                 "LvlPressure_psi_9",     
                 "LvlTemp_C_9",     
                 "LvlDepth_m_9",     
                 "RECORD",     
                 "CR6Battery_V",     
                 "CR6Panel_Temp_C",     
                 "Flag_ThermistorTemp_C_surface",     
                 "Flag_ThermistorTemp_C_1",     
                 "Flag_ThermistorTemp_C_2",     
                 "Flag_ThermistorTemp_C_3",     
                 "Flag_ThermistorTemp_C_4",     
                 "Flag_ThermistorTemp_C_5",     
                 "Flag_ThermistorTemp_C_6",     
                 "Flag_ThermistorTemp_C_7",     
                 "Flag_ThermistorTemp_C_8",     
                 "Flag_ThermistorTemp_C_9",     
                 "Flag_RDO_mgL_5",     
                 "Flag_RDOsat_percent_5",     
                 "Flag_RDOTemp_C_5",     
                 "Flag_RDO_mgL_9",     
                 "Flag_RDOsat_percent_9",     
                 "Flag_RDOTemp_C_9",     
                 "Flag_EXOTemp_C_1",     
                 "Flag_EXOCond_uScm_1",     
                 "Flag_EXOSpCond_uScm_1",     
                 "Flag_EXOTDS_mgL_1",     
                 "Flag_EXODOsat_percent_1",     
                 "Flag_EXODO_mgL_1",     
                 "Flag_EXOChla_RFU_1",     
                 "Flag_EXOChla_ugL_1",     
                 "Flag_EXOBGAPC_RFU_1",     
                 "Flag_EXOBGAPC_ugL_1",     
                 "Flag_EXOfDOM_RFU_1",     
                 "Flag_EXOfDOM_QSU_1",     
                 "Flag_EXOTurbidity_FNU_1",     
                 "Flag_EXOPressure_psi",     
                 "Flag_EXODepth_m",     
                 "Flag_EXOBattery_V",     
                 "Flag_EXOCablepower_V",     
                 "Flag_EXOWiper_V",     
                 "Flag_LvlPressure_psi_9",     
                 "Flag_LvlTemp_C_9"    ), check.names=TRUE)

unlink(infile1)

mycols <- c("Reservoir",     
            "Site",     
            "DateTime",     
            "ThermistorTemp_C_surface",     
            "ThermistorTemp_C_1",     
            "ThermistorTemp_C_2",     
            "ThermistorTemp_C_3",     
            "ThermistorTemp_C_4",     
            "ThermistorTemp_C_5",     
            "ThermistorTemp_C_6",     
            "ThermistorTemp_C_7",     
            "ThermistorTemp_C_8",     
            "ThermistorTemp_C_9",     
            "RDO_mgL_5_adjusted",     
            "RDO_mgL_9_adjusted",   
            "EXODO_mgL_1",     
            "EXOChla_ugL_1"
)

fcr <- dt1 %>%
  filter(year(DateTime) == 2023) %>%
  select(any_of(mycols)) %>%
  mutate(Date = date(DateTime)) %>%
  select(-DateTime) %>%
  group_by(Reservoir, Site, Date) %>%
  summarise_all(list(mean), na.rm = TRUE)

write.csv(fcr, "./data/FCR_catwalk_data.csv", row.names = FALSE)

