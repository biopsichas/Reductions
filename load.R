###################################################################################################
## Pakrauname į atminti reikalingus duomenis ir apdorojame, jei reikia, ne tik daugiau 3 eilučių ##
###################################################################################################

library(tidyverse)
library(readxl)
library(sf)

data_folder <- "../input/"
year_from <- 2014

##Pakrauname vandens telkinių ir geometrinio tinklo sąryšių duomenis
catch_id_wb <- read.csv(paste0(data_folder, "catch_id_wb.csv"), sep=";", stringsAsFactors = FALSE)

##Pakrauname telkinių kodus ir pavadinimis 
wb_names <- readRDS(paste0(data_folder, "wb_names.rds")) %>% 
  filter(nchar(`VT kodas`) == 11) %>% 
  distinct()

##Pakrauname modeliavimo duomenis
mod_data <- read_excel(paste0(data_folder, "aData3_incNO3PO4.xls"), sheet = "a") %>% 
  filter(Year >= year_from)

##Pakrauname upių monitoringo duomenis
mon_data <- read.table(paste0(data_folder, "WQObs.txt"), header = TRUE, sep="\t", stringsAsFactors = FALSE)[-1, ] %>% 
  filter(Date >= year_from) %>% 
  select(-N.Org, -P.Org) 

##Pakrauname ežerų monitoringo duomenis
lakes1 <- readRDS(paste0(data_folder, "ezerai_1993_2009.rds")) %>% 
  select(st_kodas, data, o2, bds7, nh4_n, no3_n, n_bendras, po4_p, p_bendras)
lakes2 <- readRDS(paste0(data_folder, "ezerai_2010_2018.rds")) %>% 
  select(st_kodas, data, o2, bds7, nh4_n, no3_n, n_bendras, po4_p, p_bendras)
lakes_type <- read_excel(paste0(data_folder, "ezeru_informacija.xlsx"), sheet = 1) %>% 
  select(vt_kodas, tipas) %>% 
  rename(l_type = tipas)

##Pakrauname sąryšį tarp vandens telkinių ir modeliuotų baseinų
wb_to_mod_sub <- readRDS(paste0(data_folder, "wb_code_to_mod_sub.rds"))

##Pakrauname sąryšį tarp vandens telkinių ir monitoringo vietų
wb_to_mst <- readRDS(paste0(data_folder, "wb_mst.rds"))

##Pakrauname reikalingus kokybės vertinimo duomenis
lake_wb_eval_14_18 <- read_excel(paste0(data_folder, "ezeru_bukle_2014_2018.xlsx"), sheet=2)
riv_wb_eval_14_18 <- read_excel(paste0(data_folder, "upiu_bukle_2014_2018m.xlsx"), sheet=1) 

##Pakrauname upių vandens telkinius su GIS informacija
wb_rivers_sf <- sf::st_read(dsn = paste0(data_folder,"GISDATA.gdb"), layer = "WB_line_150420", quiet = TRUE)

##Pakrauname ežerų vandens telkinius su GIS informacija
wb_lakes_sf <- sf::st_read(dsn = paste0(data_folder,"GISDATA.gdb"), layer = "WB_Lakes_150420", quiet = TRUE)