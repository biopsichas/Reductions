library(tidyverse)
library(readxl)

data_folder <- "../input/"

##Pakrauname modeliavimo duomenis
mod_data <- read_excel(paste0(data_folder, "aData3_incNO3PO4.xls"), sheet = "a") 

##Pakrauname upių monitoringo duomenis
mon_data <- read.table(paste0(data_folder, "WQObs.txt"), header = TRUE, sep="\t", stringsAsFactors = FALSE,)[-1, ]

##Pakrauname ežerų monitoringo duomenis
lakes1 <- readRDS(paste0(data_folder, "ezerai_1993_2009.rds")) %>% 
  select(st_kodas, data, o2, bds7, nh4_n, no3_n, n_bendras, po4_p, p_bendras)
lakes2 <- readRDS(paste0(data_folder, "ezerai_2010_2018.rds")) %>% 
  select(st_kodas, data, o2, bds7, nh4_n, no3_n, n_bendras, po4_p, p_bendras)
lakes_type <- read_excel(paste0(data_folder, "ezeru_informacija.xlsx"), sheet = 1) %>% 
  select(vt_kodas, tipas)

##Pakrauname sąryšį tarp vandens telkinių ir modeliuotų baseinų
wb_to_mod_sub <- readRDS("../input/wb_code_to_mod_sub.rds")

##Pakrauname sąryšį tarp vandens telkinių ir monitoringo vietų
wb_to_mst <- readRDS("../input/wb_mst.rds")

##Pakrauname reikalingus kokybės vertinimo duomenis
lake_wb_eval_14_18 <- read_excel("../input/ezeru_bukle_2014_2018.xlsx", sheet=2) 
lake_wb_eval_10_13 <- read_excel("../input/ezeru_bukle_2010_2013.xlsx", sheet=1) %>% 
  filter(!is.na(as.numeric(pagrindas))) %>% 
  rename (st_kodas = pagrindas) %>% 
  mutate (st_kodas = paste0("L", st_kodas)) %>% 
  select (st_kodas, everything())

riv_wb_eval_14_18 <- read_excel("../input/upiu_bukle_2014_2018m.xlsx", sheet=1) 
riv_wb_eval_10_13 <- read_excel("../input/upiu_bukle_2010_2013m.xlsx", sheet=1)




