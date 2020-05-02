###################################################
## Skaičiuojame, analizuojame, gauname rezultatus##
###################################################

source("preprocessing.R")
library("xlsx")

output_folder <- "../output/"

##Suskaičiuojame skirtumą 
reductions_tot <- max_wb_load %>% 
  left_join(load, by = c("VT kodas" = "vt_kodas")) %>% 
  mutate(r_NO3.N = NO3.N - l_NO3.N,
         r_N.total = N.total - l_N.total,
         r_PO4.P = PO4.P - l_PO4.P,
         r_P.total = P.total - l_P.total) %>% 
  filter(r_NO3.N < 0 | r_N.total < 0 | r_PO4.P < 0 | r_P.total < 0) %>% 
  mutate_at(vars(starts_with("r_")), ~replace(., . > 0, 0))

##Suskaičiuojame sumažinimus reikalingus tarp šaltinių
oldnames = c("r_NO3.N", "r_N.total", "r_PO4.P", "r_P.total", 
             "no3_agri_p", "no3_point_p", "no3_storm_p", 
             "ntotal_agri_p", "ntotal_point_p", "ntotal_storm_p", 
             "po4_agri_p", "po4_point_p", "po4_storm_p", 
             "ptotal_agri_p", "ptotal_point_p", "ptotal_storm_p")
newnames = c("NO3_total", "N_total", "PO4_total", "P_total", 
             "NO3_agri", "NO3_point", "NO3_urban_diffuse", 
             "N_agri", "N_point", "N_urban_diffuse", 
             "PO4_agri", "PO4_point", "PO4_urban_diffuse", 
             "P_agri", "P_point", "P_urban_diffuse")

reductions_by_sources <- reductions_tot %>% 
  select(Pavadinimas, `VT kodas`, tipas, source, starts_with("r_")) %>% 
  left_join(load_dist, by = c("VT kodas" = "wb_code")) %>% 
  mutate(dist_available = ifelse(is.na(no3_agri_p), "N", "Y")) %>% 
  mutate_at(vars(ends_with("_p")), ~ ifelse(is.na(.),  mean(., na.rm=TRUE), .)) %>% 
  mutate_at(vars(starts_with("no3_")), ~. * r_NO3.N) %>% 
  mutate_at(vars(starts_with("ntotal_")), ~. * r_N.total) %>% 
  mutate_at(vars(starts_with("po4_")), ~. * r_PO4.P) %>% 
  mutate_at(vars(starts_with("ptotal_")), ~. * r_P.total) %>% 
  mutate_if(is.numeric, round, 0) %>% 
  rename_at(vars(oldnames), ~ newnames) %>% 
  select(1:4, dist_available, everything())

rm(oldnames, newnames, reductions_tot)

write.xlsx(reductions_by_sources, file = paste0(output_folder, "sumazinimai.xlsx"), sheetName = "rezultatai", 
           row.names = FALSE, append = FALSE)
