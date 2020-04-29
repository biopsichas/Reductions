source("clean.R")

##Paruošiame debitų duomenis vandens telkiniams
Q_mod_basin <- mod_data %>% 
  select(Year, CATCHMENTID, YIELD_TOTAL) %>% 
  group_by(CATCHMENTID) %>% 
  summarize(q = mean(YIELD_TOTAL)) %>% 
  rename(catch_id = CATCHMENTID) %>% 
  inner_join(wb_to_mod_sub, by = c("catch_id" = "Modelled catchment ID"))

Q <- wb_names %>%
  select(`VT kodas`) %>% 
  left_join(Q_mod_basin, by = c("VT kodas" = "WB code")) %>% 
  left_join(catch_id_wb, by = c("VT kodas" = "code")) %>% 
  mutate(q = ifelse(is.na(q), 0.01576 * area, q)) %>% 
  rename(wb_code = `VT kodas`) %>% 
  select(wb_code, q) %>% 
  group_by(wb_code) %>% 
  slice(which.max(q))

rm(Q_mod_basin)

##Paruošiame krūvių duomenis vandens telkiniams
##Pirmiausia iš modeliavimo rezultatų
load_mod <- mod_data %>% 
  select(-Basin, - Outflow, -Year, -YIELD_TOTAL) %>% 
  group_by(CATCHMENTID) %>% 
  summarize_all(mean) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  inner_join(wb_to_mod_sub, by = c("CATCHMENTID" = "Modelled catchment ID")) %>% 
  select(`WB code`, NO3_TOTAL, NTOTAL_TOTAL, PO4_TOTAL, PTOTAL_TOTAL) %>% 
  rename(wb_code = `WB code`,
         NO3.N = NO3_TOTAL, 
         N.total = NTOTAL_TOTAL, 
         PO4.P = PO4_TOTAL, 
         P.total = PTOTAL_TOTAL) %>% 
  gather(key = "param", value = "mload", -wb_code)
  
##Toliau iš monitoringo rezultatų
load_mon <- mon_data %>%
  select(-Date, - N.mineral, -NO2.N) %>% 
  group_by(StationID, Year) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  select(-Year) %>% 
  group_by(StationID) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  full_join(wb_to_mst, by = c("StationID" = "st_kodas")) %>% 
  drop_na(vt_kodas) %>% 
  gather(key = "param", value = "conc", -StationID, -Flow , -vt_kodas) %>% 
  drop_na(conc) %>% 
  left_join(Q, by = c("vt_kodas" = "wb_code")) %>% 
  mutate(flow = ifelse(is.na(Flow), q, (Flow + q) / 2)) %>%  ##Debitas vidutinė vertė tarp 12x matavimo ir modeliavimo rezultatų.
  select(-Flow, -q) %>% 
  mutate(load = conc * flow * 31557.6) %>%  ##Koeficientas metuose sekundės ir kiti vienetai į vieną skaičių sudėti
  filter(param %in% c("NO3.N", "N.total", "PO4.P", "P.total")) %>% 
  select(-conc, -StationID) %>% 
  drop_na(load)

##Dalinai atnaujiname debitus pagal monitoringo rezultatus
Q_load_mon <- load_mon %>% 
  select(-param, - load) %>% 
  group_by(vt_kodas) %>% 
  slice(which.max(flow)) %>% 
  full_join(Q, by = c("vt_kodas" = "wb_code")) %>% 
  mutate(fflow = ifelse(is.na(flow), q, flow)) %>% 
  select(-flow, - q) %>% 
  distinct()

rm(Q)

##Apjungiame krūvius
load <- load_mon %>% 
  full_join(load_mod, by = c("vt_kodas"="wb_code", "param")) %>% 
  mutate(fload = ifelse(!is.na(load), load, mload),
         source = ifelse(!is.na(load), "S", "M")) %>% 
  select(-load, - mload) %>% 
  mutate_if(is.numeric, round, 0) 

rm(load_mon, load_mod)

##Suskaičiuojame leistinus krūvius į vandens telkinį
max_wb_load <- wb_names %>% 
  inner_join(Q_load_mon, by = c("VT kodas" = "vt_kodas")) %>% 
  left_join(lakes_type, by = c("VT kodas" = "vt_kodas")) 

max_wb_load <- cbind(max_wb_load, NO3.N = mapply(max_load, "NO3.N", max_wb_load$tipas, max_wb_load$fflow, max_wb_load$l_type) )

max_load <- function(param, wb_type, flow, lakes_type = NA) { 
  if (param == "NO3.N"){
    if (wb_type == "U"){
      NO3.N <- 2.3 * flow * 31557.6
    } else if (wb_type == "E") {
      if(apie ezerus)
    }
  }
}


  
plot(load_mon$Flow, load_mon$q)  
cor(load_mon$Flow, load_mon$q)
qplot(load, 
      mload, 
      data = load %>% filter(load < 500000), 
      geom = c("point", "smooth"), 
      method = "lm", 
      alpha = I(1 / 5), 
      se = FALSE)
