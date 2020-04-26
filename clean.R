library(lubridate)
source("load.R")
source("function.R")

##Apdorojame upių moniutoringo duomenis
mon_data$Date <- ymd(mon_data$Date)
mon_data[, 3:15] <- as.numeric(unlist(mon_data[, 3:15]))
mon_data$StationID <-  sub("^", "R", mon_data[grepl("^[0-9]", mon_data$StationID), "StationID"])
mon_data$Year <- lubridate::year(mon_data$Date)

##Sujungiame upių ir ežerų duomenis
lakes <- bind_rows(lakes1, lakes2) %>%
  rename(StationID = st_kodas,
         Date = data,
         DO = o2,
         BOD7 = bds7,
         NH4.N = nh4_n,
         NO3.N = no3_n,
         N.total = n_bendras,
         PO4.P = po4_p,
         P.total = p_bendras)
lakes$Year <- lubridate::year(lakes$Date)
lakes <- lakes %>% filter(Year > 1995)

mon_data <- bind_rows(mon_data, lakes)

rm(lakes1, lakes2, lakes)

##Apdorojame upių ir ežerų vertinimo duomenis
clean_14_18_0 <- bind_rows(clean_wb_eval(riv_wb_eval_14_18), clean_wb_eval(lake_wb_eval_14_18))
clean_10_13_0 <- bind_rows(clean_wb_eval(riv_wb_eval_10_13), clean_wb_eval(lake_wb_eval_10_13))

clean_14_18 <- clean_14_18_0 %>%
  select(-vt_kodas) %>%
  select(1, 2, 3, 5, 4, 6, 7, 8, 10, 9)
clean_10_13 <- clean_10_13_0 %>%
  select(-vt_kodas) %>%
  select(1, 2, 3, 5, 4, 6, 7, 8, 10, 9)

rm(clean_10_13_0, clean_14_18_0, riv_wb_eval_10_13, riv_wb_eval_14_18, lake_wb_eval_10_13, lake_wb_eval_14_18)

# lintr::lint("clean.R")
# library(profvis)
# profvis({source('clean.R')})
  