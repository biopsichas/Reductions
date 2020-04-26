##Funkcija išvalyti monitoringo vertinimo  duomenis
clean_wb_eval <- function(table){
  if (startsWith(as.character(table[1,1]), "R")){
    table <- table %>% 
      select(-c(o2, bds7, nh4_n, no3_n, n_bendras, po4_p, p_bendras)) %>% 
      mutate_at(vars(-st_kodas, -vt_kodas,-pavadinimas, -tipas), funs(replace(., startsWith(., "vid"), "vidutine"))) %>% 
      rename(`Mon. st. kodas`= st_kodas,
             Pavadinimas  = pavadinimas,
             Tipas = tipas,
             Deguonis = o2_kokybe,
             BDS7 = bds7_kokybe,
             Fosfatai = p_po4_kokybe, 
             `B. fosforas` = p_b_kokybe,
             Amonis = nh4_n_kokybe,
             Nitratai = no3_n_kokybe,
             `B. azotas` = n_b_kokybe)
  } else if (startsWith(as.character(table[1,1]), "L")){
    table <- table %>% 
      select(st_kodas, vt_kodas, ezeras, tipas, bds7_kokybe, n_b_kokybe, p_b_kokybe) %>% 
      mutate_at(vars(-st_kodas, -vt_kodas,-ezeras, -tipas), funs(replace(., startsWith(., "vid"), "vidutine"))) %>% 
      rename(`Mon. st. kodas`= st_kodas,
             Pavadinimas  = ezeras,
             Tipas = tipas,
             BDS7 = bds7_kokybe,
             `B. fosforas` = p_b_kokybe,
             `B. azotas` = n_b_kokybe)
  }
  table[table=="labai gera"] <- 5
  table[table=="gera"] <- 4
  table[table=="vidutine"] <- 3
  table[table=="bloga"] <- 2
  table[table=="labai bloga"] <- 1
  return(table)
}