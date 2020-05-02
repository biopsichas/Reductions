library(DT)
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
  table[table=="labai gera"] <- "5"
  table[table=="gera"] <- "4"
  table[table=="vidutine"] <- "3"
  table[table=="bloga"] <- "2"
  table[table=="labai bloga"] <- "1"
  return(table)
}

##Funkcija paskaičiuoti maksimalius krūvius vandens telkiniams
max_load <- function(param, wb_type, flow, lakes_type) { 
  if (param == "NO3.N"){
    if (wb_type == "U"){
      r <- 2.3 * flow * 31557.6
    } else if (wb_type == "E") {
      r <- NA
    }
  } else if (param == "PO4.P"){
    if (wb_type == "U"){
      r <- 0.09 * flow * 31557.6
    } else if (wb_type == "E") {
      r <- NA
    }
  } else if (param == "N.total"){
    if (wb_type == "U"){
      r <- 3.0 * flow * 31557.6
    } else if (wb_type == "E") {
      r <- 2.0 * flow * 31557.6
    }
  } else if (param == "P.total"){
    if (wb_type == "U"){
      r <- 0.14 * flow * 31557.6
    } else if (wb_type == "E") {
      if (!is.na(lakes_type) && lakes_type == 1){
        r <- 0.06 * flow * 31557.6
      } else {
        r <- 0.05 * flow * 31557.6
      }
    }
  }
  return(round(r, 0))
}

##Funkcija lentelėms pateikti
create_dt <- function(x,y=""){
  DT::datatable(x,
                extensions = 'Buttons',
                options = list(initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'font-size': '80%'});",
                  "}"),
                  dom = 'Blfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                  lengthMenu = list(c(10,25,50,-1),
                                    c(10,25,50,"All"))), caption = y, height = "100%")
}

##Funkcija sujungti vandens telkinių GIS duomenis ir analizės rezultatus
gis_data_prep <- function(sf_df, param){
  sf_df <- sf_df %>% 
    inner_join(reductions_by_sources, by = c("wb_code" = "VT kodas")) %>% 
    filter(!is.na(paste0(param, "_total", sep = "")))  %>% 
    filter(eval(as.name(paste0(param, "_total", sep = ""))) < 0) %>% 
    select(wb_code, Pavadinimas, source, dist_available, starts_with(paste(param,"_", sep = "")))
  sf_df$quartile <- ntile(sf_df[[paste0(param, "_total")]], 4) ##Paskaičiuoja kvartilius
  return(sf_df)
}

##Funkcija paruošti sumažimų žemėlapius
report_maps <- function(param){
  rivers <- gis_data_prep(wb_rivers_sf, param)
  lakes <- gis_data_prep(wb_lakes_sf, param)
  gc <- rivers %>% 
    leaflet() %>% 
    addTiles (group = "OSM")%>% 
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%  
    addProviderTiles(providers$Esri.WorldImagery, group = "ESRI ortofoto") %>% 
    addPolylines(color = "red", weight = 4/rivers$quartile, opacity = 1,
                 popup = ~paste("Vandens telkinio pavadinimas:", rivers$Pavadinimas, "<br>",
                                "Vandens telkinio kodas:", rivers$wb_code, "<br>",
                                "Reikalingas bendras sumažinimas:", rivers[[paste0(param, "_total")]], "kg", "<br>",
                                "Sumažinimas iš žemės ūkio:", rivers[[paste0(param, "_agri")]], "kg", "<br>",
                                "Sumažinimas iš sutelktųjų šaltinių:",rivers[[paste0(param, "_point")]], "kg", "<br>",
                                "Sumažinimas iš miesto pasklidųjų šaltinių:", rivers[[paste0(param, "_urban_diffuse")]], "kg", "<br>",
                                "Duomenų šaltinis:", rivers$source, "<br>",
                                "Prieimami paskirtymo duomenys:", rivers$dist_available, "<br>"),
                 label = ~paste(rivers$Pavadinimas), group = "Upės")
  if (param %in% c("N", "P")){
    gc <- gc %>% 
      addPolygons(data = lakes, color = "red", weight = 4/lakes$quartile, opacity = 1, 
                  popup = ~paste("Vandens telkinio pavadinimas:", lakes$Pavadinimas, "<br>",
                                 "Vandens telkinio kodas:", lakes$wb_code, "<br>",
                                 "Reikalingas bendras sumažinimas:", lakes[[paste0(param, "_total")]], "kg", "<br>",
                                 "Sumažinimas iš žemės ūkio:", lakes[[paste0(param, "_agri")]], "kg", "<br>",
                                 "Sumažinimas iš sutelktųjų šaltinių:",lakes[[paste0(param, "_point")]], "kg", "<br>",
                                 "Sumažinimas iš miesto pasklidųjų šaltinių:", lakes[[paste0(param, "_urban_diffuse")]], "kg", "<br>",
                                 "Duomenų šaltinis:", lakes$source, "<br>",
                                 "Prieimami paskirtymo duomenys:", lakes$dist_available, "<br>"),
                  label = ~paste(lakes$Pavadinimas), group = "Ežerai / tvenkiniai") %>% 
      addLayersControl(
        baseGroups = c("CartoDB (numatytas)", "OSM", "ESRI ortofoto"),
        overlayGroups = c("Upės", "Ežerai / tvenkiniai"), 
        options = layersControlOptions(collapsed = TRUE))
  } else {
    gc <- gc %>% 
      addLayersControl(
        baseGroups = c("CartoDB (numatytas)", "OSM", "ESRI ortofoto"),
        options = layersControlOptions(collapsed = TRUE))
  }
  gc
}