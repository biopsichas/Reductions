library(DT)
library(ggplot2)
library(plotly)
library(stringr)
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
create_dt <- function(x, param){
  x <- x %>% 
    filter(!is.na(paste0(param, "_total")))  %>% 
    filter(eval(as.name(paste0(param, "_total"))) < 0) %>% 
    select(Pavadinimas, `VT kodas`, tipas, source, dist_available, starts_with(paste0(param,"_"))) %>% 
    arrange(Pavadinimas) %>% 
    rename(Tipas = tipas,
           `Saltinis` = source,
           `Tarsos paskirtymas` = dist_available) %>% 
    rename_at(vars(ends_with("_total")), ~paste0(word(.,1,sep = "_")," bendras")) %>% 
    rename_at(vars(ends_with("_agri")), ~paste0(word(.,1,sep = "_")," zemes ukis")) %>% 
    rename_at(vars(ends_with("_point")), ~paste0(word(.,1,sep = "_")," nuotekos")) %>% 
    rename_at(vars(ends_with("_urban_diffuse")), ~paste0(word(.,1,sep = "_")," miesto pasklidoji"))
  
  DT::datatable(x,
                extensions = 'Buttons',
                options = list(initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'font-size': '80%'});",
                  "}"),
                  dom = 'Blfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                  lengthMenu = list(c(10,25,50,-1),
                                    c(10,25,50,"All")),
                  columnDefs = list(list(className = 'dt-center', targets="_all"))), 
                caption = paste("Vienetai lenteleje yra", substr(param, 1, 1), "kg per metus"), 
                height = "100%")
}

##Funkcija sujungti vandens telkinių GIS duomenis ir analizės rezultatus
gis_data_prep <- function(sf_df, param){
  sf_df <- sf_df %>% 
    inner_join(reductions_by_sources, by = c("wb_code" = "VT kodas")) %>% 
    filter(!is.na(paste0(param, "_total")))  %>% 
    filter(eval(as.name(paste0(param, "_total"))) < 0) %>% 
    select(wb_code, Pavadinimas, source, dist_available, starts_with(paste0(param,"_")))
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
                                "Reikalingas bendras sumazinimas:", rivers[[paste0(param, "_total")]], "kg", "<br>",
                                "Is zemes ukio:", rivers[[paste0(param, "_agri")]], "kg", "<br>",
                                "Is sutelktuju saltiniu:",rivers[[paste0(param, "_point")]], "kg", "<br>",
                                "Is miesto paskliduju saltiniu:", rivers[[paste0(param, "_urban_diffuse")]], "kg", "<br>",
                                "Duomenu saltinis:", rivers$source, "<br>",
                                "Apkrovu pasiskirtymo duomenys:", rivers$dist_available, "<br>"),
                 label = ~paste(rivers$Pavadinimas), group = "Upes")
  if (param %in% c("N", "P")){
    gc <- gc %>% 
      addPolygons(data = lakes, color = "red", weight = 4/lakes$quartile, opacity = 1, 
                  popup = ~paste("Vandens telkinio pavadinimas:", lakes$Pavadinimas, "<br>",
                                 "Vandens telkinio kodas:", lakes$wb_code, "<br>",
                                 "Reikalingas bendras sumazinimas:", lakes[[paste0(param, "_total")]], "kg", "<br>",
                                 "Is zemes ukio:", lakes[[paste0(param, "_agri")]], "kg", "<br>",
                                 "Is sutelktuju saltiniu:",lakes[[paste0(param, "_point")]], "kg", "<br>",
                                 "Is miesto paskliduju saltiniu:", lakes[[paste0(param, "_urban_diffuse")]], "kg", "<br>",
                                 "Duomenu saltinis:", lakes$source, "<br>",
                                 "Apkrovu pasiskirtymo duomenys:", lakes$dist_available, "<br>"),
                  label = ~paste(lakes$Pavadinimas), group = "Ezerai / tvenkiniai") %>% 
      addLayersControl(
        baseGroups = c("CartoDB (numatytas)", "OSM", "ESRI ortofoto"),
        overlayGroups = c("Upes", "Ezerai / tvenkiniai"), 
        options = layersControlOptions(collapsed = TRUE))
  } else {
    gc <- gc %>% 
      addLayersControl(
        baseGroups = c("CartoDB (numatytas)", "OSM", "ESRI ortofoto"),
        options = layersControlOptions(collapsed = TRUE))
  }
  gc
}

##Funkcija paskaičiuoti procentų indikatorius
calc_proc_gauge <- function(param){ 
  with_reductions <- reductions_by_sources %>% 
    select(starts_with(paste0(param,"_", sep = ""))) %>% 
    filter(!is.na(paste0(param, "_total")))  %>% 
    filter(eval(as.name(paste0(param, "_total"))) < 0) %>% 
    count()
  if (param %in% c("NO3", "PO4")){
    total_numbers <- wb_names %>%
      filter(tipas == "U") %>%
      count()
  } else {
    total_numbers <- wb_names %>%
      count()
  }
  rate <- as.integer(round((with_reductions/total_numbers) * 100, 0))
  gauge(rate, min = 0, max = 100, symbol = '%', gaugeSectors(
    success = c(0, 10), warning = c(11, 20), danger = c(21, 100)
  ))
}

##Funkcija histogramoms paruošti
plot_hist <- function(param){ 
  df <- reductions_by_sources %>% 
    select(starts_with(paste0(param,"_total"))) %>% 
    filter(!is.na(paste0(param, "_total")))  %>% 
    filter(eval(as.name(paste0(param, "_total"))) < 0) %>% 
    mutate(r = .[[1]] * -1)
  graph <- ggplot(df, aes(x = r)) + 
    geom_histogram(fill = 'skyblue', color = 'grey30', bins = 20) + 
    scale_x_log10() +
    labs(x = "Sumazinimas kg", y = "VT skaicius") +
    theme_bw() 
  fig <- ggplotly(graph, tooltip = "text") %>% layout(
    margin = list(l = 5, r = 0, b = 0, t = 0, pad = 0))
  fig
}

##Funkcija skrituliams paruošti
plot_pie <- function(param){ 
  df <- reductions_by_sources %>% 
    select(starts_with(paste0(param,"_"))) %>% 
    filter(!is.na(paste0(param, "_total")))  %>% 
    filter(eval(as.name(paste0(param, "_total"))) < 0) %>% 
    mutate_at(c(1:4), ~. * -1) %>% 
    colSums()
  df <- as.data.frame(df, ) %>% 
    rename(value = df) %>% 
    slice(-1) %>% 
    mutate(proc = (round(value / sum(value), 2) * 100))
  df$names <- c("Zemes ukis", "Nuotekos", "Miesto pasklidoji")
  colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)')
  fig <- plot_ly(df, labels = ~names, values = ~proc, type = 'pie', textposition = 'inside', 
                 textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
                 marker = list(colors = colors,
                               line = list(color = '#FFFFFF', width = 1)),
                 showlegend = FALSE)
  fig <- fig %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0))
  fig
}