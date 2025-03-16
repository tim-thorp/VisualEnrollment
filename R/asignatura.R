
asignatura_posicio <- function(posicio){
  ret <- NULL
  tryCatch(
    expr = {

      # JULIA 18/12/2022
      if (is.null(qQ)) return(NULL)

      #print("asignaturaPosicio")
      dist=sqrt((posicio$x-qQ$V1)^2+(posicio$y-qQ$V2)^2)
      dmin = min(dist)
      
      # JULIA 18/12/2022 canviar threshold i afegir else 
      if (dmin < 0.5) {
        proper=which.min(dist)
        ret <- qQ[proper,]
      } else {
        return(NULL)
      }
    },
    error = function(e){ 
      message("Error in asignaturaPosicio:")
      message(e)
      return(NULL)
    }
  )
  return(ret)
}

asignatura_tipologia <- function(lang, dadesGrau, assignatura) {
  tryCatch(
    expr = {
      tipologia_df <- dadesGrau$tipologia
      join <- assignatura %>% left_join(tipologia_df, by = c("type", "path"))
      if(nrow(join)!=1) return("")
      res <- join %>% .[[paste0("tipologia_",lang)]]
      #print("asignatura_tipologia")
      #print(res)
      return(res)
    },
    error = function(e){
      message("Error in asignaturaTipologia:")
      message(e)
      return("")
    }
  )
}

asignatura_quines <- function(lang, dadesGrau, q, input) {
  tipologia_df <- dadesGrau$tipologia
  selected <- tipologia_df %>% 
    mutate(tipologia = .[[paste0("tipologia_", lang)]]) %>% 
    filter(tipologia == input$tipologia) %>% 
    transmute(type, path)

  # básicas, troncales, etc. (no itinerarios que pueden ser compuestos)
  # JULIÀ 02/11/2023 falta separar "Totes" que no devuelve nada!!!
  if (("0" %in% selected$path) | (length(selected$path)==0)) {
    #q <- q %>%
    #  mutate(
    #    quines = (q$type %in% selected$type)
    #  )
    q$quines <- q$type %in% selected$type
  } else {
    # JULIA 31/10/23 itinerarios compuestos
    #selected_ass <-  semi_join(q, selected) %>% pull(ass)
    selected_ass <- q[str_detect(q$path,selected$path),'ass']
    
    #print("selected_ass")
    #print(selected_ass)
    #q <- q %>%
    #  mutate(
    #    quines = (ass %in% selected_ass)
    #  )
    q$quines <- q$ass %in% selected_ass
  }
  return(q)
}


asignatura_buscar <- function(lang, dadesGrau, q, input) {
  noms_df <- dadesGrau$noms
  #print("asignatura_buscar")
  #print(noms_df)
  selected_ass <- noms_df %>% 
    mutate(name = .[[paste0("name_", lang)]]) %>% 
    filter(str_detect(name, input$buscar)) %>% 
    pull(ass)
  #print("selected_ass")
  #print(selected_ass)
  q <- q %>%
    mutate(
      quines = ass %in% selected_ass
    )
  return(q)
}

asignatura_hover <- function(input, hovered) {
  div(
    class = "graf_hover_info_tooltip",
    # JULIA 03/01/2023 situar el tooltip, no funciona del tot bé !!!
    style = "position: absolute; z-index: 100; width: 200px; left: 900px; top: 0px",
    # JULIA 30/12/2022 añadir el nombre completo
    p(hovered$full_name),
    p(hovered$tipologia),
    # JULIÀ 07/11/2024 quitar semestre
    #p(paste0("Semestre: ", hovered$asem)),
    p(paste0("Codi: ", hovered$ass)),
    p(hovered$nota),
    # JULIA 07/01/2023 mostrar la posición del elemento !!!
    #p(paste0(round(input$graf_hover$coords_img$x),", ",round(input$graf_hover$coords_img$y))),
    tags$script(
      paste0(
        "graf_hover(",
        input$graf_hover$coords_img$x,
        ",",
        input$graf_hover$coords_img$y,
        ")"
      )
    )
  )
}


asignatura_nom <- function(lang, dadesGrau, hovered) {
  res <- dadesGrau$noms %>% filter(str_detect(ass, paste0("^",hovered$ass,"$"))) %>% .[[paste0("name_", lang)]]
  return(res)
}


inici_semestres <- function() {
  data <- tibble(
    day  = seq(as.Date(lubridate::today()),as.Date(lubridate::today() + 365), "day"),
  ) %>% 
    mutate(
      wday = lubridate::wday(day),
      month = lubridate::month(day),
      sem1 = lubridate::month(day) == 9,
      sem2 = lubridate::month(day) == 2
    )
  tercer_miercoles <- data %>% 
    filter(wday == 4) %>%
    group_by(year_month = lubridate::floor_date(day, unit = 'month')) %>%
    slice(3L)
  list(
    sem1 = tercer_miercoles %>% filter(sem1) %>% top_n(1) %>% pull(day),
    sem2 = tercer_miercoles %>% filter(sem2)%>% top_n(1) %>% pull(day)
  )
}



numero_asignatures <- function(list) {
  i <- 0
  if(!is.na(list$ass[1])) i <- i + 1
  if(!is.na(list$ass[2])) i <- i + 1
  if(!is.na(list$ass[3])) i <- i + 1
  if(!is.na(list$ass[4])) i <- i + 1
  if(!is.na(list$ass[5])) i <- i + 1
  if(!is.na(list$ass[6])) i <- i + 1
  #print("asignatura 6")
  #print(list$ass[6])
  return(i)
}
