find_closest_subject <- function(click_position){
  closest_subject <- NULL
  tryCatch(
    expr = {

      # JULIA 18/12/2022
      if (is.null(subject_positions)) return(NULL)

      #print("asignaturaPosicio")
      distances=sqrt((click_position$x-subject_positions$x)^2+(click_position$y-subject_positions$y)^2)
      min_distance = min(distances)
      
      # JULIA 18/12/2022 canviar threshold i afegir else 
      if (min_distance < 0.5) {
        closest_subject_index=which.min(distances)
        closest_subject <- subject_positions[closest_subject_index,]
      } else {
        return(NULL)
      }
    },
    error = function(e){ 
      message("Error in find_closest_subject:")
      message(e)
      return(NULL)
    }
  )
  return(closest_subject)
}

get_subject_type <- function(language, degree_data, subject_data) {
  tryCatch(
    expr = {
      # Check if the subject is elective (Type P)
      if (subject_data$type == "P") {
        return(translate(language, "Elective")) # Return generic Elective label
      }
      # Otherwise, get the specific type from the typology data
      subject_type_df <- degree_data$subject_type
      join <- subject_data %>% left_join(subject_type_df, by = c("type", "path"))
      if(nrow(join)!=1) return("")
      subject_type_name <- join %>% .[[paste0("tipologia_",language)]]
      return(subject_type_name)
    },
    error = function(e){
      message("Error in get_subject_type:")
      message(e)
      return("")
    }
  )
}

filter_subjects_by_type <- function(language, degree_data, subjects_df, input) {
  subject_type_df <- degree_data$subject_type
  selected <- subject_type_df %>% 
    mutate(subject_type = .[[paste0("tipologia_", language)]]) %>% 
    filter(subject_type == input$subject_type) %>% 
    transmute(type, path)

  # básicas, troncales, etc. (no itinerarios que pueden ser compuestos)
  # JULIÀ 02/11/2023 falta separar "Totes" que no devuelve nada!!!
  if (("0" %in% selected$path) | (length(selected$path)==0)) {
    subjects_df$selected_subjects <- subjects_df$type %in% selected$type
  } else {
    # JULIA 31/10/23 itinerarios compuestos
    selected_subject <- subjects_df[str_detect(subjects_df$path,selected$path),'subject_code']
    
    subjects_df$selected_subjects <- subjects_df$subject_code %in% selected_subject
  }
  return(subjects_df)
}

search_subjects <- function(language, degree_data, subjects_df, input) {
  subject_names_df <- degree_data$subject_names
  selected_subjects <- subject_names_df %>% 
    mutate(name = .[[paste0("name_", language)]]) %>% 
    filter(str_detect(name, input$search_text)) %>% 
    pull(subject_code)
  subjects_df <- subjects_df %>%
    mutate(selected_subjects = subject_code %in% selected_subjects)
  return(subjects_df)
}

get_subject_name <- function(language, degree_data, hovered) {
  subject_name <- degree_data$subject_names %>% 
    filter(str_detect(subject_code, paste0("^",hovered$subject_code,"$"))) %>% 
    .[[paste0("name_", language)]]
  return(subject_name)
}


get_semester_start_dates <- function() {
  data <- tibble(
    day  = seq(as.Date(lubridate::today()),as.Date(lubridate::today() + 365), "day"),
  ) %>% 
    mutate(
      wday = lubridate::wday(day),
      month = lubridate::month(day),
      first_semester_start = lubridate::month(day) == 9,
      second_semester_start = lubridate::month(day) == 2
    )
  third_wednesday <- data %>% 
    filter(wday == 4) %>%
    group_by(year_month = lubridate::floor_date(day, unit = 'month')) %>%
    slice(3L)
  list(
    first_semester_start = third_wednesday %>% filter(first_semester_start) %>% slice_max(day, n = 1) %>% pull(day),
    second_semester_start = third_wednesday %>% filter(second_semester_start) %>% slice_max(day, n = 1) %>% pull(day)
  )
}



count_selected_subjects <- function(list) {
  i <- 0
  if(!is.na(list$subject_code[1])) i <- i + 1
  if(!is.na(list$subject_code[2])) i <- i + 1
  if(!is.na(list$subject_code[3])) i <- i + 1
  if(!is.na(list$subject_code[4])) i <- i + 1
  if(!is.na(list$subject_code[5])) i <- i + 1
  if(!is.na(list$subject_code[6])) i <- i + 1
  return(i)
}
