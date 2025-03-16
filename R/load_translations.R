load_translations <- function() {
 
  #cli::cli_progress_step("Load translation_ca.csv")
  ca <- readr::read_csv(system.file("lang", "translation_ca.csv", package = "visualenrollment"), show_col_types = FALSE)
    
  #cli::cli_progress_step("Load translation_es.csv")
  es <- readr::read_csv(system.file("lang", "translation_es.csv", package = "visualenrollment"), show_col_types = FALSE)
  
  translations <- dplyr::inner_join(ca, es, by = "en")
  return (translations)
}

