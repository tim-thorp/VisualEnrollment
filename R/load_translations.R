load_translations <- function() {

  # Define the total number of files and initialize the progress bar
  total_files_to_load <- 2
  message("Loading translation files:")
  pb <- utils::txtProgressBar(min = 0, max = total_files_to_load, style = 3, width = 50, char = "=")
  current_file_count <- 0

  # Load translation_ca.csv
  ca <- readr::read_delim(system.file("lang", "translation_ca.csv", package = "visualenrollment"),
                          delim = ";", col_types = readr::cols(.default = "c"))
  current_file_count <- current_file_count + 1
  utils::setTxtProgressBar(pb, current_file_count)

  # Load translation_es.csv
  es <- readr::read_delim(system.file("lang", "translation_es.csv", package = "visualenrollment"),
                          delim = ";", col_types = readr::cols(.default = "c"))
  current_file_count <- current_file_count + 1
  utils::setTxtProgressBar(pb, current_file_count)

  # Join the two data frames by the "en" column
  translations <- dplyr::inner_join(ca, es, by = "en")

  # Close the progress bar
  close(pb)

  # Return the translations
  return (translations)
}
