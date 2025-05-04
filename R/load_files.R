load_files <- function() {

  files <- list()

  # Define the total number of files and initialize the progress bar
  total_files_to_load <- 13
  message("Loading data files:")
  pb <- utils::txtProgressBar(min = 0, max = total_files_to_load, style = 3, width = 50, char = "=")
  current_file_count <- 0

  ## recomanacions_INFORMATICA.csv
  files$recomanacions_INFORMATICA <- readr::read_delim(
    system.file("data_files", "recomanacions_INFORMATICA.csv", package = "visualenrollment"),
    delim = ";",
    col_names = FALSE,
    col_types = readr::cols("c", "c", "c", "d", "d", "d", "d", "d", "d", "d", "c", "c")
  )
  # Rename columns to be more descriptive
  colnames(files$recomanacions_INFORMATICA) <- c('user_id', 'program_code', 'academic_year', 'relative_semester', 
                         'total_credits', 'total_subjects_enrolled_previous', 'total_subjects_passed_previous', 
                         'subjects_enrolled_current_sem', 'subjects_passed_current_sem', 
                         'total_credits_current', 'subject_code', 'subject_mark')
  current_file_count <- current_file_count + 1
  utils::setTxtProgressBar(pb, current_file_count)

  # Solapaments i semestres
  files$solap1_INFORMATICA <- readr::read_delim(
    system.file("data_files", "solap1_INFORMATICA.csv", package = "visualenrollment"),
    delim = ";",
    col_names = TRUE,
    col_types = readr::cols(subject_code = "c")
  )
  current_file_count <- current_file_count + 1
  utils::setTxtProgressBar(pb, current_file_count)

  files$solap2_INFORMATICA <- readr::read_delim(
    system.file("data_files", "solap2_INFORMATICA.csv", package = "visualenrollment"),
    delim = ";",
    col_names = TRUE,
    col_types = readr::cols(subject_code = "c")
  )
  current_file_count <- current_file_count + 1
  utils::setTxtProgressBar(pb, current_file_count)

  ## tipologia_INFORMATICA.csv
  files$tipologia_INFORMATICA <- readr::read_delim(
    system.file("data_files", "tipologia_INFORMATICA.csv", package = "visualenrollment"),
    delim = ";",
    col_types = readr::cols(.default = "c")
  )
  current_file_count <- current_file_count + 1
  utils::setTxtProgressBar(pb, current_file_count)

  ## subjects_INFORMATICA.csv
  files$subjects_INFORMATICA <- readr::read_delim(
    system.file("data_files", "subjects_INFORMATICA.csv", package = "visualenrollment"),
    delim = ";",
    col_names = TRUE,
    col_types = readr::cols(
      subject_code = "c",
      absolute_semester = "d",
      semester_number = "c",
      type = "c",
      path = "c",
      credits = "d",
      name_en = "c",
      name_es = "c",
      name_ca = "c",
      abbreviation_en = "c",
      abbreviation_es = "c",
      abbreviation_ca = "c"
    )
  )
  current_file_count <- current_file_count + 1
  utils::setTxtProgressBar(pb, current_file_count)

  # JULIA 23/12/2022 leer matrices ya calculadas
  files$Dso1_INFORMATICA <- readr::read_delim(
    system.file("data_files", "Dso1_INFORMATICA.csv", package = "visualenrollment"),
    delim = ";",
    col_names = TRUE,
    col_types = readr::cols("d", "d", "d")
  )
  # Rename columns to be more descriptive
  colnames(files$Dso1_INFORMATICA) <- c("subject_code_1", "subject_code_2", "overlap_data")
  current_file_count <- current_file_count + 1
  utils::setTxtProgressBar(pb, current_file_count)

  files$Dso2_INFORMATICA <- readr::read_delim(
    system.file("data_files", "Dso2_INFORMATICA.csv", package = "visualenrollment"),
    delim = ";",
    col_names = TRUE,
    col_types = readr::cols("d", "d", "d")
  )
  # Rename columns to be more descriptive
  colnames(files$Dso2_INFORMATICA) <- c("subject_code_1", "subject_code_2", "overlap_data")
  current_file_count <- current_file_count + 1
  utils::setTxtProgressBar(pb, current_file_count)

  # JULIA 15/01/2024 load AEPs file
  files$aeps_INFORMATICA <- readr::read_delim(
    system.file("data_files", "aeps_INFORMATICA.csv", package = "visualenrollment"),
    delim = ";",
    col_names = FALSE,
    col_types = readr::cols("c", "c", "d", "c")
  )
  # Rename columns to be more descriptive
  colnames(files$aeps_INFORMATICA) <- c('user_id', 'subject_code', 'credits', 'status')
  current_file_count <- current_file_count + 1
  utils::setTxtProgressBar(pb, current_file_count)

  files$Dpop_INFORMATICA <- readr::read_delim(
    system.file("data_files", "Dpop_INFORMATICA.csv", package = "visualenrollment"),
    delim = ";",
    col_names = TRUE,
    col_types = readr::cols("d", "d", "d")
  )
  # Rename columns to be more descriptive
  colnames(files$Dpop_INFORMATICA) <- c("subject_code_1", "subject_code_2", "popularity_score")
  current_file_count <- current_file_count + 1
  utils::setTxtProgressBar(pb, current_file_count)

  files$Ddif_INFORMATICA <- readr::read_delim(
    system.file("data_files", "Ddif_INFORMATICA.csv", package = "visualenrollment"),
    delim = ";",
    col_names = TRUE,
    col_types = readr::cols("d", "d", "d")
  )
  # Rename columns to be more descriptive
  colnames(files$Ddif_INFORMATICA) <- c("subject_code_1", "subject_code_2", "difficulty_score")
  current_file_count <- current_file_count + 1
  utils::setTxtProgressBar(pb, current_file_count)

  files$Dabs_INFORMATICA <- readr::read_delim(
    system.file("data_files", "Dabs_INFORMATICA.csv", package = "visualenrollment"),
    delim = ";",
    col_names = TRUE,
    col_types = readr::cols("d", "d", "d")
  )
  # Rename columns to be more descriptive
  colnames(files$Dabs_INFORMATICA) <- c("subject_code_1", "subject_code_2", "semester_distance")
  current_file_count <- current_file_count + 1
  utils::setTxtProgressBar(pb, current_file_count)

  # Load prerequisites file
  files$prerequisites_INFORMATICA <- readr::read_delim(
    system.file("data_files", "prerequisites_INFORMATICA.csv", package = "visualenrollment"),
    delim = ";",
    col_names = TRUE,
    col_types = readr::cols("c", "c")
  )
  # Rename columns to be more descriptive
  colnames(files$prerequisites_INFORMATICA) <- c("prerequisite_code", "subject_code")
  current_file_count <- current_file_count + 1
  utils::setTxtProgressBar(pb, current_file_count)

  # Load restrictions file
  files$restrictions_INFORMATICA <- readr::read_delim(
    system.file("data_files", "restrictions_INFORMATICA.csv", package = "visualenrollment"),
    delim = ";",
    col_names = TRUE,
    col_types = readr::cols("c", "d", "c")
  )
  # Rename columns to be more descriptive
  colnames(files$restrictions_INFORMATICA) <- c("restricted_subject_code", "min_ects", "antirequisites")
  current_file_count <- current_file_count + 1
  utils::setTxtProgressBar(pb, current_file_count)

  # Close the progress bar
  close(pb)

  return(files)
}
