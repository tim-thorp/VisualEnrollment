load_files <- function(){
  
  files <- list()
  
  ## recomanacions_INFORMATICA.csv
  # JULIA 24/12/2022 cli_progress_step no parece hacer nada
  #cli::cli_progress_step("Load recomanacions_INFORMATICA.tsv")
  # JULIA 13/10/2023 nuevo formato de datos
  files$recomanacions_INFORMATICA <- read.table(system.file("data_files", "recomanacions_INFORMATICA.csv", 
                                                            package = "visualenrollment"), 
                  header=F, sep=";", 
                  colClasses=c("character","character","character",
                               "numeric","numeric","numeric",
                               "numeric","numeric","numeric",
                               "numeric","character","character"))


  # Solapaments i semestres
  #cli::cli_progress_step("Load solap1_INFORMATICA.csv")
  files$solap1_INFORMATICA <- read.table(system.file("data_files", "solap1_INFORMATICA.csv", 
                                                     package = "visualenrollment"), 
                       header = TRUE, sep = ";", 
                       colClasses = c("subject_code" = "character"))
  
  #cli::cli_progress_step("Load solap2_INFORMATICA.csv")
  files$solap2_INFORMATICA <- read.table(system.file("data_files", "solap2_INFORMATICA.csv", 
                                                     package = "visualenrollment"), 
                       header=TRUE, sep = ";", 
                       colClasses = c("subject_code" = "character"))
  
  ## tipologia_INFORMATICA.csv
  #cli::cli_progress_step("Load tipologia_INFORMATICA.csv")
  files$tipologia_INFORMATICA <- readr::read_csv(system.file("data_files", "tipologia_INFORMATICA.csv", 
                                                      package = "visualenrollment"), 
                                          col_types = readr::cols(.default = "c"))
  
  ## noms_INFORMATICA.csv
  #cli::cli_progress_step("Load noms_INFORMATICA.csv")
  files$noms_INFORMATICA <-  readr::read_csv(system.file("data_files", "noms_INFORMATICA.csv", 
                                                 package = "visualenrollment"), 
                                      col_types = readr::cols(.default = "c"))
  
  ## assignatures_INFORMATICA.csv
  #cli::cli_progress_step("Load assignatures_INFORMATICA.csv")
  files$assignatures_INFORMATICA <- read.table(system.file("data_files", "assignatures_INFORMATICA.csv", 
                                                           package = "visualenrollment"),
                                          header=TRUE,
                                          sep=";",
                                          colClasses=c("character", "numeric",
                                                       "character", "character", 
                                                       "character","character","character"))
  
  # JULIA 23/12/2022 leer matrices ya calculadas
  #cli::cli_progress_step("Load Dso1_INFORMATICA.csv")
  files$Dso1_INFORMATICA <- read.table(system.file("data_files", "Dso1_INFORMATICA.csv", 
                                                           package = "visualenrollment"),
                                          header=TRUE,
                                          sep=";",
                                          colClasses=c("numeric", "numeric", "numeric")) 

  #cli::cli_progress_step("Load Dso2_INFORMATICA.csv")
  files$Dso2_INFORMATICA <- read.table(system.file("data_files", "Dso2_INFORMATICA.csv", 
                                                           package = "visualenrollment"),
                                          header=TRUE,
                                          sep=";",
                                          colClasses=c("numeric", "numeric", "numeric")) 


  #cli::cli_progress_step("Load Dpop_INFORMATICA.csv")
  files$Dpop_INFORMATICA <- read.table(system.file("data_files", "Dpop_INFORMATICA.csv", 
                                                           package = "visualenrollment"),
                                          header=TRUE,
                                          sep=";",
                                          colClasses=c("numeric", "numeric", "numeric")) 

  #cli::cli_progress_step("Load Ddif_INFORMATICA.csv")
  files$Ddif_INFORMATICA <- read.table(system.file("data_files", "Ddif_INFORMATICA.csv", 
                                                           package = "visualenrollment"),
                                          header=TRUE,
                                          sep=";",
                                          colClasses=c("numeric", "numeric", "numeric")) 

  #cli::cli_progress_step("Load Dabs_INFORMATICA.csv")
  files$Dabs_INFORMATICA <- read.table(system.file("data_files", "Dabs_INFORMATICA.csv", 
                                                           package = "visualenrollment"),
                                          header=TRUE,
                                          sep=";",
                                          colClasses=c("numeric", "numeric", "numeric")) 

  #cli::cli_progress_step("Load Dreq_INFORMATICA.csv")
  files$Dreq_INFORMATICA <- read.table(system.file("data_files", "Dreq_INFORMATICA.csv", 
                                                           package = "visualenrollment"),
                                          header=TRUE,
                                          sep=";",
                                          colClasses=c("numeric", "numeric", "numeric")) 

  return(files)
}
  