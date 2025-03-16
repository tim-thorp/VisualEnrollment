
# uninstall <- setdiff(rownames(installed.packages()),c("stats","graphics","grDevices","datasets","utils","methods","base"))
# remove.packages(uninstall)

setup <- function() {
  print("Setting locales...")
  Sys.setlocale(category = "LC_ALL", locale = "es_ES.utf8")
  Sys.setlocale(category = "LC_TIME", locale = "es_ES.utf8")
  cran_packages = c("tidyverse", "shiny", "lubridate", "ggforce", "ggrepel", "shinyjs", "patchwork", "shinyscreenshot")
  github_packages = c("Appsilon/shiny.react", "Appsilon/shiny.fluent", "Appsilon/shiny.router")
  require_packages <- function(cran_packages, github_packages) {
    required_packages <- c("renv","devtools","usethis")
    required_from_cran <-setdiff(required_packages, rownames(installed.packages()))
    if (length(required_from_cran) > 0) {
      print(paste("Install required from CRAN:", required_from_cran))
      install.packages(required_from_cran, dependencies = TRUE)
    }
    # renv::restore(prompt = FALSE)
    from_cran <- setdiff(cran_packages, rownames(installed.packages()))
    if (length(from_cran) > 0) {
      print(paste("Install from CRAN:", from_cran))
      install.packages(from_cran, dependencies = TRUE)
    }
    lapply(cran_packages, require, character.only = TRUE)
    library(magrittr)
    github_tibble <- as_tibble(github_packages) %>% separate(value, into = c("subdir","ref"), sep = "/", remove = FALSE)
    from_github <- setdiff(github_tibble$ref, rownames(installed.packages()))
    if (length(from_github) > 0) {
      print(paste("Install from Github:", from_github))
      devtools::install_github(
        github_tibble %>%
          filter(ref %in% from_github) %>%
          pull(value),
        auth_token = Sys.getenv("GITHUB_PAT")
      )
    }
    lapply(github_tibble$ref, require, character.only = TRUE)
    renv::snapshot(prompt = FALSE)
  }
  #remove.packages(c("shiny.fluent","shiny.router"))
  require_packages(cran_packages, github_packages)
  tibble(locales = system("locale -a", intern = TRUE)) %>% filter(str_detect(locales, "es_ES"))
  Sys.setlocale(category = "LC_ALL", locale = "es_ES.utf8")
  Sys.setlocale(category = "LC_TIME", locale = "es_ES.utf8")
  # Set GITHUB_PAT on .Renviron with your personal github auth token
  #remove.packages("shiny.fluent")
  # renv::restore(prompt = FALSE)
  invisible(NULL)
}

run <- function(){
  visualenrollmentApp(options=list(host='0.0.0.0', port = 8080))
}
