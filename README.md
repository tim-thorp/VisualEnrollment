# Visual Enrollment

A visual recommendation system to assist university students in making informed course enrollment decisions at UOC (Universitat Oberta de Catalunya).

## Project Overview

This Shiny application helps students:  

- Visualize their academic progress on an interactive map  
- Receive personalized course recommendations based on multiple factors  
- View potential schedule conflicts through calendar visualization  
- Plan their enrollment more effectively  

## Getting Started

### Prerequisites

- [R](https://www.r-project.org/) (v4.0.0 or higher)
- [RStudio](https://posit.co/download/rstudio-desktop/) (v1.4 or higher)
- [Git](https://git-scm.com/downloads)

### Cloning the Repository

1. Open your terminal or Git Bash
2. Navigate to the directory where you want to store the project
3. Clone the repository:
   ```
   git clone https://github.com/tim-thorp/VisualEnrollment.git
   ```

### Opening in RStudio

1. Launch RStudio
2. Click File > Open Project...
3. Navigate to the cloned repository folder
4. Select the `VisualEnrollment.Rproj` file
5. Click Open

### Installing Dependencies

This project uses [renv](https://rstudio.github.io/renv/) to manage package dependencies. When you open the project for the first time, you'll see a notification about package dependencies.

To install all required packages:

```r
renv::restore()
```

If you're new to renv, it handles project-specific package management, ensuring everyone uses the same package versions.

### GitHub Authentication

Some dependencies are installed from GitHub, which requires authentication. To set up:

1. [Create a Personal Access Token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token) on GitHub
2. Set up your token in R:
   ```r
   usethis::edit_r_environ()
   ```
3. Add this line to the .Renviron file that opens:
   ```
   GITHUB_PAT=your_token_here
   ```
4. Save and restart R

### Running the Application

To run the application:

```r
source("app.R")
```

Or click the "Run App" button in RStudio if you have app.R open.

## Project Structure

```
VisualEnrollment/  
├── app.R                            # Main application entry point  
├── DESCRIPTION                      # Package metadata and dependencies  
├── LICENSE                          # License information  
├── LICENSE.md                       # License information (markdown)  
├── NAMESPACE                        # Package namespace definitions  
├── README.md                        # This documentation file  
├── VisualEnrollment.Rproj           # RStudio project configuration  
├── renv.lock                        # Package dependency snapshot  
│  
├── R/                               # R code directory  
│   ├── 000_global.R                 # First file loaded, sets global variables  
│   ├── _disable_autoload.R          # Disables Shiny autoload functionality  
│   ├── app_config.R                 # Configuration helpers  
│   ├── asignatura.R                 # Functions for subjects and semesters  
│   ├── informaticaServer.R          # Server-side logic for computer science program  
│   ├── informaticaUI.R              # User interface for computer science program  
│   ├── load_files.R                 # Functions to load data files  
│   ├── load_translations.R          # Functions to load translation files  
│   ├── setup.R                      # Setup environment and packages  
│   ├── theme.R                      # UI styling with Fluent UI  
│   ├── translations.R               # Translation functionality  
│   ├── visualenrollmentApp.R        # Main Shiny app router/setup  
│   └── zzz.R                        # Last file loaded, library setup  
│  
├── inst/                            # Installation directory  
│   ├── data_files/                  # Data files directory  
│   │   ├── Dabs_INFORMATICA.csv     # Absolute semester distance matrix  
│   │   ├── Ddif_INFORMATICA.csv     # Subject difficulty distance matrix  
│   │   ├── Dpop_INFORMATICA.csv     # Popularity distance matrix  
│   │   ├── Dreq_INFORMATICA.csv     # Prerequisites distance matrix  
│   │   ├── Dso1_INFORMATICA.csv     # Semester 1 overlaps matrix  
│   │   ├── Dso2_INFORMATICA.csv     # Semester 2 overlaps matrix  
│   │   ├── aeps_INFORMATICA.csv     # Student academic record data (not included in repository)  
│   │   ├── assignatures_INFORMATICA.csv  # Subject information  
│   │   ├── idps_pilot               # Pilot student IDs  
│   │   ├── noms_INFORMATICA.csv     # Subject names in different languages  
│   │   ├── recomanacions_INFORMATICA.csv  # Recommendation data (not included in repository)  
│   │   ├── solap1_INFORMATICA.csv   # Semester 1 schedule overlap data  
│   │   ├── solap2_INFORMATICA.csv   # Semester 2 schedule overlap data  
│   │   └── tipologia_INFORMATICA.csv  # Subject typology information  
│   │  
│   ├── golem-config.yml             # Golem package configuration  
│   │  
│   ├── lang/                        # Language files directory  
│   │   ├── translation_ca.csv       # Catalan translations  
│   │   └── translation_es.csv       # Spanish translations  
│   │  
│   └── www/                         # Web assets directory  
│       ├── css/                     # CSS directory  
│       │   └── styles.css           # CSS styling  
│       ├── img/                     # Images directory  
│       │   └── logo.png             # Application logo  
│       └── js/                      # JavaScript directory  
│           └── scripts.js           # JavaScript functionality  
│  
└── man/                             # Documentation directory  
    └── visualenrollmentApp.Rd       # Documentation for main app function
```