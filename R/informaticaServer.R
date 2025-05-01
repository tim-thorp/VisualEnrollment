# Carga de los fichero de datos
data_files <- load_files()

# Carga de los ficheros de traduccion
translations <- load_translations()

subjectEnrollmentServer <- function(id, language) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Valors reactius --------------------------------------------------------
      
      enrollment_step <- reactiveValues(current_step=0)
      hovered_list <- reactiveValues(subject_code=character())
      clicked_list <- reactiveValues(subject_code=character())
      discarded_list <- reactiveValues(subject_code=character())
      recommended_list <- reactiveValues(subject_code=character())
      selected_list <- reactiveValues(subject_code=character())
      
      static_legend_labels_base <- c(
        "Pass", "Transfer", "Fail", "Not available", "Discarded", "Pending", 
        "R1", "R2", "R3", "R4", "R5", "R6",
        "Selected"
      )

      color_palette_base <- c(
        "#b9f6ff", # Pass
        "#E3FBFF", # Transfer
        "#ffc2b9", # Fail 
        "#cccccc", # Not available 
        "#e0e0e0", # Discarded
        "#f9f9f9", # Pending 
        "#22b33b", # R1
        "#4adb63", # R2
        "#8cff8c", # R3
        "#acffa3", # R4
        "#cdffc1", # R5
        "#e8ffe0", # R6
        "#4875fb"  # Selected
      )
      
      # JULIA 24/12/2022 parámetros de entrada
      # JULIÀ 24/10/2023 de moment ho desactivem
      #claveTutor <- reactive({
      #  if (is.null(get_query_param()$tutor)) {
      #    return("")
      #  }
      #  return(get_query_param()$tutor)
      #})
      # --- Helper Function ---
      format_subject_list <- function(subject_names, language) {
        n <- length(subject_names)
        if (n == 0) {
          return("")
        } else if (n == 1) {
          return(subject_names[1])
        } else if (n == 2) {
          # Use translation key for "and"
          and_conjunction <- translate(language, "and")
          return(paste(subject_names[1], and_conjunction, subject_names[2]))
        } else {
          # Format list like "A, B, C and D"
          and_conjunction <- translate(language, "and")
          # Join all but the last two with commas
          first_part <- paste(subject_names[1:(n-2)], collapse=", ")
          # Combine the parts: "A, B, C and D"
          return(paste0(first_part, ", ", subject_names[n-1], " ", and_conjunction, " ", subject_names[n]))
        }
      } 
      # --- End Helper Function ---
      
      degree_data <- reactive({
        if (is.null(input$degree)) {
          return(list(
            # JULIA 05/10/2024
            subjects_data = NULL,
            available_student_ids = NULL,
            final_project_code = NULL,
            subject_type = NULL,
            student_data = NULL,
            transferred_credits = NULL, 
            subject_names = NULL,
            subject_overlap_data = NULL,
            difficulty_distance_matrix = NULL,
            popularity_distance_matrix = NULL, 
            overlap_distance_matrix = NULL, 
            semester_distance_matrix = NULL,
            prerequisites_data = NULL,
            restrictions_data = NULL
          ))
        }
        
        # dades del grau seleccionat
        selected_degree = input$degree
        student_enrollment_data <- data_files[[paste0("recomanacions_", selected_degree)]]

        # JULIA 17/11/23        
        # llegir AEPs (todas)
        transferred_credits_data <- data_files[[paste0("aeps_", selected_degree)]]
        
        subject_type <- data_files[[paste0("tipologia_", selected_degree)]]
        subject_names <-  data_files[[paste0("noms_", selected_degree)]]
        
        # JULIA 27/12/2022 eliminar dadesASSsem
        subjects_data = data_files[[paste0("assignatures_", input$degree)]]        
        
        # Estudiants: filtrar per nombre de semestres i tope d'assignatures
        # matriculades
        # JULIA 23/12/2022 cambiar el filtro para cargar más estudiantes
        # JULIA 13/10/2023 cambiar para el nuevo formato
        available_student_ids=unique(student_enrollment_data$user_id)
        available_student_ids=c("---", available_student_ids)
        
        # Solapaments i semestres
        semester1_overlap <- data_files[[paste0("solap1_", selected_degree)]]
        semester2_overlap <- data_files[[paste0("solap2_", selected_degree)]]
        if (input$selected_semester==1) {
          subject_overlap_data <- semester1_overlap[semester1_overlap$subject_code %in% selected_list$subject_code,]
        } else {
          subject_overlap_data <- semester2_overlap[semester2_overlap$subject_code %in% selected_list$subject_code,]
        }    
        
        # JULIÀ 18/10/2023 preparar aquí les matrius
        
        # JULIA 23/12/2022 leer la matriz Dsol (Dso1 / Dso2)  
        raw_matrix_data <- data_files[[paste0("Dso1_", selected_degree)]]
        overlap_distance_matrix = matrix(raw_matrix_data$overlap_data, nrow=length(subjects_data$subject_code), ncol=length(subjects_data$subject_code), byrow=T)
        colnames(overlap_distance_matrix)=subjects_data$subject_code
        overlap_distance_matrix=overlap_distance_matrix/max(overlap_distance_matrix)
        raw_matrix_data=NULL
        
        # JULIA 23/12/2022 leer la matriz Dpop
        raw_matrix_data <- data_files[[paste0("Dpop_", selected_degree)]]
        popularity_distance_matrix = matrix(raw_matrix_data$popularity_score, nrow=length(subjects_data$subject_code), ncol=length(subjects_data$subject_code), byrow=T)
        colnames(popularity_distance_matrix)=subjects_data$subject_code
        popularity_distance_matrix=popularity_distance_matrix/max(popularity_distance_matrix)
        raw_matrix_data=NULL
        
        # JULIA 23/12/2022 leer la matriz Ddif
        raw_matrix_data <- data_files[[paste0("Ddif_", selected_degree)]]
        difficulty_distance_matrix = matrix(raw_matrix_data$difficulty_score, nrow=length(subjects_data$subject_code), ncol=length(subjects_data$subject_code), byrow=T)
        colnames(difficulty_distance_matrix)=subjects_data$subject_code
        difficulty_distance_matrix=difficulty_distance_matrix/max(difficulty_distance_matrix)
        raw_matrix_data=NULL
        
        # JULIA 27/12/2022 ordenación semestral 
        raw_matrix_data <- data_files[[paste0("Dabs_", selected_degree)]]        
        semester_distance_matrix = matrix(raw_matrix_data$semester_distance, nrow=length(subjects_data$subject_code), ncol=length(subjects_data$subject_code), byrow=T)
        colnames(semester_distance_matrix)=subjects_data$subject_code
        semester_distance_matrix=semester_distance_matrix/max(semester_distance_matrix)
        raw_matrix_data=NULL
        
        # Load prerequisites data for the new system
        prerequisites_data <- data_files[[paste0("prerequisites_", selected_degree)]]
        
        # Load restrictions data
        restrictions_data <- data_files[[paste0("restrictions_", selected_degree)]]
        
        # dadesEST
        student_data = NULL
        transferred_credits = NULL
        if (!is.null(input$idp) && input$idp!="---") {
          student_data=student_enrollment_data[student_enrollment_data$user_id==input$idp, c('relative_semester','subject_code','subject_mark')]
          
          # la última nota de cada asignatura
          student_data=aggregate(student_data[,c('relative_semester','subject_mark')],list(student_data$subject_code),function(x){return(x[length(x)])})
          colnames(student_data)=c('subject_code','relative_semester','subject_mark')
          
          # convalidades: és com una nota especial (només les incorporades!)
          #browser()
          transferred_credits=transferred_credits_data[transferred_credits_data$user_id==input$idp & transferred_credits_data$status=="Reconeguda",c('subject_code','status')]
          colnames(transferred_credits)=c('subject_code','subject_mark')
          
          # si té convalidades
          if (nrow(transferred_credits)>0) {
            
            transferred_credits$relative_semester="N/A"
            transferred_credits=transferred_credits[,match(names(student_data),names(transferred_credits))]
            
            # afegir les convalidades a les ja matriculades
            student_data=rbind(transferred_credits,student_data)
          } else {
            transferred_credits=NULL
          }
          # JULIA 29/08/2023 mantener el semestre absoluto
          #student_data$sem=NULL
        }
        
        # Asignatura TFM es la que té tipus T
        final_project_code <- subjects_data %>% filter(type == "T") %>% pull(subject_code) 
        
        # Agrupem el resultat
        return (list(
          # JULIA 05/10/2024
          available_student_ids = available_student_ids,
          final_project_code = final_project_code,
          subject_type = subject_type,
          subjects_data = subjects_data,
          student_data = student_data,
          transferred_credits=transferred_credits,
          subject_names = subject_names,
          subject_overlap_data = subject_overlap_data,
          difficulty_distance_matrix = difficulty_distance_matrix, 
          popularity_distance_matrix = popularity_distance_matrix, 
          overlap_distance_matrix = overlap_distance_matrix, 
          semester_distance_matrix = semester_distance_matrix,
          prerequisites_data = prerequisites_data,
          restrictions_data = restrictions_data
        ))
      })  
      
      output$graf_hover_info <- renderUI({
        #print("graf_hover_info")
        hovered <- NULL
        if(!is.null(input$graf_hover)) hovered <- find_closest_subject(input$graf_hover)
        if(is.null(hovered)) return (NULL)
        hovered_list$subject_code <- hovered$subject_code
        # print("hovered")
        # print(hovered)
        degree_data <- degree_data()
        # Ensure subject_positions exists and is up-to-date
        req(exists("subject_positions"))

        hovered_code <- hovered$subject_code
        hovered$subject_type <- get_subject_type(language, degree_data, hovered)
        hovered$name <- get_subject_name(language, degree_data, hovered)

        # --- Get Credits Info ---
        subject_credits <- degree_data$subjects_data %>%
          filter(subject_code == hovered_code) %>%
          pull(credits) %>%
          as.character() # Ensure it's character

        # --- Get Current Subject Mark (potentially R1-R6) ---
        display_mark <- subject_positions %>%
          filter(subject_code == hovered_code) %>%
          pull(subject_mark) %>%
          as.character() # Ensure it's character

        # --- Check if originally failed ---
        status_text <- display_mark # Default to the displayed mark (e.g., "R1", "Pending")
        if (startsWith(display_mark, "R") && !is.null(degree_data$student_data)) {
          rank_number <- substr(display_mark, 2, 2) # Extract the number from R1, R2, etc.
          base_status_text <- paste0(translate(language, "Recommendation"), " ", rank_number)
          
          student_record <- degree_data$student_data %>% filter(subject_code == hovered_code)
          if (nrow(student_record) > 0) {
            # Check the last recorded mark for this subject
            original_mark <- student_record$subject_mark[1] # Already aggregated to last mark
            fail_marks <- c('SU', 'NP', translate(language, "Fail"))
            if (original_mark %in% fail_marks) {
              # Wrap the "(Fail)" including parentheses in a span with the fail color
              fail_text <- paste0(" <span style='color: #ff7f6d;'>(", translate(language, "Fail"), ")</span>")
              status_text <- paste0(base_status_text, fail_text)
            } else {
              status_text <- base_status_text # Set to "Recommendation X" if not failed
            }
          } else {
             status_text <- base_status_text # Set to "Recommendation X" if no student record found for this subject
          }
        } else if (startsWith(display_mark, "R")) { # Handle case where student_data might be NULL but it's still Rx
            rank_number <- substr(display_mark, 2, 2) 
            status_text <- paste0(translate(language, "Recommendation"), " ", rank_number)
        } else if (display_mark == translate(language, "Fail")) {
            # Ensure "Fail" status is displayed correctly if not an R-rank
            status_text <- translate(language, "Fail")
        } else if (display_mark == translate(language, "Pass")) {
            status_text <- translate(language, "Pass")
        } else if (display_mark == translate(language, "Transfer")) {
            status_text <- translate(language, "Transfer")
        }
        # Add other explicit translations if needed, otherwise keep display_mark

        # --- Add explanation if status is "Not available" ---
        status_text_final <- status_text # Default to the base status
        if (display_mark == translate(language, "Not available")) {
             # Get the specific row for the hovered subject from subject_positions
             hovered_pos_info <- subject_positions %>% 
                 filter(subject_code == hovered_code) %>% 
                 slice(1) # Ensure we only get one row
            
             # Check if the row and necessary columns exist before proceeding
             if (nrow(hovered_pos_info) > 0 && 
                 "unavailability_reason" %in% names(hovered_pos_info)) { 
                 
                 reason_code <- hovered_pos_info$unavailability_reason
                 # Details column might not exist if reason is SEMESTER_FAILED
                 reason_details <- if ("unavailability_details" %in% names(hovered_pos_info)) { 
                                      hovered_pos_info$unavailability_details 
                                   } else { 
                                      NA 
                                   }
             
                 # Ensure reason_code is valid
                 if (!is.na(reason_code) && nchar(reason_code) > 0) {
                    # Generate message based on the reason code
                    if (reason_code == "MIN_ECTS_FAILED") {
                        # Need to check details is not NA before using sprintf
                        if (!is.na(reason_details)) {
                            status_text_final <- sprintf(
                                translate(language, "Not available (Minimum %s ECTS required)"), 
                                reason_details
                            )
                        } else {
                           status_text_final <- translate(language, "Not available (Minimum ECTS required)") # Fallback
                        }
                    } else if (reason_code == "ANTIREQUISITE_FAILED") {
                       # Need to check details is not NA before processing
                       if (!is.na(reason_details)) {
                            blocking_codes <- unlist(str_split(reason_details, ";"))
                            blocking_codes <- blocking_codes[blocking_codes != ""]
                            
                            if (length(blocking_codes) > 0 && !is.null(degree_data$subject_names)){
                                # Look up names for the blocking codes
                                name_col <- paste0("name_", language)
                                blocking_names <- degree_data$subject_names %>% 
                                    filter(subject_code %in% blocking_codes) %>% 
                                    pull(!!sym(name_col))
                                
                                # Format names nicely (e.g., A, B and C)
                                formatted_names <- format_subject_list(blocking_names, language)
                                
                                status_text_final <- sprintf(
                                    translate(language, "Not available (You have already passed %s)"),
                                    formatted_names
                                )
                            } else {
                                # Fallback if names can't be found
                                status_text_final <- translate(language, "Not available")
                            }
                       } else {
                           # Fallback if details missing
                           status_text_final <- translate(language, "Not available")
                       } 
                    } else if (reason_code == "SEMESTER_FAILED") {
                        status_text_final <- translate(language, "Not available (Not offered this semester)")
                    } else {
                        # Fallback for unknown reason codes
                        status_text_final <- translate(language, "Not available")
                    }
                 }
             } else {
                 # Fallback if hovered_pos_info is missing or lacks the reason column
                 status_text_final <- translate(language, "Not available")
             }
          }
        # --- End "Not available" explanation ---

        # Define passed/transferred marks for checking prerequisites
        passed_marks <- c(translate(language, "Pass"), translate(language, "Transfer"))
        fail_marks <- c("SU", "NP", translate(language, "Fail")) # Needed for legend status
        # Define marks considered 'earned' for ECTS calculation
        earned_marks_for_ects <- c("A", "NO", "EX", "M", "Reconeguda", passed_marks)

        # --- Calculate Earned ECTS --- (Do this *before* prerequisite check)
        earned_ects <- 0 # Default to 0 if no student data
        if (!is.null(degree_data$student_data)) {
          passed_or_transferred_codes_for_ects <- degree_data$student_data %>%
            filter(subject_mark %in% earned_marks_for_ects) %>%
            pull(subject_code) %>%
            unique()

          if (length(passed_or_transferred_codes_for_ects) > 0) {
            earned_ects <- degree_data$subjects_data %>%
              filter(subject_code %in% passed_or_transferred_codes_for_ects) %>%
              summarise(total_ects = sum(credits, na.rm = TRUE)) %>%
              pull(total_ects)
          }
        }
        # Ensure earned_ects is numeric, default to 0 if calculation failed
        if (!is.numeric(earned_ects) || is.na(earned_ects)) {
          earned_ects <- 0
        }

        # --- Get Prerequisite Info --- (Standard prerequisites)
        prerequisites <- degree_data$prerequisites_data
        subject_prereqs_codes <- prerequisites %>%
          filter(subject_code == hovered_code) %>%
          pull(prerequisite_code)

        # Get codes of subjects actually passed or transferred by the student
        passed_or_transferred_codes <- subject_positions %>% # Use subject_positions for current status
          filter(subject_mark %in% passed_marks) %>%         # based on passed_marks
          pull(subject_code) %>%
          unique()

        prereq_display_items <- list() # Initialize as a list

        # --- Handle Standard Prerequisites ---
        if (length(subject_prereqs_codes) > 0) {
          # Filter the names data frame
          filtered_names <- degree_data$subject_names %>%
            filter(subject_code %in% subject_prereqs_codes)

          dynamic_name_col <- paste0("name_", language)
          prereq_names_df <- filtered_names[, c("subject_code", dynamic_name_col)]

          prereq_display_items <- lapply(subject_prereqs_codes, function(code) {
            is_fulfilled <- code %in% passed_or_transferred_codes
            emoji <- ifelse(is_fulfilled, "✅", "❌")
            prereq_name <- prereq_names_df[prereq_names_df$subject_code == code, 2]
            tagList(paste(emoji, prereq_name), tags$br())
          })
        }


        # --- Convert list of items to displayable elements ---
        prereq_display_elements <- NULL
        if (length(prereq_display_items) > 0) {
          prereq_display_elements <- tagList(prereq_display_items)
        } else {
          # Display "None" only if there are no standard OR ECTS prerequisites
          prereq_display_elements <- tags$span(translate(language, "None"))
        }
        # --- End Prerequisite Info Processing ---

        # --- Itinerary Info Processing ---
        itinerary_display_elements <- NULL # Default to NULL

        # Helper function to get translated itinerary name from path number
        get_itinerary_name <- function(path_num, lang) {
           switch(path_num,
             "1" = translate(lang, "Computer Engineering"),
             "2" = translate(lang, "Software Engineering"),
             "3" = translate(lang, "Computing"),
             "4" = translate(lang, "Information Systems"),
             "5" = translate(lang, "Information Technology"),
             "" # Default: Should not happen for valid paths
           )
        }

        # Only process if subject is not passed/transferred
        if (!(display_mark %in% passed_marks)) {
            hovered_subject_details <- degree_data$subjects_data %>% filter(subject_code == hovered_code)
            if (nrow(hovered_subject_details) > 0) {
                subject_path_str <- as.character(hovered_subject_details$path[1]) # Ensure character
                subject_type <- hovered_subject_details$type[1]

                # Only show itinerary info for Optional subjects (Type P)
                if (subject_type == "P" && subject_path_str != "0") { # Path 0 is not an itinerary
                    subject_paths <- unlist(str_split(subject_path_str, "_")) # Split for multiple itineraries (e.g., "1_5")
                    selected_itinerary_path <- input$itinerary # User's choice ("0" means Not Sure)

                    itinerary_items <- lapply(subject_paths, function(path_num) {
                        itinerary_name <- get_itinerary_name(path_num, language)
                        if (nchar(itinerary_name) > 0) {
                           # Determine emoji: Green ONLY if user selected this specific itinerary
                           emoji <- ifelse(selected_itinerary_path != "0" && path_num == selected_itinerary_path, "🟢", "🟡")
                           tagList(paste(emoji, itinerary_name), tags$br())
                        } else {
                           NULL # Skip if name is empty (invalid path?)
                        }
                    })
                    # Filter out NULLs from the list
                    itinerary_items <- Filter(Negate(is.null), itinerary_items)

                    if(length(itinerary_items) > 0) {
                        itinerary_display_elements <- tagList(itinerary_items)
                    }
                }
            }
        }
        # --- End Itinerary Info Processing ---

        # Generate HTML directly instead of calling create_subject_hover_info
        html <- tags$div(
          style = paste0(
            "position: absolute; ",
            "background-color: rgba(245, 245, 245, 0.85); ", # Slightly transparent background
            "border: 1px solid #ccc; ",
            "padding: 10px; ",
            "border-radius: 5px; ",
            "box-shadow: 2px 2px 5px rgba(0,0,0,0.2); ",
            "pointer-events: none; ", # Important: prevents hover issues
            # Use coords_css for positioning relative to the plot container
            "left:", input$graf_hover$coords_css$x + 15, "px; ", # Offset from cursor
            "top:", input$graf_hover$coords_css$y + 15, "px;",
            "z-index: 1000;" # Ensure it's above other elements
          ),
          tags$h5(hovered$name, style = "margin-top: 0; margin-bottom: 5px; font-weight: bold;"),
          # Combined Type and ECTS line
          tags$p(paste(hovered$subject_type, "|", subject_credits, "ECTS"), style = "margin-bottom: 5px;"),
          # Use the potentially modified status_text here, wrapped in HTML()
          tags$p(HTML(paste(translate(language, "Status:"), status_text_final)), style = "margin-bottom: 5px;"),
          # --- Add Prerequisite Section (conditionally) ---
          {
            # Check against the original display_mark before modification
            if (!(display_mark %in% passed_marks)) {
              tagList(
                # --- Display Combined Prerequisites ---
                tags$div(
                  style = "margin-top: 8px;",
                  tags$strong(paste0(translate(language, "Prerequisites:"), " ")), # Changed label
                  tags$br(),
                  prereq_display_elements # Display the combined list
                )
              )
            }
          },
          # --- Display Itinerary Section (Conditionally) ---
          if (!is.null(itinerary_display_elements)) {
            tags$div(
              style = "margin-top: 8px;",
              tags$strong(paste0(translate(language, "Itinerary:"), " ")),
              tags$br(),
              itinerary_display_elements
            )
          },
          # --- End Itinerary Section ---
        )
        return(html)
      })
      
      
      # Gràfic del mapa asignatures -------------------------------------------------------
      
      output$grafic=renderPlot({
        degree_data = degree_data()
        # JULIA 05/10/2024
        available_student_ids = degree_data$available_student_ids
        final_project_code = degree_data$final_project_code
        subjects_data =  degree_data$subjects_data
        student_data = degree_data$student_data
        
        #browser()
        
        # tots els parells d'assignatures ordenades per codi
        subjects=sort(subjects_data$subject_code)
        # JULIÀ 17/10/2023 treure ass2
        #ass2=expand.grid(ass1 = ass, ass2 = ass)
        #if(nrow((ass2))==0) return(NULL)
        # print("ass2")
        # print(head(ass2))
        
        # JULIA 23/12/2022 adaptar la matriz de solapamientos
        if (is.null(degree_data$overlap_distance_matrix)) return(NULL)
        overlap_distance_matrix=degree_data$overlap_distance_matrix
        
        # JULIA 23/12/2022 adaptar la matriz de popularidad relativa
        if (is.null(degree_data$popularity_distance_matrix)) return(NULL)
        popularity_distance_matrix=degree_data$popularity_distance_matrix
        
        # JULIA 23/12/2022 adaptar la matriz de dificultad
        if (is.null(degree_data$difficulty_distance_matrix)) return(NULL)
        difficulty_distance_matrix = degree_data$difficulty_distance_matrix
        
        # JULIA 27/12/2022 adaptar la matriz con el plan de estudios 
        if (is.null(degree_data$semester_distance_matrix)) return(NULL)
        semester_distance_matrix=degree_data$semester_distance_matrix
        
        # !!! RENDER PLOT
        # creació del gràfic, de moment és un plot
        all_inputs_valid=!is.null(input$difficulty) &&
          !is.null(input$semester) &&
          !is.null(input$popularity) &&
          !is.null(input$overlap)
        
        if (all_inputs_valid) {
          difficulty <- input$difficulty
          semester <- input$semester
          popularity <- input$popularity
          overlap <- input$overlap 
          
          # JULIA 24/12/2022 normalizar los pesos
          weight_denominator=5.0
          
          subject_distance_matrix <<- matrix(
            # JULIA 23/12/2022 cambiar matrices
            (difficulty/weight_denominator)*difficulty_distance_matrix +
              (overlap/weight_denominator)*overlap_distance_matrix + 
              (popularity/weight_denominator)*popularity_distance_matrix +
              (semester/weight_denominator)*semester_distance_matrix,
            length(subjects),length(subjects))
          
          if (!is.null(subject_distance_matrix)) {
            # Normalize the combined distance matrix to [0, 1]
            min_dist <- min(subject_distance_matrix, na.rm = TRUE)
            max_dist <- max(subject_distance_matrix, na.rm = TRUE)
            if (is.finite(min_dist) && is.finite(max_dist) && max_dist > min_dist) {
              subject_distance_matrix <- (subject_distance_matrix - min_dist) / (max_dist - min_dist)
            } else if (is.finite(min_dist)) {
              # Handle cases where all distances are the same (or matrix is empty/all NA)
              subject_distance_matrix[,] <- 0 # Set all to 0 if range is zero or invalid
            }
            
            # añadimos un "pequeño" margen a los ceros y establecemos un suelo mínimo (0.5)
            set.seed(1)
            subject_distance_matrix=apply(subject_distance_matrix,c(1,2),function(x){ 
              ifelse(x == 0, runif(1, 0.5, 0.6), ifelse(x < 0.5, 0.5, x))
            })
            # JULIA 24/12/2022 forzar una separación para poder generar cuadrícula
            subject_distance_matrix=subject_distance_matrix+1
            
            # crear mapeado 2D de la matriz de distancias resultante
            # JULIA 07/01/2023 se podrían probar otros algoritmos
            subject_coordinates <- sammon(subject_distance_matrix, trace=F)
            
            # corrección manual tomando como referencia el TF
            # el TF debería quedar lo más a la derecha y arriba posible
            # !!!
            final_project_index=which(subjects==final_project_code)
            coordinate_range=range(subject_coordinates$points[,1])
            if ((subject_coordinates$points[final_project_index,1]-coordinate_range[1])/(coordinate_range[2]-coordinate_range[1])<0.5) {
              subject_coordinates$points[,1]=-subject_coordinates$points[,1]
            }
            coordinate_range=range(subject_coordinates$points[,2])
            if ((subject_coordinates$points[final_project_index,2]-coordinate_range[1])/(coordinate_range[2]-coordinate_range[1])<0.5) {
              subject_coordinates$points[,2]=-subject_coordinates$points[,2]
            }
            
            subject_coordinates <- data.frame(
              x = subject_coordinates$points[,1],
              y = subject_coordinates$points[,2],
              subject_code = subjects
            )
            
            subject_coordinates <- merge(subject_coordinates, subjects_data, by='subject_code', all.x=T)
            
            #subject_coordinates$TFM=as.factor(ifelse(subject_coordinates$ass==final_project_code,0,0))
            
            # afegir dades estudiant
            if (!is.null(input$idp)) {
              if (input$idp!="---") {
                subject_coordinates <- merge(subject_coordinates, student_data, 'subject_code', all.x=T)
                subject_coordinates[is.na(subject_coordinates$subject_mark),'subject_mark']=translate(language, "Pending")
              } else {
                subject_coordinates$subject_mark=translate(language, "Pending")
              }
            } else {
              subject_coordinates$subject_mark=translate(language, "Pending")
            }
            
            # Initialize unavailability_reason column using mutate right after data setup
            subject_coordinates <- subject_coordinates %>%
             mutate(
                 unavailability_reason = NA_character_,
                 unavailability_details = NA_character_ # Add details column
             )

            # Define FINAL passed/transfer marks (raw codes) - needed for rule overrides
            final_passed_transfer_marks <- c("A", "NO", "EX", "M", "Reconeguda")
            passed_transfer_marks <- c(final_passed_transfer_marks, translate(language, "Pass"), translate(language, "Transfer"))

            # --- Apply Restrictions from CSV --- 
            if (!is.null(input$idp) && input$idp != "---" && !is.null(degree_data$restrictions_data)) {
              restrictions <- degree_data$restrictions_data
              student_subjects <- degree_data$student_data # Already has passed/transferred

              # Calculate earned ECTS once if needed
              earned_ects <- 0
              min_ects_rules_exist <- any(!is.na(restrictions$min_ects))
              if (min_ects_rules_exist) {
                  # (Earned ECTS calculation logic as before)
                  passed_or_transferred_codes_for_ects <- student_subjects %>%
                    filter(subject_mark %in% passed_transfer_marks) %>%
                    pull(subject_code) %>%
                    unique()
                  if (length(passed_or_transferred_codes_for_ects) > 0) {
                    earned_ects <- degree_data$subjects_data %>%
                      filter(subject_code %in% passed_or_transferred_codes_for_ects) %>%
                      summarise(total_ects = sum(credits, na.rm = TRUE)) %>%
                      pull(total_ects)
                  }
                  if (!is.numeric(earned_ects) || is.na(earned_ects)) { earned_ects <- 0 }
              }

              # Get codes of passed/transferred subjects once if needed
              passed_or_transferred_codes <- character(0)
              antireq_rules_exist <- any(!is.na(restrictions$antirequisites))
              if (antireq_rules_exist) {
                  passed_or_transferred_codes <- student_subjects %>%
                      filter(subject_mark %in% passed_transfer_marks) %>%
                      pull(subject_code) %>%
                      unique()
              }

              # Apply each restriction rule from the CSV
              for(i in 1:nrow(restrictions)) {
                rule <- restrictions[i, ]
                restricted_code <- rule$restricted_subject_code

                # Find index of target subject in coordinates dataframe
                target_idx <- which(subject_coordinates$subject_code == restricted_code)

                # Proceed only if the target subject exists and is not already passed/transferred
                if (length(target_idx) > 0 && !(subject_coordinates$subject_mark[target_idx] %in% passed_transfer_marks)) {
                  apply_restriction <- FALSE
                  reason_code <- NA_character_
                  details <- NA_character_

                  # Check MIN_ECTS rule
                  if (!is.na(rule$min_ects)) {
                    if (earned_ects < rule$min_ects) {
                      apply_restriction <- TRUE
                      reason_code <- "MIN_ECTS_FAILED"
                      details <- as.character(rule$min_ects)
                    }
                  }
                  
                  # Check ANTIREQUISITE rule (only if MIN_ECTS didn't already fail)
                  if (!apply_restriction && !is.na(rule$antirequisites)) { # Use !apply_restriction
                    blocking_codes_rule <- unlist(str_split(rule$antirequisites, ";"))
                    # Filter out empty strings that might result from splitting
                    blocking_codes_rule <- blocking_codes_rule[blocking_codes_rule != ""]
                    # Find which of these the student has actually passed
                    passed_blocking_codes <- intersect(blocking_codes_rule, passed_or_transferred_codes)

                    if (length(passed_blocking_codes) > 0) {
                       apply_restriction <- TRUE
                       reason_code <- "ANTIREQUISITE_FAILED"
                       # Store only the codes the student passed that triggered the rule
                       details <- paste(passed_blocking_codes, collapse=";")
                    }
                  }

                  # Apply the restriction if triggered
                  if (apply_restriction) {
                    # Set status to Not available
                    subject_coordinates$subject_mark[target_idx] <- translate(language, "Not available")
                    # Set reason code and details, only if no reason is already set
                    if (is.na(subject_coordinates$unavailability_reason[target_idx])) {
                       subject_coordinates$unavailability_reason[target_idx] <- reason_code
                       subject_coordinates$unavailability_details[target_idx] <- details
                    }
                  }
                }
              }
            }
            # --- End Apply Restrictions from CSV ---

            # --- Apply Semester Availability Rule ---
            # Check semester availability AFTER other rules, but BEFORE assigning R1-R6/Selected
            # semester_unavailable_key <- "Not available (Not offered this semester)" # No longer needed
             semester_reason_code <- "SEMESTER_FAILED"
             if (!is.null(input$selected_semester)) {
               subject_coordinates <- subject_coordinates %>% 
                 mutate(
                   # Set reason code and mark, only if no reason already exists
                   unavailability_reason = ifelse(!semester_number %in% c(0,input$selected_semester) & !(subject_mark %in% final_passed_transfer_marks) & is.na(unavailability_reason), semester_reason_code, unavailability_reason),
                   # Set mark to Not available if this rule triggers
                   subject_mark = ifelse(!semester_number %in% c(0,input$selected_semester) & !(subject_mark %in% final_passed_transfer_marks), translate(language,"Not available"), subject_mark)
                 )
             } else { 
               subject_coordinates <- subject_coordinates %>% 
                 mutate(
                   unavailability_reason = ifelse(!semester_number %in% c(0,1) & !(subject_mark %in% final_passed_transfer_marks) & is.na(unavailability_reason), semester_reason_code, unavailability_reason),
                   subject_mark = ifelse(!semester_number %in% c(0,1) & !(subject_mark %in% final_passed_transfer_marks), translate(language,"Not available"), subject_mark)
                 )
             }
             # --- End Semester Availability Rule ---
             
             # --- Apply Ranks, Discarded, Selected Statuses ---
             # These should overwrite the 'Not available' status if applicable
             # JULIA 09/01/2023 cambiar full_name por abrv
             subject_coordinates <- subject_coordinates %>% 
               mutate(subject_mark = ifelse(subject_code %in% recommended_list$subject_code[1], "R1", subject_mark)) %>%
               mutate(subject_mark = ifelse(subject_code %in% recommended_list$subject_code[2], "R2", subject_mark)) %>%
               mutate(subject_mark = ifelse(subject_code %in% recommended_list$subject_code[3], "R3", subject_mark)) %>%
               mutate(subject_mark = ifelse(subject_code %in% recommended_list$subject_code[4], "R4", subject_mark)) %>%
               mutate(subject_mark = ifelse(subject_code %in% recommended_list$subject_code[5], "R5", subject_mark)) %>%
               mutate(subject_mark = ifelse(subject_code %in% recommended_list$subject_code[6], "R6", subject_mark)) %>%
               mutate(subject_mark = ifelse(subject_code %in% discarded_list$subject_code, translate(language, "Discarded"), subject_mark)) %>% 
               mutate(subject_mark = ifelse(subject_code %in% selected_list$subject_code, translate(language, "Selected"), subject_mark))
             
             if (!is.null(input$selected_semester)) {
               subject_coordinates <- subject_coordinates %>% 
                 mutate(
                   unavailability_reason = ifelse(!semester_number %in% c(0,input$selected_semester) & !(subject_mark %in% final_passed_transfer_marks) & is.na(unavailability_reason), semester_reason_code, unavailability_reason),
                   subject_mark = ifelse(!semester_number %in% c(0,input$selected_semester) & !(subject_mark %in% final_passed_transfer_marks), translate(language,"Not available"), subject_mark)
                 )
             } else { 
               subject_coordinates <- subject_coordinates %>% 
                 mutate(
                   unavailability_reason = ifelse(!semester_number %in% c(0,1) & !(subject_mark %in% final_passed_transfer_marks) & is.na(unavailability_reason), semester_reason_code, unavailability_reason),
                   subject_mark = ifelse(!semester_number %in% c(0,1) & !(subject_mark %in% final_passed_transfer_marks), translate(language,"Not available"), subject_mark)
                 )
             }
             
             # forzar y ordenar todas las notas posibles
             # incloure les convalidades (Reconeguda)
             subject_coordinates[subject_coordinates$subject_mark %in% c('A','NO','EX','M'),'subject_mark']=translate(language, "Pass")
             subject_coordinates[subject_coordinates$subject_mark %in% c('NP','SU'),'subject_mark']=translate(language, "Fail")
             subject_coordinates[subject_coordinates$subject_mark %in% c('Reconeguda'),'subject_mark']=translate(language, "Transfer")
             
             subject_coordinates$subject_mark=factor(subject_coordinates$subject_mark,c(
                                    translate(language, "Pass"), 
                                    translate(language, "Transfer"),
                                    translate(language, "Fail"),
                                    translate(language, "Not available"),
                                    translate(language, "Discarded"),
                                    translate(language, "Pending"),
                                    "R1", "R2", "R3", "R4", "R5", "R6",
                                    translate(language, "Selected")
                                    ))

            color_palette <- color_palette_base # Use the base definition
            
            # graf
            colorT="black"
            subject_plot=ggplot(subject_coordinates, aes(x=x,y=y,label=subject_abbreviation)) +
              theme_void() +
              theme(
                panel.background=element_rect(fill=NA, color=NA),
                legend.position = "none" # Disable ggplot legend
              )
            
            # si no hi ha cap indicador triat
            if (input$idp!="---" || is.null(input$indicador)) {
              # color de l'àrea segons la nota
              # JULIA 07/01/2023 cambiar position a right
              if (input$bubbles) {
                subject_plot = subject_plot +
                  geom_voronoi_tile(aes(x=x,y=y,fill=subject_mark,group=-1L), colour="white",max.radius=input$bubbles) +
                  scale_fill_manual(values=color_palette, drop = F)
              } else {
                subject_plot = subject_plot + 
                  geom_voronoi_tile(aes(x=x,y=y,fill=subject_mark,group=-1L), colour="white") +
                  scale_fill_manual(values=color_palette, drop = F)
              }
            } else {
              # color de l'àrea en funció de l'indicador
            }
            
            # afegir les assignatures
            subject_coordinates$selected_subjects=1
            if (!is.null(input$subject_type)) {
              subject_coordinates <- filter_subjects_by_type(language, degree_data, subject_coordinates, input)
            }
            if (!is.null(input$search_subject) && str_length(input$search_subject) > 0) {
              subject_coordinates <- search_subjects(language, degree_data, subject_coordinates, input)
            }
            
            # JULIA 20/12/2022 forzar que aparezcan todas las asignaturas
            # cuando se seleccionan las asignaturas a matricular no se
            # muestran los "centros" de las recomendadas, pero solo si la
            # selección de nombre de asignatura está vacía
            if (!is.null(input$subject_type) & is.null(input$search_subject)) {
              if (input$subject_type==translate(language, "All")) {
                subject_coordinates$selected_subjects=1
              }
            }
            
            filtered_subject_coordinates = subject_coordinates[subject_coordinates$subject_mark==translate(language, "Pending") | subject_coordinates$selected_subjects==1,]
            # JULIA 07/01/2023 cambiar a 1 columna en vertical
            subject_plot <- subject_plot +
              # Centered text for all subjects
              geom_text(data = filtered_subject_coordinates, 
                      aes(alpha=ifelse(selected_subjects==1,1,0.72)), size=5, colour=colorT, hjust=0.5, vjust=0.5)
            
            # guardamos el mapa para hover, etc.
            #print("save qQ")
            subject_positions <<- subject_coordinates
            
            # show map
            subject_plot
          }
        }
        
      }, height=600)
      
      
      # Custom legend using HTML
      output$uiLegend <- renderUI({
        ns <- session$ns
        
        # Define colors and labels (ensure these match the plot)
        color_palette <- color_palette_base
        degree_data <- degree_data() # Need degree_data for student records
        
        # Translate all base labels for the HTML legend display
        final_legend_labels <- sapply(static_legend_labels_base, function(label) translate(language, label))

        # Generate items for the first legend (Statuses)
        labels1 <- final_legend_labels[1:6]
        colors1 <- color_palette[1:6]
        legend_items1 <- lapply(seq_along(labels1), function(i) {
          tags$li(
            style = "display: flex; align-items: baseline; margin-bottom: 5px;",
            tags$span(
              style = paste0(
                "display: inline-block; ",
                "width: 15px; height: 15px; ",
                "background-color: ", colors1[i], "; ", 
                "margin-right: 8px; border: 1px solid #ccc;",
                "flex-shrink: 0; box-sizing: border-box;",
                "position: relative; top: 3px;"
              )
            ),
            tags$span(labels1[i]) 
          )
        })
        
        # Generate items for the second legend (Recommendations & selection)
        colors2 <- color_palette[7:13]
        
        # Get subject abbreviations and check for failure status
        custom_labels <- vector("character", 7)
        fail_marks <- c('SU', 'NP', translate(language, "Fail"))
        
        # Only try to get info if we have recommendations and student data
        if(length(recommended_list$subject_code) > 0) {
          # Get abbreviations and failure status for each recommended subject
          for(i in 1:min(6, length(recommended_list$subject_code))) {
            subject_code <- recommended_list$subject_code[i]
            if(!is.na(subject_code)) {
              # Fetch full name
              full_name_col <- paste0("name_", language)
              subject_full_name <- degree_data$subject_names %>%
                filter(subject_code == !!subject_code) %>%
                pull(!!sym(full_name_col))
              subject_display_name <- ifelse(length(subject_full_name) > 0, subject_full_name[1], "") # Use full name or empty string

              # Fetch ECTS credits
              subject_ects <- degree_data$subjects_data %>%
                filter(subject_code == !!subject_code) %>%
                pull(credits)
              ects_display <- ifelse(!is.na(subject_ects) && length(subject_ects) > 0, paste0(" (", subject_ects[1], " ECTS)"), "")

              # Check original status if student data exists
              is_failed <- FALSE
              if (!is.null(degree_data$student_data)) {
                 student_record <- degree_data$student_data %>% filter(subject_code == !!subject_code)
                 if (nrow(student_record) > 0) {
                    original_mark <- student_record$subject_mark[1] # Already aggregated
                    if (original_mark %in% fail_marks) {
                      is_failed <- TRUE
                    }
                 }
              }

              # Construct label using the full name and ECTS
              label_prefix <- paste0(i, ": ", subject_display_name, ects_display)
              # Wrap the "(Fail)" including parentheses in a span with the fail color if needed
              fail_span <- paste0("<span style='color: #ff7f6d;'>(", translate(language, "Fail"), ")</span>")
              label_suffix <- ifelse(is_failed, paste0(" ", fail_span), "")
              custom_labels[i] <- paste0(label_prefix, label_suffix)

            } else {
              custom_labels[i] <- paste0(i, ":") # Empty if no subject
            }
          }
        } else {
           # Default labels if no recommendations yet
           for(i in 1:6) { custom_labels[i] <- paste0(i, ":") }
        }
        
        # Add the "Selected" label
        custom_labels[7] <- translate(language, "Selected")
        
        # --- Generate items for the second legend (Recommendations ONLY) ---
        num_recommendations <- length(na.omit(recommended_list$subject_code))
        recommendation_labels <- custom_labels[1:num_recommendations] # Only generated labels
        recommendation_colors <- colors2[1:num_recommendations]      # Only needed colors

        legend_items2 <- lapply(seq_along(recommendation_labels), function(i) {
          tags$li(
            style = "display: flex; align-items: baseline; margin-bottom: 5px;",
            tags$span(
              style = paste0(
                "display: inline-block; ",
                "width: 15px; height: 15px; ",
                "background-color: ", recommendation_colors[i], "; ", # Use recommendation_colors
                "margin-right: 8px; border: 1px solid #ccc;",
                "flex-shrink: 0; box-sizing: border-box;",
                "position: relative; top: 3px;"
              )
            ),
            tags$span(HTML(recommendation_labels[i])) # Use recommendation_labels
          )
        })
        
        # --- Generate items for the third legend (Selection ONLY) ---
        selected_color <- colors2[7] # The last color is for Selected
        
        # Calculate dynamic label based on selected subjects
        selected_codes <- na.omit(selected_list$subject_code)
        num_selected <- length(selected_codes)
        total_ects <- 0
        if (num_selected > 0 && !is.null(degree_data$subjects_data)) {
          selected_subjects_data <- degree_data$subjects_data %>%
            filter(subject_code %in% selected_codes)
          # Ensure credits are numeric and handle potential NAs
          credits_numeric <- as.numeric(selected_subjects_data$credits)
          total_ects <- sum(credits_numeric, na.rm = TRUE) 
        }
        
        # Determine singular or plural subject word
        subject_word_key_en <- ifelse(num_selected == 1, "Subject", "Subjects")
        subject_word_translated <- translate(language, subject_word_key_en)
        
        dynamic_selected_label <- sprintf(
          "%d %s (%d ECTS)", 
          num_selected, 
          subject_word_translated,
          total_ects
        )

        legend_item_selected <- tags$li(
          style = "display: flex; align-items: baseline; margin-bottom: 5px;",
          tags$span(
            style = paste0(
              "display: inline-block; ",
              "width: 15px; height: 15px; ",
              "background-color: ", selected_color, "; ",
              "margin-right: 8px; border: 1px solid #ccc;",
              "flex-shrink: 0; box-sizing: border-box;",
              "position: relative; top: 3px;"
            )
          ),
          tags$span(dynamic_selected_label)
        )

        # --- Return the legends based on the current step ---
        # Always show the first legend
        tag_list_elements <- list(
          tags$div(
            style = "margin-top: 20px; padding: 10px; border: 1px solid #eee; background-color: #f9f9f9;",
            h5(translate(language, "Subject status"), style="margin-top: 0; margin-bottom: 10px; font-weight: bold;"),
            tags$ul(
              style = "list-style: none; padding-left: 0; margin-bottom: 0;", 
              legend_items1
            )
          )
        )
        
        # Conditionally add the second legend if step is "Recommendations" or "Selection"
        if (enrollment_step$current_step >= 2) {
          tag_list_elements[[length(tag_list_elements) + 1]] <- 
            tags$div(
              style = "margin-top: 15px; padding: 10px; border: 1px solid #eee; background-color: #f9f9f9;", 
              h5(translate(language, "Recommendations"), style="margin-top: 0; margin-bottom: 10px; font-weight: bold;"), # Changed title
              tags$ul(
                style = "list-style: none; padding-left: 0; margin-bottom: 0;",
                legend_items2 # Use the modified recommendation items
              )
            )
        }
        
        # Conditionally add the third legend if step is "Selection" (Step 3 or 4)
        if (enrollment_step$current_step >= 3) {
          tag_list_elements[[length(tag_list_elements) + 1]] <-
            tags$div(
              style = "margin-top: 15px; padding: 10px; border: 1px solid #eee; background-color: #f9f9f9;",
              h5(translate(language, "Selection"), style="margin-top: 0; margin-bottom: 10px; font-weight: bold;"),
              tags$ul(
                style = "list-style: none; padding-left: 0; margin-bottom: 0;",
                legend_item_selected # Add the single selected item
              )
            )
        }
        
        # Return the final tagList
        do.call(tagList, tag_list_elements)
      })
      
      
      # Gràfic del calendari --------------------------------------------------------------
      
      output$cal=renderPlot({
        degree_data = degree_data()
        submissions=NULL
        activity_deadline_calendar=NULL
        has_selected_subjects <- !is.null(selected_list$subject_code[1]) || 
          !is.null(selected_list$subject_code[2]) ||
          !is.null(selected_list$subject_code[3]) ||
          !is.null(selected_list$subject_code[4]) || 
          !is.null(selected_list$subject_code[5]) || 
          !is.null(selected_list$subject_code[6])
        workload <- if(!is.null(input$workload)){ input$workload } else{ 1 }
        inputsOK <- !is.null(input$selected_semester) && has_selected_subjects
        if (!inputsOK) return(NULL)
        activity_deadline_calendar <- degree_data$subject_overlap_data
        if (nrow(activity_deadline_calendar)==0) return(NULL)
        
        # per cada activitat de cada assignatura nomes darrers N dies indicats 
        for (i in 1:nrow(activity_deadline_calendar)) {
          j=ncol(activity_deadline_calendar)
          
          # buscar el primer 1 per la dreta
          while (activity_deadline_calendar[i,j]==0) {
            j=j-1
          }
          # guardar els lliuraments
          submissions=rbind(submissions,data.frame(subject_code=activity_deadline_calendar[i,1],day_number=j-4,load=1))
          
          # "saltar" els dies indicats
          j=j-workload
          
          # treure els 1 anteriors
          while ((activity_deadline_calendar[i,j]==1) && (j>4)) {
            activity_deadline_calendar[i,j]=0
            j=j-1
          }
        }
        
        # acumular les activitats de cada assignatura
        accumulated_activities=aggregate(activity_deadline_calendar[,-(1:4)],list(activity_deadline_calendar$subject_code), sum)
        colnames(accumulated_activities)[1]='subject_code'
        
        # format long, recuperant el dia
        accumulated_activities_long=gather(accumulated_activities,key='day_number',value='load',-subject_code)
        accumulated_activities_long$day_number=as.numeric(substring(accumulated_activities_long$day_number,2))
        accumulated_activities_long=accumulated_activities_long[order(accumulated_activities_long$subject_code,accumulated_activities_long$day_number),]
        unique_subject_codes=unique(accumulated_activities_long$subject_code)
        accumulated_activities_long$subject_code=factor(accumulated_activities_long$subject_code, levels=unique_subject_codes)
        
        # calculem la carrega mitjana per dia 
        daily_workload=aggregate(accumulated_activities_long$load,list(accumulated_activities_long$day_number),sum)$x
        
        # OLD: Average daily activities;  load=round(mean(daily_workload)*100)/100
        active_days <- sum(daily_workload > 0)
        active_days_prop <- round(100 * (active_days / length(daily_workload)), 2)
        
        # OLD: Percentage of overlapping deadlines: load2=round((sum(daily_workload>1)/sum(daily_workload>0))*100)/100
        overlap_days <- sum(daily_workload > 1)
        overlap_days_prop <- round(100 * (overlap_days / length(daily_workload)), 2)
        
        # Preparem dates
        semester_dates <- get_semester_start_dates()
        # 09/02/2023 JULIA: no funciona para el segundo semestre !!!
        #semester_start_date <- if_else(input$selected_semester == 1, inici_sem$first_semester_start, inici_sem$second_semester_start)
        semester_start_date <- semester_dates$first_semester_start
        subjects_info <- tibble(
          subject_code = degree_data$subject_names[["subject_code"]],
          full_name = degree_data$subject_names[[paste0("name_",language)]]
        ) %>% 
          left_join(degree_data$subjects_data, by = "subject_code") %>% 
          mutate(full_name = paste0(full_name," (",subject_abbreviation,")"))
        accumulated_activities_long <- as_tibble(accumulated_activities_long) %>% 
          mutate(semester_date = (semester_start_date + day_number)) %>% 
          left_join(subjects_info, by = "subject_code")
        submissions <- as_tibble(submissions) %>% 
          mutate(
            semester_date = (semester_start_date + day_number),
            subject_code = factor(subject_code, levels = unique_subject_codes)
          ) %>% 
          left_join(subjects_info, by = "subject_code")
        
        # fem el gràfic
        gg <- accumulated_activities_long %>% ggplot(aes(x = semester_date, y = load)) +
          geom_col( width = 0.5, alpha = 0.28, fill = "#4875fb") +
          geom_col(data = submissions, aes(x = semester_date, y = load), fill = "#4875fb", width = 0.72, alpha = 1) +
          scale_y_continuous(breaks = seq(0, 8)) +
          scale_x_date(
            name = "Dia",
            limits = c((semester_start_date), (semester_start_date + 135)),
            date_labels = "%d/%m", 
            date_breaks = "1 weeks",
            sec.axis = dup_axis(
              name = "Semana",
              labels = scales::date_format("%V")
            )
          ) +
          labs(
            fill = translate(language, "Subjects:"), 
            x = NULL,
            y = translate(language, "Number of concurrent activities")
          ) +
          theme(
            plot.title=element_text(size=24),
            legend.position="none",
            axis.text.x = element_text(size = rel(1.25)),
            panel.grid.minor = element_blank()
          )
        
        gg2 <- gg + 
          facet_wrap(c("full_name"), ncol = 1)
        gg <- gg + 
          labs(
            title=paste0(
              translate(language, "Total number of activities"),": ",
              nrow(submissions),"\n",
              translate(language, "Average daily activities"),": ",
              active_days," (", active_days_prop,"%)\n",
              translate(language, "Percentage of overlapping (days with more than 1 activity)"),": ",
              overlap_days, " (", overlap_days_prop,"%)\n"
            )
          )
        
        (gg2 / gg) + plot_layout(heights = c(count_selected_subjects(selected_list), 2))
        
      }, height = 800)
      
      
      # Taula de l'expedient de l'estudiant -----------------------
      output$academic_record=renderTable({
        # JULIA 19/10/2023 afegir nom assignatura, capçalera amb idioma
        degree_dataOUT = degree_data()
        if (!is.null(degree_dataOUT$student_data) && nrow(degree_dataOUT$student_data) > 0) {
          academic_record_data=merge(degree_dataOUT$student_data, degree_dataOUT$subject_names, by.x='subject_code', by.y='subject_code')
          academic_record_data=academic_record_data[,c('relative_semester', paste0('name_',language), 'subject_mark')] 
          academic_record_data=academic_record_data[order(academic_record_data$relative_semester),]
          # JULIÀ 07/11/2023 notas
          academic_record_data[academic_record_data$subject_mark=="M",'subject_mark']=translate(language, "With Honors")
          academic_record_data[academic_record_data$subject_mark=="EX",'subject_mark']=translate(language, "Excellent")
          academic_record_data[academic_record_data$subject_mark=="NO",'subject_mark']=translate(language, "Good")
          academic_record_data[academic_record_data$subject_mark=="A",'subject_mark']=translate(language, "Satisfactory")
          academic_record_data[academic_record_data$subject_mark=="SU",'subject_mark']=translate(language, "Fail")
          academic_record_data[academic_record_data$subject_mark=="NP",'subject_mark']=translate(language, "Withdrawal")
          # falta traduir les convalidacions
          academic_record_data[academic_record_data$subject_mark=="Reconeguda",'subject_mark']=translate(language, "Transfer")
          colnames(academic_record_data)=c(translate(language, "Semester"), translate(language, "Subject"), translate(language, "Mark")) 
          # mostrar la taula tal qual
          academic_record_data
        }
      }, bordered=T, digits=0)
      
      
      # Resum de la matricula ------------------------------------
      output$uiEnrollment=renderUI({
        if(enrollment_step$current_step < 4) {
          return()
        }
        ns <- session$ns
        degree_data <- degree_data()
        subjects_info <- tibble(
          subject_code = degree_data$subject_names[["subject_code"]],
          full_name = degree_data$subject_names[[paste0("name_",language)]]
        ) %>% 
          left_join(degree_data$subjects_data, by = "subject_code") %>% 
          mutate(full_name = paste0(full_name," (",subject_abbreviation,")"))
        descartaStr <- subjects_info %>% 
          filter(subject_code %in% discarded_list$subject_code) %>% 
          pull(full_name)
        recomanaStr <- subjects_info %>% 
          filter(subject_code %in% recommended_list$subject_code) %>% 
          pull(full_name)
        seleccioStr <- subjects_info %>% 
          filter(subject_code %in% selected_list$subject_code) %>% 
          pull(full_name)
        tagList(
          h2(translate(language, "¡Gracias por utilizar Visual Enrollment!")),
          h3(translate(language, "Tus preferencias y selección para la siguiente matricula")),
          tags$ol(
            tags$li(class="step0", translate(language, "Discard"), p(paste(descartaStr, collapse = ", "))),
            tags$li(class="step1", translate(language, "Preferences"),
                    div(
                      translate(language,"Difficulty:"), paste0(input$difficulty, "/5"),
                      br(),
                      translate(language, "Popularity:"),  paste0(input$popularity, "/5"),
                      br(),
                      translate(language, "Overlaps between deadlines:"),  paste0(input$overlap, "/5"),
                      br(),
                      br(),
                    )
            ),
            tags$li(class="step2", translate(language, "Recommendations"), p(paste(recomanaStr, collapse = ", "))),
            tags$li(class="step3", translate(language, "Selection"), p(paste(seleccioStr, collapse = ", "))),
          ),
          actionButton(ns("screenshot"), translate(language, "Download enrolment")),
          NULL
        )
      })
      
      
      # Cercar per tipologia o nom ----------------------------------------------------------------
      
      output$uiSubjectType=renderUI({
        req(input$degree)
        ns <- session$ns
        degree_data = degree_data()
        subject_type <- degree_data$subject_type[[paste0("tipologia_",language)]]
        tagList(
          selectInput(
            ns("subject_type"),
            translate(language, "Highlight each type of subject"),
            choices = c(translate(language, "All"), subject_type)
          )
        )
      })
      
      output$uiSearchSubject=renderUI({
        req(input$degree)
        ns <- session$ns
        degree_data = degree_data()
        # JULIA 10/01/2023 cambiar noms por full_name
        full_name <- degree_data$subject_names[[paste0("name_",language)]]
        tagList(
          textInput(
            ns("search_subject"),
            translate(language, "Search subject"),
          )
        )
      })
      
      
      # Selectors dinamics admin ----------------------------------------------------------------
      
      output$uiGrau=renderUI({
        ns <- session$ns
        selectInput(
          ns("degree"),
          translate(language, "Choose degree"),
          choices=setNames(
            c(
              "INFORMATICA",
              "DATASCIENCE"
            ),
            c(
              translate(language,"Computer science degree"),
              translate(language,"Applied data science")
            )
          ),
          selected=1
        )
      })
      
      
      output$uiSem=renderUI({
        ns <- session$ns
        selectInput(
          ns("selected_semester"),
          translate(language, "Choose a semester:"),
          choices=c(1,2),
          selected=1
        )
      })
      
      # !!! JULIA 24/12/2022 filtrar los idps por tutor
      # falta decidir los idps por tutor y la clave de cada uno
      output$uiEst=renderUI({
        if(!is.null(input$degree)) {
          degree_data = degree_data()
          # JULIÀ 17/10/2023 per defecte la llista d'idps disponibles
          student_choices = degree_data$available_student_ids
          # JULIÀ 24/10/2023 de moment treiem lo dels tutors
          #claveTutor = claveTutor()
          #if (claveTutor=="1A2B3C4D5E6F") {
          #  student_choices = c("XXX")
          #}
          #if (claveTutor=="DEMOECTEL2023") {
          #  student_choices = c("00000000000000000000000000000000")
          #}
          
          ns <- session$ns
          selectInput(
            ns("idp"),
            translate(language, "Choose a student:"),
            choices = student_choices,
            selected = input$idp
          )
        } else{
          NULL
        }
      })
      
      # Events: Click mapa -----------------------------------------------------------------
      
      observeEvent(input$graf_click, {
        #print("input$graf_click")
        if(!is.null(input$graf_click)) clicked <- find_closest_subject(input$graf_click)
        if(!is.null(clicked)) clicked_list$subject_code <- unique(c(clicked$subject_code, clicked_list$subject_code))
        # JULIA 18/12/2022 afegir protecció 
        if (is.null(clicked)) return(NULL)
        if(enrollment_step$current_step == 0) {
          descartable <- !(clicked$subject_code %in% selected_list$subject_code) &&
            clicked$subject_mark %in% c("SU", "NP", translate(language, "Fail"), translate(language, "Pending"), translate(language, "Discarded"))
          if(descartable) { 
            if(length(discarded_list$subject_code)>0 && clicked_list$subject_code[[1]] %in% discarded_list$subject_code) {
              discarded_list$subject_code <- discarded_list$subject_code[ !discarded_list$subject_code %in% clicked_list$subject_code[[1]] ]
            } else {
              discarded_list$subject_code <- unique(c(clicked_list$subject_code[[1]], discarded_list$subject_code))
            }
          }
        }
        if(enrollment_step$current_step > 1) {
          matriculable <- enrollment_step$current_step != 0 && !(clicked$subject_code %in% discarded_list$subject_code) 
          matriculable <- matriculable & (clicked$subject_mark %in% c("SU", "NP", translate(language, "Fail"), translate(language, "Pending"), translate(language, "Selected")))
          matriculable <- matriculable | str_detect(clicked$subject_mark, "^(R1|R2|R3|R4|R5|R6)")
          
          if(matriculable){
            enrollment_step$current_step <- 3
            session$sendCustomMessage(type = "steps",  message = paste0("step",enrollment_step$current_step))
            session$sendCustomMessage(type = "show", message = ".widgets_cal")
            session$sendCustomMessage(type = "show", message = ".workload_asignatures")
            session$sendCustomMessage(type = "show", message = ".download_asignatures")
            session$sendCustomMessage(type = "show", message = ".cal_asignatures")
            # Show the scroll actionLink using shinyjs
            shinyjs::show("scroll_to_cal_container")
            if(length(selected_list$subject_code)>0 && clicked_list$subject_code[[1]] %in% selected_list$subject_code) {
              selected_list$subject_code <- selected_list$subject_code[ !selected_list$subject_code %in% clicked_list$subject_code[[1]] ]
            } else {
              # Calculate current and clicked ECTS
              degree_data_click <- degree_data() # Get current data
              current_selected_codes <- na.omit(selected_list$subject_code)
              current_ects <- 0
              if (length(current_selected_codes) > 0 && !is.null(degree_data_click$subjects_data)) {
                 current_ects <- degree_data_click$subjects_data %>%
                   filter(subject_code %in% current_selected_codes) %>%
                   # Ensure credits are numeric and handle potential NAs
                   mutate(credits_num = as.numeric(credits)) %>%
                   summarise(total_ects = sum(credits_num, na.rm = TRUE)) %>%
                   pull(total_ects)
              }
              
              clicked_subject_code <- clicked_list$subject_code[[1]]
              clicked_ects <- 0
              if (!is.null(degree_data_click$subjects_data)) {
                clicked_ects_val <- degree_data_click$subjects_data %>%
                  filter(subject_code == clicked_subject_code) %>%
                  pull(credits)
                # Ensure credits are numeric and handle potential NAs
                if(length(clicked_ects_val) > 0 && !is.na(clicked_ects_val[1])) {
                  clicked_ects <- as.numeric(clicked_ects_val[1])
                }
              }

              # Check if adding the subject exceeds 36 ECTS
              if((current_ects + clicked_ects) > 36) {
                notification_message_key <- "If you want to enroll in more than 36 credits, you must first <a href='https://campus.uoc.edu/estudiant/tramits/en/altres_tramits/autoritzacions/index.html?s=80b162a0322bf2949627660ed48acf10d0f6c1c4d4b5e7a4acbba088bc106c22c25bd1e0332d0a8365dff9701d20fbf1de48672a2d48f63d242e5353a2cb34cd' target='_blank'>request authorization</a>."
                show_translated_notice(language, notification_message_key)
                return(NULL) # Stop execution for this click
              }
              # Add subject if within limits
              selected_list$subject_code <- unique(c(clicked_list$subject_code[[1]], selected_list$subject_code))
            }
          }
        }
      })
      
      
      # Events: Recommend -----------------------------------------------------------------
      
      observeEvent(input$recommend, {
        degree_data = degree_data()
        final_project_code = degree_data$final_project_code
        enrollment_step$current_step <- 2
        session$sendCustomMessage(type = "steps",  message = paste0("step",enrollment_step$current_step))
        enrollable <- as_tibble(subject_positions) %>% 
          filter(subject_mark %in% c("SU", "NP", translate(language, "Fail"), translate(language, "Pending"), translate(language, "Selected")) | str_detect(subject_mark, "^(R1|R2|R3|R4|R5|R6)")
        )
        
        # Itinerary mapping (using path numbers from CSV)
        # Note: path=0 means Not Itinerary-Specific (B, O, C, T, Practicas)
        #       path=1: Ing. Comp.
        #       path=2: Ing. Soft.
        #       path=3: Comp.
        #       path=4: Sist. Info.
        #       path=5: Tec. Info.
        #       path=X_Y: Belongs to multiple

        # Get completed subjects codes (used multiple times)
        passed_or_transferred_codes <- subject_positions %>%
          filter(subject_mark %in% c(translate(language, "Pass"), translate(language, "Transfer"))) %>%
          pull(subject_code) %>%
          unique()

        # Define subjects data once
        all_subjects_data <- degree_data$subjects_data

        # --- Itinerary Filtering Logic ---
        selected_itinerary_path <- input$itinerary # Value is "1", "2", "3", "4", "5", or "0" (Not Sure)
        final_enrollable_pool <- enrollable # Start with the initial list

        if (selected_itinerary_path == "0") { # "Not Sure" selected
          # Target: B, O, C and Practicas (05.615)
          target_codes <- all_subjects_data %>%
            filter(type %in% c("B", "O", "C") | subject_code == "05.615") %>%
            pull(subject_code) %>%
            unique()

          available_uncompleted_target_subjects <- intersect(
            setdiff(target_codes, passed_or_transferred_codes),
            enrollable$subject_code # Must be available in current semester/status
          )

          if (length(available_uncompleted_target_subjects) > 0) {
            # If B/O/Practicas are available, recommend ONLY from them
            final_enrollable_pool <- enrollable %>% filter(subject_code %in% available_uncompleted_target_subjects)
          } else {
            # If all B/O/Practicas done, recommend from anything available in enrollable (P, T included)
            final_enrollable_pool <- enrollable
          }

        } else { # Specific Itinerary selected
          # Target: B, O, C, and P matching the itinerary path
          target_codes_for_itinerary <- all_subjects_data %>%
            filter(type %in% c("B", "O", "C") | (type == "P" & str_detect(path, selected_itinerary_path))) %>%
            pull(subject_code) %>%
            unique()

          # Add Practicas (05.615) if it's enrollable and not already included (it's P/path 0)
          practicas_code <- "05.615"
          if (!(practicas_code %in% target_codes_for_itinerary) && (practicas_code %in% enrollable$subject_code)) {
              target_codes_for_itinerary <- c(target_codes_for_itinerary, practicas_code)
          }

          available_uncompleted_target_subjects_for_itinerary <- intersect(
             setdiff(target_codes_for_itinerary, passed_or_transferred_codes),
             enrollable$subject_code # Must be available in current semester/status
          )

          if (length(available_uncompleted_target_subjects_for_itinerary) > 0) {
            # If relevant B/O/P are available, recommend ONLY from them
            final_enrollable_pool <- enrollable %>% filter(subject_code %in% available_uncompleted_target_subjects_for_itinerary)
          } else {
            # If all relevant B/O/P done, recommend from anything available in enrollable
            final_enrollable_pool <- enrollable
          }
        }
        # --- End Itinerary Filtering Logic ---

        # Check if the final pool is empty after filtering
        if (nrow(final_enrollable_pool) == 0) {
            # Use shiny::showNotification for user feedback
            showNotification(
               translate(language, "No subjects available for recommendation based on your current status and itinerary selection."),
               type = "warning",
               duration = 5 # Show for 5 seconds
            )
            recommended_list$subject_code <- character() # Clear recommendations
            # Keep user in Step 1 (preferences) if no recommendations found
            enrollment_step$current_step <- 1
            session$sendCustomMessage(type = "steps",  message = paste0("step",enrollment_step$current_step))
            session$sendCustomMessage(type = "show",  message = ".step-sliders") # Ensure sliders are visible
            session$sendCustomMessage(type = "show",  message = ".step-recommend") # Ensure recommend button area is visible
            session$sendCustomMessage(type = "hide",  message = ".selecciona_asignatures") # Hide selection area
            return() # Stop further processing
        }
        
        # recomendador basado en distancias
        if (input$recommender==translate(language,"Distance")) {
          failed <- final_enrollable_pool  %>% filter(subject_mark %in% c("SU","NP", translate(language, "Fail"))) # Use filtered pool
          # JULIA: 03/10/2023
          #passed <- as_tibble(subject_positions) %>% filter(subject_mark %in% c("A","NO","EX","M"))
          passed <- as_tibble(subject_positions) %>% filter(subject_mark %in% c("A","NO","EX","M", translate(language, "Pass")))	  
          transferred <- as_tibble(subject_positions) %>% filter(subject_mark %in% c(translate(language, "Transfer")))
          completed_subjects <- rbind(passed, transferred)
          completed_subjects_n <- length(completed_subjects$subject_code)
          subject_distances = tibble(subject_code = final_enrollable_pool$subject_code, d = 0)

          # para cada asignatura matriculable (in the filtered pool)
          for(current_subject in final_enrollable_pool$subject_code){
            total_distance = 0
            # acumular la distancia a todas las asignaturas ya superadas o convalidadas
            current_x <- final_enrollable_pool[final_enrollable_pool$subject_code==current_subject,'x'] # Use filtered pool
            current_y <- final_enrollable_pool[final_enrollable_pool$subject_code==current_subject,'y'] # Use filtered pool
            if (nrow(completed_subjects) > 0) { # Avoid loop if no completed subjects
            for(completed_subject in completed_subjects$subject_code){
              completed_x <- completed_subjects[completed_subjects$subject_code==completed_subject,'x']
              completed_y <- completed_subjects[completed_subjects$subject_code==completed_subject,'y']
              
              # JULIA 18/11/2023 fòrmula errònia!!!
              subject_distance=sqrt((current_x-completed_x)^2+(current_y-completed_y)^2)
              
              # si se trata de una asignatura suspendida, "forzar" que sea
              # más probable recomendarla reduciendo la distancia
                if(current_subject %in% failed$subject_code){ # Check against codes in failed df
                subject_distance=subject_distance/input$failed_subjects_distance_adjustment
              }
              total_distance <- total_distance + subject_distance
            }
              # guardar la distancia promedio calculada (only if completed_subjects_n > 0)
              if (completed_subjects_n > 0) {
                 subject_distances[subject_distances$subject_code==current_subject,'d'] = total_distance / completed_subjects_n
              } # If no completed subjects, distance remains 0
            }
          }
          
          if(length(passed$subject_code)>0 | length(transferred$subject_code)>0){
            arranged_distances <- subject_distances %>% arrange(d)
          }else{
            arranged_distances <- subject_distances %>% arrange(desc(d))
          }
          
          # PREREQUISITE CHECK
          if (input$recommender==translate(language,"Distance")) {
            # Get prerequisite data
            prerequisites <- degree_data$prerequisites_data

            # Add subject type/path info needed for prioritization later
            subject_details_for_merge <- degree_data$subjects_data %>%
              dplyr::select(subject_code, type, path)
            arranged_distances <- left_join(arranged_distances, subject_details_for_merge, by = "subject_code")

            # --- Calculate Earned ECTS for recommendation check ---
            earned_marks_for_ects <- c("A", "NO", "EX", "M", "Reconeguda", translate(language, "Pass"), translate(language, "Transfer"))
            earned_ects_for_rec <- 0 # Default to 0
            if (!is.null(degree_data$student_data)) {
              passed_or_transferred_codes_for_ects <- degree_data$student_data %>%
                filter(subject_mark %in% earned_marks_for_ects) %>%
                pull(subject_code) %>%
                unique()
              if (length(passed_or_transferred_codes_for_ects) > 0) {
                earned_ects_for_rec <- degree_data$subjects_data %>%
                  filter(subject_code %in% passed_or_transferred_codes_for_ects) %>%
                  summarise(total_ects = sum(credits, na.rm = TRUE)) %>%
                  pull(total_ects)
              }
            }
            if (!is.numeric(earned_ects_for_rec) || is.na(earned_ects_for_rec)) {
              earned_ects_for_rec <- 0
            }
            # --- End ECTS Calculation ---

            # --- Check Prerequisites AND Restrictions for ALL potential recommendations --- 
            # Add columns to arranged_distances to store check results
            arranged_distances <- arranged_distances %>%
              mutate(
                  meets_std_prereqs = NA,
                  meets_restrictions = NA, # Combined check for MIN_ECTS and ANTIREQUISITE
                  credits = NA
              )
            
            # Get restrictions data
            restrictions <- degree_data$restrictions_data

            for(idx in 1:nrow(arranged_distances)) {
              potential_subject_code <- arranged_distances$subject_code[idx]

              # 1. Check standard prerequisites (from prerequisites_data)
              subject_prereqs <- prerequisites %>% 
                filter(subject_code == potential_subject_code) %>% 
                pull(prerequisite_code)
              all_standard_prereqs_met <- TRUE
              if (length(subject_prereqs) > 0) {
                all_standard_prereqs_met <- all(subject_prereqs %in% passed_or_transferred_codes)
              }
              arranged_distances$meets_std_prereqs[idx] <- all_standard_prereqs_met

              # 2. Check restrictions (MIN_ECTS, ANTIREQUISITE from restrictions_data)
              meets_all_restrictions <- TRUE # Assume true unless a restriction fails
              if (!is.null(restrictions)) {
                # Find rules specifically for this potential subject
                subject_rules <- restrictions %>% filter(restricted_subject_code == potential_subject_code)
                if (nrow(subject_rules) > 0) {
                  # Normally, a subject would have only one row in the restrictions CSV,
                  # but iterate just in case (though the logic below assumes one rule)
                  for (r_idx in 1:nrow(subject_rules)) { 
                    rule <- subject_rules[r_idx, ]
                    
                    # Check MIN_ECTS rule
                    if (!is.na(rule$min_ects)) {
                      if (earned_ects_for_rec < rule$min_ects) {
                        meets_all_restrictions <- FALSE
                        break # Restriction failed, no need to check others for this subject
                      }
                    }
                    
                    # Check ANTIREQUISITE rule (only if MIN_ECTS didn't already fail)
                    if (meets_all_restrictions && !is.na(rule$antirequisites)) { # Check meets_all_restrictions instead of !apply_restriction
                      blocking_codes_rule <- unlist(str_split(rule$antirequisites, ";"))
                      # Filter out empty strings that might result from splitting
                      blocking_codes_rule <- blocking_codes_rule[blocking_codes_rule != ""]
                      # Find which of these the student has actually passed
                      passed_blocking_codes <- intersect(blocking_codes_rule, passed_or_transferred_codes)
                      
                      if (length(passed_blocking_codes) > 0) {
                        # Set restriction flag to FALSE and break
                         meets_all_restrictions <- FALSE
                         break # Restriction failed
                      }
                    }
                  }
                }
              }
              arranged_distances$meets_restrictions[idx] <- meets_all_restrictions

              # 3. Store credits (needed for budget check later)
              subject_credits_val <- degree_data$subjects_data %>%
                 filter(subject_code == potential_subject_code) %>%
                  pull(credits)
              arranged_distances$credits[idx] <- ifelse(length(subject_credits_val) > 0 && is.numeric(subject_credits_val[1]), subject_credits_val[1], NA)
            }

            # Filter down to valid candidates based on prereqs AND restrictions
            valid_candidates <- arranged_distances %>%
                filter(meets_std_prereqs == TRUE & meets_restrictions == TRUE & !is.na(credits))
 
            # --- Build Final Recommendation List with Simplified Prioritization ---
            filtered_recommendations <- character() # Initialize empty list
            num_recommendations_needed <- 6      # Max subjects
            cumulative_ects <- 0                  # Track ECTS
            desired_ects <- input$desired_ects    # Target ECTS
            selected_itinerary_path <- input$itinerary # User selection

            # Identify failed subjects from student data if available
            failed_subject_codes <- character()
            if (!is.null(degree_data$student_data)) {
              fail_marks_check <- c("SU", "NP", translate(language, "Fail"))
              failed_subject_codes <- degree_data$student_data %>%
                filter(subject_mark %in% fail_marks_check) %>%
                pull(subject_code) %>%
                unique()
            }
 
            add_subject_to_recommendation <- function(code, ects) {
               # Helper to add subject and update state if budget/limit allows
               # Returns TRUE if added, FALSE otherwise
               if (length(filtered_recommendations) < num_recommendations_needed &&
                   (cumulative_ects + ects <= desired_ects || length(filtered_recommendations) == 0)) # Allow first even if slightly over
               {
                 filtered_recommendations <<- c(filtered_recommendations, code)
                 cumulative_ects <<- cumulative_ects + ects
                 return(TRUE)
               } else {
                 return(FALSE)
               }
            }

            # --- Prioritization Passes --- 

            # Pass 1: Prioritize Failed Subjects (that are valid candidates)
            if (length(failed_subject_codes) > 0) {
               for (i in 1:nrow(valid_candidates)) {
                  candidate_code <- valid_candidates$subject_code[i]
                  candidate_ects <- valid_candidates$credits[i]
                  if (candidate_code %in% failed_subject_codes) {
                     added <- add_subject_to_recommendation(candidate_code, candidate_ects)
                  }
                  if (length(filtered_recommendations) >= num_recommendations_needed) break
               }
            }

            # Pass 2: Prioritize Matching Itinerary Electives (if itinerary selected)
            if (selected_itinerary_path != "0" && length(filtered_recommendations) < num_recommendations_needed) {
                for (i in 1:nrow(valid_candidates)) {
                   candidate_code <- valid_candidates$subject_code[i]
                   candidate_type <- valid_candidates$type[i]
                   candidate_path <- as.character(valid_candidates$path[i]) # Ensure character
                   candidate_ects <- valid_candidates$credits[i]
                   is_matching_elective <- (candidate_type == "P" && str_detect(candidate_path, selected_itinerary_path))

                   if (is_matching_elective && !(candidate_code %in% filtered_recommendations)) {
                      added <- add_subject_to_recommendation(candidate_code, candidate_ects)
                   }
                   if (length(filtered_recommendations) >= num_recommendations_needed) break
                }
            }

            # Pass 3: Add Remaining Valid Subjects (fills for "Not Sure" or remaining slots)
            if (length(filtered_recommendations) < num_recommendations_needed) {
               for (i in 1:nrow(valid_candidates)) {
                   candidate_code <- valid_candidates$subject_code[i]
                   candidate_ects <- valid_candidates$credits[i]
                   if (!(candidate_code %in% filtered_recommendations)) {
                      added <- add_subject_to_recommendation(candidate_code, candidate_ects)
                   }
                   if (length(filtered_recommendations) >= num_recommendations_needed) break
                }
            }

            # --- End Recommendation Building ---

            # Assign the final list
            recommended_list$subject_code <- filtered_recommendations
          } else {
            # Original random recommender logic (unchanged)
            recommended_list$subject_code <-
                final_enrollable_pool %>% # Use filtered pool for random sampling too
                sample_n(min(6, nrow(final_enrollable_pool))) %>% # Sample max 6 or fewer if pool is smaller
                pull(subject_code)
          }
        }
        
        # show_translated_notice(language, "The system has marked in shades of green the ranking of the most recommended subjects for your enrollment.")
        session$sendCustomMessage(type = "hide",  message = ".cal_asignatures")
        session$sendCustomMessage(type = "hide", message = ".workload_asignatures")
        session$sendCustomMessage(type = "hide", message = ".download_asignatures")
        session$sendCustomMessage(type = "hide", message = ".widgets_cal")
        session$sendCustomMessage(type = "show",  message = ".selecciona_asignatures") # Ensure selection step UI is visible
        # Hide the scroll actionLink when going back to step 2
        shinyjs::hide("scroll_to_cal_container")
      })
      
      # Events: Previous, continue, search, etc -----------------------------------------------------------------
      
      # --- Observe click on the scroll-down link ---
      observeEvent(input$scroll_to_cal_container, {
        # Use shinyjs to run JavaScript that scrolls to the calendar plot container
        shinyjs::runjs(sprintf(
          "document.getElementById('%s').scrollIntoView({ behavior: 'smooth' });",
          session$ns("cal_plot_container")
        ))
      })
      # ---------------------------------------------

      observeEvent(input$previous1, {
        clicked_list$subject_code = character() 
        discarded_list$subject_code = character()
        selected_list$subject_code = character()
        recommended_list$subject_code = character()
        enrollment_step$current_step <- 0
        session$sendCustomMessage(type = "steps",  message = paste0("step",enrollment_step$current_step))
        session$sendCustomMessage(type = "hide",  message = ".cal_asignatures")
        session$sendCustomMessage(type = "hide", message = ".workload_asignatures")
        session$sendCustomMessage(type = "hide", message = ".download_asignatures")
        session$sendCustomMessage(type = "hide", message = ".widgets_cal")
        updateSliderInput(session, "workload", value = 15) # Reset workload slider
        # Hide the scroll actionLink
        shinyjs::hide("scroll_to_cal_container")
      })
      
      observeEvent(input$previous2, {
        selected_list$subject_code = character()
        recommended_list$subject_code = character()
        enrollment_step$current_step <- 1
        session$sendCustomMessage(type = "steps",  message = paste0("step",enrollment_step$current_step))
        session$sendCustomMessage(type = "show",  message = ".step-sliders")
        session$sendCustomMessage(type = "show",  message = ".step-recommend")
        session$sendCustomMessage(type = "hide",  message = ".selecciona_asignatures")
        session$sendCustomMessage(type = "hide",  message = ".cal_asignatures")
        session$sendCustomMessage(type = "hide", message = ".workload_asignatures")
        session$sendCustomMessage(type = "hide", message = ".download_asignatures")
        session$sendCustomMessage(type = "hide", message = ".widgets_cal")
        updateSliderInput(session, "workload", value = 15) # Reset workload slider
        # Hide the scroll actionLink
        shinyjs::hide("scroll_to_cal_container")
      })
      
      observeEvent(input$continue_button, {
        enrollment_step$current_step <- 1
        session$sendCustomMessage(type = "steps",  message = paste0("step",enrollment_step$current_step))
      })
      
      observeEvent(input$subject_type, {
        updateTextInput(session, "search_subject", value = "")
      })
      
      # Reset enrollment process when student changes
      observeEvent(input$idp, {
        # Ignore initial load ("---") or null value
        req(input$idp != "---") 
        
        # Reset reactive values
        clicked_list$subject_code = character() 
        discarded_list$subject_code = character()
        selected_list$subject_code = character()
        recommended_list$subject_code = character()
        
        # Reset step counter
        enrollment_step$current_step <- 0
        
        # Reset sliders to default values
        updateSliderInput(session, "difficulty", value = 1)
        updateSliderInput(session, "popularity", value = 1)
        updateSliderInput(session, "overlap", value = 1)
        updateSliderInput(session, "workload", value = 15) # Reset workload slider
        updateSliderInput(session, "desired_ects", value = 30) # Reset ECTS slider
        updateSelectInput(session, "itinerary", selected = "0") # Reset Itinerary dropdown
        
        # Reset UI elements to initial state (Step 0)
        session$sendCustomMessage(type = "steps",  message = paste0("step", enrollment_step$current_step))
        session$sendCustomMessage(type = "hide",  message = ".cal_asignatures")
        session$sendCustomMessage(type = "hide", message = ".workload_asignatures")
        session$sendCustomMessage(type = "hide", message = ".download_asignatures")
        session$sendCustomMessage(type = "hide", message = ".widgets_cal")
        session$sendCustomMessage(type = "hide",  message = ".selecciona_asignatures")
        session$sendCustomMessage(type = "hide",  message = ".step-sliders")
        session$sendCustomMessage(type = "hide",  message = ".step-recommend")
        session$sendCustomMessage(type = "show",  message = ".step-convalida")
        # Hide the scroll arrow on student change/reset
        session$sendCustomMessage(type = "hide", message = paste0("#", session$ns("scroll_to_cal_container")))
        
      }, ignoreNULL = TRUE, ignoreInit = TRUE)
      
      
      # Events: Descarregar i screenshot-----------------------------------------------------------------
      
      observeEvent(input$download, {
        enrollment_step$current_step <- 4
        session$sendCustomMessage(type = "hide",  message = ".tabbable")
        session$sendCustomMessage(type = "hide",  message = ".well")
        session$sendCustomMessage(type = "hide",  message = ".steps")
        session$sendCustomMessage(type = "hide",  message = ".graf")
        session$sendCustomMessage(type = "hide",  message = ".cal_asignatures")
        session$sendCustomMessage(type = "hide", message = ".workload_asignatures")
        session$sendCustomMessage(type = "hide", message = ".download_asignatures")
      })
      
      observeEvent(input$screenshot, {
        shinyscreenshot::screenshot(filename = "visualenrollment")
      })
      
    }
  )
}
