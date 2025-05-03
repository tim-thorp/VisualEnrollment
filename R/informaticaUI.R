subjectEnrollmentUI <- function(id, language) {
  namespace <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fluent_header(
      lang = language,
      title = "Visual Enrollment", 
      tags$ul(
        class = paste0("language ",language),
        tags$li(a(class = "item ca", href = route_link("ca"), "Català")),
        tags$li(a(class = "item es", href = route_link("es"), "Castellano")),
        tags$li(a(class = "item en", href = route_link("en"), "English"))
      )
    ),
    fluent_grid(
      fluent_grid_item(
        fluent_widgets(
          # sidebarPanel sidebarPanel
          # generar les opcions de menú
          
          # Accions amb asignatura clicada
          #uiOutput(namespace("graf_click_info"), class ="graf_click_info"),
          
          ### OPCIONS USUARI
        
          # Steps
          tags$div(
            class="step-convalida",
            h4(translate(
              language,
              "We've loaded your academic record. Select subjects you plan to transfer credit for, or do not plan to take. This step is optional. Click 'Continue' when ready."
            )),
            actionButton(
              namespace("continue_button"),
              label = translate(language, "Continue"),
              onclick = "$('.step-convalida').hide();$('.step-sliders').show();$('.step-recommend').show();"
            )
            
          ),
          tags$div(
            class="step-sliders",
            style="margin-bottom: 15px;",
            # --- Main Settings ---
            h4(tags$strong(translate(language, "Main Settings")), style="margin-bottom: 15px;"),
            tags$div(
              title=translate(language, "Select ECTS for enrollment. 1 subject is usually 6 ECTS, with exceptions like internships or final projects."),
              style="height: 72px; margin-bottom: 40px;",
              sliderInput(
                namespace("desired_ects"),
                h4(translate(language, "Desired ECTS for Enrollment:"), icon("info-circle")),
                min = 6,
                max = 36,
                value = 30,
                step = 6,
                ticks = TRUE
              )
            ),
            # --- Itinerary Dropdown ---
            tags$div(
              title=translate(language, "Selecting a track will prioritize recommending subjects from that branch. Choose 'Not Sure' if you haven't decided yet."),
              style="height: 72px; margin-bottom: 40px;",
              uiOutput(namespace("uiItinerary"))
            ),
            # --- Advanced Settings (Map Appearance) ---
            hr(), # Add a horizontal rule separator
            h4(tags$strong(translate(language, "Advanced Settings")), style="margin-top: 20px; margin-bottom: 15px;"),
            p(tags$small(translate(language, "Rate the importance of the following criteria for separating subjects you prefer not to take together:")), style="font-style: italic; margin-bottom: 20px;"),
            # --- END Itinerary Dropdown ---
            tags$div(
              title=translate(language,"Separate subjects often failed when taken together"),
              sliderInput(
                namespace("difficulty"),
                tags$label(tags$strong(translate(language,"Difficulty:")), icon("info-circle")),
                min = 1, 
                max = 5, 
                value = 1,
                step = 1,
                ticks=F
              ),
            ),
            tags$div(
              title=translate(language, 
                "Separate subjects that are rarely enrolled together"
              ),
              sliderInput(
                namespace("popularity"),
                tags$label(tags$strong(translate(language, "Popularity:")), icon("info-circle")),
                min = 1,
                max = 5,
                value = 1,
                step = 1,
                ticks=F
              )
            ),
            tags$div( # Start of containing div
              title=translate(language,
                "Separate the subjects that have more overlapping deadlines"
              ),
              sliderInput(
                namespace("overlap"),
                tags$label(tags$strong(translate(language, "Assignment Deadline Overlaps:")), icon("info-circle")),
                min = 1,
                max = 5,
                value = 1,
                step = 1,
                ticks=F
              ), # End sliderInput
              style="margin-bottom: 0px;"
            ), # End of containing div
            # Add min/max labels below the last slider for context
            tags$div(
              tags$span(translate(language, "Less important")),
              tags$span(translate(language, "More important"), style="float: right")
            )
          ),
          tags$div(
            class="step-recommend",
            actionButton(
              namespace("previous1"),
              translate(language, "Previous"),
              onclick = "$('.step-convalida').show();$('.step-sliders').hide();$('.step-recommend').hide();"
            ),
            actionButton(
              namespace("recommend"),
              translate(language, "Recommend Subjects"),
              onclick = "$('.step-convalida').hide();$('.step-sliders').hide();$('.step-recommend').hide();$('.selecciona_asignatures').show()"
            ),
          ),
          div(
            class="selecciona_asignatures",
            tagList(
              h4(translate(language, 
                "The system has recommended in shades of green the most appropriate subjects to enroll according to your preferences."
              )),
              h4(translate(language, 
                "On the map, select the subjects that you want to enroll in."
              )),
              actionButton(
                namespace("previous2"),
                translate(language, "Previous")
              )
            )
          ),
          NULL
        ),
        fluent_main( #  fluent_main mainPanel
         tabsetPanel(
           id=namespace("main_tabs"),
           type="tabs", 
           selected=translate(language, "Configuration"),
           tabPanel(
             translate(language, "Subject recommender"), 
             fluent_grid(
              fluent_grid_item(
                # Use columns for layout: 8 for plot, 4 for legend
                class = "ms-Grid-col ms-sm12 ms-md12 ms-lg8",
                # Step indicators
                tags$div(
                  class = "steps step0",
                  tags$ol(
                    tags$li(class="step0", translate(language, "Discard Subjects")),
                    tags$li(class="step1", translate(language, "Preferences")),
                    tags$li(class="step2", translate(language, "Recommendations")),
                    tags$li(class="step3", translate(language, "Selection"))
                  )
                ),
                div(
                  class = "graf",
                  tagList( 
                    plotOutput(
                      namespace("grafic"),
                      click=namespace("graf_click"),
                      hover=namespace("graf_hover"),
                      height = "auto"
                    )
                  )
                ),
                # Wrap the actionLink in a div for centering
                tags$div(
                  style = "text-align: center; margin-top: 5px;",
                  actionLink(
                    inputId = namespace("scroll_to_cal_container"),
                    label = tagList(
                      icon("chevron-down", style = "font-size: 2.0em; vertical-align: middle; margin-right: 5px;"),
                      tags$span(translate(language, "Continue"), style = "font-size: 1.5em; vertical-align: middle;")
                    ),
                    style = "display: inline-block; text-decoration: none; color: inherit;",
                    class = "scroll-link-hidden"
                  )
                ),
                uiOutput(namespace("graf_hover_info"), 
                  class ="graf_hover_info", 
                  # JULIA 07/01/2023 cambiar el estilo en la propia función
                  #style="position:absolute;top:0;left:0"
                ),
              ),
              fluent_grid_item(
                # Use columns for layout: 4 for legend
                class = "ms-Grid-col ms-sm12 ms-md12 ms-lg4",
                # Dynamic search/filter controls
                uiOutput(namespace("search_controls_ui")),
                uiOutput(namespace("uiLegend"))
              )
             )
           ),
           tabPanel(
             translate(language, "Academic record"), 
             fluent_grid(
               fluent_grid_item(
                 div(
                   tagList( 
                     uiOutput(
                       namespace("academic_record"),
                       height = "auto"
                     )
                   )
                 ),
               ),
             )
           ),
           tabPanel(
             translate(language, "Configuration"),
             
             selectInput(
               namespace("recommender"),
               translate(language, "Choose recommender:"),
               choices=c(translate(language,"Distance"),translate(language,"Random")),
               selected=translate(language,"Distance")
             ),

             tags$div(
               title="Impose semestral organization", 
               style="height: 72px;",
               sliderInput(
                 namespace("semester"),
                 translate(language, "Importance of semester planning:"),
                 # JULIA 18/12/2022 forzar la organizacion semestral a 5
                 min = 1,
                 max = 5,
                 value = 5,
                 step = 1,
                 ticks=F
               )
             ),
             
             sliderInput(
               namespace("bubbles"),
               translate(language, "Max radius for bubbles:"),
               # JULIA 02/01/2023 de momento a 0 para ver el Voronoi
               min = 0,
               max = 1, 
               value = 0,
               step = 0.1,
               ticks=F
             ),

             
             sliderInput(
               namespace("failed_subjects_distance_adjustment"),
               translate(language, 
                 "Distance adjustment for failed or non-submitted subjects:"
               ),
               # JULIA 18/12/2022 dejarlo en bajo/medio/alto
               min = 1,
               max = 3, 
               value = 2,
               step = 1,
               ticks=F
             ),
             
             
             # Graus disponibles:
             uiOutput(namespace("uiGrau")),
             
             # Semestre 1 o 2
             uiOutput(namespace("uiSem")),
             
             # Estudiants per carregar expedient
             uiOutput(namespace("uiEst")),
             
             NULL
           )
         ),
         width = 10
        )
      ),
      fluent_grid_item(
        fluent_widgets(
          class = "widgets_cal",
          div(class = "flechita"),
          div(
            class="workload_asignatures",
            tagList(
              h4(translate(language,
                "Calculate the workload of the selected subjects."
              )),
              sliderInput(
                namespace("workload"),
                # Combine label text and tooltip icon
                label = tagList(
                  translate(language, "Optionally, specify the number of days you expect to dedicate to each activity:"),
                  # Add icon with translated tooltip text
                  icon("info-circle", 
                       title = translate(language, "We estimate that an average activity requires about two weeks of dedication (15 days). Adjust this value according to your personal planning."),
                       style = "margin-left: 5px;" # Adds space between text and icon
                  )
                ),
                min = 1,
                max = 28,
                value = 15
              )
            )
          ),
          div(
            class="download_asignatures",
            h4(translate(language, "When you have selected the most suitable combination of subjects, you can download your enrollment plan.")),
            actionButton(namespace("download"), translate(language, "Download enrollment plan"))
          )
        ),
        fluent_main(
          div(
            class = "cal_asignatures ms-Grid-col ms-sm12 cal",
            # Add a unique ID to the calendar container
            id = namespace("cal_plot_container"), 
            tagList(
              h2(translate(language, "Assignment Submission Calendar")),
              plotOutput(namespace("cal"))
            )
          ),
          width = 10
        )
      ),
      uiOutput(namespace("uiEnrollment"))
    ),
    NULL
  )
}