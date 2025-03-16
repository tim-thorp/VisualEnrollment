
informaticaUI <- function(id, lang) {
  ns <- NS(id)
  tagList(
    fluent_header(
      lang = lang,
      title = "Visual Enrollment", 
      tags$ul(
        class = paste0("language ",lang),
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
          #uiOutput(ns("graf_click_info"), class ="graf_click_info"),
          
          ### OPCIONS USUARI
        
          # Steps
          tags$div(
            class="step-convalida",
            h4(translate(
              lang,
              "Here's your academic status. Mark subjects as validated or discarded (it is optional)"
            )),
            actionButton(
              ns("next_button"),
              label = translate(lang, "Next"),
              onclick = "$('.step-convalida').hide();$('.step-sliders').show();$('.step-recommend').show();"
            )
            
          ),
          tags$div(
            class="step-sliders",
            h4(translate(lang, "Assign a value to the topics based on their importance:")),
            tags$div(
              title=translate(lang,"Separate the most difficult subjects"),
              style="height: 72px;",
              sliderInput(
                ns("dificultat"),
                h4(translate(lang,"Difficulty:"), icon("info")),
                min = 1, 
                max = 5, 
                value = 1,
                step = 1,
                ticks=F
              )
            ),
            tags$div(
              tags$span(translate(lang, "Less important")),
              tags$span(translate(lang, "More important"), style="float: right"),
              style = "margin-top: 10px"
            ),
            br(),
            tags$div(
              title=translate(lang,
                "Separate subjects that are never enrolled together"
              ),
              style="height: 72px;",
              sliderInput(
                ns("popularitat"),
                h4(translate(lang, "Popularity:"), icon("info")),
                min = 1,
                max = 5,
                value = 1,
                step = 1,
                ticks=F
              )
            ),
            tags$div(
              tags$span(translate(lang, "Less important")),
              tags$span(translate(lang, "More important"), style="float: right"),
              style = "margin-top: 10px"
            ),
            br(),
            tags$div(
              title=translate(lang, 
                "Separate subjects that are taken in different semesters"
              ),
              style="height: 72px;",
              sliderInput(
                ns("requisit"),
                h4(translate(lang, "Previous requirements:"), icon("info")),
                min = 1,
                max = 5,
                value = 5,
                step = 1,
                ticks=F
              )
            ),
            tags$div(
              tags$span(translate(lang, "Less important")),
              tags$span(translate(lang, "More important"), style="float: right"),
              style = "margin-top: 10px"
            ),
            br(),
            tags$div(
              title=translate(lang, 
                "Separate the subjects that have more overlapping deadlines"
              ),
              style="height: 72px;",
              sliderInput(
                ns("overlap"),
                h4(translate(lang, "Overlaps between deadlines:"), icon("info")),
                min = 1,
                max = 5, 
                value = 1,
                step = 1,
                ticks=F
              )
            ),
            tags$div(
              tags$span(translate(lang, "Less important")),
              tags$span(translate(lang, "More important"), style="float: right"),
              style = "margin-top: 10px"
            ),
            br()
          ),
          tags$div(
            class="step-recommend",
            actionButton(
              ns("previous1"),
              translate(lang, "Previous"),
              onclick = "$('.step-convalida').show();$('.step-sliders').hide();$('.step-recommend').hide();"
            ),
            actionButton(
              ns("recommend"),
              translate(lang, "Recommend"),
              onclick = "$('.step-convalida').hide();$('.step-sliders').hide();$('.step-recommend').hide();$('.selecciona_asignatures').show()"
            ),
          ),
          div(
            class="selecciona_asignatures",
            tagList(
              h4(translate(lang, 
                "The system has recommended in shades of green the most appropriate subjects to enroll according to your preferences."
              )),
              h4(translate(lang, 
                "Select on the map up to 6 subjects that you want to enroll."
              )),
              uiOutput(ns("uiTipologia")),
              uiOutput(ns("uiBuscar")),
              actionButton(
                ns("previous2"),
                translate(lang, "Previous")
              )
            )
          ),
          NULL
        ),
        fluent_main( #  fluent_main mainPanel
         tabsetPanel(
           type="tabs", 
           selected=translate(lang, "Configuration"),
           tabPanel(
             translate(lang, "Subject recommender"), 
             fluent_grid(
              fluent_grid_item(
                class = "steps step0",
                tags$ol(
                  tags$li(class="step0", translate(lang, "Discard")),
                  tags$li(class="step1", translate(lang, "Preferences")),
                  tags$li(class="step2", translate(lang, "Recommendations")),
                  tags$li(class="step3", translate(lang, "Selection"))
                )
              ),
              fluent_grid_item(
               # JULIA 23/12/2022 cambiar h2 por h4
               h4(translate(lang, "Map of subjects")),
               div(
                class = "graf",
                  tagList( 
                    plotOutput(
                      ns("grafic"),
                      click=ns("graf_click"),
                      hover=ns("graf_hover"),
                      height = "auto"
                    )
                  )
                ),
                uiOutput(ns("graf_hover_info"), 
                  class ="graf_hover_info", 
                  # JULIA 07/01/2023 cambiar el estilo en la propia función
                  #style="position:absolute;top:0;left:0"
                ),
               ),
               NULL
             )
           ),
           tabPanel(
             translate(lang, "Academic record"), 
             fluent_grid(
               fluent_grid_item(
                 div(
                   tagList( 
                     uiOutput(
                       ns("expedient"),
                       height = "auto"
                     )
                   )
                 ),
               ),
             )
           ),
           tabPanel(
             translate(lang, "Configuration"),
             
             selectInput(
               ns("recommender"),
               translate(lang, "Choose recommender:"),
               choices=c(translate(lang,"Distance"),translate(lang,"Random")),
               selected=translate(lang,"Distance")
             ),

             tags$div(
               title="Impose semestral organization", 
               style="height: 72px;",
               sliderInput(
                 ns("semestre"),
                 translate(lang, "Importance of semi-annual organization:"),
                 # JULIA 18/12/2022 forzar la organizacion semestral a 5
                 min = 1,
                 max = 5,
                 value = 5,
                 step = 1,
                 ticks=F
               )
             ),
             
             sliderInput(
               ns("bubbles"),
               translate(lang, "Max radius for bubbles:"),
               # JULIA 02/01/2023 de momento a 0 para ver el Voronoi
               min = 0,
               max = 1, 
               value = 0,
               step = 0.1,
               ticks=F
             ),

             
             sliderInput(
               ns("distancia_suspeses"),
               translate(lang, 
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
             uiOutput(ns("uiGrau")),
             
             # # TFM
             # tags$div(
             #   title="TFM", 
             #   style="height: 72px;",
             #   textInput(
             #     ns("tfm"),
             #     translate(lang, "TFM:"),
             #     value = "05.616"
             #   )
             # ),
             
             # Semestre 1 o 2
             uiOutput(ns("uiSem")),
             
             # Estudiants per carregar expedient
             uiOutput(ns("uiEst")),
             
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
              h4(translate(lang,
                "Calculate the workload of the selected subjects."
              )),
              sliderInput(
                ns("workload"),
                translate(lang, "You can specify the number of days you think you are going to dedicate to each activity:"),
                min = 1,
                max = 28,
                value = 1
              )
            )
          ),
          div(
            class="download_asignatures",
            h4(translate(lang, "When you have selected the most suitable combination of subjects, you can download your enrollment configuration.")),
            actionButton(ns("descargar"), translate(lang, "Download enrolment"))
          )
        ),
        fluent_main(
          div(
            class = "cal_asignatures ms-Grid-col ms-sm12 cal",
            tagList(
              h2(translate(lang, "Activity calendar")),
              plotOutput(ns("cal"))
            )
          ),
          width = 10
        )
      ),
      uiOutput(ns("uiEnrollment"))
    ),
    NULL
  )
}