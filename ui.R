

sidebar <- dashboardSidebar(width = 300,
  
  # INTRODUCTION                          
  div(style = "text-align: center;",h3("Lions")),                          
                        
  div(style = "padding: 10px;","Lions rule the the Serengeti. Throughout the year they live in prides which hunt together across the the plains.  
       While they are fast enough to catch a gazelle, and strong enough to destroy animals twice their size, 
       the lions' true power comes from their ability to work together. No animal is safe."),     
  
  br(),
  
  div(style = "padding: 10px;","Use this site to explore life in the Serengeti, and how the risks of predation for different species of prey change across the landscape."),
  
  HTML("<hr>"),
  
  #SELECT SEASON
  div(style = "text-align: center;",h3("Seasons")),   
  
  radioButtons("radio_season_wetdry",
               "Choose Season:",
               c("Wet","Dry"),
               inline = TRUE,
               selected = init_selected_season),
  
  uiOutput("detail_season_wetdry"),
                            
  HTML("<hr>"),
  
  #SELECT PREY SPECIES
  div(style = "text-align: center;",h3("Prey")),    
  
  selectInput("select_prey_species",
              "Select Prey Species:",
              choices = setNames(as.list(Species[species_type == "Prey",][order(species_label)]$species),
                                 Species[species_type == "Prey",][order(species_label)][, .(species_label_color = paste0(species_label," (Color: ",species_color,")")),]$species_label_color),
              multiple = FALSE,
              selected = init_selected_prey),
  
  uiOutput("detail_species")#,                                
  
  #HTML("<hr>"),
  
  
  #selectInput("select_landscape_variable",
  #            "Select Landscape Feature:",
  #            LandscapeVariableDetail$land_var_type,
  #            multiple = FALSE), 
  
  #div(style = "padding: 10px;", uiOutput("detail_landscape_var_type"))
  
  
)

body <- dashboardBody(
  
  tags$style(type="text/css",
             ".recalculating {opacity: 1.0;}"),
  
  #TOP ROW - 1 FULL-WIDTH COLUMN
  fluidRow(
    
    column(width = 12,
           
           #MAP
           tabBox(width = NULL, side = "right",
                  title = p(textOutput("map_title")),
                  tabPanel(p(actionButton("button_include_all_sites", "Include/Exclude All Sites", class = "btn-xs")),
                           "Select a camera trap site by clicking on its marker... click the marker again to deactivate (or reactivate) the site.",
                           leafletOutput("selectized_cameratrap_map"))
                  )
           
           #box(width = NULL,
          #     div(style = "display: inline-block;vertical-align:top; width: 200px;",
           #        selectInput("select_yaxis_analysis_plot","Y-Axis", choices = c("*Histogram*", "Prey Abundance", "Lion Abundance", "Kill-Based Risk", "Habitat-Based Risk"))),
               
            #   div(style="display: inline-block;vertical-align:top; width: 75px;",HTML("<br>")),
                 
             #  div(style = "display: inline-block;vertical-align:top; width: 200px;",
              #     selectInput("select_xaxis_analysis_plot", "X-Axis", choices = c("Landscape Feature", "Prey Abundance", "Lion Abundance", "Kill-Based Risk", "Habitat-Based Risk")))
               
               #plotOutput("var.plot")
           )
    ),
  
  #BOTTOM ROW - 2 HALF-WIDTH COLUMNS  
  fluidRow(
    column(width = 6, 
           tabBox(title = p(textOutput("snapshot_title_site")), width = NULL, side = "right", #add highlighted camera trap ID? 
                  
                  tabPanel(p(textOutput("snapshot_title_lion")), 
                           
                           imageOutput("snapshot_image_lion"),
                           
                           sliderInput("sliding_image_number_lion", 
                                       label = NULL, ticks = TRUE,
                                       min = 1, max = 1,
                                       value = 1, step = 1,
                                       width = '100%')),
                  
                  tabPanel(p(textOutput("snapshot_title_prey")),
                           
                           imageOutput("snapshot_image_prey"),
                           
                           sliderInput("sliding_image_number_prey", 
                                       label = NULL, ticks = TRUE, 
                                       min = 1, max = 1,
                                       value = 1, step = 1,
                                       width = '100%')))
           ),
    
    column(width = 6, 
           box(title = "Capture Rate", width = NULL,
               
               plotOutput("capture_timedensity_plot"), 
               
               "(Only activated camera traps are represented. Minimum: 3 sites)")
           )
    )
)


ui <- dashboardPage(skin = "yellow",
  dashboardHeader(title = "Topigraphic", titleWidth = 300), 
  sidebar,
  body
)

