dashboardPage(
preloader = list(html = waiter::spin_loaders(id = 16, color = "#01125f"), color = "#b8c7c5"),   
dark = FALSE,
title = paste( "FIELDWORKER", year(Sys.Date()) ),
help = NULL, 

header = dashboardHeader(
    title = dashboardBrand(
        title = paste( pagetitle, year(Sys.Date()) ),
        image = "ICO.png"
    )
),

sidebar = dashboardSidebar(disable = TRUE),
controlbar = dashboardControlbar(

  uiOutput("clock"),
  code("Hard drive:"),
  uiOutput("hdd_state")
  ),


body = dashboardBody(

  bs4Dash::tabBox(id = "main", width = 12, collapsible =FALSE,

  tabPanel(title = "News",
    hr(),
    includeMarkdown("./www/help/news.md")
  ),

  tabPanel(title = "GPS",
    uiOutput("open_gps"),
    hr(),
    includeMarkdown("./www/help/gps.md")
  ),

  tabPanel(title = "ENTER DATA",
    uiOutput("new_data"),
    hr(),
    includeMarkdown("./www/help/enter_data.md")
  ),

  tabPanel(title = "DATABASE",
    
    div(class ="btn-toolbar btn-group-lg", style = "gap: 5px;" , 
    uiOutput("open_db")
    ),
    hr(),
    includeMarkdown("./www/help/database.md")

  ),

  tabPanel(title = "VIEW DATA", 

    bs4Dash::tabsetPanel(
      id = "tabset",
      .list = lapply(getOption('dbtabs_view'), function(i) {
        tabPanel(
          title = paste0("[", i, "]"),
          active = FALSE,
          DT::DTOutput(outputId = paste0(i, "_show"))
        )
      }) 
    )
  ),

  tabPanel(title = "NESTS MAP",

      bs4Dash::box(width = 12,maximizable = TRUE,

      sidebar = boxSidebar(
        id = "nest_controls",
        background = 'rgba(51, 58, 64, 0.7)',
        startOpen = TRUE, 
        width = 75, 

          sliderInput(
            inputId = "nest_size",
            label = "Text and symbol size:",
            min = 1, max = 7, step = 0.2, value = 3
          ),

          selectInput(
            inputId = "nest_species", label = "Species:",
            multiple = TRUE,
            choices = getOption("species"),
            selected = getOption("species")
          ),

          sliderInput(
            inputId = "days_to_hatch",
            label = "Days till hatching",
            min = 0, max = 30, step = 1, value = 30
          ),

          pickerInput(
            inputId = "nest_state", label = "Nest state:",
            multiple = TRUE,
            choices = c(
              "Found"             = "F",
              "Collected"         = "C",
              "Incubated"         = "I",
              "possibly Predated" = "pP",
              "possibly Deserted" = "pD",
              "Predated"          = "P",
              "Deserted"          = "D",
              "Hatched"           = "H",
              "Not Active"        = "notA"
            ),
            selected = c("F", "C", "I", "pP", "pD", "P", "D", "H", "notA")
          ),

          downloadBttn(outputId = "map_nests_pdf",label = "PDF map")
      ), # end sidebar
        
      
      
    shiny::tags$style(type = "text/css", "#map_nests_show {height: calc(93vh - 1px) !important;}"),
    plotOutput('map_nests_show')

    )


  ),

  tabPanel(title = "LIVE NEST MAP",

      bs4Dash::box(width = 12,maximizable = TRUE,

      shiny::tags$style(type = "text/css", "#nest_dynmap_show {height: calc(95vh - 1px) !important;}"),
      leafletOutput(outputId = "nest_dynmap_show")

    )


  ), 

  tabPanel(title = "NESTS OVERVIEW",

    DT::DTOutput(outputId = "nests_overview")


  )
  
  )

)

)