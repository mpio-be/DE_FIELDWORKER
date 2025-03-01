dashboardPage(
  preloader = list(
    html = waiter::spin_loaders(id = 16, color = "#01125f"), 
    color = "#b8c7c5"
  ),
  dark = FALSE,
  title = paste(app_nam, year(Sys.Date())),
  
  header = dashboardHeader(
    title = dashboardBrand(
      title = paste(pagetitle, year(Sys.Date())),
      image = "ICO.png"
    )
  ),
  
  sidebar = dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      id = "main",  # Assigning an id here allows input$main to be set
      menuItem("News", tabName = "news", icon = icon("newspaper")),
      menuItem("GPS",           tabName = "gps",           icon = icon("location-arrow")),
      menuItem("Enter Data",    tabName = "enter_data",    icon = icon("edit")),
      menuItem("Database",      tabName = "database",      icon = icon("database")),
      menuItem("View Data",     tabName = "view_data",     icon = icon("table")),
      menuItem("Nests Map",     tabName = "nests_map",     icon = icon("map")),
      menuItem("Live Nest Map", tabName = "live_nest_map", icon = icon("broadcast-tower")),
      menuItem("To-Do",         tabName = "todo",          icon = icon("tasks"))
    )
  ),
  
  body = dashboardBody(
    tabItems(
      # News tab
      tabItem(
        tabName = "news",
        includeMarkdown("./www/help/news.md")
      ),
      # GPS tab
      tabItem(
        tabName = "gps",
          includeMarkdown("./www/help/gps.md"),
          uiOutput("open_gps")
      ),
      # Enter Data tab
      tabItem(
        tabName = "enter_data",
          uiOutput("new_data"),
          hr(),
          includeMarkdown("./www/help/enter_data.md")
    ),
      # DB tab
      tabItem(
        tabName = "database",
        uiOutput("open_db"),
        includeMarkdown("./www/help/database.md")
      ),
      # View Data tab
      tabItem(
        tabName = "view_data",
          bs4Dash::tabsetPanel(
            id = "tabset",
            .list = lapply(dbtabs_view, function(i) {
              tabPanel(
                title = paste0("[", i, "]"),
                active = FALSE,
                DT::DTOutput(outputId = paste0(i, "_show"))
              )
            })
          )
      ),
      # Nests Map tab
      tabItem(
        tabName = "nests_map",
        fluidRow(
          box(width = 12,maximizable = TRUE,
            
            sidebar = boxSidebar(
              id = "nest_controls",
              startOpen = TRUE, 
              sliderInput(
                inputId = "nest_size",
                label = "Text and symbol size:",
                min = 1, max = 7, step = 0.2, value = 3
              ),
              selectInput(
                inputId = "nest_species", label = "Species:",
                multiple = TRUE,
                choices = species,
                selected = species
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
              downloadBttn(outputId = "map_nests_pdf", label = "PDF map")
            ),
            shiny::tags$style(type = "text/css", "#map_nests_show {height: calc(93vh - 1px) !important;}"),
            plotOutput('map_nests_show')
          )
        )
      ),
      # Live Nest Map tab
      tabItem(
        tabName = "live_nest_map",
        fluidRow(
          box(
            width = 12,
            maximizable = TRUE,
            shiny::tags$style(type = "text/css", "#nest_dynmap_show {height: calc(95vh - 1px) !important;}"),
            leafletOutput(outputId = "nest_dynmap_show")
          )
        )
      ),
      # To-Do tab
      tabItem(
        tabName = "todo",
        DT::DTOutput(outputId = "nests_overview")
      )
    )
  ),
  
  controlbar = dashboardControlbar(
    uiOutput("clock"),
    code("Hard drive:"),
    uiOutput("hdd_state")
  )
)
