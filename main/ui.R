bs4Dash::dashboardPage(
  help = NULL,
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
      menuItem("Start",         tabName = "start",         icon = icon("circle-play")),
      menuItem("GPS",           tabName = "gps",           icon = icon("location-arrow")),
      menuItem("Enter Data",    tabName = "enter_data",    icon = icon("edit")),
      menuItem("Database",      tabName = "database",      icon = icon("database")),
      menuItem("View Data",     tabName = "view_data",     icon = icon("table")),
      menuItem("Nests Map",     tabName = "nests_map",     icon = icon("map")),
      menuItem("Live Nest Map", tabName = "live_nest_map", icon = icon("broadcast-tower")),
      menuItem("To-Do list",    tabName = "todo_list",     icon = icon("tasks")),
      menuItem("To-Do map",     tabName = "todo_map",      icon = icon("street-view")),
      menuItem("Overview",      tabName = "overview",      icon = icon("chart-line")),
      menuItem("Hatching",      tabName = "hatching_est",  icon = icon("egg"))
    )
  ),
  
  body = dashboardBody(
    includeCSS("./www/style.css"), 

    tabItems(
      # Start tab (k4)
        tabItem(
        tabName = "start",
        
        fluidRow(
          box(title = 'Info',icon = icon("rss"), width = 10,
            includeMarkdown("./www/help/news.md")
          ), 

          box(title = 'Settings',icon = icon("gears"),width = 2,
            dateInput(inputId = 'refdate', 'Reference date',  format = "yyyy-mm-dd")
          )
        )
      
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
          box(width = 12, maximizable = TRUE,
            
            sidebar = boxSidebar(
              id = "nest_controls",
              startOpen = TRUE, 
              sliderInput(
                inputId = "nest_size",
                label = "Text and symbol size:",
                min = 1, max = 7, step = 0.2, value = 3
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
            
            spinner(
              plotOutput("map_nests_show")
            )
          
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
            
            spinner(
            leafletOutput(outputId = "nest_dynmap_show",width = "100%", height = "calc(99vh - 1px)") 
            ),
            
            sidebar = boxSidebar(
              id = "live_nest_map_controls",
              width = 25,
              startOpen = TRUE, 

              checkboxInput(
                "live_nest_map_show_past_nests", 
                "Show last season's nests", 
                value = TRUE)

            )
          )
        )
      ),
      # To-Do list tab
        tabItem(
        tabName = "todo_list",
        spinner(
        DT::DTOutput(outputId = "todo_list_show") 
        )
      ), 
      # Overview tab
        tabItem(
        tabName = "overview",
        spinner(
          plotOutput("overview_show")
        )
      ), 
      # To-Do map tab
        tabItem(
        tabName = "todo_map",
        fluidRow(
          box(width = 12, maximizable = TRUE,
            
            sidebar = boxSidebar(
              id = "todo_map_controls",
              startOpen = TRUE, 
              sliderInput(
                inputId = "todo_map_size",
                label = "Text and symbol size:",
                min = 1, max = 7, step = 0.2, value = 3
              ),

              downloadBttn(outputId = "map_todo_pdf", label = "PDF to-do map")
            ),
            
            spinner(
              plotOutput("map_todo_show")
            )
          
          )
        )
      ),
      
      # Hatching tab
        tabItem(
        tabName = "hatching_est",

        fluidRow(
          box(title = 'Select flotation ',icon = icon("gears"), width = 2,
            sliderInput('float_angle', 'Angle:', value = 50, min = 14, max = 90,step = 1),
            sliderInput('float_height', 'Height:',value = 2, min = 0, max = 6,step = 1)
          ), 

          box(width = 8,
            spinner(
              plotOutput(outputId = "hatching_est_plot") 
              )
          )
        )

      )

    )
  ),
  
  controlbar = dashboardControlbar(
    uiOutput("clock"),
    code("Hard drive:"),
    uiOutput("hdd_state")
  )
)
