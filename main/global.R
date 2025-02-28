#+ NOTE:
  #' list.files('./main/R', full.names = TRUE) |> lapply(source) |> invisible(); source('./main/global.R')
  #' ss = function() shiny::runApp('main', launch.browser = TRUE)

SERVER = "localhost" # dbo::my.cnf()


#- VARIABLES

  cnf  = configr::read.config(getOption("dbo.my.cnf"))[[SERVER]]
  user = cnf$user
  host = cnf$host
  pwd  = cnf$password
  db   = cnf$database

  app_nam              = "FIELDWORKER"
  dbtabs_entry         = c("CAPTURES", "RESIGHTINGS", "CHICKS", "NESTS", "EGGS", "AUTHORS")
  dbtabs_view          = c("CAPTURES", "RESIGHTINGS", "CHICKS", "NESTS", "EGGS", "AUTHORS")
  species              = c("NOLA", "REDS")
  studySiteCenter      = c(x = 8.341151, y = 52.55065)


#! OPTIONS
  options(shiny.autoreload = TRUE)
  options(dbo.tz = "Europe/Berlin")



#! PACKAGES & SETTINGS
  sapply(
    c(
    "dbo",
    "sf",
    "data.table",
    "stringr",
    "forcats",
    "zip",
    "glue",
    "ggplot2",
    "ggrepel",
    "patchwork",
    
    "shinyWidgets",
    "bs4Dash",
    "DT",
    
    "leaflet",
    "leafem",
    "leaflet.extras"
  ), require, character.only = TRUE, quietly = TRUE)

  data(OsterFeinerMoor, package = "wadeR")

  tags = shiny::tags



#! OPTIONS


  options(shiny.autoreload = TRUE)

  options(dbo.tz = "Europe/Berlin")


#! UI DEFAULTS
  
  ver                 = "v 0.0.2"
  apptitle            = "DÜMMER-SEE"
  pagetitle           = apptitle
  set_capturedDaysAgo = 3
  set_seenDaysAgo     = 3

 
