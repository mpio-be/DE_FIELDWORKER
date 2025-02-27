#+ NOTE:
  #' list.files('./main/R', full.names = TRUE) |> lapply(source) |> invisible(); source('./main/global.R')
  #' ss = function() shiny::runApp('main', launch.browser = TRUE)


#! PACKAGES & DATA
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



#! OPTIONS


  options(shiny.autoreload = TRUE)

  options(dbo.tz = "Europe/Berlin")

  source(file.path("..", "config.R"))

#! UI DEFAULTS
  
  ver                 = "v 0.0.2"
  apptitle            = "DÜMMER-SEE"
  pagetitle           = apptitle
  set_capturedDaysAgo = 3
  set_seenDaysAgo     = 3

 
