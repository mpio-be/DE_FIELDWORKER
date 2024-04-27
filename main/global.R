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
  options(
    app_nam              = "FIELDWORKER",
    server               = "nola24",
    db                   = "FIELD_2024_NOLAatDUMMERSEE",
    dbtabs_entry         = c("CAPTURES", "RESIGHTINGS", "CHICKS", "NESTS", "EGGS", "AUTHORS"),
    dbtabs_view          = c("CAPTURES", "RESIGHTINGS", "CHICKS", "NESTS", "EGGS", "AUTHORS"),
    species              = c("NOLA", "REDS"),
    ggrepel.max.overlaps = 20,
    studySiteCenter      = c(x = 8.341151, y = 52.55065)
  )

  options(shiny.autoreload = TRUE)

  options(dbo.tz = "Europe/Berlin")



#! UI DEFAULTS
  
  ver                 = "v 0.0.1"
  apptitle            = "DÜMMER-SEE"
  pagetitle           = apptitle
  set_capturedDaysAgo = 3
  set_seenDaysAgo     = 3

 
