#+ NOTE:
  #' list.files('./R', full.names = TRUE) |> lapply(source) |> invisible(); source('global.R')
  #' ss = function() shiny::runApp(launch.browser = TRUE)

SERVER = "de_fieldworker" # dbo::my.cnf()


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

#- VARIABLES

  cnf       = configr::read.config(getOption("dbo.my.cnf"))[[SERVER]]
  user      = cnf$user
  host      = cnf$host
  pwd       = cnf$password
  db        = cnf$database
  dbbasenam = 'NOLAatDUMMERSEE'
  years     = c(2024, 2025)

  app_nam              = "DE_FIELDWORKER"
  dbtabs_entry         = c("AUTHORS", "CAPTURES", "RESIGHTINGS", "CHICKS", "NESTS", "EGGS", "SAMPLES")
  dbtabs_view          = c("AUTHORS", "CAPTURES", "CAPTURES_ARCHIVE", "RESIGHTINGS", "CHICKS", "NESTS", "EGGS", "SAMPLES", "COMBOS")
  species              = c("NOLA", "REDS")
  studySiteCenter      = c(x = 8.341151, y = 52.55065)


  hatch_pred_gam = "./data/gam_float_to_hach.rds"



#! OPTIONS
  options(shiny.autoreload = TRUE)
  options(dbo.tz = "Europe/Berlin")


#! UI DEFAULTS
  
  ver                 = "v 0.0.2"
  apptitle            = "DÜMMER-SEE"
  pagetitle           = apptitle
  set_capturedDaysAgo = 3
  set_seenDaysAgo     = 3
