
# ==========================================================================
# UI for fetching, visualizing and exporting GPS data
#     ~/github/mpio-be/gpxui/inst/Garmin65s
#' shiny::runApp('./gpxui', launch.browser =  TRUE)
# ==========================================================================

SERVER = "de_fieldworker" # dbo::my.cnf()

#! Packages, functions
    sapply(c( 
      "gpxui",
      "leaflet",
      "gridlayout",
      "bslib", 
      "sf",
      "dbo"
    ), require, character.only = TRUE, quietly = TRUE)


#! Options
  options(shiny.autoreload = TRUE)
  options(shiny.maxRequestSize = 20 * 1024^3)
  options(dbo.tz = "CET")

#* Variables
  cnf  = configr::read.config(getOption("dbo.my.cnf"))[[SERVER]]
  user = cnf$user
  host = cnf$host
  pwd  = cnf$password
  DB   = cnf$database

  GPS_IDS       = 1:15
  EXPORT_TABLES = c("nest_locations")
