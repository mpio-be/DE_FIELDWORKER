


#' shiny::runApp('./DataEntry/AUTHORS', launch.browser = TRUE, port = 1234)

SERVER = "de_fieldworker" # dbo::my.cnf()

# SETTINGS
  sapply(c(
    "DataEntry",            # remotes::install_github('mpio-be/DataEntry')
    "DataEntry.validation", # remotes::install_github('mpio-be/DataEntry.validation')
    "shinyjs", 
    "shinyWidgets",
    "shinytoastr",
    "tableHTML", 
    "glue", 
    "stringr", 
    "beR",
    "dbo"
  ), require, character.only = TRUE, quietly = TRUE)


#- VARIABLES
  cnf  = configr::read.config(getOption("dbo.my.cnf"))[[SERVER]]
  user = cnf$user
  host = cnf$host
  pwd  = cnf$password
  db   = cnf$database

  

#* FUNCTIONS
    DBq <- function(x) {
    con <- dbo::dbcon(server = SERVER, db = db)
    on.exit(DBI::dbDisconnect(con))

    o <- DBI::dbGetQuery(con, x)
    setDT(o)
    o
  }
  
  describeTable <- function() {
    x <- DBq("SELECT * FROM AUTHORS")

    data.table(
      N_entries    = nrow(x)
    )
  }



#! PARAMETERS
  tableName       = "AUTHORS"


  backupdir = paste0('~/backup_', db)

  # UI elements
  comments = column_comment(
    user           = user,
    host           = host,
    db             = db,
    pwd            = pwd,
    table          = tableName
  )
