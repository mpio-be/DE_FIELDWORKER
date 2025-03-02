


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

  tags = shiny::tags

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
  n_empty_lines   = 10
  


  # UI elements
  comments = column_comment(
    user           = user,
    host           = host,
    db             = db,
    pwd            = pwd,
    table          = tableName
  )

  uitable = 
    emptyFrame(   
    user           = user,
    host           = host,
    db             = db,
    pwd            = pwd,
    table          = tableName,
    n              = n_empty_lines

    ) |> 
    rhandsontable(afterGetColHeader = js_hot_tippy_header(comments, "description")) |>
      hot_cols(columnSorting = FALSE, manualColumnResize = TRUE) |>
      hot_rows(fixedRowsTop = 1) 