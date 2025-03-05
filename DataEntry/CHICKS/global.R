
#' shiny::runApp('./DataEntry/CHICKS', launch.browser =  TRUE)

SERVER = "de_fieldworker" # dbo::my.cnf()


#! SETTINGS
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
    "dbo", 
    "configr"
  ), require, character.only = TRUE, quietly = TRUE)
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
    x <- DBq("SELECT nest, date FROM CHICKS WHERE nest is not NULL
              ORDER BY date DESC")

    data.table(
      N_entries    = nrow(x),
      N_nests      = length(unique(x$nest)),
      last_entry   = paste(x[1, ], collapse = ", ")
    )
  }


#! PARAMETERS
  tableName       = "CHICKS"
  excludeColumns  = c("pk", "nov")
  n_empty_lines   = 20
  cnf = read.config(getOption("dbo.my.cnf"))[[SERVER]]
  user = cnf$user
  host = cnf$host
  pwd  = cnf$password
  db   = cnf$database


# UI elements
  comments = column_comment(
    user           = user,
    host           = host,
    db             = db,
    pwd            = pwd,
    table          = tableName,
    excludeColumns = excludeColumns
  )

  uitable = 
    emptyFrame(   
    user           = user,
    host           = host,
    db             = db,
    pwd            = pwd,
    table          = tableName,
    excludeColumns = excludeColumns,
    n              = n_empty_lines, 
    preFilled = list(
      date = format(Sys.Date(), "%Y-%m-%d")
      # UL   = "M", 
      # UR   = "W"
    )
    ) |> 
    rhandsontable(afterGetColHeader = js_hot_tippy_header(comments, "description")) |>
      hot_cols(columnSorting = FALSE, manualColumnResize = TRUE) |>

      hot_col(
        col    = "location",
        type   = "autocomplete",
        source = c("N", "F", "H"),
        strict = TRUE
      ) |>

      hot_rows(fixedRowsTop = 1) 
