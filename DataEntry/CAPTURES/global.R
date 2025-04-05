
#' shiny::runApp('./DataEntry/CAPTURES', launch.browser = TRUE)

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


#! PARAMETERS
  tableName       = "CAPTURES"
  excludeColumns  = c("pk", "nov")
  n_empty_lines = 10
  
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
    x <- DBq("SELECT ID, CAST( CONCAT_WS(' ', `date`,released) AS DATETIME) datetime_ FROM CAPTURES WHERE ID is not NULL
              ORDER BY datetime_ DESC")

    data.table(
      N_entries    = nrow(x),
      N_unique_IDs = length(unique(x$ID)),
      last_captures  = paste(x[1, ], collapse = ", ")
    )
  }

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
      user = user,
      host = host,
      db = db,
      pwd = pwd,
      table = tableName,
      excludeColumns = excludeColumns,
      n = n_empty_lines,
      preFilled = list(
        date = format(Sys.Date(), "%Y-%m-%d"),
        species = "NOLA"
      )
    ) |>
    rhandsontable(afterGetColHeader = js_hot_tippy_header(comments, "description")) |>
    hot_cols(columnSorting = FALSE, manualColumnResize = TRUE) |>
    
    hot_col(col = "capture_meth", type = "autocomplete", source = c("T", "M", "O")) |>
    hot_col(col = "tag_action",   type = "autocomplete", source = c("on", "off"))   |>
    hot_col(col = "tagType",      type = "autocomplete", source = c("D", "G"))      |>
    hot_col(col = "sex_observed", type = "autocomplete", source = c("F", "M", "U")) |>
    
    hot_rows(fixedRowsTop = 1) 
