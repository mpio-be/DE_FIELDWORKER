


#' shiny::runApp('./DataEntry/CAPTURES', launch.browser = TRUE)

#! SETTINGS
  source(file.path("..", "..", "config.R"))
  DataEntry_packages()
  tags = shiny::tags

#* FUNCTIONS
  
  describeTable <- function() {
    x <- DBq("SELECT ID, CAST( CONCAT_WS(' ', `date`,released) AS DATETIME) datetime_ FROM CAPTURES WHERE ID is not NULL
              ORDER BY datetime_ DESC")

    data.table(
      N_entries    = nrow(x),
      N_unique_IDs = length(unique(x$ID)),
      last_captures  = paste(x[1, ], collapse = ", ")
    )
  }



#! PARAMETERS
  tableName       = "CAPTURES"
  excludeColumns  = c("pk", "nov")
  n_empty_lines   = 10

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
      date = format(Sys.Date(), "%Y-%m-%d"),
      species = "NOLA" #,
      # UL   = "M", 
      # UR   = "W"
    )
    ) |> 
    rhandsontable(afterGetColHeader = js_hot_tippy_header(comments, "description")) |>
      hot_cols(columnSorting = FALSE, manualColumnResize = TRUE) |>
      hot_rows(fixedRowsTop = 1) 