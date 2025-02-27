


#' shiny::runApp('./DataEntry/AUTHORS', launch.browser = TRUE, port = 1234)


#! SETTINGS
  source('file.path("..", "..", "config.R")')
  DataEntry_packages()
  tags = shiny::tags

#* FUNCTIONS
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