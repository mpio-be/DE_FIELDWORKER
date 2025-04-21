
#' source("./DataEntry/todo_white_list/global.R")
#' source("./DataEntry/todo_white_list/inspector.R")
#' 
#' dat = DBq('SELECT * FROM todo_white_list')
#' class(dat) = c(class(dat), 'todo_white_list')
#' ii = inspector(dat)
#' evalidators(ii)


inspector.todo_white_list <- function(dat, ...) {

x = copy(dat)
x[ , rowid := .I]

list(
# Mandatory values
  x[, .(variable, value, rowid)] |>
  is.na_validator() |> try_validator(nam = 1)
)


}
