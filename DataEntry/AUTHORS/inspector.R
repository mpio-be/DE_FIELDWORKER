
#' source("./DataEntry/AUTHORS/global.R")
#' source("./DataEntry/AUTHORS/inspector.R")
#' 
#' dat = DBq('SELECT * FROM AUTHORS')
#' class(dat) = c(class(dat), 'AUTHORS')
#' ii = inspector(dat)
#' evalidators(ii)


inspector.AUTHORS <- function(dat, ...) {

x = copy(dat)
x[ , rowid := .I]

list(
# Mandatory values
  x[, .(Name, author, START, STOP, rowid)] |>
  is.na_validator() |> try_validator(nam = 1)
)


}
