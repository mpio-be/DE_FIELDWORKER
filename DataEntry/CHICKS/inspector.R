

#' source("./DataEntry/CHICKS/global.R")
#' source("./DataEntry/CHICKS/inspector.R")
#'
#' dat = DBq('SELECT * FROM CHICKS')
#' class(dat) = c(class(dat), 'CHICKS')
#' ii = inspector(dat)
#' evalidators(ii)




inspector.CHICKS <- function(dat, ...){  

x <- copy(dat)
x[, rowid := .I]


list(
# Mandatory values
  x[, .(author,nest,date,ID, rowid)] |>
    is.na_validator()
,

# Re-inforce formats
  x[, .(date, rowid)] |> POSIXct_validator()
,
  x[, .(caught, rowid)] |> hhmm_validator()
,
  x[, .(released, rowid)] |> hhmm_validator()
,

# Reinforce values (from existing db tables or lists)
  x[, .(author, rowid)] |>
    is.element_validator(v = data.table(
      variable = "author",
      set = list(DBq("SELECT author ii FROM AUTHORS")$ii),
      reason = "entry not in the AUTHORS table"
    )) |> try_validator(nam = "author_exists")
,
  x[, .(nest, rowid)] |>
    is.element_validator(v = data.table(
      variable = "nest",
      set = list(DBq("SELECT nest ii FROM NESTS")$ii),
      reason = "entry not in the NESTS table"
    )) |> try_validator(nam = "nest_exists")
,


# Values should be UNIQUE within their containing table
  x[, .(ID, rowid)] |>
    is.duplicate_validator(
      v = data.table(
        variable = "ID",
        set = list(DBq("SELECT distinct ID FROM CHICKS")$nest)
      ),
      reason = "ID already exists in the CHICKS table."
    )|> try_validator(nam = "val5")
  ,

# morphometrics
  x[, .(tarsus, weight, rowid)] |>
  interval_validator(
    v = fread("    
        variable   lq    uq
          tarsus   22    28
          weight   14    20"),
    reason = "Measurement out of the typical range."
  )|> try_validator(nam = "morph")



)}