
#' source("./DataEntry/CAPTURES/global.R")
#' source("./DataEntry/CAPTURES/inspector.R")
#' 
#' dat = DBq('SELECT * FROM CAPTURES')
#' class(dat) = c(class(dat), 'CAPTURES')
#' ii = inspector(dat)
#' evalidators(ii)


inspector.CAPTURES <- function(dat, ...) {

x = copy(dat)
x[ , rowid := .I]

list(
# Mandatory values
  x[, .(date, form_id,  gps_id, gps_point, ID, recapture, weight, rowid)] |>
  is.na_validator() |> try_validator(nam = 1)
  ,

  x[recapture == 0, .(tarsus, culmen, total_head, wing, crest, rowid)] |>
    is.na_validator("Mandatory at first capture.") |>
    try_validator(nam = 2)
  ,

  x[, .(gps_id, gps_point, rowid)] |>
    is.na_validator() |>
    try_validator(nam = 'gps')
  ,

# Re-enforce formats
  x[, .(date, rowid)] |>
    POSIXct_validator() |>
    try_validator(nam = 3)

,
# Reinforce values (from existing db tables or lists)
{
  z = x[, .(tag_action, recapture)]

  v <- data.table(
    variable = names(z),
    set = c(
      list(c("on", "off")),
      list(c(1, 0))
    )
  )

    is.element_validator(z, v)
  } |> try_validator(nam = 4)
  ,

  x[, .(author, rowid)] |>
  is.element_validator(v = data.table(
      variable = "author",
      set = list(DBq("SELECT author ii FROM AUTHORS")$ii) ), 
      reason = 'entry not in the AUTHORS table' ) |> try_validator(nam = 5)
  ,


  x[, .(gps_id, rowid)] |>
    is.element_validator(
      v = data.table(
      variable = "gps_id",
      set = list(1:15)
      ), 
     reason = "GPS ID not in use") |> try_validator(nam = 6)
  ,

# morphometrics
  # select * FROM AVES_morphometry.body_mass where scinam = "vanellus vanellus"
  # TODO
  x[, .(culmen, total_head, tarsus, wing, weight, rowid)] |>
  interval_validator(
    v = fread("    
        variable   lq         uq
          culmen   10         100  
      total_head   10         100
          tarsus   10         100
            wing   100        500
          weight   140  317"),
    reason = "Measurement out of the typical range."
  )|> try_validator(nam = 9)
  
,
# Values should be UNIQUE within their containing table
  x[recapture == 0 & !is.na(ID), .(ID, rowid)] |>
  is.duplicate_validator(
    v = data.table(
      variable = "ID",
      set = list(DBq("SELECT distinct ID FROM CAPTURES")$ID)
      ),
    reason = "Metal band already in use! Is this a recapture?"
  ) |> try_validator(nam = 12)
  
  


)


}
