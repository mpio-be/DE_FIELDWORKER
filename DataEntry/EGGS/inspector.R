

#' source("./DataEntry/EGGS/global.R")
#' source("./DataEntry/EGGS/inspector.R")
#'
#' dat = DBq('SELECT * FROM EGGS')
#' class(dat) = c(class(dat), 'EGGS')
#' ii = inspector(dat)
#' evalidators(ii)




inspector.EGGS <- function(dat, ...){  

x <- copy(dat)
x[, rowid := .I]


list(
# Mandatory values
  x[, .(species,nest,date,float_angle,surface, rowid)] |>
    is.na_validator()
  ,

# Re-inforce formats
  x[, .(date, rowid)] |> POSIXct_validator()
  ,


# Reinforce values (from existing db tables or lists)
  {
  z = x[, .(species,rowid)]

  v <- data.table(
    variable = names(z),
    set = c(
      list( c("NOLA")  )
    )
  )

  is.element_validator(z, v)

  }
  ,


 
  # nest should exist in NESTS 
  {
    z = x[, .(nest,rowid)]  
    is.element_validator(z,
      v = data.table(
        variable = "nest",
        set = list(DBq("SELECT distinct nest FROM NESTS")$nest )
      ),
      reason = "nest does not exist in NESTS."
    )
  }
  ,

# Values should be within given intervals
  x[, .(float_angle, rowid)] |>
    interval_validator(
      v = data.table(variable = "float_angle", lq = 0, uq = 90),
      "Float angle should be >= 0 and =< 90"
    ) |> try_validator()
  ,
  x[!is.na(surface), .(surface)]  |> 
  interval_validator(  
    v = data.table(variable = "surface", lq = 0, uq = 7 ),  
    reason = "Unusual floating size." 
  )|> try_validator(nam = "val4")


)}