

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
  x[, .(nest,date,location, rowid)] |>
    is.na_validator()
  ,

  x[action == 'in_incubator', .(float_angle,surface,weight, rowid)] |>
    is.na_validator("did you float and measure the eggs?")
  ,

# Re-inforce formats
  x[, .(date, rowid)] |> POSIXct_validator()
  ,


# Reinforce values (from existing db tables or lists)
  {
  z = x[, .(location, state, action)]

  v <- data.table(
    variable = names(z),
    set = c(
      list( c('F', 'L')  ), # location
      list( c('normal','star','crack','hatched','fertile','broken','dead_embryo','killed')  ), # state
      list( c('in_incubator', 'in_hatcher', 'out_hatcher', 'tissue', 'blood', 'delivered')  ) # action
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
    ) |> try_validator('float angle')
  ,
  x[!is.na(surface), .(surface)]  |> 
  interval_validator(  
    v = data.table(variable = "surface", lq = 0, uq = 7 ),  
    reason = "Unusual floating size." 
  )|> try_validator(nam = "float height")


)}