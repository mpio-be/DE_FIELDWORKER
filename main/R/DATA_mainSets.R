# NOTE: subsets are done when mapping


#' x = CAPTURES()
CAPTURES <- function() {

  x <- DBq("SELECT UL,LL,UR,LR, tagID, lat, lon, datetime_ FROM CAPTURES c JOIN  GPS_POINTS g
            ON g.gps_id = c.gps_id AND g.gps_point = c.gps_point
              WHERE ID not in (SELECT ID FROM CAPTURES where dead = 1)")
  
  x[!is.na(LL), LR := LL]
  x[, tagID := str_remove(tagID, "^0")]

  x[, combo := make_combo(.SD, short = "LR")]
  x[, combo := glue_data(.SD, "{combo}")]
  #x[, combo := glue_data(.SD, "{combo}[{ ifelse(is.na(tagID), '', tagID)}]")]
  #x[, combo := str_remove(combo, "\\[\\]")]

  x[, lastCaptured := max(datetime_), by = .(combo)]

  x <- x[lastCaptured == datetime_]

  x[, capturedDaysAgo := difftime(Sys.time(), lastCaptured, units = "days") |> as.numeric() |> round(1)]


  x

}

#' x = RESIGHTINGS()
RESIGHTINGS <- function() {
  x = DBq('SELECT r.UR, r.UL, r.LR, r.LL, lat, lon, datetime_ - interval 8  hour  datetime_  from
              GPS_POINTS g
              JOIN
                  RESIGHTINGS r ON
                      g.gps_id = r.gps_id AND g.gps_point = r.gps_point_start
                  ')
  x[!is.na(LL), LR := LL]
  x[, combo := make_combo(.SD, short = "LR")][, ":="(UL = NULL, LL = NULL, UR = NULL, LR = NULL)]

  cc = DBq("SELECT distinct UL,LL,UR,LR,tagID FROM CAPTURES")
  cc[!is.na(LL), LR := LL]
  cc[, combo :=  make_combo(.SD, short = "LR")]
  cc[, ":="(UL = NULL, LL = NULL, UR = NULL, LR = NULL)]
  
  x = merge(x, cc, by = "combo", allow.cartesian = TRUE)


  x[,lastSeen := max(datetime_), by = .(combo) ]
  x = x[lastSeen == datetime_]

  x[, seenDaysAgo := difftime(Sys.time(), lastSeen, units = "days") |> as.numeric() |> round(1)]


  colbyID(x)

}

#' n = NESTS()
NESTS <- function() {
  # last state
  n = DBq('SELECT nest, max(CONCAT_WS(" ",date,time_appr)) datetime_, nest_state
                        FROM NESTS
                          GROUP BY nest, nest_state')
  n[, species := fcase(
    str_starts(nest, "L"), "NOLA",
    str_starts(nest, "R"), "REDS",
    default = NA_character_
    )]

  setorder(n, nest)
  n[, lastd := max(datetime_), by = .(nest)]
  n = n[datetime_ == lastd][, lastd := NULL]
  n[, lastCheck := difftime(Sys.time(), datetime_, units = "days") |> as.numeric() |> round(1)]

  # lat, lon for F state, species
  g = DBq('SELECT n.gps_id, n.gps_point, CONCAT_WS(" ",n.date,n.time_appr) datetime_found, n.nest, lat, lon
                  FROM NESTS n JOIN GPS_POINTS g on n.gps_id = g.gps_id AND n.gps_point = g.gps_point
                    WHERE n.gps_id is not NULL and n.nest_state = "F"')
  g[, datetime_ := as.POSIXct(datetime_found)]

  g = g[, .(lat = mean(lat), lon = mean(lon), datetime_found = min(datetime_found)), .(nest)]

  n = merge(n, g, by = c("nest"), all.x = TRUE)
  n[, datetime_found := as.POSIXct(datetime_found)]
  n[, firstCheck := difftime(Sys.time(), datetime_found, units = "days") |> as.numeric() |> round(1)]

  # clutch size
  cs = DBq("SELECT  nest, min(clutch_size) iniClutch, max(clutch_size) clutch FROM NESTS GROUP BY nest")

  # collected
  e = DBq("select distinct nest from NESTS where nest_state = 'C' ")
  e[, collected := 1]

  # days till hatching
  dth = DBq("SELECT * FROM EGGS ")
  h = hatching_table()
  dth = merge(dth, h, by = c("float_angle", "surface"))
  dth[, est_hatch_date := date + days_till_hatching]
  dth[, days_till_hatching := difftime(est_hatch_date, Sys.time(), units = "days") |> as.numeric() |> round(1)]
  dth = dth[, .(est_hatch_date = min(est_hatch_date), days_till_hatching = min(days_till_hatching)), by = nest]

  # male, female confirmed identity
  id = DBq("SELECT distinct n.nest,c.ID , c.sex_observed sex
                from NESTS n
                 left join CAPTURES c on c.nest = n.nest
                   where c.ID is not NULL")
  id = dcast(id, nest ~ sex, value.var = "ID")

  # prepare final set
  setnames(n, "nest_state", "last_state")
  
  o = merge(n, cs,  by = "nest", all.x = TRUE)
  o = merge(o, e,   by = "nest", all.x = TRUE)
  o[is.na(collected), collected := 0]
  o = merge(o, dth, by = "nest", all.x = TRUE)
  o = merge(o, id,  by = "nest", all.x = TRUE)

  o
}

#' ns = subsetNESTS(NESTS(), state = input$nest_state, sp = input$nest_species, d2h = input$days_to_hatch)
#' Keep the subset separated from NESTS() so that N() is loaded only once. Subset is done through input$
subsetNESTS <- function(n, state, sp, d2h) {
  
  n = n[!is.na(lat)]
  
  # subsets
  if (!missing(state) | !is.null(state)) {
    n= n[last_state %in% state]
  }

  if (!missing(sp) | !is.null(sp)) {
    n= n[species %in% sp]
  }

  if (!missing(d2h) | !is.null(d2h)) {
    n = n[days_till_hatching <= d2h | is.na(days_till_hatching)]
  }
  
  n

}