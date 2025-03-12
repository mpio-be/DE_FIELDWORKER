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
  x = DBq("SELECT r.UR, r.UL, r.LR, r.LL, lat, lon, datetime_ - interval 8  hour  datetime_  from
              GPS_POINTS g
              JOIN
                  RESIGHTINGS r ON
                      g.gps_id = r.gps_id AND g.gps_point = r.gps_point_start
                  ")
  x[!is.na(LL), LR := LL]
  x[, combo := make_combo(.SD, short = "LR")][, ":="(UL = NULL, LL = NULL, UR = NULL, LR = NULL)]

  cc = DBq("SELECT distinct UL,LL,UR,LR,tagID FROM CAPTURES")
  cc[!is.na(LL), LR := LL]
  cc[, combo := make_combo(.SD, short = "LR")]
  cc[, ":="(UL = NULL, LL = NULL, UR = NULL, LR = NULL)]

  x = merge(x, cc, by = "combo", allow.cartesian = TRUE)


  x[, lastSeen := max(datetime_), by = .(combo)]
  x = x[lastSeen == datetime_]

  x[, seenDaysAgo := difftime(Sys.time(), lastSeen, units = "days") |> as.numeric() |> round(1)]


  colbyID(x)
}

#' n = NESTS()
#' n = NESTS(DB = "FIELD_2024_NOLAatDUMMERSEE", .refdate = "2024-04-26")
#' 
NESTS <- function(DB, .refdate) {
  

    # last state (all nests)
      x[ , lastDate := max(date), by = nest]
      last_state =  x[date == lastDate, .(nest, last_clutch = clutch_size, nest_state,lastDate)]
      last_state[, lastCheck := difftime(as.Date(.refdate ), lastDate, units = "days") |> as.numeric() |> round(1)]
      x = DBq(glue("SELECT *  FROM NESTS WHERE date <= {shQuote(.refdate)}"), .db = DB)

    # lat, long,datetime_found
      g = DBq('SELECT n.gps_id, n.gps_point, CONCAT_WS(" ",n.date,n.time_appr) datetime_found, n.nest, lat, lon
                      FROM NESTS n JOIN GPS_POINTS g on n.gps_id = g.gps_id AND n.gps_point = g.gps_point
                        WHERE n.gps_id is not NULL and n.nest_state = "F"', .db = DB)
      g[, datetime_ := as.POSIXct(datetime_found)]

      g = g[, .(lat = mean(lat), lon = mean(lon), datetime_found = min(datetime_found)), .(nest)]

    # male, female COMBO
      id = DBq("SELECT DISTINCT ID,nest,sex_observed sex, UL, LL, UR,LR FROM CAPTURES where nest like 'L%'", .db = DB)
      # remove only metal
      id = id[,M_only := rowSums(.SD == "M", na.rm = TRUE) == 1 & rowSums(!is.na(.SD)) == 1,  .SDcols = c("UL", "LL", "UR", "LR")]
      id = id[!(M_only)][, M_only := NULL]

      id[, combo := make_combo(.SD)]
      id = dcast(id, nest ~ sex, value.var = "combo",fun.aggregate = function(x) paste(x, collapse =','))

      #TODO how to mark acrylic paint (new column?)


    # # days till hatching
      dth = DBq("SELECT * FROM EGGS ",.db = DB)
      # h = hatching_table()
      # dth = merge(dth, h, by = c("float_angle", "surface"))
      # dth[, est_hatch_date := date + days_till_hatching]
      # dth[, days_till_hatching := difftime(est_hatch_date, Sys.time(), units = "days") |> as.numeric() |> round(1)]
      # dth = dth[, .(est_hatch_date = min(est_hatch_date), days_till_hatching = min(days_till_hatching)), by = nest]


  # # prepare final set
  # setnames(n, "nest_state", "last_state")
  
  # o = merge(n, cs,  by = "nest", all.x = TRUE)
  # o = merge(o, e,   by = "nest", all.x = TRUE)
  # o[is.na(collected), collected := 0]
  # o = merge(o, dth, by = "nest", all.x = TRUE)
  # o = merge(o, id,  by = "nest", all.x = TRUE)

  # o
}

#' ns = subsetNESTS(NESTS(), state = input$nest_state, sp = input$nest_species, d2h = input$days_to_hatch)
#' Keep the subset separated from NESTS() so that N() is loaded only once. Subset is done through input$
subsetNESTS <- function(n, state, sp, d2h) {
  
  n = n[!is.na(lat)]
  
  # subsets
  if (!missing(state) | !is.null(state)) {
    n= n[last_state %in% state]
  }


  if (!missing(d2h) | !is.null(d2h)) {
    n = n[days_till_hatching <= d2h | is.na(days_till_hatching)]
  }
  
  n

}