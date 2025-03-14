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

NESTS_last_seasons <- function() {
  o = DBq("SELECT nest, lat latit,lon longit FROM NESTS n JOIN GPS_POINTS g
          ON n.gps_id = g.gps_id AND n.gps_point = g.gps_point AND nest_state = 'F'
          WHERE species = 'NOLA'",
    .db = "FIELD_2024_NOLAatDUMMERSEE"
  )
  o[, nest := str_remove(nest, "^L")]
  o
}



#' n = NESTS()
#' n = NESTS(DB = "FIELD_2024_NOLAatDUMMERSEE", .refdate = "2024-04-26")
#' 
NESTS <- function(DB = db, .refdate = input$refdate) {
  
    x = DBq(glue("SELECT *  FROM NESTS WHERE date <= {shQuote(.refdate)}"), .db = DB)
    x[, lastDate := max(date), by = nest]
    x[, collected := any(nest_state == 'C'), nest]

  # last state (all nests); collected
    lst =  x[date == lastDate, .(nest, last_clutch = clutch_size, nest_state, collected,lastDate)]
    lst[, lastCheck := difftime(as.Date(.refdate), lastDate, units = "days") |> as.numeric() |> round(1)]


  # lat, long,datetime_found
    gps = DBq('SELECT n.gps_id, n.gps_point, CONCAT_WS(" ",n.date,n.time_appr) datetime_found, n.nest, lat, lon
                    FROM NESTS n JOIN GPS_POINTS g on n.gps_id = g.gps_id AND n.gps_point = g.gps_point
                      WHERE n.gps_id is not NULL and n.nest_state = "F"', .db = DB)
    gps[, datetime_ := as.POSIXct(datetime_found)]
    gps = gps[, .(lat = mean(lat), lon = mean(lon), datetime_found = min(datetime_found)), .(nest)]

  # male, female COMBO
    # from nests
    f = x[apply(x[, .SD, .SDcols = patterns("^f_")], 1,  function(x) any(str_detect(x, ","), na.rm = TRUE))]
    f = f[,F := make_combo(.SD, UL = "f_UL", LL = "f_LL", UR = "f_UR", LR = "f_LR")][, .(nest, F)]

    m = x[apply(x[, .SD, .SDcols = patterns("^m_")], 1,  function(x) any(str_detect(x, ","), na.rm = TRUE))]
    m = m[,M := make_combo(.SD, UL = "m_UL", LL = "m_LL", UR = "m_UR", LR = "m_LR")][, .(nest, M)]

    mfc1 = merge(m, f, by = "nest", all = TRUE)
    
    # from captures
    mfc2 = DBq("SELECT DISTINCT ID,nest,sex_observed sex, UL, LL, UR,LR FROM CAPTURES where nest like 'L%'", .db = DB)
    # remove only metal
    mfc2 = mfc2[,M_only := rowSums(.SD == "M", na.rm = TRUE) == 1 & rowSums(!is.na(.SD)) == 1,  .SDcols = c("UL", "LL", "UR", "LR")]
    mfc2 = mfc2[!(M_only)][, M_only := NULL]

    mfc2[, combo := make_combo(.SD)]
    mfc2 = dcast(mfc2, nest ~ sex, value.var = "combo",fun.aggregate = function(x) paste(x, collapse =','))

    mfc = merge(mfc1, mfc2, by = 'nest', all.x = TRUE)

    mfc[, M := paste(c(M.x, M.y) |> unique() |> na.omit(), collapse = " ; "), by = .I]
    mfc[, F := paste(c(F.x, F.y) |> unique() |> na.omit(), collapse = " ; "), by = .I]
    mfc = mfc[,.(nest, M,F)]

  # days to hatching
    x = DBq("SELECT nest, date, float_angle, surface FROM EGGS", .db = DB)
    d2h = hatching_prediction(x, .gampath = "./data/gam_float_to_hach.rds")
    d2h = d2h[, .(
      min_days_to_hatch = min(conf.low, na.rm = TRUE),
      date = max(date, na.rm = TRUE) 
    ), nest]
    d2h = d2h[, min_pred_hatch_date := date + min_days_to_hatch]
    d2h = d2h[, date := NULL]

  # eggs
    x = DBq("SELECT * FROM EGGS", .db = DB)



  # prepare final set
    
    o = merge(lst, gps,  by = "nest", all.x = TRUE)
    o = merge(o, mfc2,   by = "nest", all.x = TRUE)
    o = merge(o, d2h,    by = "nest", all.x = TRUE)


    o
}

#' ns = subsetNESTS(NESTS(), state = input$nest_state, d2h = input$days_to_hatch)
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


#' x = NESTS(DB = "FIELD_2024_NOLAatDUMMERSEE", .refdate = "2024-04-26")
extract_TODO <- function(x) {

  x[, TODO := ""]

  #TODO codes: 
    # NC = regular nest check
    # HC = hatch check
    # CATCH
    # CHICK PROCESSING

  # CATCH 
    # if min_days_to_hatch <= 14  then try catch unless check attempt yesterday

    x[min_days_to_hatch >= 14 & is.na(F)  & is.na(M),  TODO := 'catch_any']
    x[min_days_to_hatch >= 14 & !is.na(F) & is.na(M),  TODO := 'catch_male']
    x[min_days_to_hatch >= 14 & is.na(F)  & !is.na(M), TODO := "catch_female"]
    


    # if bird ID (M or F) known from NESTS or CAPTURES: no catch

  # NESTS CHECK: nest survival
    # if last next check >=7 then check for nest_state 
    # if nest_state = pD, pP then check next day

  # NESTS CHECK: hatch check  
    # if min_est_hatch_date <= 4 OR not all chicks hatched then check next day
    
  # CHICK PROCESSING   
    # if hatch_state contains CC, C then go process chicks.

  # COLLECT EGGS
    # if nest in A3, A4: collect as soon as clutch complete. 


  

}