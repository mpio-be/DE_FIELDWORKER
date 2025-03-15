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


  # lat, long, datetime_found
  gps = DBq(glue('SELECT n.gps_id, n.gps_point, CONCAT_WS(" ",n.date,n.time_appr) datetime_found, n.nest, lat, lon
                    FROM NESTS n JOIN GPS_POINTS g on n.gps_id = g.gps_id AND n.gps_point = g.gps_point
                      WHERE n.gps_id is not NULL and
                      n.nest_state = "F" AND
                      n.date <= {shQuote(.refdate)}'), .db = DB)
    gps[, datetime_ := as.POSIXct(datetime_found)]
    gps = gps[, .(lat = mean(lat), lon = mean(lon), datetime_found = min(datetime_found)), .(nest)]

  # male, female COMBO
    # from nests (keep only proper combos, M only is exclude!)
    f = x[apply(x[, .SD, .SDcols = patterns("^f_")], 1,  function(x) any(str_detect(x, ","), na.rm = TRUE))]
    f = f[,F_nest := make_combo(.SD, UL = "f_UL", LL = "f_LL", UR = "f_UR", LR = "f_LR")][, .(nest, F_nest)]

    m = x[apply(x[, .SD, .SDcols = patterns("^m_")], 1,  function(x) any(str_detect(x, ","), na.rm = TRUE))]
    m = m[,M_nest := make_combo(.SD, UL = "m_UL", LL = "m_LL", UR = "m_UR", LR = "m_LR")][, .(nest, M_nest)]

    mfc1 = merge(m, f, by = "nest", all = TRUE)
    
    # from captures (metal only is included as combo!)
    mfc2 = DBq(glue("SELECT DISTINCT ID,nest,sex_observed sex, UL, LL, UR,LR FROM CAPTURES
                  where nest like 'L%' AND
                  date <= {shQuote(.refdate)}"), .db = DB)
    mfc2[, combo := make_combo(.SD)]
    mfc2 = mfc2[combo != '~/~|~/~']
    mfc2 = dcast(mfc2, nest ~ sex, value.var = "combo", fun.aggregate = function(x) paste(x, collapse = ","))
    mfc2[nchar(F) == 0, F := NA]
    mfc2[nchar(M) == 0, M := NA]
    setnames(mfc2, c("F", "M"), c("F_cap", "M_cap"))


  # days to hatching
    x = DBq(glue("SELECT nest, date, float_angle, surface FROM EGGS
              WHERE date <= {shQuote(.refdate)}"), .db = DB)
    d2h = hatching_prediction(x, .gampath = "./data/gam_float_to_hach.rds")
    d2h = d2h[, .(
      min_days_to_hatch_at_found = min(conf.low, na.rm = TRUE),
      date = max(date, na.rm = TRUE) 
    ), nest]
    d2h = d2h[, min_pred_hatch_date := date + min_days_to_hatch_at_found]
    d2h = d2h[, date := NULL]
    d2h[, min_days_to_hatch := as.numeric(min_pred_hatch_date - as.Date(.refdate)) |> round(1) ]


  # prepare final set
    
    o = merge(lst, gps,  by = "nest", all.x = TRUE)
    o = merge(o, mfc1,   by = "nest", all.x = TRUE)
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


#' x = NESTS(DB = "FIELD_2024_NOLAatDUMMERSEE", .refdate = "2024-04-25")
extract_TODO <- function(x) {

  o = x[ !nest_state %in% c("P", "D", "notA")]
  

  # CATCH
    #! RULES
      #! min_days_to_hatch <= 14
      #! nest_state != 'H'
      #! catch or caching attempt with one day break (use NESTS.trap_on ) #TODO
      #! hatch_state does not contain S,C,CC #TODO

    # male
    cm = o[min_days_to_hatch <= 14 & nest_state != 'H', .(nest, M_cap, M_nest)]
    cm[is.na(M_cap) , todo := "catch M"]
    cm[!is.na(M_nest), todo := NA]
    cm = unique(cm)

    # female
    cf = o[min_days_to_hatch <= 14 & nest_state != 'H', .(nest, F_cap, F_nest)]
    cf[is.na(F_cap) , todo := "catch F"]
    cf[!is.na(F_nest), todo := NA]
    cf = unique(cf)

    catch = merge(cm, cf, by = 'nest' )
    catch[, todo := paste(c(todo.x, todo.y) |> na.omit(), collapse = ", "), by = .I]
    catch = catch[nchar(todo) > 0, .(nest, todo)]

    catch[, .N, todo]
    catch[todo == 'catch M']

  # NESTS CHECK
    #! RULES
      #! if last check >=7
      #! if nest_state = pD, pP

    nc = o[
      lastCheck >= 7 |
        nest_state %in% c("pD", "pP"),
      .(nest, todo = "nest check")
    ] |> unique()


  # HATCH CHECK
    #! RULES
      #! if min_days_to_hatch <= 4
      #!  if eggs show no signs of hatch then do not check next day but in 2 days #TODO
      #! if not all chicks hatched (using, hatch_state, brood_size, clutch_size) #TODO
      
    hc = o[
      ! collected & 
      min_days_to_hatch <= 4,
      .(nest, todo_hc = "hatch check")
    ] |> unique()
    
    cc = merge(nc, hc, by = "nest", all = TRUE)
    cc[!is.na(todo_hc), todo := todo_hc]
    cc[, todo_hc := NULL]

    
  # CHICK PROCESSING
    #! RULES
      #! if hatch_state contains CC, C then go process chicks. #TODO

  # prepare final set
  out = merge(catch, cc, all = TRUE, suffixes = c("", "_x"))
  out[, todo := paste(c(todo, todo_x) |> na.omit(), collapse = ", "), by = .I]

  out[, todo_x := NULL]

  out
  

}