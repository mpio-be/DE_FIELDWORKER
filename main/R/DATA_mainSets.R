# NOTE: subsets are done when mapping

NESTS_last_seasons <- function() {
  o = DBq("SELECT nest, lat latit,lon longit FROM NESTS n JOIN GPS_POINTS g
          ON n.gps_id = g.gps_id AND n.gps_point = g.gps_point AND nest_state = 'F'
          WHERE species = 'NOLA'",
    .db = "FIELD_2024_NOLAatDUMMERSEE"
  )
  o[, nest := str_remove(nest, "^L")]
  o
}

ALL_EGGS <- function(yy = years) {


  o = lapply(
    yy,
    function(x) {
      DBq(glue("SELECT date, nest, float_angle, surface
        FROM EGGS WHERE
        float_angle IS NOT NULL AND
        surface IS NOT NULL"), .db = yr2dbnam(x))
    }
  )
  
  o = rbindlist(o)

  d2h = hatching_prediction(o, .gampath = hatch_pred_gam)
  d2h = d2h[, .(
    min_days_to_hatch_at_found = min(conf.low, na.rm = TRUE),
    date = max(date, na.rm = TRUE) 
  ), nest]
  d2h = d2h[, min_pred_hatch_date := date + min_days_to_hatch_at_found]


  d2h



}

#* This function should work on all seasons. Any difference between seasons should be implemented by extensions to this function. 
#' n = NESTS()
#' n = NESTS(DB = yr2dbnam(2024), .refdate = "2024-04-26")
#' 
NESTS <- function(DB = db, .refdate = input$refdate) {
  
  # data
    x = DBq(glue("SELECT *  FROM NESTS WHERE date <= {shQuote(.refdate)}"), .db = DB)
    x[, pk := NULL]
    x = unique(x)

    x[, date := lubridate::ymd_hms(paste(date, time_appr))]
    setorder(x, nest, date)
  
  # last trap on date
    ton = x[!is.na(trap_on), .(trap_on_date = max(date)), by = nest]

  # last last_clutch = last counted clutch. 
    x[, clutch_size := nafill(clutch_size, "locf")]

    x[, lastDate := max(date), by = nest]
    x[, collected := any(nest_state == 'C'), nest]
  
  # last state (all nests); collected
    lst =  x[date == lastDate, .(nest, last_clutch = clutch_size, nest_state, collected,lastDate)]
    lst[, lastCheck := difftime(as.Date(.refdate), lastDate, units = "days") |> as.numeric() |> round(1)]
  
  # hatch_state (all recorded hatch signs)
    hst = x[str_detect(hatch_state, "[0-9]+(S|CC|C)"), .(nest, hatch_state = hatch_state)]
    hst = hst[, .(hatch_state = paste(unique(hatch_state), collapse = ";")), by = nest]


  # lat, long, datetime_found
    gps = DBq(glue('SELECT n.gps_id, n.gps_point, CONCAT_WS(" ",n.date,n.time_appr) datetime_found, n.nest, lat, lon
                    FROM NESTS n JOIN GPS_POINTS g on n.gps_id = g.gps_id AND n.gps_point = g.gps_point
                      WHERE n.gps_id is not NULL and
                      n.nest_state = "F" AND
                      n.date <= {shQuote(.refdate)}'), .db = DB)
    gps[, datetime_ := as.POSIXct(datetime_found)]
    gps = gps[, .(lat = mean(lat), lon = mean(lon), datetime_found = min(datetime_found)), .(nest)]

  # male, female COMBO
    # from nests (keep only proper combos, M only is excluded!)
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
    if(nrow(mfc2)>0) {
      mfc2[nchar(F) == 0, F := NA]
      mfc2[nchar(M) == 0, M := NA]
      setnames(mfc2, c("F", "M"), c("F_cap", "M_cap"))
    } else {
      mfc2[, let(F_cap = NA, M_cap = NA)]
    }

  # days to hatching
    x = DBq(glue("SELECT nest, date, float_angle, surface FROM EGGS
              WHERE date <= {shQuote(.refdate)}"), .db = DB)
    d2h = hatching_prediction(x, .gampath = hatch_pred_gam)
    d2h = d2h[, .(
      min_days_to_hatch_at_found = median(conf.low, na.rm = TRUE),
      date = max(date, na.rm = TRUE) 
    ), nest]
    d2h = d2h[, min_pred_hatch_date := date + min_days_to_hatch_at_found]
    d2h = d2h[, date := NULL]
    d2h[, min_days_to_hatch := as.numeric(min_pred_hatch_date - as.Date(.refdate)) |> round(1) ]


  # prepare final set
    
    o = merge(lst, gps,  by = "nest", all.x = TRUE)
    o = merge(o, mfc1,   by = "nest", all.x = TRUE)
    o = merge(o, mfc2,   by = "nest", all.x = TRUE)
    o = merge(o, ton,    by = "nest", all.x = TRUE)
    o = merge(o, hst,    by = "nest", all.x = TRUE)
    o = merge(o, d2h,    by = "nest", all.x = TRUE)


    o
}
