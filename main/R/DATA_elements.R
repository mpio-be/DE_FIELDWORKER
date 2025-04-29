

yr2dbnam <- function(yr, prefix = 'FIELD',  dbnam = dbbasenam){
  glue("{prefix}_{yr}_{dbnam}")

}


colbyID <- function(x, id = "combo") {
  cc = x[, ..id] |> unique()
  cc[, col := rainbow(.N)]

  merge(x, cc, by = id, all.x = TRUE, sort = FALSE)
}

#' c('1:3', '1-3') |> expand_numeric_string()
#' c('1,4,5,6,7') |> expand_numeric_string()
expand_numeric_string <- function(x) {
  o = str_squish(x) |> str_remove("\\W$")
  o <- str_replace(o, "\\-", ":")
  o <- glue("c({o})")
  o <- try(parse(text = o) |> eval(), silent = TRUE)
  if (inherits(o, "try-error")) o <- NA
  as.numeric(o)
}

#' nest2species( nest = c('N201', 'P201', 'X102', 'N101')  )

nest2species <- function(nest) {
  sapply(nest, function(x) {
    n <- substr(x, 1, 1)
    ifelse(n == "N", "NOLA", NA)

  })
}

make_combo <- function(d, UL = "UL", LL = "LL", UR = "UR", LR = "LR", short) {
  
  x = copy(d)

  if (missing(short)) {
    cols <- c(UL, LL, UR, LR)
    cc <- x[, ..cols]
    setnames(cc, c("UL", "LL", "UR", "LR"))

    o <- cc[, .(COMBO = glue_data(.SD, "{UL}/{LL}|{UR}/{LR}", .na = "~"))]$COMBO
  }

  if (!missing(short)) {
    stopifnot(short %in% c("UL", "LL", "UR", "LR"))

    o <- glue("{x[, ..short][[1]]}", .na = "~")
  }

  o
}

tagID_from_combo <- function(co) {
  x = DBq("SELECT DISTINCT tagID, UL, LL, UR, LR FROM CAPTURES where tagID is not NULL")
  x[, combo := make_combo(.SD, short = "LR")]
  x[co == combo, tagID]
}



#' x = DBq("SELECT * FROM EGGS", .db = 'FIELD_2024_NOLAatDUMMERSEE')
#' 
#' 
hatching_prediction <- function(x, .gampath = hatch_pred_gam) {

  require(mgcv)
  fm = readRDS(.gampath)

  pp = predict(fm, newdata = x, se.fit = TRUE) |>
    data.frame() |>
    data.table()
  pp[, let(conf.low = fit - 1.96 * se.fit, conf.high = fit + 1.96 * se.fit)]
  setnames(pp, "fit", "predicted")

  pp = pp[, .(predicted, conf.low, conf.high)]

  cbind(x[,.(nest, date)], pp)


}