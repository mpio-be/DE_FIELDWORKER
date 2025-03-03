
# PACKAGES,SETTINGS
  sapply(
  c('data.table', 'dbo','stringr','here','glue','magrittr','lubridate', 
  'mgcv','gratia',
  'patchwork', 'ggplot2', 'ggbeeswarm'),
  require, character.only = TRUE, quietly = TRUE)


# NESTS
  x = dbq(
    q = 'SELECT * FROM NESTS where species = "NOLA" and nest_state = "F"', 
    db = "FIELD_2024_NOLAatDUMMERSEE"
  )

  g1 =
    ggplot(x, aes(x = date)) +
    geom_histogram()

# HATCH DATE ESTIMATION
  x = dbq(
    q = 'SELECT nest, plot, found_datetime,initial_clutch_size, clutch_size,hatching_datetime,
          mean_float_height2 egg_float_height, mean_float_angle2 egg_float_angle,
          CASE  WHEN comments LIKE "%incubator%" THEN 3 ELSE NULL END AS cls
          FROM BREEDING where 
            hatching_datetime is NOT NULL and 
            species = "NOLA" 
            ', 
    db = "FIELD_2024_NOLAatDUMMERSEE"
    )
  # x[mean_float_datetime != as.Date(found_datetime)]

  x = x[!(is.na(egg_float_angle) & initial_clutch_size == clutch_size)]
  x[, days_to_hatch :=  difftime(hatching_datetime, found_datetime, units = "days") |> as.numeric()]


  x[initial_clutch_size < clutch_size, cls := 1]
  x[is.na(cls), cls := 2]

  # incubation duration (direct computation)
  x[cls == 1, inc_start := found_datetime + days(clutch_size - initial_clutch_size)]
  x[cls == 1, incubation_duration := difftime(hatching_datetime, inc_start, units = "days") ]

  x[cls == 1, range(incubation_duration)]

  # incubation estimation (from floating data)
  z = x[cls == 2]

  g1 = ggplot(z, aes(x = egg_float_angle, y = days_to_hatch)) +
    geom_beeswarm(alpha = 0.5) + geom_smooth( )

  g2 = ggplot(z, aes(x = egg_float_height, y = days_to_hatch)) +
    geom_beeswarm(alpha = 0.5) +
    geom_smooth(method = "loess", span = 1.0)
  g1 + g2 + plot_annotation(title = "FIELD_2024_NOLAatDUMMERSEE") + plot_layout(axes ='collect')

  fm = gam(days_to_hatch ~ s(egg_float_angle ) + s(egg_float_height, k = 5), data = z)
  summary(fm)
  
  saveRDS(fm, "./main/data/gam_float_to_hach.rds")

  # MINIMUM PREDICTED (95 lower CI) HATCHING DATE
    pp = cbind(x[cls %in% c(1,3)], predict(fm, newdata = x[cls %in% c(1,3)], se.fit = TRUE) |> data.frame() )
    pp[, min_days_to_hatch := fit -  qnorm(0.975) * se.fit]
    pp[, predicted_hatch_date := found_datetime + min_days_to_hatch * 60 * 60 * 24]
    pp[, diff_to_pred := difftime(predicted_hatch_date, hatching_datetime, units = "days") |> round(1)]
    pp[!is.na(predicted_hatch_date), .(nest, cls,hatching_datetime, predicted_hatch_date, diff_to_pred)]
    median(pp$diff_to_pred, na.rm = TRUE)