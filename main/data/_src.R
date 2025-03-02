
# PACKAGES,SETTINGS
  sapply(
  c('data.table', 'dbo','stringr','here','glue', 'ggplot2'),
  require, character.only = TRUE, quietly = TRUE)


# NESTS
  x = dbq(
    q = 'SELECT * FROM NESTS where species = "NOLA" and nest_state = "F"', 
    db = "FIELD_2024_NOLAatDUMMERSEE"
  )

  g1 =
    ggplot(x, aes(x = date)) +
    geom_histogram()