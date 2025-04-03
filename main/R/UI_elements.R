


HR <- function() {
  a(hr(style = "border-top: 1px solid #9aaeb6;"))
}

S <- function(x = "-------", z = 1) {
  v <- str_split(x, ":", simplify = TRUE)
  if (length(v) == 2) {
    v <- glue("{em(v[1])}:{v[2]}")
  }
  v <- HTML(v)

  switch(z,
    "1" = strong(v, style = "color:#1d3658"),
    "2" = strong(v, style = "color:#d70427"),
    "3" = strong(v, style = "color:#ffb6a3"),
    "4" = strong(v, style = "color:#e8c468"),
    "5" = strong(v, style = "color:#b8d2ff")
  )
}


select_combo_list <- function() {
  DBq("SELECT DISTINCT UL, LL, UR, LR FROM CAPTURES where tagID is not NULL") |>
    make_combo(short = "LR")
}


spinner <- function(x) {

  shinycssloaders::withSpinner(x, 
  image = 'animated_ICO.png',
  image.width = "100cqw")

}