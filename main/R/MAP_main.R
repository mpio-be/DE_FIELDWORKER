map_empty <- function(x = OsterFeinerMoor ) {
  
  ggplot() +
    geom_sf(data = x, fill = NA, color = "#cacaca") +
    ggtitle(format(Sys.time(), "%a, %d %b %y %H:%M")) +
    theme_minimal() +
    theme(
      panel.border    = element_blank(),
      panel.grid      = element_line(colour = "#ffffff", linewidth = 0),
      panel.spacing   = unit(c(0, 0, 0, 0), "cm"),
      axis.text.x     = element_blank(),
      axis.text.y     = element_blank(), 
      legend.position = "bottom"
    )

}




#' d = NESTS()
#' d = NESTS()[species == "NOLA"]
#' ds = sf::st_as_sf(d, coords = c("lon", "lat"))
#' mapview::mapview(ds, zcol = "nest")
#' map_nests(d)
map_nests <- function(d, size = 2.5) { # state  = "F"
  
  g = map_empty()

  x = d

  x[, LAB := glue_data(.SD, "{nest}({lastCheck}),{clutch}", .na = "")] # [{days_till_hatching}]

  if(nrow(x) > 0)
  
  g = 
  g +
    geom_point(data = x, aes(lon, lat, color = last_state), size = size) +
    geom_point(data = x[collected==1], aes(lon, lat), size = size+0.2, shape = 5, inherit.aes = FALSE) +
    geom_text_repel(data = x, aes(lon, lat, label = LAB), size = size) +
    labs(subtitle = glue("
    {nrow(x) } nests, {nrow(x[collected == 1])} collected clutches ◈.\n
    LEGEND: Nest (Last check - days ago), Clutch
    ")) +
    xlab(NULL) + ylab(NULL)


  print(g)

}


#' d = CAPTURES()
#' g = map_captures(d, 2.1) 
#' ggsave("~/Desktop/daily_map_cap.png", g, width = 8, height =   8, units = "in")


map_captures <- function(d, size = 2.5,  lastCapture = 5) {
  x = d[capturedDaysAgo <= lastCapture]

  map_empty() +
    geom_point(data = x, aes(lon, lat, color = capturedDaysAgo), size = size) +
    geom_text_repel(data = x, aes(lon, lat, label = combo), size = size) +
    scale_color_continuous(type = "viridis")
}


#' x = RESIGHTINGS() 
#' g = map_resightings(x, daysAgo = 1)
#' ggsave("~/Desktop/daily_map_res.png", g, width = 8, height =   8, units = "in")

map_resightings <- function(d, size = 2.5, daysAgo = 5) {
  x = copy(d)

  if (!missing(daysAgo) && daysAgo > 0) {
    x <- x[seenDaysAgo <= daysAgo]
  }
  x[, lab := glue_data(.SD, "{combo} [{seenDaysAgo}]", .na = '')]


  map_empty() +
    geom_point(data = x, aes(lon, lat), size = 1, color = "grey") +
    geom_label_repel(data = x, aes(lon, lat, label = lab), size = size) 

  
}


#' x = RESIGHTINGS_BY_ID('DB,Y', 'Y') 
#' map_resights_by_id(x)
#'
map_resights_by_id <- function(x, size = 2.5) {
  map_empty() +
    geom_point(data = x, aes(lon, lat, color = seenDaysAgo), size = size) +
    scale_colour_gradient(low = "red", high = "navy") +
    coord_wader()
}

#' x = RESIGHTINGS_BY_DAY() 
#' n = NESTS()
#' map_resights_by_day(x)
#' map_resights_by_day(x, n)
#'
map_resights_by_day <- function(x, n, size = 2.5) {
  g <-
    map_empty() +
    geom_point(data = x, aes(lon, lat), size = 1, color = "grey") +
    geom_text_repel(data = x, aes(lon, lat, label = combo), size = size) +
    coord_wader()


  if (!missing(n)) {
    g <- g +
      geom_point(data = n, aes(lon, lat), size = size, color = "red") +
      geom_text_repel(data = n, aes(lon, lat, label = nest), size = size)
  }


  g
}
