

leaflet_map <- function(x = 11.235206 ,y =47.969993){
  leaflet(options = leafletOptions(zoomControl = TRUE)) |>
    setView(lng = as.numeric(x), lat = as.numeric(y), zoom = 15) |>
    addTiles(group = "OpenStreetMap") |>
  addControlGPS(options = 
    gpsOptions(position = "topleft", activate = TRUE, 
      autoCenter = TRUE, maxZoom = 10, 
      setView = TRUE)) |>
  activateGPS()


}
