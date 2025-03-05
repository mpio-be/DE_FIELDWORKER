

leaflet_map <- function(x = studySiteCenter[1], y = studySiteCenter[2]){
  
  lsn = readRDS("./data/last_season_nests.rds")
  lsn$nest = str_remove(lsn$nest, '^L')
  
  leaflet(
    options = leafletOptions(zoomControl = TRUE)
  ) |>
  
  addTiles(group = "Street Map") |>
    
  addProviderTiles("OpenStreetMap",     group = "Street Map") |>
  addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
  addProviderTiles("OpenTopoMap",       group = "Topo Map")  |>

  addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = FALSE, maxWidth = 200) ) |>

  setView(
    lng = as.numeric(x),
    lat = as.numeric(y),
    zoom = 15
  ) |>
  addPolygons(
    group = "subplots",
    data = OsterFeinerMoor,
    label = ~id,
    labelOptions = labelOptions(permanent = TRUE, textOnly = TRUE),
    fillOpacity = 0,
    color = "#e24c4c",
    weight = 1
  ) |>
  
  addCircleMarkers(
    group = "last season nests",
    data = lsn,
    label = ~nest,
    radius = 5, 
    fillColor = "#03175a", 
    fillOpacity = 0.2, 
    color = NA,
    stroke = FALSE,
    fill = TRUE
  ) |>

  addLayersControl(
    baseGroups = c("Street Map", "Satellite",  "Topo Map"),
    
    overlayGroups = c("subplots", "last season nests"),   
    
    options = layersControlOptions(
      collapsed = FALSE,
      position = "topleft"
      )   
    )


}
