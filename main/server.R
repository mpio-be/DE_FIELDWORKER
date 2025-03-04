shinyServer(function(input, output, session) {

# For debugging purposes only (assign input values to global environment)
  observe({on.exit(assign('input', reactiveValuesToList(input), envir = .GlobalEnv))})

# Control bar: clock and hard drive status
  output$clock <- renderUI({
    invalidateLater(5000, session)
    glue('<kbd>{format(Sys.time(), "%d-%B %H:%M %Z")}</kbd>') %>% HTML()
  })
  
  output$hdd_state <- renderUI({
    dfsys_output()
  })
  
# ENTER DATA UI
  output$new_data <- renderUI({
    startApp(app_nam, "DataEntry", dbtabs_entry,
      host = session$clientData$url_hostname,
      labels = paste(icon("pencil"), dbtabs_entry)
    )
  })
  
# GPS UI
  output$open_gps <- renderUI({
    startApp(app_nam, "gpxui",
      host = session$clientData$url_hostname,
      labels = p(icon("location-crosshairs"), "GPS upload/download")
    )
  })
  
# DB interface UI
  output$open_db <- renderUI({
    startApp("db_ui", "field_db.php",
      isShiny = FALSE,
      host = session$clientData$url_hostname,
      labels = p(icon("database"), "Database interface")
    )
  })
  
  output$dbdump <- downloadHandler(
    filename = paste0(db, ".zip"),
    content = function(file) {
      dbTxtDump(zipfile = file)
    }
  )
  
# VIEW DATA: Helper function to create DataTables
  TABLE_show <- function(table_nam) {
    DT::renderDataTable({
      get_data <- reactivePoll(5000, session,
        checkFunc = function() {
          dbtable_is_updated(table_nam)
        },
        valueFunc = function() {
          DBq(glue("select * FROM {table_nam}"))[, ":="(pk = NULL, nov = NULL)] 
        }
      )
      get_data()
    },
    server        = FALSE,
    rownames      = FALSE,
    escape        = FALSE,
    extensions    = c("Scroller", "Buttons"),
    options       = list(
      dom         = "Blfrtip",
      buttons     = list("copy", list(
        extend = "collection",
        buttons = "excel",
        text = "Download"
      )),
      scrollX     = "600px",
      deferRender = TRUE,
      scrollY     = 900,
      scroller    = TRUE,
      searching   = TRUE,
      columnDefs  = list(list(className = "dt-center", targets = "_all"))
    ),
    class = c("compact", "stripe", "order-column", "hover")
    )
  }
  

  output$AUTHORS_show     <- TABLE_show("AUTHORS")
  output$CAPTURES_show    <- TABLE_show("CAPTURES")
  output$RESIGHTINGS_show <- TABLE_show("RESIGHTINGS")
  output$CHICKS_show      <- TABLE_show("CHICKS")
  output$NESTS_show       <- TABLE_show("NESTS")
  output$EGGS_show        <- TABLE_show("EGGS")
  output$SAMPLES_show     <- TABLE_show("SAMPLES")
  output$COMBOS_show      <- TABLE_show("COMBOS")
  
# Reactive for NESTS data (only update when one of the nest-related tabs is active)
  N <- reactive({
    if (input$main %in% c("nests_map", "live_nest_map", "todo")) {
      WaitToast("Processing nests...")
      n <- NESTS()
      nolat <- n[is.na(lat)]
      if (nrow(nolat) > 0) {
        ErrToast(
          glue("{paste(nolat$nest, collapse = ';')} without coordinates. Did you download all GPS units?")
        )
      }
      n[, N := .N, nest]
      doubleEntry <- n[N > 1]
      if (nrow(doubleEntry) > 0) {
        WarnToast(
          glue("Nests with inconsistent states: {paste(unique(doubleEntry$nest), collapse = ';')}")
        )
      }
      n
    }
  })
  
# STATIC NESTS MAP
  output$map_nests_show <- renderPlot({
    n <- N()
    req(n)
    grandN <- nrow(n)
    n <- subsetNESTS(n, state = input$nest_state, sp = input$nest_species, d2h = input$days_to_hatch)
    map_nests(n, size = input$nest_size, grandTotal = grandN)
  })
  
  output$map_nests_pdf <- downloadHandler(
    filename = "map_nests.pdf",
    content = function(file) {
      n <- N()
      req(n)
      grandN <- nrow(n)
      n <- subsetNESTS(n, state = input$nest_state, sp = input$nest_species, d2h = input$days_to_hatch)
      cairo_pdf(file = file, width = 11, height = 8.5)
      print(map_nests(n, size = input$nest_size, grandTotal = grandN))
      dev.off()
    }
  )
  
# DYNAMIC NESTS MAP: Initialize with a base leaflet map
  leafmap <- leaflet_map(x = studySiteCenter[1], y = studySiteCenter[2])
  output$nest_dynmap_show <- renderLeaflet(leafmap)
  
# Update dynamic map only when the "live_nest_map" tab is active
  observeEvent(input$main, {
    if (input$main == "live_nest_map") {
      n <- N()
      req(n)
      n <- st_as_sf(n[!is.na(lat)], coords = c("lon", "lat"), crs = 4326)
      if (nrow(n) > 0) {
        leafletProxy(mapId = "nest_dynmap_show") |>
          clearMarkers() |>
          clearShapes() |>
          addCircleMarkers(
            data        = n,
            fillOpacity = 0.5,
            opacity     = 0.5,
            radius      = ~3,
            label       = ~nest
          )
      }
    }
  })
  
# TO-DO
  output$nests_overview <- DT::renderDataTable({
    n <- N()
    req(n)
    setorder(n, days_till_hatching)
    n
  },
  server        = FALSE,
  rownames      = TRUE,
  escape        = FALSE,
  extensions    = c("Scroller", "Buttons"),
  options       = list(
    dom         = "Blfrtip",
    buttons     = list("copy", list(
      extend    = "collection",
      buttons   = c("excel", "pdf"),
      text = "Download"
    )),
    scrollX     = "600px",
    deferRender = TRUE,
    scrollY     = 900,
    scroller    = TRUE,
    searching   = TRUE,
    columnDefs  = list(list(className = "dt-center", targets = "_all"))
  ),
  class = c("compact", "stripe", "order-column", "hover")
  )

# HATCHING
  output$hatching_est_plot <- renderPlot({
    
    require(mgcv)     

    h = readRDS("./data/gam_float_to_hach.rds")

    pred =
      ggeffects::ggpredict(h, terms = c(
        glue("egg_float_angle [{input$float_angle}]"),
        glue("egg_float_height [{input$float_height}]")
      )) |> data.table()
    pred = pred[, .(predicted, conf.low, conf.high)]
    pred = melt(pred, measure.vars = names(pred))
    pred[, date_ := as.Date(input$refdate) + value]
    pred[, value := round(value, 1)]
    pred[, variable := factor(variable,
      labels = c("Most probable [average]", "Ealiest [95%CI-low]", "Latest [95%CI-high]")
    )]
    setnames(pred, c("", "Days to hatch", "Hatching date"))

    gtab = ggpubr::ggtexttable(pred,
      rows = NULL,
      theme =  ggpubr::ttheme(base_size = 18)
    )
      


    g1 =
      ggplot(h$model, aes(x = egg_float_angle, y = days_to_hatch)) +
      ggbeeswarm::geom_beeswarm(alpha = 0.5) +
      geom_smooth() +
      geom_vline(aes(xintercept = input$float_angle)) 

    g2 =
      ggplot(h$model, aes(x = egg_float_height, y = days_to_hatch)) +
      ggbeeswarm::geom_beeswarm(alpha = 0.5) +
      geom_smooth(method = "loess", span = 1.0) +
      geom_vline(aes(xintercept = input$float_height))
    
    gtab / (g1 + g2 ) + plot_layout(axes = "collect",heights = c(1,2) )



    
    })



session$allowReconnect(TRUE)

})
