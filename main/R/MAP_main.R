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
      axis.text.y     = element_blank()
    )

}


#' d = NESTS()
#' d = NESTS(DB = "FIELD_2024_NOLAatDUMMERSEE", .refdate = "2024-04-26")[!is.na(lat)]
#' map_nests(d)
map_nests <- function(d, size = 2.5, grandTotal = nrow(d)) { 
  
  g = map_empty()

  x = d
  x[, nest := str_remove(nest, "^L")]

  x[, LAB := glue_data(.SD, "{nest}", .na = "")]  

  if (nrow(x) > 0) {
    g =
      g +
      geom_point(data = x, aes(lon, lat, color = nest_state), size = size) +
      geom_point(data = x[collected == 1], aes(lon, lat), size = size + 0.2, shape = 5, inherit.aes = FALSE) +
      geom_text_repel(data = x, aes(lon, lat, label = LAB), size = size) +

      labs(
        subtitle =
          glue("{grandTotal} nests, {nrow(x) } shown, {nrow(x[collected == 1])} collected clutches ◈")
      ) +

      scale_color_manual(values = nest_states, name = NULL) +
      scale_shape_manual(values = 5, name = NULL) +

      xlab(NULL) +
      ylab(NULL) +

      guides(
        color = guide_legend(nrow = 1),
        shape = guide_legend(nrow = 1)
      ) +
      
      ggspatial::annotation_scale(location = "bl", width_hint = 0.1, tick_height = 0.2) +
      
      theme(
        legend.position        = "inside",
        legend.position.inside = c(0.02, 0.90),
        legend.justification   = c(0, 1),
        legend.background      = element_blank(),     
        legend.key             = element_blank(),            
        legend.box             = "horizontal",               
        legend.text            = element_text(size = 10), 
        plot.subtitle          = element_text(size = 9, margin = margin(t = 20)) 
      )
  
  }


  print(g)

}


#' n = NESTS()
map_todo <- function(n, size = 2.5 ) {

  x = extract_TODO(n)
  x = x[, todo_yn := !is.na(todo)]
  
  g = map_empty()

  if (nrow(n) > 0) {

    g =
      g +

      geom_text_repel(
        data = x, aes(lon, lat, label = str_remove(nest, "^L"), color = todo_yn),
        size = size * 0.8,
        , show.legend = FALSE
      ) +
      scale_color_manual(values = c("grey", "black")) +

      ggnewscale::new_scale_colour() +

      geom_point(data = x, aes(lon, lat, color = todo), size = size) +
      
      guides(
        color = guide_legend(nrow = 2)
      ) +

      scale_color_brewer(palette = "Dark2", name = NULL, na.translate = FALSE) +

      ggspatial::annotation_scale(location = "bl", width_hint = 0.1, tick_height = 0.2) +
      
      xlab(NULL) + ylab(NULL) +
      
      theme(
        legend.position        = "inside",
        legend.position.inside = c(0.02, 0.98),
        legend.justification   = c(0, 1),
        legend.background      = element_blank(),     
        legend.key             = element_blank(),            
        legend.box             = "horizontal",               
        legend.text            = element_text(size = 10), 
        plot.subtitle          = element_text(size = 9) 
      )
  
  }
  




  print(g)






}
