# ============================================================================
# CONFIGURATION & PARAMETERS
# ============================================================================

CONFIG <- list(
  font_size = 15,
  scale_height = (dial_outer_edge_dia_in - dial_diameter_in) / length(spindle_pulley_diameters),
  gap_after = stop_angle - start_angle,
  cell_padding = c(0.00, 1.00, 0.00, 1.00),
  center_mark_length = 0.1,
  center_mark_color = "lightgray",
  
  # Define all speed layers in a structured way
  speed_layers = list(
    list(name = "speeds1", data = speeds1, border_color = "light blue", canvas_lim = 1.0),
    list(name = "speeds2", data = speeds2, border_color = "yellow", canvas_lim = 1.16),
    list(name = "speeds3", data = speeds3, border_color = "red", canvas_lim = 1.4),
    list(name = "speeds4", data = speeds4, border_color = "purple", canvas_lim = 1.75)
  ),
  
  # Text parameters
  text_params = list(
    facing = "outside",
    niceFacing = TRUE,
    font = 2,
    family = "serif"
  )
)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Draw a circular speed track
#' @param layer_config List containing layer configuration
#' @param config Global configuration object
#' @param is_first_layer Boolean indicating if this is the first layer
draw_speed_track <- function(layer_config, config, is_first_layer = FALSE) {
  # Set up canvas parameters
  if (!is_first_layer) {
    par(new = TRUE)
    circos.par(
      canvas.xlim = c(-layer_config$canvas_lim, layer_config$canvas_lim),
      canvas.ylim = c(-layer_config$canvas_lim, layer_config$canvas_lim),
      clock.wise = TRUE,
      start.degree = start_angle,
      gap.after = config$gap_after,
      cell.padding = config$cell_padding
    )
  } else {
    circos.par(
      clock.wise = TRUE,
      start.degree = start_angle,
      gap.after = config$gap_after,
      cell.padding = config$cell_padding
    )
  }
  
  # Initialize and draw track
  speeds <- layer_config$data
  circos.initialize(
    factors = layer_config$name,
    xlim = c(min(speeds), max(speeds))
  )
  
  # Pre-compute font size once
  font_cex <- fontsize(config$font_size)
  
  circos.track(
    ylim = c(0, 1),
    bg.border = layer_config$border_color,
    track.height = convert_length(config$scale_height, "in"),
    panel.fun = function(x, y) {
      circos.text(
        speeds,
        rep(0.5, length(speeds)),
        speeds,
        facing = config$text_params$facing,
        niceFacing = config$text_params$niceFacing,
        cex = font_cex,
        font = config$text_params$font,
        family = config$text_params$family
      )
    }
  )
  
  circos.clear()
}

#' Draw center marks and boundaries
#' @param config Global configuration object
draw_center_marks <- function(config) {
  # Draw center crosshairs
  lines(
    x = c(-config$center_mark_length, config$center_mark_length),
    y = c(0, 0),
    col = config$center_mark_color
  )
  lines(
    x = c(0, 0),
    y = c(-config$center_mark_length, config$center_mark_length),
    col = config$center_mark_color
  )
  
  # Draw dial boundary circles
  symbols(
    x = c(0, 0),
    y = c(0, 0),
    circles = c(dial_diameter_in / 2, mounting_hole_dia_in / 2),
    fg = config$center_mark_color,
    inches = FALSE,
    add = TRUE
  )
}

# ============================================================================
# MAIN RENDERING FUNCTION
# ============================================================================

#' Render complete dial with all speed tracks
#' @param config Configuration object containing all parameters
render_dial <- function(config) {
  # Clear any existing circos plots
  circos.clear()
  
  # Draw all speed tracks
  for (i in seq_along(config$speed_layers)) {
    draw_speed_track(
      layer_config = config$speed_layers[[i]],
      config = config,
      is_first_layer = (i == 1)
    )
    
    # Draw center marks after first layer
    if (i == 1) {
      draw_center_marks(config)
    }
  }
}

# ============================================================================
# EXECUTE
# ============================================================================
render_dial(CONFIG)
