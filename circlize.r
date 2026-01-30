# ============================================================================
# CIRCULAR SPEED DIAL VISUALIZATION
# Uses circlize package for industrial-style RPM dial display
# ============================================================================

library(circlize)

# Load shared configuration
source("R/config.r")

# ============================================================================
# LOCAL REFERENCES (from shared config)
# ============================================================================

# Machine dimensions (from MACHINE_CONFIG)
dial_diameter_in <- MACHINE_CONFIG$dial_diameter_in
dial_outer_edge_dia_in <- MACHINE_CONFIG$dial_outer_edge_dia_in
mounting_hole_dia_in <- MACHINE_CONFIG$mounting_hole_dia_in

# Dial angles (from MACHINE_CONFIG)
start_angle <- MACHINE_CONFIG$start_angle
stop_angle <- MACHINE_CONFIG$stop_angle

# Pulley configurations (from MACHINE_CONFIG)
spindle_pulley_diameters <- MACHINE_CONFIG$spindle_pulley_diameters
motor_hp <- MACHINE_CONFIG$motor_hp
motor_rpm <- MACHINE_CONFIG$motor_rpm

# Speed data for each pulley configuration (calculated from pulley ratios)
speeds1 <- generate_speed_sequence(1, num_steps = 10)
speeds2 <- generate_speed_sequence(2, num_steps = 10)
speeds3 <- generate_speed_sequence(3, num_steps = 10)
speeds4 <- generate_speed_sequence(4, num_steps = 10)

# ============================================================================
# CONFIGURATION & PARAMETERS
# ============================================================================

# Canvas limits for each layer (innermost to outermost)
CANVAS_LIMITS <- c(1.0, 1.16, 1.4, 1.75)

CONFIG <- list(
  # Visual parameters
  font_size = 15,
  scale_height = (dial_outer_edge_dia_in - dial_diameter_in) / length(spindle_pulley_diameters),
  gap_after = stop_angle - start_angle,
  cell_padding = c(0.00, 1.00, 0.00, 1.00),
  center_mark_length = 0.1,
  center_mark_color = COLOR_SCHEME$center_mark_color,

  # Machine specifications (from shared config)
  motor_hp = MACHINE_CONFIG$motor_hp,
  motor_rpm = MACHINE_CONFIG$motor_rpm,
  pulley_diameters = MACHINE_CONFIG$spindle_pulley_diameters,

  # Current operating parameters
  current_speed = NULL,           # Set to show indicator arrow
  active_layer = 1,               # Which pulley is currently installed (1-4)
  show_tick_marks = TRUE,
  show_speed_zones = TRUE,
  show_rpm_label = TRUE,
  show_machine_info = TRUE,

  # Optimal speed ranges by material (from shared config)
  optimal_ranges = MATERIAL_RANGES,
  current_material = "wood",      # Which material zone to highlight

  # Define all speed layers using DRY generator function
  speed_layers = list(
    create_speed_layer(1, speeds1, COLOR_SCHEME$layer_colors[1], CANVAS_LIMITS[1], spindle_pulley_diameters[1]),
    create_speed_layer(2, speeds2, COLOR_SCHEME$layer_colors[2], CANVAS_LIMITS[2], spindle_pulley_diameters[2]),
    create_speed_layer(3, speeds3, COLOR_SCHEME$layer_colors[3], CANVAS_LIMITS[3], spindle_pulley_diameters[3]),
    create_speed_layer(4, speeds4, COLOR_SCHEME$layer_colors[4], CANVAS_LIMITS[4], spindle_pulley_diameters[4])
  ),

  # Text parameters
  text_params = list(
    facing = "outside",
    niceFacing = TRUE,
    font = 2,
    family = "serif"
  ),

  # Tick mark parameters
  tick_params = list(
    major_pch = "|",
    major_cex = 1.5,
    major_col = "black",
    minor_divisions = 50,
    minor_pch = "|",
    minor_cex = 0.5,
    minor_col = "gray60"
  ),

  # Speed zone highlighting (colors from shared config)
  zone_params = list(
    fill_alpha = 0.2,
    border_lwd = 2,
    wood_col = COLOR_SCHEME$zone_colors$wood,
    metal_col = COLOR_SCHEME$zone_colors$metal,
    plastic_col = COLOR_SCHEME$zone_colors$plastic
  )
)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Draw tick marks on the speed track
#' @param speeds Vector of speed values
#' @param config Global configuration object
draw_tick_marks <- function(speeds, config) {
  if (!config$show_tick_marks) return()
  
  # Major ticks at labeled speeds
  circos.points(
    speeds, 
    rep(0.5, length(speeds)), 
    pch = config$tick_params$major_pch, 
    cex = config$tick_params$major_cex,
    col = config$tick_params$major_col
  )
  
  # Minor ticks between major ones
  all_ticks <- seq(
    min(speeds), 
    max(speeds), 
    length.out = config$tick_params$minor_divisions
  )
  circos.points(
    all_ticks, 
    rep(0.5, length(all_ticks)), 
    pch = config$tick_params$minor_pch, 
    cex = config$tick_params$minor_cex, 
    col = config$tick_params$minor_col
  )
}

#' Draw optimal speed zones for different materials
#' @param speeds Vector of speed values
#' @param config Global configuration object
draw_speed_zones <- function(speeds, config) {
  if (!config$show_speed_zones || is.null(config$current_material)) return()
  
  material <- config$current_material
  if (!(material %in% names(config$optimal_ranges))) return()
  
  optimal_range <- config$optimal_ranges[[material]]
  
  # Only draw if range overlaps with current speeds
  if (optimal_range[1] <= max(speeds) && optimal_range[2] >= min(speeds)) {
    # Clip range to actual speed range
    zone_start <- max(optimal_range[1], min(speeds))
    zone_end <- min(optimal_range[2], max(speeds))
    
    # Select color based on material
    zone_color <- switch(
      material,
      wood = config$zone_params$wood_col,
      metal = config$zone_params$metal_col,
      plastic = config$zone_params$plastic_col,
      "green"  # default
    )
    
    # Draw highlighted zone
    circos.rect(
      xleft = zone_start,
      xright = zone_end,
      ybottom = 0, 
      ytop = 1,
      col = rgb(
        col2rgb(zone_color)[1]/255,
        col2rgb(zone_color)[2]/255,
        col2rgb(zone_color)[3]/255,
        config$zone_params$fill_alpha
      ),
      border = zone_color,
      lwd = config$zone_params$border_lwd
    )
  }
}

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
      # Draw speed zones first (background)
      draw_speed_zones(speeds, config)
      
      # Draw tick marks
      draw_tick_marks(speeds, config)
      
      # Draw speed labels
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

#' Draw RPM label in center of dial
#' @param config Global configuration object
draw_rpm_label <- function(config) {
  if (!config$show_rpm_label) return()
  
  # Main RPM label
  text(
    0, 0, 
    "RPM", 
    cex = 1.8, 
    font = 2, 
    family = "serif",
    col = "black"
  )
  
  # Active pulley indicator
  active_layer <- config$speed_layers[[config$active_layer]]
  text(
    0, -0.15, 
    active_layer$label,
    cex = 0.9,
    font = 1,
    family = "serif",
    col = active_layer$border_color
  )
}

#' Draw current RPM indicator arrow
#' @param current_rpm Current machine speed
#' @param config Global configuration object
draw_rpm_indicator <- function(current_rpm, config) {
  if (is.null(current_rpm)) return()
  
  # Get active layer data
  layer <- config$speed_layers[[config$active_layer]]
  speeds <- layer$data
  
  # Check if current_rpm is within range
  if (current_rpm < min(speeds) || current_rpm > max(speeds)) {
    warning("Current RPM outside of active layer range")
    return()
  }
  
  # Calculate angle for current speed
  speed_fraction <- (current_rpm - min(speeds)) / (max(speeds) - min(speeds))
  angle_degrees <- start_angle - speed_fraction * (start_angle - stop_angle)
  angle_rad <- angle_degrees * pi / 180
  
  # Calculate arrow endpoint
  arrow_length <- dial_diameter_in / 2 * 0.85
  arrow_x <- cos(angle_rad) * arrow_length
  arrow_y <- sin(angle_rad) * arrow_length
  
  # Draw indicator arrow
  arrows(
    0, 0, 
    arrow_x, arrow_y,
    col = "red", 
    lwd = 4, 
    length = 0.15,
    angle = 20
  )
  
  # Draw current speed text near arrow tip
  text_offset <- 1.15
  text(
    arrow_x * text_offset, 
    arrow_y * text_offset,
    paste0(current_rpm, " RPM"),
    cex = 1.0,
    font = 2,
    col = "red"
  )
}

#' Add machine specifications and legend
#' @param config Global configuration object
draw_machine_info <- function(config) {
  if (!config$show_machine_info) return()
  
  # Prepare legend text
  legend_text <- c(
    paste0("Motor: ", config$motor_hp, " HP @ ", config$motor_rpm, " RPM"),
    paste0("Pulleys: ", paste(config$pulley_diameters, collapse = '", '), '"'),
    "",
    "Speed Layers:"
  )
  
  # Add layer information
  for (i in seq_along(config$speed_layers)) {
    layer <- config$speed_layers[[i]]
    speed_range <- paste0(min(layer$data), "-", max(layer$data), " RPM")
    legend_text <- c(legend_text, paste0("  ", layer$label, ": ", speed_range))
  }
  
  # Add material zone if active
  if (config$show_speed_zones && !is.null(config$current_material)) {
    zone <- config$optimal_ranges[[config$current_material]]
    legend_text <- c(
      legend_text,
      "",
      paste0("Optimal for ", config$current_material, ": ", zone[1], "-", zone[2], " RPM")
    )
  }
  
  # Draw legend
  legend(
    "topright", 
    legend = legend_text,
    bty = "n", 
    cex = 0.75,
    text.font = c(2, 1, 1, 2, rep(1, length(config$speed_layers) + 
                   ifelse(config$show_speed_zones && !is.null(config$current_material), 2, 0)))
  )
}

# ============================================================================
# MAIN RENDERING FUNCTION
# ============================================================================

#' Render complete dial with all speed tracks and enhancements
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
    
    # Draw center elements after first layer
    if (i == 1) {
      draw_center_marks(config)
      draw_rpm_label(config)
    }
  }
  
  # Draw current speed indicator (if set)
  if (!is.null(config$current_speed)) {
    draw_rpm_indicator(config$current_speed, config)
  }
  
  # Draw machine information legend
  draw_machine_info(config)
}

#' Update and re-render dial with new current speed
#' @param new_rpm New current RPM value
#' @param new_pulley Optional: new active pulley layer (1-4)
#' @param new_material Optional: new material type for optimal zone
update_rpm_display <- function(new_rpm, new_pulley = NULL, new_material = NULL) {
  if (!is.null(new_pulley)) {
    if (new_pulley < 1 || new_pulley > length(CONFIG$speed_layers)) {
      warning("Invalid pulley number. Must be 1-", length(CONFIG$speed_layers))
    } else {
      CONFIG$active_layer <<- new_pulley
    }
  }
  
  if (!is.null(new_material)) {
    if (new_material %in% names(CONFIG$optimal_ranges)) {
      CONFIG$current_material <<- new_material
    } else {
      warning("Unknown material. Options: ", paste(names(CONFIG$optimal_ranges), collapse = ", "))
    }
  }
  
  CONFIG$current_speed <<- new_rpm
  render_dial(CONFIG)
}

# ============================================================================
# EXECUTE
# ============================================================================

# Render the dial (initial display without indicator)
render_dial(CONFIG)

# ============================================================================
# USAGE EXAMPLES
# ============================================================================

# Example 1: Show current speed on active pulley
# update_rpm_display(1200)

# Example 2: Change to different pulley and show speed
# update_rpm_display(800, new_pulley = 2)

# Example 3: Change material zone highlighting
# update_rpm_display(500, new_material = "metal")

# Example 4: Turn off optional features
# CONFIG$show_tick_marks <- FALSE
# CONFIG$show_speed_zones <- FALSE
# CONFIG$show_machine_info <- FALSE
# render_dial(CONFIG)
