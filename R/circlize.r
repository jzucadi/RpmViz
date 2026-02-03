# ============================================================================
# CIRCULAR SPEED DIAL VISUALIZATION
# Uses circlize package for industrial-style RPM dial display
# ============================================================================

# Note: circlize is imported via NAMESPACE, aaa_config.r is loaded automatically by R

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
speeds1 <- generate_speed_sequence(1, num_steps = DIAL_PARAMS$num_speed_steps)
speeds2 <- generate_speed_sequence(2, num_steps = DIAL_PARAMS$num_speed_steps)
speeds3 <- generate_speed_sequence(3, num_steps = DIAL_PARAMS$num_speed_steps)
speeds4 <- generate_speed_sequence(4, num_steps = DIAL_PARAMS$num_speed_steps)

# ============================================================================
# CONFIGURATION & PARAMETERS
# ============================================================================

# Canvas limits for each layer (from shared config)
CANVAS_LIMITS <- DIAL_PARAMS$canvas_limits

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
#' @param dial_params Dial visual parameters (defaults to DIAL_PARAMS)
draw_tick_marks <- function(speeds, config, dial_params = DIAL_PARAMS) {
  if (!config$show_tick_marks) return()

  tick_y <- dial_params$tick_y_position

  # Major ticks at labeled speeds
  circos.points(
    speeds,
    rep(tick_y, length(speeds)),
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
    rep(tick_y, length(all_ticks)),
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
  speed_range <- c(min(speeds), max(speeds))

  # Only draw if ranges overlap (using utility function)
  if (!ranges_overlap(optimal_range, speed_range)) return()

  # Clip optimal range to actual speed range (using utility function)
  clipped <- clip_range(optimal_range, speed_range)

  # Get zone color (using utility function)
  zone_color <- get_zone_color(material)

  # Draw highlighted zone with semi-transparent fill (using utility function)
  circos.rect(
    xleft = clipped[1],
    xright = clipped[2],
    ybottom = 0,
    ytop = 1,
    col = color_with_alpha(zone_color, config$zone_params$fill_alpha),
    border = zone_color,
    lwd = config$zone_params$border_lwd
  )
}

#' Draw a circular speed track
#' @param layer_config List containing layer configuration
#' @param config Global configuration object
#' @param is_first_layer Boolean indicating if this is the first layer
draw_speed_track <- function(layer_config, config, is_first_layer = FALSE) {
  # Common circos parameters (DRY - defined once)
  common_params <- list(
    clock.wise = TRUE,
    start.degree = start_angle,
    gap.after = config$gap_after,
    cell.padding = config$cell_padding
  )

  # Set up canvas parameters
  if (!is_first_layer) {
    par(new = TRUE)
    # Add canvas limits for non-first layers
    do.call(circos.par, c(
      common_params,
      list(
        canvas.xlim = c(-layer_config$canvas_lim, layer_config$canvas_lim),
        canvas.ylim = c(-layer_config$canvas_lim, layer_config$canvas_lim)
      )
    ))
  } else {
    do.call(circos.par, common_params)
  }
  
  # Initialize and draw track
  speeds <- layer_config$data
  circos.initialize(
    factors = layer_config$name,
    xlim = c(min(speeds), max(speeds))
  )
  
  # Pre-compute font size once
  font_cex <- fontsize(config$font_size)
  
  # Get label Y position from dial params
  label_y <- DIAL_PARAMS$label_y_position

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
        rep(label_y, length(speeds)),
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
#' @param indicator_params Indicator parameters (defaults to INDICATOR_PARAMS)
draw_rpm_label <- function(config, indicator_params = INDICATOR_PARAMS) {
  if (!config$show_rpm_label) return()

  # Main RPM label (using configurable parameters)
  text(
    0, 0,
    "RPM",
    cex = indicator_params$rpm_label_cex,
    font = indicator_params$rpm_label_font,
    family = indicator_params$rpm_label_family,
    col = indicator_params$rpm_label_color
  )

  # Active pulley indicator
  active_layer <- config$speed_layers[[config$active_layer]]
  text(
    0, indicator_params$pulley_label_y_offset,
    active_layer$label,
    cex = indicator_params$pulley_label_cex,
    font = indicator_params$pulley_label_font,
    family = indicator_params$rpm_label_family,
    col = active_layer$border_color
  )
}

#' Draw current RPM indicator arrow
#' @param current_rpm Current machine speed
#' @param config Global configuration object
#' @param indicator_params Indicator parameters (defaults to INDICATOR_PARAMS)
draw_rpm_indicator <- function(current_rpm, config, indicator_params = INDICATOR_PARAMS) {
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

  # Calculate arrow endpoint (using utility function)
  arrow_length <- dial_diameter_in / 2 * indicator_params$arrow_length_factor
  arrow_pos <- polar_to_cartesian(angle_degrees, arrow_length)

  # Draw indicator arrow (using configurable parameters)
  arrows(
    0, 0,
    arrow_pos$x, arrow_pos$y,
    col = indicator_params$arrow_color,
    lwd = indicator_params$arrow_lwd,
    length = indicator_params$arrow_head_length,
    angle = indicator_params$arrow_head_angle
  )

  # Draw current speed text near arrow tip (using configurable parameters)
  text_pos <- polar_to_cartesian(angle_degrees, arrow_length * indicator_params$text_offset_factor)
  text(
    text_pos$x,
    text_pos$y,
    paste0(current_rpm, " RPM"),
    cex = indicator_params$text_cex,
    font = indicator_params$text_font,
    col = indicator_params$arrow_color
  )
}

#' Add machine specifications and legend
#' @param config Global configuration object
#' @param dial_params Dial visual parameters (defaults to DIAL_PARAMS)
draw_machine_info <- function(config, dial_params = DIAL_PARAMS) {
  if (!config$show_machine_info) return()

  # Prepare legend text
  legend_text <- c(
    paste0("Motor: ", config$motor_hp, " HP @ ", config$motor_rpm, " RPM"),
    paste0("Pulleys: ", paste(config$pulley_diameters, collapse = '", '), '"'),
    "",
    "Speed Layers:"
  )

  # Add layer information (using format_speed_range utility)
  for (i in seq_along(config$speed_layers)) {
    layer <- config$speed_layers[[i]]
    speed_range <- format_speed_range(min(layer$data), max(layer$data))
    legend_text <- c(legend_text, paste0("  ", layer$label, ": ", speed_range))
  }

  # Add material zone if active (using format_speed_range utility)
  if (config$show_speed_zones && !is.null(config$current_material)) {
    zone <- config$optimal_ranges[[config$current_material]]
    legend_text <- c(
      legend_text,
      "",
      paste0("Optimal for ", config$current_material, ": ", format_speed_range(zone[1], zone[2]))
    )
  }

  # Build font weight vector for legend
  base_fonts <- c(dial_params$title_font_weight, 1, 1, dial_params$title_font_weight)
  layer_fonts <- rep(1, length(config$speed_layers))
  zone_fonts <- if (config$show_speed_zones && !is.null(config$current_material)) c(1, 1) else c()

  # Draw legend (using configurable position and size)
  legend(
    dial_params$legend_position,
    legend = legend_text,
    bty = "n",
    cex = dial_params$legend_cex,
    text.font = c(base_fonts, layer_fonts, zone_fonts)
  )
}

# ============================================================================
# MAIN RENDERING FUNCTION
# ============================================================================

#' Render complete dial with all speed tracks and enhancements
#' @param config Configuration object containing all parameters
#' @export
render_dial <- function(config) {
  # Validate config object
  if (is.null(config) || !is.list(config)) {
    stop("config must be a non-null list")
  }

  required_fields <- c("speed_layers", "gap_after", "cell_padding",
                       "center_mark_length", "center_mark_color",
                       "show_tick_marks", "show_speed_zones", "show_rpm_label",
                       "show_machine_info", "active_layer")
  validate_config_fields(config, required_fields, "dial config")

  if (length(config$speed_layers) == 0) {
    stop("config$speed_layers must contain at least one layer")
  }

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

#' Create a modified copy of config with updated values (immutable)
#' @param config Configuration object to copy
#' @param current_speed New current RPM value (optional)
#' @param active_layer New active pulley layer 1-4 (optional)
#' @param current_material New material type for optimal zone (optional)
#' @return New config object with updated values
#' @export
update_config <- function(config,
                          current_speed = NULL,
                          active_layer = NULL,
                          current_material = NULL) {
  # Validate config object
  if (is.null(config) || !is.list(config)) {
    stop("config must be a non-null list")
  }
  validate_config_fields(config, c("speed_layers", "optimal_ranges"), "config")

  # Create a copy (immutable - don't modify original)
  new_config <- config

  # Update active layer if provided (with validation)
  if (!is.null(active_layer)) {
    if (!is.numeric(active_layer) || length(active_layer) != 1) {
      stop("active_layer must be a single numeric value")
    }
    if (active_layer < 1 || active_layer > length(config$speed_layers)) {
      stop(paste("active_layer must be between 1 and", length(config$speed_layers),
                 "- got:", active_layer))
    }
    new_config$active_layer <- active_layer
  }

  # Update material if provided (with validation)
  if (!is.null(current_material)) {
    validate_material(current_material, names(config$optimal_ranges))
    new_config$current_material <- current_material
  }

  # Update current speed if provided (with validation)
  if (!is.null(current_speed)) {
    validate_rpm(current_speed)
    new_config$current_speed <- current_speed
  }

  return(new_config)
}

#' Update config and re-render dial (convenience function)
#' Returns the updated config for further use
#' @param config Configuration object
#' @param new_rpm New current RPM value
#' @param new_pulley Optional: new active pulley layer (1-4)
#' @param new_material Optional: new material type for optimal zone
#' @return Updated configuration object
#' @export
update_and_render <- function(config, new_rpm, new_pulley = NULL, new_material = NULL) {
  updated_config <- update_config(
    config,
    current_speed = new_rpm,
    active_layer = new_pulley,
    current_material = new_material
  )
  render_dial(updated_config)
  return(invisible(updated_config))
}

#' Legacy wrapper for backward compatibility (uses global CONFIG)
#' @param new_rpm New current RPM value
#' @param new_pulley Optional: new active pulley layer (1-4)
#' @param new_material Optional: new material type for optimal zone
update_rpm_display <- function(new_rpm, new_pulley = NULL, new_material = NULL) {
  # Update the global CONFIG and re-render
  CONFIG <<- update_config(
    CONFIG,
    current_speed = new_rpm,
    active_layer = new_pulley,
    current_material = new_material
  )
  render_dial(CONFIG)
  return(invisible(CONFIG))
}

# ============================================================================
# EXECUTE (only when running interactively)
# ============================================================================

if (interactive()) {
  # Render the dial (initial display without indicator)
  render_dial(CONFIG)
}

# ============================================================================
# USAGE EXAMPLES
# ============================================================================
#
# IMMUTABLE APPROACH (recommended):
# ---------------------------------
# # Create a modified config without changing the original
# my_config <- update_config(CONFIG, current_speed = 1200, active_layer = 2)
# render_dial(my_config)
#
# # Chain multiple updates
# my_config <- update_config(CONFIG, current_speed = 800)
# my_config <- update_config(my_config, current_material = "metal")
# render_dial(my_config)
#
# # Convenience function that updates and renders in one call
# my_config <- update_and_render(CONFIG, new_rpm = 1500, new_pulley = 3)
#
# LEGACY APPROACH (modifies global CONFIG):
# -----------------------------------------
# # These functions modify the global CONFIG variable
# update_rpm_display(1200)
# update_rpm_display(800, new_pulley = 2)
# update_rpm_display(500, new_material = "metal")
#
# MANUAL CONFIG CHANGES:
# ----------------------
# # Create a custom config
# my_config <- CONFIG
# my_config$show_tick_marks <- FALSE
# my_config$show_speed_zones <- FALSE
# my_config$current_speed <- 1000
# render_dial(my_config)
