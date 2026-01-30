# ============================================================================
# SHARED CONFIGURATION MODULE
# Central location for all machine specifications and shared parameters
# ============================================================================

# ============================================================================
# MACHINE SPECIFICATIONS
# Replace these with your actual machine specifications
# ============================================================================

MACHINE_CONFIG <- list(
  # Machine dimensions (inches)
  dial_diameter_in = 3.5,
  dial_outer_edge_dia_in = 4.5,
  mounting_hole_dia_in = 0.25,


  # Dial angles (degrees)
  start_angle = 90,
  stop_angle = 450,

  # Motor specifications
  motor_hp = 1.5,
  motor_rpm = 1750,
  motor_pulley_diameter = 2.0,

  # Pulley configurations (inches)
  spindle_pulley_diameters = c(2.5, 3.0, 3.5, 4.0),

  # Belt efficiency (accounts for slip)
  belt_slip_factor = 0.98
)

# ============================================================================
# DISPLAY COLOR SCHEMES
# ============================================================================

COLOR_SCHEME <- list(
  # Speed layer colors (innermost to outermost)
  layer_colors = c("light blue", "yellow", "red", "purple"),

  # RPM safety thresholds and colors
  safe_color = "green",
  warning_color = "yellow",
  danger_color = "red",

  # Threshold values (RPM)
  max_safe_rpm = 3000,
  max_warning_rpm = 3500,

  # Material zone colors
  zone_colors = list(
    wood = "green",
    metal = "blue",
    plastic = "orange"
  ),

  # Neutral colors
  bg_color = "black",
  label_color = "white",
  center_mark_color = "lightgray"
)

# ============================================================================
# OPTIMAL SPEED RANGES BY MATERIAL (RPM)
# ============================================================================

MATERIAL_RANGES <- list(
  wood = c(800, 2000),
  metal = c(300, 800),
  plastic = c(1000, 3000)
)

# ============================================================================
# INDICATOR PARAMETERS (for dial arrow and labels)
# ============================================================================

INDICATOR_PARAMS <- list(
  # Arrow settings
  arrow_length_factor = 0.85,    # Arrow length as fraction of dial radius
  arrow_color = "red",
  arrow_lwd = 4,
  arrow_head_length = 0.15,
  arrow_head_angle = 20,

  # Text settings
  text_offset_factor = 1.15,     # Distance of label from arrow tip
  text_cex = 1.0,
  text_font = 2,

  # Center label settings
  rpm_label_cex = 1.8,
  rpm_label_font = 2,
  rpm_label_family = "serif",
  rpm_label_color = "black",

  # Pulley indicator settings
  pulley_label_cex = 0.9,
  pulley_label_font = 1,
  pulley_label_y_offset = -0.15
)

# ============================================================================
# 7-SEGMENT DISPLAY PARAMETERS
# ============================================================================

SEGMENT_PARAMS <- list(
  # Digit positioning
  digit_positions = c(-1.5, -0.5, 0.5, 1.5),
  digit_size = 0.8,
  digit_spacing = 1.0,

  # Segment line settings
  segment_lwd = 4,

  # RPM label position
  rpm_label_x = 2.5,
  rpm_label_y = 0,

  # Pulley indicator position
  pulley_label_y = -0.8,
  pulley_label_cex = 0.9,

  # Display border
  border_color = "gray40",
  border_lwd = 3,
  border_margin = 0.1
)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Determine RPM status color based on thresholds
#' @param rpm Current RPM value
#' @param colors Color scheme list (defaults to COLOR_SCHEME)
#' @return Color string for the given RPM level
get_rpm_color <- function(rpm, colors = COLOR_SCHEME) {
  if (rpm > colors$max_warning_rpm) {
    return(colors$danger_color)
  } else if (rpm > colors$max_safe_rpm) {
    return(colors$warning_color)
  } else {
    return(colors$safe_color)
  }
}

#' Calculate actual spindle RPM based on pulley ratio and motor speed
#' @param motor_rpm Motor RPM
#' @param motor_pulley_dia Motor pulley diameter (inches)
#' @param spindle_pulley_dia Spindle pulley diameter (inches)
#' @param belt_slip_factor Optional efficiency loss (default from MACHINE_CONFIG)
#' @return Calculated spindle RPM (rounded)
calculate_rpm <- function(motor_rpm, motor_pulley_dia, spindle_pulley_dia,
                          belt_slip_factor = MACHINE_CONFIG$belt_slip_factor) {
  pulley_ratio <- motor_pulley_dia / spindle_pulley_dia
  actual_rpm <- motor_rpm * pulley_ratio * belt_slip_factor
  return(round(actual_rpm))
}

#' Calculate all available RPMs for current pulley configuration
#' @param machine_config Machine configuration list (defaults to MACHINE_CONFIG)
#' @return Named vector of RPMs for each pulley
calculate_all_rpms <- function(machine_config = MACHINE_CONFIG) {
  rpms <- sapply(machine_config$spindle_pulley_diameters, function(sp) {
    calculate_rpm(
      machine_config$motor_rpm,
      machine_config$motor_pulley_diameter,
      sp,
      machine_config$belt_slip_factor
    )
  })
  names(rpms) <- paste0("Pulley_", machine_config$spindle_pulley_diameters, "in")
  return(rpms)
}

#' Create a speed layer configuration
#' @param index Layer index (1-4)
#' @param speeds Vector of speed values for this layer
#' @param color Border color for the layer
#' @param canvas_lim Canvas limit for positioning
#' @param pulley_dia Pulley diameter for this layer
#' @return List containing layer configuration
create_speed_layer <- function(index, speeds, color, canvas_lim, pulley_dia) {
  list(
    name = paste0("speeds", index),
    data = speeds,
    border_color = color,
    canvas_lim = canvas_lim,
    pulley_size = pulley_dia,
    label = paste0("Pulley ", pulley_dia, '"')
  )
}

#' Generate speed sequence for a pulley
#' @param pulley_index Index of the pulley (1-4)
#' @param num_steps Number of speed steps to generate
#' @param machine_config Machine configuration list
#' @return Vector of speed values
generate_speed_sequence <- function(pulley_index, num_steps = 10,
                                    machine_config = MACHINE_CONFIG) {
  pulley_dia <- machine_config$spindle_pulley_diameters[pulley_index]
  max_rpm <- calculate_rpm(
    machine_config$motor_rpm,
    machine_config$motor_pulley_diameter,
    pulley_dia,
    machine_config$belt_slip_factor
  )

  # Generate evenly spaced sequence ending at max RPM
  step_size <- ceiling(max_rpm / num_steps / 50) * 50  # Round to nearest 50
  seq(step_size, max_rpm, by = step_size)
}

#' Validate pulley index is within valid range
#' @param index Pulley index to validate
#' @param machine_config Machine configuration list
#' @return TRUE if valid, stops with error if invalid
validate_pulley_index <- function(index, machine_config = MACHINE_CONFIG) {
  max_pulleys <- length(machine_config$spindle_pulley_diameters)
  if (is.null(index) || index < 1 || index > max_pulleys) {
    stop(paste("Pulley index must be between 1 and", max_pulleys))
  }
  return(TRUE)
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Convert a color to RGBA with specified alpha
#' @param color Color name or hex code
#' @param alpha Alpha value (0-1)
#' @return RGB color string with alpha
color_with_alpha <- function(color, alpha = 0.5) {
  rgb_vals <- col2rgb(color)
  rgb(
    rgb_vals[1] / 255,
    rgb_vals[2] / 255,
    rgb_vals[3] / 255,
    alpha
  )
}

#' Format a speed range as a string
#' @param min_speed Minimum speed value
#' @param max_speed Maximum speed value
#' @param unit Unit string (default "RPM")
#' @return Formatted string like "200-2000 RPM"
format_speed_range <- function(min_speed, max_speed, unit = "RPM") {
  paste0(min_speed, "-", max_speed, " ", unit)
}

#' Clamp a value between min and max bounds
#' @param x Value to clamp
#' @param min_val Minimum allowed value
#' @param max_val Maximum allowed value
#' @return Clamped value
clamp <- function(x, min_val = 0, max_val = 1) {
  max(min_val, min(max_val, x))
}

#' Convert degrees to radians
#' @param degrees Angle in degrees
#' @return Angle in radians
degrees_to_radians <- function(degrees) {
  degrees * pi / 180
}

#' Calculate position on a circle given angle and radius
#' @param angle_degrees Angle in degrees
#' @param radius Distance from center
#' @return List with x and y coordinates
polar_to_cartesian <- function(angle_degrees, radius) {
  angle_rad <- degrees_to_radians(angle_degrees)
  list(
    x = cos(angle_rad) * radius,
    y = sin(angle_rad) * radius
  )
}

#' Get zone color for a material type
#' @param material Material name (wood, metal, plastic)
#' @param colors Color scheme (defaults to COLOR_SCHEME)
#' @return Color string for the material zone
get_zone_color <- function(material, colors = COLOR_SCHEME) {
  if (material %in% names(colors$zone_colors)) {
    return(colors$zone_colors[[material]])
  }
  return(colors$zone_colors$wood)  # default
}

#' Check if two ranges overlap
#' @param range1 First range as c(min, max)
#' @param range2 Second range as c(min, max)
#' @return TRUE if ranges overlap
ranges_overlap <- function(range1, range2) {
  range1[1] <= range2[2] && range1[2] >= range2[1]
}

#' Clip a range to fit within bounds
#' @param range Range to clip as c(min, max)
#' @param bounds Bounds to clip to as c(min, max)
#' @return Clipped range as c(min, max)
clip_range <- function(range, bounds) {
  c(
    max(range[1], bounds[1]),
    min(range[2], bounds[2])
  )
}
