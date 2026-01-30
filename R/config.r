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
