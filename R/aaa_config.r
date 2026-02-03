#' @title rpmviz: Industrial RPM Visualization Tools
#' @description Shared configuration module for RPM visualization.
#' Contains machine specifications, color schemes, and helper functions.
#' @name rpmviz-config
NULL

#' Machine Configuration
#'
#' Default machine specifications for RPM calculations and dial display.
#' Customize these values for your specific machine.
#'
#' @format A list containing:
#' \describe{
#'   \item{dial_diameter_in}{Inner dial diameter in inches (default: 3.5)}
#'   \item{dial_outer_edge_dia_in}{Outer dial edge diameter in inches (default: 4.5)}
#'   \item{mounting_hole_dia_in}{Mounting hole diameter in inches (default: 0.25)}
#'   \item{start_angle}{Dial start angle in degrees (default: 90)}
#'   \item{stop_angle}{Dial stop angle in degrees (default: 450)}
#'   \item{motor_hp}{Motor horsepower (default: 1.5)}
#'   \item{motor_rpm}{Motor RPM (default: 1750)}
#'   \item{motor_pulley_diameter}{Motor pulley diameter in inches (default: 2.0)}
#'   \item{spindle_pulley_diameters}{Vector of spindle pulley diameters (default: c(2.5, 3.0, 3.5, 4.0))}
#'   \item{belt_slip_factor}{Belt efficiency factor, 0-1 (default: 0.98 for 2\% slip)}
#' }
#' @export
#' @examples
#' # View default configuration
#' MACHINE_CONFIG$motor_rpm
#'
#' # Create custom configuration
#' my_config <- create_machine_config(motor_rpm = 1800, motor_hp = 2.0)
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

#' Color Scheme Configuration
#'
#' Default color scheme for RPM visualization displays.
#'
#' @format A list containing:
#' \describe{
#'   \item{layer_colors}{Vector of colors for speed dial layers (innermost to outermost)}
#'   \item{safe_color}{Color for safe RPM values (default: "green")}
#'   \item{warning_color}{Color for warning RPM values (default: "yellow")}
#'   \item{danger_color}{Color for danger RPM values (default: "red")}
#'   \item{max_safe_rpm}{Maximum RPM considered safe (default: 3000)}
#'   \item{max_warning_rpm}{Maximum RPM before danger (default: 3500)}
#'   \item{zone_colors}{List of colors for material zones (wood, metal, plastic)}
#'   \item{bg_color}{Background color for digital displays (default: "black")}
#'   \item{label_color}{Label text color (default: "white")}
#'   \item{center_mark_color}{Dial center mark color (default: "lightgray")}
#' }
#' @export
#' @examples
#' # Get the safe color
#' COLOR_SCHEME$safe_color
#'
#' # Check RPM thresholds
#' COLOR_SCHEME$max_safe_rpm
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

#' Optimal Speed Ranges by Material
#'
#' Recommended RPM ranges for different materials.
#'
#' @format A list where each element is a 2-element vector c(min_rpm, max_rpm):
#' \describe{
#'   \item{wood}{Optimal RPM range for wood (default: 800-2000)}
#'   \item{metal}{Optimal RPM range for metal (default: 300-800)}
#'   \item{plastic}{Optimal RPM range for plastic (default: 1000-3000)}
#' }
#' @export
#' @examples
#' # Get optimal range for wood
#' MATERIAL_RANGES$wood
#'
#' # Check if 1500 RPM is optimal for wood
#' rpm <- 1500
#' range <- MATERIAL_RANGES$wood
#' rpm >= range[1] && rpm <= range[2]  # TRUE
MATERIAL_RANGES <- list(
  wood = c(800, 2000),
  metal = c(300, 800),
  plastic = c(1000, 3000)
)

#' Indicator Parameters
#'
#' Visual parameters for the dial indicator arrow and labels.
#'
#' @format A list containing arrow, text, and label settings.
#' @export
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

#' 7-Segment Display Parameters
#'
#' Visual parameters for the digital 7-segment RPM display.
#'
#' @format A list containing digit positioning, segment styling, and border settings.
#' @export
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

#' Dial Visual Parameters
#'
#' Visual parameters for the circular speed dial display.
#'
#' @format A list containing speed sequence, canvas, tick, label, and legend settings.
#' @export
DIAL_PARAMS <- list(
  # Speed sequence generation
  num_speed_steps = 10,

  # Canvas limits for each layer (innermost to outermost)
  canvas_limits = c(1.0, 1.16, 1.4, 1.75),

  # Track Y positions (in circos 0-1 coordinate space)
  tick_y_position = 0.5,
  label_y_position = 0.5,

  # Legend styling
  legend_cex = 0.75,
  legend_position = "topright",

  # Title font weight (1=normal, 2=bold)
  title_font_weight = 2
)

#' Bar Chart Parameters
#'
#' Visual parameters for the RPM bar chart display.
#'
#' @format A list containing RPM range defaults, margins, axis settings, and label styling.
#' @export
BAR_PARAMS <- list(
  # Default RPM range for bar display
  default_min_rpm = 200,
  default_max_rpm = 3500,

  # Plot margins (bottom, left, top, right)
  margins = c(3, 4, 2, 2),

  # Axis settings
  num_axis_labels = 5,
  axis_ticks = c(0, 25, 50, 75, 100),

  # Value label positioning
  value_label_x = 0.7,
  value_label_y_offset = 5,
  value_label_cex = 1.5,
  value_label_font = 2
)

#' Display Layout Parameters
#'
#' Layout parameters for display panels and dashboards.
#'
#' @format A list containing margin settings and dashboard grid dimensions.
#' @export
LAYOUT_PARAMS <- list(
  # Digital display margins (bottom, left, top, right)
  digital_margins = c(1, 1, 2, 1),

  # Dashboard layout
  dashboard_rows = 1,
  dashboard_cols = 2,

  # Title settings
  title_font = 2
)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Determine RPM Status Color
#'
#' Returns the appropriate color (safe, warning, or danger) based on RPM value.
#'
#' @param rpm Current RPM value
#' @param colors Color scheme list (defaults to COLOR_SCHEME)
#' @return Color string for the given RPM level
#' @export
#' @examples
#' get_rpm_color(1000)  # Returns safe color (green)
#' get_rpm_color(3200)  # Returns warning color (yellow)
#' get_rpm_color(4000)  # Returns danger color (red)
get_rpm_color <- function(rpm, colors = COLOR_SCHEME) {
  if (rpm > colors$max_warning_rpm) {
    return(colors$danger_color)
  } else if (rpm > colors$max_safe_rpm) {
    return(colors$warning_color)
  } else {
    return(colors$safe_color)
  }
}

#' Calculate Spindle RPM
#'
#' Calculates actual spindle RPM based on pulley ratio and motor speed,
#' accounting for belt slip.
#'
#' @param motor_rpm Motor RPM
#' @param motor_pulley_dia Motor pulley diameter (inches)
#' @param spindle_pulley_dia Spindle pulley diameter (inches)
#' @param belt_slip_factor Efficiency factor 0-1 (default from MACHINE_CONFIG, 0.98 = 2\% slip)
#' @return Calculated spindle RPM (rounded to nearest integer)
#' @export
#' @examples
#' # Calculate RPM for a 3.5" spindle pulley with 2" motor pulley
#' calculate_rpm(1750, 2.0, 3.5)
#'
#' # Calculate with custom slip factor
#' calculate_rpm(1750, 2.0, 3.5, belt_slip_factor = 0.95)
calculate_rpm <- function(motor_rpm, motor_pulley_dia, spindle_pulley_dia,
                          belt_slip_factor = MACHINE_CONFIG$belt_slip_factor) {
  # Input validation
  if (!is.numeric(motor_rpm) || length(motor_rpm) != 1 || motor_rpm <= 0) {
    stop("motor_rpm must be a positive number")
  }
  if (!is.numeric(motor_pulley_dia) || length(motor_pulley_dia) != 1 || motor_pulley_dia <= 0) {
    stop("motor_pulley_dia must be a positive number")
  }
  if (!is.numeric(spindle_pulley_dia) || length(spindle_pulley_dia) != 1 || spindle_pulley_dia <= 0) {
    stop("spindle_pulley_dia must be a positive number")
  }
  if (!is.numeric(belt_slip_factor) || length(belt_slip_factor) != 1 ||
      belt_slip_factor <= 0 || belt_slip_factor > 1) {
    stop("belt_slip_factor must be a number between 0 and 1")
  }

  pulley_ratio <- motor_pulley_dia / spindle_pulley_dia
  actual_rpm <- motor_rpm * pulley_ratio * belt_slip_factor
  return(round(actual_rpm))
}

#' Calculate All Available RPMs
#'
#' Calculates RPM for all pulley configurations in the machine config.
#'
#' @param machine_config Machine configuration list (defaults to MACHINE_CONFIG)
#' @return Named vector of RPMs for each pulley
#' @export
#' @examples
#' # Get all available RPMs
#' calculate_all_rpms()
#'
#' # Use custom machine config
#' my_config <- create_machine_config(motor_rpm = 2000)
#' calculate_all_rpms(my_config)
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

#' Create Speed Layer Configuration
#'
#' Factory function to create a speed layer configuration for the dial display.
#'
#' @param index Layer index (1-4)
#' @param speeds Vector of speed values for this layer
#' @param color Border color for the layer
#' @param canvas_lim Canvas limit for positioning
#' @param pulley_dia Pulley diameter for this layer
#' @return List containing layer configuration
#' @export
#' @examples
#' layer <- create_speed_layer(1, seq(200, 2000, 200), "blue", 1.0, 2.5)
create_speed_layer <- function(index, speeds, color, canvas_lim, pulley_dia) {
  # Input validation
  if (!is.numeric(index) || length(index) != 1 || index < 1) {
    stop("index must be a positive integer")
  }
  if (!is.numeric(speeds) || length(speeds) < 1) {
    stop("speeds must be a numeric vector with at least one element")
  }
  if (!is.character(color) || length(color) != 1) {
    stop("color must be a single character string")
  }
  if (!is.numeric(canvas_lim) || length(canvas_lim) != 1 || canvas_lim <= 0) {
    stop("canvas_lim must be a positive number")
  }
  if (!is.numeric(pulley_dia) || length(pulley_dia) != 1 || pulley_dia <= 0) {
    stop("pulley_dia must be a positive number")
  }

  list(
    name = paste0("speeds", index),
    data = speeds,
    border_color = color,
    canvas_lim = canvas_lim,
    pulley_size = pulley_dia,
    label = paste0("Pulley ", pulley_dia, '"')
  )
}

#' Generate Speed Sequence
#'
#' Generates a sequence of speed values for a given pulley configuration.
#'
#' @param pulley_index Index of the pulley (1-4)
#' @param num_steps Number of speed steps to generate (default: 10)
#' @param machine_config Machine configuration list
#' @return Vector of speed values
#' @export
#' @examples
#' # Generate speeds for pulley 1
#' generate_speed_sequence(1)
#'
#' # Generate with more steps
#' generate_speed_sequence(2, num_steps = 20)
generate_speed_sequence <- function(pulley_index, num_steps = 10,
                                    machine_config = MACHINE_CONFIG) {
  # Input validation
  if (!is.numeric(pulley_index) || length(pulley_index) != 1) {
    stop("pulley_index must be a single numeric value")
  }
  if (!is.numeric(num_steps) || length(num_steps) != 1 || num_steps < 1) {
    stop("num_steps must be a positive integer")
  }
  max_pulleys <- length(machine_config$spindle_pulley_diameters)
  if (pulley_index < 1 || pulley_index > max_pulleys) {
    stop(paste("pulley_index must be between 1 and", max_pulleys))
  }

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

#' Validate Pulley Index
#'
#' Validates that a pulley index is within the valid range.
#'
#' @param index Pulley index to validate
#' @param machine_config Machine configuration list
#' @return TRUE if valid, stops with error if invalid
#' @export
#' @examples
#' validate_pulley_index(1)  # TRUE
#' \dontrun{
#' validate_pulley_index(5)  # Error
#' }
validate_pulley_index <- function(index, machine_config = MACHINE_CONFIG) {
  max_pulleys <- length(machine_config$spindle_pulley_diameters)
  if (is.null(index)) {
    stop("Pulley index cannot be NULL")
  }
  if (!is.numeric(index) || length(index) != 1) {
    stop("Pulley index must be a single numeric value")
  }
  if (index < 1 || index > max_pulleys) {
    stop(paste("Pulley index must be between 1 and", max_pulleys, "- got:", index))
  }
  return(TRUE)
}

# ============================================================================
# INPUT VALIDATION FUNCTIONS
# ============================================================================

#' Validate that a value is a positive number
#' @param value Value to validate
#' @param name Name of the parameter (for error messages)
#' @param allow_zero Whether zero is allowed (default FALSE)
#' @return TRUE if valid, stops with error if invalid
validate_positive_number <- function(value, name = "value", allow_zero = FALSE) {
  if (is.null(value)) {
    stop(paste(name, "cannot be NULL"))
  }
  if (!is.numeric(value) || length(value) != 1) {
    stop(paste(name, "must be a single numeric value"))
  }
  if (is.na(value) || is.nan(value) || is.infinite(value)) {
    stop(paste(name, "must be a finite number, got:", value))
  }
  if (allow_zero) {
    if (value < 0) {
      stop(paste(name, "must be >= 0, got:", value))
    }
  } else {
    if (value <= 0) {
      stop(paste(name, "must be > 0, got:", value))
    }
  }
  return(TRUE)
}

#' Validate that a value is within a specified range
#' @param value Value to validate
#' @param min_val Minimum allowed value
#' @param max_val Maximum allowed value
#' @param name Name of the parameter (for error messages)
#' @return TRUE if valid, stops with error if invalid
validate_range <- function(value, min_val, max_val, name = "value") {
  validate_positive_number(value, name, allow_zero = TRUE)
  if (value < min_val || value > max_val) {
    stop(paste(name, "must be between", min_val, "and", max_val, "- got:", value))
  }
  return(TRUE)
}

#' Validate RPM Value
#'
#' Validates that an RPM value is valid (non-negative, finite number).
#'
#' @param rpm RPM value to validate
#' @param max_rpm Maximum allowed RPM before warning (default 10000)
#' @return TRUE if valid, stops with error if invalid
#' @export
#' @examples
#' validate_rpm(1500)  # TRUE
#' \dontrun{
#' validate_rpm(-100)  # Error
#' }
validate_rpm <- function(rpm, max_rpm = 10000) {
  if (is.null(rpm)) {
    stop("RPM value cannot be NULL")
  }
  if (!is.numeric(rpm) || length(rpm) != 1) {
    stop("RPM must be a single numeric value")
  }
  if (is.na(rpm) || is.nan(rpm) || is.infinite(rpm)) {
    stop(paste("RPM must be a finite number, got:", rpm))
  }
  if (rpm < 0) {
    stop(paste("RPM cannot be negative, got:", rpm))
  }
  if (rpm > max_rpm) {
    warning(paste("RPM value", rpm, "exceeds typical maximum of", max_rpm))
  }
  return(TRUE)
}

#' Validate Material Type
#'
#' Validates that a material name is one of the allowed types.
#'
#' @param material Material name to validate (NULL is allowed)
#' @param valid_materials List of valid material names (defaults to MATERIAL_RANGES keys)
#' @return TRUE if valid, stops with error if invalid
#' @export
#' @examples
#' validate_material("wood")  # TRUE
#' validate_material(NULL)    # TRUE (NULL allowed)
#' \dontrun{
#' validate_material("glass") # Error
#' }
validate_material <- function(material, valid_materials = names(MATERIAL_RANGES)) {
  if (is.null(material)) {
    return(TRUE)  # NULL is allowed (means no material zone)
  }
  if (!is.character(material) || length(material) != 1) {
    stop("Material must be a single character string")
  }
  if (!(material %in% valid_materials)) {
    stop(paste("Unknown material:", material,
               "- valid options:", paste(valid_materials, collapse = ", ")))
  }
  return(TRUE)
}

#' Validate color value
#' @param color Color value to validate
#' @param name Name of the parameter (for error messages)
#' @return TRUE if valid, stops with error if invalid
validate_color <- function(color, name = "color") {
  if (is.null(color)) {
    stop(paste(name, "cannot be NULL"))
  }
  if (!is.character(color) || length(color) != 1) {
    stop(paste(name, "must be a single character string"))
  }
  # Try to convert color - will error if invalid
  tryCatch({
    col2rgb(color)
  }, error = function(e) {
    stop(paste("Invalid color for", name, ":", color))
  })
  return(TRUE)
}

#' Validate a numeric vector
#' @param vec Vector to validate
#' @param name Name of the parameter (for error messages)
#' @param min_length Minimum required length (default 1)
#' @param require_positive Whether all values must be positive (default FALSE)
#' @return TRUE if valid, stops with error if invalid
validate_numeric_vector <- function(vec, name = "vector", min_length = 1, require_positive = FALSE) {
  if (is.null(vec)) {
    stop(paste(name, "cannot be NULL"))
  }
  if (!is.numeric(vec)) {
    stop(paste(name, "must be numeric"))
  }
  if (length(vec) < min_length) {
    stop(paste(name, "must have at least", min_length, "element(s)"))
  }
  if (any(is.na(vec)) || any(is.nan(vec)) || any(is.infinite(vec))) {
    stop(paste(name, "contains invalid values (NA, NaN, or Inf)"))
  }
  if (require_positive && any(vec <= 0)) {
    stop(paste(name, "must contain only positive values"))
  }
  return(TRUE)
}

#' Validate a configuration list has required fields
#' @param config Configuration list to validate
#' @param required_fields Character vector of required field names
#' @param config_name Name of the config (for error messages)
#' @return TRUE if valid, stops with error if invalid
validate_config_fields <- function(config, required_fields, config_name = "config") {
 if (is.null(config)) {
    stop(paste(config_name, "cannot be NULL"))
  }
  if (!is.list(config)) {
    stop(paste(config_name, "must be a list"))
  }
  missing_fields <- required_fields[!(required_fields %in% names(config))]
  if (length(missing_fields) > 0) {
    stop(paste(config_name, "is missing required fields:",
               paste(missing_fields, collapse = ", ")))
  }
  return(TRUE)
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Convert Color to RGBA
#'
#' Converts a color name or hex code to an RGBA color with transparency.
#'
#' @param color Color name or hex code
#' @param alpha Alpha value 0-1 (default: 0.5)
#' @return RGB color string with alpha
#' @export
#' @examples
#' color_with_alpha("red", 0.5)
#' color_with_alpha("#FF0000", 0.3)
color_with_alpha <- function(color, alpha = 0.5) {
  rgb_vals <- col2rgb(color)
  rgb(
    rgb_vals[1] / 255,
    rgb_vals[2] / 255,
    rgb_vals[3] / 255,
    alpha
  )
}

#' Format Speed Range String
#'
#' Formats a speed range as a human-readable string.
#'
#' @param min_speed Minimum speed value
#' @param max_speed Maximum speed value
#' @param unit Unit string (default: "RPM")
#' @return Formatted string like "200-2000 RPM"
#' @export
#' @examples
#' format_speed_range(200, 2000)       # "200-2000 RPM"
#' format_speed_range(100, 500, "Hz")  # "100-500 Hz"
format_speed_range <- function(min_speed, max_speed, unit = "RPM") {
  paste0(min_speed, "-", max_speed, " ", unit)
}

#' Clamp Value to Range
#'
#' Restricts a value to be within specified bounds.
#'
#' @param x Value to clamp
#' @param min_val Minimum allowed value (default: 0)
#' @param max_val Maximum allowed value (default: 1)
#' @return Clamped value
#' @export
#' @examples
#' clamp(0.5, 0, 1)   # 0.5 (within range)
#' clamp(-1, 0, 1)    # 0 (below min)
#' clamp(2, 0, 1)     # 1 (above max)
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

# ============================================================================
# CONFIG FACTORY FUNCTIONS (for immutable config management)
# ============================================================================

#' Create Custom Machine Configuration
#'
#' Creates a new machine configuration with custom values, using defaults for unspecified parameters.
#'
#' @param ... Named parameters to override defaults (e.g., motor_rpm = 1800)
#' @param base_config Base configuration to copy from (defaults to MACHINE_CONFIG)
#' @return New machine configuration list
#' @export
#' @examples
#' # Create config with faster motor
#' my_config <- create_machine_config(motor_rpm = 2000, motor_hp = 2.0)
create_machine_config <- function(..., base_config = MACHINE_CONFIG) {
  overrides <- list(...)
  new_config <- base_config

  for (name in names(overrides)) {
    if (name %in% names(new_config)) {
      new_config[[name]] <- overrides[[name]]
    } else {
      warning(paste("Unknown config parameter:", name))
    }
  }

  return(new_config)
}

#' Create Custom Color Scheme
#'
#' Creates a new color scheme with custom values, using defaults for unspecified parameters.
#'
#' @param ... Named parameters to override defaults
#' @param base_scheme Base scheme to copy from (defaults to COLOR_SCHEME)
#' @return New color scheme list
#' @export
#' @examples
#' # Create scheme with different safe thresholds
#' my_scheme <- create_color_scheme(max_safe_rpm = 2500, safe_color = "cyan")
create_color_scheme <- function(..., base_scheme = COLOR_SCHEME) {
  overrides <- list(...)
  new_scheme <- base_scheme

  for (name in names(overrides)) {
    if (name %in% names(new_scheme)) {
      new_scheme[[name]] <- overrides[[name]]
    } else {
      warning(paste("Unknown color scheme parameter:", name))
    }
  }

  return(new_scheme)
}

#' Merge Configuration Lists
#'
#' Merges two configuration lists, with values from the second overriding the first.
#' Handles nested lists recursively.
#'
#' @param base Base configuration list
#' @param overrides Configuration list with values to override
#' @return Merged configuration list
#' @export
#' @examples
#' base <- list(a = 1, b = 2)
#' overrides <- list(b = 20, c = 3)
#' merge_config(base, overrides)  # list(a = 1, b = 20, c = 3)
merge_config <- function(base, overrides) {
  result <- base
  for (name in names(overrides)) {
    if (is.list(overrides[[name]]) && name %in% names(base) && is.list(base[[name]])) {
      # Recursively merge nested lists
      result[[name]] <- merge_config(base[[name]], overrides[[name]])
    } else {
      result[[name]] <- overrides[[name]]
    }
  }
  return(result)
}

#' Copy Configuration
#'
#' Creates a deep copy of a configuration list to ensure immutability.
#'
#' @param config Configuration list to copy
#' @return Deep copy of the configuration
#' @export
#' @examples
#' original <- MACHINE_CONFIG
#' copied <- copy_config(original)
copy_config <- function(config) {
  # In R, lists are copy-on-modify, but this makes the copy explicit
  # and handles nested lists properly
  if (is.list(config)) {
    return(lapply(config, copy_config))
  }
  return(config)
}
