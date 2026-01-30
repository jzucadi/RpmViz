# ============================================================================
# DIGITAL RPM DISPLAY MODULE
# Complementary to circlize.r speed dial - displays actual RPM values
# ============================================================================

library(circlize)

# Load shared configuration (provides MACHINE_CONFIG, COLOR_SCHEME,
# calculate_rpm, calculate_all_rpms, get_rpm_color, etc.)
source("R/config.r")

# ============================================================================
# LOCAL REFERENCES (from shared config for backward compatibility)
# ============================================================================

spindle_pulley_diameters <- MACHINE_CONFIG$spindle_pulley_diameters
motor_pulley_diameter <- MACHINE_CONFIG$motor_pulley_diameter
motor_rpm <- MACHINE_CONFIG$motor_rpm

# Note: calculate_rpm() and calculate_all_rpms() are now provided by R/config.r

# ============================================================================
# DIGITAL DISPLAY CONFIGURATION
# ============================================================================

DISPLAY_CONFIG <- list(
  # Display dimensions
  width = 4,
  height = 2,

  # Colors (from shared COLOR_SCHEME)
  bg_color = COLOR_SCHEME$bg_color,
  digit_color = COLOR_SCHEME$danger_color,
  label_color = COLOR_SCHEME$label_color,
  warning_color = COLOR_SCHEME$warning_color,
  danger_color = COLOR_SCHEME$danger_color,
  safe_color = COLOR_SCHEME$safe_color,

  # Warning thresholds (from shared COLOR_SCHEME)
  max_safe_rpm = COLOR_SCHEME$max_safe_rpm,
  max_warning_rpm = COLOR_SCHEME$max_warning_rpm,

  # Font settings
  digit_cex = 5,
  label_cex = 1.2,
  unit_cex = 2
)

# ============================================================================
# DISPLAY RENDERING FUNCTIONS
# ============================================================================

#' Render a 7-segment style digit
#' @param digit Single digit (0-9) or '-'
#' @param x X position
#' @param y Y position
#' @param size Digit size
#' @param color Digit color
draw_segment_digit <- function(digit, x, y, size = 1, color = "red") {
  # Segment definitions (which segments are on for each digit)
  # Segments: top, top-right, bottom-right, bottom, bottom-left, top-left, middle
  segments <- list(
    "0" = c(1, 1, 1, 1, 1, 1, 0),
    "1" = c(0, 1, 1, 0, 0, 0, 0),
    "2" = c(1, 1, 0, 1, 1, 0, 1),
    "3" = c(1, 1, 1, 1, 0, 0, 1),
    "4" = c(0, 1, 1, 0, 0, 1, 1),
    "5" = c(1, 0, 1, 1, 0, 1, 1),
    "6" = c(1, 0, 1, 1, 1, 1, 1),
    "7" = c(1, 1, 1, 0, 0, 0, 0),
    "8" = c(1, 1, 1, 1, 1, 1, 1),
    "9" = c(1, 1, 1, 1, 0, 1, 1),
    "-" = c(0, 0, 0, 0, 0, 0, 1)
  )
  
  # Segment positions (relative to center)
  seg_pos <- list(
    list(x = c(-0.3, 0.3), y = c(0. 5, 0.5)),   # top
    list(x = c(0.35, 0.35), y = c(0.25, 0.45)), # top-right
    list(x = c(0.35, 0.35), y = c(-0.45, -0.05)), # bottom-right
    list(x = c(-0. 3, 0. 3), y = c(-0.5, -0.5)),  # bottom
    list(x = c(-0.35, -0.35), y = c(-0.45, -0.05)), # bottom-left
    list(x = c(-0.35, -0.35), y = c(0.05, 0.45)), # top-left
    list(x = c(-0.3, 0.3), y = c(0, 0))         # middle
  )
  
  digit_char <- as.character(digit)
  if (!(digit_char %in% names(segments))) return()
  
  active_segs <- segments[[digit_char]]
  
  for (i in seq_along(active_segs)) {
    if (active_segs[i] == 1) {
      lines(
        x + seg_pos[[i]]$x * size,
        y + seg_pos[[i]]$y * size,
        col = color,
        lwd = 4
      )
    }
  }
}

#' Render complete RPM value as 7-segment display
#' @param rpm RPM value to display
#' @param config Display configuration
draw_rpm_value <- function(rpm, config = DISPLAY_CONFIG) {
  # Determine display color based on RPM (using shared helper)
  digit_color <- get_rpm_color(rpm, config)

  # Format RPM as 4-digit string
  rpm_str <- sprintf("%4d", rpm)
  
  # Draw each digit
  digits <- strsplit(rpm_str, "")[[1]]
  x_positions <- c(-1.5, -0.5, 0.5, 1.5)
  
  for (i in seq_along(digits)) {
    if (digits[i] != " ") {
      draw_segment_digit(digits[i], x_positions[i], 0, size = 0.8, color = digit_color)
    }
  }
  
  # Draw "RPM" unit label
  text(2. 5, 0, "RPM", col = config$label_color, cex = config$unit_cex, font = 2)
}

#' Create the complete digital display
#' @param current_rpm Current RPM to display
#' @param active_pulley Active pulley index (1-4)
#' @param config Display configuration
render_digital_display <- function(current_rpm, active_pulley = 1, 
                                   config = DISPLAY_CONFIG) {
  # Set up plot
  par(bg = config$bg_color, mar = c(1, 1, 2, 1))
  plot(
    NULL,
    xlim = c(-config$width/2, config$width/2),
    ylim = c(-config$height/2, config$height/2),
    xlab = "", ylab = "", axes = FALSE,
    main = ""
  )
  
  # Draw display border
  rect(
    -config$width/2 + 0.1, -config$height/2 + 0.1,
    config$width/2 - 0.1, config$height/2 - 0.1,
    border = "gray40", lwd = 3
  )
  
  # Draw RPM value
  draw_rpm_value(current_rpm, config)
  
  # Draw pulley indicator at bottom
  pulley_text <- paste0("Pulley ", active_pulley, " (", 
                        spindle_pulley_diameters[active_pulley], '")')
  text(0, -0.8, pulley_text, col = config$label_color, cex = 0.9)
  
  # Draw title
  title(main = "SPINDLE SPEED", col. main = config$label_color, font. main = 2)
}

#' Display RPM bar graph showing current position in range
#' @param current_rpm Current RPM value
#' @param min_rpm Minimum RPM for scale
#' @param max_rpm Maximum RPM for scale
#' @param config Display configuration
render_rpm_bar <- function(current_rpm, min_rpm = 200, max_rpm = 3500,
                           config = DISPLAY_CONFIG) {
  par(bg = config$bg_color, mar = c(3, 4, 2, 2))
  
  # Calculate percentage
  pct <- (current_rpm - min_rpm) / (max_rpm - min_rpm)
  pct <- max(0, min(1, pct))  # Clamp to 0-1

  # Determine color (using shared helper)
  bar_color <- get_rpm_color(current_rpm, config)

  # Draw bar
  barplot(
    pct * 100,
    col = bar_color,
    border = NA,
    ylim = c(0, 100),
    ylab = "",
    xlab = "",
    axes = FALSE
  )
  
  # Add scale
  axis(2, at = c(0, 25, 50, 75, 100), 
       labels = round(seq(min_rpm, max_rpm, length.out = 5)),
       col = config$label_color, col.axis = config$label_color)
  
  # Add current value label
  text(0. 7, pct * 100 + 5, paste0(current_rpm, " RPM"), 
       col = config$label_color, cex = 1.5, font = 2)
  
  title(main = "RPM Level", col.main = config$label_color)
}

#' Create combined dashboard with dial reference and digital display
#' @param current_rpm Current RPM to display
#' @param active_pulley Active pulley (1-4)
render_rpm_dashboard <- function(current_rpm, active_pulley = 1) {
  # Set up 2-panel layout
  par(mfrow = c(1, 2))
  
  # Panel 1: Digital display
  render_digital_display(current_rpm, active_pulley)
  
  # Panel 2: Bar indicator
  render_rpm_bar(current_rpm)
  
  # Reset layout
  par(mfrow = c(1, 1))
}

# ============================================================================
# EXECUTE - Example Usage
# ============================================================================

# Calculate RPMs for all pulleys (using shared function with MACHINE_CONFIG)
all_rpms <- calculate_all_rpms()
print("Available RPMs by pulley:")
print(all_rpms)

# Example: Display current RPM
current_rpm <- 1200
active_pulley <- 2

# Render the digital display
render_digital_display(current_rpm, active_pulley)

# ============================================================================
# USAGE EXAMPLES
# ============================================================================

# Example 1: Show current speed on digital display
# render_digital_display(1500, active_pulley = 1)

# Example 2: Show RPM bar graph
# render_rpm_bar(2200)

# Example 3: Full dashboard view
# render_rpm_dashboard(1800, active_pulley = 3)

# Example 4: Calculate RPM from pulley sizes
# my_rpm <- calculate_rpm(motor_rpm = 1750, motor_pulley_dia = 2.0, 
#                         spindle_pulley_dia = 3.5)
# render_digital_display(my_rpm)
