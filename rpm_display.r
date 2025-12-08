# ============================================================================
# DIGITAL RPM DISPLAY MODULE
# Complementary to circlize.r speed dial - displays actual RPM values
# ============================================================================

library(circlize)

# ============================================================================
# MACHINE SPECIFICATIONS (sync with circlize. r)
# ============================================================================

# Pulley configurations
spindle_pulley_diameters <- c(2. 5, 3. 0, 3. 5, 4. 0)
motor_pulley_diameter <- 2.0
motor_rpm <- 1750

# ============================================================================
# RPM CALCULATION FUNCTIONS
# ============================================================================

#' Calculate actual spindle RPM based on pulley ratio and motor speed
#' @param motor_rpm Motor RPM
#' @param motor_pulley_dia Motor pulley diameter (inches)
#' @param spindle_pulley_dia Spindle pulley diameter (inches)
#' @param belt_slip_factor Optional efficiency loss (default 0.98 = 2% slip)
#' @return Calculated spindle RPM
calculate_rpm <- function(motor_rpm, motor_pulley_dia, spindle_pulley_dia, 
                          belt_slip_factor = 0.98) {
  pulley_ratio <- motor_pulley_dia / spindle_pulley_dia
  actual_rpm <- motor_rpm * pulley_ratio * belt_slip_factor
  return(round(actual_rpm))
}

#' Calculate all available RPMs for current pulley configuration
#' @param motor_rpm Motor RPM
#' @param motor_pulley_dia Motor pulley diameter
#' @param spindle_pulleys Vector of spindle pulley diameters
#' @return Named vector of RPMs
calculate_all_rpms <- function(motor_rpm, motor_pulley_dia, spindle_pulleys) {
  rpms <- sapply(spindle_pulleys, function(sp) {
    calculate_rpm(motor_rpm, motor_pulley_dia, sp)
  })
  names(rpms) <- paste0("Pulley_", spindle_pulleys, "in")
  return(rpms)
}

# ============================================================================
# DIGITAL DISPLAY CONFIGURATION
# ============================================================================

DISPLAY_CONFIG <- list(
  # Display dimensions
  width = 4,
  height = 2,
  
  # Colors
  bg_color = "black",
  digit_color = "red",
  label_color = "white",
  warning_color = "yellow",
  danger_color = "red",
  safe_color = "green",
  
  # Warning thresholds
  max_safe_rpm = 3000,
  max_warning_rpm = 3500,
  
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
  # Determine display color based on RPM
  if (rpm > config$max_warning_rpm) {
    digit_color <- config$danger_color
  } else if (rpm > config$max_safe_rpm) {
    digit_color <- config$warning_color
  } else {
    digit_color <- config$safe_color
  }
  
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
  
  # Determine color
  if (current_rpm > config$max_warning_rpm) {
    bar_color <- config$danger_color
  } else if (current_rpm > config$max_safe_rpm) {
    bar_color <- config$warning_color
  } else {
    bar_color <- config$safe_color
  }
  
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

# Calculate RPMs for all pulleys
all_rpms <- calculate_all_rpms(motor_rpm, motor_pulley_diameter, spindle_pulley_diameters)
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
