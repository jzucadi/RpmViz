# ============================================================================
# DIGITAL RPM DISPLAY MODULE
# Complementary to circlize.r speed dial - displays actual RPM values
# ============================================================================

# Note: circlize is imported via NAMESPACE, aaa_config.r is loaded automatically by R

# ============================================================================
# LOCAL REFERENCES (from shared config for backward compatibility)
# ============================================================================

spindle_pulley_diameters <- MACHINE_CONFIG$spindle_pulley_diameters
motor_pulley_diameter <- MACHINE_CONFIG$motor_pulley_diameter
motor_rpm <- MACHINE_CONFIG$motor_rpm

# Note: calculate_rpm() and calculate_all_rpms() are now provided by R/aaa_config.r

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

# 7-segment digit definitions (which segments are on for each digit)
# Segments: top, top-right, bottom-right, bottom, bottom-left, top-left, middle
SEGMENT_PATTERNS <- list(
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

# Segment positions (relative to digit center)
SEGMENT_POSITIONS <- list(
  list(x = c(-0.3, 0.3), y = c(0.5, 0.5)),      # top
  list(x = c(0.35, 0.35), y = c(0.25, 0.45)),   # top-right
  list(x = c(0.35, 0.35), y = c(-0.45, -0.05)), # bottom-right
  list(x = c(-0.3, 0.3), y = c(-0.5, -0.5)),    # bottom
  list(x = c(-0.35, -0.35), y = c(-0.45, -0.05)), # bottom-left
  list(x = c(-0.35, -0.35), y = c(0.05, 0.45)), # top-left
  list(x = c(-0.3, 0.3), y = c(0, 0))           # middle
)

#' Render a 7-segment style digit
#' @param digit Single digit (0-9) or '-'
#' @param x X position
#' @param y Y position
#' @param size Digit size
#' @param color Digit color
#' @param segment_params Segment parameters (defaults to SEGMENT_PARAMS)
draw_segment_digit <- function(digit, x, y, size = 1, color = "red",
                               segment_params = SEGMENT_PARAMS) {
  # Validate inputs
  if (is.null(digit)) {
    warning("digit cannot be NULL, skipping")
    return(invisible(NULL))
  }
  if (!is.numeric(x) || length(x) != 1) {
    stop("x must be a single numeric value")
  }
  if (!is.numeric(y) || length(y) != 1) {
    stop("y must be a single numeric value")
  }
  if (!is.numeric(size) || length(size) != 1 || size <= 0) {
    stop("size must be a positive number")
  }

  digit_char <- as.character(digit)
  if (!(digit_char %in% names(SEGMENT_PATTERNS))) {
    warning(paste("Invalid digit:", digit_char,
                  "- valid characters: 0-9 and '-'. Skipping."))
    return(invisible(NULL))
  }

  active_segs <- SEGMENT_PATTERNS[[digit_char]]

  for (i in seq_along(active_segs)) {
    if (active_segs[i] == 1) {
      lines(
        x + SEGMENT_POSITIONS[[i]]$x * size,
        y + SEGMENT_POSITIONS[[i]]$y * size,
        col = color,
        lwd = segment_params$segment_lwd
      )
    }
  }
}

#' Render complete RPM value as 7-segment display
#' @param rpm RPM value to display
#' @param config Display configuration
#' @param segment_params Segment display parameters (defaults to SEGMENT_PARAMS)
draw_rpm_value <- function(rpm, config = DISPLAY_CONFIG, segment_params = SEGMENT_PARAMS) {
  # Validate inputs
  validate_rpm(rpm)
  validate_config_fields(config, c("label_color", "unit_cex", "max_safe_rpm", "max_warning_rpm"),
                         "display config")

  # Determine display color based on RPM (using shared helper)
  digit_color <- get_rpm_color(rpm, config)

  # Format RPM as 4-digit string
  rpm_str <- sprintf("%4d", rpm)

  # Draw each digit (using configurable positions and size)
  digits <- strsplit(rpm_str, "")[[1]]

  for (i in seq_along(digits)) {
    if (digits[i] != " ") {
      draw_segment_digit(
        digits[i],
        segment_params$digit_positions[i],
        0,
        size = segment_params$digit_size,
        color = digit_color,
        segment_params = segment_params
      )
    }
  }

  # Draw "RPM" unit label (using configurable position)
  text(
    segment_params$rpm_label_x,
    segment_params$rpm_label_y,
    "RPM",
    col = config$label_color,
    cex = config$unit_cex,
    font = 2
  )
}

#' Create the complete digital display
#' @param current_rpm Current RPM to display
#' @param active_pulley Active pulley index (1-4)
#' @param config Display configuration
#' @param segment_params Segment display parameters (defaults to SEGMENT_PARAMS)
#' @param layout_params Layout parameters (defaults to LAYOUT_PARAMS)
#' @export
render_digital_display <- function(current_rpm, active_pulley = 1,
                                   config = DISPLAY_CONFIG,
                                   segment_params = SEGMENT_PARAMS,
                                   layout_params = LAYOUT_PARAMS) {
  # Validate inputs
  validate_rpm(current_rpm)
  validate_pulley_index(active_pulley)
  validate_config_fields(config, c("bg_color", "width", "height", "label_color"),
                         "display config")
  validate_config_fields(segment_params, c("border_margin", "border_color", "border_lwd",
                                           "pulley_label_y", "pulley_label_cex"),
                         "segment params")

  # Set up plot (using configurable margins)
  par(bg = config$bg_color, mar = layout_params$digital_margins)
  plot(
    NULL,
    xlim = c(-config$width/2, config$width/2),
    ylim = c(-config$height/2, config$height/2),
    xlab = "", ylab = "", axes = FALSE,
    main = ""
  )

  # Draw display border (using configurable parameters)
  margin <- segment_params$border_margin
  rect(
    -config$width/2 + margin, -config$height/2 + margin,
    config$width/2 - margin, config$height/2 - margin,
    border = segment_params$border_color,
    lwd = segment_params$border_lwd
  )

  # Draw RPM value
  draw_rpm_value(current_rpm, config, segment_params)

  # Draw pulley indicator at bottom (using configurable position)
  pulley_text <- paste0("Pulley ", active_pulley, " (",
                        spindle_pulley_diameters[active_pulley], '")')
  text(
    0, segment_params$pulley_label_y,
    pulley_text,
    col = config$label_color,
    cex = segment_params$pulley_label_cex
  )

  # Draw title (using configurable font)
  title(main = "SPINDLE SPEED", col.main = config$label_color,
        font.main = layout_params$title_font)
}

#' Display RPM bar graph showing current position in range
#' @param current_rpm Current RPM value
#' @param min_rpm Minimum RPM for scale (defaults to BAR_PARAMS$default_min_rpm)
#' @param max_rpm Maximum RPM for scale (defaults to BAR_PARAMS$default_max_rpm)
#' @param config Display configuration
#' @param bar_params Bar chart parameters (defaults to BAR_PARAMS)
#' @export
render_rpm_bar <- function(current_rpm,
                           min_rpm = BAR_PARAMS$default_min_rpm,
                           max_rpm = BAR_PARAMS$default_max_rpm,
                           config = DISPLAY_CONFIG,
                           bar_params = BAR_PARAMS) {
  # Validate inputs
  validate_rpm(current_rpm)
  validate_positive_number(min_rpm, "min_rpm", allow_zero = TRUE)
  validate_positive_number(max_rpm, "max_rpm")
  if (min_rpm >= max_rpm) {
    stop(paste("min_rpm must be less than max_rpm - got min:", min_rpm, "max:", max_rpm))
  }
  validate_config_fields(config, c("bg_color", "label_color"), "display config")

  par(bg = config$bg_color, mar = bar_params$margins)

  # Calculate percentage (using clamp utility)
  pct <- (current_rpm - min_rpm) / (max_rpm - min_rpm)
  pct <- clamp(pct, 0, 1)

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

  # Add scale (using configurable axis settings)
  axis(2, at = bar_params$axis_ticks,
       labels = round(seq(min_rpm, max_rpm, length.out = bar_params$num_axis_labels)),
       col = config$label_color, col.axis = config$label_color)

  # Add current value label (using configurable position and styling)
  text(bar_params$value_label_x,
       pct * 100 + bar_params$value_label_y_offset,
       paste0(current_rpm, " RPM"),
       col = config$label_color,
       cex = bar_params$value_label_cex,
       font = bar_params$value_label_font)

  title(main = "RPM Level", col.main = config$label_color)
}

#' Create combined dashboard with dial reference and digital display
#' @param current_rpm Current RPM to display
#' @param active_pulley Active pulley (1-4)
#' @param layout_params Layout parameters (defaults to LAYOUT_PARAMS)
#' @export
render_rpm_dashboard <- function(current_rpm, active_pulley = 1,
                                 layout_params = LAYOUT_PARAMS) {
  # Validate inputs (detailed validation happens in sub-functions)
  validate_rpm(current_rpm)
  validate_pulley_index(active_pulley)

  # Set up multi-panel layout (using configurable dimensions)
  par(mfrow = c(layout_params$dashboard_rows, layout_params$dashboard_cols))

  # Panel 1: Digital display
  render_digital_display(current_rpm, active_pulley)

  # Panel 2: Bar indicator
  render_rpm_bar(current_rpm)

  # Reset layout
  par(mfrow = c(1, 1))
}

# ============================================================================
# EXECUTE (only when running interactively)
# ============================================================================

if (interactive()) {
  # Calculate RPMs for all pulleys (using shared function with MACHINE_CONFIG)
  all_rpms <- calculate_all_rpms()
  print("Available RPMs by pulley:")
  print(all_rpms)

  # Example: Display current RPM
  current_rpm <- 1200
  active_pulley <- 2

  # Render the digital display
  render_digital_display(current_rpm, active_pulley)
}

# ============================================================================
# USAGE EXAMPLES
# ============================================================================
#
# BASIC USAGE:
# ------------
# # Show current speed on digital display
# render_digital_display(1500, active_pulley = 1)
#
# # Show RPM bar graph
# render_rpm_bar(2200)
#
# # Full dashboard view (digital + bar)
# render_rpm_dashboard(1800, active_pulley = 3)
#
# CALCULATING RPM:
# ----------------
# # Calculate RPM for a specific pulley configuration
# my_rpm <- calculate_rpm(
#   motor_rpm = 1750,
#   motor_pulley_dia = 2.0,
#   spindle_pulley_dia = 3.5
# )
# render_digital_display(my_rpm)
#
# # Get all available RPMs
# all_rpms <- calculate_all_rpms()
# print(all_rpms)
#
# CUSTOM DISPLAY CONFIG:
# ----------------------
# # Create custom display settings
# my_display_config <- DISPLAY_CONFIG
# my_display_config$max_safe_rpm <- 2500
# my_display_config$safe_color <- "cyan"
# render_digital_display(1800, config = my_display_config)
