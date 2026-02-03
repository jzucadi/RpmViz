# =============================================================================
# Generate RPM Visualization Graphics
# This script generates sample graphics for the repository
# =============================================================================

# Load required packages
library(circlize)

# Source the package files (all now in R/ directory)
# aaa_config.r loads first alphabetically to define config objects
source("R/aaa_config.r")
source("R/circlize.r")
source("R/rpm_display.r")

# Create output directory if it doesn't exist
if (!dir.exists("images")) {
  dir.create("images")
}

# =============================================================================
# 1. Generate Circular Speed Dial
# =============================================================================
cat("Generating circular speed dial...\n")

png("images/speed_dial.png", width = 800, height = 800, res = 120)
render_dial(CONFIG)
dev.off()

cat("  Saved: images/speed_dial.png\n")

# =============================================================================
# 2. Generate Speed Dial with RPM Indicator
# =============================================================================
cat("Generating speed dial with RPM indicator...\n")

png("images/speed_dial_with_indicator.png", width = 800, height = 800, res = 120)
dial_config <- update_config(CONFIG, current_speed = 1400, active_layer = 2)
render_dial(dial_config)
dev.off()

cat("  Saved: images/speed_dial_with_indicator.png\n")

# =============================================================================
# 3. Generate Speed Dial with Metal Zone
# =============================================================================
cat("Generating speed dial with metal zone...\n")

png("images/speed_dial_metal_zone.png", width = 800, height = 800, res = 120)
metal_config <- update_config(CONFIG, current_speed = 600, active_layer = 1, current_material = "metal")
render_dial(metal_config)
dev.off()

cat("  Saved: images/speed_dial_metal_zone.png\n")

# =============================================================================
# 4. Generate Digital Display
# =============================================================================
cat("Generating digital RPM display...\n")

png("images/digital_display.png", width = 600, height = 300, res = 120)
render_digital_display(1500, active_pulley = 2)
dev.off()

cat("  Saved: images/digital_display.png\n")

# =============================================================================
# 5. Generate RPM Bar Chart
# =============================================================================
cat("Generating RPM bar chart...\n")

png("images/rpm_bar.png", width = 400, height = 500, res = 120)
render_rpm_bar(2200)
dev.off()

cat("  Saved: images/rpm_bar.png\n")

# =============================================================================
# 6. Generate Full Dashboard
# =============================================================================
cat("Generating full RPM dashboard...\n")

png("images/rpm_dashboard.png", width = 1000, height = 400, res = 120)
render_rpm_dashboard(1800, active_pulley = 3)
dev.off()

cat("  Saved: images/rpm_dashboard.png\n")

# =============================================================================
# 7. Generate High RPM Warning Display
# =============================================================================
cat("Generating high RPM warning display...\n")

png("images/high_rpm_warning.png", width = 600, height = 300, res = 120)
render_digital_display(3300, active_pulley = 4)
dev.off()

cat("  Saved: images/high_rpm_warning.png\n")

# =============================================================================
# Summary
# =============================================================================
cat("\n")
cat("===========================================\n")
cat("Graphics generation complete!\n")
cat("===========================================\n")
cat("Generated files:\n")
list.files("images", pattern = "\\.png$", full.names = TRUE)
