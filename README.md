# Circlize Package Prototype

A collection of R scripts for creating industrial-style RPM visualizations using the circlize package.  This project provides both analog dial and digital display representations for spindle speed monitoring.

| Feature | Description |
|<img width="1344" height="960" alt="download" src="https://github.com/user-attachments/assets/e584f291-62ab-4744-9fb7-8192befc21e8" />|<img width="754" height="735" alt="Screenshot 2025-12-17 at 4 35 47 PM" src="https://github.com/user-attachments/assets/7c750f3f-144f-484b-8dfb-96762190582e" />
|




## Overview

### circlize.r - Circular Speed Dial Visualization

Creates a circular "speed dial" style chart using the circlize package, ideal for visualizing where the current RPM falls within the available speed range. 

### rpm_display.r - Digital RPM Display

A complementary script that displays actual RPM values in a digital format.  While the circular dial shows where you are on the speed range, the digital display shows the exact numerical RPM value.

#### Features

| Feature | Description |
|---------|-------------|
| 7-Segment Digital Display | Renders RPM values in a classic digital readout style |
| Color-Coded Warnings | Green (safe), Yellow (warning), Red (danger) based on RPM thresholds |
| RPM Calculation | Automatically calculates actual spindle RPM from pulley ratios |
| Bar Graph Indicator | Visual bar showing where current RPM falls in the available range |
| Dashboard Mode | Combined view with both digital readout and bar indicator |

#### Key Functions

- `calculate_rpm()` - Computes actual spindle RPM based on motor speed and pulley diameters
- `render_digital_display()` - Shows a 7-segment style digital RPM readout
- `render_rpm_bar()` - Displays a vertical bar graph of the current RPM level
- `render_rpm_dashboard()` - Combined view with both displays

## Integration

Both scripts share the same machine specifications (pulley diameters, motor RPM) and are designed to work together. Use the circular dial for an at-a-glance view of the speed range position, and the digital display for precise numerical readings.

This R script creates a circular dial visualization that displays available spindle speeds (RPMs) across multiple pulley configurations. The chart mimics the appearance of traditional drill press speed charts, with concentric rings representing different belt-pulley combinations.

Perfect for creating custom speed reference charts for your drill press control panel.

## Features

- **Multi-layered circular design**: Each concentric ring represents speeds achievable with a specific spindle pulley diameter
- **Color-coded tracks**: Different colors (light blue, yellow, red, purple) distinguish between pulley configurations
- **Outward-facing labels**: RPM values are oriented for easy reading around the dial
- **Center reference marks**: Visual guides for dial center and mounting hole placement
- **Customizable dimensions**: Adjust dial size, font size, and track heights to match your specific drill press

## How It Works

The script draws a dial with multiple concentric tracks, each representing a set of speeds (RPMs) achievable with different pulley combinations on your drill press. When you adjust the variable speed handle on a machine like the Powermatic 1200, you're changing the effective pulley ratio - this chart shows you what RPM you can expect at each setting.

### Key Parameters

```r
font_size <- 15 #pt
scale_height <- (dial_outer_edge_dia_in - dial_diameter_in) / length(spindle_pulley_diameters)
gap_after = stop_angle - start_angle
cell_padding <- c(0.00, 1.00, 0.00, 1.00)
```

- **font_size**: Sets text size for speed labels
- **scale_height**: Calculates the height of the circular tracks based on dial dimensions and the number of pulleys
- **gap_after**: Determines the angular gap between tracks
- **cell_padding**: Sets padding between circular cells (tracks)

## Visualization Structure

### Outermost Track (Highest Speeds)

```r
circos.par(clock.wise = TRUE, start.degree = start_angle, gap.after = gap_after, cell.padding = cell_padding)
circos.initialize(factors = "speeds1", xlim = c(min(speeds1), max(speeds1)))
circos.track(
  ylim = c(0, 1),
  bg.border = "light blue",
  track.height = convert_length(scale_height, "in")
)
circos.text(speeds1, rep(0.5, length(speeds1)), speeds1, facing = "outside", niceFacing = TRUE, cex = fontsize(font_size), font = 2, family = "serif")
circos.clear()
```

Initializes the circular plot with the outermost speed layer (speeds1), draws a track with a blue border, and places speed values outward around the ring.

### Center Mark and Boundaries

```r
lines(x = c(-0.1, 0.1), y = c(0, 0), fg = "lightgray")
lines(x = c(0, 0), y = c(-0.1, 0.1), fg = "lightgray")
symbols(c(0, 0), c(0, 0), circles = c(dial_diameter_in / 2, mounting_hole_dia_in / 2), fg = "lightgray", add = TRUE)
```

Draws faint cross lines to mark the dial center and circles for the dial edge and mounting hole, helping visually anchor the dial.

### Additional Tracks (Lower Speeds)

For each subsequent speed layer (speeds2, speeds3, speeds4), the code:
- Uses `par(new = TRUE)` to overlay new tracks on the same plot
- Expands the canvas limits (xy_canvas_lim) to fit the next ring
- Initializes and draws colored tracks using `circos.track`
- Places the speed values around each ring

**Example for Second Layer:**

```r
xy_canvas_lim <- 1.16
circos.par(canvas.xlim = c(-xy_canvas_lim, xy_canvas_lim), ...)
circos.initialize(factors = "speeds2", xlim = c(min(speeds2), max(speeds2)))
circos.track(
  ylim = c(0, 1),
  bg.border = "yellow",
  ...,
  panel.fun = function(x, y) {
    circos.text(speeds2, rep(0.5, length(speeds2)), speeds2, facing = "outside", ...)
  }
)
circos.clear()
```

Yellow, red, and purple borders are used for layers 2–4, respectively.

## Use Case: Powermatic 1200 Drill Press

The Powermatic 1200 features a variable speed handle that allows the operator to change spindle speeds without stopping the machine. This chart helps you:

1. **Quickly reference available speeds** for different pulley positions
2. **Optimize drilling operations** by selecting the correct RPM for your material and bit size
3. **Create a professional speed chart** that can be mounted directly on your drill press

Simply adjust your pulley belt position and use the variable speed handle to dial in the exact RPM shown on the corresponding ring.

## Requirements

- R (version 3.6+)
- `circlize` package

## Installation

```r
install.packages("circlize")
```

## Usage

```r
source("circlize. r")
source("rpm_display. r")
```

## License

This project is licensed under the [MIT License](LICENSE) - it's free and open source software.  You are free to use, modify, and distribute this project for any purpose, including commercial use. 

## Contributing

Contributions welcome! Feel free to submit issues or pull requests.
