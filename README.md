# rpmviz

An R package for creating industrial-style RPM visualizations using the circlize package. Provides both analog dial and digital display representations for spindle speed monitoring.

[![R Package CI](https://github.com/jzucadi/RpmViz/actions/workflows/r.yml/badge.svg)](https://github.com/jzucadi/RpmViz/actions/workflows/r.yml)

<img width="377" height="355" alt="Screenshot 2025-12-17 at 4 38 49 PM" src="https://github.com/user-attachments/assets/3401ea57-bd63-4f3a-bb96-2be14b496160" /><img width="377" height="355" alt="Screenshot 2025-12-17 at 4 35 47 PM" src="https://github.com/user-attachments/assets/7c750f3f-144f-484b-8dfb-96762190582e" />

## Features

- **Circular Speed Dial**: Multi-layered dial visualization showing speeds across pulley configurations
- **Digital 7-Segment Display**: Classic digital readout style RPM display
- **Color-Coded Warnings**: Green (safe), Yellow (warning), Red (danger) based on RPM thresholds
- **Material Speed Zones**: Optimal RPM ranges for wood, metal, and plastic
- **RPM Calculations**: Automatic spindle RPM calculation from motor speed and pulley ratios
- **Customizable Configuration**: Factory functions for creating custom machine configs
- **Comprehensive Validation**: Input validation for all parameters

## Installation

```r
# Install from GitHub
devtools::install_github("jzucadi/RpmViz")

# Or install from local source
devtools::install_local("path/to/RpmViz")
```

## Requirements

- R (>= 3.6.0)
- circlize package

## Quick Start

```r
library(rpmviz)

# Render the default speed dial
render_dial(CONFIG)

# Render with a specific RPM indicator
dial_config <- update_config(CONFIG, current_speed = 1400, active_layer = 2)
render_dial(dial_config)

# Digital display
render_digital_display(1500, active_pulley = 2)

# Full dashboard with dial and bar
render_rpm_dashboard(1800, active_pulley = 3)
```

## API Reference

### Configuration Objects

| Object | Description |
|--------|-------------|
| `MACHINE_CONFIG` | Machine specifications (motor RPM, pulley diameters, belt slip factor) |
| `COLOR_SCHEME` | Colors for layers, safety thresholds, and material zones |
| `MATERIAL_RANGES` | Optimal RPM ranges by material (wood, metal, plastic) |
| `DIAL_PARAMS` | Dial display parameters (canvas limits, label positions) |
| `BAR_PARAMS` | Bar graph display parameters |
| `INDICATOR_PARAMS` | Arrow indicator settings |
| `SEGMENT_PARAMS` | 7-segment digital display settings |
| `LAYOUT_PARAMS` | Dashboard layout parameters |

### Core Calculation Functions

| Function | Description |
|----------|-------------|
| `calculate_rpm(motor_rpm, motor_dia, spindle_dia, belt_slip_factor)` | Calculate spindle RPM from pulley ratios |
| `calculate_all_rpms(machine_config)` | Get RPMs for all pulley configurations |
| `generate_speed_sequence(pulley_index, num_steps)` | Generate speed sequence for a pulley |

### Visualization Functions

| Function | Description |
|----------|-------------|
| `render_dial(config)` | Render circular speed dial |
| `render_digital_display(rpm, active_pulley)` | Render 7-segment digital display |
| `render_rpm_bar(rpm)` | Render vertical RPM bar graph |
| `render_rpm_dashboard(rpm, active_pulley)` | Combined dashboard view |
| `update_and_render(config, ...)` | Update config and render in one call |

### Configuration Factory Functions

| Function | Description |
|----------|-------------|
| `create_machine_config(...)` | Create custom machine configuration |
| `create_color_scheme(...)` | Create custom color scheme |
| `create_speed_layer(index, speeds, color, canvas_lim, pulley_dia)` | Create a speed layer for the dial |
| `update_config(config, ...)` | Update specific config values |
| `merge_config(base, updates)` | Merge two configurations |
| `copy_config(config)` | Deep copy a configuration |

### Utility Functions

| Function | Description |
|----------|-------------|
| `get_rpm_color(rpm)` | Get color based on RPM (green/yellow/red) |
| `validate_rpm(rpm)` | Validate RPM value |
| `validate_pulley_index(index)` | Validate pulley index (1-4) |
| `validate_material(material)` | Validate material name |
| `color_with_alpha(color, alpha)` | Add transparency to a color |
| `format_speed_range(range)` | Format speed range for display |
| `clamp(value, min, max)` | Clamp value to range |

## Examples

### Custom Machine Configuration

```r
# Create config for a different machine
my_machine <- create_machine_config(
  motor_rpm = 1800,
  motor_hp = 2.0,
  motor_pulley_diameter = 2.5,
  spindle_pulley_diameters = c(3.0, 4.0, 5.0, 6.0)
)

# Calculate RPMs with custom config
rpms <- calculate_all_rpms(my_machine)
print(rpms)
```

### Material Zone Highlighting

```r
# Show optimal zone for metal work
metal_config <- update_config(CONFIG,
  current_speed = 600,
  active_layer = 1,
  current_material = "metal"
)
render_dial(metal_config)
```

### Generating Graphics Files

```r
# Save dial to PNG
png("speed_dial.png", width = 800, height = 800, res = 120)
render_dial(CONFIG)
dev.off()

# Save dashboard
png("dashboard.png", width = 1000, height = 400, res = 120)
render_rpm_dashboard(1800, active_pulley = 3)
dev.off()
```

## Use Case: Powermatic 1200 Drill Press

The default configuration is based on the Powermatic 1200 drill press with a variable speed mechanism. The dial helps you:

1. **Quickly reference available speeds** for different pulley positions
2. **Optimize drilling operations** by selecting the correct RPM for your material and bit size
3. **Create a professional speed chart** that can be mounted directly on your drill press

## Development

### Running Tests

```r
# Run all tests
devtools::test()

# Run with coverage
covr::package_coverage()
```

### Project Structure

```
rpmviz/
├── R/
│   ├── aaa_config.r    # Configuration objects and factory functions
│   ├── circlize.r      # Circular dial visualization
│   └── rpm_display.r   # Digital display and bar graph
├── tests/
│   └── testthat/       # Unit tests (157+ tests)
├── scripts/
│   └── generate_graphics.R  # Generate sample images
├── DESCRIPTION
├── NAMESPACE
└── LICENSE
```

### CI/CD

The package includes GitHub Actions workflows for:
- R CMD check on multiple platforms (Ubuntu, macOS, Windows)
- Multiple R versions (release, devel, oldrel)
- Unit testing with testthat
- Code coverage reporting
- Linting with lintr
- Automatic graphics generation

## License

This project is licensed under the [MIT License](LICENSE).

## Contributing

Contributions welcome! Feel free to submit issues or pull requests.
