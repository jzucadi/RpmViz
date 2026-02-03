# Tests for configuration objects and factory functions

test_that("MACHINE_CONFIG has required fields", {
  required <- c("dial_diameter_in", "motor_rpm", "motor_pulley_diameter",
                "spindle_pulley_diameters", "belt_slip_factor")
  for (field in required) {
    expect_true(field %in% names(MACHINE_CONFIG),
                info = paste("Missing field:", field))
  }
})

test_that("COLOR_SCHEME has required fields", {
  required <- c("layer_colors", "safe_color", "warning_color", "danger_color",
                "max_safe_rpm", "max_warning_rpm", "zone_colors")
  for (field in required) {
    expect_true(field %in% names(COLOR_SCHEME),
                info = paste("Missing field:", field))
  }
})

test_that("MATERIAL_RANGES has valid structure", {
  expect_true(is.list(MATERIAL_RANGES))
  expect_true("wood" %in% names(MATERIAL_RANGES))
  expect_true("metal" %in% names(MATERIAL_RANGES))
  expect_true("plastic" %in% names(MATERIAL_RANGES))

  # Each range should be a 2-element vector
  for (name in names(MATERIAL_RANGES)) {
    expect_length(MATERIAL_RANGES[[name]], 2)
    expect_true(MATERIAL_RANGES[[name]][1] < MATERIAL_RANGES[[name]][2])
  }
})

test_that("create_machine_config creates valid config", {
  config <- create_machine_config()
  expect_type(config, "list")
  expect_equal(config$motor_rpm, MACHINE_CONFIG$motor_rpm)
})

test_that("create_machine_config applies overrides", {
  config <- create_machine_config(motor_rpm = 2000, motor_hp = 2.0)
  expect_equal(config$motor_rpm, 2000)
  expect_equal(config$motor_hp, 2.0)
  # Other fields should be unchanged
  expect_equal(config$belt_slip_factor, MACHINE_CONFIG$belt_slip_factor)
})

test_that("create_machine_config warns on unknown parameters", {
  expect_warning(create_machine_config(unknown_param = 123), "Unknown config parameter")
})

test_that("create_color_scheme creates valid scheme", {
  scheme <- create_color_scheme()
  expect_type(scheme, "list")
  expect_equal(scheme$safe_color, COLOR_SCHEME$safe_color)
})

test_that("create_color_scheme applies overrides", {
  scheme <- create_color_scheme(safe_color = "blue", max_safe_rpm = 2000)
  expect_equal(scheme$safe_color, "blue")
  expect_equal(scheme$max_safe_rpm, 2000)
})

test_that("merge_config merges correctly", {
  base <- list(a = 1, b = 2, c = 3)
  overrides <- list(b = 20, d = 4)
  result <- merge_config(base, overrides)

  expect_equal(result$a, 1)   # Unchanged
  expect_equal(result$b, 20)  # Overridden
  expect_equal(result$c, 3)   # Unchanged
  expect_equal(result$d, 4)   # Added
})

test_that("merge_config handles nested lists", {
  base <- list(outer = list(inner1 = 1, inner2 = 2))
  overrides <- list(outer = list(inner2 = 20, inner3 = 3))
  result <- merge_config(base, overrides)

  expect_equal(result$outer$inner1, 1)   # Unchanged
  expect_equal(result$outer$inner2, 20)  # Overridden
  expect_equal(result$outer$inner3, 3)   # Added
})

test_that("copy_config creates independent copy", {
  original <- list(a = 1, nested = list(b = 2))
  copied <- copy_config(original)

  # Should be equal
  expect_equal(copied$a, original$a)
  expect_equal(copied$nested$b, original$nested$b)

  # Modifying copy shouldn't affect original (R handles this automatically,
  # but copy_config makes it explicit)
  copied$a <- 100
  expect_equal(original$a, 1)
})

test_that("create_speed_layer creates valid layer", {
  layer <- create_speed_layer(1, c(100, 200, 300), "blue", 1.0, 2.5)

  expect_equal(layer$name, "speeds1")
  expect_equal(layer$data, c(100, 200, 300))
  expect_equal(layer$border_color, "blue")
  expect_equal(layer$canvas_lim, 1.0)
  expect_equal(layer$pulley_size, 2.5)
  expect_match(layer$label, "Pulley 2.5")
})

test_that("create_speed_layer validates inputs", {
  expect_error(create_speed_layer(0, c(100), "blue", 1.0, 2.5), "must be a positive integer")
  expect_error(create_speed_layer(1, c(), "blue", 1.0, 2.5), "at least one element")
  expect_error(create_speed_layer(1, c(100), 123, 1.0, 2.5), "must be a single character")
  expect_error(create_speed_layer(1, c(100), "blue", -1.0, 2.5), "must be a positive number")
  expect_error(create_speed_layer(1, c(100), "blue", 1.0, -2.5), "must be a positive number")
})
