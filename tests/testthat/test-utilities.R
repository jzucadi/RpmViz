# Tests for utility functions

test_that("get_rpm_color returns correct colors", {
  # Safe (green) - below max_safe_rpm (3000)
  expect_equal(get_rpm_color(1000), COLOR_SCHEME$safe_color)
  expect_equal(get_rpm_color(2999), COLOR_SCHEME$safe_color)

  # Warning (yellow) - between max_safe_rpm and max_warning_rpm
  expect_equal(get_rpm_color(3001), COLOR_SCHEME$warning_color)
  expect_equal(get_rpm_color(3499), COLOR_SCHEME$warning_color)

  # Danger (red) - above max_warning_rpm (3500)
  expect_equal(get_rpm_color(3501), COLOR_SCHEME$danger_color)
  expect_equal(get_rpm_color(5000), COLOR_SCHEME$danger_color)
})

test_that("get_rpm_color uses custom color scheme", {
  custom_scheme <- COLOR_SCHEME
  custom_scheme$max_safe_rpm <- 1000
  custom_scheme$safe_color <- "blue"

  expect_equal(get_rpm_color(500, custom_scheme), "blue")
  expect_equal(get_rpm_color(1500, custom_scheme), custom_scheme$warning_color)
})

test_that("color_with_alpha returns valid color", {
  result <- color_with_alpha("red", 0.5)
  expect_type(result, "character")
  expect_match(result, "^#")  # Should be hex color
})

test_that("color_with_alpha respects alpha parameter", {
  color_50 <- color_with_alpha("red", 0.5)
  color_100 <- color_with_alpha("red", 1.0)
  expect_false(color_50 == color_100)
})

test_that("format_speed_range produces correct format", {
  expect_equal(format_speed_range(100, 2000), "100-2000 RPM")
  expect_equal(format_speed_range(0, 500), "0-500 RPM")
  expect_equal(format_speed_range(100, 200, "Hz"), "100-200 Hz")
})

test_that("clamp restricts values to range", {
  expect_equal(clamp(0.5, 0, 1), 0.5)  # Within range
  expect_equal(clamp(-1, 0, 1), 0)     # Below min
  expect_equal(clamp(2, 0, 1), 1)      # Above max
  expect_equal(clamp(0, 0, 1), 0)      # At min
  expect_equal(clamp(1, 0, 1), 1)      # At max
})

test_that("clamp works with custom ranges", {
  expect_equal(clamp(50, 0, 100), 50)
  expect_equal(clamp(-10, 0, 100), 0)
  expect_equal(clamp(150, 0, 100), 100)
})

test_that("ranges_overlap detects overlapping ranges", {
  expect_true(ranges_overlap(c(0, 10), c(5, 15)))    # Partial overlap
  expect_true(ranges_overlap(c(0, 10), c(0, 10)))    # Exact match
  expect_true(ranges_overlap(c(0, 10), c(2, 8)))     # One inside other
  expect_true(ranges_overlap(c(2, 8), c(0, 10)))     # Other inside one
  expect_true(ranges_overlap(c(0, 10), c(10, 20)))   # Touch at edge
})

test_that("ranges_overlap detects non-overlapping ranges", {
  expect_false(ranges_overlap(c(0, 10), c(11, 20)))  # Gap between
  expect_false(ranges_overlap(c(20, 30), c(0, 10)))  # Reversed order
})

test_that("clip_range clips correctly", {
  expect_equal(clip_range(c(5, 15), c(0, 10)), c(5, 10))   # Clip end
  expect_equal(clip_range(c(-5, 5), c(0, 10)), c(0, 5))    # Clip start
  expect_equal(clip_range(c(-5, 15), c(0, 10)), c(0, 10))  # Clip both
  expect_equal(clip_range(c(2, 8), c(0, 10)), c(2, 8))     # No clipping needed
})

test_that("get_zone_color returns correct colors", {
  expect_equal(get_zone_color("wood"), COLOR_SCHEME$zone_colors$wood)
  expect_equal(get_zone_color("metal"), COLOR_SCHEME$zone_colors$metal)
  expect_equal(get_zone_color("plastic"), COLOR_SCHEME$zone_colors$plastic)
})

test_that("get_zone_color returns default for unknown material", {
  expect_equal(get_zone_color("unknown"), COLOR_SCHEME$zone_colors$wood)
})

test_that("degrees_to_radians converts correctly", {
  expect_equal(degrees_to_radians(0), 0)
  expect_equal(degrees_to_radians(180), pi)
  expect_equal(degrees_to_radians(360), 2 * pi)
  expect_equal(degrees_to_radians(90), pi / 2)
})

test_that("polar_to_cartesian converts correctly", {
  # 0 degrees should be on positive x-axis
  pos_0 <- polar_to_cartesian(0, 1)
  expect_equal(pos_0$x, 1, tolerance = 1e-10)
  expect_equal(pos_0$y, 0, tolerance = 1e-10)

  # 90 degrees should be on positive y-axis
  pos_90 <- polar_to_cartesian(90, 1)
  expect_equal(pos_90$x, 0, tolerance = 1e-10)
  expect_equal(pos_90$y, 1, tolerance = 1e-10)

  # 180 degrees should be on negative x-axis
  pos_180 <- polar_to_cartesian(180, 1)
  expect_equal(pos_180$x, -1, tolerance = 1e-10)
  expect_equal(pos_180$y, 0, tolerance = 1e-10)
})
