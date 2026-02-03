# Tests for calculation functions

test_that("calculate_rpm computes correct values", {
  # With 1:1 ratio and no slip, RPM should equal motor RPM
  expect_equal(calculate_rpm(1750, 2.0, 2.0, belt_slip_factor = 1.0), 1750)

  # With 2:1 ratio (motor smaller), spindle should be slower
  expect_equal(calculate_rpm(1750, 1.0, 2.0, belt_slip_factor = 1.0), 875)

  # With 1:2 ratio (motor larger), spindle should be faster
  expect_equal(calculate_rpm(1750, 4.0, 2.0, belt_slip_factor = 1.0), 3500)
})

test_that("calculate_rpm applies belt slip factor", {
  # 2% slip (0.98 factor)
  rpm_with_slip <- calculate_rpm(1750, 2.0, 2.0, belt_slip_factor = 0.98)
  rpm_no_slip <- calculate_rpm(1750, 2.0, 2.0, belt_slip_factor = 1.0)
  expect_lt(rpm_with_slip, rpm_no_slip)
  expect_equal(rpm_with_slip, round(1750 * 0.98))
})

test_that("calculate_rpm validates inputs", {
  expect_error(calculate_rpm(-100, 2.0, 2.0), "must be a positive number")
  expect_error(calculate_rpm(1750, -2.0, 2.0), "must be a positive number")
  expect_error(calculate_rpm(1750, 2.0, -2.0), "must be a positive number")
  expect_error(calculate_rpm(1750, 2.0, 2.0, belt_slip_factor = 1.5), "must be a number between 0 and 1")
  expect_error(calculate_rpm(1750, 2.0, 2.0, belt_slip_factor = 0), "must be a number between 0 and 1")
})

test_that("calculate_all_rpms returns named vector", {
  rpms <- calculate_all_rpms()
  expect_type(rpms, "integer")
  expect_length(rpms, 4)
  expect_true(all(grepl("Pulley_", names(rpms))))
})

test_that("calculate_all_rpms uses machine config", {
  custom_config <- MACHINE_CONFIG
  custom_config$motor_rpm <- 2000
  rpms_default <- calculate_all_rpms()
  rpms_custom <- calculate_all_rpms(custom_config)
  expect_true(all(rpms_custom > rpms_default))
})

test_that("generate_speed_sequence returns valid sequence", {
  speeds <- generate_speed_sequence(1)
  expect_type(speeds, "double")
  expect_true(length(speeds) > 0)
  expect_true(all(speeds > 0))
  expect_true(all(diff(speeds) > 0))  # Monotonically increasing
})

test_that("generate_speed_sequence respects num_steps", {
  speeds_5 <- generate_speed_sequence(1, num_steps = 5)
  speeds_20 <- generate_speed_sequence(1, num_steps = 20)
  expect_true(length(speeds_5) <= length(speeds_20))
})

test_that("generate_speed_sequence validates pulley_index", {
  expect_error(generate_speed_sequence(0), "must be between 1 and")
  expect_error(generate_speed_sequence(5), "must be between 1 and")
  expect_error(generate_speed_sequence(-1), "must be between 1 and")
})
