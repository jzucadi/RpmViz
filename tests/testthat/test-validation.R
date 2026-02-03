# Tests for validation functions

test_that("validate_rpm accepts valid RPM values", {
 expect_true(validate_rpm(0))
  expect_true(validate_rpm(1000))
  expect_true(validate_rpm(3500))
})
test_that("validate_rpm rejects invalid RPM values", {
  expect_error(validate_rpm(NULL), "cannot be NULL")
  expect_error(validate_rpm(-100), "cannot be negative")
  expect_error(validate_rpm("1000"), "must be a single numeric")
  expect_error(validate_rpm(c(100, 200)), "must be a single numeric")
  expect_error(validate_rpm(NA), "must be a finite number")
  expect_error(validate_rpm(Inf), "must be a finite number")
})

test_that("validate_rpm warns for high RPM values", {
  expect_warning(validate_rpm(15000), "exceeds typical maximum")
})

test_that("validate_pulley_index accepts valid indices", {
  expect_true(validate_pulley_index(1))
  expect_true(validate_pulley_index(2))
  expect_true(validate_pulley_index(3))
  expect_true(validate_pulley_index(4))
})

test_that("validate_pulley_index rejects invalid indices", {
  expect_error(validate_pulley_index(NULL), "cannot be NULL")
  expect_error(validate_pulley_index(0), "must be between 1 and")
  expect_error(validate_pulley_index(5), "must be between 1 and")
  expect_error(validate_pulley_index(-1), "must be between 1 and")
  expect_error(validate_pulley_index("1"), "must be a single numeric")
})

test_that("validate_positive_number accepts valid values", {
  expect_true(validate_positive_number(1, "test"))
  expect_true(validate_positive_number(0.5, "test"))
  expect_true(validate_positive_number(1000, "test"))
})

test_that("validate_positive_number rejects invalid values", {
  expect_error(validate_positive_number(NULL, "test"), "cannot be NULL")
  expect_error(validate_positive_number(-1, "test"), "must be > 0")
  expect_error(validate_positive_number(0, "test"), "must be > 0")
  expect_error(validate_positive_number(0, "test", allow_zero = FALSE), "must be > 0")
})

test_that("validate_positive_number allows zero when specified", {
  expect_true(validate_positive_number(0, "test", allow_zero = TRUE))
})

test_that("validate_material accepts valid materials", {
  expect_true(validate_material("wood"))
  expect_true(validate_material("metal"))
  expect_true(validate_material("plastic"))
  expect_true(validate_material(NULL))  # NULL is allowed
})

test_that("validate_material rejects invalid materials", {
  expect_error(validate_material("glass"), "Unknown material")
  expect_error(validate_material(""), "Unknown material")
  expect_error(validate_material(123), "must be a single character")
})

test_that("validate_config_fields checks required fields", {
  config <- list(a = 1, b = 2, c = 3)
  expect_true(validate_config_fields(config, c("a", "b"), "test"))
  expect_error(validate_config_fields(config, c("a", "d"), "test"), "missing required fields")
  expect_error(validate_config_fields(NULL, c("a"), "test"), "cannot be NULL")
  expect_error(validate_config_fields("not a list", c("a"), "test"), "must be a list")
})
