test_that("dec2hex", {
  expect_equal(dec2hex(0), "00")
  expect_equal(dec2hex(255), "ff")
  expect_equal(dec2hex(255, n = 4), "00ff")
  expect_equal(dec2hex(256), "0100")
})

