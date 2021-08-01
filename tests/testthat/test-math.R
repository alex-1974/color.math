test_that("deg2rad", {
  expect_equal(deg2rad(0),( 0 / 180) * pi)
  expect_equal(deg2rad(1),( 1 / 180) * pi)
  expect_equal(deg2rad(180),( 180 / 180) * pi)
  expect_equal(deg2rad(360),( 360 / 180) * pi)
  expect_equal(deg2rad(1.5),( 1.5 / 180) * pi)
})

test_that("rad2deg", {
  expect_equal(rad2deg(1),( 1 * 180) / pi)
  expect_equal(rad2deg(pi),( pi * 180) / pi)
})
