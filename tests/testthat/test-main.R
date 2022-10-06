test_that('changing the value of d changes the rational integer', {
  # the tritone and the minor seventh should change when switching
  # from rational tuning #1 to rational tuning #2
  # rational tuning #1
  expect_equal(get_rational_interval(6,d=0.010),c(17,12))
  expect_equal(get_rational_interval(10,d=0.010),c(16,9))
  # rational tuning #2
  expect_equal(get_rational_interval(6,d=0.011),c(7,5))
  expect_equal(get_rational_interval(10,d=0.011),c(9,5))
})
