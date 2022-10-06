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
test_that('inverting the intervals is symmetrical',{
  tonic_ref = 69 # A above middle C in MIDI
  intervals = 0:12 + tonic_ref
  # tonic reference
  tonic_dissonance = intervals %>% purrr::map_dbl(
    ~smooth_log_periodicity(c(tonic_ref,.x), d=0.0102))

  #octave reference
  octave_ref = tonic_ref + 12 # A an octave above middle C in MIDI
  octave_dissonance = intervals %>% purrr::map_dbl(
    ~smooth_log_periodicity(c(.x,octave_ref), d=0.0102))

  # expect symmetry between tonic and octave
  expect_equal(tonic_dissonance,rev(octave_dissonance))
})
