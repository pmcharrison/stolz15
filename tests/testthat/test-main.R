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
test_that('direction up +1 (tonic perspective ) and down -1 (octave perspective) ratios make sense',{
  expect_equal(get_rational_interval(7, d=0.0102,direction=+1),c(3,2))
  expect_equal(get_rational_interval(7, d=0.0102,direction=-1),c(3,4))
  expect_equal(get_rational_interval(7, d=0.0102,direction=-1),
               rev(get_rational_interval(5, d=0.0102,direction=+1)))

  expect_equal(get_rational_interval(0, d=0.0102,direction=+1),c(1,1))
  expect_equal(get_rational_interval(12, d=0.0102,direction=+1),c(2,1))
  expect_equal(get_rational_interval(0, d=0.0102,direction=-1),c(1,2))
  expect_equal(get_rational_interval(12, d=0.0102,direction=-1),c(1,1))

  expect_equal(rationalise_chord(0, d=0.0102, direction=+1),matrix(c(1,1),nrow=2))
  expect_equal(rationalise_chord(12, d=0.0102, direction=+1),matrix(c(2,1),nrow=2))
  expect_equal(rationalise_chord(0, d=0.0102, direction=-1),matrix(c(1,2),nrow=2))
  expect_equal(rationalise_chord(12, d=0.0102, direction=-1),matrix(c(1,1),nrow=2))

  expect_equal(rationalise_chord(69, d=0.0102, direction=+1),matrix(c(160,3),nrow=2))

  expect_equal(rationalise_chord(c(0,4,7), d=0.0102, direction=+1),
               matrix(c(1,1,5,4,3,2),nrow=2))
  expect_equal(rationalise_chord(c(0,4,7), d=0.0102, direction=-1),
               matrix(c(1,2,5,8,3,4),nrow=2))
})

test_that('pitches work up and down',{
  expect_equal(smooth_log_periodicity(c(69,69+12), d=0.0102, direction=+1),0)
  expect_equal(smooth_log_periodicity(c(69,69+12), d=0.0102, direction=-1),0)
})

test_that('triads land in an interesting way on brightness affinity',{
  tonic_dissonance = smooth_log_periodicity(69+c(0,4,7), d=0.0102)
  octave_dissonance = smooth_log_periodicity(69+c(4,7,12), d=0.0102)
  expect_gt(octave_dissonance,tonic_dissonance)

  tonic_dissonance = smooth_log_periodicity(69+c(0,5,8), d=0.0102)
  octave_dissonance = smooth_log_periodicity(69+c(5,8,12), d=0.0102)
  expect_gt(tonic_dissonance,octave_dissonance)

  tonic_dissonance = smooth_log_periodicity(69+c(0,4,7), d=0.0102, direction=+1)
  octave_dissonance = smooth_log_periodicity(69+c(0,4,7), d=0.0102, direction=-1)
  # major means octave > tonic
  expect_gt(octave_dissonance,tonic_dissonance)

  tonic_dissonance = smooth_log_periodicity(69+c(0,3,7), d=0.0102, direction=+1)
  octave_dissonance = smooth_log_periodicity(69+c(0,3,7), d=0.0102, direction=-1)
  # minor means tonic > octave
  expect_gt(tonic_dissonance,octave_dissonance)
})
