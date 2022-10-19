test_that('we can explain stolz in tonic octave space',{
  tonic  = dplyr::bind_rows(0:12 %>% purrr::map(~stolzenburg_interval(c(0 ,.x))))
  octave = dplyr::bind_rows(0:12 %>% purrr::map(~stolzenburg_interval(c(.x,12))))

  major_from_octave=octave$smooth_log_periodicity[c(2,4,7,9,11)+1]
  minor_from_octave=octave$smooth_log_periodicity[c(1,3,5,8,10)+1]
  expect_true((major_from_octave > minor_from_octave) %>% all)

  major_from_tonic=tonic$smooth_log_periodicity[c(2,4,7,9,11)+1]
  minor_from_tonic=tonic$smooth_log_periodicity[c(1,3,5,8,10)+1]
  expect_true((major_from_tonic < minor_from_tonic) %>% all)
})
