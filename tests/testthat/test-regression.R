context("test-regression")

test_that("regression tests", {
  df <- read.csv(system.file("regression-tests/data-formatted.csv",
                             package = "stolz15"),
                 stringsAsFactors = FALSE)
  chords <- lapply(strsplit(gsub("\\{|\\}", "", df$chord),
                                split = ","),
                   as.integer)

  for (i in seq_along(chords)) {
    expect_equal(
      smooth_log_periodicity(chords[[i]]),
      df$periodicity.stolz_smooth_t2_log[[i]],
      tolerance = 1e-3
    )
  }
})
