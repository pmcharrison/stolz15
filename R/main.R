stolzenburg_dyad <- function(x) {
  orig = x-x[1]
  orig_matrix = rationalise_chord(orig)
  inversion = x-x[2]
  inversion_matrix = rationalise_chord(inversion)
  tibble::tibble_row(
    orig_chord = toString(orig),
    orig_ratio_1 = toString(orig_matrix[,1]),
    orig_ratio_2 = toString(orig_matrix[,2]),
    orig_relative_periodicity = log2(relative_periodicity(orig_matrix)),
    inversion_chord = toString(inversion),
    inversion_ratio_1 = toString(inversion_matrix[,1]),
    inversion_ratio_2 = toString(inversion_matrix[,2]),
    inversion_relative_periodicity = log2(relative_periodicity(inversion_matrix)),
    smooth_log_periodicity = smooth_log_periodicity(x)
  )
}
stolzenburg_triad <- function(x) {
  orig = x-x[1]
  orig_matrix = rationalise_chord(orig)
  inversion = x-x[2]
  inversion_matrix = rationalise_chord(inversion)
  sec_inversion = x-x[3]
  sec_inversion_matrix = rationalise_chord(sec_inversion)
  tibble::tibble_row(
    # root
    orig_chord = toString(orig),
    orig_ratio_1 = toString(orig_matrix[,1]),
    orig_ratio_2 = toString(orig_matrix[,2]),
    orig_relative_periodicity = log2(relative_periodicity(orig_matrix)),
    # first inversion
    inversion_chord = toString(inversion),
    inversion_ratio_1 = toString(inversion_matrix[,1]),
    inversion_ratio_2 = toString(inversion_matrix[,2]),
    inversion_relative_periodicity = log2(relative_periodicity(inversion_matrix)),
    # second inversion
    sec_inversion_chord = toString(sec_inversion),
    sec_inversion_ratio_1 = toString(sec_inversion_matrix[,1]),
    sec_inversion_ratio_2 = toString(sec_inversion_matrix[,2]),
    sec_inversion_relative_periodicity = log2(relative_periodicity(sec_inversion_matrix)),
    smooth_log_periodicity = smooth_log_periodicity(x)
  )
}
#' Smoothed log periodicity
#'
#' This function computes a chord's smoothed logarithmic periodicity,
#' after \insertCite{Stolzenburg2015;textual}{stolz15}.
#' @param x Sonority to analyse.
#' This will be coerced to an object of class \code{\link[hrep]{pi_chord}}.
#' Numeric inputs will be interpreted as MIDI note numbers.
#' @param d (numeric scalar):
#' Maximal allowed error in the algorithm's
#' interval approximation step, expressed as
#' a fraction of the original interval.
#' The default value, 0.0102, corresponds to 'Rational Tuning II'
#' in Stolzenburg's paper.
#' @return A numeric scalar identifying the chord's periodicity.
#' High values mean a higher period length, lower periodicity,
#' and lower consonance.
#' @references
#'   \insertAllCited{}
#' @rdname smooth_log_periodicity
#' @export
smooth_log_periodicity <- function(x, d = 0.0102) {
  UseMethod("smooth_log_periodicity")
}

#' @rdname smooth_log_periodicity
#' @export
smooth_log_periodicity.default <- function(x, d = 0.0102) {
  x <- hrep::pi_chord(x)
  do.call(smooth_log_periodicity, as.list(environment()))
}

#' @rdname smooth_log_periodicity
#' @export
smooth_log_periodicity.pi_chord <- function(x, d = 0.011) {
  checkmate::qassert(d, "N1(0,)")
  chord <- as.numeric(x)
  mean(vapply(seq_along(x), function(i) {
    tmp_chord <- x - x[i]
    print(x)
    print('tmp_chord')
    print(tmp_chord)
    print(log2(relative_periodicity(rationalise_chord(tmp_chord, d = d))))
    log2(relative_periodicity(rationalise_chord(tmp_chord, d = d)))
  }, numeric(1)))
}

# See DOI: 10.1080/17459737.2015.1033024
# @param x Number to approximate
# @param d Tolerance ratio
fraction <- function(x, d, verbose = FALSE) {
  x_min <- (1 - d) * x
  x_max <- (1 + d) * x
  a_l <- floor(x)
  b_l <- 1
  a_r <- floor(x) + 1
  b_r <- 1
  a <- round(x)
  b <- 1
  while(a / b < x_min || x_max < a / b) {
    x_0 <- 2 * x - a / b
    if (x < a / b) {
      a_r <- a
      b_r <- b
      k <- floor((x_0 * b_l - a_l) / (a_r - x_0 * b_r))
      a_l <- a_l + k * a_r
      b_l <- b_l + k * b_r
    } else {
      a_l <- a
      b_l <- b
      k <- floor((a_r - x_0 * b_r) / (x_0 * b_l - a_l))
      a_r <- a_r + k * a_l
      b_r <- b_r + k * b_l
    }
    a <- a_l + a_r
    b <- b_l + b_r
    if (verbose) message("a = ", a, ", b = ", b)
  }
  c(a, b)
}

# Uses rational tuning system 2
# Non-integer inputs permitted
# @param d = 0.011 corresponds to rational tuning 2
get_rational_interval <- function(x, d = 0.0102) {
  stopifnot(length(x) == 1L)
  octave <- floor(x / 12)
  pitch_class <- x %% 12
  res <- fraction(2 ^ (pitch_class / 12), d = d)
  while (octave != 0) {
    if (octave < 0) {
      res <- half_fraction(res)
      octave <- octave + 1L
    } else if (octave > 0) {
      res <- double_fraction(res)
      octave <- octave - 1L
    }
  }
  res
}

half_fraction <- function(x) {
  stopifnot(length(x) == 2L)
  if (x[1] %% 2L == 0L) x[1] <- x[1] / 2L else x[2] <- x[2] * 2L
  x
}

double_fraction <- function(x) {
  stopifnot(length(x) == 2L)
  if (x[2] %% 2L == 0L) x[2] <- x[2] / 2L else x[1] <- x[1] * 2L
  x
}

rationalise_chord <- function(x, d=0.0102) {
  sapply(x, function(y) get_rational_interval(y, d))
}

relative_periodicity <- function(x) {
  stopifnot(is.matrix(x), nrow(x) == 2, ncol(x) > 0L)
  print(x)
  lcm(x[2, ])  * x[1, 1] / x[2, 1]
}

lcm <- function(x) {
  if (length(x) == 1L) x else if (length(x) == 2L) {
    gmp::lcm.default(x[1], x[2])
  } else lcm(c(x[1], lcm(x[-1])))
}
