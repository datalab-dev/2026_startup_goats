# file to help store functions and global variables that are used across multiple files in the app

# the maximum number of decimal places to round to when displaying numeric values in the UI
NUMERIC_PRECISION <- 2

# rounds to the NUMERIC_PRECISION amount of decimal places, with null and numeric checking
round_precision <- function(x, digits = NUMERIC_PRECISION) {
  if (is.null(x) || !is.numeric(x)) return(x)
  round(x, digits)
}

# checks for not NULL, numeric, and length
is_valid_number <- function(x, len = 1) {
  !is.null(x) && length(x) == len && is.numeric(x) && !anyNA(x)
}

# clamps to the numeric range based on the min and max values, and rounds to the NUMERIC_PRECISION amount of decimal places
clamp_numeric <- function(x, min_val, max_val,
                          digits = NUMERIC_PRECISION, len = 1) {
  if (!is_valid_number(x, len)) return(NULL)
  round_precision(max(min_val, min(max_val, x)), digits)
}

# utilizes is_valid_number() to print a warning
check_num <- function(x) {
  if (!is_valid_number(x, len = length(x))) {
    print("Argument is not a valid number (NULL, non-numeric, or NA)")
  }
}

# uses R's assertions to check if the score is in a valid range from min_score to max_score (default 1-50)
# throws an error if not, using stop()
check_score_in_valid_range <- function(score, min_score = 1, max_score = 50) {
  if (!is_valid_number(score)) {
    stop("Linear score must be a single non-missing number; got: ",
         paste(format(score), collapse = ", "))
  }
  if (score < min_score || score > max_score) {
    stop(sprintf("Linear score %s is outside the valid range [%g, %g].",
                 format(score), min_score, max_score))
  }
}
