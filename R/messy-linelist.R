library(data.table)

# constructor
new_messy_linelist <- function(true_linelist, true_suffix = "_true") {
  dt <- copy(as.data.table(true_linelist))
  if (any(grepl(paste0(true_suffix, "$"), names(dt)))) {
    stop(paste(
      "The linelist must not contain column(s) ending with", true_suffix,
      "as these are reserved for storing the true values."
    ))
  }
  # add true columns as copies with suffix
  for (nm in names(dt)) set(dt, j = paste0(nm, true_suffix), value = dt[[nm]])
  # set class and attributes
  class(dt) <- c("messy_linelist", "data.table", "data.frame")
  attr(dt, "ml_true_suffix") <- true_suffix
  attr(dt, "ml_hidden_cols") <- character(0)
  attr(dt, "ml_history") <- list()
  return(dt)
}

# helper accessors
is_messy_linelist <- function(x) inherits(x, "messy_linelist")

get_mll_true_suffix <- function(x) {
  stopifnot(is_messy_linelist(x))
  attr(x, "ml_true_suffix")
}

get_mll_hidden_cols <- function(x) {
  stopifnot(is_messy_linelist(x))
  attr(x, "ml_hidden_cols")
}

get_mll_history <- function(x) {
  stopifnot(is_messy_linelist(x))
  attr(x, "ml_history")
}

get_true_cols <- function(x) {
  suf <- get_mll_true_suffix(x)
  names(x)[grepl(paste0(suf, "$"), names(x))]
}

get_messy_cols <- function(x) setdiff(names(x), get_true_cols(x))

get_true_pairs <- function(x) {
  suf <- get_mll_true_suffix(x)
  true_cols <- get_true_cols(x)
  messy_cols <- sub(paste0(suf, "$"), "", true_cols)
  setNames(messy_cols, true_cols) # map true -> obs
}

# make print show only messy columns
print.messy_linelist <- function(x, ...) {
  messy <- get_messy_cols(x)
  hidden <- get_mll_hidden_cols(x)
  visible <- setdiff(messy, hidden)
  print(as.data.table(x[, ..visible]))
  invisible(x)
}

# Subsetting: preserve attributes/class when subsetting with [.messy_linelist
# `[.messy_linelist` <- function(x, i, j, ..., drop) {
#   res <- data.table:::`[.data.table`(x, i, j, ..., drop = drop)
#   # reattach attributes
#   attr(res, "ml_true_suffix") <- attr(x, "ml_true_suffix")
#   class(res) <- class(x)
#   res
# }

# safe getters that return copies
get_messy <- function(x, rows = NULL) {
  stopifnot(is_messy_linelist(x))
  messy <- get_messy_cols(x)
  if (is.null(rows)) copy(as.data.table(x[, ..messy])) else copy(as.data.table(x[rows, ..messy]))
}

get_true <- function(x, rows = NULL) {
  stopifnot(is_messy_linelist(x))
  true <- get_true_cols(x)
  true_suffix <- get_mll_true_suffix(x)
  
  if (is.null(rows)) {
    result <- copy(as.data.table(x[, ..true]))
  } else {
    result <- copy(as.data.table(x[rows, ..true]))
  }
  
  # Remove the true suffix from column names
  original_names <- sub(paste0(true_suffix, "$"), "", names(result))
  setnames(result, names(result), original_names)
  
  return(result)
}

get_full <- function(x, rows = NULL) {
  stopifnot(is_messy_linelist(x))
  if (is.null(rows)) copy(as.data.table(x)) else copy(as.data.table(x[rows]))
}

# helper function to add history entries
add_to_mll_history <- function(x, func_name, args) {
  current_history <- get_mll_history(x)
  new_entry <- list(args)
  names(new_entry) <- func_name
  updated_history <- c(current_history, new_entry)
  setattr(x, "ml_history", updated_history)
  invisible(x)
}

#' Hide columns from display
#'
#' Mark specified columns as hidden so they don't appear when printing the
#' messy_linelist object. Hidden columns remain in the data but are not shown
#' in the default print output.
#'
#' @param x A messy_linelist object
#' @param cols Character vector of column names to hide. Must be names of messy
#'   columns (not true columns with suffix).
#'
#' @return Invisibly returns the modified messy_linelist object with updated
#'   hidden columns attribute.
#'
#' @examples
#' \dontrun{
#' # Hide age and location columns from display
#' mll %>% be_hidden(c("age", "location"))
#' }
#'
#' @export
be_hidden <- function(x, cols) {
  stopifnot(is_messy_linelist(x))
  if (is.character(cols)) {
    # validate columns exist in messy columns
    messy_cols <- get_messy_cols(x)
    invalid_cols <- setdiff(cols, messy_cols)
    if (length(invalid_cols) > 0) {
      stop("Column(s) not found in messy columns: ", paste(invalid_cols, collapse = ", "))
    }
    # update hidden columns attribute using setattr for data.table
    current_hidden <- get_mll_hidden_cols(x)
    new_hidden <- unique(c(current_hidden, cols))
    setattr(x, "ml_hidden_cols", new_hidden)
    # log to history
    add_to_mll_history(x, "be_hidden", list(cols = cols))
  } else {
    stop("cols must be a character vector of column names")
  }
  invisible(x)
}

#' Introduce missing values
#'
#' Randomly set values in a specified column to NA based on a given probability.
#'
#' @param x A messy_linelist object
#' @param col Character string specifying the column name to introduce missing
#'   values in. Cannot be a true column (one ending with the true suffix).
#' @param prob Numeric value between 0 and 1 indicating the probability that
#'   each value will be set to missing.
#' @param rows Optional integer vector or logical vector specifying which rows to target
#'   for introducing missing values. If integer vector, specifies row indices.
#'   If logical vector, TRUE values indicate rows to target. If NULL (default), all rows are considered.
#' @param rng_seed Optional integer for reproducible random number generation.
#'   If NULL, no seed is set.
#'
#' @return Invisibly returns the modified messy_linelist object with missing
#'   values introduced in the specified column.
#'
#' @examples
#' \dontrun{
#' # Introduce 20% missing values in the age column
#' mll %>% be_missing("age", prob = 0.2)
#' 
#' # With reproducible seed
#' mll %>% be_missing("symptoms", prob = 0.15, rng_seed = 123)
#' 
#' # Target specific rows by index
#' mll %>% be_missing("age", prob = 0.5, rows = c(1, 3, 5))
#' 
#' # Target rows using logical vector
#' mll %>% be_missing("age", prob = 0.3, rows = mll$gender == "female")
#' }
#'
#' @export
be_missing <- function(x, col, prob, rows = NULL, rng_seed = NULL) {
  stopifnot(is_messy_linelist(x))
  if (col %in% get_true_cols(x)) stop("Cannot target true column.")
  if (!is.null(rng_seed)) set.seed(rng_seed)
  
  # Determine which rows to consider
  if (is.null(rows)) {
    candidate_rows <- 1:nrow(x)
  } else if (is.logical(rows)) {
    # Validate logical vector length
    if (length(rows) != nrow(x)) {
      stop("Logical vector must have length equal to number of rows (", nrow(x), ")")
    }
    candidate_rows <- which(rows)
  } else if (is.numeric(rows)) {
    # Validate row indices
    if (any(rows < 1) || any(rows > nrow(x))) {
      stop("rows must be valid row indices between 1 and ", nrow(x))
    }
    candidate_rows <- as.integer(rows)
  } else {
    stop("rows must be NULL, a logical vector, or a numeric vector of row indices")
  }
  
  mask <- runif(length(candidate_rows)) < prob
  if (!any(mask)) return(invisible(x))
  target <- candidate_rows[mask]
  
  # attempt to preserve column type for NA assignment
  cl <- class(x[[col]])
  na_val <- switch(
    cl[1],
    integer = NA_integer_,
    numeric = NA_real_,
    character = NA_character_,
    logical = NA,
    Date = as.Date(NA),
    POSIXct = as.POSIXct(NA),
    NA
  )
  set(x, i = target, j = col, value = na_val)
  # log to history
  add_to_mll_history(x, "be_missing", list(col = col, prob = prob, rows = rows, rng_seed = rng_seed))
  invisible(x)
}

#' Convert dates to uncertain date ranges
#'
#' Transform precise dates into uncertain date ranges by adding random variation
#' around the true dates. This simulates uncertainty in date reporting that is
#' common in outbreak investigations.
#'
#' @param x A messy_linelist object
#' @param col Character string specifying the Date column to make uncertain.
#'   Cannot be a true column.
#' @param sd Numeric value specifying the standard deviation (in days) for the
#'   random offsets used to create date ranges.
#' @param col_latest Optional character string specifying a Date column to use
#'   as an upper bound for truncating date ranges. Useful for ensuring dates
#'   don't exceed known reference dates like interview dates.
#' @param rng_seed Optional integer for reproducible random number generation.
#'   If NULL, no seed is set.
#'
#' @return Invisibly returns the modified messy_linelist object with the specified
#'   date column converted to character strings representing date ranges.
#'
#' @details Date ranges are created by sampling random offsets from a normal
#'   distribution and applying them to create lower and upper bounds around the
#'   true dates. The resulting format is "YYYY-MM-DD to YYYY-MM-DD". If col_latest
#'   is provided, upper bounds are truncated to not exceed those reference dates.
#'
#' @examples
#' \dontrun{
#' # Create uncertain date ranges with 3-day standard deviation
#' mll %>% be_uncertain_date("date_onset", sd = 3)
#' 
#' # With upper bound truncation and seed
#' mll %>% be_uncertain_date("date_onset", sd = 2, 
#'                          col_latest = "date_interview", 
#'                          rng_seed = 456)
#' }
#'
#' @export
be_uncertain_date <- function(x, col, sd, col_latest = NULL, rng_seed = NULL) {
  stopifnot(is_messy_linelist(x))
  if (col %in% get_true_cols(x)) stop("Cannot target true column.")
  if (!col %in% get_messy_cols(x)) stop("Column not found in messy columns: ", col)

  # Check if col is a Date column
  if (!inherits(x[[col]], "Date")) stop("Column '", col, "' must be a Date column")

  # Get the true date column
  true_suffix <- get_mll_true_suffix(x)
  true_col <- paste0(col, true_suffix)

  if (!true_col %in% names(x)) stop("True column not found: ", true_col)
  if (!inherits(x[[true_col]], "Date")) stop("True column must be a Date column")

  if (!is.null(rng_seed)) set.seed(rng_seed)

  # Get true dates as numeric for calculations
  true_dates <- as.numeric(x[[true_col]])
  n <- length(true_dates)

  # Sample lower and upper bounds from normal distribution
  lower_offsets <- round(rnorm(n, mean = 0, sd = sd))
  upper_offsets <- round(rnorm(n, mean = 0, sd = sd))

  # Calculate date bounds
  lower_dates <- true_dates + pmin(lower_offsets, upper_offsets)
  upper_dates <- true_dates + pmax(lower_offsets, upper_offsets)

  # Apply upper truncation if col_latest is specified
  if (!is.null(col_latest)) {
    if (!col_latest %in% get_messy_cols(x)) stop("Latest date column not found in messy columns: ", col_latest)
    # Check if col_latest is a Date column
    if (!inherits(x[[col_latest]], "Date")) stop("Column '", col_latest, "' must be a Date column")

    latest_true_col <- paste0(col_latest, true_suffix)
    if (!latest_true_col %in% names(x)) stop("True latest date column not found: ", latest_true_col)
    if (!inherits(x[[latest_true_col]], "Date")) stop("Latest date column must be a Date column")

    latest_dates <- as.numeric(x[[latest_true_col]])
    # Only apply truncation where latest_dates is not NA
    valid_latest <- !is.na(latest_dates)
    upper_dates[valid_latest] <- pmin(upper_dates[valid_latest], latest_dates[valid_latest])
    # Ensure lower bound doesn't exceed truncated upper bound (only for valid latest dates)
    lower_dates[valid_latest] <- pmin(lower_dates[valid_latest], upper_dates[valid_latest])
  }

  # Convert back to dates and create range strings
  lower_date_strings <- format(as.Date(lower_dates, origin = "1970-01-01"), "%Y-%m-%d")
  upper_date_strings <- format(as.Date(upper_dates, origin = "1970-01-01"), "%Y-%m-%d")

  # Create date range strings (format: "YYYY-MM-DD to YYYY-MM-DD")
  date_ranges <- paste(lower_date_strings, "to", upper_date_strings)

  # Update the column
  set(x, j = col, value = date_ranges)
  # log to history
  add_to_mll_history(x, "be_uncertain_date", list(col = col, sd = sd, col_latest = col_latest, rng_seed = rng_seed))
  invisible(x)
}

#' Convert dates to weekly bins relative to reference date
#'
#' Transform precise dates into weekly date ranges based on their position
#' relative to a reference date. This simulates the common epidemiological
#' practice of reporting dates in weekly bins relative to interview or
#' investigation dates.
#'
#' @param x A messy_linelist object
#' @param col Character string specifying the Date column to convert to weekly
#'   bins. Cannot be a true column.
#' @param col_reference Character string specifying the Date column to use as
#'   the reference for calculating weekly bins.
#'
#' @return Invisibly returns the modified messy_linelist object with the specified
#'   date column converted to character strings representing weekly date ranges.
#'
#' @details Dates are binned into 7-day periods working backwards from the
#'   reference date. For dates occurring before the reference date, they are
#'   placed in appropriate weekly bins (0-6 days back, 7-13 days back, etc.).
#'   For dates occurring after the reference date, exact date ranges are used.
#'   The resulting format is "YYYY-MM-DD to YYYY-MM-DD". A warning is issued
#'   if any dates are later than their corresponding reference dates.
#'
#' @examples
#' \dontrun{
#' # Convert onset dates to weekly bins relative to interview dates
#' mll %>% be_uncertain_date_week("date_onset", "date_interview")
#' }
#'
#' @export
be_uncertain_date_week <- function(x, col, col_reference) {
  stopifnot(is_messy_linelist(x))
  if (col %in% get_true_cols(x)) stop("Cannot target true column.")
  if (!col %in% get_messy_cols(x)) stop("Column not found in messy columns: ", col)
  if (!col_reference %in% get_messy_cols(x)) stop("Reference column not found in messy columns: ", col_reference)

  # Check if both columns are Date columns
  if (!inherits(x[[col]], "Date")) stop("Column '", col, "' must be a Date column")
  if (!inherits(x[[col_reference]], "Date")) stop("Column '", col_reference, "' must be a Date column")

  # Get the true date columns
  true_suffix <- get_mll_true_suffix(x)
  true_col <- paste0(col, true_suffix)
  true_reference_col <- paste0(col_reference, true_suffix)

  if (!true_col %in% names(x)) stop("True column not found: ", true_col)
  if (!true_reference_col %in% names(x)) stop("True reference column not found: ", true_reference_col)
  if (!inherits(x[[true_col]], "Date")) stop("True column must be a Date column")
  if (!inherits(x[[true_reference_col]], "Date")) stop("True reference column must be a Date column")

  # Get true dates
  true_dates <- x[[true_col]]
  reference_dates <- x[[true_reference_col]]

  # Check for dates later than reference and issue warning
  later_than_ref <- !is.na(true_dates) & !is.na(reference_dates) & true_dates > reference_dates
  if (any(later_than_ref)) {
    warning("Some dates in '", col, "' are later than their corresponding reference dates in '", col_reference, "'")
  }

  # Initialize result vector
  date_ranges <- character(length(true_dates))

  # Process each row
  for (i in seq_along(true_dates)) {
    if (is.na(true_dates[i]) || is.na(reference_dates[i])) {
      date_ranges[i] <- NA_character_
      next
    }

    target_date <- true_dates[i]
    ref_date <- reference_dates[i]

    if (target_date > ref_date) {
      # Date is later than reference: use exact date range
      date_str <- format(target_date, "%Y-%m-%d")
      date_ranges[i] <- paste(date_str, "to", date_str)
    } else {
      # Calculate weekly bin
      # Days back from reference date
      days_back <- as.numeric(ref_date - target_date)

      # Find which weekly bin (0-based)
      bin_number <- floor(days_back / 7)

      # Calculate bin boundaries
      bin_end <- ref_date - (bin_number * 7)
      bin_start <- bin_end - 6

      # Format date range
      start_str <- format(bin_start, "%Y-%m-%d")
      end_str <- format(bin_end, "%Y-%m-%d")
      date_ranges[i] <- paste(start_str, "to", end_str)
    }
  }

  # Update the column
  set(x, j = col, value = date_ranges)
  # log to history
  add_to_mll_history(x, "be_uncertain_date_week", list(col = col, col_reference = col_reference))
  invisible(x)
}

#' Impute missing values from another column
#'
#' Fill missing values in one column using non-missing values from another
#' column.
#'
#' @param x A messy_linelist object
#' @param col Character string specifying the target column to impute missing
#'   values in. Cannot be a true column.
#' @param col_from Character string specifying the source column to take values
#'   from. Cannot be a true column.
#'
#' @return Invisibly returns the modified messy_linelist object with missing
#'   values in the target column filled from the source column.
#'
#' @examples
#' \dontrun{
#' # Impute missing ages from age_reported column
#' mll %>% do_impute_from_other("age", "age_reported")
#' }
#'
#' @export
do_impute_from_other <- function(x, col, col_from) {
  stopifnot(is_messy_linelist(x))
  if (col %in% get_true_cols(x)) stop("Cannot target true column.")
  if (col_from %in% get_true_cols(x)) stop("Cannot use true column as source.")
  if (!col %in% get_messy_cols(x)) stop("Column not found in messy columns: ", col)
  if (!col_from %in% get_messy_cols(x)) stop("Source column not found in messy columns: ", col_from)

  # Find missing values in target column
  missing_mask <- is.na(x[[col]])
  if (!any(missing_mask)) return(invisible(x))

  # Get indices of missing values
  missing_indices <- which(missing_mask)

  # Impute missing values with values from source column
  impute_values <- x[[col_from]][missing_indices]
  set(x, i = missing_indices, j = col, value = impute_values)

  # log to history
  add_to_mll_history(x, "do_impute_from_other", list(col = col, col_from = col_from))
  invisible(x)
}

#' Resolve uncertain dates to single dates
#'
#' Convert uncertain date ranges (in "YYYY-MM-DD to YYYY-MM-DD" format) back
#' to single Date values using different resolution strategies.
#'
#' @param x A messy_linelist object
#' @param col Character string specifying the column containing uncertain date
#'   ranges to resolve. Cannot be a true column.
#' @param method Character string specifying the resolution method. Options are:
#'   \itemize{
#'     \item "min" - Use the earliest date in the range
#'     \item "max" - Use the latest date in the range  
#'     \item "middle" - Use the middle date of the range
#'   }
#'
#' @return Invisibly returns the modified messy_linelist object with the specified
#'   column converted from date range strings to Date objects.
#'
#' @examples
#' \dontrun{
#' # Resolve to earliest dates in ranges
#' mll %>% do_resolve_uncertain_date("date_onset", method = "min")
#' 
#' # Resolve to middle dates
#' mll %>% do_resolve_uncertain_date("date_onset", method = "middle")
#' }
#'
#' @export
do_resolve_uncertain_date <- function(x, col, method = c("min", "max", "middle")) {
  stopifnot(is_messy_linelist(x))
  if (col %in% get_true_cols(x)) stop("Cannot target true column.")
  if (!col %in% get_messy_cols(x)) stop("Column not found in messy columns: ", col)
  
  method <- match.arg(method)
  
  # Get the date range strings
  date_ranges <- x[[col]]
  resolved_dates <- rep(as.Date(NA), length(date_ranges))
  
  # Process each date range
  for (i in seq_along(date_ranges)) {
    if (is.na(date_ranges[i])) {
      resolved_dates[i] <- as.Date(NA)
      next
    }
    
    # Parse date range string (format: "YYYY-MM-DD to YYYY-MM-DD")
    if (grepl(" to ", date_ranges[i])) {
      parts <- strsplit(date_ranges[i], " to ")[[1]]
      if (length(parts) == 2) {
        start_date <- as.Date(parts[1])
        end_date <- as.Date(parts[2])
        
        if (!is.na(start_date) && !is.na(end_date)) {
          resolved_dates[i] <- switch(method,
            "min" = start_date,
            "max" = end_date,
            "middle" = start_date + round(as.numeric(end_date - start_date) / 2)
          )
        }
      }
    } else {
      # Try to parse as single date
      single_date <- tryCatch(as.Date(date_ranges[i]), error = function(e) as.Date(NA))
      resolved_dates[i] <- single_date
    }
  }
  
  # Update the column
  set(x, j = col, value = resolved_dates)
  # log to history
  add_to_mll_history(x, "do_resolve_uncertain_date", list(col = col, method = method))
  invisible(x)
}

#' Fill target column with first non-missing value from source columns
#'
#' Populate a target column by taking the first non-missing value from a list
#' of source columns for each row. This implements a coalescing operation
#' useful for combining multiple potential data sources in order of preference.
#'
#' @param x A messy_linelist object
#' @param col Character string specifying the target column to fill. Cannot be
#'   a true column.
#' @param cols_from Character vector specifying the source columns to coalesce
#'   from, in order of preference. Cannot include true columns.
#'
#' @return Invisibly returns the modified messy_linelist object with the target
#'   column populated using the coalescing logic.
#'
#' @examples
#' \dontrun{
#' # Fill age column from multiple sources in order of preference
#' mll %>% do_coalesce("age", c("age_years", "age_months", "age_estimated"))
#' 
#' # Combine location information
#' mll %>% do_coalesce("location", c("address", "district", "region"))
#' }
#'
#' @export
do_coalesce <- function(x, col, cols_from) {
  stopifnot(is_messy_linelist(x))
  if (col %in% get_true_cols(x)) stop("Cannot target true column.")
  if (any(cols_from %in% get_true_cols(x))) stop("Cannot use true columns as source.")
  if (!col %in% get_messy_cols(x)) stop("Column not found in messy columns: ", col)
  
  # Check that all source columns exist
  missing_cols <- setdiff(cols_from, get_messy_cols(x))
  if (length(missing_cols) > 0) {
    stop("Source column(s) not found in messy columns: ", paste(missing_cols, collapse = ", "))
  }
  
  n_rows <- nrow(x)
  
  # Initialize result with the same type as the target column
  target_col_value <- x[[col]]
  result_values <- target_col_value
  
  # For each row, find the first non-NA value across source columns
  for (i in seq_len(n_rows)) {
    for (src_col in cols_from) {
      value <- x[[src_col]][i]
      if (!is.na(value)) {
        result_values[i] <- value
        break
      }
    }
  }
  
  # Update the target column
  set(x, j = col, value = result_values)
  
  # log to history
  add_to_mll_history(x, "do_coalesce", list(col = col, cols_from = cols_from))
  invisible(x)
}
