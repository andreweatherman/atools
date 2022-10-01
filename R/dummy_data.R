#' Extend built-in letter vector
#'
#' Stupid little utility to extend the default letter vector past 26 (e.g.
#' 'AA', 'AB' ...)
#'
#' @export
more_letters <- function(upper=FALSE) {
  dat <- c(LETTERS, c(t(outer(LETTERS, LETTERS, paste, sep = ""))))

  if (upper) {
    return(dat)
  }

  else {
    return(tolower(dat))
  }
}


#' Create dummy numeric data
#'
#' Create a tibble of random numeric data
#'
#' Supports a maximum of 702 columns and a random seed
#'
#' @export
dummy_ints <- function(cols, rows, limit = 10, replacement = TRUE, seed = NULL) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  names <- more_letters()[1:cols]
  dat <- dplyr::tibble(a = sample(1:limit, rows, replace=replacement))

  for(i in 2:length(names)) {
    dat[[names[[i]]]] <- sample(1:limit, rows, replace=replacement)
  }

  return(dat)

}
