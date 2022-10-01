#' Single Var. Group By and Summarize
#'
#' Helper function that groups by a single variable and applies a basic summary
#' to one column
#'
#' I do this too often to justify continuing to write group_by and summarize
#'
#' @export
group_sum <- function(data, group, method, col) {

  data <-
    data %>%
      dplyr::group_by({{group}}) %>%
      dplyr::summarize(
        {{col}} := {{method}}({{col}})
      )

  return(data)

}
