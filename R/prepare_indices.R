#' Prepare indices for structure analysis
#'
#' @title
#' @param indices_clean
#' @param indices_selection
#' @param keep_result
prepare_indices <- function(indices_clean, indices_selection,
                            keep_result = NULL) {
  if (is.null(keep_result)) keep_result = "target"
  indices_clean |>
    full_join(
      indices_selection,
      by = c("game_name", "game_name_abbr", "index_name")
    ) |>
    filter(check_result %in% keep_result) |>
    mutate(score = if_else(reversed == "yes", -test, test)) |>
    select(
      user_id, game_name, game_name_abbr,
      index_name, dimension, reversed,
      score, test, retest
    )
}
