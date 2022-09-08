#' Check participants' responses
#'
#' This will try to find out those unmotivated participants, especially by the
#' percent of correct for each task.
#'
#' @title
#' @param indices_clean
#' @param games_calibrate
check_responses <- function(indices_clean, games_calibrate) {

  indices_clean |>
    left_join(
      games_calibrate,
      by = c("game_name", "index_name" = "target_index")
    ) |>
    drop_na() |>
    mutate(
      is_motivated = if_else(
        str_starts(index_name, "nc"),
        test > qbinom(0.95, num_trials, chance),
        test > qbinom(0.95, num_trials, chance) / num_trials
      )
    ) |>
    select(user_id, game_name, game_name_abbr, is_motivated)

}
