#' Custom Reshape Performance Data
#'
#' @param data Original long format data.
#' @param name_value The
#' @param always_suffix Logical value indicates if index name is appended to
#'   names or not.
#' @return A wider format data.
pivot_wider_indices <- function(data, name_value = "test", always_suffix = FALSE) {
  data |>
    group_by(game_name) |>
    mutate(n_index = n_distinct(index_name)) |>
    ungroup() |>
    mutate(
      game_index = if (always_suffix) {
        str_c(game_name, index_name, sep = ".")
      } else {
        if_else(
          n_index == 1,
          game_name,
          str_c(game_name, index_name, sep = ".")
        )
      }
    ) |>
    pivot_wider(
      id_cols = user_id,
      names_from = game_index,
      values_from = .data[[name_value]]
    )
}
