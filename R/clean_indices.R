clean_indices <- function(indices) {
  indices |>
    left_join(data.iquizoo::game_info, by = c("game_id", "game_name")) |>
    group_by(user_id, game_name, index_name) |>
    filter(
      if_else(
        str_detect(game_name_abbr, "[A|B]$"),
        row_number(desc(game_time)) == 1,
        row_number(desc(game_time)) <= 2
      )
    ) |>
    ungroup() |>
    group_by(user_id, game_name_abbr, game_name, index_name) |>
    mutate(
      occasion = case_when(
        str_detect(game_name_abbr, "A$") ~ "test",
        str_detect(game_name_abbr, "B$") ~ "retest",
        row_number(game_time) == 1 ~ "test",
        TRUE ~ "retest"
      ) |>
        factor(c("test", "retest"))
    ) |>
    ungroup() |>
    mutate(
      game_name = if_else(
        str_detect(game_name_abbr, "[A|B]$"),
        str_remove(game_name, "[A|B]$"),
        game_name
      ),
      game_name_abbr = str_remove(game_name_abbr, "[A|B]$")
    ) |>
    pivot_wider(
      id_cols = c(user_id, game_name, game_name_abbr, index_name),
      names_from = occasion,
      values_from = score
    )
}

clean_indices_single <- function(indices, users_completed) {
  indices |>
    semi_join(users_completed, by = "user_id") |>
    left_join(data.iquizoo::game_info, by = c("game_id", "game_name")) |>
    mutate(
      game_name_real = if_else(
        str_detect(game_name_abbr, "[A|B]$"),
        str_remove(game_name, "[A|B]$"),
        game_name
      ),
      game_name_abbr = str_remove(game_name_abbr, "[A|B]$")
    ) |>
    select(user_id, game_id, game_name, game_name_abbr, game_time,
           index_name, score)
}
