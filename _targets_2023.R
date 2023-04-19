library(targets)
Sys.setenv(R_CONFIG_ACTIVE = "2023")
future::plan(future.callr::callr)
purrr::walk(fs::dir_ls("R"), source)
tar_option_set(
  package = c("tidyverse", "preproc.iquizoo", "tarflow.iquizoo", "bit64"),
  format = "qs",
  imports = "preproc.iquizoo",
  memory = "transient",
  garbage_collection = TRUE
)
search_games_mem <- memoise::memoise(
  tarflow.iquizoo::search_games,
  cache = cachem::cache_disk("~/.cache.tarflow")
)
games <- search_games_mem(config_where = config::get("where"))
targets_data <- tarchetypes::tar_map(
  values = games,
  names = game_name_abbr,
  # major targets
  tar_target(data, pickup(query_tmpl_data, config_where_single_game)),
  tar_target(data_parsed, wrangle_data(data)),
  tar_target(
    data_valid,
    validate_raw_parsed(data_parsed, games_req_kb)
  ),
  tar_target(
    indices,
    if (!is.na(prep_fun_name)) {
      preproc_data(data_valid, prep_fun, .input = input, .extra = extra)
    }
  ),
  # configurations
  tar_target(
    config_where_single_game,
    insert_where_single_game(config_where, game_id)
  )
)
list(
  tar_target(file_config, "config.yml", format = "file"),
  tar_target(config_where, config::get("where", file = file_config)),
  tar_target(games_req_kb, config::get("require_keyboard", file = file_config)),
  tar_target(query_tmpl_users, fs::path("sql", "users.tmpl.sql"), format = "file"),
  tar_target(users, tarflow.iquizoo::pickup(query_tmpl_users, config_where)),
  tar_target(query_tmpl_data, fs::path("sql", "data.tmpl.sql"), format = "file"),
  tarchetypes::tar_file_read(
    users_project_progress,
    fs::path("sql", "progress.tmpl.sql"),
    read = pickup(!!.x, config_where)
  ),
  tar_target(
    users_completed,
    users_project_progress |>
      filter(str_detect(project_name, "^认知实验[A-E]$")) |>
      summarise(n = sum(project_progress) / 100, .by = user_id) |>
      filter(n == 5)
  ),
  targets_data,
  tarchetypes::tar_combine(
    data_parsed,
    targets_data$data_parsed
  ),
  tarchetypes::tar_combine(
    data_valid,
    targets_data$data_valid
  ),
  tarchetypes::tar_combine(
    indices,
    targets_data$indices
  ),
  tar_target(
    indices_clean,
    clean_indices_single(indices, users_completed)
  ),
  tarchetypes::tar_file_read(
    config_indices,
    "config/indices_filtering_2023.csv",
    read = read_csv(!!.x, show_col_types = FALSE)
  ),
  tar_target(
    indices_of_interest,
    indices_clean |>
      left_join(
        config_indices,
        join_by(game_name, game_name_abbr, index_name)
      ) |>
      filter(!is.na(dimension)) |>
      mutate(score = if_else(reversed, -score, score)) |>
      mutate(n_indices = n_distinct(index_name), .by = game_id) |>
      mutate(
        game_index = if_else(
          n_indices == 2,
          str_c(game_name_abbr, index_name, sep = "."),
          game_name_abbr
        )
      ) |>
      pivot_wider(
        id_cols = user_id,
        names_from = game_index,
        values_from = score
      )
  )
)
