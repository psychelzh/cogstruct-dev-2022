library(targets)
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
    preproc_data(data_valid, prep_fun, .input = input, .extra = extra)
  ),
  tar_target(
    data_valid_even,
    data_valid |>
      mutate(raw_parsed = map(raw_parsed, ~ slice(., seq(2, n(), 2))))
  ),
  tar_target(
    data_valid_odd,
    data_valid |>
      mutate(raw_parsed = map(raw_parsed, ~ slice(., seq(1, n(), 2))))
  ),
  tar_target(
    indices_even,
    preproc_data(data_valid_even, prep_fun, .input = input, .extra = extra)
  ),
  tar_target(
    indices_odd,
    preproc_data(data_valid_odd, prep_fun, .input = input, .extra = extra)
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
  targets_data,
  tarchetypes::tar_combine(
    data_parsed,
    targets_data[[2]]
  ),
  tarchetypes::tar_combine(
    data_valid,
    targets_data[[3]]
  ),
  tarchetypes::tar_combine(
    indices,
    targets_data[[4]]
  ),
  tarchetypes::tar_combine(
    indices_even,
    targets_data[[7]]
  ),
  tarchetypes::tar_combine(
    indices_odd,
    targets_data[[8]]
  ),
  tar_target(indices_clean, clean_indices(indices)),
  tar_target(indices_clean_even, clean_indices(indices_even)),
  tar_target(indices_clean_odd, clean_indices(indices_odd)),
  tarchetypes::tar_file_read(
    indices_selection,
    "config/indices_filtering.csv",
    read = read_csv(!!.x, show_col_types = FALSE)
  ),
  tar_target(
    indices_struct,
    prepare_indices(
      indices_clean,
      indices_selection,
      keep_result = c("target", "target-alt2")
    )
  ),
  tarchetypes::tar_quarto(quarto_site, quiet = FALSE)
)
