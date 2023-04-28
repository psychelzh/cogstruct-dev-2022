library(targets)
tar_option_set(
  packages = c("tidyverse", "lavaan", "collapse"),
  memory = "transient",
  garbage_collection = TRUE,
  storage = "worker",
  retrieval = "worker",
  error = "null",
  format = "qs",
  controller = crew::crew_controller_local(workers = 10)
)
tar_source()
future::plan(future.callr::callr)

max_num_vars <- 76 # we have 20 indicators in total (can be more)
cfg_rsmp_vars <- withr::with_seed(
  1,
  tidyr::expand_grid(
    num_vars = round(seq(3, floor(max_num_vars / 2), length.out = 10)),
    idx_rsmp = seq_len(100)
  ) |>
    dplyr::reframe(
      purrr::map(
        num_vars,
        ~ data.frame(
          id_pairs = rep(c(1, 2), .),
          idx_vars = sample.int(max_num_vars, . * 2, replace = FALSE)
        )
      ) |>
        purrr::list_rbind(),
      .by = c(num_vars, idx_rsmp)
    ) |>
    tidyr::chop(idx_vars) |>
    tidyr::chop(c(idx_rsmp, idx_vars))
)
g_invariance <- tarchetypes::tar_map(
  values = cfg_rsmp_vars,
  names = c(num_vars, id_pairs),
  tar_target(
    data_names,
    tibble(
      idx_rsmp = idx_rsmp, # use this to track samples
      tasks = map(idx_vars, ~ data_names_all[.])
    ),
    deployment = "main"
  ),
  tar_target(
    mdl_fitted,
    data_names |>
      mutate(
        mdl = map(tasks, ~ fit_g(data, all_of(.))),
        .keep = "unused"
      )
  ),
  tar_target(
    scores_g,
    mdl_fitted |>
      mutate(
        scores = map(mdl, ~ predict_g_score(data, .)),
        .keep = "unused"
      )
  )
)

list(
  tarchetypes::tar_file_read(
    indices_of_interest,
    "_targets/2023/objects/indices_of_interest",
    read = qs::qread(!!.x)
  ),
  tar_target(col_rapm, "RAPM"),
  tar_target(indices_rapm, indices_of_interest[, c("user_id", col_rapm)]),
  tar_target(data, select(indices_of_interest, -all_of(col_rapm))),
  tar_target(data_names_all, names(data)[-1]),
  g_invariance,
  tarchetypes::tar_combine(
    scores_g,
    g_invariance$scores_g,
    command = bind_rows(!!!.x, .id = "id") |>
      clean_combined(
        "scores_g",
        c("num_vars", "id_pairs")
      )
  )
)
