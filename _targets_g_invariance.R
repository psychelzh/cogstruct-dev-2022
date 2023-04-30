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

max_num_vars <- 77
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
  tar_target(
    indices_clean_outliers_iqr,
    indices_of_interest |>
      filter(!is_outlier_iqr)
  ),
  # RAPM is the gold standard and should no be included in data sampling
  tar_target(name_rapm, "RAPM"),
  tar_target(
    data_rapm,
    indices_clean_outliers_iqr |>
      filter(game_name_abbr == name_rapm) |>
      pivot_wider(
        id_cols = user_id,
        names_from = game_name_abbr,
        # `score_adj` ensure positive relation between ability and score
        values_from = score_adj
      )
  ),
  tar_target(
    data,
    indices_clean_outliers_iqr |>
      filter(game_name_abbr != name_rapm) |>
      pivot_wider(
        id_cols = user_id,
        names_from = game_index,
        # `score_adj` ensure positive relation between ability and score
        values_from = score_adj
      )
  ),
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
  ),
  tar_target(
    scores_g_cor_pairwise,
    scores_g |>
      pivot_wider(
        id_cols = c(num_vars, idx_rsmp),
        names_from = id_pairs,
        values_from = scores
      ) |>
      mutate(
        r = map2_dbl(
          `1`, `2`,
          ~ cor(.x$g, .y$g, use = "pairwise")
        ),
        .keep = "unused"
      )
  ),
  tar_target(
    scores_g_cor_rapm,
    scores_g |>
      mutate(
        map(
          scores,
          ~ . |>
            inner_join(data_rapm, by = "user_id") |>
            summarise(r_rapm = cor(g, RAPM, use = "pairwise"))
        ) |>
          list_rbind(),
        .keep = "unused"
      )
  )
)
