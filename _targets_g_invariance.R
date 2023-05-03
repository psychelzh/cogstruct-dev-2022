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
g_invariance <- targets_sample_tasks(77, data)
g_invariance_random <- targets_sample_tasks(
  77, data_random,
  name_suffix = "_random"
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
  g_invariance,
  combine_targets(scores_g, g_invariance, c("num_vars", "id_pairs")),
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
  ),
  # random data checking
  tar_target(
    data_random,
    rnorm(500 * 77) |>
      matrix(nrow = 500) |>
      as.data.frame() |>
      mutate(user_id = seq_len(n()), .before = 1L)
  ),
  g_invariance_random,
  combine_targets(
    scores_g_random,
    g_invariance_random,
    c("num_vars", "id_pairs")
  ),
  tar_target(
    scores_g_cor_pairwise_random,
    scores_g_random |>
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
  )
)
