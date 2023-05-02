targets_sample_tasks <- function(num_tasks, data,
                                 name_id_col = 1,
                                 sample_times = 10,
                                 sample_size = 100,
                                 name_suffix = "") {
  cfg_rsmp_vars <- withr::with_seed(
    1,
    tidyr::expand_grid(
      num_vars = round(seq(3, floor(num_tasks / 2), length.out = sample_times)),
      idx_rsmp = seq_len(sample_size)
    ) |>
      dplyr::reframe(
        purrr::map(
          num_vars,
          ~ data.frame(
            id_pairs = rep(c(1, 2), .),
            idx_vars = sample.int(num_tasks, . * 2, replace = FALSE)
          )
        ) |>
          purrr::list_rbind(),
        .by = c(num_vars, idx_rsmp)
      ) |>
      tidyr::chop(idx_vars) |>
      tidyr::chop(c(idx_rsmp, idx_vars))
  )
  tarchetypes::tar_map(
    values = cfg_rsmp_vars,
    names = c(num_vars, id_pairs),
    tar_target_raw(
      paste0("data_names", name_suffix),
      substitute(
        tibble(
          idx_rsmp = idx_rsmp, # use this to track samples
          tasks = map(idx_vars, ~ names(data)[-name_id_col][.])
        )
      ),
      deployment = "main"
    ),
    tar_target_raw(
      paste0("mdl_fitted", name_suffix),
      bquote(
        mutate(
          .(as.name(paste0("data_names", name_suffix))),
          mdl = map(tasks, ~ fit_g(.(substitute(data)), all_of(.))),
          .keep = "unused"
        )
      )
    ),
    tar_target_raw(
      paste0("scores_g", name_suffix),
      bquote(
        mutate(
          .(as.name(paste0("mdl_fitted", name_suffix))),
          scores = map(mdl, ~ predict_g_score(.(substitute(data)), .)),
          .keep = "unused"
        )
      )
    )
  )
}

clean_combined <- function(df, name, to) {
  df |>
    mutate(id = str_remove(id, name)) |>
    separate(id, c(NA, to), convert = TRUE)
}
