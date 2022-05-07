library(targets)
future::plan(future.callr::callr)
tar_option_set(packages = "tidyverse", format = "qs")
list(
  tar_target(
    exclude_thu,
    c("图形推理A", "三维心理旋转A", "图形折叠A")
  ),
  tar_target(
    only_thu,
    c("数字推理", "文字推理", "数字推理A", "数字推理B", "文字推理A", "文字推理B")
  ),
  tarchetypes::tar_file_read(
    users,
    "preproc/_targets/objects/users",
    qs::qread(!!.x)
  ),
  tar_target(
    file_data_valid,
    "preproc/_targets/objects/data_valid",
    format = "file"
  ),
  tar_target(
    data_raw,
    qs::qread(file_data_valid) |>
      group_by(game_name) |>
      filter(game_version == max(game_version)) |>
      ungroup()
  ),
  tar_target(
    file_indices,
    "preproc/_targets/objects/indices_clean",
    format = "file"
  ),
  tar_target(
    indices_clean,
    qs::qread(file_indices) |>
      left_join(users, by = "user_id") |>
      filter(
        !(game_name %in% exclude_thu & school == "清华大学认知实验") |
          (game_name %in% only_thu & school == "清华大学认知实验")
      ) |>
      group_by(game_name) |>
      filter(game_version == max(game_version)) |>
      ungroup()
  ),
  tar_target(
    file_indices_even,
    "preproc/_targets/objects/indices_clean_even",
    format = "file"
  ),
  tar_target(
    indices_even,
    qs::qread(file_indices_even) |>
      group_by(game_name) |>
      filter(game_version == max(game_version)) |>
      ungroup()
  ),
  tar_target(
    file_indices_odd,
    "preproc/_targets/objects/indices_clean_odd",
    format = "file"
  ),
  tar_target(
    indices_odd,
    qs::qread(file_indices_odd) |>
      group_by(game_name) |>
      filter(game_version == max(game_version)) |>
      ungroup()
  ),
  tarchetypes::tar_file_read(
    config_ic,
    "config/internal-consistency.csv",
    read_csv(!!.x, show_col_types = FALSE)
  ),
  tar_target(
    reliability_split_half,
    bind_rows(
      odd = indices_odd,
      even = indices_even,
      .id = "halves"
    ) |>
      semi_join(filter(config_ic, method == "prophecy"), by = "game_name") |>
      pivot_wider(
        id_cols = c(user_id, game_name, game_name_abbr, game_version, index_name),
        names_from = halves,
        values_from = test,
        names_prefix = "score_"
      ) |>
      group_by(game_name, game_name_abbr, game_version, index_name) |>
      summarise(
        n_split_half = sum(!is.na(score_odd) & !is.na(score_even)),
        r_split_half = cor(score_odd, score_even, use = "pairwise"),
        split_half = (2 * r_split_half) / (1 + r_split_half),
        .groups = "drop"
      ) |>
      drop_na() |>
      mutate(game_name_origin = game_name) |>
      mutate(
        across(
          c(game_name, game_name_abbr),
          ~ str_remove(., "[A|B]$")
        )
      )
  ),
  # tar_target(
  #   reliability_alpha,
  #   data_raw |>
  #     semi_join(filter(config_ic, method == "alpha"), by = "game_name") |>
  #     # BNU source data were incorrect for these games
  #     left_join(users, by = "user_id") |>
  #     filter(
  #       !(game_name %in% exclude_thu & school == "清华大学认知实验") |
  #         (game_name %in% only_thu & school == "清华大学认知实验")
  #     ) |>
  #     # data from the last time of each test is deemed the right one
  #     left_join(data.iquizoo::game_info, by = c("game_id", "game_name")) |>
  #     group_by(user_id, game_name, game_version) |>
  #     filter(row_number(desc(game_time)) == 1) |>
  #     ungroup() |>
  #     unnest(raw_parsed) |>
  #     filter(acc != -1) |>
  #     mutate(
  #       block = if_else(
  #         block == 1,
  #         "prac", "test", ""
  #       )
  #     ) |>
  #     group_by(user_id, game_id) |>
  #     mutate(item = row_number(itemid)) |>
  #     ungroup() |>
  #     group_by(game_name, game_name_abbr, game_version, block) |>
  #     group_modify(
  #       ~ .x |>
  #         pivot_wider(
  #           id_cols = user_id,
  #           names_from = item,
  #           values_from = acc
  #         ) |>
  #         select(-user_id) |>
  #         psych::alpha(warnings = FALSE) |>
  #         pluck("total", "std.alpha") |>
  #         as_tibble_col(column_name = "alpha")
  #     ) |>
  #     ungroup() |>
  #     mutate(game_name_origin = game_name) |>
  #     mutate(
  #       across(
  #         c(game_name, game_name_abbr),
  #         ~ str_remove(., "[A|B]$")
  #       )
  #     )
  # ),
  tar_target(
    reliability_test_retest,
    indices_clean |>
      filter(if_all(contains("test"), is.finite)) |>
      group_by(game_name, index_name) |>
      mutate(
        data.frame(test = test, retest = retest) |>
          performance::check_outliers(method = "mahalanobis") |>
          as_tibble()
      ) |>
      ungroup() |>
      group_by(game_name, game_name_abbr, game_version, index_name) |>
      group_modify(
        ~ tibble(
          n_test_retest = nrow(.x),
          n_no_outlier = .x |>
            filter(!Outlier) |>
            nrow(),
          icc = .x |>
            select(contains("test")) |>
            psych::ICC() |>
            pluck("results", "ICC", 2),
          icc_no_outlier = .x |>
            filter(!Outlier) |>
            select(contains("test")) |>
            psych::ICC() |>
            pluck("results", "ICC", 2),
          r_test_retest = cor(.x$test, .x$retest),
          r_test_retest_no_outlier = with(
            subset(.x, !Outlier),
            cor(test, retest)
          ),
          avg_test = mean(.x$test[!.x$Outlier], na.rm = TRUE),
          avg_retest = mean(.x$retest[!.x$Outlier], na.rm = TRUE),
          retest_change = .x |>
            filter(!Outlier) |>
            summarise(t.test(retest, test, paired = TRUE) |> broom::tidy()) |>
            rstatix::p_format() |>
            rstatix::p_mark_significant() |>
            mutate(
              summary_msg = str_glue("{round(estimate, 3)}({p.value})")
            ) |>
            pull(summary_msg)
        )
      ) |>
      ungroup()
  ),
  tar_target(
    reliability_test_retest_odd,
    indices_odd |>
      left_join(users, by = "user_id") |>
      filter(
        !(game_name %in% exclude_thu & school == "清华大学认知实验") |
          (game_name %in% only_thu & school == "清华大学认知实验")
      ) |>
      filter(if_all(contains("test"), is.finite)) |>
      group_by(game_name, game_name_abbr, game_version, index_name) |>
      mutate(
        data.frame(test = test, retest = retest) |>
          performance::check_outliers(method = "mahalanobis") |>
          as_tibble()
      ) |>
      group_modify(
        ~ tibble(
          icc_odd_half = .x |>
            filter(!Outlier) |>
            select(contains("test")) |>
            psych::ICC() |>
            pluck("results", "ICC", 2),
          r_odd_half = with(
            subset(.x, !Outlier),
            cor(test, retest)
          )
        )
      ) |>
      ungroup()
  ),
  tar_target(
    reliability,
    reliability_test_retest |>
      full_join(
        reliability_test_retest_odd,
        by = c("game_name", "game_name_abbr", "game_version", "index_name")
      ) |>
      full_join(
        bind_rows(reliability_split_half, reliability_alpha) |>
          mutate(index_name = coalesce(index_name, "nc")),
        by = c("game_name", "game_name_abbr", "game_version", "index_name")
      ) |>
      mutate(game_name_origin = coalesce(game_name_origin, game_name)) |>
      select(game_name_origin, everything())
  ),
  tar_target(
    file_reliability, {
      file_name <- "output/reliability.xlsx"
      writexl::write_xlsx(reliability, file_name)
      file_name
    }
  )
)
