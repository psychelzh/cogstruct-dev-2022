#' Validate Raw Data
#'
#' This step will remove invalid data following these rules:
#' 1. Only use the data from the correct version ([check_version()]).
#' 1. Remove data of invalid device (keyboard required, but used mouse).
#' 1. Special cases: some special updates before versioning online.
#'
#' @param data_parsed Data with parsed raw data.
#' @param games_req_kb Character vector contains names of the games requiring
#'   keyboard response.
#' @return Validated data of class [data.frame()].
validate_raw_parsed <- function(data_parsed, games_req_kb) {
  data_parsed |>
    check_version() |>
    filter(
      # some games require keyboard input
      map2_lgl(
        raw_parsed, game_name,
        ~ !(check_used_mouse(.x) & .y %in% games_req_kb)
      ),
      map2_lgl(
        raw_parsed, game_name,
        check_raw_data
      )
    )
}
check_version <- function(data) {
  data |>
    filter(
      case_when(
        game_name == "瑞文高级推理" ~ TRUE,
        game_name == "舒尔特方格（中级）" ~ game_version == "3.0.0",
        game_name == "过目不忘PRO" ~ game_version == "2.0.0",
        # vital errors in the task programs for these days
        str_detect(
          game_name,
          str_c(
            c("图形推理", "图形折叠", "三维心理旋转测试"),
            collapse = "|"
          )
        ) ~ game_time < "2022-04-28" | game_time > "2022-05-07",
        # items not proper in these games before this date
        str_detect(
          game_name,
          str_c(
            c("文字推理", "数字推理"),
            collapse = "|"
          )
        ) ~ game_time > "2022-06-01",
        TRUE ~ game_version == max(game_version)
      )
    )
}
check_used_mouse <- function(raw_parsed) {
  if (!has_name(raw_parsed, "device")) {
    return(TRUE)
  }
  raw_parsed$device |>
    str_c(collapse = "-") |>
    str_split("-") |>
    map_lgl(~ any(.x == "mouse"))
}
check_raw_data <- function(raw_parsed, game_name) {
  if (game_name == "各得其所") {
    return(has_name(raw_parsed, "minmove"))
  }
  if (game_name == "小狗回家") {
    return(has_name(raw_parsed, "escortscore"))
  }
  if (game_name == "我是大厨") {
    return(any(str_detect(raw_parsed$status, "\\d")))
  }
  if (game_name %in% c("格子卡片", "美术卡片", "数字卡片", "文字卡片")) {
    return(any(raw_parsed$type == "lure"))
  }
  return(TRUE)
}
