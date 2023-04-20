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
        ~ !(check_used_mouse(.x, .y) & .y %in% games_req_kb)
      ),
      map2_lgl(
        raw_parsed, game_name,
        check_raw_data
      )
    )
}
check_version <- function(data) {
  data |>
    mutate(
      ver_major = game_version |>
        str_extract("\\d+\\.\\d+\\.\\d+") |>
        numeric_version() |>
        #TODO: could be _[, 1] in future release of R
        (\(.) .[, 1])()
    ) |>
    filter(
      case_when(
        game_name == "瑞文高级推理" ~ TRUE,
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
        # make sure the latest major revisions were included
        .default = ver_major == max(ver_major)
      )
    ) |>
    select(-ver_major)
}
check_used_mouse <- function(raw_parsed, game_name) {
  if (!has_name(raw_parsed, "device")) {
    return(TRUE)
  }
  # keyboard press of right-arrow was recorded as "mouse" device
  if (game_name %in% c("注意警觉", "注意指向")) {
    raw_parsed$device <- if_else(
      raw_parsed$resp == "right",
      "keyboard",
      raw_parsed$device
    )
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
