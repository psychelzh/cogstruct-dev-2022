clean_combined <- function(df, name, to) {
  df |>
    mutate(id = str_remove(id, name)) |>
    separate(id, c(NA, to), convert = TRUE)
}
