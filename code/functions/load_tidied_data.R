load_tidied_data <- function(base_url, file_names) {
  dfs <- map(file_names, ~ read_csv(glue::glue("{base_url}{.x}"), show_col_types = FALSE))
  names(dfs) <- names(file_names)
  return(dfs)
}