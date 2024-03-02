export_to_csv <- function(df, df_name, dir_path) {
  current_date <- format(Sys.Date(), "%Y-%m-%d")
  file_name <- gsub("_", "-", df_name)
  full_file_path <- paste0(dir_path, "/", current_date, "_", file_name, ".csv")
  write.csv(df, full_file_path, row.names = FALSE)
  message("Exported file to ", full_file_path)
}
