create_vector_file_paths <- function(directory_path) {
  # List all files in the given directory path
  files_to_import <- fs::dir_ls(path = directory_path)
  
  # Loop through the files and print each with an index
  for (i in seq_along(files_to_import)) {
    cat(i, "= ", files_to_import[i], "\n")
  }
  
  # Return the vector of file paths
  return(files_to_import)
}