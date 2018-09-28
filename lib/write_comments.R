write_comments <- function(df, file_name) {
  unlink(file_name)
  file_handle <- file(file_name, "w")
  for (var in get_vars(df, "comment")) {
    cat(paste("##", var, " ##\n\n\n\n"), file = file_handle, append = TRUE)
    # Randomly shuffle comments for privacy
    for (comment in sample(df[[var]])) {
      if (!is.na(comment)) {
        cat(paste0(comment, "\n\n"), file = file_handle, append = TRUE)
      }
    }
  }
  close(file_handle)
}
