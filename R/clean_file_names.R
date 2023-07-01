

#' Rename file names witih janitor convention
#'
#' @param file.path
#' @param ... additional args for janitor::make_clean_names()
#'
#' @return
#' @export
#'
#' @examples
clean_file_names <- function(file.path,...){

fp <- file.path

  old_names <- list.files(fp,full.names = TRUE)

  extensions_raw <- list.files(fp) %>% str_extract("\\..*")

  extensions <- if_else(is.na(extensions_raw),"",extensions_raw)

  base <- list.files(fp) %>% str_remove("\\..*") %>% janitor::make_clean_names(...)

  new_names <-  paste0(fp,"/",base,extensions)

  fs::file_move(old_names,new_names)
  print("works1")
  files_impacted <- length(old_names)
  terminal_files <- crayon::green$bold(files_impacted)
  print("works2")
  fp_output <- crayon::yellow$bold(fp)


  cli::cli_alert_success("{terminal_files} files named  at {fp_output}")
}



