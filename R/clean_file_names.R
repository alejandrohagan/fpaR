

#' Rename file names witih janitor convention
#'
#' @param file_path file path with files you want to rename
#' @param ... additional args for janitor::make_clean_names()
#'
#' @return invisible
#' @export
#'
#' @examples
#' clean_file_names()
clean_file_names <- function(file_path,...){

fp <- file_path

  old_names <- list.files(fp,full.names = TRUE)

  extensions_raw <- list.files(fp) %>% str_extract("\\..*")

  extensions <- if_else(is.na(extensions_raw),"",extensions_raw)

  base <- list.files(fp) %>% str_remove("\\..*") %>% janitor::make_clean_names(...)

  new_names <-  paste0(fp,"/",base,extensions)

  fs::file_move(old_names,new_names)

  files_impacted <- length(old_names)

  terminal_files <- crayon::green$bold(files_impacted)

  fp_output <- crayon::yellow$bold(fp)


  cli::cli_alert_success("{terminal_files} files named  at {fp_output}")

}



