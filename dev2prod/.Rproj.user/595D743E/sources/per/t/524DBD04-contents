#' Title
#'
#' @param prod_dir
#' @param dev_dir
#' @param prod_srv
#'
#' @return
#' @export
#'
#' @examples
push_into_prod <- function(prod_dir,
                           dev_dir = ".",
                           prod_srv = "sftp://shiny:@shiny") {
 lftp_command <- paste0("mirror -e -R ", dev_dir, " ", prod_dir, "; quit")
 shell_command <- paste0("lftp ", prod_srv, " -e \"", lftp_command, "\"")
 cat("$", shell_command, "\n")
 system(shell_command)
}
