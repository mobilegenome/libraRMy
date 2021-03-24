#' Conditional loading of RDS object
#'
#' Use this to store outputs of time-consuming calculations to disk *only* if the RDS file does not yet exists. If that file exists, load it from disk.
#' In other words, avoid the time-consuming step.
#'
#' Note that this function does not check the timestamp. If you want to force your calculation to be repeated, delete the RDS file.
#'
#' @param obj Name of the R object to create.
#' @param fpath Path to the file, storing the R object.
#' @param FUN function to create *obj*.
#' @return obj
#'
#' @export
conditional_RDS <- function(obj, fpath, FUN) {

  if (file.exists(fpath)) {
    obj_from_disk <- readRDS(fpath)
    return(obj_from_disk)
  } else {
    obj <- eval(FUN)
    dir.create(dirname(fpath), recursive = T)
    saveRDS(obj, fpath)
    return(obj)
  }
}



#' Read data via SSH
#'
#' This functions applies a user-defined function to data stored on a remote SSH server.
#' It expects that you have key-based access to the server because it does not support password-prompts.
#'
#' @param path Path to the file on the remote server
#' @param host Hostname of the remote server
#' @param FUN function to be called on the file
#' @return Output of FUN.
#'
#'
#' Example:
#'  Load a dataframe from remote
#'  df <- read_via_SSH("/path/to/my/file.csv", "user@host", read.csv)
#'
#'  Load an R object using 'source'. Note that a output variable is not necessary, as source loads all data into the current environment.
#'
#'  read_via_SSH("/path/to/my/rdata.R", "user@host", source)
#'
#' @export
read_via_SSH <- function(path, host, FUN) {
  ssh_connection <- paste('ssh', host, '  cat', path, sep=" ")
  response <- FUN(pipe(ssh_connection))
  return(response)
}
