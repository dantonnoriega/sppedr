
tools_main_dir <- function() {

  get = function() {
    env <- Sys.getenv('SPPED_MAIN_DIR')

    if(identical(env, "")) {
      message("Couldn't find env var SPPED_MAIN_DIR. This the path to the main SPPED directory on your computer. Please set using set_main_dir().")
      return(NULL)
    } else env

  }

  set = function(path = NULL) {

    if(!is.null(path)) {
      main_dir <- path
    } else {
      # have user input main_dir
      message("Please enter the path to your SPPED directory, and press enter:")
      main_dir <- readline(": ")

      main_dir <- normalizePath(main_dir)
    }


    stopifnot(dir.exists(main_dir))

    text <- paste0("SPPED_MAIN_DIR=",main_dir,"\n")

    env <- Sys.getenv('SPPED_MAIN_DIR')

    # check for existing SPPED_MAIN_DIR
    if (!identical(env, "")) { # if found, replace line and rewrite
      renv <- readLines(file.path(normalizePath("~/"), ".Renviron"))
      loc <- grep("SPPED_MAIN_DIR", renv)
      renv[loc] <- text
      Sys.setenv(SPPED_MAIN_DIR = main_dir)
      writeLines(renv, file.path(normalizePath("~/"), ".Renviron"))
    } else { # if not found, append to file
      Sys.setenv(SPPED_MAIN_DIR = main_dir)
      cat(text, file=file.path(normalizePath("~/"), ".Renviron"), append=TRUE)
    }

  }

  has = function() {
    env <- Sys.getenv('SPPED_MAIN_DIR')
    if(!identical(env, "")) TRUE else FALSE
  }

  clear = function() {
    env <- Sys.getenv('SPPED_MAIN_DIR')

    # clear SPPED_MAIN_DIR variable
    if (!identical(env, "")) { # if found, replace line and rewrite
      renv <- readLines(file.path(normalizePath("~/"), ".Renviron"))
      indx <- grepl("SPPED_MAIN_DIR", renv)
      Sys.setenv(SPPED_MAIN_DIR = "")
      writeLines(renv[!indx], file.path(normalizePath("~/"), ".Renviron"))
    }
  }

  list(get = get, set = set, has = has, clear = clear)

}


#' Assign the tools function environment
#' @noRd
spped_main_dir <- tools_main_dir()

#' Clear the sep = "|", encoding = "UTF-8") main_dir.
#' @export
clear_main_dir <- spped_main_dir$clear

#' Get main dir
#' @export

set_main_dir <- spped_main_dir$set


#' Get sep = "|", encoding = "UTF-8") main_dir
#' @export

get_main_dir <- function() {
  if(!spped_main_dir$has()) stop("Path to SPPED main directory not found. Please set your the path to your SPPED main_dir using function 'set_main_dir()'.")

  spped_main_dir$get()

}


#' Make sure sep = "|", encoding = "UTF-8") main_dir string is not empty
#'
#' @return logical TRUE if main_dir is not empty
#'
#' @export

has_main_dir <- spped_main_dir$has
