#' Format a string
#' @name str_format
#' @title format a string like python
#'
#' @param str A string
#' @param ... See details
#' @return A string or a string vector
#'
#' @reference \url{https://docs.python.org/3/library/stdtypes.html#str.format}
#' \url{https://github.com/nicolewhite/pystr/blob/master/R/pystr_format.R}
#'
#' @examples
#' #Numeric placeholders
#' str_format("Hello {1}, Hello {2}", "world", "data")
#' str_format("Hello {1}, Hello {2}", c("world", "data"))
#' str_format("Hello {1}, Hello {2}", list("world", "data"))
#'
#' #Named placeholders
#' str_format("Hello {name}, Hello {name_}", name="world", name_="data")
#' str_format("Hello {name}, Hello {name_}", list(name="world", name_="data"))
#' str_format("Hello {name}, Hello {name_}", c(name="world", name_="data"))
#'
#' #vector paramters
#' str_format("Hello {name}, Hello {name_}", name=paste0("World", 1:10), name_=paste0("Data", 1:10))

str_format <- function(str, ...)
{
  args <- list(...)
  if (length(args) == 0) return(str)

  params <- if (length(args) == 1) args[[1]] else args
  len <- sapply(params, length)
  if (any(len != mean(len))) stop("The arguments should have the same length")

  if (is.null(names(params))) names(params) <- 1:length(params)
  str_tmp <- rep(str, mean(len))
  for (i in 1:length(params))
  {
    pattern <- paste0("\\{", names(params[i]), "\\}")
    sub_params <- params[[i]]
    for (j in 1:len[1])
    {
      str_tmp[j] <- gsub(pattern, sub_params[j], str_tmp[j])
    }
  }

  str_res <- if (mean(len) == 1) str_tmp[1] else str_tmp
  return(str_res)
}
