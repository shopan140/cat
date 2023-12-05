
#' Title
#'
#' @param vec
#' @param start
#' @param end
#' @param first2Digit
#'
#' @return
#' @export
#'
#' @examples
makeYearFromVector <- function(vec, start = 5, end = 6, first2Digit = 20){
  last2Digit <- substr(vec, start, end)
  as.integer(paste(as.character(first2Digit), last2Digit, sep = ""))

}

strSplit <- function(x, splitChar = "[(),]", position = 1){
  x_split <- unlist(strsplit(x, split = splitChar))
  out <- trimws(x_split[[position]])
  out
}

# columns names
geoCode <- "geoCode"
indicator <- "indicator"
value <- "value"
year <- "yr"


interporlate <- function(val, weight){
  total = sum(weight)

  weight*val/total
}
