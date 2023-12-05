#' Setting the cancensus API key
#'
#' @param apiKey for user.
#' @param overWrite if want to overwrite the existing API in the system.
#'
#' @return nothing
#' @export
#'
#' @examples
#' setCancensusApi(apiKey)
setCancensusApi <- function(apiKey, overWrite = FALSE){
  if(missing(apiKey)){
    stop("API key is missing. Please provide API key.")
  }

  if(overWrite){
    # overwrite the existing api
    cancensus::set_cancensus_api_key(key = apiKey, install = TRUE, overwrite = overWrite)
    #restart the environment
    readRenviron("~/.Renviron")
    print("Cancensus API key is installed and overwritten the exising.")
    return(invisible(NULL))
  }

  if(is.null(cancensus::show_cancensus_api_key())){
    cancensus::set_cancensus_api_key(key = apiKey, install = TRUE)
    print("Cancensus API key is installed.")
  }

  invisible(NULL)
}

#' Fetching data from stat can using vector
#'
#' @param vec vector of a the dataset to be fetched
#' @param level census subdivision or division, default is CSD
#' @param dataset which census,
#' @param regions specifies the province
#' @param geoColumn specify which column contains GEO_CODE
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' getCensusData(vec = 'v_CA21_1', dataset = "CA21", regions = list(PR='59'))
getCensusData <- function(vec,
                          level = "CSD",
                          dataset = NULL,
                          regions = list(PR = "59"),
                          geoCol = 1,
                          vectorValueCol = 11,
                          yrPrefix = 20){
# if dataset is not provided, it is prepared based on vector
  if (is.null(dataset)){
    dataset = paste("CA",
                    substr(as.character(vec), 5, 6),
                    sep = "")
  }

  df <- cancensus::get_census(dataset = dataset,
                   regions = regions,
                   vectors = vec, level=level)

  col_name <- names(df)
  print("All the Column names\n")
  print("Based on these you can edit the column paremeters.\n")
  print(col_name)
  col_to_keep = c(geoCol,
                  vectorValueCol)
# getting rid of all the extra columns
  df <- df[, col_to_keep]
  col_name <- names(df)
  splits <- unlist(strsplit(col_name[[2]], split = ":"))
  if (length(splits) < 2) {
    coln <- trimws(splits[1])
  }
  else{
    coln <- trimws(splits[2])
  }

  names(df) <- c(geoCode, coln)

  df <- df %>%
    tidyr::pivot_longer(cols = coln,
                 names_to = indicator,
                 values_to = value)

  df[, year] = paste(yrPrefix, substr(dataset, start = 3, stop = 4), sep = "")

  df
}

getCensusTable <- function(tableNo){

  df <- cansim::get_cansim(tableNo,language = "EN")
  df
}

