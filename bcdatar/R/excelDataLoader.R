readDataFile <- function(fileName){
  filePath <- system.file("data", fileName, package = "bcdatar")

  data <- read.csv(filePath)
  data
}

loadData <- function(fileName){
  filePath <- system.file("data", fileName, package = "bcdatar")
  load(filePath)
}


getDataFromExcel <- function(fileName,
                             sheetName,
                             colToKeep,
                             colName,
                             indicatorName, yr){
  df <- readxl::read_xlsx(path = fileName, sheet = sheetName)
  df <- df[, colToKeep]
  names(df) <- colName
  df[, indicator] <-  indicatorName
  df[, year] <-  yr
  df
}
