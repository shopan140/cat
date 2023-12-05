test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
vec = "v_CA21_1"

df <- getCensusData(vec = vec)


tableNo = "17-10-0140-01"

df <- getCensusTable(tableNo)

df %>%
  mutate(GeoUID = as.integer(GeoUID)) %>%
  filter(GeoUID > 590000)

dd <- df[grep("British Columbia", df$GEO), ]

unique(dd["REF_DATE" == "2021", "VECTOR"])

unique(dd$REF_DATE)


dd %>%
  filter(REF_DATE == "2021") %>%
  group_by(VECTOR) %>%
  mutate(count = row_number()) %>%
  filter(count==1)

read.csv("1710014001.csv")



hs <- getDataFromExcel(fileName = "C:\\Users\\MIRAHMAN\\Downloads\\rCode\\bcdatar\\data\\bc-stats_2021-new-homes-data.xlsx",
                       sheetName = "Single Detached ",
                       colToKeep = c(1,3),
                       colName = c("geoName",
                                   "value"),
                       indicatorName = "Single House",
                       yr = 2021)
