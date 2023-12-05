loadMigrationData <- function(fileName = NULL, overwrite = FALSE){
  if (is.null(fileName)){
    loadData("migration.rda")
  }
  else{
    migration <- readDataFile(fileName = fileName)
    usethis::use_data(migration, overwrite = overwrite)
  }

}

streamlineMigrationData <- function(migration,
                                  pr = "British Columbia",
                                  geoCol = "GEO",
                                  ageGroup = "Age.group",
                                  sex = "Sex",
                                  ageGroupField = "All ages",
                                  sexField = "Both sexes",
                                  selectCol = c("DGUID",
                                                "Components.of.population.growth",
                                                "VALUE",
                                                "REF_DATE"),
                                  geoCodSubStr = 10) {

  migration_bc <- migration[grep(pr, migration[[geoCol]]), ]
  migration_bc <- migration_bc[migration_bc[[ageGroup]] == ageGroupField &
                 migration_bc[[sex]] == sexField, selectCol]

  names(migration_bc) <- c(geoCode, indicator, value, year)

  migration_bc[[geoCode]] <- substr(migration_bc[[geoCode]],
                                    start = geoCodSubStr,
                                    stop = nchar(migration_bc[[geoCode]]))
  migration_bc[[year]] <- unlist(lapply(migration_bc[[year]],
                                        strSplit, "/", 2))
  migration_bc
}

estimateNetMigration <- function(df, multiplier){
  yearList <- unique(df[[year]])
  ll <- vector("list", length = length(yearList))

  for (i in range(1:length(yearList))){
    dd <- df[df[[year]] == yearList[[i]], ]
    kk <- lapply(unique(dd[[geoCode]]), estNtMigSnglCd, dd, multiplier)

    ll[[i]] <- bind_rows(kk)
  }

  bind_rows(ll)
}

multiplier = c("Births" = 0,
               "Deaths" = 0,
               "Immigrants" = 1,
               "Emigrants" = -1,
               "Returning emigrants" = 1,
               "Net temporary emigration" = 0,
               "Net interprovincial migration" = 1,
               "Net intraprovincial migration" = 1,
               "Net non-permanent residents"= 1,
               "Residual deviation" = 0)

#geoCodeList <- unique(migration_bc[[geoCode]])

estNtMigSnglCd <- function(gc, df, multiplier, indicatorName = "Net migration"){
  temp <- df[df[[geoCode]] == gc, ]
  ntmg <- sum(temp[[value]]*multiplier[temp[[indicator]]])
  outdf <- data.frame(matrix(ncol = 4))
  names(outdf) <- c(geoCode, indicator, value, year)
  outdf[1, geoCode] <-  gc
  outdf[1, indicator] <- indicatorName
  outdf[1, value] <-  ntmg
  outdf[1, year] <-  unique(df[[year]])[[1]]
  outdf
}
