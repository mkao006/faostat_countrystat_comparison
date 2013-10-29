getComparisonData = function(countryName, countryCode){
    require(FAOSTAT)
    countryStat.df =
        read.csv(file = paste0(countryName, "_primary_production.csv"),
                     stringsAsFactors = FALSE)
    colnames(countryStat.df) =
        c("Year", "itemCode", "itemName", "countrystatValue",
          "coutrystatFlag")
    countryStat.df$countryCode = countryCode
    ## countryStat.df$Flag = NULL

    uniqueItem = unique(countryStat.df$itemCode)

    faostat.df = getFAOtoSYB(domainCode = rep("QC", length(uniqueItem)),
        itemCode = uniqueItem,
        elementCode = rep(5510, length(uniqueItem)),
        outputFormat = "long", returnFlags = TRUE)


    faostat_country.df =
        faostat.df$entity[faostat.df$entity$FAOST_CODE == countryCode, ]
    ## print(faostat_country.df)
    colnames(faostat_country.df) =
        c("countryCode", "Year", "faostatValue", "faostatFlag",
          "domainCode", "itemCode", "elementCode", "name")

    full_set.df = merge(countryStat.df, faostat_country.df,
        all.x = TRUE, by = c("countryCode", "Year", "itemCode"))
    full_set.df$pctdiff = with(full_set.df,
        (faostatValue - countrystatValue)/faostatValue)
    full_set.df
}
    

dataConcordance = function(data, pcttolerance = 0.1, subset){
    ## if(!missing(subset))
    ##     subData = data[substitute(subset), ]
    cat("Percentage of perfect agreement:\n ",
          round(sum(na.omit(data$pctdiff == 0))/NROW(data), 4) * 100,
        "%\n", sep = "")

    cat("Percentage of partial agreement:\n ",
          round(sum(na.omit(abs(data$pctdiff) <= pcttolerance &
              abs(data$pctdiff) > 0))/NROW(data), 4) * 100, "%\n",
        sep = "")

    cat("Percentage of non agreement:\n ",
        round(sum(na.omit(abs(data$pctdiff) >
                          pcttolerance))/NROW(data), 4) * 100,
        "%\n", sep = "")

    cat("Percentage of missing comparison:\n ",
        round(sum(is.na(data$pctdiff))/NROW(data), 4) * 100, "%\n",
        sep = "")
}
    



## Write the result to table
write.comparedata = function(x, file){
    foo = function(x){
        ifelse(x %in% c("", NA), "", paste0(" (", x, ")"))
    }

    x[, faostatCharValue := paste0(faostatValue, foo(faostatFlag))]
    x[faostatCharValue == "NA", faostatCharValue := as.character(NA)]
    table.dt =
        x[, list(countryCode, Year, itemCode, itemName,
                    countrystatValue, faostatCharValue, pctdiff)]
    table.dt[, pctdiff := round(pctdiff, digits = 4)]
    setnames(table.dt,
             old = c("countrystatValue", "faostatCharValue", "pctdiff"),
             new = c("countryStat", "FAOSTAT", "Percentage difference"))
    finalTable.dt = dcast(melt(table.dt,
        id.var = c("countryCode", "Year", "itemCode",
            "itemName")),
        formula = countryCode + itemCode + itemName +
        variable ~ Year, value.var = "value")

    write.csv(x = finalTable.dt,
              row.names = FALSE, na = "",
              file = file)
}
