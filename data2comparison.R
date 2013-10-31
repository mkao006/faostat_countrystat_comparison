########################################################################
## Title: Download all FAOSTAT and countrySTAT data and convert them
## to comparison format
## Date: 2013-10-29
########################################################################

library(plyr)
library(lattice)
library(data.table)
library(FAOSTAT)
source("faostat_countrystat_comparison.R")

countrySTATFiles = dir(pattern = "_primary_production.csv")
index = sapply(X = gregexpr("_", countrySTATFiles),
    FUN = function(x) x[[1]])
countryNames = substring(countrySTATFiles, 1, index - 1)


meta.df = data.frame(countryName = countryNames)
for(i in 1:NROW(meta.df)){
    try({meta.df[i, "countryCode"] =
        FAOcountryProfile[grep(meta.df[i, "countryName"],
                               FAOcountryProfile$ABBR_FAO_NAME,
                               ignore.case = TRUE), "FAOST_CODE"]})
}
meta.df[meta.df$countryName == "burkinafaso", "countryCode"] = 233
meta.df[meta.df$countryName == "cotedivore", "countryCode"] = 107
meta.df[meta.df$countryName == "guineabissau", "countryCode"] = 175
meta.df[meta.df$countryName == "mali", "countryCode"] = 133
meta.df[meta.df$countryName == "niger", "countryCode"] = 158 



for(i in 1:NROW(meta.df)){
    try(
        {tmp.dt = data.table(getComparisonData(meta.df[i, "countryName"],
             meta.df[i, "countryCode"]))
         tmp.dt = tmp.dt[, list(countryCode, Year, itemCode,
             itemName, countrystatValue, faostatValue, faostatFlag,
             pctdiff)]
     }
        )
    if(!inherits(tmp.dt, "try-error")){
        write.comparedata(x = tmp.dt,
                          file = paste0(meta.df[i, "countryName"],
                              "_comparison_table.csv"))
        meta.df[i, "result"] = "PASS"
     } else {
         meta.df[i, "result"] = "FAIL"
     }
}
