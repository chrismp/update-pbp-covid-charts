pkgs <- c("jsonlite")

for(x in pkgs){
  if(!is.element(x, installed.packages()[,1])){
    install.packages(x)
  } else {
    print(paste(x, " library already installed"))
  }
}

library(jsonlite)

print("Starting script to download raw data")

args <- commandArgs(trailingOnly=TRUE)
options(scipen = 999)

url <- args[1]

endOfRecords <- F
offset <- 0
esriStandardMaxLength <- 32000
cases <- list()

repeat{
  fullURL <- paste0(url,"&resultOffset=",offset)
  print(paste0(
    "Opening URL: ",
    fullURL
  ))
  
  json <- fromJSON(
    txt = fullURL
  )
  
  if(length(json$features)==0) break
  
  latestListIndex <- length(cases)+1
  cases[[latestListIndex]] <- json$features$attributes
  
  offset <- offset + esriStandardMaxLength
}

print("Done parsing JSON")

outdf <- do.call(rbind,cases)

downloadedFiles <- list.files(
  path = args[2],
  full.names = T
)

latestDownload <- downloadedFiles[length(downloadedFiles)]
latestFileDF <- read.csv(latestDownload)

print("Comparing latest state data to most recently downloaded data file.")
# print(nrow(latestFileDF))
# print(nrow(outdf))

inspectingPositiveCaseFiles <- grepl(
  pattern = "Florida_COVID19_Case_Line_Data",
  x = url
)

inspectingTestsByCounty <- grepl(
  pattern = "Florida_Testing",
  x = url
)

if(inspectingPositiveCaseFiles && nrow(latestFileDF)>=nrow(outdf)){
  print("Latest case line data is smaller than latest downloaded dataset.")
  stop(1)
}

if(inspectingPositiveCaseFiles && sum(latestFileDF$T_total)==sum(outdf$T_total)){
  print("Latest testing data is smaller than latest downloaded dataset.")
  stop(1)
}

write.csv(
  x = outdf,
  file = args[3],
  na = '',
  row.names = F
)
