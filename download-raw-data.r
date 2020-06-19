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

previousData <- downloadedFiles[length(downloadedFiles)]
latestFileDF <- read.csv(previousData)


tmp <- "temp.csv"
write.csv(
  x = outdf,
  file = tmp,
  na = '',
  row.names = F
)

print("Comparing latest state data to most recently downloaded data file.")
if(file.size(previousData)>file.size(tmp)){
  print("State's latest data has not been changed.")
  file.remove(tmp)
  stop(1)
}


write.csv(
  x = outdf,
  file = args[3],
  na = '',
  row.names = F
)
