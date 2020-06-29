args <- commandArgs(trailingOnly=TRUE)

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

options(scipen = 999)

url <- args[1]

offset <- 0
esriStandardMaxLength <- 32000
data <- list()

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
  
  latestListIndex <- length(data)+1
  data[[latestListIndex]] <- json$features$attributes
  
  offset <- offset + esriStandardMaxLength
}

print("Done parsing JSON")

outdf <- do.call(rbind,data)

downloadedFiles <- list.files(
  path = args[2],
  full.names = T
)

previousData <- downloadedFiles[length(downloadedFiles)]
if(length(previousData)>0){
  latestFileDF <- read.csv(previousData)  
  
  tmp <- "temp.csv"
  write.csv(
    x = outdf,
    file = tmp,
    na = '',
    row.names = F
  )
  
  print("Comparing latest state data to most recently downloaded data file.")
  notDownloadingHospitalBedData <- !grepl("HOSPITALS_esri",args[2],fixed=T)
  if (notDownloadingHospitalBedData) {
    if(file.size(previousData)>=file.size(tmp)){
      print("State's latest data has not been changed.")
      file.remove(tmp)
      stop(1)
    }
  } else {
    if(file.size(previousData)==file.size(tmp)){
      print("Hospital beds data has not been changed.")
      file.remove(tmp)
      stop(1)
    }
  }

}


write.csv(
  x = outdf,
  file = args[3],
  na = '',
  row.names = F
)
