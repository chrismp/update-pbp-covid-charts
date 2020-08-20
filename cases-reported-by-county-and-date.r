args <- commandArgs(trailingOnly=TRUE)

options(scipen = 999)

pkgs <- c(
  "dplyr",
  "reshape2"
)

for(x in pkgs){
  if(!is.element(x, installed.packages()[,1])){
    install.packages(x)
  } else {
    print(paste(x, " library already installed"))
  }
}

library(dplyr)
library(reshape2)

rawFilenames <- list.files(
  path = paste0(args[1],'/')
)

dfs <- list()


for (i in 1:length(rawFilenames)){
  fname <- rawFilenames[[i]]
  print(fname)
  timeString <- gsub(
    pattern = "FL-|.csv",
    replacement = '',
    x = fname
  )
  
  timeDate <- as.Date(
    x = timeString,
    format = "%Y-%m-%d_%H%M%S"
  )
  
  raw <- read.csv(
    file = paste0(args[1],'/',fname)
  )
  
  resDeathsCol <- ifelse(
    test = "FLResDeaths" %in% colnames(raw),
    yes = "FLResDeaths",
    no = "C_FLResDeaths"
  )
  
  countyCol <- ifelse(
    test = "County_1" %in% colnames(raw),
    yes = "County_1",
    "County"
  )
  
  keeps <- c(
    countyCol,
    "T_positive",
    resDeathsCol,
    "T_negative"
  )
  
  df <- raw[keeps]
  df$unixDownloadTime <- as.numeric(as.POSIXct(timeString, format = "%Y-%m-%d_%H%M%S"))
  df$Date <- as.POSIXct(timeString, format = "%Y-%m-%d")
  
  colnames(df) <- c(
    "County",
    "Cases",
    "FL resident deaths",
    "Negative test results",
    "UNIX download timestamp",
    "Date"
  )
  
  dfs[[i]] <- df
}

cases <- do.call(rbind,dfs) %>%
  group_by(Date,County) %>%
  slice(which.max(`UNIX download timestamp`)) %>%
  dcast(
    formula = Date ~ County,
    value.var = "Cases"
  )

deaths <- do.call(rbind,dfs) %>%
  group_by(Date,County) %>%
  slice(which.max(`UNIX download timestamp`)) %>%
  dcast(
    formula = Date ~ County,
    value.var = "FL resident deaths"
  )

negatives <- do.call(rbind,dfs) %>%
  group_by(Date,County) %>%
  slice(which.max(`UNIX download timestamp`)) %>%
  dcast(
    formula = Date ~ County,
    value.var = "Negative test results"
  )

o <- args[2]
dir.create(o)

write.csv(
  x = cases,
  file = paste0(o,"/Cases by county and date.csv"),
  na = '',
  row.names = F
)

write.csv(
  x = deaths,
  file = paste0(o,"/FL resident deaths by county and date.csv"),
  na = '',
  row.names = F
)

write.csv(
  x = negatives,
  file = paste0(o,"/Negative results by county and date.csv"),
  na = '',
  row.names = F
)


