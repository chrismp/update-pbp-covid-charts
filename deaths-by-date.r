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

deathsByDate <- data.frame(
  DownloadTimestampUnix = numeric(),
  Date = as.Date(character()),
  CumulativeDeaths = numeric()
)

southFLDeathsByDate <- data.frame(
  DownloadTimestampUnix = numeric(),
  Date = as.Date(character()),
  County = character(),
  CumulativeDeaths = numeric()
)

for (i in 1:length(rawFilenames)) {
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
  
  if(!("Died" %in% names(raw))) next()
  
  cumulativeDeaths <- nrow(
    filter(
      .data = raw,
      Died == "Yes"
    )
  )
  
  deathsByDate[i, ] <- c(
    as.numeric(as.POSIXct(timeString, format = "%Y-%m-%d_%H%M%S")),
    as.character(timeDate),
    cumulativeDeaths
  )

  southFLRecord <- filter(
    .data = raw,
    Died == "Yes",
    County %in% c("Dade","Broward","Palm Beach")
  ) %>%
    group_by(County) %>%
    summarise(
      CumulativeDeaths = n()
    )
  
  southFLRecord$DownloadTimestampUnix <- as.numeric(as.POSIXct(timeString, format = "%Y-%m-%d_%H%M%S"))
  southFLRecord$Date <- timeDate
  southFLDeathsByDate <- rbind(southFLDeathsByDate, southFLRecord)
}

deathsByDate$CumulativeDeaths <- as.numeric(deathsByDate$CumulativeDeaths)
deathsByDate2 <- na.omit(deathsByDate) %>%
  group_by(Date) %>%
  top_n(1,DownloadTimestampUnix)
deathsByDate2$DailyDeathCount <- deathsByDate2$CumulativeDeaths - lag(deathsByDate2$CumulativeDeaths,1) 


southFLDeathsByDate <- southFLDeathsByDate %>%
  group_by(Date,County) %>%
  slice(which.max(DownloadTimestampUnix)) %>%
  dcast(
    formula = Date ~ County,
    value.var = "CumulativeDeaths"
  )
names(southFLDeathsByDate)[names(southFLDeathsByDate)=="Dade"] <- "Miami-Dade"

deathDFs <- list()
deathDFs[["fl-deaths-by-date"]] <- deathsByDate2
deathDFs[["south-fl-deaths-by-date"]] <- southFLDeathsByDate

out <- paste0(args[2])
dir.create(out)

for (i in 1:length(deathDFs)) {
  write.csv(
    x = deathDFs[[i]],
    file = paste0(out,'/',names(deathDFs)[i],".csv"),
    na = '',
    row.names = F
  )
}