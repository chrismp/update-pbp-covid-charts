devtools::install_github("munichrocker/DatawRappr")
library(DatawRappr)

args = commandArgs(trailingOnly=TRUE)

apikey <- Sys.getenv("DATAWRAPPER_API")

datetime <- as.POSIXct(
  x = args[1],
  format = "%Y-%m-%d_%H%M%S"  
)

if(args[4]==0){
  # Update South FL chart data
  sflChartTimestamp <- format(
    x = datetime,
    format = "%m/%d/%Y %H:%M"
  )
  
  countyCases <- read.csv(args[2])
  southFLCases <- read.csv(
    file = args[3],
    check.names = F,
    stringsAsFactors = F
  )
  
  southFLCases <- rbind(
    southFLCases,
    c(
      sflChartTimestamp,
      countyCases[which(countyCases$County=="Broward"),]$Confirmed.cases,
      countyCases[which(countyCases$County=="Dade"),]$Confirmed.cases,
      countyCases[which(countyCases$County=="Palm Beach"),]$Confirmed.cases
    )
  )
  
  write.csv(
    x = southFLCases,
    file = args[3],
    row.names = F,
    na = ''
  )
  
  
  # Update annotation for each chart
  updateDateFormat <- format(
    x = datetime,
    format = "%B %d, %Y"
  )
  
  updateTimeFormat <- format(
    x = datetime,
    "%l:%M %P"
  )
  updateTimeFormat <- trimws(updateTimeFormat)
  updateTimeFormat <- gsub(
    pattern = 'm',
    replacement = ".m.",
    x = updateTimeFormat
  )
  
  chartNote <- paste0("Figures reflect all known COVID-19 cases as of ",updateTimeFormat," on ",updateDateFormat,".")
  
  chartIDs <- list(
    "county-cases-chart" = "Vdnj6",
    "travel-related" = "ORvDZ",
    "age-group" = "BSF3m",
    "south-fl" = "aof13",
    "cases-map" = "3OyJM"
  )
  
  for (i in 1:length(chartIDs)) {
    dw_edit_chart(
      chart_id = chartIDs[[i]],
      api_key = apikey,
      annotate = chartNote
    )
    
    dw_publish_chart(
      chart_id = chartIDs[[i]],
      api_key = apikey,
      return_urls = TRUE
    )  
  }
}


# Update COVID19 testing map
if(args[5]==0){
  chartIDs[["tests-map"]] <- "4nU0g"
  dw_edit_chart(
    chart_id = chartIDs[["tests-map"]],
    api_key = apikey,
    annotate = paste0("Figures reflect all known COVID-19 tests administered as of ",updateTimeFormat," on ",updateDateFormat,".")
  )
  
  dw_publish_chart(
    chart_id = chartIDs[[i]],
    api_key = apikey,
    return_urls = TRUE
  )  
}