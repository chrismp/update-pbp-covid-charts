devtools::install_github("munichrocker/DatawRappr")
library(DatawRappr)

args <- commandArgs(trailingOnly=TRUE)

apikey <- Sys.getenv("DATAWRAPPER_API")

datetime <- as.POSIXct(
  x = args[1],
  format = "%Y-%m-%d_%H%M%S"  
)

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

chartIDs <- list(
  "county-cases-chart" = "Vdnj6",
  "travel-related" = "ORvDZ",
  "age-group" = "BSF3m",
  "south-fl" = "aof13",
  "cases-map" = "3OyJM",
  "daily-cases" = "eXjOw",
  "total-cases-daily" = "C7GGb",
  "current-deaths-counties" = "Kbjsq",
  "fl-deaths-by-date" = "aLim8",
  "south-fl-deaths-by-date" = "4vTEM"
)


if(args[4]==0){
  # Update annotation for each chart  
  chartNote <- paste0("Figures reflect all known COVID-19 cases as of ",updateTimeFormat," on ",updateDateFormat,", including cases discovered in non-Florida residents in the state and in Florida residents outside the state.")
  
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
    chart_id = chartIDs[["tests-map"]],
    api_key = apikey,
    return_urls = TRUE
  )  
}