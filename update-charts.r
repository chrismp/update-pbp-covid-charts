args <- commandArgs(trailingOnly=TRUE)

devtools::install_github("munichrocker/DatawRappr")
library(DatawRappr)

apikey <- Sys.getenv("DATAWRAPPER_API")

datetime <- as.POSIXct(
  x = args[1],
  format = "%Y-%m-%d_%H%M%S"  
)

updateDateFormat <- gsub(
  pattern = " 0",
  replacement = ' ',
  x = format(
    x = datetime,
    format = "%B %d, %Y"
  )
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
  "daily-hospitalizations" = "A7nri",
  "total-hospitalizations" = "xHrl1",
  "current-deaths-counties" = "Kbjsq",
  "fl-cumulative-deaths-by-date" = "aLim8",
  "fl-daily-deaths" = "w6vI2",
  "south-fl-cumulative-deaths-by-date" = "4vTEM"
)


if(args[4]==0){
  for (i in 1:length(chartIDs)) {
    chartNote <- paste0("Figures reflect all known COVID-19 cases as of ",updateTimeFormat," on ",updateDateFormat,", including cases discovered in non-Florida residents in the state and in Florida residents outside the state. Errors in state reporting can lead to erroneous, wild fluctuations in the statistics.")
    # if(chartIDs[[i]]==chartIDs$`age-group`){
    #   chartNote <- paste0(chartNote," Florida does not report coronavirus deaths of minors.")  
    # }
    
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
    annotate = paste0("Figures reflect all known COVID-19 tests administered as of ",updateTimeFormat," on ",updateDateFormat,". Does not include antibody testing.")
  )
  
  dw_publish_chart(
    chart_id = chartIDs[["tests-map"]],
    api_key = apikey,
    return_urls = TRUE
  )  
}