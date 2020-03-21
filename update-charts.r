devtools::install_github("munichrocker/DatawRappr")
library(DatawRappr)

args = commandArgs(trailingOnly=TRUE)

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

chartNote <- paste0("Figures reflect all known COVID-19 cases as  ",updateTimeFormat," on ",updateDateFormat,".")

chartIDs <- list(
  "county-cases-chart" = "Vdnj6",
  "travel-related" = "ORvDZ",
  "age-group" = "BSF3m"
)

for (i in 1:length(chartIDs)) {
  dw_edit_chart(
    chart_id = chartIDs[[i]],
    api_key = apikey,
    annotate = chartNote
  )

  dw_publish_chart(
    chart_id = chartIDs[[i]],
    api_key = apikey
  )  
}