print("Starting chart updater")

args <- commandArgs(trailingOnly=TRUE)

devtools::install_github("munichrocker/DatawRappr",ref="master")
library(DatawRappr)

apikey <- Sys.getenv("DATAWRAPPER_API")

caseLineFiles <- list.files(
  path = args[1],
  full.names = T
)

latestFile <- caseLineFiles[length(caseLineFiles)]

latestFileTimeString <- gsub(
  pattern = ".*FL-|.csv",
  replacement = '',
  x = latestFile
)
# latestFileTime <- as.numeric(as.POSIXct(latestFileTimeString,format="%Y-%m-%d_%H%M%S"))


datetime <- as.POSIXct(
  x = latestFileTimeString,
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
  "county" = "Vdnj6",
  "age-group" = "BSF3m",
  "south-fl-cumulative" = "aof13",
  "cases-by-date-SouthFL" = "eXjOw",
  "cases-by-date" = "C7GGb",
  "current-deaths" = "Kbjsq",
  "median-age-by-case-date" = "hMtwa",
  "fl-deaths-by-date" = "aLim8",
  "south-fl-deaths-by-date" = "4vTEM"
)


for (i in 1:length(chartIDs)) {
  chartName <- names(chartIDs)[[i]]
  f <- paste0(args[2],'/',chartName,".csv")
  fIn <- file.info(f)
  editTime <- as.numeric(as.POSIXct(fIn$mtime))
  
  print(
    paste0(
      "Attempting to update chart ",
      chartIDs[[i]]
    )
  )

  # processedDatasetAlreadyMade <- editTime > latestFileTime
  # if(processedDatasetAlreadyMade){
  #   print("Chart file newer than raw data. Skipping.")
  #   next
  # } 

  if(chartName %in% c("fl-deaths-by-date","south-fl-deaths-by-date")){
  	chartNote <- paste0("Figures reflect all known COVID-19 deaths as of ",updateTimeFormat," on ",updateDateFormat," including non-Florida residents.")
  } else {
  	chartNote <- paste0("Figures reflect all known COVID-19 cases as of ",updateTimeFormat," on ",updateDateFormat,", including cases discovered in non-Florida residents in the state and in Florida residents outside the state.")	
  }


  print("Editing chart")
  dw_edit_chart(
    chart_id = chartIDs[[i]],
    api_key = apikey,
    annotate = chartNote
  )
  print("Publishing chart")  
  dw_publish_chart(
    chart_id = chartIDs[[i]],
    api_key = apikey,
    return_urls = TRUE
  )  
}

# 
# # Update COVID19 testing map
# if(args[5]==0){
#   chartIDs[["tests-map"]] <- "4nU0g"
#   dw_edit_chart(
#     chart_id = chartIDs[["tests-map"]],
#     api_key = apikey,
#     annotate = paste0("Figures reflect all known COVID-19 tests administered as of ",updateTimeFormat," on ",updateDateFormat,", not including antibody testing.")
#   )
#   
#   dw_publish_chart(
#     chart_id = chartIDs[["tests-map"]],
#     api_key = apikey,
#     return_urls = TRUE
#   )  
# }
