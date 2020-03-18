args = commandArgs(trailingOnly=TRUE)

pkgs <- c(
  "devtools",
  "dplyr",
  "forcats"
)

for(x in pkgs){
  if(!is.element(x, installed.packages()[,1])){
    install.packages(x, repos="http://cran.fhcrc.org")
  } else {
    print(paste(x, " library already installed"))
  }
}

#devtools::install_github("munichrocker/DatawRappr")

#library(DatawRappr)
library(dplyr)
library(forcats) 

func.SummCases <- function(gdf){
  return(
    gdf %>%
      summarise(
        `Confirmed cases` = n()
      )
  )
}

# Read latest CSV of COVID19 data and create CSVs that will be uploaded to DataWrapper
df <- read.csv(args[1])

df$AgeGroup <- ifelse(
  test = df$age >= 80,
  yes = "80 or older",
  no = ifelse(
    test = df$age >= 70,
    yes = "70-79",
    no = ifelse(
      test = df$age >= 60,
      yes = "60-69",
      no = ifelse(
        test = df$age >= 50,
        yes = "50-59",
        no = ifelse(
          test = df$age >= 40,
          yes = "40-49",
          no = ifelse(
            test = df$age >= 30,
            yes = "30-39",
            no = ifelse(
              test = df$age >= 18,
              yes = "18-29",
              no = ifelse(
                test = df$age > 0,
                yes = "17 or younger",
                no = "Unknown"
              )
            )
          )
        )
      )
    )
  )
)

df$TravelStatus <- ifelse(
  test = tolower(substr(x = df$travel,  start = 1,  stop = 3)) == "yes",
  yes = "Yes",
  no = ifelse(
    test = tolower(df$travel)=="no",
    yes = "No",
    no = ifelse(
      test = tolower(df$travel)=="unknown",
      yes = "Unknown",
      no = NA
    )
  )
)

chartDFs <- list()

chartDFs[["county"]] <- func.SummCases(
    group_by(
    .data = df,
    county 
  )
)

chartDFs[["sex"]] <- func.SummCases(
  group_by(
    .data = df,
    sex = fct_explicit_na(gender, na_level = "Unknown")
  ) 
)

chartDFs[["age-group"]] <- func.SummCases(
  group_by(
    .data = df,
    AgeGroup
  )
) 

chartDFs[["travel-status"]] <- func.SummCases(
  group_by(
    .data = df,
    TravelStatus
  )
)


# Write files
out <- paste0(args[2],"/datawrapper")
dir.create(out)

for (i in 1:length(chartDFs)) {
  write.csv(
    x = chartDFs[[i]],
    file = paste0(out,'/',names(chartDFs)[i],".csv"),
    na = '',
    row.names = F
  )
}


# # Update charts
# dwapi <- Sys.getenv("DATAWRAPPER_API")
# datawrapper_auth(dwapi)
# 
# dw_edit_chart(
#   chart_id = "BSF3m", # age groups
#   api_key = dwapi,
#   data = chartDFs[["age group"]]
# )












