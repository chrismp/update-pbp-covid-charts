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

chartDFs <- list()


# Create CSVs for DataWrapper charts showing positive cases
positives <- read.csv(
  file = args[1],
  stringsAsFactors = F
)

positives$AgeGroup <- ifelse(
  test = positives$Age >= 80,
  yes = "80 or older",
  no = ifelse(
    test = positives$Age >= 70,
    yes = "70-79",
    no = ifelse(
      test = positives$Age >= 60,
      yes = "60-69",
      no = ifelse(
        test = positives$Age >= 50,
        yes = "50-59",
        no = ifelse(
          test = positives$Age >= 40,
          yes = "40-49",
          no = ifelse(
            test = positives$Age >= 30,
            yes = "30-39",
            no = ifelse(
              test = positives$Age >= 18,
              yes = "18-29",
              no = ifelse(
                test = positives$Age > 0,
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

chartDFs[["county"]] <- func.SummCases(
  group_by(
    .data = positives,
    County 
  )
)

countyPops <- read.csv(
  file = "source/2019-flbebr-county-pop-estimates.csv",
  check.names = F,
  stringsAsFactors = F
)

positiveRateName <- "counties-positive-cases-rate"
chartDFs[[positiveRateName]] <- merge(
  x = chartDFs[["county"]],
  y = countyPops,
  by = "County"
)

chartDFs[[positiveRateName]]$`Confirmed cases per 100,000 people` <- chartDFs[[positiveRateName]]$`Confirmed cases` / chartDFs[[positiveRateName]]$`2019 population estimate` * 100000

chartDFs[["sex"]] <- func.SummCases(
  group_by(
    .data = positives,
    Sex = fct_explicit_na(Gender, na_level = "Unknown")
  ) 
)

chartDFs[["age-group"]] <- func.SummCases(
  group_by(
    .data = positives,
    AgeGroup
  )
) 

chartDFs[["travel-status"]] <- func.SummCases(
  group_by(
    .data = positives,
    Travel_related
  )
)


# Create CSVs for COVID19 testing data
tests <- read.csv(
  file = args[2],
  stringsAsFactors = F
)

tests[tests$County_1=="Dade",]$County_1 <- "Miami-Dade"

testRateName <- "testing-rate"
chartDFs[[testRateName]] <- merge(
  x = tests,
  y = countyPops,
  by.x = "County_1",
  by.y = "County"
)
chartDFs[[testRateName]]$`Tests per 100,000 people` <- chartDFs[[testRateName]]$PUIsTotal / chartDFs[[testRateName]]$`2019 population estimate` * 100000


# Write CSVs
out <- paste0(args[3])
dir.create(out)

for (i in 1:length(chartDFs)) {
  write.csv(
    x = chartDFs[[i]],
    file = paste0(out,'/',names(chartDFs)[i],".csv"),
    na = '',
    row.names = F
  )
}


