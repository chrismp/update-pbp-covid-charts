args <- commandArgs(trailingOnly=TRUE)

options(scipen = 999)

pkgs <- c(
  "devtools",
  "dplyr",
  "forcats",
  "reshape2",
  "zoo"
)

for(x in pkgs){
  if(!is.element(x, installed.packages()[,1])){
    install.packages(x)
  } else {
    print(paste(x, " library already installed"))
  }
}

library(dplyr)
library(forcats)
library(reshape2)
library(zoo)

func.SummCases <- function(gdf){
  return(
    gdf %>%
      summarise(
        `Confirmed cases` = n()
      )
  )
}


countyPops <- read.csv(
  file = "source/2019-flbebr-county-pop-estimates.csv",
  check.names = F,
  stringsAsFactors = F
)

chartDFs <- list()

# Create dataframes for DataWrapper charts showing positive cases
if(args[4]==0){
  positives <- read.csv(
    file = args[1],
    stringsAsFactors = F
  )

  positives$County <- ifelse(is.na(positives$County),"Unknown",positives$County)
  positives[positives$County=="Dade",]$County <- "Miami-Dade"
  positives[positives$County=="Desoto",]$County <- "DeSoto"
  
  positives$SouthFLCounties <- ifelse(
    test = positives$County == "Miami-Dade" | positives$County == "Broward" | positives$County == "Palm Beach",
    yes = positives$County,
    no = "Rest of state"
  )
  
  correctUnixTimePositiveCases <- (positives$Case_/1000)
  positives$caseDate <- format(
    x = as.POSIXct(
      x = correctUnixTimePositiveCases,
      origin = "1970-01-01",
      tz = "EST"
    ),
    "%Y-%m-%d"
  ) 
  
  # statewide cumulative cases by date
  cByD <- "cases-by-date"
  chartDFs[[cByD]] <- func.SummCases(
    group_by(
      .data = positives,
      caseDate
    )
  )
  chartDFs[[cByD]][,"Cumulative cases"] <- cumsum(chartDFs[[cByD]]$`Confirmed cases`)
  
  # Cumulative cases in South FL by date
  sflC <- "south-fl-cumulative"
  chartDFs[[sflC]] <- filter(positives, SouthFLCounties != "Rest of state") %>%
    group_by(caseDate, SouthFLCounties) %>% 
    summarise(`Daily confirmed cases` = n()) %>%
    dcast(caseDate ~ SouthFLCounties)
  chartDFs[[sflC]][is.na(chartDFs[[sflC]])] <- 0
  chartDFs[[sflC]]$`Broward sum cases` <- cumsum(chartDFs[[sflC]]$Broward)
  chartDFs[[sflC]]$`Miami-Dade sum cases` <- cumsum(chartDFs[[sflC]]$`Miami-Dade`)
  chartDFs[[sflC]]$`Palm Beach sum cases` <- cumsum(chartDFs[[sflC]]$`Palm Beach`)

  
  # Hospitalizations statewide by date
  hosp <- "hospitalizations-by-date"
  chartDFs[[hosp]] <- group_by(
    .data = filter(
      .data = positives,
      Hospitalized %in% c("Yes","YES","yes")
    ),
    caseDate
  ) %>%
    summarise(
      Hospitalizations = n()
    )
  chartDFs[[hosp]][,"Cumulative hospitalizations"] <- cumsum(chartDFs[[hosp]]$Hospitalizations)
  
  # Add hospitalizations to cumulative cases data frame
  chartDFs[[cByD]] <- merge(
    x = chartDFs[[cByD]],
    y = chartDFs[[hosp]],
    by = "caseDate",
    all = T
  ) %>%
    mutate(
      `Cumulative hospitalizations` = na.locf(`Cumulative hospitalizations`)
    )
  
  chartDFs[[cByD]][is.na(chartDFs[[cByD]])] <- 0
  chartDFs[[cByD]]$`Rest of cases` <- chartDFs[[cByD]]$`Cumulative cases` - chartDFs[[cByD]]$`Cumulative hospitalizations`
  
  
  # South FL
  sflVFL <- "cases-by-date-SouthFL"
  chartDFs[[sflVFL]] <- func.SummCases(
    group_by(
      .data = filter(
        .data = positives,
        !is.na(caseDate)
      ),
      caseDate,
      SouthFLCounties
    )
  ) %>%
    dcast(SouthFLCounties ~ caseDate)
  
  chartDFs[[sflVFL]]$Order <- ifelse(
    test = chartDFs[[sflVFL]]$SouthFLCounties == "Rest of state",
    yes = 1,
    no = ifelse(
      test = chartDFs[[sflVFL]]$SouthFLCounties == "Miami-Dade",
      yes = 2,
      no = ifelse(
        test = chartDFs[[sflVFL]]$SouthFLCounties == "Broward",
        yes = 3,
        no = 4
      )
    )
  ) 
  chartDFs[[sflVFL]] <- chartDFs[[sflVFL]][order(chartDFs[[sflVFL]]$Order),]
  
  # Cases by county
  chartDFs[["county"]] <- func.SummCases(
    group_by(
      .data = positives,
      County 
    )
  )
  
  # Cases per capita by county
  positiveRateName <- "counties-positive-cases-rate"
  chartDFs[[positiveRateName]] <- merge(
    x = chartDFs[["county"]],
    y = countyPops,
    by = "County"
  )
  
  chartDFs[[positiveRateName]]$`Confirmed cases per 100,000 people` <- chartDFs[[positiveRateName]]$`Confirmed cases` / chartDFs[[positiveRateName]]$`2019 population estimate` * 100000
  
  # Gender
  chartDFs[["sex"]] <- func.SummCases(
    group_by(
      .data = positives,
      Sex = fct_explicit_na(Gender, na_level = "Unknown")
    ) 
  )
  
  # Age group
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
  
  ag <- "age-group"
  chartDFs[[ag]] <- func.SummCases(
    group_by(
      .data = positives,
      AgeGroup
    )
  )
  chartDFs[[ag]][is.na(chartDFs[[ag]])] <- "Unknown"
  chartDFs[[ag]] <- chartDFs[[ag]] %>% 
    group_by(AgeGroup) %>% 
    summarise(`Confirmed cases` = sum(`Confirmed cases`))
  
  
  # Travel related
  chartDFs[["travel-related"]] <- func.SummCases(
    group_by(
      .data = positives,
      Travel_related
    )
  )
}


# Create dataframes for COVID19 testing data
if(args[5]==0){
  tests <- read.csv(
    file = args[2],
    stringsAsFactors = F
  )
  
  tests[tests$County_1=="Dade",]$County_1 <- "Miami-Dade"
  tests[tests$County_1=="Desoto",]$County_1 <- "DeSoto"
  
  testRateName <- "testing-rate"
  chartDFs[[testRateName]] <- merge(
    x = tests,
    y = countyPops,
    by.x = "County_1",
    by.y = "County"
  )
  chartDFs[[testRateName]]$`Tests per 100,000 people` <- chartDFs[[testRateName]]$PUIsTotal / chartDFs[[testRateName]]$`2019 population estimate` * 100000
}


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
