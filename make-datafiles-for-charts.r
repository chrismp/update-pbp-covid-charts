args <- commandArgs(trailingOnly=TRUE)

options(scipen = 999)

pkgs <- c(
  "devtools",
  "dplyr",
  "forcats",
  "reshape2",
  "zoo",
  "janitor"
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
library(janitor)

func.SummCases <- function(gdf){
  return(
    gdf %>%
      summarise(
        `Confirmed cases` = n()
      )
  )
}

func.SummAgeRelatedDFs <- function(p){
  df <- func.SummCases(
    group_by(
      .data = p,
      `Age group`
    )
  )
  df[is.na(df)] <- "Unknown"
  df <- df %>% 
    group_by(`Age group`) %>% 
    summarise(`Confirmed cases` = sum(`Confirmed cases`))
  
  return(df)
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
  
  # positives$caseDate <- as.Date(x = positives$Case1, format = "%m/%d/%Y")
  correctUnixTimePositiveCases <- (positives$Case1/1000)
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
  
  # Daily cases in each county
  dailyCounties <- "daily-fl-counties"
  chartDFs[[dailyCounties]] <- positives %>%
    group_by(County,caseDate) %>%
    summarise(`Daily confirmed cases` = n()) %>%
    dcast(County ~ caseDate)

  
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
  positives$`Age group` <- ifelse(
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
                  test = positives$Age >= 0,
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
  
  casesByAge <- func.SummAgeRelatedDFs(positives)

  hospByAge <- func.SummAgeRelatedDFs(
    filter(
      .data = positives,
      grepl(
        pattern = "yes",
        x = Hospitalized,
        ignore.case = T
      )
    )
  ) %>%
    rename(c("Hospitalizations"="Confirmed cases"))
  
  deathsbyAge <- func.SummAgeRelatedDFs(
    filter(
      .data = positives,
      grepl(
        pattern = "yes",
        x = Died,
        ignore.case = T
      )
    )
  ) %>%
    rename(c("Deaths"="Confirmed cases"))
  
  ag <- "age-group"
  chartDFs[[ag]] <- Reduce(function(x,y) merge(x,y,all=T,by="Age group"), list(casesByAge,hospByAge,deathsbyAge))
  
  
  # Travel related
  chartDFs[["travel-related"]] <- func.SummCases(
    group_by(
      .data = positives,
      Travel_related
    )
  )
  
  
  # Current deaths, resident v non-resident
  cd <- "current-deaths"
  chartDFs[[cd]] <- filter(
    .data = positives,
    tolower(Died) == "yes"
  ) %>%
    group_by(
      County,
      Jurisdiction
    ) %>%
    summarise(
      Deaths = n()
    ) %>%
    dcast(County ~ Jurisdiction) %>% 
    rowwise() %>%
    mutate(
      Residents = sum(
        `FL resident`,
        `Not diagnosed/isolated in FL`,
        na.rm = T
      )
    ) %>%
    mutate(
      Total = sum(
        Residents,
        `Non-FL resident`,
        na.rm = T
      )
    ) %>%
    rename(
      `Non-residents` = `Non-FL resident`
    )

  chartDFs[[cd]] <- chartDFs[[cd]][, -which(names(chartDFs[[cd]]) %in% c("FL resident","Not diagnosed/isolated in FL"))] %>%
    adorn_totals(
      where = c("row"),
      na.rm = T,
      name = "Statewide"
    )
  
  chartDFs[[cd]] <- chartDFs[[cd]][,c("County","Total","Residents","Non-residents")]
}


# Create dataframes for COVID19 testing data
if(args[5]==0){
  tests <- read.csv(
    file = args[2],
    stringsAsFactors = F
  )
  
  tests[tests$County_1=="Dade",]$County_1 <- "Miami-Dade"
  tests[tests$County_1=="Desoto",]$County_1 <- "DeSoto"
  
  # Tests per capita in each county
  testRateName <- "testing-rate"
  chartDFs[[testRateName]] <- merge(
    x = tests,
    y = countyPops,
    by.x = "County_1",
    by.y = "County"
  )
  chartDFs[[testRateName]]$`Tests per 100 people` <- chartDFs[[testRateName]]$PUIsTotal / chartDFs[[testRateName]]$`2019 population estimate` * 100

  rawTestFilenames <- list.files(args[7])
  
  # Testing per day
  testingByDate <- data.frame(
    DownloadTimestampUnix = numeric(),
    Date = as.Date(character()),
    CumulativePeopleTested = numeric(),
    CumulativeTests = numeric(),
    CumulativePositives = numeric(),
    CumulativeNonPositives = numeric()
  )
  
  for (i in 1:length(rawTestFilenames)) {
    fname <- rawTestFilenames[[i]]
    
    timeString <- gsub(
      pattern = "FL-|.csv",
      replacement = '',
      x = fname
    )
    
    timeDate <- as.Date(
      x = timeString,
      format = "%Y-%m-%d_%H%M%S"
    )
    
    rawTestFile <- read.csv(
      file = paste0(args[7],'/',fname)
    )
    
    testingByDate[i, ] <- c(
      as.numeric(as.POSIXct(timeString,format="%Y-%m-%d_%H%M%S")),
      as.character(timeDate),
      sum(rawTestFile$PUIsTotal),
      sum(rawTestFile$T_total),
      sum(rawTestFile$T_positive),
      sum(rawTestFile$T_total) - sum(rawTestFile$T_positive)
    )
  }
  
  testingByDate <- testingByDate %>% 
    group_by(Date) %>%
    top_n(1,DownloadTimestampUnix)
  
    
  testingByDate$DailyPeopleTested <- as.numeric(testingByDate$CumulativePeopleTested) - as.numeric(lag(testingByDate$CumulativePeopleTested,1)) 
  testingByDate$DailyPositives <- as.numeric(testingByDate$CumulativePositives) - as.numeric(lag(testingByDate$CumulativePositives,1))
  testingByDate$DailyTests <- as.numeric(testingByDate$CumulativeTests) - as.numeric(lag(testingByDate$CumulativeTests,1))
  # testingByDate$DailyPercentPeoplePositive <- testingByDate$DailyPositives / testingByDate$DailyPeopleTested
  testingByDate$DailyPercentTestsPositive <- testingByDate$DailyPositives / testingByDate$DailyTests
  testingByDate$DailyNonPositiveTests <- testingByDate$DailyTests - testingByDate$DailyPositives
  
  testsDateName <- "tests-by-date"
  chartDFs[[testsDateName]] <- testingByDate
  
  testsDateStackedBar <- "tests-by-date-stacked-bar"
  x <- chartDFs[[testsDateName]] %>%
    dcast(DailyPositives ~ Date)
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