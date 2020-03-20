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

# Create CSVs for DataWrapper charts
df <- read.csv(args[1])

df$AgeGroup <- ifelse(
  test = df$Age >= 80,
  yes = "80 or older",
  no = ifelse(
    test = df$Age >= 70,
    yes = "70-79",
    no = ifelse(
      test = df$Age >= 60,
      yes = "60-69",
      no = ifelse(
        test = df$Age >= 50,
        yes = "50-59",
        no = ifelse(
          test = df$Age >= 40,
          yes = "40-49",
          no = ifelse(
            test = df$Age >= 30,
            yes = "30-39",
            no = ifelse(
              test = df$Age >= 18,
              yes = "18-29",
              no = ifelse(
                test = df$Age > 0,
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


chartDFs <- list()

chartDFs[["county"]] <- func.SummCases(
  group_by(
    .data = df,
    County 
  )
)


chartDFs[["sex"]] <- func.SummCases(
  group_by(
    .data = df,
    Sex = fct_explicit_na(Gender, na_level = "Unknown")
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
    Travel_related
  )
)


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


