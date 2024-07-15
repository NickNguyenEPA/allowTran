# library(data.table)
# library(httr)
# library(htmltools)
# library(jsonlite)
# library(tidyverse)
# library(lubridate)
# library(zoo)


#
# res <- GET(paste0(allowanceUrl, "&transactionBeginDate=1994-01-01&transactionEndDate=2024-07-15"))
# allowTrans <- fromJSON(rawToChar(res$content))
# allowTrans <- allowTrans %>% mutate_all(~replace(., is.na(.), "NULL"))
# allowTrans <- allowTrans %>% mutate(across(everything(), ~gsub("[[:punct:]]", " ", .x)))
# allowTrans$transactionDate <- as.Date(allowTrans$transactionDate, format = "%Y %m %d")
# allowTrans$totalBlock <- as.numeric(allowTrans$totalBlock)
# save(allowTrans,file="./data/allowanceTransaction/allowTrans.RData")

# allowTrans <- read.csv("./data/allowanceTransaction.csv")
# print(fromJSON(rawToChar(res$content))$error$message)
#anyNA(allowTrans$sellOwner)

# glimpse(allowTrans)

# Load packages ----------------------------------------------------------------

# Package names
packages <- c("shiny", "ggplot2", "tidyquant", "dplyr", "jsonlite", "readr", "httr", "htmltools", "shinyWidgets")

# Install packages not yet installed
install.packages(packages[!(packages %in% rownames(installed.packages()))])

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

#load data
load("./data/allowanceTransaction/allowTrans.RData")

#min and max for date range input value
min_date <- min(allowTrans$transactionDate, na.rm = TRUE)
max_date <- max(allowTrans$transactionDate, na.rm = TRUE)


#convert as.Ddate for transaction date

current_date <- Sys.Date()


#prepare for API call
apiKEY <- read_lines("./data/apiKey/api_key.txt")

apiUrlBase <- "https://api.epa.gov/easey"

allowanceUrl <- paste0(apiUrlBase,"/streaming-services/allowance-transactions?API_KEY=",apiKEY)



