library(httr)
library(readr)

kaggle.api <- "https://www.kaggle.com/api/v1/datasets/download/zusmani/us-mass-shootings-last-50-years/Mass%20Shootings%20Dataset.csv"
kaggle.auth <- function() {
  source("credentials.R")
  httr::authenticate(username, key)
}
response <- httr::GET(kaggle.api, kaggle.auth())
massshooting.raw <- read_csv(response$content)
