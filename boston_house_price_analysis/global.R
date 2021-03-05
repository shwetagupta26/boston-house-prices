
#setting the working directory
#setwd("~/shweta/2019_Fall/FallA_2019/Communicate Visual Data Analysis/Week 5/practice/Final_code")
# loading the libraries
library('shiny')
library(ggplot2)
library(tidyverse)
#install.packages("sqldf")
library(sqldf)

# supressing the scientifc notation
options(scipen=9999)

# reading the input csv
housing <- read.csv('House_Price_data.csv', header = TRUE
                   , stringsAsFactors = FALSE)
#read.csv("~/shweta/2019_Fall/FallA_2019/Communicate Visual Data Analysis/Week 5/practice/ShinyApp/House_Price_data.csv", header=FALSE, stringsAsFactors = FALSE)

# rename the coloumns to lowecase
names(housing) <- tolower(names(housing))

# combining yrsold and month sold and converting it to date format
# Since in R dat format requries, date, defaulting the date as 01.

housing$datesold <- as.Date(paste(housing$yrsold,as.numeric(housing$mosold), "01", sep="-"),
                            format = "%Y-%m-%d")
