# Uses CALPADS and TOMS files to estimate Dashboard rates before their release
# Based on files from CERS-CAASPP and ChronicSCESD projects

# Toms > CAASPP Student Score Data File > By Tested LEA
# TOMS > Summatuve ELPAC and Summative Alternative ELPAC Student Score Data File
# "14.2_StudentAbsencesStudentList.xlsx" and "8.1_StudentProfileList(EOY3).xlsx
# "7.12_IncidentResultsStudentList.csv" and "8.1_StudentProfileList

library(tidyverse)
library(here)
library(MCOE)
library(janitor)
library(readxl)
library(ggthemes)
library(googlesheets4)

options(scipen=999)

con <- mcoe_sql_con()

# 2022 Google Sheet 
# sheet <- "https://docs.google.com/spreadsheets/d/1iS2Sd37hU7LYzakI2fbiotnzYn60cVp0d0pMB9k6zXk/edit#gid=0"

# 2023 Google Sheet
# sheet <- "https://docs.google.com/spreadsheets/d/1E7x2W-bWkZGenZTPVmlyGSl0LQPfrHc2s8ZLILKUOGw/edit#gid=0"

# 2024 Google Sheet
sheet <- "https://docs.google.com/spreadsheets/d/1RSRPRRqcS8tOg7-uXG6dFgoHwn4dcAitBjfrnhkFYHA/edit?gid=0#gid=0"



dash <- tbl(con,"DASH_ALL") %>%
    filter(countyname == "Monterey",
         #  rtype == "D",
           #        indicator == "ela" | indicator == "math",
           reportingyear == "2023") %>%
    collect()  %>%
    mutate(Group = case_match(studentgroup,
                              "HOM" ~ "Homeless",
                              "SWD" ~ "Students with \nDisabilities",
                              "SED" ~ "Socio-Economically \nDisadvantaged",
                              "HI" ~ "Latino",
                              "EL" ~ "English \nLearner",
                              "AS" ~ "Asian",
                              "FI" ~ "Filipino",
                              "WH" ~ "White",
                              "ALL" ~ "All",
                              "AA" ~ "Black/\nAfrican Am",
                              "PI" ~ "Pacific Islander",
                              "MR" ~ "Multiple \nRaces",
                              .default = studentgroup
    ))



save.folder <- "mpusd"
