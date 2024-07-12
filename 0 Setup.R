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
