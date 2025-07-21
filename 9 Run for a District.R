
# Used to run for a district for all indicators available 

# Set district name for folder to same images 
save.folder <- "soledad"

print(save.folder)

### Load TOMS CAASPP results ----


alisal.24 <- read_xlsx(here("data","alisal", "27659610000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                       skip = 1) %>%
  use.TOMS()

alisal.25 <- read_xlsx(here("data","alisal", "2025", "27659610000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2025.xlsx"),
                       skip = 1) %>%
  use.TOMS()




chualar.24 <- read_xlsx(here("data","chualar", "27659950000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                        skip = 1) %>%
  use.TOMS()



gonz.24 <- read_xlsx(here("data","gonzales", "27754730000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                     skip = 1) %>%
  use.TOMS()


greenfield.24 <- read_xlsx(here("data","greenfield", "27660350000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                           skip = 1) %>%
  use.TOMS()

greenfield.25 <- read_xlsx(here("data","greenfield", "2025" ,"27660350000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2025.xlsx"),
                           skip = 1) %>%
  use.TOMS()


kingcity.24 <- read_xlsx(here("data","king city", "27660500000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                         skip = 1) %>%
  use.TOMS()

kingcity.25 <- read_xlsx(here("data","king city", "2025", "27660500000000_CAASPP_Student_Score_Data_File_EnrolledStudentScoreData_2025.xlsx"),
                         skip = 1) %>%
  use.TOMS()



lagunita.24 <- read_xlsx(here("data","lagunita", "27660760000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                         skip = 1) %>%
  use.TOMS()



mcoe.24 <- read_xlsx(here("data","mcoe", "27102720000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                     skip = 1)
mcoe.24 <- mcoe.24 %>%
  filter(!str_detect(CALPADSSchoolName,"Special"),
         !str_detect(CALPADSSchoolName,"Home")) %>%
  use.TOMS()


mission.24 <- read_xlsx(here("data","mission", "27660840000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                        skip = 1) %>%
  use.TOMS()



mpusd.25 <- read_xlsx(here("data","mpusd", "2025" ,"27660920000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2025.xlsx"),
                      skip = 1)
mpusd.25 <- use.TOMS(mpusd.25)


nmcusd.24 <- read_xlsx(here("data","nmcusd", "27738250000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                       skip = 1)
nmcusd.24 <- use.TOMS(nmcusd.24)


nmcusd.25 <- read_xlsx(here("data","nmcusd", "2025" ,"NMCUSD_CAASPP_Student_ScoreData_2025_unofficial.xlsx"),
                       skip = 1)
nmcusd.25 <- use.TOMS(nmcusd.25)


pg.24 <- read_xlsx(here("data","pg", "27661340000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                   skip = 1) %>%
  use.TOMS()



scesd.24 <- read_xlsx(here("data","scesd", "27661420000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                      skip = 1) %>%
  use.TOMS()

scesd.25 <- read_xlsx(here("data","scesd","2025" , "27661420000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2025.xlsx"),
                      skip = 1) %>%
  use.TOMS()



suhsd.24 <- read_xlsx(here("data","suhsd", "27661590000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                      skip = 1) %>%
  use.TOMS()

suhsd.24 <- read_csv(here("data","suhsd", "27661590000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024_1011.csv")) %>%
  use.TOMS()


sanantonio.24 <- read_xlsx(here("data","san antonio", "27661670000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                           skip = 1) %>%
  use.TOMS()


sanardo.24 <- read_xlsx(here("data","san ardo", "27661750000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                        skip = 1) %>%
  use.TOMS()


santarita.24 <- read_xlsx(here("data","santa rita", "27661910000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                          skip = 1) %>%
  use.TOMS()

santarita.25 <- read_xlsx(here("data","santa rita","2025" , "27661910000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2025.xlsx"),
                          skip = 1) %>%
  use.TOMS()


soledad.24 <- read_xlsx(here("data","soledad", "27754400000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                        skip = 1) %>%
  use.TOMS()

soledad.25 <- read_xlsx(here("data","soledad", "2025", "27754400000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2025.xlsx"),
                        skip = 1) %>%
  use.TOMS()


somoco.24 <- read_xlsx(here("data","somoco", "CAASPP2024.xlsx")) %>%
  use.TOMS()

spreck.24 <- read_xlsx(here("data","spreckels", "27662250000000_CAASPP_Student_Score_Data_File_EnrolledStudentScoreData_2024.xlsx"),
                       skip = 1)
spreck.24 <- use.TOMS(spreck.24)


wash.24 <- read_xlsx(here("data","washington", "27662330000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                          skip = 1) %>%
    use.TOMS()



### Load TOMS ELPAC results -----

alisal.elpac.24 <- read_xlsx(here("data","alisal","27659610000000_Summative_ELPAC_and_Summative_Alternate_ELPAC_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                             skip = 1
)


alisal.elpac.25 <- read_xlsx(here("data","alisal", "2025" ,"27659610000000_Summative_ELPAC_and_Summative_Alternate_ELPAC_Student_Score_Data_File_TestedStudentScoreData_2025.xlsx"),
                             skip = 1)

chualar.elpac.24 <- read_xlsx(here("data","chualar","27659950000000_Summative_ELPAC_and_Summative_Alternate_ELPAC_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                              skip = 1
)


gonz.elpac.24 <- read_xlsx(here("data","gonzales","27754730000000_Summative_ELPAC_and_Summative_Alternate_ELPAC_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                           skip = 1
)


greenfield.elpac.24 <- read_xlsx(here("data","greenfield","27660350000000_Summative_ELPAC_and_Summative_Alternate_ELPAC_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                                 skip = 1
)


kingcity.elpac.24 <- read_xlsx(here("data","king city","27660500000000_Summative_ELPAC_and_Summative_Alternate_ELPAC_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                             skip = 1
)


kingcity.elpac.25 <- read_xlsx(here("data","king city","2025" ,"27660500000000_Summative_ELPAC_and_Summative_Alternate_ELPAC_Student_Score_Data_File_EnrolledStudentScoreData_2025.xlsx"),
                               skip = 1
)

mpusd.elpac.24 <- read_xlsx(here("data","mpusd","27660920000000_Summative_ELPAC_and_Summative_Alternate_ELPAC_Student_Score_Data_File_TestedStudentScoreData_2024 (1).xlsx"),
                            skip = 1
)

mpusd.elpac.25 <- read_xlsx(here("data","mpusd","2025" ,"27660920000000_Summative_ELPAC_and_Summative_Alternate_ELPAC_Student_Score_Data_File_TestedStudentScoreData_2025.xlsx"),
                            skip = 1
)


nmcusd.elpac.24 <- read_xlsx(here("data","nmcusd","27738250000000_Summative_ELPAC_and_Summative_Alternate_ELPAC_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                             skip = 1
)





scesd.elpac.24 <- read_xlsx(here("data","scesd","27661420000000_Summative_ELPAC_and_Summative_Alternate_ELPAC_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                            skip = 1
)

scesd.elpac.25 <- read_xlsx(here("data","scesd", "2025" , "27661420000000_Summative_ELPAC_and_Summative_Alternate_ELPAC_Student_Score_Data_File_TestedStudentScoreData_2025.xlsx"),
                              skip = 1
)

suhsd.elpac.24 <- read_xlsx(here("data","suhsd","27661590000000_Summative_ELPAC_and_Summative_Alternate_ELPAC_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                            skip = 1
)

santarita.elpac.24 <- read_xlsx(here("data","santa rita","27661910000000_Summative_ELPAC_and_Summative_Alternate_ELPAC_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                            skip = 1
)

soledad.elpac.24 <- read_xlsx(here("data","soledad","27754400000000_Summative_ELPAC_and_Summative_Alternate_ELPAC_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                              skip = 1
)

somoco.elpac.24 <- read_xlsx(here("data","somoco","ELPAC2024.xlsx")
)


wash.elpac.24 <- read_xlsx(here("data","washington","27662330000000_Summative_ELPAC_and_Summative_Alternate_ELPAC_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                                skip = 1
)


### Load CALPADS files ------


greenfield.abs.24 <- read_csv(here("data", "greenfield" , "14.2 StudentAbsencesStudentList.csv"))
greenfield.demo.24 <- read_csv(here("data", "greenfield" , "8.1_StudentProfileList(EOY3).csv"))
greenfield.sus.24 <- read_csv(here("data", "greenfield" , "7.12 IncidentResultsStudentList.csv"))

greenfield.abs.25 <- read_csv(here("data", "greenfield" , "2025" , "14.2_StudentAbsencesStudentList.csv"))
greenfield.demo.25 <- read_csv(here("data", "greenfield" ,"2025" , "8.1_StudentProfileList(EOY3).csv"))
greenfield.sus.25 <- read_csv(here("data", "greenfield" , "2025" ,"7.12_IncidentResultsStudentList.csv"))


kingcity.abs.25 <- read_csv(here("data", "king city" , "2025" , "14.2_StudentAbsencesStudentList.csv"))
kingcity.demo.25 <- read_csv(here("data", "king city" ,"2025" , "8.1_StudentProfileList(EOY3) (1).csv"))
kingcity.sus.25 <- read_csv(here("data", "king city" , "2025","7.12_IncidentResultsStudentList (1).csv"))



mpusd.abs.24 <- read_csv(here("data", "mpusd" , "14.2_StudentAbsencesStudentList.InReviewUncertified_20240715.csv"))
mpusd.demo.24 <- read_csv(here("data", "mpusd" , "8.1_StudentProfileList(EOY3).InReviewUncertified_20240715.csv"))
mpusd.sus.24 <- read_csv(here("data", "mpusd" , "7.12_IncidentResultsStudentList.InReviewUncertified_20240715.csv"))
mpusd.2024.grad.24 <- read_csv(here("data", "mpusd" , "15.2 - Cohort Outcome - Student Details 2023-2024.csv"))
mpusd.list.grad.24 <- read_csv(here("data", "mpusd" , "1.23_GraduatesandCompletersStudentList.csv"))
mpusd.2023.grad.24 <- read_csv(here("data", "mpusd" , "15.2 - Cohort Outcome - Student Details 2022-2023.csv"))

mpusd.abs.25 <- read_csv(here("data", "mpusd" , "2025", "14.2_StudentAbsencesStudentList.csv"))
mpusd.demo.25 <- read_csv(here("data", "mpusd" ,"2025", "8.1_StudentProfileList(EOY3).csv"))
mpusd.sus.25 <- read_csv(here("data", "mpusd" ,"2025", "7.12_IncidentResultsStudentList.csv"))
mpusd.2025.grad.25 <- read_csv(here("data", "mpusd" ,"2025", "15.2 - Cohort Outcome - Student Details_2025.csv"))
mpusd.list.grad.25 <- read_csv(here("data", "mpusd" ,"2025", "1.23_GraduatesandCompletersStudentList.csv"))
mpusd.2024.grad.25 <- read_csv(here("data", "mpusd" ,"2025", "15.2 - Cohort Outcome - Student Details_2024.csv"))


spreck.abs.24 <- read_csv(here("data", "spreckels" , "14.2_StudentAbsencesStudentList.csv"))
spreck.demo.24 <- read_csv(here("data", "spreckels" , "8.1_StudentProfileList(EOY3).csv"))
spreck.sus.24 <- read_csv(here("data", "spreckels" , "7.12_IncidentResultsStudentList.csv"))


nmcusd.abs.24 <- read_csv(here("data", "nmcusd" ,  "14.2_StudentAbsencesStudentList.csv"))
nmcusd.demo.24 <- read_csv(here("data", "nmcusd" , "8.1_StudentProfileList(EOY3).csv"))
nmcusd.sus.24 <- read_csv(here("data", "nmcusd" , "7.12_IncidentResultsStudentList.csv"))

nmcusd.abs.25 <- read_csv(here("data", "nmcusd" , "2025" , "14.2_StudentAbsencesStudentList.csv"))
nmcusd.demo.25 <- read_csv(here("data", "nmcusd" ,"2025" , "8.1_StudentProfileList(EOY3).csv"))
nmcusd.sus.25 <- read_csv(here("data", "nmcusd" , "2025","7.12_IncidentResultsStudentList.csv"))
nmcusd.2025.grad.25 <- read_csv(here("data", "nmcusd" ,"2025" ,"15.2 - Cohort Outcome - Student Details.csv"))
nmcusd.list.grad.25 <- read_csv(here("data", "nmcusd" ,"2025", "1.23_GraduatesandCompletersStudentList.csv"))
nmcusd.2024.grad.25 <- read_csv(here("data", "nmcusd" ,"2025", "15.2 - Cohort Outcome - Student Details 2023-2024.csv"))

sanardo.abs.24 <- read_csv(here("data", "san ardo" , "14.2_StudentAbsencesStudentList.csv"))
sanardo.demo.24 <- read_csv(here("data", "san ardo" , "8.1_StudentProfileList(EOY3).csv"))
sanardo.sus.24 <- read_csv(here("data", "san ardo" , "7.12_IncidentResultsStudentList.csv"))


mcoe.abs.24 <- read_csv(here("data", "mcoe" , "14.2.csv"))
mcoe.demo.24 <- read_csv(here("data", "mcoe" , "8.1.csv"))
mcoe.sus.24 <- read_csv(here("data", "mcoe" , "7.12.csv"))
mcoe.grad.24 <- read_csv(here("data", "mcoe" , "15.2.csv"))
mcoe.5th.grad.24 <- read_csv(here("data", "mcoe" , "1.23_GraduatesandCompletersStudentList.csv"))
mcoe.2023.grad.24 <- read_csv(here("data", "mcoe" , "15.2 - Cohort Outcome - Student Details (2023).csv"))




sanantonio.abs.24 <- read_csv(here("data", "san antonio" , "S_14.2_StudentAbsencesStudentList.csv"))
sanantonio.demo.24 <- read_csv(here("data", "san antonio" , "S_8.1_StudentProfileList(EOY3).csv"))
sanantonio.sus.24 <- read_csv(here("data", "san ardo" , "7.12_IncidentResultsStudentList.csv"))



soledad.abs.24 <- read_csv(here("data", "soledad" , "14.2_StudentAbsencesStudentList.csv"))
soledad.demo.24 <- read_csv(here("data", "soledad" , "8.1_StudentProfileList(EOY3).csv"))
soledad.sus.24 <- read_csv(here("data", "soledad" , "7.12_IncidentResultsStudentList.csv"))
soledad.2024.grad.24 <- read_csv(here("data", "soledad" , "15.2 - Cohort Outcome - Student Details (2024).csv"))
soledad.list.grad.24 <- read_csv(here("data", "soledad" , "1.23_GraduatesandCompletersStudentList.csv"))
soledad.2023.grad.24 <- read_csv(here("data", "soledad" , "15.2 - Cohort Outcome - Student Details (2023).csv"))



gonz.abs.24 <- read_csv(here("data", "gonzales" , "14.2_StudentAbsencesStudentList.csv"))
gonz.demo.24 <- read_csv(here("data", "gonzales" , "8.1_StudentProfileList(EOY3).csv"))
gonz.sus.24 <- read_csv(here("data", "gonzales" , "7.12_IncidentResultsStudentList.csv"))
gonz.2024.grad.24 <- read_csv(here("data", "gonzales" , "15.2 - Cohort Outcome - Student Details (2024).csv"))
gonz.list.grad.24 <- read_csv(here("data", "gonzales" , "1.23_GraduatesandCompletersStudentList.csv"))
gonz.2023.grad.24 <- read_csv(here("data", "gonzales" , "15.2 - Cohort Outcome - Student Details (2023).csv"))


scesd.abs.24 <- read_csv(here("data", "scesd" , "14.2_StudentAbsencesStudentList 23-24.csv"))
scesd.demo.24 <- read_csv(here("data", "scesd" , "8.1_StudentProfileList(EOY3) 23-24.csv"))
scesd.sus.24 <- read_csv(here("data", "scesd" , "7.12_IncidentResultsStudentList 23-24.csv"))


alisal.abs.24 <- read_csv(here("data", "alisal" , "14.2_StudentAbsencesStudentList.csv"))
alisal.demo.24 <- read_csv(here("data", "alisal" , "8.1_StudentProfileList(EOY3).csv"))
alisal.sus.24 <- read_csv(here("data", "alisal" , "7.12_IncidentResultsStudentList.csv"))


santarita.abs.24 <- read_csv(here("data", "santa rita" , "14.2_StudentAbsencesStudentList.csv"))
santarita.demo.24 <- read_csv(here("data", "santa rita" , "8.1_StudentProfileList(EOY3).csv"))
santarita.sus.24 <- read_csv(here("data", "santa rita" , "7.12_IncidentResultsStudentList.csv"))


lagunita.abs.24 <- read_csv(here("data", "lagunita" , "14.2_StudentAbsencesStudentList.csv"))
lagunita.demo.24 <- read_csv(here("data", "lagunita" , "8.1_StudentProfileList.csv"))
lagunita.sus.24 <- read_csv(here("data", "lagunita" , "7.12_IncidentResultsStudentList.csv"))


chualar.abs.24 <- read_csv(here("data", "chualar" , "14.2_StudentAbsencesStudentList.csv"))
chualar.demo.24 <- read_csv(here("data", "chualar" , "8.1_StudentProfileList(EOY3).csv"))
chualar.sus.24 <- read_csv(here("data", "chualar" , "7.12_IncidentResultsStudentList.csv"))


somoco.demo.24 <- read_csv(here("data", "somoco" , "8.1_StudentProfileList(EOY3) 2023-2024.csv"))
somoco.sus.24 <- read_csv(here("data", "somoco" , "7.12_IncidentResultsStudentList 2023-2024.csv"))
somoco.2024.grad.24 <- read_csv(here("data", "somoco" , "15.2 - Cohort Outcome - Student Details 2023-2024.csv"))
somoco.list.grad.24 <- read_csv(here("data", "somoco" , "1.23_GraduatesandCompletersStudentList 2023-2024.csv"))
somoco.2023.grad.24 <- read_csv(here("data", "somoco" , "15.2 - Cohort Outcome - Student Details 2022-2023.csv"))



suhsd.abs.24 <- read_csv(here("data", "suhsd" , "14.2 Student Absences Student List_23-24.csv"))
suhsd.demo.24 <- read_csv(here("data", "suhsd" , "8.1_StudentProfileList(EOY3).csv")) 
suhsd.sus.24 <- read_csv(here("data", "suhsd" , "7.12 Incident Results Student List_23-24.csv"))
suhsd.2024.grad.24 <- read_csv(here("data", "suhsd" , "15.2 - Cohort Outcome - Student Details 23-24.csv"))
suhsd.list.grad.24 <- read_csv(here("data", "suhsd" , "1.23 Graduates and Completers Student List_23-24.csv"))
suhsd.2023.grad.24 <- read_csv(here("data", "suhsd" , "15.2 - Cohort Outcome - Student Details 22-23.csv"))





#### CAASPP Analysis ----


graph.wrap(mpusd.25)
graph.grid(mpusd.25)
save.overall(mpusd.25)
save.wrap(mpusd.25)
save.grid(mpusd.25)


passing.perc(mpusd.25)

dfs.w.change(mpusd.25, 27660920000000) # Remember to update the cds code

student.group.size(mpusd.25, limit.30 = TRUE) %>% print(n = 30)


pme2(mpusd.25,HispanicOrLatinoEthnicity)
pme2(mpusd.25,SWD)
pme2(mpusd.25,HOM)
 pme2(mpusd.25,SED)
 pme2(mpusd.25,Asian)
 pme2(mpusd.25,HawaiianOrOtherPacificIslander)
# pme2(mpusd.25, AmericanIndianorAlaskaNative)
 pme2(mpusd.25,Filipino)
 pme2(mpusd.25,BlackorAfricanAmerican)
 pme2(mpusd.25,White)
 pme2(mpusd.25,TwoorMoreRaces)
 pme2(mpusd.25,ELdash)
 pme2(mpusd.25,LTELdash)
 

 dfs2(mpusd.25,HispanicOrLatinoEthnicity)
 dfs2(mpusd.25,SWD)
 dfs2(mpusd.25,HOM)
 dfs2(mpusd.25 ,SED)
# dfs2(mpusd.25, AmericanIndianorAlaskaNative)
dfs2(mpusd.25,Filipino)
dfs2(mpusd.25,White)
dfs2(mpusd.25,TwoorMoreRaces)
 dfs2(mpusd.25,ELdash)
 dfs2(mpusd.25,LTELdash)
 dfs2(mpusd.25,Asian)
# dfs2(mpusd.25,MigrantStatus)
 dfs2(mpusd.25,BlackorAfricanAmerican)
 dfs2(mpusd.25,HawaiianOrOtherPacificIslander)
 

# Check out the sheet and make sure there are not duplicates or incompletes 


working <- read_sheet(ss = sheet,
                      sheet = "Distance from Standard Group") %>%
    mutate(Group = case_match(StudentGroup,
                              "HOM" ~ "Homeless",
                              "SWD" ~ "Students with \nDisabilities",
                              "SED" ~ "Socio-Economically \nDisadvantaged",
                              "AmericanIndianorAlaskaNative" ~ "American\nIndian/\nAlaska\nNative",
                              "HispanicOrLatinoEthnicity" ~ "Latino",
                              "ELdash" ~ "English \nLearner",
                              "LTELdash" ~ "Long Term\nEnglish\nLearner",
                              "BlackorAfricanAmerican" ~ "Black/\nAfrican Am",
                              "HawaiianOrOtherPacificIslander" ~ "Pacific Islander",
                              "TwoorMoreRaces" ~ "Multiple \nRaces",
                              
                              .default = StudentGroup
    ))

working.plus <- read_sheet(ss = sheet,
                           sheet = "Distance from Standard") %>%
    mutate(Group = "All")

working <- working %>%
    bind_rows(working.plus)



dfs.graph(dist = "mpusd.25",
          assessment = "ELA",
          dist.name = "Monterey Peninsula"
          )

dfs.graph(dist = "mpusd.25",
          assessment = "Math",
          dist.name = "Monterey Peninsula"
)

dfs.comp(dist = "mpusd.25",
         assessment = "ELA",
         dist.name = "Monterey Peninsula"
         ,         old.colors = TRUE)

dfs.comp(dist = "mpusd.25",
         assessment = "Math",
         dist.name = "Monterey Peninsula"
         ,         old.colors = TRUE)





school.split <-  soledad.25  %>%
    filter(str_detect(CALPADSDistrictName,"Soledad")) 


holder <-    school.split %>%
    # split(school.split$SchoolName) %>%
    split(school.split$CALPADSSchoolName) %>%
    map_df(~add.school.dfs(.)) %>%
    rename(#Group = students,
        DFS = mean.dist.stand,
        Test = Subject)  %>%
    mutate(Group = case_match(students,
                              "HOM" ~ "Homeless",
                              "SWD" ~ "Students with \nDisabilities",
                              "SED" ~ "Socio-Economically \nDisadvantaged",
                              "AmericanIndianorAlaskaNative" ~ "American\nIndian/\nAlaska\nNative",
                              "TwoorMoreRaces" ~ "Multiple \nRaces",
                              "HispanicOrLatinoEthnicity" ~ "Latino",
                              "ELdash" ~ "English Learner",
                              "LTELdash" ~ "Long Term\nEnglish\nLearner",
                              .default = students
    ))

    

school.list <- holder$CDS %>% unique()


for (i in 1:length(school.list)) {
    
    math.df <- dfs.comp.school(df = holder, cds = school.list[i], assessment = "Math", limit.case.count = TRUE, old.colors = TRUE) 
    
    dfs.comp.school.graph(math.df, old.colors = TRUE)
    dfs.school.graph(math.df)

    ela.df <-   dfs.comp.school(df = holder, cds = school.list[i], assessment = "ELA", limit.case.count = TRUE, old.colors = TRUE)

    dfs.comp.school.graph(ela.df, old.colors = TRUE)
    dfs.school.graph(ela.df)

}


##### ELPI ------



elpi.calc(mpusd.elpac.25 #%>%
          #   filter(str_detect(TestedSchoolName1,"Prune"))
          , "D"
)

elpi.calc(mpusd.elpac.25 #%>%
          #   filter(str_detect(TestedSchoolName1,"Prune"))
          , "D", "LTEL"
)

school.list <- mpusd.elpac.25 %>%
  filter(str_detect(CALPADSDistrictName,"Peninsula")) %>%
  select(TestedSchoolName1) %>%
  unlist() %>%
  unique()


for (i in school.list) {
    elpi.calc(mpusd.elpac.25%>%
                  filter(str_detect(TestedSchoolName1,i)),
              "S"
    )
    
  elpi.calc(mpusd.elpac.25%>%
              filter(str_detect(TestedSchoolName1,i)),
            "S", "LTEL"
  )
  
  
}







### Chronic -------




mpusd.abs.joint <- calpads.join(mpusd.abs.25, mpusd.demo.25)


chronic.group.rate.w.change(mpusd.abs.joint, EthnicityRace, 27660920000000)
chronic.group.rate.w.change(mpusd.abs.joint, Homeless, 27660920000000)
chronic.group.rate.w.change(mpusd.abs.joint, StudentswithDisabilities, 27660920000000)
chronic.group.rate.w.change(mpusd.abs.joint, EnglishLearner, 27660920000000)
chronic.group.rate.w.change(mpusd.abs.joint, LTEL, 27660920000000)
chronic.group.rate.w.change(mpusd.abs.joint, SocioEconomicallyDisadvantaged, 27660920000000)
chronic.group.rate.w.change(mpusd.abs.joint, All, 27660920000000)


working <- read_sheet(ss = sheet,
                      sheet = "Chronic Group") %>%
    filter(#StudentGroup != "N",
           #StudentGroup != "Missing",
           NumberStudents >= 30) 


chronic.dash.graph(
    dist = "mpusd.abs.joint",
    dist.name = "Monterey Peninsula"
    )


chronic.dash.comp(
  dist = "mpusd.abs.joint",
  dist.name = "Monterey Peninsula"
  ,old.colors = TRUE)




mpusd.abs.school.joint <- chr.joint.school(mpusd.abs.25, mpusd.demo.25, grade.filt = TRUE)



holder <- mpusd.abs.school.joint %>%
    # filter(str_detect(DistrictName,dist.name)) %>%
    split(.$SchoolName) %>%
    map_df(~add.school.car(.))  %>%
    mutate(Group = case_match(students,
                              "All" ~ "All",
                              "Homeless" ~ "Homeless",
                              "StudentswithDisabilities" ~ "Students with \nDisabilities",
                              "SocioEconomicallyDisadvantaged" ~ "Socio-Economically \nDisadvantaged",
                              "Hispanic" ~ "Latino",
                              "EnglishLearner" ~ "English \nLearner",
                              "LTEL" ~ "Long Term\nEnglish\nLearner",
                              
                              "Black/African Am" ~ "Black/\nAfrican Am",
                              "Nat Hwiin/Othr Pac Islndr" ~ "Pacific Islander",
                              "Multiple" ~ "Multiple \nRaces",
                              .default = students
    ))

chron.all.schools(mpusd.abs.school.joint , dist.cd = 66092, limit.case.cnt = TRUE, old.culrs = TRUE)


#### Suspension -----


# mcoe.sus.joint <- susp.df(mcoe.sus.24,mcoe.demo.24 %>% filter(str_detect(SchoolName,"Salin|Well")) )

mpusd.sus.joint <- susp.df(mpusd.sus.25,mpusd.demo.25 )

susp.group.rate.w.change(mpusd.sus.joint, EthnicityRace, 27660920000000)
susp.group.rate.w.change(mpusd.sus.joint, Homeless, 27660920000000)
susp.group.rate.w.change(mpusd.sus.joint, StudentswithDisabilities, 27660920000000)
susp.group.rate.w.change(mpusd.sus.joint, EnglishLearner, 27660920000000)
susp.group.rate.w.change(mpusd.sus.joint, LTEL, 27660920000000)
susp.group.rate.w.change(mpusd.sus.joint, SocioEconomicallyDisadvantaged, 27660920000000)
susp.group.rate.w.change(mpusd.sus.joint, All, 27660920000000)



working <- read_sheet(ss = sheet,
                      sheet = "Susp Group") %>%
    filter(#StudentGroup != "N",
           #StudentGroup != "Missing",
           NumberStudents >= 30) 


susp.dash.graph(
    dist = "mpusd.sus.joint",
    dist.name = "Monterey Peninsula"
)


susp.dash.comp(
  dist = "mpusd.sus.joint",
  dist.name = "Monterey Peninsula"
  ,old.colors = TRUE)



mpusd.sus.school.joint <-  susp.joint.school(mpusd.sus.25,mpusd.demo.25)

# soledad.sus.school.joint %>% 
#     filter(str_detect(SchoolName,"Soledad"),
#            # Grade %in% c("KN","1","2"),
#            # StudentswithDisabilities == "Yes"
#     ) %>%
#     add.school.susp()

# 
# mpusd.sus.school.joint %>%
#   #  filter(!str_detect(SchoolName, "Virtual")) %>%
#     susp.all.schools(dist.cd = 66092, limit.case.cnt = TRUE, old.culrs = FALSE )

susp.all.schools(mpusd.sus.school.joint , dist.cd = 66092, limit.case.cnt = TRUE, old.culrs = TRUE)

### Grad -------

grad.func(cohort.old = mpusd.2024.grad.25, cohort.new = mpusd.2025.grad.25, completer.list = mpusd.list.grad.25,
          level = "S") %>%
  grad.all.schools(dist.cd = 66092, limit.case.cnt = TRUE)


grad.func(cohort.old = mpusd.2024.grad.25, cohort.new = mpusd.2025.grad.25, completer.list = mpusd.list.grad.25,
          level = "D") %>%
  grad.comp(dist.code = 66092,  old.colors = TRUE) %>%
  grad.comp.school.graph(old.colors = TRUE, level = "D")


grad.func(cohort.old = mpusd.2024.grad.25, cohort.new = mpusd.2025.grad.25, completer.list = mpusd.list.grad.25,
          level = "D") %>%
  grad.comp(dist.code = 66092,  old.colors = TRUE) %>%
  grad.graph()


### End -------

