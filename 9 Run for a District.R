


# Set district name for folder to same images 
save.folder <- "soledad"


### Load files ------

# Load TOMS CAASPP results 
nmcusd.24 <- read_xlsx(here("data","nmcusd", "27738250000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                       skip = 1)
nmcusd.24 <- use.TOMS(nmcusd.24)


spreck.24 <- read_xlsx(here("data","spreckels", "27662250000000_CAASPP_Student_Score_Data_File_EnrolledStudentScoreData_2024.xlsx"),
                       skip = 1)
spreck.24 <- use.TOMS(spreck.24)


mcoe.24 <- read_xlsx(here("data","mcoe", "27102720000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                       skip = 1)
mcoe.24 <- mcoe.24 %>%
    filter(!str_detect(CALPADSSchoolName,"Special"),
           !str_detect(CALPADSSchoolName,"Home")) %>%
    use.TOMS()



sanardo.24 <- read_xlsx(here("data","san ardo", "27661750000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                     skip = 1) %>%
    use.TOMS()


mission.24 <- read_xlsx(here("data","mission", "27660840000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                        skip = 1) %>%
    use.TOMS()


pg.24 <- read_xlsx(here("data","pg", "27661340000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                        skip = 1) %>%
    use.TOMS()



lagunita.24 <- read_xlsx(here("data","lagunita", "27660760000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                   skip = 1) %>%
    use.TOMS()



kingcity.24 <- read_xlsx(here("data","king city", "27660500000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                         skip = 1) %>%
    use.TOMS()


sanantonio.24 <- read_xlsx(here("data","san antonio", "27661670000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                         skip = 1) %>%
    use.TOMS()

greenfield.24 <- read_xlsx(here("data","greenfield", "27660350000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                           skip = 1) %>%
    use.TOMS()





# Load TOMS ELPAC results 
nmcusd.elpac.24 <- read_xlsx(here("data","nmcusd","27738250000000_Summative_ELPAC_and_Summative_Alternate_ELPAC_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                             skip = 1
                             )

kingcity.elpac.24 <- read_xlsx(here("data","king city","27660500000000_Summative_ELPAC_and_Summative_Alternate_ELPAC_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                             skip = 1
)

greenfield.elpac.24 <- read_xlsx(here("data","greenfield","27660350000000_Summative_ELPAC_and_Summative_Alternate_ELPAC_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                               skip = 1
)



# Load CALPADS files 
mpusd.abs.24 <- read_csv(here("data", "mpusd" , "14.2_StudentAbsencesStudentList.InReviewUncertified_20240715.csv"))
mpusd.demo.24 <- read_csv(here("data", "mpusd" , "8.1_StudentProfileList(EOY3).InReviewUncertified_20240715.csv"))
mpusd.sus.24 <- read_csv(here("data", "mpusd" , "7.12_IncidentResultsStudentList.InReviewUncertified_20240715.csv"))




spreck.abs.24 <- read_csv(here("data", "spreckels" , "14.2_StudentAbsencesStudentList.csv"))
spreck.demo.24 <- read_csv(here("data", "spreckels" , "8.1_StudentProfileList(EOY3).csv"))
spreck.sus.24 <- read_csv(here("data", "spreckels" , "7.12_IncidentResultsStudentList.csv"))


nmcusd.abs.24 <- read_csv(here("data", "nmcusd" , "14.2_StudentAbsencesStudentList.csv"))
nmcusd.demo.24 <- read_csv(here("data", "nmcusd" , "8.1_StudentProfileList(EOY3).csv"))
nmcusd.sus.24 <- read_csv(here("data", "nmcusd" , "7.12_IncidentResultsStudentList.csv"))


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


greenfield.abs.24 <- read_csv(here("data", "greenfield" , "14.2 StudentAbsencesStudentList.csv"))
greenfield.demo.24 <- read_csv(here("data", "greenfield" , "8.1_StudentProfileList(EOY3).csv"))
greenfield.sus.24 <- read_csv(here("data", "greenfield" , "7.12 IncidentResultsStudentList.csv"))


soledad.abs.24 <- read_csv(here("data", "soledad" , "14.2_StudentAbsencesStudentList.csv"))
soledad.demo.24 <- read_csv(here("data", "soledad" , "8.1_StudentProfileList(EOY3).csv"))
soledad.sus.24 <- read_csv(here("data", "soledad" , "7.12_IncidentResultsStudentList.csv"))





#### CAASPP Analysis ----


graph.wrap(greenfield.24)

graph.grid(greenfield.24)

save.overall(greenfield.24)
save.wrap(greenfield.24)
save.grid(greenfield.24)


passing.perc(greenfield.24)

dfs(greenfield.24)

student.group.size(greenfield.24, limit.30 = TRUE) %>% print(n = 30)


 pme2(greenfield.24,HOM)
 pme2(greenfield.24,SWD)
 pme2(greenfield.24,SED)
 pme2(greenfield.24,ELdash)
pme2(greenfield.24,HispanicOrLatinoEthnicity)
# pme2(pg.24,Asian)
# pme2(greenfield.24,White)
#  pme2(pg.24,TwoorMoreRaces)


# dfs2(sanantonio.24,White)
dfs2(greenfield.24,ELdash)
#dfs2(sanardo.24,Asian)
# dfs2(nmcusd.24,MigrantStatus)
# dfs2(nmcusd.24,Filipino)
#dfs2(sanardo.24,TwoorMoreRaces)
# dfs2(nmcusd.24,BlackorAfricanAmerican)
# dfs2(nmcusd.24,HawaiianOrOtherPacificIslander)
dfs2(greenfield.24,HispanicOrLatinoEthnicity)
#  dfs2(nmcusd.24,SpecialEducationforTesting)
#  dfs2(nmcusd.24,EconomicDisadvantageTesting)
dfs2(greenfield.24 ,SED)
 dfs2(greenfield.24,HOM)
dfs2(greenfield.24,SWD)



# Check out the sheet and make sure there are not duplicates or incompletes 


working <- read_sheet(ss = sheet,
                      sheet = "Distance from Standard Group") %>%
    mutate(Group = case_match(StudentGroup,
                              "HOM" ~ "Homeless",
                              "SWD" ~ "Students with \nDisabilities",
                              "SED" ~ "Socio-Economically \nDisadvantaged",
                              "HispanicOrLatinoEthnicity" ~ "Latino",
                              "ELdash" ~ "English \nLearner",
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



dfs.graph(dist = "greenfield.24",
          assessment = "ELA",
          dist.name = "Greenfield")


dfs.graph(dist = "greenfield.24",
          assessment = "Math",
          dist.name = "Greenfield")



dfs.comp(dist = "greenfield.24",
         assessment = "ELA",
         dist.name = "Greenfield", 
old.colors = FALSE)


dfs.comp(dist = "greenfield.24",
         assessment = "Math",
         dist.name = "Greenfield",
old.colors = FALSE)






school.split <-  greenfield.24  %>%
    filter(str_detect(CALPADSDistrictName,"Greenfield")) 

school.split %>%
    split(school.split$CALPADSSchoolName) %>%
    map(~graph.wrap2(.))


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
                              "HispanicOrLatinoEthnicity" ~ "Latino",
                              "ELdash" ~ "English Learner",
                              .default = students
    ))






school.list <- holder$CDS %>% unique()


for (i in 1:length(school.list)) {
    
    math.df <- dfs.comp.school(df = holder, cds = school.list[i], assessment = "Math", limit.case.count = TRUE, old.colors = FALSE) 
    
    dfs.comp.school.graph(math.df, old.colors = FALSE)
    dfs.school.graph(math.df)

    #   ggsave(here("output",save.folder ,paste0(school.list[i], " - ","Math"," CAASPP Student Group Results 2023 and 2024 Comparison ", Sys.Date(),".png")), width = 8, height = 5)
    
    ela.df <-   dfs.comp.school(df = holder, cds = school.list[i], assessment = "ELA", limit.case.count = TRUE, old.colors = FALSE) 
    
    dfs.comp.school.graph(ela.df, old.colors = FALSE)
    dfs.school.graph(ela.df)

    #   ggsave(here("output",save.folder ,paste0(school.list[i], " - ","ELA"," CAASPP Student Group Results 2023 and 2024 Comparison ", Sys.Date(),".png")), width = 8, height = 5)
    
}


##### ELPI ------



elpi.calc(greenfield.elpac.24 #%>%
          #   filter(str_detect(TestedSchoolName1,"Prune"))
          , "D"
)


school.list <- unique(greenfield.elpac.24$TestedSchoolName1)

for (i in school.list) {
    elpi.calc(greenfield.elpac.24%>%
                  filter(str_detect(TestedSchoolName1,i)),
              "S"
    )
    
}


### Chronic -------




soledad.abs.joint <- calpads.join(soledad.abs.24, soledad.demo.24)

chronic.group.rate(soledad.abs.joint, EthnicityRace)
chronic.group.rate(soledad.abs.joint, Homeless)
chronic.group.rate(soledad.abs.joint, StudentswithDisabilities)
chronic.group.rate(soledad.abs.joint, EnglishLearner)
chronic.group.rate(soledad.abs.joint, SocioEconomicallyDisadvantaged)
chronic.group.rate(soledad.abs.joint, All)



spreck.abs.joint <- calpads.join(spreck.abs.24, spreck.demo.24)

chronic.group.rate(spreck.abs.joint, EthnicityRace)
chronic.group.rate(spreck.abs.joint, Homeless)
chronic.group.rate(spreck.abs.joint, StudentswithDisabilities)
chronic.group.rate(spreck.abs.joint, EnglishLearner)
chronic.group.rate(spreck.abs.joint, SocioEconomicallyDisadvantaged)
chronic.group.rate(spreck.abs.joint, All)


mcoe.abs.joint <- calpads.join(mcoe.abs.24, mcoe.demo.24, grade.filt = FALSE)

chronic.group.rate(mcoe.abs.joint, EthnicityRace)
chronic.group.rate(mcoe.abs.joint, Homeless)
chronic.group.rate(mcoe.abs.joint, StudentswithDisabilities)
chronic.group.rate(mcoe.abs.joint, EnglishLearner)
chronic.group.rate(mcoe.abs.joint, SocioEconomicallyDisadvantaged)
chronic.group.rate(mcoe.abs.joint, All)




working <- read_sheet(ss = sheet,
                      sheet = "Chronic Group") %>%
    filter(StudentGroup != "N",
           StudentGroup != "Missing",
           NumberStudents >= 30) %>%
    mutate(Group = case_match(StudentGroupCategory,
                              "All" ~ "All",
                              "Homeless" ~ "Homeless",
                              "StudentswithDisabilities" ~ "Students with \nDisabilities",
                              "SocioEconomicallyDisadvantaged" ~ "Socio-Economically \nDisadvantaged",
                              "Hispanic" ~ "Latino",
                              "EnglishLearner" ~ "English \nLearner",
                              "Black/African Am" ~ "Black/\nAfrican Am",
                              "Nat Hwiin/Othr Pac Islndr" ~ "Pacific Islander",
                              "Multiple" ~ "Multiple \nRaces",
                              .default = StudentGroup
    ))


chronic.dash.graph(
    dist = "soledad.abs.joint",
    dist.name = "Soledad")


chronic.dash.comp(
    dist = "soledad.abs.joint",
    dist.name = "Soledad")




soledad.abs.school.joint <- chr.joint.school(soledad.abs.24, soledad.demo.24, grade.filt = TRUE)



holder <- soledad.abs.school.joint %>%
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
                              "Black/African Am" ~ "Black/\nAfrican Am",
                              "Nat Hwiin/Othr Pac Islndr" ~ "Pacific Islander",
                              "Multiple" ~ "Multiple \nRaces",
                              .default = students
    ))

chron.all.schools(soledad.abs.school.joint , dist.cd = 75440, limit.case.cnt = TRUE)


#### Suspension -----


soledad.sus.joint <- susp.df(soledad.sus.24,soledad.demo.24 )

susp.group.rate(soledad.sus.joint, EthnicityRace)
susp.group.rate(soledad.sus.joint, Homeless)
susp.group.rate(soledad.sus.joint, StudentswithDisabilities)
susp.group.rate(soledad.sus.joint, EnglishLearner)
susp.group.rate(soledad.sus.joint, SocioEconomicallyDisadvantaged)
susp.group.rate(soledad.sus.joint, All)



working <- read_sheet(ss = sheet,
                      sheet = "Susp Group") %>%
    filter(StudentGroup != "N",
           StudentGroup != "Missing",
           NumberStudents >= 30) %>%
    mutate(Group = case_match(StudentGroupCategory,
                              "All" ~ "All",
                              "Homeless" ~ "Homeless",
                              "StudentswithDisabilities" ~ "Students with \nDisabilities",
                              "SocioEconomicallyDisadvantaged" ~ "Socio-Economically \nDisadvantaged",
                              "Hispanic" ~ "Latino",
                              "EnglishLearner" ~ "English \nLearner",
                              "Black/African Am" ~ "Black/\nAfrican Am",
                              "Nat Hwiin/Othr Pac Islndr" ~ "Pacific Islander",
                              "Multiple" ~ "Multiple \nRaces",
                              .default = StudentGroup
    ))


susp.dash.graph(
    dist = "soledad.sus.joint",
    dist.name = "Soledad")


susp.dash.comp(
    dist = "soledad.sus.joint",
    dist.name = "Soledad")



soledad.sus.school.joint <- susp.joint.school(soledad.sus.24, soledad.demo.24)


# soledad.sus.school.joint %>% 
#     filter(str_detect(SchoolName,"Soledad"),
#            # Grade %in% c("KN","1","2"),
#            # StudentswithDisabilities == "Yes"
#     ) %>%
#     add.school.susp()




soledad.sus.school.joint %>%
 #   filter(!str_detect(SchoolName, "Monterey Peninsula Unified")) %>%
    susp.all.schools(dist.cd = 75440, limit.case.cnt = TRUE )
