

### Load files ------


# Set district name for folder to same images 
save.folder <- "nmcusd"


# Load TOMS CAASPP results 
nmcusd.24 <- read_xlsx(here("data","nmcusd", "27738250000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                       skip = 1)
nmcusd.24 <- use.TOMS(nmcusd.24)


spreck.24 <- read_xlsx(here("data","spreckels", "27662250000000_CAASPP_Student_Score_Data_File_EnrolledStudentScoreData_2024.xlsx"),
                       skip = 1)
spreck.24 <- use.TOMS(spreck.24)




# Load TOMS ELPAC results 
nmcusd.elpac.24 <- read_xlsx(here("data","nmcusd","27738250000000_Summative_ELPAC_and_Summative_Alternate_ELPAC_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                             skip = 1
                             )

kingcity.elpac.24 <- read_xlsx(here("data","king city","27660500000000_Summative_ELPAC_and_Summative_Alternate_ELPAC_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
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


graph.wrap(spreck.24)

graph.grid(spreck.24)

save.overall(spreck.24)
save.wrap(spreck.24)
save.grid(spreck.24)


passing.perc(spreck.24)

dfs(spreck.24)

student.group.size(spreck.24, limit.30 = FALSE) %>% print(n = 30)


# pme2(spreck.24,HOM)
pme2(spreck.24,SWD)
pme2(spreck.24,SED)
pme2(spreck.24,ELdash)
pme2(spreck.24,HispanicOrLatinoEthnicity)
pme2(spreck.24,White)
pme2(spreck.24,TwoorMoreRaces)


dfs2(spreck.24,White) 
dfs2(spreck.24,ELdash) 
# dfs2(nmcusd.24,Asian)
#  dfs2(nmcusd.24,MigrantStatus) 
# dfs2(nmcusd.24,Filipino) 
 dfs2(spreck.24,TwoorMoreRaces) 
# dfs2(nmcusd.24,BlackorAfricanAmerican) 
# dfs2(nmcusd.24,HawaiianOrOtherPacificIslander) 
dfs2(spreck.24,HispanicOrLatinoEthnicity) 
#   dfs2(nmcusd.24,SpecialEducationforTesting) 
#   dfs2(nmcusd.24,EconomicDisadvantageTesting) 
dfs2(spreck.24 ,SED) 
# dfs2(spreck.24,HOM) 
dfs2(spreck.24,SWD) 



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



dfs.graph(dist = "mpusd.24",
          assessment = "ELA",
          dist.name = "Monterey Peninsula")


dfs.graph(dist = "mpusd.24",
          assessment = "Math",
          dist.name = "Monterey Peninsula")



dfs.comp(dist = "mpusd.24",
         assessment = "ELA",
         dist.name = "Monterey Peninsula")


dfs.comp(dist = "mpusd.24",
         assessment = "Math",
         dist.name = "Monterey Peninsula")






school.split <-  spreck.24  %>%
    filter(str_detect(CALPADSDistrictName,"Spreckels")) 

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
    
    math.df <- dfs.comp.school(df = holder, cds = school.list[i], assessment = "Math", limit.case.count = TRUE) 
    
    dfs.comp.school.graph(math.df)
    dfs.school.graph(math.df)

    #   ggsave(here("output",save.folder ,paste0(school.list[i], " - ","Math"," CAASPP Student Group Results 2023 and 2024 Comparison ", Sys.Date(),".png")), width = 8, height = 5)
    
    ela.df <-   dfs.comp.school(df = holder, cds = school.list[i], assessment = "ELA", limit.case.count = TRUE) 
    
    dfs.comp.school.graph(ela.df)
    dfs.school.graph(ela.df)

    #   ggsave(here("output",save.folder ,paste0(school.list[i], " - ","ELA"," CAASPP Student Group Results 2023 and 2024 Comparison ", Sys.Date(),".png")), width = 8, height = 5)
    
}


##### ELPI ------



elpi.calc(kingcity.elpac.24 #%>%
          #   filter(str_detect(TestedSchoolName1,"Prune"))
          , "D"
)


school.list <- unique(kingcity.elpac.24$TestedSchoolName1)

for (i in school.list) {
    elpi.calc(kingcity.elpac.24%>%
                  filter(str_detect(TestedSchoolName1,i)),
              "S"
    )
    
}


### Chronic -------




mpusd.abs.joint <- calpads.join(mpusd.abs.24, mpusd.demo.24)

chronic.group.rate(mpusd.abs.joint, EthnicityRace)
chronic.group.rate(mpusd.abs.joint, Homeless)
chronic.group.rate(mpusd.abs.joint, StudentswithDisabilities)
chronic.group.rate(mpusd.abs.joint, EnglishLearner)
chronic.group.rate(mpusd.abs.joint, SocioEconomicallyDisadvantaged)
chronic.group.rate(mpusd.abs.joint, All)



spreck.abs.joint <- calpads.join(spreck.abs.24, spreck.demo.24)

chronic.group.rate(spreck.abs.joint, EthnicityRace)
chronic.group.rate(spreck.abs.joint, Homeless)
chronic.group.rate(spreck.abs.joint, StudentswithDisabilities)
chronic.group.rate(spreck.abs.joint, EnglishLearner)
chronic.group.rate(spreck.abs.joint, SocioEconomicallyDisadvantaged)
chronic.group.rate(spreck.abs.joint, All)


nmcusd.abs.joint <- calpads.join(nmcusd.abs.24, nmcusd.demo.24)

chronic.group.rate(nmcusd.abs.joint, EthnicityRace)
chronic.group.rate(nmcusd.abs.joint, Homeless)
chronic.group.rate(nmcusd.abs.joint, StudentswithDisabilities)
chronic.group.rate(nmcusd.abs.joint, EnglishLearner)
chronic.group.rate(nmcusd.abs.joint, SocioEconomicallyDisadvantaged)
chronic.group.rate(nmcusd.abs.joint, All)




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
    dist = "nmcusd.abs.joint",
    dist.name = "North Monterey County")


chronic.dash.comp(
    dist = "nmcusd.abs.joint",
    dist.name = "North Monterey County")




nmcusd.abs.school.joint <- chr.joint.school(nmcusd.abs.24, nmcusd.demo.24)



holder <- nmcusd.abs.school.joint %>%
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

chron.all.schools(nmcusd.abs.school.joint , dist.cd = 73825)


#### Suspension -----


nmcusd.sus.joint <- susp.df(nmcusd.sus.24,nmcusd.demo.24 )

susp.group.rate(nmcusd.sus.joint, EthnicityRace)
susp.group.rate(nmcusd.sus.joint, Homeless)
susp.group.rate(nmcusd.sus.joint, StudentswithDisabilities)
susp.group.rate(nmcusd.sus.joint, EnglishLearner)
susp.group.rate(nmcusd.sus.joint, SocioEconomicallyDisadvantaged)
susp.group.rate(nmcusd.sus.joint, All)



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
    dist = "nmcusd.sus.joint",
    dist.name = "North Monterey County")


susp.dash.comp(
    dist = "nmcusd.sus.joint",
    dist.name = "North Monterey County")



nmcusd.sus.school.joint <- susp.joint.school(nmcusd.sus.24, nmcusd.demo.24)


nmcusd.sus.school.joint %>% 
    filter(str_detect(SchoolName,"Los Arboles"),
           # Grade %in% c("KN","1","2"),
           # StudentswithDisabilities == "Yes"
    ) %>%
    add.school.susp()

# 
# holder <- mpusd.sus.school.joint %>%
#     # filter(str_detect(DistrictName,dist.name)) %>%
#     split(.$SchoolName) %>%
#     map_df(~add.school.susp(.))  %>%
#     mutate(Group = case_match(students,
#                               "All" ~ "All",
#                               "Homeless" ~ "Homeless",
#                               "StudentswithDisabilities" ~ "Students with \nDisabilities",
#                               "SocioEconomicallyDisadvantaged" ~ "Socio-Economically \nDisadvantaged",
#                               "Hispanic" ~ "Latino",
#                               "EnglishLearner" ~ "English \nLearner",
#                               "Black/African Am" ~ "Black/\nAfrican Am",
#                               "Nat Hwiin/Othr Pac Islndr" ~ "Pacific Islander",
#                               "Multiple" ~ "Multiple \nRaces",
#                               .default = students
#     ))



nmcusd.sus.school.joint %>%
 #   filter(!str_detect(SchoolName, "Monterey Peninsula Unified")) %>%
    susp.all.schools(dist.cd = 73825)
