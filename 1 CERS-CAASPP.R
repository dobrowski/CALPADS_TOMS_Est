



### Load files -----


caa.equivalent <- function(df) {
    
 df2 <-   df %>%
        mutate(Subject = case_match(RecordType,
                                    "01" ~ "ELA",
                                    "02" ~ "Math",
                                    "03" ~ "CAA for ELA",
                                    "04" ~ "CAA for Math",
                                    "05" ~ "CAA for Science",
                                    "06" ~ "Science"),
               subject.equiv = str_sub(Subject, 8,-1),
               ScaleScoreAchievementLevel = as.character(AchievementLevels),
               ScaleScore,
               GradeLevelWhenAssessed = GradeAssessed,
               AssessmentName = str_c("Grade ",GradeLevelWhenAssessed," ",Subject),
        ) 
    
    df2 %>%
        left_join(reference2 , by = c(#GradeLevelWhenAssessed == GradeLevelWhenAssessed,
                                      subject.equiv == Subject,
                                      ScaleScoreAchievementLevel == ScaleScoreAchievementLevel)
                  )
    
}


caa.equ <- function(level, subj, grad) {
    
    subj2 <- str_sub(subj, 9,-1)
    
#    print(subj2)
    
reference2 %>%
        filter( Subject == subj2,
               ScaleScoreAchievementLevel == level,
                GradeLevelWhenAssessed == grad
               ) %>%
        arrange(desc(ScaleScoreNext)) %>%
    bind_rows(tibble(Subject = "1", GradeLevelWhenAssessed = "1", ScaleScoreAchievementLevel = "1",ScaleScoreNext = 1, MeetStandard = 1)) %>%
                             slice(1) %>%
                             pull(ScaleScoreNext)
}


caa.equ(subj = "Math",
        level = 1,
        grad = "04")

use.TOMS <- function(df) {
    
df %>%
    mutate(RealSubject = case_match(RecordType,
                                "01" ~ "ELA",
                                "02" ~ "Math",
                                "03" ~ "CAA for ELA",
                            "04" ~ "CAA for Math",
                            "05" ~ "CAA for Science",
                                "06" ~ "Science"),
           Subject = case_match(RecordType,
                                "01" ~ "ELA",
                                "02" ~ "Math",
                                "03" ~ "ELA",
                                "04" ~ "Math",
                                "05" ~ "Science",
                                "06" ~ "Science") ,
           ScaleScoreAchievementLevel = AchievementLevels,
           ScaleScore,
           GradeLevelWhenAssessed = as.character(GradeAssessed),
           AssessmentName = str_c("Grade ",GradeLevelWhenAssessed," ",RealSubject),
    ) %>%
     filter(!is.na(ScaleScoreAchievementLevel))  %>%
     rowwise() %>%
        mutate(AltScaleScore = caa.equ(subj = RealSubject,
                                      level = ScaleScoreAchievementLevel,
                                      grad = GradeLevelWhenAssessed),
               ScaleScore= max(ScaleScore,AltScaleScore)
        ) %>%
     ungroup() %>%
    select(SSID,
           CALPADSDistrictCode:CALPADSSchoolName,
           Subject, RealSubject,
           ScaleScoreAchievementLevel,
           ScaleScore,
           GradeLevelWhenAssessed,
           AssessmentName,

           CALPADSSpecialEducation:TwoorMoreRaces) %>%
    filter(!is.na(ScaleScore)) %>%
    rename(StudentIdentifier = SSID,
           HispanicOrLatinoEthnicity = HispanicorLatino,
           EL2 = ELStatus,
           ELexit = RFEPDate,
           SWD = CALPADSSpecialEducation,
           SED = EconomicDisadvantageStatus,
           HOM = CALPADSHomelessStatus,
    ) %>%
    mutate(ELdash = case_when(EL2 == "Yes" ~ "Yes",
                              ymd(ELexit) >= ymd(paste0(yr - 4, "-06-15") ) ~ "Yes",
                              TRUE ~ NA),
           LTELdash = case_when(EL2 == "Yes" & ELEntryDate <= ymd( paste0(yr -7,"-06-15")) ~ "Yes",
                              ymd(ELexit) <= ymd( paste0(yr -1, "-08-01") ) ~ NA,
                              TRUE ~ NA),
           StudentIdentifier = as.numeric(StudentIdentifier)
    ) %>%
    select(-EL2,-ELexit, -ELEntryDate, -FirstEntryDateInUSSchool) %>%
    select(-EnrollmentEffectiveDate:-CALPADSFosterStatus) %>%
    select(-ends_with("esting"), -ends_with("Flag")) %>%
    relocate(HispanicOrLatinoEthnicity, .before = SWD) %>%
    mutate(across(HispanicOrLatinoEthnicity:ELdash, ~na_if(., "No")))

}

### Import 2024 TOMS files --------

nmcusd.24 <- read_xlsx(here("data","nmcusd", "27738250000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                     skip = 1)
 nmcusd.24 <- use.TOMS(nmcusd.24)

 
 
 mpusd.24 <- read_xlsx(here("data","mpusd", "27660920000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                        skip = 1)
 mpusd.24 <- use.TOMS(mpusd.24)
 
 
 
 
pg.24 <- read_csv(here("data","pg","2024_CAASPP_Student_Score_Data_File.csv")) 
 

### Reference -------

reference <- read_excel(here("data","ScaleScoreReference.xlsx"))

reference2 <- pivot_longer(reference, cols = c(`1`,`2`,`3`,`4`) ) %>%
    mutate(Grade = if_else(str_length(Grade) >= 2, Grade, paste0(0,Grade))) %>%
    rename(Subject = Subject,
           GradeLevelWhenAssessed = Grade,
           ScaleScoreAchievementLevel = name,
           ScaleScoreNext = value)


reference3 <- reference2 %>%
    filter(ScaleScoreAchievementLevel == 2) %>%
    select(-ScaleScoreAchievementLevel) %>%
    rename(MeetStandard = ScaleScoreNext)

reference2 <- reference2 %>%
    left_join(reference3)


 nmcusd.24 %>% 
  #   filter(AssessmentType == "Summative") %>%
     group_by(AssessmentName) %>%
     tabyl(Subject,ScaleScoreAchievementLevel)


### Cleaning (only needed for CERS files) -------
 # cleans the dataframe from CERS
 
 clean.df <- function(df) {
     
     df %>% 
         # filter(#AssessmentType == "Summative",
         #        Completeness == "Complete") %>%
  #       mutate(GradeLevelWhenAssessed = as.character(GradeLevelWhenAssessed)) %>%
         mutate(GradeLevelWhenAssessed = if_else(str_length(GradeLevelWhenAssessed) >= 2,
                                                 GradeLevelWhenAssessed, 
                                                 paste0(0,GradeLevelWhenAssessed))) %>%
         mutate(GradeLevelWhenAssessed = factor(GradeLevelWhenAssessed, levels = c(
                                                    "KG",
                                                    "01",
                                                    "02",
                                                    "03",
                                                    "04",
                                                    "05",
                                                    "06",
                                                    "07",
                                                    "08",
                                                    "09",
                                                    "10",
                                                    "11",
                                                    "12"))
                ) %>%
         mutate(EL = ifelse(EnglishLanguageAcquisitionStatus == "EL" | EnglishLanguageAcquisitionStatus == "RFEP", "Yes", NA))
     
 }
 
 clean.df(nmcusd.24)
 
 lagunita.23 <- lagunita.23 %>%
     clean.df()
 
 
 lagunita <- lagunita %>%
     clean.df() %>%
     filter(LanguageCode != "ger",
            Subject != "Science") %>%
     mutate(Race = case_when(White == "Yes" ~ "White",
                             HispanicOrLatinoEthnicity == "Yes" ~ "Latino",
                             #     NativeHawaiianOrOtherPacificIslander == "Yes" ~ "Pacific Islander",
                             TRUE ~ "Unknown"))
 
 
### Graphs --------- 

 
 # Graphs all assessment results
overall.graph <- function(df) {
    
    df %>% 
        mutate(AchievementLevel = case_match(ScaleScoreAchievementLevel,
                                             1 ~ "Not Met",
                                             2 ~ "Nearly Met",
                                             3 ~ "Met",
                                             4 ~ "Exceeded"),
               AchievementLevel = factor(AchievementLevel, levels = c("Not Met",
                                                                  "Nearly Met",
                                                                  "Met",
                                                                  "Exceeded"))
               ) %>%
        ggplot( aes( y = RealSubject, fill = AchievementLevel)) +
        geom_bar(color = "black") +
        geom_text(    stat = "count",
                      aes(label = ..count..), 
                      position = position_stack(vjust = 0.5), size = 2) +
        theme_hc() +
        scale_fill_brewer() +
        guides(fill = guide_legend(reverse = TRUE)) + 
        labs(y = "",
             x = "",
             fill = "Achievement Level",
             title = paste0(df$CALPADSDistrictName[1],"\nCount of Students at each Achievement Level"))
}


nmcusd.24 %>%
  #   filter(SWD == "Yes") %>%
     graph.grid()

# Graphs assessment results by grade
graph.wrap <- function(df) {
   
df %>% 
    mutate(AchievementLevel = case_match(ScaleScoreAchievementLevel,
                                         1 ~ "Not Met",
                                         2 ~ "Nearly Met",
                                         3 ~ "Met",
                                         4 ~ "Exceeded"),
           AchievementLevel = factor(AchievementLevel, levels = c("Not Met",
                                                                  "Nearly Met",
                                                                  "Met",
                                                                  "Exceeded")),
           GradeLevelWhenAssessed = factor(GradeLevelWhenAssessed, levels = c("KG",1,2,3,4,5,6,7,8,11)),
           AssessmentName = case_when(AssessmentName == "Kindergarten Summative ELPAC" ~ "Grade  KG Summative ELPAC",
                                      AssessmentName == "Grade 11 ELA Summative" ~ "Grade11 ELA Summative",
                                      AssessmentName == "Grade 11 Math Summative" ~ "Grade11 Math Summative",
                                      AssessmentName == "Grade 10 Summative ELPAC" ~ "Grade10 Summative ELPAC",
                                      AssessmentName == "Grade 11 Summative ELPAC" ~ "Grade11 Summative ELPAC",
                                      AssessmentName == "Grade 12 Summative ELPAC" ~ "Grade12 Summative ELPAC",
                                      TRUE ~AssessmentName)
    ) %>%
    ggplot( aes( y = AssessmentName, fill = AchievementLevel)) +
    geom_bar(color = "black") +
    facet_wrap(vars(RealSubject),
               # vars(GradeLevelWhenAssessed),
               scales = "free") +
    geom_text(    stat = "count",
                  aes(label = ..count..), 
                  position = position_stack(vjust = 0.5), size = 2) +
    # coord_flip() +
    theme_hc() +
    scale_fill_brewer() +
        guides(fill = guide_legend(reverse = TRUE)) + 
    labs(y = "",
         x = "",
         fill = "Achievement Level",
         title = paste0(df$CALPADSDistrictName[1],"\nCount of Students at each Achievement Level"))

}

# Makes a grid so the grades are all lined up across assessments
graph.grid <- function(df) {
    
    df %>% 
        mutate(AchievementLevel = case_match(ScaleScoreAchievementLevel,
                                             1 ~ "Not Met",
                                             2 ~ "Nearly Met",
                                             3 ~ "Met",
                                             4 ~ "Exceeded"),
               AchievementLevel = factor(AchievementLevel, levels = c("Not Met",
                                                                      "Nearly Met",
                                                                      "Met",
                                                                      "Exceeded")),
               GradeLevelWhenAssessed2 = factor(GradeLevelWhenAssessed, levels = c("KG",1,2,3,4,5,6,7,8,11)),
               AssessmentName = case_when(AssessmentName == "Kindergarten Summative ELPAC" ~ "Grade  KG Summative ELPAC",
                                          AssessmentName == "Grade 11 ELA Summative" ~ "Grade11 ELA Summative",
                                          AssessmentName == "Grade 11 Math Summative" ~ "Grade11 Math Summative",
                                          AssessmentName == "Grade 10 Summative ELPAC" ~ "Grade10 Summative ELPAC",
                                          AssessmentName == "Grade 11 Summative ELPAC" ~ "Grade11 Summative ELPAC",
                                          AssessmentName == "Grade 12 Summative ELPAC" ~ "Grade12 Summative ELPAC",
                                          TRUE ~AssessmentName)
        ) %>%
        ggplot( aes( y = AssessmentName, fill = AchievementLevel)) +
        geom_bar(color = "black") +
        facet_grid(vars(RealSubject),
                   vars(GradeLevelWhenAssessed),
                   scales = "free") +
        geom_text(    stat = "count",
                      aes(label = ..count..), 
                      position = position_stack(vjust = 0.5), size = 2) +
        theme_hc() +
        scale_fill_brewer() +
        guides(fill = guide_legend(reverse = TRUE)) + 
        labs(y = "",
             x = "",
             fill = "Achievement Level",
             title = paste0(df$CALPADSDistrictName[1],"\nCount of Students at each Achievement Level"))
    
}

# Saves the wrap graph
save.wrap <- function(df) {
    
    print(df$CALPADSDistrictName[1])
    
    graph.wrap(df)
    
    ggsave(here("output",save.folder,paste0(df$CALPADSDistrictName[1], " wrap ", Sys.Date(),".png")), width = 12, height = 7)
}

# Saves the grid graph
save.grid <- function(df) {
    
    print(df$CALPADSDistrictName[1])
    
    graph.grid(df)
    
    ggsave(here("output",save.folder,paste0(df$CALPADSDistrictName[1], " grid ", Sys.Date(),".png")), width = 12, height = 7)
}

# Saves the overall graph
save.overall <- function(df) {
    
    print(df$CALPADSDistrictName[1])
    
    overall.graph(df)
    
    ggsave(here("output",save.folder,paste0(df$CALPADSDistrictName[1], " overall ", Sys.Date(),".png")), width = 8, height = 7)
}


graph.wrap(nmcusd.24)

graph.grid(nmcusd.24)

save.overall(king.city)
save.wrap(king.city)
save.grid(king.city)


# Run for multiple LEAs 


leas <- c("santa.rita", "san.lucas", "alisal", "san.antonio", "soledad")

leas <- list(santa.rita, san.lucas, alisal, san.antonio, soledad, pg, salinas.city, king.city, mpusd)


map(leas, save.wrap)
map(leas, save.grid)
map(leas, save.overall)


###  Passing Percentage -----

# Calculates the percentage of students meeting or exceeding standards by assessment

passing.perc <- function(df) {
    
    # To save dataframe name and put in final table
    ddff <-     deparse(substitute(df)) 
    
hold <- df %>%
    group_by(RealSubject) %>%
    mutate(Above = ifelse(ScaleScoreAchievementLevel >= 3, TRUE, FALSE),
           perc = mean(Above)*100) %>%
    select(RealSubject, perc) %>%
    distinct()%>%
    mutate(district = ddff)

# Posts to google sheet
sheet_append(ss = sheet,
             sheet = "Percent Met or Exceeded",
             data = hold )

hold

}


passing.perc(nmcusd.24)

### TEmp passing percentage for Soledad ------

# soledad.23 %>%
#     filter(SWD == "Yes",
#            GradeLevelWhenAssessed == "05") %>%
#     passing.perc()
# 
# 
# 
# 
# temp <- salinas.union.23.demo %>%
#     select(Subject, ScaleScoreAchievementLevel) %>%
#     group_by(Subject) %>%
#     mutate(Above = ifelse(ScaleScoreAchievementLevel >= 3, TRUE, FALSE),
#            perc = mean(Above)*100) # %>%
#     select(Subject, perc) %>%
#     distinct()
# 
# 
# 
# 
# 
# 
# temp <- soledad.23 %>%
#     group_by(Subject, GradeLevelWhenAssessed) %>%
#     transmute(Level1perc = 100*mean(ifelse(ScaleScoreAchievementLevel == 1, TRUE, FALSE)),
#               Level2perc = 100*mean(ifelse(ScaleScoreAchievementLevel == 2, TRUE, FALSE)),
#               Level3perc = 100*mean(ifelse(ScaleScoreAchievementLevel == 3, TRUE, FALSE)),
#               Level4perc = 100*mean(ifelse(ScaleScoreAchievementLevel == 4, TRUE, FALSE)),
#               MeetOrExceedperc = 100*mean(ifelse(ScaleScoreAchievementLevel >= 3, TRUE, FALSE)),
#   #            Above = ifelse(ScaleScoreAchievementLevel >= 3, TRUE, FALSE),
#    #        perc = mean(Above)*100
#            ) %>%
# #    select(Subject, GradeLevelWhenAssessed, perc) %>%
#     distinct()
# 
# temp
# 
# write_csv(temp, "Soledad Percent Met by Grade Level.csv")



### Distance from Standard ------

# Calculates distance from standard for All students 

dfs <- function(df) {
    
    # Saves dataframe name
    ddff <-     deparse(substitute(df)) 
    
   holder <-  df %>% 
        filter(Subject %in% c("ELA","Math")) %>%
        mutate(ScaleScoreAchievementLevel = factor(ScaleScoreAchievementLevel),
        ) %>%
        left_join(reference2) %>%
        group_by(Subject) %>%
        mutate(dist.standard = ScaleScore - MeetStandard,
               mean.dist.stand = mean(dist.standard),
               count = n())   %>%
        select(Subject,mean.dist.stand, count) %>%
        distinct() %>%
       mutate(district = ddff)
    
   # Posts to the google sheet
    sheet_append(ss = sheet,
                 sheet = "Distance from Standard",
                 data = holder )
    
    holder
    
}





dfs.w.change <- function(df, cds) {
  
  # Saves dataframe name
  ddff <-     deparse(substitute(df)) 
  
  holder <-  df %>% 
    filter(Subject %in% c("ELA","Math")) %>%
    mutate(ScaleScoreAchievementLevel = factor(ScaleScoreAchievementLevel),
    ) %>%
    left_join(reference2) %>%
    group_by(Subject) %>%
    mutate(dist.standard = ScaleScore - MeetStandard,
           mean.dist.stand = mean(dist.standard),
           count = n())   %>%
    select(Subject,mean.dist.stand, count) %>%
    distinct() %>%
    mutate(district = ddff)
  
  
  # Gets Dashboard data and compares 
  
dash.LEA  <- dash.district(cds) %>%
  filter(
    studentgroup == "ALL",
    indicator == "ELA" | indicator == "MATH"
  ) %>%
  select(cds, Subject = indicator, oldDFS = currstatus, oldcolor = color ,Group, hscutpoints) %>%
  mutate(Subject = case_when(Subject == "MATH" ~ "Math",
                             TRUE ~ Subject)) %>%
  mutate( old.colors = case_when(#old.colors == FALSE ~ "Light Gray",
    oldcolor == 1 ~ "Red",
    oldcolor == 2 ~ "Orange",
    oldcolor == 3 ~ "Yellow",
    oldcolor == 4 ~ "Green",
    oldcolor == 5 ~ "Blue",
                                     TRUE ~ "White") 
  ) 

print(dash.LEA)

holder <- left_join(holder, dash.LEA) %>%
  mutate( change = mean.dist.stand - oldDFS,
          EstimatedColor = case_when(
            #  count < 30 ~ "White",
            
            # High Schools
            hscutpoints == "Y" & Subject == "ELA" & mean.dist.stand <=-45.1 & change <= 2.99 ~ "Red",
            hscutpoints == "Y" & Subject == "ELA" & mean.dist.stand <=-45.1 & change >= 3.0 ~ "Orange",
            hscutpoints == "Y" & Subject == "ELA" & mean.dist.stand <=-0.1 & change <= 2.99 ~ "Orange",
            hscutpoints == "Y" & Subject == "ELA" & mean.dist.stand <=-0.1 & change >= 3.0 ~ "Yellow",    
            hscutpoints == "Y" & Subject == "ELA" & mean.dist.stand <=29.9 & change <= 2.99 ~ "Yellow",
            hscutpoints == "Y" & Subject == "ELA" & mean.dist.stand <=29.9 & change >= 3.0 ~ "Green",
            hscutpoints == "Y" & Subject == "ELA" & mean.dist.stand <=74.9 & change <= 14.99 ~ "Green",
            hscutpoints == "Y" & Subject == "ELA" & mean.dist.stand <=74.9 & change >= 15.0 ~ "Blue",           
            hscutpoints == "Y" & Subject == "ELA" & mean.dist.stand >=75.0 & change <= -3.0 ~ "Green",
            hscutpoints == "Y" & Subject == "ELA" & mean.dist.stand >=75.0 & change  >= -3.0 ~ "Blue",
            
            hscutpoints == "Y" & Subject == "Math" & mean.dist.stand <=-115.1 & change <= 2.99 ~ "Red",
            hscutpoints == "Y" & Subject == "Math" & mean.dist.stand <=-115.1 & change >= 3.0 ~ "Orange",
            hscutpoints == "Y" & Subject == "Math" & mean.dist.stand <=-60.1 & change <= 2.99 ~ "Orange",
            hscutpoints == "Y" & Subject == "Math" & mean.dist.stand <=-60.1 & change >= 3.0 ~ "Yellow",    
            hscutpoints == "Y" & Subject == "Math" & mean.dist.stand <=-0.1 & change <= 2.99 ~ "Yellow",
            hscutpoints == "Y" & Subject == "Math" & mean.dist.stand <=-0.1 & change >= 3.0 ~ "Green",
            hscutpoints == "Y" & Subject == "Math" & mean.dist.stand <=24.9 & change <= 14.99 ~ "Green",
            hscutpoints == "Y" & Subject == "Math" & mean.dist.stand <=24.9 & change >= 15.0 ~ "Blue",           
            hscutpoints == "Y" & Subject == "Math" & mean.dist.stand >=25.0 & change <= -3.0 ~ "Green",
            hscutpoints == "Y" & Subject == "Math" & mean.dist.stand >=25.0 & change  >= -3.0 ~ "Blue",
            
            # Not High Schools
            
            Subject == "ELA" & mean.dist.stand <=-70.1 & change <= 2.9 ~ "Red",
            Subject == "ELA" & mean.dist.stand <=-70.1 & change >= 3.0 ~ "Orange",
            Subject == "ELA" & mean.dist.stand <=-5.1 & change <= 2.9 ~ "Orange",
            Subject == "ELA" & mean.dist.stand <=-5.1 & change >= 3.0 ~ "Yellow",    
            Subject == "ELA" & mean.dist.stand <=9.9 & change <= 2.9 ~ "Yellow",
            Subject == "ELA" & mean.dist.stand <=9.9 & change >= 3.0 ~ "Green",
            Subject == "ELA" & mean.dist.stand <=44.9 & change <= 14.9 ~ "Green",
            Subject == "ELA" & mean.dist.stand <=44.9 & change >= 15.0 ~ "Blue",           
            Subject == "ELA" & mean.dist.stand >=45.0 & change <= -3.0 ~ "Green",
            Subject == "ELA" & mean.dist.stand >=45.0 & change  >= -3.0 ~ "Blue",
            
            Subject == "Math" & mean.dist.stand <=-95.1 & change <= 2.9 ~ "Red",
            Subject == "Math" & mean.dist.stand <=-95.1 & change >= 3.0 ~ "Orange",
            Subject == "Math" & mean.dist.stand <=-25.1 & change <= 2.9 ~ "Orange",
            Subject == "Math" & mean.dist.stand <=-25.1 & change >= 3.0 ~ "Yellow",    
            Subject == "Math" & mean.dist.stand <=-0.1 & change <= 2.9 ~ "Yellow",
            Subject == "Math" & mean.dist.stand <=-0.1 & change >= 3.0 ~ "Green",
            Subject == "Math" & mean.dist.stand <=34.9 & change <= 14.9 ~ "Green",
            Subject == "Math" & mean.dist.stand <=34.9 & change >= 15.0 ~ "Blue",           
            Subject == "Math" & mean.dist.stand >=35.0 & change <= -3.0 ~ "Green",
            Subject == "Math" & mean.dist.stand >=35.0 & change  >= -3.0 ~ "Blue"
            
            
            #  !is.na(mean.dist.stand) & is.na(DFS.y) ~ "Black",
          )
  ) %>%
  select(-oldcolor, - hscutpoints)
  


holder


  # Posts to the google sheet
  sheet_append(ss = sheet,
               sheet = "Distance from Standard",
               data = holder )
  
  holder
  
}

dfs.w.change(nmcusd.25, "27738250000000")

# Works for ALL now need to do the calcs for each student group 



dfs.w.change(suhsd.24, "27661590000000")
temp <- dash.district("27661590000000")


temp <- dfs(soledad.24)

 
 ### Student Group Size ------

# Calculates with student groups are large enough to appear on dashboard
 
 student.group.size <- function(df, limit.30 = TRUE) {
     
 df %>%
#         mutate(EL = ifelse(EnglishLanguageAcquisitionStatus == "EL", "Yes", NA)) %>%
         filter(Subject %in% c("ELA", "Math")) %>%
         group_by(Subject) %>%
         # summarise( across(c(HispanicOrLatinoEthnicity:Filipino,EL), ~  sum(!is.na(.)))) %>%
         # pivot_longer(cols = c(HispanicOrLatinoEthnicity:Filipino,EL)) %>%
         summarise( across(c(HispanicOrLatinoEthnicity:LTELdash), ~  sum(!is.na(.)))) %>%
         pivot_longer(cols = c(HispanicOrLatinoEthnicity:LTELdash)) %>%
         filter(if(limit.30 == TRUE )value >= 30 | name == "HOM" & value >= 15 | name == "LTELdash" & value >= 15 else value >= 1) %>%
         print(n = 30)
 }

 
 student.group.size(nmcusd.24, limit.30 = FALSE) %>% print(n = 30)
 
 
 ### Records ----
 
 
 my.list <- student.group.size(nmcusd.24) %>%
     select(name) %>%
     distinct() %>%
     as.vector()
 
for (i in my.list) {
    
    ii <-     noquote(i) 
    
    print(ii)
    
    
}
 
 # Calculates Distance from Standard by Student Group listed 
 
 dfs2 <- function(df,students) {
     
 cds <-   df$CALPADSDistrictCode[1]
 
 print(cds)
 
     ddff <-     deparse(substitute(df)) 
studentsss <-     deparse(substitute(students))
     
    holder <-  df %>% 
         filter(Subject %in% c("ELA","Math")) %>%
         filter({{students}} == "Yes") %>%
         mutate(ScaleScoreAchievementLevel = factor(ScaleScoreAchievementLevel),
         ) %>%
         left_join(reference2) %>%
         group_by(Subject) %>%
         mutate(dist.standard = ScaleScore - MeetStandard,
                mean.dist.stand = mean(dist.standard))  %>%
         select(Subject,mean.dist.stand) %>%
         distinct() %>%
         mutate(district = ddff,
                students = studentsss
         )
    
    
    
    # Gets Dashboard data and compares 
    
    group_map <- c(
      "SED" = "SED",
      "SWD" = "SWD",
      "White" = "WH",
      "ELdash" = "EL",                              
      "AmericanIndianorAlaskaNative" = "AI",
      "Asian" = "AS",
      "Filipino" = "FI",
      "HawaiianOrOtherPacificIslander" = "PI",
      "TwoorMoreRaces" = "MR",
      "BlackorAfricanAmerican" = "AA",
      "HispanicOrLatinoEthnicity" = "HI",
      "HOM" = "HOM",
      "LTELdash" = "LTEL"
    )
    

    dash.LEA  <- dash.district(cds) %>%
      filter(
        studentgroup == group_map[studentsss],
        indicator == "ELA" | indicator == "MATH",
        
      )     %>%
      select(cds, Subject = indicator, oldDFS = currstatus, oldcolor = color ,Group, hscutpoints) %>%
      mutate(Subject = case_when(Subject == "MATH" ~ "Math",
                                 TRUE ~ Subject)) %>%
      mutate( old.colors = case_when(#old.colors == FALSE ~ "Light Gray",
        oldcolor == 1 ~ "Red",
        oldcolor == 2 ~ "Orange",
        oldcolor == 3 ~ "Yellow",
        oldcolor == 4 ~ "Green",
        oldcolor == 5 ~ "Blue",
        TRUE ~ "White")
      )
    
    
    holder <- left_join(holder, dash.LEA) %>%
      mutate( change = mean.dist.stand - oldDFS,
              EstimatedColor = case_when(
                #  count < 30 ~ "White",
                
                # High Schools
                hscutpoints == "Y" & Subject == "ELA" & mean.dist.stand <=-45.1 & change <= 2.99 ~ "Red",
                hscutpoints == "Y" & Subject == "ELA" & mean.dist.stand <=-45.1 & change >= 3.0 ~ "Orange",
                hscutpoints == "Y" & Subject == "ELA" & mean.dist.stand <=-0.1 & change <= 2.99 ~ "Orange",
                hscutpoints == "Y" & Subject == "ELA" & mean.dist.stand <=-0.1 & change >= 3.0 ~ "Yellow",    
                hscutpoints == "Y" & Subject == "ELA" & mean.dist.stand <=29.9 & change <= 2.99 ~ "Yellow",
                hscutpoints == "Y" & Subject == "ELA" & mean.dist.stand <=29.9 & change >= 3.0 ~ "Green",
                hscutpoints == "Y" & Subject == "ELA" & mean.dist.stand <=74.9 & change <= 14.99 ~ "Green",
                hscutpoints == "Y" & Subject == "ELA" & mean.dist.stand <=74.9 & change >= 15.0 ~ "Blue",           
                hscutpoints == "Y" & Subject == "ELA" & mean.dist.stand >=75.0 & change <= -3.0 ~ "Green",
                hscutpoints == "Y" & Subject == "ELA" & mean.dist.stand >=75.0 & change  >= -3.0 ~ "Blue",
                
                hscutpoints == "Y" & Subject == "Math" & mean.dist.stand <=-115.1 & change <= 2.99 ~ "Red",
                hscutpoints == "Y" & Subject == "Math" & mean.dist.stand <=-115.1 & change >= 3.0 ~ "Orange",
                hscutpoints == "Y" & Subject == "Math" & mean.dist.stand <=-60.1 & change <= 2.99 ~ "Orange",
                hscutpoints == "Y" & Subject == "Math" & mean.dist.stand <=-60.1 & change >= 3.0 ~ "Yellow",    
                hscutpoints == "Y" & Subject == "Math" & mean.dist.stand <=-0.1 & change <= 2.99 ~ "Yellow",
                hscutpoints == "Y" & Subject == "Math" & mean.dist.stand <=-0.1 & change >= 3.0 ~ "Green",
                hscutpoints == "Y" & Subject == "Math" & mean.dist.stand <=24.9 & change <= 14.99 ~ "Green",
                hscutpoints == "Y" & Subject == "Math" & mean.dist.stand <=24.9 & change >= 15.0 ~ "Blue",           
                hscutpoints == "Y" & Subject == "Math" & mean.dist.stand >=25.0 & change <= -3.0 ~ "Green",
                hscutpoints == "Y" & Subject == "Math" & mean.dist.stand >=25.0 & change  >= -3.0 ~ "Blue",
                
                # Not High Schools
                
                Subject == "ELA" & mean.dist.stand <=-70.1 & change <= 2.9 ~ "Red",
                Subject == "ELA" & mean.dist.stand <=-70.1 & change >= 3.0 ~ "Orange",
                Subject == "ELA" & mean.dist.stand <=-5.1 & change <= 2.9 ~ "Orange",
                Subject == "ELA" & mean.dist.stand <=-5.1 & change >= 3.0 ~ "Yellow",    
                Subject == "ELA" & mean.dist.stand <=9.9 & change <= 2.9 ~ "Yellow",
                Subject == "ELA" & mean.dist.stand <=9.9 & change >= 3.0 ~ "Green",
                Subject == "ELA" & mean.dist.stand <=44.9 & change <= 14.9 ~ "Green",
                Subject == "ELA" & mean.dist.stand <=44.9 & change >= 15.0 ~ "Blue",           
                Subject == "ELA" & mean.dist.stand >=45.0 & change <= -3.0 ~ "Green",
                Subject == "ELA" & mean.dist.stand >=45.0 & change  >= -3.0 ~ "Blue",
                
                Subject == "Math" & mean.dist.stand <=-95.1 & change <= 2.9 ~ "Red",
                Subject == "Math" & mean.dist.stand <=-95.1 & change >= 3.0 ~ "Orange",
                Subject == "Math" & mean.dist.stand <=-25.1 & change <= 2.9 ~ "Orange",
                Subject == "Math" & mean.dist.stand <=-25.1 & change >= 3.0 ~ "Yellow",    
                Subject == "Math" & mean.dist.stand <=-0.1 & change <= 2.9 ~ "Yellow",
                Subject == "Math" & mean.dist.stand <=-0.1 & change >= 3.0 ~ "Green",
                Subject == "Math" & mean.dist.stand <=34.9 & change <= 14.9 ~ "Green",
                Subject == "Math" & mean.dist.stand <=34.9 & change >= 15.0 ~ "Blue",           
                Subject == "Math" & mean.dist.stand >=35.0 & change <= -3.0 ~ "Green",
                Subject == "Math" & mean.dist.stand >=35.0 & change  >= -3.0 ~ "Blue"
                
                
                #  !is.na(mean.dist.stand) & is.na(DFS.y) ~ "Black",
              )
      ) %>%
      select(-oldcolor, -hscutpoints)
    
    
    
    holder
    
    
    
    
    
    sheet_append(ss = sheet,
                 sheet = "Distance from Standard Group",
                data = holder )
    holder
     
 }

 dfs2(nmcusd.25,White) 
  dfs2(nmcusd.24,EL) 
 dfs2(nmcusd.24, HispanicOrLatinoEthnicity)
 
 
 ### Percent Meet or Exceed by Student group ------
 
 
 pme2 <- function(df,students) {
     
     ddff <-     deparse(substitute(df)) 
     studentsss <-     deparse(substitute(students))
     
     holder <-  df %>% 
         filter(Subject %in% c("ELA","Math")) %>%
         filter({{students}} == "Yes") %>%
         mutate(ScaleScoreAchievementLevel = factor(ScaleScoreAchievementLevel),
         ) %>%
         left_join(reference2) %>%
         group_by(Subject) %>%
         mutate(met.exceed = ScaleScoreAchievementLevel >= 3 ,
                met.exceed.perc = 100*mean(met.exceed))  %>%
         select(Subject,met.exceed.perc ) %>%
         distinct() %>%
         mutate(district = ddff,
                students = studentsss
         )
     
     sheet_append(ss = sheet,
                  sheet = "Percent Meet by Group",
                  data = holder )
     holder
     
 }
 
  
 pme2(nmcusd.24,HOM)
 pme2(nmcusd.24,SWD)
 pme2(nmcusd.24,SED)
 pme2(nmcusd.24,ELdash)
 pme2(nmcusd.24,EL)
 pme2(nmcusd.24,HispanicOrLatinoEthnicity)
 
 
 south.monterey.23.demo %>%
     filter(str_detect(CALPADSSchoolName,"Portola")) %>%
     pme2(HOM)
 
 
 south.monterey.23.demo %>%
     filter(str_detect(CALPADSSchoolName,"King")) %>%
     passing.perc()
 
 
 
 
  ### Student Growth in Year ----
  
  student.growth <- function(df.old, df.new, filename) {
      
      ddff <-     deparse(substitute(df.new)) 
      
      
      temp.new <- df.new %>%
          filter(Subject %in% c("ELA","Math"),
                 GradeLevelWhenAssessed != "11" ,
                 GradeLevelWhenAssessed != "03") %>% 
          select(StudentIdentifier, SchoolName, GradeLevelWhenAssessed, FirstName, LastOrSurname, Subject, ScaleScore.new = ScaleScore, HispanicOrLatinoEthnicity:ELdash) 
     
           temp.old <- df.old %>%
          filter(Subject %in% c("ELA","Math")) %>% 
          select(StudentIdentifier, Subject, ScaleScore.old = ScaleScore) 
                                  
      
      temp.join <- left_join(temp.new,temp.old) %>%
          # na.omit() %>%
           mutate(ScaleScore.change = as.numeric(ScaleScore.new) - as.numeric(ScaleScore.old),
          )
      
      write_csv(temp.join, here(paste0(filename,".csv")))
      
      temp.join

      
  }
  
  
temp <-   student.growth(pg.22,pg.23, "Pacific Grover 2023 Student Scale Score Change")
  
student.growth(wash.22,wash.23, "Washington 2023 Student Scale Score Change")
  


 ### Add demo -----
 
 
 add.demo <- function(df, df.demo) {
     
temp <- df.demo %>%
     select(StudentIdentifier = `Statewide Student Identifier (SSID)`,
            Gender,
            EL2 = `English Learner (EL)`,
            ELexit = `EL Exit Date`,
            SWD = `CALPADS Special Education`,
            SED = `CALPADS Socioeconomically Disadvantage (SED) Status`,
            HOM = `Homeless Status`) %>%
     mutate(ELdash = case_when(EL2 == "Yes" ~ "Yes",
                               ymd(ELexit) >= ymd("2019-06-15") ~ "Yes",
                               TRUE ~ "No"),
            StudentIdentifier = as.numeric(StudentIdentifier)
            ) %>%
                select(-EL2,-ELexit) %>%
    mutate(across(Gender:ELdash, ~na_if(., "No")))
 
 
 df %>%
     left_join(temp)
 
 } 
 
 
 
 spreckels.22 <- clean.df(spreckels.22) 
 spreckels.23 <- clean.df(spreckels.23) 
 spreckels.23 <-  add.demo(spreckels.23, spreckels.23.demo)


ss23demo<- soledad.23.demo %>%
    mutate(`Statewide Student Identifier (SSID)` = SSID)

temp <-  add.demo(soledad, ss23demo)
 
 
 ###  All of it ------
  
wash.22 <- wash.22 %>%
    filter(str_detect( DistrictName, "Washington"))


chualar.23 <- clean.df(chualar.23) 
mcoe.alt.ed.23 <- clean.df(mcoe.alt.ed.23) 
soledad.23 <-  add.demo(soledad.23, soledad.23.demo)


  overall.graph(mpusd.24)
  
  graph.wrap(mpusd.24)
  
  graph.grid(mpusd.24)
  
  save.overall(mpusd.24)
  save.wrap(mpusd.24)
  save.grid(mpusd.24)
  
  
  elpi.change("Washington", wash.22, wash.23, "Washington ELPI 2023")
  
  passing.perc(mpusd.24)
  
  
  dfs(mpusd.24)
  
  student.group.size(mpusd.24) 
  
  
   dfs2(mpusd.24,White) 
   dfs2(mpusd.24,ELdash) 
   dfs2(mpusd.24,Asian)
    #  dfs2(mpusd.24,MigrantStatus) 
   dfs2(mpusd.24,Filipino) 
   dfs2(mpusd.24,TwoorMoreRaces) 
   dfs2(mpusd.24,BlackorAfricanAmerican) 
   dfs2(mpusd.24,HawaiianOrOtherPacificIslander) 
   dfs2(mpusd.24,HispanicOrLatinoEthnicity) 
#   dfs2(mpusd.24,SpecialEducationforTesting) 
#   dfs2(mpusd.24,EconomicDisadvantageTesting) 
   dfs2(mpusd.24 ,SED) 
   dfs2(mpusd.24,HOM) 
   dfs2(mpusd.24,SWD) 

   
 ### By school in a district -------  
   
   
#    nmcusd.24.nest <-   nmcusd.24 %>% 
#      group_by(CALPADSSchoolName) %>%
#        nest() %>%
#      walk(graph.wrap(data) )
#  
#  
# soledad2 <-  soledad %>%
#      filter(str_detect(DistrictName,"Soledad")) 
# 
# nmcusd.24 %>%
#      split(nmcusd.24$CALPADSSchoolName) %>%
#      map(~graph.wrap2(.))
#  
 
 
 
   # Runs the graph.wrap for every school in a district

   graph.wrap2 <- function(df) {
       
       namer <- unique(df$CALPADSSchoolName)
       
       df %>% 
           mutate(ScaleScoreAchievementLevel = factor(ScaleScoreAchievementLevel),
                  GradeLevelWhenAssessed = factor(GradeLevelWhenAssessed, levels = c("KG",1,2,3,4,5,6,7,8,11)),
                  AssessmentName = case_when(AssessmentName == "Kindergarten Summative ELPAC" ~ "Grade  KG Summative ELPAC",
                                             AssessmentName == "Grade 11 ELA Summative" ~ "Grade11 ELA Summative",
                                             AssessmentName == "Grade 11 Math Summative" ~ "Grade11 Math Summative",
                                             AssessmentName == "Grade 10 Summative ELPAC" ~ "Grade10 Summative ELPAC",
                                             AssessmentName == "Grade 11 Summative ELPAC" ~ "Grade11 Summative ELPAC",
                                             AssessmentName == "Grade 12 Summative ELPAC" ~ "Grade12 Summative ELPAC",
                                             TRUE ~AssessmentName)
           ) %>%
           ggplot( aes( y = AssessmentName, fill = ScaleScoreAchievementLevel)) +
           geom_bar(color = "black") +
           facet_wrap(vars(Subject),
                      # vars(GradeLevelWhenAssessed),
                      scales = "free") +
           geom_text(    stat = "count",
                         aes(label = ..count..), 
                         position = position_stack(vjust = 0.5), size = 2) +
           # coord_flip() +
           theme_hc() +
           scale_fill_brewer() +
           guides(fill = guide_legend(reverse = TRUE)) + 
           labs(y = "",
                x = "",
                fill = "Achievement Level",
                title = paste0(namer," Count of Students at each Achievement Level"))
       
       
       ggsave(here("output",paste0(df[1,2]," ",namer ," wrap ", Sys.Date(),".png")), width = 12, height = 7)
       
       
       
   }
   
   dfs2.school <- function(df,students) {
       
       ddff <-     deparse(substitute(df)) 
       studentsss <-     deparse(substitute(students))
       
       holder <-  df %>% 
           filter(Subject %in% c("ELA","Math")) %>%
           filter({{students}} == "Yes") %>%
           mutate(ScaleScoreAchievementLevel = factor(ScaleScoreAchievementLevel),
           ) %>%
           left_join(reference2) %>%
           group_by(Subject) %>%
           mutate(dist.standard = ScaleScore - MeetStandard,
                  mean.dist.stand = mean(dist.standard),
                  count = n())  %>%
           select(Subject,mean.dist.stand, count) %>%
           distinct() %>%
           mutate(district = ddff,
                  students = studentsss
           )
       
       # sheet_append(ss = sheet,
       #              sheet = "Distance from Standard School Group",
       #              data = holder )
       holder
       
   }
   
   add.school.dfs <- function(df) {
       
       namer <- unique(df$CALPADSSchoolName)
       coder <- unique(df$CALPADSSchoolCode)
       
   waiting.room <- dfs2.school(df %>% mutate(All = "Yes"),All) %>%
       bind_rows(  
           dfs2.school(df,White) ) %>%
   bind_rows(  dfs2.school(df,ELdash) ) %>%
     bind_rows(  dfs2.school(df,LTELdash) ) %>% ##################
     
   bind_rows( dfs2.school(df,Asian) )  %>%
   bind_rows( dfs2.school(df,Filipino) )  %>%
   bind_rows( dfs2.school(df,TwoorMoreRaces) )  %>%
   bind_rows( dfs2.school(df,HispanicOrLatinoEthnicity) )  %>%
   bind_rows( dfs2.school(df,SWD) )  %>%
   bind_rows( dfs2.school(df,SED) )  %>%
   bind_rows( dfs2.school(df,HOM) ) %>%
       mutate(SchoolName = namer,
              CDS = coder)
       
   waiting.room


   }
   
   
   
   school.split <-  suhsd.24 %>%
       filter(str_detect(CALPADSDistrictName,"Salinas Union")) 
   
   school.split %>%
       split(school.split$CALPADSSchoolName) %>%
       map(~graph.wrap2(.))
   
   
   
 # Calculate DFS for student groups in district
   
   
   # # Testing add.school.dfs()
   # nmcusd.24 %>%
   #     filter(str_detect(CALPADSSchoolName,"Prunedale")) %>%
   #     add.school.dfs()
   # 
   # 
   # 
   # # Use for SUHSD
   #  school.split <-  salinas.union.23.demo %>%
   #      filter(str_detect(CALPADSDistrictName,"Salinas Union")) 
   #  
   #  # Use for Santa Rita schools
   #  school.split <-  sr.combo.23 %>%
   #      rename(ScaleScoreAchievementLevel = AchievementLevels,
   #              ELdash = ELStatus,
   #             HispanicOrLatinoEthnicity = HispanicorLatino,
   #             SWD = IDEAIndicatorforTesting,
   #             SED = EconomicDisadvantageTesting,
   #             HOM = HomelessStatus) %>%
   #      mutate(GradeLevelWhenAssessed = str_pad(GradeAssessed, 2, "left", "0" )
   #      ) %>%
   #      filter(!is.na(ScaleScore))
   #  
   #  
   #  # Use for SoMoCo
   #  school.split <-  mcoe.23 %>%
   #      filter(str_detect(CALPADSDistrictName,"Monterey")) 
   #  
   #  # USe for King City or others
   #  school.split <-  king.city.23 %>%
   #      rename(CALPADSSchoolName = SchoolName,
   #             CALPADSSchoolCode = SchoolId,
   #             TwoorMoreRaces = TwoOrMoreRaces) %>%
   #      filter(str_detect(DistrictName,"King City")) 
   # 
   #  # USe for Soledad or others
   #  school.split <-  mcoe.alt.ed.23 %>%
   #      rename(CALPADSSchoolName = SchoolName,
   #             CALPADSSchoolCode = SchoolId,
   #             TwoorMoreRaces = TwoOrMoreRaces) %>%
   #      filter(str_detect(DistrictName,"Monterey")) 
   #  
   #  # Used as basis for graphing in DFS student group Graph
   
   
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
                              "TwoorMoreRaces" ~ "Multiple \nRaces",
                              
                              "ELdash" ~ "English Learner",
                              .default = students
    ))
  
   
   
### By demo split -------
   
   
   graph.demo <- function(df, demo) {
       
       demodemo <-     deparse(substitute(demo))
       
       df %>% 
           mutate(ScaleScoreAchievementLevel = factor(ScaleScoreAchievementLevel),
                  GradeLevelWhenAssessed = factor(GradeLevelWhenAssessed, levels = c("KG",1,2,3,4,5,6,7,8,11)),
                  AssessmentName = case_when(AssessmentName == "Kindergarten ELPAC Summative" ~ "Grade  KG ELPAC Summative",
                                             AssessmentName == "Grade 11 ELA Summative" ~ "Grade11 ELA Summative",
                                             AssessmentName == "Grade 11 Math Summative" ~ "Grade11 Math Summative",
                                             AssessmentName == "Grade 10 ELPAC Summative" ~ "Grade10 ELPAC Summative",
                                             AssessmentName == "Grade 11 ELPAC Summative" ~ "Grade11 ELPAC Summative",
                                             AssessmentName == "Grade 12 ELPAC Summative" ~ "Grade12 ELPAC Summative",
                                             TRUE ~AssessmentName)
           ) %>%
           ggplot( aes( y = Subject, fill = ScaleScoreAchievementLevel)) +
           geom_bar(color = "black") +
           facet_wrap(vars({{demo}}),
                      # vars(GradeLevelWhenAssessed),
                      scales = "free") +
           geom_text(    stat = "count",
                         aes(label = ..count..), 
                         position = position_stack(vjust = 0.5), size = 2) +
           # coord_flip() +
           theme_hc() +
           scale_fill_brewer() + 
           labs(y = "",
                x = "",
                fill = "Achievement Level",
                title = paste0("Count of Students at each Achievement Level by ", demodemo))
       
   }

   
   graph.demo(san.ardo.23, LanguageCode)
   
  ggsave(here("output",paste0("Lagunita Language ", Sys.Date(),".png")), width = 12, height = 7)
 
  # lagunita %>%
  #     filter(LanguageCode != "ger") %>%
  # graph.demo( LanguageCode)
  # 
  # ggsave(here("output",paste0("Lagunita Language2 ", Sys.Date(),".png")), width = 12, height = 7)
  
  
lagunita2 <- lagunita.23 %>%
      mutate(Race = case_when(White == "Yes" ~ "White",
                              HispanicOrLatinoEthnicity == "Yes" ~ "Latino",
                         #     NativeHawaiianOrOtherPacificIslander == "Yes" ~ "Pacific Islander",
                              TRUE ~ "Unknown"))
  

graph.demo(lagunita2, Race)

ggsave(here("output",paste0("Lagunita Race ", Sys.Date(),".png")), width = 12, height = 7)




### Comparison to prior year by grade ----



comp.grade.year <- function(df22,df23, assessment = "Math", dist) {
    

df23 %>%
    bind_rows(df22) %>% 
    filter(Subject == assessment) %>%
    mutate(ScaleScoreAchievementLevel = factor(ScaleScoreAchievementLevel),
    ) %>%
    left_join(reference2) %>%
    group_by(SchoolYear, Subject, GradeLevelWhenAssessed ) %>%
    transmute(dist.standard = ScaleScore - MeetStandard,
           mean.dist.stand = mean(dist.standard))  %>%
    select(SchoolYear, Subject, GradeLevelWhenAssessed, DFS = mean.dist.stand) %>%
    distinct() %>%
    mutate(SchoolYear = factor(SchoolYear),
           DFS = as.numeric(DFS)) %>%
    ggplot(aes(x = GradeLevelWhenAssessed, y = DFS, )) +
    geom_col(aes(fill = SchoolYear, 
                 color = "black"),
             position = "dodge2") +
    mcoe_theme +
 #   scale_fill_identity() +
    scale_color_identity() +
    labs(y = "Distance from Standard",
         title = paste0(dist," - ", assessment," CAASPP Results by Grade")
         )

    ggsave(here("output",paste0(dist," - ", assessment," CAASPP Results by Grade ", Sys.Date(),".png")), width = 8, height = 5)
    
}


comp.grade.year(spreckels.22, spreckels.23, "Math", "Spreckels")

comp.grade.year(spreckels.22, spreckels.23, "ELA", "Spreckels")


comp.grade.year(king.city.22, king.city.23, "Math", "King City")




comp.grade.year.meet.exceed <- function(df22,df23, assessment = "Math", dist) {
    
    
    df23 %>%
        bind_rows(df22) %>% 
        filter(Subject == assessment) %>%
        # mutate(ScaleScoreAchievementLevel = factor(ScaleScoreAchievementLevel),
        # ) %>%
        # left_join(reference2) %>%
        group_by(SchoolYear, Subject, GradeLevelWhenAssessed ) %>%
        mutate(Above = ifelse(ScaleScoreAchievementLevel >= 3, TRUE, FALSE),
               perc = mean(Above)*100) %>%
        # transmute(dist.standard = ScaleScore - MeetStandard,
        #           mean.dist.stand = mean(dist.standard))  %>%
        select(SchoolYear, Subject, GradeLevelWhenAssessed, perc) %>%
        distinct() %>%
        mutate(SchoolYear = factor(SchoolYear),
    #           DFS = as.numeric(DFS)
               ) %>%
        ggplot(aes(x = GradeLevelWhenAssessed, y = perc, )) +
        geom_col(aes(fill = SchoolYear, 
                     color = "black"),
                 position = "dodge2") +
        mcoe_theme +
        #   scale_fill_identity() +
        scale_color_identity() +
        labs(y = "Percent Meeting \nor Exceeding",
             title = paste0(dist," - ", assessment," CAASPP Results by Grade")
        )
    
    ggsave(here("output",paste0(dist," - ", assessment," CAASPP Results by Grade ", Sys.Date(),".png")), width = 8, height = 5)
    
}

comp.grade.year.meet.exceed(spreckels.22, spreckels.23, "Math", "Spreckels")

comp.grade.year.meet.exceed(spreckels.22, spreckels.23, "ELA", "Spreckels")









### Calculate by grade level and student group  ----

pg.23 %>%
    filter(Subject %in% c("ELA","Math")) %>%
 #  filter(EL == "Yes") %>%    
    group_by(Subject, GradeLevelWhenAssessed) %>%
    mutate(Above = ifelse(ScaleScoreAchievementLevel >= 3, TRUE, FALSE),
           perc = mean(Above)*100) %>%
    select(Subject, perc)  %>%
    distinct() %>%
    print(n=38)




### END ---------
