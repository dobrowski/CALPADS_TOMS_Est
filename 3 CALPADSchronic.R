
# Uses the CALPADS EOY3 files to calculate Chronic Absenteeism rates for student groups

### Load files -------


mpusd.abs.24 <- read_csv(here("data", "mpusd" , "14.2_StudentAbsencesStudentList.InReviewUncertified_20240715.csv"))
mpusd.demo.24 <- read_csv(here("data", "mpusd" , "8.1_StudentProfileList(EOY3).InReviewUncertified_20240715.csv"))



### Functions ----

calpads.join <- function(df, df.demo, grade.filt = TRUE) {
    
    
    df.calpads2 <- df %>%
        mutate(keeper = case_when(grade.filt == FALSE ~ TRUE, 
                                  Grade %in% c("TK","KN",1,2,3,4,5,6,7,8,  "01","02","03","04","05","06","07","08" ,  "1.0","2.0","3.0","4.0","5.0","6.0","7.0","8.0"   ) ~ TRUE,  
                                  TRUE ~ FALSE)
  ) %>%
        filter(DaysExpectedA >= 1,
               keeper == TRUE
             #  Grade %in% c("TK","KN",1,2,3,4,5,6,7,8,  "01","02","03","04","05","06","07","08" ,  "1.0","2.0","3.0","4.0","5.0","6.0","7.0","8.0"   )
               ) %>%
        group_by(SSID, StudentName ) %>% #, Ethnicity, EnglishLearner, SocioEconomicallyDisadvantaged) %>%
        summarise(across(.cols =   c(DaysExpectedA:DaysAbsentCEFG),
                         ~ sum(.x, na.rm = TRUE)
        )
        ) %>%
        mutate(AbsenceRate2 = 100*DaysAbsentCEFG/DaysExpectedA,
               chronic = if_else(AbsenceRate2 >= 10, TRUE, FALSE),
               dupes = duplicated(SSID))
    

    df.calpads.demo2 <- df.demo %>%
      
     mutate( LTEL = case_when(ELAStatus == "EL" & mdy(ELAStatusStartDate) <= ymd( paste0(yr -7,"-06-15")) ~ "Y",
                           TRUE ~ "N")
     ) %>%
        select(SSID, EthnicityRace, Homeless, StudentswithDisabilities, EnglishLearner = EnglishLearner_ReclassifiedFluentEnglishProficient, LTEL, SocioEconomicallyDisadvantaged) %>%
        distinct() %>%
        group_by(SSID) %>%
        mutate(Homeless = if_else(any(Homeless == "Y"), "Y", "N" ),
               StudentswithDisabilities = if_else(any(StudentswithDisabilities == "Y"), "Y", "N" ),
               EnglishLearner = if_else(any(EnglishLearner == "Y"), "Y", "N" ),
               LTEL = if_else(any(LTEL == "Y"), "Y", "N" ),
               SocioEconomicallyDisadvantaged = if_else(any(SocioEconomicallyDisadvantaged == "Y"), "Y", "N" ),
               All = "Y"
        ) %>%
        distinct() # %>%
    #   mutate(dupes = duplicated(SSID)) 

    df.calpads2 %>%
        left_join(df.calpads.demo2)
    
    
}


chronic.group.rate <- function(df, studentgroup) {

    ddff <-     deparse(substitute(df)) 
    studentsss <-     deparse(substitute(studentgroup))
    
    holder <- df %>%
        filter(DaysExpectedA >= 31) %>%
        group_by({{studentgroup}}) %>%
        transmute(count = n(),
                  perc.chronic = 100*mean(chronic)) %>%
        distinct()%>%
        mutate(district = ddff,
               students = studentsss
        )
    
    sheet_append(ss = sheet,
                 sheet = "Chronic Group",
                 data = holder )
    holder

    
}

chronic.group.rate.w.change <- function(df, studentgroup, cds) {
  
  ddff <-     deparse(substitute(df)) 
  studentsss <-     deparse(substitute(studentgroup))
  
  holder <- df %>%
    filter(DaysExpectedA >= 31) %>%
    group_by({{studentgroup}}) %>%
    transmute(count = n(),
              perc.chronic = 100*mean(chronic)) %>%
    distinct()%>%
    { if (!"EthnicityRace" %in% names(.)) mutate(., EthnicityRace = "blank") else . } %>%
    mutate(district = ddff,
           students = if_else(studentsss == "EthnicityRace", EthnicityRace ,studentsss)
    ) %>%
 #   mutate(Group = students)
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
  )) %>%
    filter(Group != "Missing")
  
  
  
  
  print("holder")
  print(holder)
  
  # Gets Dashboard data and compares 
  
  dash.LEA  <- dash.district(cds) %>%
    filter(
   #   studentgroup == "ALL",
      indicator == "CHRO"
    ) %>%
    select(cds, Subject = indicator, oldstatus = currstatus, oldcolor = color ,Group) %>%
    mutate( old.colors = case_when(#old.colors == FALSE ~ "Light Gray",
      oldcolor == 1 ~ "Red",
      oldcolor == 2 ~ "Orange",
      oldcolor == 3 ~ "Yellow",
      oldcolor == 4 ~ "Green",
      oldcolor == 5 ~ "Blue",
      TRUE ~ "White") 
    ) 
  
  print("dash.LEA")
  print(dash.LEA)
  
  holder <- left_join(holder, dash.LEA) %>%
    mutate( change = perc.chronic - oldstatus,
            EstimatedColor = case_when(
              count < 30 ~ "White",
              perc.chronic >=20 & change > -0.5 ~ "Red",
              perc.chronic >=20 & change <= -3.0 ~ "Yellow",
              perc.chronic >=20 & change <= -0.5 ~ "Orange",
              
              perc.chronic >=10 & change >= 3.0 ~ "Red",
              perc.chronic >=10 & change <= -0.5 ~ "Yellow", 
              perc.chronic >=10 & change < 3.0 ~ "Orange", 
              
              perc.chronic >=5 & change > 0.5 ~ "Orange",
              perc.chronic >=5 & change <= -0.5 ~ "Green",
              perc.chronic >=5 & change < 0.5 ~ "Yellow",
              
              perc.chronic >=2.5 & change >= 3.0 ~ "Orange",
              perc.chronic >=2.5 & change <= -3.0 ~ "Blue",    
              perc.chronic >=2.5 & change <= 0.5 ~ "Green",    
              perc.chronic >=2.5 & change < 3.0 ~ "Yellow",    
              
              perc.chronic < 2.5 & change >= 3.0 ~ "Yellow",
              perc.chronic < 2.5 & change <= 0.5 ~ "Blue",
              perc.chronic < 2.5 & change  < 3.0 ~ "Green"

            )
    ) %>%
    select(-oldcolor, -Subject, -EthnicityRace) %>%
    filter({{studentgroup}} != "N") 
  
  
  sheet_append(ss = sheet,
               sheet = "Chronic Group",
               data = holder )
  holder
  
  
}



mpusd.abs.joint <- calpads.join(mpusd.abs.24, mpusd.demo.24)

chronic.group.rate(mpusd.abs.joint, EthnicityRace)
chronic.group.rate(mpusd.abs.joint, Homeless)
chronic.group.rate(mpusd.abs.joint, StudentswithDisabilities)
chronic.group.rate(mpusd.abs.joint, EnglishLearner)
chronic.group.rate(mpusd.abs.joint, SocioEconomicallyDisadvantaged)
chronic.group.rate(mpusd.abs.joint, All)



### Graphing Single Year -----


chronic.dash.graph <- function(dist, dist.name ) {
    
    
    work.group <-   working %>%
        filter(District == dist) %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    
    
    working %>%
        filter(District == dist) %>%
        #       mutate(DFS = as.numeric(DFS)) %>%
        ggplot(aes(x = Group, y = PercentChronicAbsent, )) +
#        ggplot(aes(x = fct_reorder(Group,PercentChronicAbsent), y = PercentChronicAbsent, )) +
        geom_col(aes(fill = EstimatedColor, 
                     color = "black")) +
        mcoe_theme +
        {if(length(work.group) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
        scale_fill_identity() +
        scale_color_identity() +
        labs(y = "Percent Chronically Absent",
             title = paste0(dist.name," - Chronically Absent Student Group Estimates ", thisyear))
    
    
    ggsave(here("output",save.folder ,paste0(dist.name, " - Chronically Absent Student Group Estimates ", thisyear , Sys.Date(),".png")), width = 8, height = 5)    
    
}


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
                              "LTEL" ~ "Long Term\nEnglish\nLearner",
                              
                              "Black/African Am" ~ "Black/\nAfrican Am",
                              "Nat Hwiin/Othr Pac Islndr" ~ "Pacific Islander",
                              "Multiple" ~ "Multiple \nRaces",
                              .default = StudentGroup
    ))


chronic.dash.graph(dist = "mpusd.abs.joint",
          dist.name = "Monterey Peninsula")



### Comparison to prior year ----


chronic.dash.comp <- function(dist, dist.name , old.colors = FALSE) {
    
    
    work.group <-   working %>%
        filter(District == dist
               ) %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    dash2 <- dash.all %>%
        filter(str_detect(districtname, dist.name),
               reportingyear == yr - 1, 
               rtype == "D",
               Group %in% work.group,
               indicator == "CHRO" 
        ) %>%
        select(districtname, indicator, currstatus, Group, color) %>%
        mutate(EstimatedColor = case_when(old.colors == FALSE ~ "Light Gray",
                                          color == 1 ~ "Red",
                                          color == 2 ~ "Orange",
                                          color == 3 ~ "Yellow",
                                          color == 4 ~ "Green",
                                          color == 5 ~ "Blue",
                                          color == 0 ~ "White"
        )
        
                                           ) %>%
        rename(PercentChronicAbsent = currstatus)  %>%
        mutate(year = "1old")
    
    
    
    
df <-    working %>%
        filter(District == dist
               ) %>%
        mutate(PercentChronicAbsent = as.numeric(PercentChronicAbsent)) %>%
    mutate(
        year = "2new") %>%
        bind_rows(dash2) %>%
    mutate(EstimatedColor = factor(EstimatedColor),
           EstimatedColor = fct_relevel(EstimatedColor,"Light Gray" ) ,
           year = factor(year),
           year = fct_relevel(year,"1old" ) ,
           
    )

        
# Sorts only by the current year
 leveler <- df %>% 
     filter(EstimatedColor != "Light Gray") %>%
 arrange(PercentChronicAbsent) 

 levelss <- leveler$Group %>% union(working$Group %>% unique())



df %>%
    mutate(# Group = factor(Group, levels = levelss), # Sorts only by the current year
           EstimatedColor = fct_relevel(EstimatedColor,"Light Gray" ) # Puts gray to the left of color
           ) %>%
    ggplot(aes(x = Group, y = PercentChronicAbsent, group = year)) +
#    ggplot(aes(x = fct_reorder(Group,PercentChronicAbsent), y = PercentChronicAbsent)) +
        geom_col_pattern(aes(fill = EstimatedColor,
                             pattern = year,
                     color = "black"),
                 position = "dodge2") +
    {if(old.colors==TRUE)scale_pattern_manual(values=c('stripe', 'wave'))else scale_pattern_manual(values=c('wave', 'wave'))    } +
    
        mcoe_theme +
    {if(length(unique(df$Group)) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
        scale_fill_identity() +
        scale_color_identity() +
    theme(legend.position = "none") +
    
        labs(y = "Percent Chronically Absent",
             title = paste0(dist.name," - Chronically Absent Student Group Estimates ", thisyear ,""),
             subtitle = if_else(old.colors == FALSE,
                                paste0("Gray is ", lastyear, " results and Colored bars are ", thisyear ," with the estimated Dashboard color"),
                                paste0("", lastyear, " results are on the left and ", thisyear ," estimates are on the right for each student group")
             )
        )

    ggsave(here("output",save.folder ,paste0(dist.name," - Chronically Absent Student Group Results ", lastyear, " and ", thisyear ," Comparison ",if_else(old.colors == TRUE, "old colors ",""), Sys.Date(),".png")), width = 8, height = 5)

}


chronic.dash.comp(dist = "mpusd.abs.joint",
                  dist.name = "Monterey Peninsula",
                  old.colors = TRUE)




### School Graphs ----


chr.joint.school <- function(df, df.demo, dist.name, grade.filt = TRUE) {
    
    
    df.calpads2 <- df %>%
        mutate(keeper = case_when(grade.filt == FALSE ~ TRUE, 
                                  Grade %in% c("TK","KN",1,2,3,4,5,6,7,8,  "01","02","03","04","05","06","07","08" ,  "1.0","2.0","3.0","4.0","5.0","6.0","7.0","8.0"   ) ~ TRUE,  
                                  TRUE ~ FALSE)
        ) %>%
        filter(DaysExpectedA >= 1,
               keeper == TRUE
               #  Grade %in% c("TK","KN",1,2,3,4,5,6,7,8,  "01","02","03","04","05","06","07","08" ,  "1.0","2.0","3.0","4.0","5.0","6.0","7.0","8.0"   )
        ) %>%
        group_by(SSID, StudentName, SchoolName, SchoolCode, Grade) %>% #, Ethnicity, EnglishLearner, SocioEconomicallyDisadvantaged) %>%
        summarise(across(.cols =   c(DaysExpectedA:DaysAbsentCEFG),
                         ~ sum(.x, na.rm = TRUE)
        )
        ) %>%
        filter(DaysExpectedA >= 31) %>%
        mutate(AbsenceRate2 = 100*DaysAbsentCEFG/DaysExpectedA,
               chronic = if_else(AbsenceRate2 >= 10, TRUE, FALSE),
               dupes = duplicated(SSID))
    
    
    df.calpads.demo2 <- df.demo %>%
      mutate( LTEL = case_when(ELAStatus == "EL" & mdy(ELAStatusStartDate) <= ymd( paste0(yr -7,"-06-15")) ~ "Y",
                               TRUE ~ "N")
      ) %>%
        select(SSID, EthnicityRace, Homeless, StudentswithDisabilities, EnglishLearner = EnglishLearner_ReclassifiedFluentEnglishProficient, LTEL, SocioEconomicallyDisadvantaged) %>%
        distinct() %>%
        group_by(SSID) %>%
        mutate(Homeless = if_else(any(Homeless == "Y"), "Yes", "N" ),
               StudentswithDisabilities = if_else(any(StudentswithDisabilities == "Y"), "Yes", "N" ),
               EnglishLearner = if_else(any(EnglishLearner == "Y"), "Yes", "N" ),
               LTEL = if_else(any(LTEL == "Y"), "Yes", "N" ),
               SocioEconomicallyDisadvantaged = if_else(any(SocioEconomicallyDisadvantaged == "Y"), "Yes", "N" ),
        ) %>%
        distinct()  %>%
        mutate(All = "Yes") %>%
        pivot_wider(names_from = EthnicityRace, values_from = All) %>%
        mutate(All = "Yes")
    
joint <- df.calpads2 %>%
        left_join(df.calpads.demo2)
    
joint


}


mpusd.abs.school.joint <- chr.joint.school(mpusd.abs.24, mpusd.demo.24)

# soledad.calpads.school.joint <- joint.school(soledad.calpads, soledad.calpads.demo)

car.school <- function(df,students) {
    
    ddff <-     deparse(substitute(df)) 
    studentsss <-     deparse(substitute(students))
    
    holder <-  df %>% 
        ungroup() %>%
        filter({{students}} == "Yes")  %>%
        
        mutate( # dist.standard = ScaleScore - MeetStandard,
            chronic.rate = 100*mean(chronic),
            count = n())  %>%
        select(chronic.rate, count) %>%
        distinct() %>%
        mutate(district = ddff,
               students = studentsss
        )
    
    # sheet_append(ss = sheet,
    #              sheet = "Distance from Standard Group",
    #              data = holder )
    holder
    
}


add.school.car <- function(df) {
    
    namer <- unique(df$SchoolName)
    coder <- unique(df$SchoolCode)
    
    waiting.room <- car.school(df,All) %>%
        bind_rows(  car.school(df,White) ) %>%
        bind_rows(  car.school(df,EnglishLearner) ) %>%
      bind_rows(  car.school(df,LTEL) ) %>%
      
              bind_rows( car.school(df,Asian) )  %>%
        bind_rows( car.school(df,Filipino) )  %>%
#        bind_rows( car.school(df,Multiple) )  %>%
        bind_rows( car.school(df,`Black/African Am`) )  %>%
        bind_rows( car.school(df,`Am Indian/Alskn Nat`) )  %>%
        bind_rows( car.school(df,`Nat Hwiin/Othr Pac Islndr`) )  %>%
        bind_rows( car.school(df,Hispanic) )  %>%
        bind_rows( car.school(df,StudentswithDisabilities) )  %>%
        bind_rows( car.school(df,SocioEconomicallyDisadvantaged) )  %>%
        bind_rows( car.school(df,Homeless) ) %>%
        mutate(SchoolName = namer,
               SchoolCode = coder
        )
    
    waiting.room
    
    
}


mpusd.abs.school.joint %>% 
    filter(str_detect(SchoolName,"Los Arboles"),
           # Grade %in% c("KN","1","2"),
           # StudentswithDisabilities == "Yes"
    ) %>%
    add.school.car()





mcoe.abs.school.joint %>%
    filter(str_detect(SchoolName,"Well"),
    ) %>%
    add.school.car()



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



dash.school.chr <- function(cdsCode) {
    
    dash.all %>%
        filter(cds == cdsCode,
               reportingyear == yr - 1, 
               rtype == "S",
               indicator == "CHRO")
    
}

chron.comp.school <- function(df, dist.code, school.code, limit.case.count = TRUE, old.colors = FALSE ) {
    
    cds <- paste0("27",dist.code, str_pad(school.code, 7, side="left", pad="0"))


    work.group <-   df %>%
        filter(SchoolCode == school.code #| SchoolCode == as.numeric(str_pad(school.code, 7, side="left", pad="0"))
        ) %>%
        filter(if(limit.case.count == TRUE )count >= 30 else count >= 10) %>%
        ungroup() %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    print(work.group)

    dash2 <- dash.school.chr( cds ) %>%
        filter(# str_detect(districtname, dist.name),
            Group %in% work.group
        ) %>%
        select(districtname, indicator, currstatus, Group, color) %>%
        mutate(EstimatedColor = case_when(old.colors == FALSE ~ "Light Gray",
                                          color == 1 ~ "Red",
                                          color == 2 ~ "Orange",
                                          color == 3 ~ "Yellow",
                                          color == 4 ~ "Green",
                                          color == 5 ~ "Blue",
                                          color == 0 ~ "White"
        )
        
        ) %>%
        rename(chronic.rate = currstatus) %>%
        mutate(year = "1old")

    print(dash2)
    
    
    df %>%
        filter(SchoolCode == school.code # | SchoolCode == as.numeric(str_pad(school.code, 7, side="left", pad="0"))
        ) %>%
        filter(if(limit.case.count == TRUE )count >= 30 else count >= 10) %>%
  #      mutate(DFS = as.numeric(DFS)) %>%
        left_join(dash2, by = c("Group")) %>%
        mutate(change = chronic.rate.x - chronic.rate.y,
               EstimatedColor = case_when(
                   count < 30 ~ "White",
                   
                   # All Schools
                   chronic.rate.x >=20 & change > -0.5 ~ "Red",
                   chronic.rate.x >=20 & change <= -3.0 ~ "Yellow",
                   chronic.rate.x >=20 & change <= -0.5 ~ "Orange",
                   
                   chronic.rate.x >=10 & change >= 3.0 ~ "Red",
                   chronic.rate.x >=10 & change <= -0.5 ~ "Yellow", 
                   chronic.rate.x >=10 & change < 3.0 ~ "Orange", 
                   
                   chronic.rate.x >=5 & change > 0.5 ~ "Orange",
                   chronic.rate.x >=5 & change <= -0.5 ~ "Green",
                   chronic.rate.x >=5 & change < 0.5 ~ "Yellow",

                   chronic.rate.x >=2.5 & change >= 3.0 ~ "Orange",
                   chronic.rate.x >=2.5 & change <= -3.0 ~ "Blue",    
                   chronic.rate.x >=2.5 & change <= 0.5 ~ "Green",    
                   chronic.rate.x >=2.5 & change < 3.0 ~ "Yellow",    
                   
                   chronic.rate.x < 2.5 & change >= 3.0 ~ "Yellow",
                   chronic.rate.x < 2.5 & change <= 0.5 ~ "Blue",
                   chronic.rate.x < 2.5 & change  < 3.0 ~ "Green",
                   
                   TRUE ~ EstimatedColor
                   
               ),
    chronic.rate = chronic.rate.x
        ) %>%
        mutate(
            year = "2new") %>%
        
        
        bind_rows(dash2) %>%
        mutate(EstimatedColor = factor(EstimatedColor),
               EstimatedColor = fct_relevel(EstimatedColor,"Light Gray" ) ,
               year = factor(year),
               year = fct_relevel(year,"1old" ) ,
               
        )
    
    
    
    
}



test <- chron.comp.school(holder, dist.code = 66225, school.code = 6115182)


chron.school.graph <- function(df) {
    
    
    
    work.group <-   df %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    
    skul <- df$SchoolName[1]
    
    df %>%
        filter(!is.na(district)) %>%
        ggplot(aes(x = Group, y = chronic.rate)) +
        geom_col(aes(fill = EstimatedColor,
                     color = "black"),
                 position = "dodge2") +
        {if(length(work.group) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
        
        mcoe_theme +
        scale_fill_identity() +
        scale_color_identity() +
        labs(y = "Chronic Absenteeism Rate",
             title = paste0(skul, " Chronic Absenteeism Student Group Estimates ", thisyear),
            # subtitle = "Gray is 2023 results and Colored bars are 2024 with the estimated Dashboard color"
             )
    
    
    ggsave(here("output",save.folder ,paste0(skul," Chronic Student Group Estimates ",thisyear , Sys.Date(),".png")), width = 8, height = 5)
    
    
} 


chron.comp.school.graph <- function(df, old.colors = FALSE) {
    
    
    
    work.group <-   df %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    
    skul <- unique(df$SchoolName)[1]

    df %>%
        ggplot(aes(x = Group, y = chronic.rate, group = year)) +
        geom_col_pattern(aes(fill = EstimatedColor,
                             pattern = year,
                             color = "black"),
                         position = "dodge2") +
        {if(old.colors==TRUE)scale_pattern_manual(values=c('stripe', 'wave'))else scale_pattern_manual(values=c('wave', 'wave'))    } +
        {if(length(work.group) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
        
        mcoe_theme +
        scale_fill_identity() +
        scale_color_identity() +
        theme(legend.position = "none") +
        labs(y = "Chronic Absenteeism Rate",
             title = paste0(skul," - Chronically Absent Student Group Estimates ", thisyear ,""),
             subtitle = if_else(old.colors == FALSE,
                                paste0("Gray is ", lastyear, " results and Colored bars are ", thisyear ," with the estimated Dashboard color"),
                                paste0("", lastyear, " results are on the left and ", thisyear ," estimates are on the right for each student group")
             )
        )
    
    
        ggsave(here("output",save.folder ,paste0(skul," - Chronically Absent Student Group Results ", lastyear, " and ", thisyear ," Comparison ",if_else(old.colors == TRUE, "old colors ",""), Sys.Date(),".png")), width = 8, height = 5)
    
    
} 

chron.comp.school(holder, dist.code = 75440, school.code = 6026678, limit.case.count = TRUE, old.colors = TRUE) %>%
    chron.comp.school.graph(old.colors = TRUE)

chron.comp.school(holder, dist.code = 66092, school.code = 6026181, limit.case.count = TRUE) %>%
    chron.school.graph()



chron.all.schools <- function(df, dist.cd, limit.case.cnt = TRUE, old.culrs = TRUE) {
    

holder <- df %>%
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


school.list <- holder$SchoolCode %>% unique()

for (i in 1:length(school.list)) {
    
chron.df <- chron.comp.school(df = holder, dist.code = dist.cd, school.code = school.list[i], limit.case.count = limit.case.cnt, old.colors = old.culrs) 

chron.comp.school.graph(chron.df, old.colors = old.culrs)
chron.school.graph(chron.df)
    
 #   ggsave(here("output",save.folder ,paste0(school.list[i], " - ","Chronic Absenteeism Student Group Results 2023 and 2024 Comparison ", if_else(old.culrs == TRUE, "old colors ",""), Sys.Date(),".png")), width = 8, height = 5)
    
}


}


#nmcusd.calpads.school.joint <- joint.school(nmcusd.calpads, nmcusd.calpads.demo)
chron.all.schools(mpusd.abs.school.joint , dist.cd = 66092)





### High Schools -----



chr.hs.joint.school <- function(df, df.demo, dist.name) {
    
    
    df.calpads2 <- df %>%
        filter(DaysExpectedA >= 1,
               Grade %in% c( 9, 10, 11, 12,  "09","10","11","12"   )
        ) %>%
        group_by(SSID, StudentName, SchoolName, SchoolCode, Grade) %>% #, Ethnicity, EnglishLearner, SocioEconomicallyDisadvantaged) %>%
        summarise(across(.cols =   c(DaysExpectedA:DaysAbsentCEFG),
                         ~ sum(.x, na.rm = TRUE)
        )
        ) %>%
        filter(DaysExpectedA >= 31) %>%
        mutate(AbsenceRate2 = 100*DaysAbsentCEFG/DaysExpectedA,
               chronic = if_else(AbsenceRate2 >= 10, TRUE, FALSE),
               dupes = duplicated(SSID))
    
    
    df.calpads.demo2 <- df.demo %>%
        select(SSID, EthnicityRace, Homeless, StudentswithDisabilities, EnglishLearner, SocioEconomicallyDisadvantaged) %>%
        distinct() %>%
        group_by(SSID) %>%
        mutate(Homeless = if_else(any(Homeless == "Y"), "Yes", "N" ),
               StudentswithDisabilities = if_else(any(StudentswithDisabilities == "Y"), "Yes", "N" ),
               EnglishLearner = if_else(any(EnglishLearner == "Y"), "Yes", "N" ),
               SocioEconomicallyDisadvantaged = if_else(any(SocioEconomicallyDisadvantaged == "Y"), "Yes", "N" ),
        ) %>%
        distinct()  %>%
        mutate(All = "Yes") %>%
        pivot_wider(names_from = EthnicityRace, values_from = All) %>%
        mutate(All = "Yes")
    
    joint <- df.calpads2 %>%
        left_join(df.calpads.demo2)
    
    joint
    
    
}


nmcusd.hs.abs.joint <- chr.hs.joint.school(nmcusd.abs.24, nmcusd.demo.24)




chron.hs.schools <- function(df) {
    
    
    holder <- df %>%
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
    
    
    school.list <- holder$SchoolCode %>% unique()
    
    print(school.list)
    
    for (i in 1:length(school.list)) {
        
        chron.df <- holder %>% 
            filter(SchoolCode == school.list[i]) 
        
        print(chron.df)
 
        skul <- chron.df$SchoolName[1]

        chron.df %>%
            ggplot(aes(x = Group, y = chronic.rate)) +
            geom_col(aes(fill = "pink",
                         color = "black"),
                     position = "dodge2")  +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) + #Fixes the overlapping axis labels to make them alternate if lots of columns

            mcoe_theme +
            scale_fill_identity() +
            scale_color_identity() +
            labs(y = "Chronic Absenteeism Rate",
                 title = paste0(skul, " - Chronic Absenteeism Student Group Estimates ", thisyear),
                 # subtitle = "Gray is 2023 results and Colored bars are 2024 with the estimated Dashboard color"
            )


        ggsave(here("output",save.folder ,paste0(skul," Chronic Estimates ", thisyear, Sys.Date(),".png")), width = 8, height = 5)
        

    }
    
    
}

chron.hs.schools(nmcusd.hs.abs.joint)


nmcusd.hs.abs.joint %>%
    filter(str_detect(SchoolName, "North Monterey County High")) %>% 
    chron.hs.schools()








chron.school.graph <- function(df) {
    
    
} 









chron.hs.graph <- function(df, dist.code, school.code, school.name) {
  

chr.hs.old <- tbl(con,"CHRONIC") %>%
    filter(#countyname == "Monterey",
           county_code == 27,
           district_code == dist.code, # 73825, # 75440
           school_code == school.code, # 2730034, # 2730190
           academic_year == max(academic_year)
           # rtype == "S",
           # indicator == "chronic"
           ) %>%
    collect()  %>%
    mutate(Group = case_match(reporting_category,
                              "SH" ~ "Homeless",
                              "SD" ~ "Students with \nDisabilities",
                              "SS" ~ "Socio-Economically \nDisadvantaged",
                              "RH" ~ "Latino",
                              "SE" ~ "English Learner",
                              "RA" ~ "Asian",
                              "RB" ~ "Black/African Am",
                              
                              "RF" ~ "Filipino",
                              "RW" ~ "White",
                              "TA" ~ "All",
                              .default = NA
    ),
    EstimatedColor = "Light Gray",
    chronic.rate = chronic_absenteeism_rate)  %>%
    filter(!is.na( chronic_absenteeism_rate ),
           !is.na( Group )
    )
    



df %>%
     filter(
         SchoolCode == school.code # 2730034, # 2730190
     ) %>%
    add.school.car()  %>%
    mutate(Group = case_match(students,
                              "StudentswithDisabilities" ~ "Students with \nDisabilities",
                              "SocioEconomicallyDisadvantaged" ~ "Socio-Economically \nDisadvantaged",
                              "Hispanic" ~ "Latino",
                              "EnglishLearner" ~ "English Learner",
                              .default = students
    ),
    EstimatedColor = "Pink") %>%
    filter(count > 10) %>%
    bind_rows(chr.hs.old) %>%
    ggplot(aes(x = Group, y = chronic.rate)) +
    geom_col(aes(fill = EstimatedColor,
                 color = "black"),
             position = "dodge2") +
    mcoe_theme +
    scale_fill_identity() +
    scale_color_identity() +
    labs(y = "Chronic Absenteeism Rate",
         title = paste0(school.name, " Chronic Absenteeism Student Group Results 2023"),
         subtitle = "Gray is 2022 results and Pink is 2023. There are no Dashboard colors for High School.")


    ggsave(here("output",save.folder ,paste0(school.name, " - ","Chronic Absenteeism Student Group Results 2022 and 2023 Comparison ", Sys.Date(),".png")), width = 8, height = 5)
 
    
}

chron.hs.graph(nmcusd.calpads.school.joint, 73825, 2730034, "North Monterey High" )

chron.hs.graph(soledad.calpads.school.joint, 75440, 6026686, "Soledad High" )


### Run Everything with working functions --------


mpusd.abs.24 <- read_csv(here("data", "mpusd" , "14.2_StudentAbsencesStudentList.InReviewUncertified_20240715.csv"))
mpusd.demo.24 <- read_csv(here("data", "mpusd" , "8.1_StudentProfileList(EOY3).InReviewUncertified_20240715.csv"))


mpusd.abs.joint <- calpads.join(mpusd.abs.24, mpusd.demo.24)

chronic.group.rate(mpusd.abs.joint, EthnicityRace)
chronic.group.rate(mpusd.abs.joint, Homeless)
chronic.group.rate(mpusd.abs.joint, StudentswithDisabilities)
chronic.group.rate(mpusd.abs.joint, EnglishLearner)
chronic.group.rate(mpusd.abs.joint, SocioEconomicallyDisadvantaged)
chronic.group.rate(mpusd.abs.joint, All)


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
                              "LTEL" ~ "Long Term\nEnglish\nLearner",
                              
                              "Black/African Am" ~ "Black/\nAfrican Am",
                              "Nat Hwiin/Othr Pac Islndr" ~ "Pacific Islander",
                              "Multiple" ~ "Multiple \nRaces",
                              .default = StudentGroup
    ))


chronic.dash.graph(dist = "mpusd.abs.joint",
                   dist.name = "Monterey Peninsula")

chronic.dash.comp(dist = "mpusd.abs.joint",
                  dist.name = "Monterey Peninsula")


mpusd.abs.school.joint <- chr.joint.school(mpusd.abs.24, mpusd.demo.24)

chron.all.schools(mpusd.abs.school.joint , dist.cd = 66092)





# Run all doesn't work for now because of passing df names to google sheets and then retrieving

# run.all.chronic <- function(df.abs, df.demo, dist.namer , dist.code) {
#     
#     abs.joint <- calpads.join(df.abs, mpusd.demo.24)
#     
#     chronic.group.rate(abs.joint, EthnicityRace)
#     chronic.group.rate(abs.joint, Homeless)
#     chronic.group.rate(abs.joint, StudentswithDisabilities)
#     chronic.group.rate(abs.joint, EnglishLearner)
#     chronic.group.rate(abs.joint, SocioEconomicallyDisadvantaged)
#     chronic.group.rate(abs.joint, All)
#     
#     
#     working <- read_sheet(ss = sheet,
#                           sheet = "Chronic Group") %>%
#         filter(StudentGroup != "N",
#                StudentGroup != "Missing",
#                NumberStudents >= 30) %>%
#         mutate(Group = case_match(StudentGroupCategory,
#                                   "All" ~ "All",
#                                   "Homeless" ~ "Homeless",
#                                   "StudentswithDisabilities" ~ "Students with \nDisabilities",
#                                   "SocioEconomicallyDisadvantaged" ~ "Socio-Economically \nDisadvantaged",
#                                   "Hispanic" ~ "Latino",
#                                   "EnglishLearner" ~ "English \nLearner",
#                                   "Black/African Am" ~ "Black/\nAfrican Am",
#                                   "Nat Hwiin/Othr Pac Islndr" ~ "Pacific Islander",
#                                   "Multiple" ~ "Multiple \nRaces",
#                                   .default = StudentGroup
#         ))
#     
#     
#     chronic.dash.graph(dist = "abs.joint",
#                        dist.name = dist.namer)
#     
#     chronic.dash.comp(dist = "abs.joint",
#                       dist.name = dist.namer)
#     
#     
#     abs.school.joint <- joint.school(df.abs, mpusd.demo.24)
#     
#     chron.all.schools(abs.school.joint , dist.cd = dist.code)
#     
#     
#     
# }
# 
# 
# run.all.chronic(mpusd.abs.24, mpusd.demo.24,"Monterey Peninsula", 66092 )




### BY Grade level by site ------

nmcusd.abs.school.joint <- chr.joint.school(nmcusd.abs.24, nmcusd.demo.24, "North Monterey County" , FALSE)


nmcusd.abs.school.long <- nmcusd.abs.school.joint %>%
    pivot_longer(cols = c(Homeless:All)) %>%
    filter(value == "Yes")

nmcusd.abs.school.sum <- nmcusd.abs.school.long %>%
    group_by(
        SchoolName,
        name,
    #    Grade
        ) %>%
    summarise(perc = 100*mean(chronic),
              n = n()) %>%
    mutate(
        #Grade = factor(Grade, levels = c("TK","KN", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")),
         #  Grade = fct_relevel(Grade, "TK","KN"),
           name = str_replace(name, "/", " - ")
           )




# 
# nmcusd.abs.school.sum %>%
#     filter(name == "All",
#            n >=10) %>%
#     lollipop( perc, Grade, "pink")
# 
# ggsave(here("output",save.folder,"chronic","example 1.png"))

colorme <- "seagreen"


for (i in unique(nmcusd.abs.school.sum$name)) {
    
nmcusd.abs.school.sum %>%
    filter(name == i,
           n >=10) %>%
ggplot2::ggplot( aes( y = perc/100,
                      x = Grade, #forcats::fct_reorder(District_Name,Percentage_Standard_Met_and_Above) ,
                      label = scales::percent(perc/100, accuracy = .1))) +
    geom_segment( aes(x= Grade, #forcats::fct_reorder(District_Name, Percentage_Standard_Met_and_Above/100),
                      xend= Grade, #forcats::fct_reorder(District_Name, Percentage_Standard_Met_and_Above/100),
                      y=0,
                      yend=perc/100),
                  color=colorme,
                  size =2 ) +
    geom_point( color=colorme, size=5, alpha=0.6) +
    coord_flip() +
    geom_text(size = 3, color = "black") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    #  facet_grid(facets = vars(`Student Group`), scales = "free" ) +
    theme_hc() +
    mcoe_theme +
        labs(title = paste0("2023-24 Chronic Absenteeism Rates by Grade for ",i, " students"))
    
 ggsave(here("output",save.folder, "chronic" , paste0("2023-24 Chronic Absenteeism Rates by Grade for ",i,".png")), width = 8, height = 4.5 )

}



for (i in unique(nmcusd.abs.school.sum$Grade)) {
    

nmcusd.abs.school.sum %>%
    filter(Grade == i,
           n >=10) %>%
    lollipop( perc, name, "orange")+
        labs(title = paste0("2023-24 Chronic Absenteeism Rates by Student Group for Grade ",i))
    

ggsave(here("output",save.folder, "chronic" , paste0("2023-24 Chronic Absenteeism Rates by Student Group for ",i,".png")), width = 8, height = 4.5 )

}







for (i in unique(nmcusd.abs.school.sum$SchoolName)) {
    
    
    colorme <- "bisque4"
    
    nmcusd.abs.school.sum %>%
        filter(SchoolName == i,
               n >=10) %>%
        ggplot2::ggplot( aes( y = perc/100,
                              x = Grade, #forcats::fct_reorder(District_Name,Percentage_Standard_Met_and_Above) ,
                              label = scales::percent(perc/100, accuracy = .1))) +
        geom_segment( aes(x= Grade, #forcats::fct_reorder(District_Name, Percentage_Standard_Met_and_Above/100),
                          xend= Grade, #forcats::fct_reorder(District_Name, Percentage_Standard_Met_and_Above/100),
                          y=0,
                          yend=perc/100),
                      color=colorme,
                      size =2 ) +
        geom_point( color=colorme, size=5, alpha=0.6) +
        coord_flip() +
        geom_text(size = 3, color = "black") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        #  facet_grid(facets = vars(`Student Group`), scales = "free" ) +
        theme_hc() +
        mcoe_theme +
        labs(title = paste0("2023-24 Chronic Absenteeism Rates by Grade for ",i))
    
    ggsave(here("output",save.folder, "chronic" , paste0("2023-24 Chronic Absenteeism Rates by Grade for ",i,".png")), width = 8, height = 4.5 )
    
}






for (i in unique(nmcusd.abs.school.sum$SchoolName)) {
    
    
    nmcusd.abs.school.sum %>%
        filter(SchoolName == i,
               n >=10) %>%
        lollipop( perc, name, "cyan")+
        labs(title = paste0("2023-24 Chronic Absenteeism Rates by Student Group for ",i))
    
    
    ggsave(here("output",save.folder, "chronic" , paste0("2023-24 Chronic Absenteeism Rates by Student Group for ",i,".png")), width = 8, height = 4.5 )
    
}


nmcusd.curr <- read_xlsx(here("data", "nmcusd" ,"2024-2025 NMCUSD Weekly Student Attendance_8.14-10.11.xlsx"))









##### END -------
