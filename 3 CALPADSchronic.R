
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
        select(SSID, EthnicityRace, Homeless, StudentswithDisabilities, EnglishLearner, SocioEconomicallyDisadvantaged) %>%
        distinct() %>%
        group_by(SSID) %>%
        mutate(Homeless = if_else(any(Homeless == "Y"), "Y", "N" ),
               StudentswithDisabilities = if_else(any(StudentswithDisabilities == "Y"), "Y", "N" ),
               EnglishLearner = if_else(any(EnglishLearner == "Y"), "Y", "N" ),
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
             title = paste0(dist.name," - Chronically Absent Student Group Estimates 2024"))
    
    
    ggsave(here("output",save.folder ,paste0(dist.name, " - Chronically Absent Student Group Estimates 2024 ", Sys.Date(),".png")), width = 8, height = 5)    
    
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
                              "Black/African Am" ~ "Black/\nAfrican Am",
                              "Nat Hwiin/Othr Pac Islndr" ~ "Pacific Islander",
                              "Multiple" ~ "Multiple \nRaces",
                              .default = StudentGroup
    ))


chronic.dash.graph(dist = "mpusd.abs.joint",
          dist.name = "Monterey Peninsula")



### Comparison to prior year ----


chronic.dash.comp <- function(dist, dist.name ) {
    
    
    work.group <-   working %>%
        filter(District == dist
               ) %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    dash2 <- dash %>%
        filter(str_detect(districtname, dist.name),
               rtype == "D",
               Group %in% work.group,
               indicator == "CHRO" 
        ) %>%
        select(districtname, indicator, currstatus, Group) %>%
        mutate(EstimatedColor = "Light Gray") %>%
        rename(PercentChronicAbsent = currstatus)
    
    
    
df <-    working %>%
        filter(District == dist
               ) %>%
        mutate(PercentChronicAbsent = as.numeric(PercentChronicAbsent)) %>%
        bind_rows(dash2) %>%
        mutate(EstimatedColor = as_factor(EstimatedColor)) 
        
# Sorts only by the current year
 leveler <- df %>% 
     filter(EstimatedColor != "Light Gray") %>%
     arrange(PercentChronicAbsent) 

 levelss <- leveler$Group %>% union(working$Group %>% unique())


df %>%   
    mutate(# Group = factor(Group, levels = levelss), # Sorts only by the current year
           EstimatedColor = fct_relevel(EstimatedColor,"Light Gray" ) # Puts gray to the left of color
           ) %>%
    ggplot(aes(x = Group, y = PercentChronicAbsent)) +
#    ggplot(aes(x = fct_reorder(Group,PercentChronicAbsent), y = PercentChronicAbsent)) +
        geom_col(aes(fill = EstimatedColor,
                     color = "black"),
                 position = "dodge2") +
        mcoe_theme +
    {if(length(unique(df$Group)) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
        scale_fill_identity() +
        scale_color_identity() +
        labs(y = "Percent Chronically Absent",
             title = paste0(dist.name," - Chronically Absent Student Group Estimates 2024"),
             subtitle = "Gray is 2023 results and Colored bars are 2024 with the estimated Dashboard color")
    
    
    ggsave(here("output",save.folder ,paste0(dist.name," - Chronically Absent Student Group Results 2023 and 2024 Comparison ", Sys.Date(),".png")), width = 8, height = 5)    
    
}



chronic.dash.comp(dist = "mpusd.abs.joint",
                  dist.name = "Monterey Peninsula")




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
        bind_rows( car.school(df,Asian) )  %>%
        bind_rows( car.school(df,Filipino) )  %>%
    #    bind_rows( car.school(df,Multiple) )  %>%
    #    bind_rows( car.school(df,`Black/African Am`) )  %>%
        bind_rows( car.school(df,`Am Indian/Alskn Nat`) )  %>%
   #     bind_rows( car.school(df,`Nat Hwiin/Othr Pac Islndr`) )  %>%
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
                              "Black/African Am" ~ "Black/\nAfrican Am",
                              "Nat Hwiin/Othr Pac Islndr" ~ "Pacific Islander",
                              "Multiple" ~ "Multiple \nRaces",
                              .default = students
     ))



dash.school.chr <- function(cdsCode) {
    
    dash %>%
        filter(cds == cdsCode,
               rtype == "S",
               indicator == "CHRO")
    
}

chron.comp.school <- function(df, dist.code, school.code, limit.case.count = TRUE ) {
    
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
        select(districtname, schoolname ,indicator, currstatus, Group) %>%
        mutate(EstimatedColor = "Light Gray") %>%
        rename(chronic.rate = currstatus)
    
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
                   

                   
    #               !is.na(DFS.x) & is.na(DFS.y) ~ "Black",
                   
                   TRUE ~ EstimatedColor
                   
               ),
    chronic.rate = chronic.rate.x
        ) %>%
        
        
        bind_rows(dash2) %>%
        mutate(EstimatedColor = factor(EstimatedColor),
               EstimatedColor = fct_relevel(EstimatedColor,"Light Gray" ) )
    
    
    
    
}



test <- chron.comp.school(holder, dist.code = 66225, school.code = 6115182)


chron.school.graph <- function(df) {
    
    
    
    work.group <-   df %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    
    skul <- df$schoolname[1]
    
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
             title = paste0(skul, " Chronic Absenteeism Student Group Estimates 2024"),
            # subtitle = "Gray is 2023 results and Colored bars are 2024 with the estimated Dashboard color"
             )
    
    
    ggsave(here("output",save.folder ,paste0(skul," Chronic Student Group Estimates 2024 ", Sys.Date(),".png")), width = 8, height = 5)
    
    
} 


chron.comp.school.graph <- function(df) {
    
    
    
    work.group <-   df %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    
    skul <- df$schoolname[1]

    df %>%
        ggplot(aes(x = Group, y = chronic.rate)) +
        geom_col(aes(fill = EstimatedColor,
                     color = "black"),
                 position = "dodge2") +
        {if(length(work.group) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
        
        mcoe_theme +
        scale_fill_identity() +
        scale_color_identity() +
        labs(y = "Chronic Absenteeism Rate",
             title = paste0(skul, " Chronic Absenteeism Student Group Estimates 2024"),
             subtitle = "Gray is 2023 results and Colored bars are 2024 with the estimated Dashboard color")
    
    
        ggsave(here("output",save.folder ,paste0(skul," Chronic Student Group Results 2023 and 2024 Comparison ", Sys.Date(),".png")), width = 8, height = 5)
    
    
} 

chron.comp.school(holder, dist.code = 66092, school.code = 6026181, limit.case.count = TRUE) %>%
    chron.comp.school.graph()

chron.comp.school(holder, dist.code = 66092, school.code = 6026181, limit.case.count = TRUE) %>%
    chron.school.graph()



chron.all.schools <- function(df, dist.cd, limit.case.cnt = TRUE) {
    

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
                              "Black/African Am" ~ "Black/\nAfrican Am",
                              "Nat Hwiin/Othr Pac Islndr" ~ "Pacific Islander",
                              "Multiple" ~ "Multiple \nRaces",
                              .default = students
    ))


school.list <- holder$SchoolCode %>% unique()

for (i in 1:length(school.list)) {
    
chron.df <- chron.comp.school(df = holder, dist.code = dist.cd, school.code = school.list[i], limit.case.count = limit.case.cnt) 

chron.comp.school.graph(chron.df)
chron.school.graph(chron.df)
    
 #   ggsave(here("output",save.folder ,paste0(school.list[i], " - ","Chronic Absenteeism Student Group Results 2023 and 2024 Comparison ", Sys.Date(),".png")), width = 8, height = 5)
    
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
                 title = paste0(skul, " - Chronic Absenteeism Student Group Estimates 2024"),
                 # subtitle = "Gray is 2023 results and Colored bars are 2024 with the estimated Dashboard color"
            )


        ggsave(here("output",save.folder ,paste0(skul," Chronic Estimates 2024 ", Sys.Date(),".png")), width = 8, height = 5)
        

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


##### END -------
