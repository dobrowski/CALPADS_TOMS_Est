
# Looks at CALPADS Suspension data to give dashboard estimates 
# Need CALPADS 7.12 and 8.1 reports



mpusd.sus.24 <- read_csv(here("data", "mpusd" , "7.12_IncidentResultsStudentList.InReviewUncertified_20240715.csv"))
# mpusd.demo.24 <- read_csv(here("data", "mpusd" , "8.1_StudentProfileList(EOY3).InReviewUncertified_20240715.csv"))

# 
# sol.susp <- read_excel(here("data","soledad","7.12_IncidentResultsStudentList (1).xlsx"),
#                              range = "C9:AD802")
# 
# 
# 
# somoco.susp <- read_csv(here("data","somoco","susp" ,"7.12_IncidentResultsStudentList.csv"))
# 
# somoco.demo <- read_csv(here("data","somoco","susp" ,"8.1_StudentProfileList (1).csv")) %>%
#     rename(EthnicityRace = Ethnicity_Race_Name,
#            SocioEconomicallyDisadvantaged = SocioEconomically)



susp.df <- function(suspenion.incidences, demographics) {
    
sol.susp.sum <- suspenion.incidences %>%
    group_by(SSID) %>%
    mutate(SSID = as.character(SSID)) %>%
    summarise(days = sum(`DurationDays`)) %>%
    filter(days >= 0.5) 


sol.susp.demo <- demographics %>%
    mutate(SSID = as.character(SSID)) %>%
    select(SSID, EthnicityRace, Homeless:SocioEconomicallyDisadvantaged) %>%
    distinct() %>%
  #  mutate(Yes = "Y") %>%
 #   pivot_wider(names_from = EthnicityRace, values_from = Yes, values_fill = "N")
    group_by(SSID) %>%
    mutate(Homeless = if_else(any(Homeless == "Y"), "Y", "N" ),
           StudentswithDisabilities = if_else(any(StudentswithDisabilities == "Y"), "Y", "N" ),
           EnglishLearner = if_else(any(EnglishLearner == "Y"), "Y", "N" ),
           SocioEconomicallyDisadvantaged = if_else(any(SocioEconomicallyDisadvantaged == "Y"), "Y", "N" ),
           All = "Y"
    ) %>%
    distinct() # %>%



sol.susp.final <- sol.susp.demo %>%
    left_join(sol.susp.sum) %>%
    mutate(susp = if_else(is.na(days), FALSE, TRUE))

} 


susp.group.rate <- function(df, studentgroup) {
    
    ddff <-     deparse(substitute(df)) 
    studentsss <-     deparse(substitute(studentgroup))
    
    holder <- df %>%
        group_by({{studentgroup}}) %>%
        transmute(count = n(),
                  perc.susp = 100*mean(susp)) %>%
        distinct()%>%
        mutate(district = ddff,
               students = studentsss
        )
    
    sheet_append(ss = sheet,
                 sheet = "Susp Group",
                 data = holder )
    
    holder
}




mcoe.sus.joint <- susp.df(mcoe.sus.24,mcoe.demo.24 )

susp.group.rate(mcoe.sus.joint, EthnicityRace)
susp.group.rate(mcoe.sus.joint, Homeless)
susp.group.rate(mcoe.sus.joint, StudentswithDisabilities)
susp.group.rate(mcoe.sus.joint, EnglishLearner)
susp.group.rate(mcoe.sus.joint, SocioEconomicallyDisadvantaged)
susp.group.rate(mcoe.sus.joint, All)



### Graphing Single Year -----


susp.dash.graph <- function(dist, dist.name ) {
    
    
    work.group <-   working %>%
        filter(District == dist) %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    
    
    working %>%
        filter(District == dist) %>%
        #       mutate(DFS = as.numeric(DFS)) %>%
        ggplot(aes(x = Group, y = PercentSusp, )) +
        #        ggplot(aes(x = fct_reorder(Group,PercentChronicAbsent), y = PercentChronicAbsent, )) +
        geom_col(aes(fill = EstimatedColor, 
                     color = "black")) +
        mcoe_theme +
        {if(length(work.group) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
        scale_fill_identity() +
        scale_color_identity() +
        labs(y = "Percent Suspended",
             title = paste0(dist.name," - Suspended Student Group Estimates 2024"))
    
    
    ggsave(here("output", save.folder ,paste0(dist.name, " - Suspended Student Group Estimates 2024 ", Sys.Date(),".png")), width = 8, height = 5)    
    
}




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


susp.dash.graph(dist = "mpusd.sus.joint",
                   dist.name = "Monterey Peninsula")



### Comparison to prior year ----


susp.dash.comp <- function(dist, dist.name ) {
    
    
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
               indicator == "SUSP" 
        ) %>%
        select(districtname, indicator, currstatus, Group) %>%
        mutate(EstimatedColor = "Light Gray") %>%
        rename(PercentSusp = currstatus)
    
    
    
    df <-    working %>%
        filter(District == dist
        ) %>%
        mutate(PercentSusp = as.numeric(PercentSusp)) %>%
        bind_rows(dash2) %>%
        mutate(EstimatedColor = as_factor(EstimatedColor)) 
    
    # Sorts only by the current year
    leveler <- df %>% 
        filter(EstimatedColor != "Light Gray") %>%
        arrange(PercentSusp) 
    
    levelss <- leveler$Group %>% union(working$Group %>% unique())
    
    
    df %>%   
        mutate(# Group = factor(Group, levels = levelss), # Sorts only by the current year
            EstimatedColor = fct_relevel(EstimatedColor,"Light Gray" ) # Puts gray to the left of color
        ) %>%
        ggplot(aes(x = Group, y = PercentSusp)) +
        #    ggplot(aes(x = fct_reorder(Group,PercentChronicAbsent), y = PercentChronicAbsent)) +
        geom_col(aes(fill = EstimatedColor,
                     color = "black"),
                 position = "dodge2") +
        mcoe_theme +
        {if(length(unique(df$Group)) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
        scale_fill_identity() +
        scale_color_identity() +
        labs(y = "Percent Suspended",
             title = paste0(dist.name," - Suspended Student Group Estimates 2024"),
             subtitle = "Gray is 2023 results and Colored bars are 2024 with the estimated Dashboard color")
    
    
    ggsave(here("output",save.folder ,paste0(dist.name," - Suspended Student Group Results 2023 and 2024 Comparison ", Sys.Date(),".png")), width = 8, height = 5)    
    
}



susp.dash.comp(dist = "mcoe.sus.joint",
                  dist.name = "Monterey County")



### School Graphs ----


susp.joint.school <- function(df, df.demo, dist.name) {
    
    
    df.calpads2 <- df %>%
        mutate(SSID = as.character(SSID)) %>%
        group_by(SSID, SchoolName, SchoolCode, Grade) %>% 
        summarise(days = sum(`DurationDays`)) %>%
        filter(days >= 0.5) 
    

    df.calpads.demo2 <- df.demo %>%
        mutate(SSID = as.character(SSID)) %>%
        select(SSID, SchoolName, SchoolCode, Grade, EthnicityRace, Homeless:SocioEconomicallyDisadvantaged) %>%
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
    
    joint <- df.calpads.demo2  %>%
        left_join(df.calpads2) %>%
        mutate(susp = if_else(is.na(days), FALSE, TRUE))
    
    joint
    
    
}


mpusd.sus.school.joint <- susp.joint.school(mpusd.sus.24, mpusd.demo.24)



susp.school <- function(df,students) {
    
    ddff <-     deparse(substitute(df)) 
    studentsss <-     deparse(substitute(students))
    
    holder <-  df %>% 
        ungroup() %>%
        filter({{students}} == "Yes")  %>%
        
        mutate( # dist.standard = ScaleScore - MeetStandard,
            susp.rate = 100*mean(susp),
            count = n())  %>%
        select(susp.rate, count) %>%
        distinct() %>%
        mutate(district = ddff,
               students = studentsss
        )
    
    # sheet_append(ss = sheet,
    #              sheet = "Distance from Standard Group",
    #              data = holder )
    holder
    
}


add.school.susp <- function(df) {
    
    namer <- unique(df$SchoolName)
    coder <- unique(df$SchoolCode)
    
    waiting.room <- susp.school(df,All) %>%
        bind_rows(  susp.school(df,White) ) %>%
        bind_rows(  susp.school(df,EnglishLearner) ) %>%
        bind_rows( susp.school(df,Asian) )  %>%
        bind_rows( susp.school(df,Filipino) )  %>%
   #     bind_rows( susp.school(df,Multiple) )  %>%
        bind_rows( susp.school(df,`Black/African Am`) )  %>%
        bind_rows( susp.school(df,`Am Indian/Alskn Nat`) )  %>%
        bind_rows( susp.school(df,`Nat Hwiin/Othr Pac Islndr`) )  %>%
        bind_rows( susp.school(df,Hispanic) )  %>%
        bind_rows( susp.school(df,StudentswithDisabilities) )  %>%
        bind_rows( susp.school(df,SocioEconomicallyDisadvantaged) )  %>%
        bind_rows( susp.school(df,Homeless) ) %>%
        mutate(SchoolName = namer,
               SchoolCode = coder
        )
    
    waiting.room
    
    
}


mcoe.sus.school.joint %>% 
    filter(str_detect(SchoolName,"Well"),
           # Grade %in% c("KN","1","2"),
           # StudentswithDisabilities == "Yes"
    ) %>%
    add.school.susp()


holder <- spreck.sus.school.joint %>%
    # filter(str_detect(DistrictName,dist.name)) %>%
    split(.$SchoolName) %>%
    map_df(~add.school.susp(.))  %>%
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



dash.school.sus <- function(cdsCode) {
    
    dash %>%
        filter(cds == cdsCode,
               rtype == "S",
               indicator == "SUSP")
    
}

susp.comp.school <- function(df, dist.code, school.code, limit.case.count = TRUE ) {
    
    cds <- paste0("27",dist.code, str_pad(school.code, 7, side="left", pad="0"))
    
    ent <- tbl(con,"SCHOOLS") %>%
        filter(
            CDSCode == cds
            #        County == "Monterey"
        ) %>%
        collect()  %>%
        select(EILName) %>%
        simplify()
    
    print(ent)
    
    
    
    work.group <-   df %>%
        filter(SchoolCode == school.code #| SchoolCode == as.numeric(str_pad(school.code, 7, side="left", pad="0"))
        ) %>%
        filter(if(limit.case.count == TRUE )count >= 30 else count >= 10) %>%
        ungroup() %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    print(work.group)
    
    dash2 <- dash.school.sus( cds ) %>%
        filter(# str_detect(districtname, dist.name),
            Group %in% work.group
        ) %>%
        select(districtname, schoolname ,indicator, currstatus, Group) %>%
        mutate(EstimatedColor = "Light Gray") %>%
        rename(susp.rate = currstatus)
    
    print(dash2)
    
    
    df %>%
        filter(SchoolCode == school.code # | SchoolCode == as.numeric(str_pad(school.code, 7, side="left", pad="0"))
        ) %>%
        filter(if(limit.case.count == TRUE )count >= 30 else count >= 10) %>%
        #      mutate(DFS = as.numeric(DFS)) %>%
        left_join(dash2, by = c("Group")) %>%
        mutate(change = susp.rate.x - susp.rate.y,
               EstimatedColor = case_when(
                   count < 30 ~ "White",
                   
                   # High Schools
                   str_detect(ent,"High") & susp.rate.x >=10.1 & change > -0.2 ~ "Red",
                   str_detect(ent,"High") & susp.rate.x >=10.1 & change <= -2.0 ~ "Yellow",
                   str_detect(ent,"High") & susp.rate.x >=10.1 & change <= -0.3 ~ "Orange",
                   
                   str_detect(ent,"High") & susp.rate.x >=6.1 & change >= 3.1 ~ "Red",
                   str_detect(ent,"High") & susp.rate.x >=6.1 & change <= -0.3 ~ "Yellow", 
                   str_detect(ent,"High") & susp.rate.x >=6.1 & change < 3.0 ~ "Orange", 
                   
                   str_detect(ent,"High") & susp.rate.x >=1.6 & change >= 0.3 ~ "Orange",
                   str_detect(ent,"High") & susp.rate.x >=1.6 & change <= -0.3 ~ "Green",
                   str_detect(ent,"High") & susp.rate.x >=1.6 & change < 0.3 ~ "Yellow",
                   
                   str_detect(ent,"High") & susp.rate.x >=0.6 & change >= 0.3 ~ "Yellow",
                   str_detect(ent,"High") & susp.rate.x >=0.6 & change <= -2.0 ~ "Blue",    
                   str_detect(ent,"High") & susp.rate.x >=0.6 & change <= 0.3 ~ "Green",    

                   str_detect(ent,"High") & susp.rate.x < 0.6 & change >= 0.3 ~ "Green",
                   str_detect(ent,"High") & susp.rate.x < 0.6 & change  < 0.3 ~ "Blue",
                   
                   
                   # Middle Schools
                   str_detect(ent,"Middle") & susp.rate.x >=12.1 & change > -0.2 ~ "Red",
                   str_detect(ent,"Middle") & susp.rate.x >=12.1 & change <= -3.0 ~ "Yellow",
                   str_detect(ent,"Middle") & susp.rate.x >=12.1 & change <= -0.3 ~ "Orange",
                   
                   str_detect(ent,"Middle") & susp.rate.x >=8.1 & change >= 4.1 ~ "Red",
                   str_detect(ent,"Middle") & susp.rate.x >=8.1 & change <= -0.3 ~ "Yellow", 
                   str_detect(ent,"Middle") & susp.rate.x >=8.1 & change < 4.0 ~ "Orange", 
                   
                   str_detect(ent,"Middle") & susp.rate.x >=2.1 & change >= 0.3 ~ "Orange",
                   str_detect(ent,"Middle") & susp.rate.x >=2.1 & change <= -0.3 ~ "Green",
                   str_detect(ent,"Middle") & susp.rate.x >=2.1 & change < 0.3 ~ "Yellow",
                   
                   str_detect(ent,"Middle") & susp.rate.x >=0.6 & change >= 0.3 ~ "Yellow",
                   str_detect(ent,"Middle") & susp.rate.x >=0.6 & change <= -3.0 ~ "Blue",    
                   str_detect(ent,"Middle") & susp.rate.x >=0.6 & change <= 0.3 ~ "Green",    
                   
                   str_detect(ent,"Middle") & susp.rate.x < 0.6 & change >= 0.3 ~ "Green",
                   str_detect(ent,"Middle") & susp.rate.x < 0.6 & change  < 0.3 ~ "Blue",
                   
                   
                   # Elementary Schools
                   str_detect(ent,"Elem") & susp.rate.x >=6.1 & change > -0.2 ~ "Red",
                   str_detect(ent,"Elem") & susp.rate.x >=6.1 & change <= -1.0 ~ "Yellow",
                   str_detect(ent,"Elem") & susp.rate.x >=6.1 & change <= -0.3 ~ "Orange",
                   
                   str_detect(ent,"Elem") & susp.rate.x >=3.1 & change >= 2.1 ~ "Red",
                   str_detect(ent,"Elem") & susp.rate.x >=3.1 & change <= -0.3 ~ "Yellow", 
                   str_detect(ent,"Elem") & susp.rate.x >=3.1 & change < 2.0 ~ "Orange", 
                   
                   str_detect(ent,"Elem") & susp.rate.x >=1.1 & change >= 0.3 ~ "Orange",
                   str_detect(ent,"Elem") & susp.rate.x >=1.1 & change <= -0.3 ~ "Green",
                   str_detect(ent,"Elem") & susp.rate.x >=1.1 & change < 0.3 ~ "Yellow",
                   
                   str_detect(ent,"Elem") & susp.rate.x >=0.6 & change >= 0.3 ~ "Yellow",
                   str_detect(ent,"Elem") & susp.rate.x >=0.6 & change <= -1.0 ~ "Blue",    
                   str_detect(ent,"Elem") & susp.rate.x >=0.6 & change <= 0.3 ~ "Green",    
                   
                   str_detect(ent,"Elem") & susp.rate.x < 0.6 & change >= 0.3 ~ "Green",
                   str_detect(ent,"Elem") & susp.rate.x < 0.6 & change  < 0.3 ~ "Blue",
                   

                   TRUE ~ EstimatedColor
                   
               ),
               susp.rate = susp.rate.x
        ) %>%
        
        
        bind_rows(dash2) %>%
        mutate(EstimatedColor = factor(EstimatedColor),
               EstimatedColor = fct_relevel(EstimatedColor,"Light Gray" ) )
    

}



test <- susp.comp.school(holder, dist.code = 66092, school.code = 6026181)



susp.school.graph <- function(df) {
    
    
    work.group <-   df %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    
    skul <- df$schoolname[1]
    
    df %>%
        filter(!is.na(district)) %>%
        ggplot(aes(x = Group, y = susp.rate)) +
        geom_col(aes(fill = EstimatedColor,
                     color = "black"),
                 position = "dodge2") +
        {if(length(work.group) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
        mcoe_theme +
        scale_fill_identity() +
        scale_color_identity() +
        labs(y = "Suspension Rate",
             title = paste0(skul, " Suspension Student Group Estimates 2024"),
    #         subtitle = "Gray is 2023 results and Colored bars are 2024 with the estimated Dashboard color"
             )
    
    
    ggsave(here("output",save.folder ,paste0(skul," Suspension Group Results 2024 ", Sys.Date(),".png")), width = 8, height = 5)
    
    
} 

susp.comp.school.graph <- function(df) {
    
    
    work.group <-   df %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    
    skul <- df$schoolname[1]
    
    df %>%
        ggplot(aes(x = Group, y = susp.rate)) +
        geom_col(aes(fill = EstimatedColor,
                     color = "black"),
                 position = "dodge2") +
        {if(length(work.group) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
        mcoe_theme +
        scale_fill_identity() +
        scale_color_identity() +
        labs(y = "Suspension Rate",
             title = paste0(skul, " Suspension Student Group Estimates 2024"),
             subtitle = "Gray is 2023 results and Colored bars are 2024 with the estimated Dashboard color")
    
    
    ggsave(here("output",save.folder ,paste0(skul," Suspension Group Results 2023 and 2024 Comparison ", Sys.Date(),".png")), width = 8, height = 5)
    
    
} 

susp.comp.school(holder, dist.code = 66092, school.code = 6026181, limit.case.count = TRUE) %>%
    susp.comp.school.graph()



susp.all.schools <- function(df, dist.cd, limit.case.cnt = TRUE) {
    
    
    holder <- df %>%
        # filter(str_detect(DistrictName,dist.name)) %>%
        split(.$SchoolName) %>%
        map_df(~add.school.susp(.))  %>%
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
        
susp.df    <-    susp.comp.school(df = holder, dist.code = dist.cd, school.code = school.list[i], limit.case.count = limit.case.cnt) 
        
            susp.comp.school.graph(susp.df)
            susp.school.graph(susp.df)
            
        
 #       ggsave(here("output",save.folder ,paste0(school.list[i], " - ","Suspension Student Group Results 2023 and 2024 Comparison ", Sys.Date(),".png")), width = 8, height = 5)
        
    }
    
    
}


mpusd.sus.school.joint %>%
    filter(!str_detect(SchoolName, "Monterey Peninsula Unified")) %>%
    susp.all.schools(dist.cd = 66092)
