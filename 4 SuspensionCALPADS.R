
# Looks at CALPADS Suspension data to give dashboard estimates 
# Need CALPADS 7.12 and 8.1 reports



mpusd.sus.24 <- read_csv(here("data", "mpusd" , "7.12_IncidentResultsStudentList.InReviewUncertified_20240715.csv"))
# mpusd.demo.24 <- read_csv(here("data", "mpusd" , "8.1_StudentProfileList(EOY3).InReviewUncertified_20240715.csv"))


susp.df <- function(suspenion.incidences, demographics) {
    
sol.susp.sum <- suspenion.incidences %>%
    group_by(SSID) %>%
    mutate(SSID = as.character(SSID)) %>%
    summarise(days = sum(`DurationDays`)) %>%
    filter(days >= 0.5) 


sol.susp.demo <- demographics %>%
    mutate(SSID = as.character(SSID)) %>%
  mutate( LTEL = case_when(ELAStatus == "EL" & mdy(ELAStatusStartDate) <= ymd( paste0(yr -7,"-06-15")) ~ "Y",
                           TRUE ~ "N")
  ) %>%
  
    select(SSID, EthnicityRace, Homeless, StudentswithDisabilities, EnglishLearner = EnglishLearner_ReclassifiedFluentEnglishProficient, LTEL, SocioEconomicallyDisadvantaged) %>%
    distinct() %>%
  #  mutate(Yes = "Y") %>%
 #   pivot_wider(names_from = EthnicityRace, values_from = Yes, values_fill = "N")
    group_by(SSID) %>%
    mutate(Homeless = if_else(any(Homeless == "Y"), "Y", "N" ),
           StudentswithDisabilities = if_else(any(StudentswithDisabilities == "Y"), "Y", "N" ),
           EnglishLearner = if_else(any(EnglishLearner == "Y"), "Y", "N" ),
           LTEL = if_else(any(LTEL == "Y"), "Y", "N" ),
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



susp.group.rate.w.change <- function(df, studentgroup, cds) {
  
  ddff <-     deparse(substitute(df)) 
  studentsss <-     deparse(substitute(studentgroup))
  
  holder <- df %>%
    group_by({{studentgroup}}) %>%
    transmute(count = n(),
              perc.susp = 100*mean(susp)) %>%
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
                              "Am Indian/Alskn Nat" ~ "American\nIndian/\nAlaska\nNative",
                              
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
      indicator == "SUSP"
    ) %>%
    select(cds, type, Subject = indicator, oldstatus = currstatus, oldcolor = color ,Group) %>%
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
    mutate( change = perc.susp - oldstatus,
            EstimatedColor = case_when(
              count < 30 ~ "White",
              
              
              # High School District
              type == "HD" & perc.susp >=9.1 & change > -0.5 ~ "Red",
              type == "HD" & perc.susp >=9.1 & change <= -3.0 ~ "Yellow",
              type == "HD" & perc.susp >=9.1 & change <= -0.5 ~ "Orange",
              
              type == "HD" & perc.susp >=6.1 & change >= 3.1 ~ "Red",
              type == "HD" & perc.susp >=6.1 & change <= -0.5 ~ "Yellow", 
              type == "HD" & perc.susp >=6.1 & change < 3.0 ~ "Orange", 
              
              type == "HD" & perc.susp >=3.6 & change >= 0.5 ~ "Orange",
              type == "HD" & perc.susp >=3.6 & change <= -0.5 ~ "Green",
              type == "HD" & perc.susp >=3.6 & change < 0.5 ~ "Yellow",
              
              type == "HD" & perc.susp >=1.6 & change >= 3.1 ~ "Orange",
              type == "HD" & perc.susp >=1.6 & change <= -3.0 ~ "Blue",    
              type == "HD" & perc.susp >=1.6 & change <= 0.4 ~ "Green",    
              type == "HD" & perc.susp >=1.6 & change > 0.4 ~ "Yellow",    
              
              type == "HD" & perc.susp < 1.6 & change >= 0.5 ~ "Green",
              type == "HD" & perc.susp < 1.6 & change  < 0.5 ~ "Blue",
              
              
              # Unified 
              type == "UD" & perc.susp >=8.1 & change >= -0.2 ~ "Red",
              type == "UD" & perc.susp >=8.1 & change <= -2.0 ~ "Yellow",
              type == "UD" & perc.susp >=8.1 & change <= -0.3 ~ "Orange",
              
              type == "UD" & perc.susp >=4.6 & change >= 2.1 ~ "Red",
              type == "UD" & perc.susp >=4.6 & change <= -0.3 ~ "Yellow", 
              type == "UD" & perc.susp >=4.6 & change < 2.1 ~ "Orange", 
              
              type == "UD" & perc.susp >=2.6 & change >= 0.3 ~ "Orange",
              type == "UD" & perc.susp >=2.6 & change <= -0.3 ~ "Green",
              type == "UD" & perc.susp >=2.6 & change < 0.3 ~ "Yellow",
              
              type == "UD" & perc.susp >=1.1 & change >= 2.1 ~ "Orange",
              type == "UD" & perc.susp >=1.1 & change <= -2.0 ~ "Blue",    
              type == "UD" & perc.susp >=1.1 & change <= 0.2 ~ "Green",    
              type == "UD" & perc.susp >=1.1 & change < 2.1 ~ "Yellow",    
              
              type == "UD" & perc.susp < 1.1 & change >= 0.3 ~ "Green",
              type == "UD" & perc.susp < 1.1 & change  < 0.3 ~ "Blue",
              
              
              # Elementary District
              type %in% c("ED") & perc.susp >=6.1 & change > -0.2 ~ "Red",
              type %in% c("ED") & perc.susp >=6.1 & change <= -2.0 ~ "Yellow",
              type %in% c("ED") & perc.susp >=6.1 & change <= -0.3 ~ "Orange",
              
              type %in% c("ED") & perc.susp >=3.1 & change >= 2.1 ~ "Red",
              type %in% c("ED") & perc.susp >=3.1 & change <= -0.3 ~ "Yellow", 
              type %in% c("ED") & perc.susp >=3.1 & change < 2.1 ~ "Orange", 
              
              type %in% c("ED") & perc.susp >=1.6 & change >= 0.3 ~ "Orange",
              type %in% c("ED") & perc.susp >=1.6 & change <= -0.3 ~ "Green",
              type %in% c("ED") & perc.susp >=1.6 & change < 0.3 ~ "Yellow",
              
              type %in% c("ED") & perc.susp >=0.6 & change >= 0.3 ~ "Yellow",
              type %in% c("ED") & perc.susp >=0.6 & change <= -2.0 ~ "Blue",    
              type %in%  c("ED") & perc.susp >=0.6 & change <= 0.3 ~ "Green",    
              
              type %in%  c("ED") & perc.susp < 0.6 & change >= 0.3 ~ "Green",
              type %in%  c("ED") & perc.susp < 0.6 & change  < 0.3 ~ "Blue",
              
              # Elementary School and Single School District
              type == "ES" & perc.susp >=6.1 & change > -0.2 ~ "Red",
              type == "ES" & perc.susp >=6.1 & change <= -1.0 ~ "Yellow",
              type == "ES" & perc.susp >=6.1 & change <= -0.3 ~ "Orange",
              
              type == "ES" & perc.susp >=3.1 & change >= 2.1 ~ "Red",
              type == "ES" & perc.susp >=3.1 & change <= -0.3 ~ "Yellow", 
              type == "ES" & perc.susp >=3.1 & change < 2.0 ~ "Orange", 
              
              type == "ES" & perc.susp >=1.1 & change >= 0.3 ~ "Orange",
              type == "ES" & perc.susp >=1.1 & change <= -0.3 ~ "Green",
              type == "ES" & perc.susp >=1.1 & change < 0.3 ~ "Yellow",
              
              type == "ES" & perc.susp >=0.6 & change >= 0.3 ~ "Yellow",
              type == "ES" & perc.susp >=0.6 & change <= -1.0 ~ "Blue",    
              type == "ES" & perc.susp >=0.6 & change <= 0.3 ~ "Green",    
              
              type == "ES" & perc.susp < 0.6 & change >= 0.3 ~ "Green",
              type == "ES" & perc.susp < 0.6 & change  < 0.3 ~ "Blue"
              

            )
    ) %>%
    select(-oldcolor, -Subject, -EthnicityRace) %>%
    filter({{studentgroup}} != "N") 
  
  
  
  
  
  
  
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
             title = paste0(dist.name," - Suspended Student Group Estimates ", thisyear))
    
    
    ggsave(here("output", save.folder ,paste0(dist.name, " - Suspended Student Group Estimates ",thisyear , Sys.Date(),".png")), width = 8, height = 5)    
    
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
                              "LTEL" ~ "Long Term\nEnglish\nLearner",
                              "Black/African Am" ~ "Black/\nAfrican Am",
                              "Nat Hwiin/Othr Pac Islndr" ~ "Pacific Islander",
                              "Multiple" ~ "Multiple \nRaces",
                              .default = StudentGroup
    ))


susp.dash.graph(dist = "mpusd.sus.joint",
                   dist.name = "Monterey Peninsula")



### Comparison to prior year ----


susp.dash.comp <- function(dist, dist.name, old.colors = FALSE ) {
    
    
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
               indicator == "SUSP" 
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
        rename(PercentSusp = currstatus) %>%
        mutate(year = "1old")
    
    
    
    df <-    working %>%
        filter(District == dist
        ) %>%
        mutate(PercentSusp = as.numeric(PercentSusp)) %>%
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
        arrange(PercentSusp) 
    
    levelss <- leveler$Group %>% union(working$Group %>% unique())
    
    
    df %>%   
        mutate(# Group = factor(Group, levels = levelss), # Sorts only by the current year
            EstimatedColor = fct_relevel(EstimatedColor,"Light Gray" ) # Puts gray to the left of color
        ) %>%
        ggplot(aes(x = Group, y = PercentSusp, group = year)) +
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
        
        labs(y = "Percent Suspended",
             title = paste0(dist.name," - Suspended Student Group Estimates ",thisyear),
             subtitle = if_else(old.colors == FALSE,
                                paste0("Gray is ", lastyear, " results and Colored bars are ", thisyear ," with the estimated Dashboard color"),
                                paste0("", lastyear, " results are on the left and ", thisyear ," estimates are on the right for each student group")
             )
        )
    
    
    ggsave(here("output",save.folder ,paste0(dist.name," - Suspended Student Group Results ", lastyear, " and ", thisyear ," Comparison ",if_else(old.colors == TRUE, "old colors ",""), Sys.Date(),".png")), width = 8, height = 5)    
    
}



susp.dash.comp(dist = "mpusd.sus.joint",
                  dist.name = "Monterey Peninsula",
               old.colors = TRUE)



### School Graphs ----

# Merge suspensions with demographics at school level
susp.joint.school <- function(df, df.demo, dist.name) {
    
    
    df.calpads2 <- df %>%
        mutate(SSID = as.character(SSID),
               SchoolCode = as.character(str_pad(as.character(SchoolCode), width = 7, pad = "0")),
               Grade = as.character(str_pad(as.character(Grade), width = 2, pad = "0"))
               ) %>%
        group_by(SSID, SchoolName, SchoolCode, Grade) %>% 
        summarise(days = sum(`DurationDays`)) %>%
        filter(days >= 0.5) 
    

    df.calpads.demo2 <- df.demo %>%
      mutate( LTEL = case_when(ELAStatus == "EL" & mdy(ELAStatusStartDate) <= ymd( paste0(yr -7,"-06-15")) ~ "Y",
                               TRUE ~ "N")
      ) %>%
      
        mutate(SSID = as.character(SSID),
               SchoolCode = as.character(str_pad(as.character(SchoolCode), width = 7, pad = "0")),
               Grade = as.character(str_pad(as.character(Grade), width = 2, pad = "0"))
               ) %>%
        select(SSID, SchoolName,  SchoolCode, Grade, EthnicityRace, Homeless, StudentswithDisabilities, EnglishLearner = EnglishLearner_ReclassifiedFluentEnglishProficient, LTEL, SocioEconomicallyDisadvantaged) %>%
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
    
    joint <- df.calpads.demo2  %>%
        left_join(df.calpads2) %>%
        mutate(susp = if_else(is.na(days), FALSE, TRUE))
    
    joint
    
    
}


mpusd.sus.school.joint <- susp.joint.school(mpusd.sus.24, mpusd.demo.24)


# Calculate suspension rate for student group
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

# Run suspension calculation for all student groups
add.school.susp <- function(df) {
    
    namer <- unique(df$SchoolName)
    coder <- unique(df$SchoolCode)
    
    waiting.room <- susp.school(df,All) %>%
        bind_rows(  susp.school(df,White) ) %>%
        bind_rows(  susp.school(df,EnglishLearner) ) %>%
      bind_rows(  susp.school(df,LTEL) ) %>%
      bind_rows( susp.school(df,Asian) )  %>%
        bind_rows( susp.school(df,Filipino) )  %>%
        bind_rows( susp.school(df,Multiple) )  %>%
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


holder <- suhsd.sus.school.joint %>%
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
                              "LTEL" ~ "Long Term\nEnglish\nLearner",
                              "Black/African Am" ~ "Black/\nAfrican Am",
                              "Nat Hwiin/Othr Pac Islndr" ~ "Pacific Islander",
                              "Multiple" ~ "Multiple \nRaces",
                              .default = students
    ))


# Find last year suspension details from dashboard
dash.school.sus <- function(cdsCode) {
    
  dash.all %>%
    filter(cds == cdsCode,
           reportingyear == yr - 1, 
           rtype == "S",
               indicator == "SUSP")
    
}

# Compare this year and last year suspensions
susp.comp.school <- function(df, dist.code, school.code, limit.case.count = TRUE, old.colors = FALSE ) {
    
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
    
    dash2 <- dash.school.sus( cds ) %>%
        filter(# str_detect(districtname, dist.name),
            Group %in% work.group
        ) %>%
        select(districtname, type, indicator, currstatus, Group, color) %>%
        mutate(EstimatedColor = case_when(old.colors == FALSE ~ "Light Gray",
                                          color == 1 ~ "Red",
                                          color == 2 ~ "Orange",
                                          color == 3 ~ "Yellow",
                                          color == 4 ~ "Green",
                                          color == 5 ~ "Blue",
                                          color == 0 ~ "White"
        )
        
        ) %>%
        rename(susp.rate = currstatus)%>%
        mutate(year = "1old")
    
    print(dash2)
    
    
    df %>%
        filter(SchoolCode == school.code # | SchoolCode == as.numeric(str_pad(school.code, 7, side="left", pad="0"))
        ) %>%
        filter(if(limit.case.count == TRUE )count >= 30 else count >= 10) %>%
        #      mutate(DFS = as.numeric(DFS)) %>%
        left_join(dash2, by = c("Group")) %>%
        mutate(change = susp.rate.x - susp.rate.y,
               EstimatedColor = case_when(
                   Group != "Long Term\nEnglish\nLearner" & count < 30 ~ "White",
                   
                   # High Schools
                   type == "HS"  & susp.rate.x >=10.1 & change > -0.2 ~ "Red",
                   type == "HS"  & susp.rate.x >=10.1 & change <= -2.0 ~ "Yellow",
                   type == "HS"  & susp.rate.x >=10.1 & change <= -0.3 ~ "Orange",
                   
                   type == "HS"  & susp.rate.x >=6.1 & change >= 3.1 ~ "Red",
                   type == "HS"  & susp.rate.x >=6.1 & change <= -0.3 ~ "Yellow", 
                   type == "HS"  & susp.rate.x >=6.1 & change < 3.0 ~ "Orange", 
                   
                   type == "HS"  & susp.rate.x >=1.6 & change >= 0.3 ~ "Orange",
                   type == "HS"  & susp.rate.x >=1.6 & change <= -0.3 ~ "Green",
                   type == "HS"  & susp.rate.x >=1.6 & change < 0.3 ~ "Yellow",
                   
                   type == "HS"  & susp.rate.x >=0.6 & change >= 0.3 ~ "Yellow",
                   type == "HS"  & susp.rate.x >=0.6 & change <= -2.0 ~ "Blue",    
                   type == "HS"  & susp.rate.x >=0.6 & change <= 0.3 ~ "Green",    

                   type == "HS"  & susp.rate.x < 0.6 & change >= 0.3 ~ "Green",
                   type == "HS"  & susp.rate.x < 0.6 & change  < 0.3 ~ "Blue",
                   
                   
                   # Middle Schools
                   type == "MS" & susp.rate.x >=12.1 & change > -0.2 ~ "Red",
                   type == "MS" & susp.rate.x >=12.1 & change <= -3.0 ~ "Yellow",
                   type == "MS" & susp.rate.x >=12.1 & change <= -0.3 ~ "Orange",
                   
                   type == "MS" & susp.rate.x >=8.1 & change >= 4.1 ~ "Red",
                   type == "MS" & susp.rate.x >=8.1 & change <= -0.3 ~ "Yellow", 
                   type == "MS" & susp.rate.x >=8.1 & change < 4.0 ~ "Orange", 
                   
                   type == "MS" & susp.rate.x >=2.1 & change >= 0.3 ~ "Orange",
                   type == "MS" & susp.rate.x >=2.1 & change <= -0.3 ~ "Green",
                   type == "MS" & susp.rate.x >=2.1 & change < 0.3 ~ "Yellow",
                   
                   type == "MS" & susp.rate.x >=0.6 & change >= 0.3 ~ "Yellow",
                   type == "MS" & susp.rate.x >=0.6 & change <= -3.0 ~ "Blue",    
                   type == "MS" & susp.rate.x >=0.6 & change <= 0.3 ~ "Green",    
                   
                   type == "MS" & susp.rate.x < 0.6 & change >= 0.3 ~ "Green",
                   type == "MS" & susp.rate.x < 0.6 & change  < 0.3 ~ "Blue",
                   
                   
                   # Elementary Schools
                   type == "ES" & susp.rate.x >=6.1 & change > -0.2 ~ "Red",
                   type == "ES" & susp.rate.x >=6.1 & change <= -1.0 ~ "Yellow",
                   type == "ES" & susp.rate.x >=6.1 & change <= -0.3 ~ "Orange",
                   
                   type == "ES" & susp.rate.x >=3.1 & change >= 2.1 ~ "Red",
                   type == "ES" & susp.rate.x >=3.1 & change <= -0.3 ~ "Yellow", 
                   type == "ES" & susp.rate.x >=3.1 & change < 2.0 ~ "Orange", 
                   
                   type == "ES" & susp.rate.x >=1.1 & change >= 0.3 ~ "Orange",
                   type == "ES" & susp.rate.x >=1.1 & change <= -0.3 ~ "Green",
                   type == "ES" & susp.rate.x >=1.1 & change < 0.3 ~ "Yellow",
                   
                   type == "ES" & susp.rate.x >=0.6 & change >= 0.3 ~ "Yellow",
                   type == "ES" & susp.rate.x >=0.6 & change <= -1.0 ~ "Blue",    
                   type == "ES" & susp.rate.x >=0.6 & change <= 0.3 ~ "Green",    
                   
                   type == "ES" & susp.rate.x < 0.6 & change >= 0.3 ~ "Green",
                   type == "ES" & susp.rate.x < 0.6 & change  < 0.3 ~ "Blue",
                   

                   TRUE ~ EstimatedColor
                   
               ),
               susp.rate = susp.rate.x
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



test <- susp.comp.school(holder, dist.code = 66159, school.code = 6058770)



susp.school.graph <- function(df) {
    
    
    work.group <-   df %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    
    skul <- df$SchoolName[1]
    
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
             title = paste0(skul, " Suspension Student Group Estimates ", thisyear),
    #         subtitle = "Gray is 2023 results and Colored bars are 2024 with the estimated Dashboard color"
             )
    
    
    ggsave(here("output",save.folder ,paste0(skul," Suspension Group Results ", thisyear, Sys.Date(),".png")), width = 8, height = 5)
    
    
} 

susp.comp.school.graph <- function(df, old.colors = FALSE) {
    
    
    work.group <-   df %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    
    skul <- df$SchoolName[1]
    
    df %>%
        ggplot(aes(x = Group, y = susp.rate, group = year)) +
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
        
        labs(y = "Suspension Rate",
             title = paste0(skul, " Suspension Student Group Estimates ", thisyear),
             subtitle = if_else(old.colors == FALSE,
                                paste0("Gray is ", lastyear, " results and Colored bars are ", thisyear ," with the estimated Dashboard color"),
                                paste0("", lastyear, " results are on the left and ", thisyear ," estimates are on the right for each student group")
             )
        )
    
    ggsave(here("output",save.folder ,paste0(skul," Suspension Group Results ", lastyear, " and ", thisyear ," Comparison ",if_else(old.colors == TRUE, "old colors ",""), Sys.Date(),".png")), width = 8, height = 5)
    
    
} 

susp.comp.school(holder, dist.code = 66159, school.code = 6058770, limit.case.count = TRUE, old.colors = TRUE) %>%
    susp.comp.school.graph(old.colors = TRUE)

susp.df    <-    susp.comp.school(df = holder, dist.code = 66142, school.code = school.list[13], limit.case.count = TRUE) 

susp.comp.school.graph(susp.df)

susp.comp.school(holder, dist.code = 66159, school.code = 6058770, limit.case.count = TRUE, old.colors = TRUE) %>%
susp.school.graph()



susp.all.schools <- function(df, dist.cd, limit.case.cnt = TRUE, old.culrs = TRUE) {
    
    
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
                                  "LTEL" ~ "Long Term\nEnglish\nLearner",
                                  "Black/African Am" ~ "Black/\nAfrican Am",
                                  "Nat Hwiin/Othr Pac Islndr" ~ "Pacific Islander",
                                  "Multiple" ~ "Multiple \nRaces",
                                  .default = students
        ))
    
    
    school.list <- holder$SchoolCode %>% unique()
    
    for (i in 1:length(school.list)) {
        
susp.df    <-    susp.comp.school(df = holder, dist.code = dist.cd, school.code = school.list[i], limit.case.count = limit.case.cnt, old.colors = old.culrs) 
        
            susp.comp.school.graph(susp.df, old.colors = old.culrs)
            susp.school.graph(susp.df)
            
        
 #       ggsave(here("output",save.folder ,paste0(school.list[i], " - ","Suspension Student Group Results 2023 and 2024 Comparison ", Sys.Date(),".png")), width = 8, height = 5)
        
    }
    
    
}


mpusd.sus.school.joint %>%
    filter(!str_detect(SchoolName, "Monterey Peninsula Unified")) %>%
    susp.all.schools(dist.cd = 66092)
