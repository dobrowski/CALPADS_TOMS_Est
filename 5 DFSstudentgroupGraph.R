
# This file is to generate a bar graph to display distance from standard for groups. 

### get from google sheet ----
working <- read_sheet(ss = sheet,
           sheet = "Distance from Standard Group") %>%
    mutate(Group = case_match(StudentGroup,
                              "HOM" ~ "Homeless",
                              "SWD" ~ "Students with \nDisabilities",
                              "SED" ~ "Socio-Economically \nDisadvantaged",
                              "HispanicOrLatinoEthnicity" ~ "Latino",
                              "AmericanIndianorAlaskaNative" ~ "American\nIndian/\nAlaska\nNative",
                              "LTELdash" ~ "Long Term\nEnglish\nLearner",
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


### functions ----
dfs.graph <- function(dist, assessment = "ELA", dist.name ) {
    
  thisyear <- as.character(yr)
  lastyear <- as.character(yr - 1)
  
    work.group <-   working %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    
working %>%
    filter(District == dist,
           Test == assessment) %>%
    mutate(DFS = as.numeric(DFS)) %>%
    ggplot(aes(x = Group, y = DFS, )) +
#    ggplot(aes(x = fct_reorder(Group,DFS), y = DFS, )) +. # To sort by lowest to highest
    geom_col(aes(fill = EstimatedColor, 
                 color = "black")) +
    {if(length(work.group) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
        
    mcoe_theme +
    scale_fill_identity() +
    scale_color_identity() +
    labs(y = "Distance from Standard",
         title = paste0(dist.name," - ",assessment," CAASPP Student Group Results ", thisyear ,""))


ggsave(here("output",save.folder ,paste0(dist.name, " - ",assessment," CAASPP Student Group Results ", thisyear ," ", Sys.Date(),".png")), width = 8, height = 5)    

}


dfs.graph(dist = "nmcusd.24",
          assessment = "ELA",
          dist.name = "North Monterey County")


dfs.graph(dist = "mpusd.24",
          assessment = "Math",
          dist.name = "Monterey Peninsula")



### Comparison to prior year ----


caaspp.mry <- tbl(con, "CAASPP") %>% 
    filter(County_Code == "27",
           # DistrictCode == "10272",
           Test_Year >= "2023",
           School_Code == "0000000"
#           Type_ID == 6
 ) %>%
    collect() 



caaspp.mry2 <- caaspp.mry %>%
    mutate(Subgroup_ID = as.character(Subgroup_ID)) %>%
    left_join_codebook("CAASPP", "Subgroup_ID") %>%
    rename(Subgroup = definition) %>%
 #   left_join(ent2) %>%
    mutate(Type_ID = as.character(Type_ID)) %>%
    left_join_codebook("CAASPP", "Type_ID") %>%
    rename(Entity_Type = definition) %>%
    mutate(across(CAASPP_Reported_Enrollment:Area_4_Percentage_Near_Standard, as.numeric))



### DFS from Dash ------



dfs.comp <- function(dist, assessment = "ELA", dist.name , limit.case.count = TRUE, old.colors = FALSE) {
    
    
  thisyear <- as.character(yr)
  lastyear <- as.character(yr - 1)
  
  
work.group <-   working %>%
         filter(District == dist,
                Test == assessment) %>%
    select(Group) %>%
    unique() %>%
    flatten()
    
    ass2 <- str_to_upper(assessment) 
    
    dash2 <- dash.all %>%
        filter(str_detect(districtname, dist.name),
               reportingyear == yr - 1, 
               rtype == "D",
               indicator == ass2,
               Group %in% work.group
                             ) %>%
        select(districtname, indicator, currstatus, color ,Group) %>%
        mutate( EstimatedColor = case_when(old.colors == FALSE ~ "Light Gray",
                                           color == 1 ~ "Red",
                                           color == 2 ~ "Orange",
                                           color == 3 ~ "Yellow",
                                           color == 4 ~ "Green",
                                           color == 5 ~ "Blue",
                                           TRUE ~ "White") 
                ) %>%
        rename(DFS = currstatus) %>%
        mutate(year = "1old")
    
 

    working %>%
        filter(District == dist,
               Test == assessment) %>%
        mutate(DFS = as.numeric(DFS),
               year = "2new") %>%
        bind_rows(dash2) %>%
        mutate(EstimatedColor = factor(EstimatedColor),
               EstimatedColor = fct_relevel(EstimatedColor,"Light Gray" ),
               year = factor(year),
               year = fct_relevel(year,"1old" ) ,
               
               ) %>%


        ggplot(aes(x = Group, y = DFS, group = year)) +
 #       ggplot(aes(x = fct_reorder(Group,DFS), y = DFS)) +
        geom_col_pattern(aes(fill = EstimatedColor,
                             pattern = year,
                     color = "black"),
                 position = "dodge2") +
        {if(old.colors==TRUE)scale_pattern_manual(values=c('stripe', 'wave'))else scale_pattern_manual(values=c('wave', 'wave'))    } +
        mcoe_theme +
        {if(length(work.group) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
        scale_fill_identity() +
        scale_color_identity() +
        theme(legend.position = "none") +
        labs(y = "Distance from Standard",
             title = paste0(dist.name," - ",assessment," CAASPP Student Group Results ",thisyear),
             subtitle = if_else(old.colors == FALSE,
                                paste0("Gray is ", lastyear ," results and Colored bars are ", thisyear ," with the estimated Dashboard color"),
                                paste0(lastyear ," results are on the left and ", thisyear ," estimates are on the right for each student group")
             )
             )
    
    
    ggsave(here("output",save.folder ,paste0(dist.name, " - ",assessment," CAASPP Student Group Results ", lastyear ," and ", thisyear ," Comparison ", if_else(old.colors == TRUE, "old colors ","") ,Sys.Date(),".png")), width = 8, height = 5)    
    
}



dfs.comp(old.colors = TRUE,
         dist = "kingcity.24",
          assessment = "Math",
          dist.name = "King City")


dfs.comp(dist = "mpusd.24",
         assessment = "Math",
         dist.name = "Monterey Peninsula")



#### School level versions -------


dash.district <- function(cdsCode, yer = yr-1) {
  
  dash.all %>%
    filter(countyname == "Monterey",
           cds == cdsCode,
           rtype == "D",
 #          indicator == "ELA" | indicator == "MATH",
           reportingyear == as.character(yer) ) %>%
    collect()  %>%
    mutate(Group = case_match(studentgroup,
                              "HOM" ~ "Homeless",
                              "SWD" ~ "Students with \nDisabilities",
                              "SED" ~ "Socio-Economically \nDisadvantaged",
                              "HI" ~ "Latino",
                              "EL" ~ "English \nLearner",
                              "LTEL" ~ "Long Term\nEnglish\nLearner",
                              "AA" ~ "Black/\nAfrican Am",
                              "AI" ~ "American\nIndian/\nAlaska\nNative",
                              "AS" ~ "Asian",
                              "FI" ~ "Filipino",
                              "WH" ~ "White",
                              "MR" ~ "Multiple \nRaces",
                              "PI" ~ "Pacific Islander",
                              
                              "ALL" ~ "All",
                              .default = studentgroup
    ))
  
}


dash.school <- function(cdsCode, yer = yr-1) {
    
    dash.all %>%
    filter(countyname == "Monterey",
           cds == cdsCode,
           rtype == "S",
           indicator %in% c("ELA","MATH","SCIENCE" ),
           reportingyear == yer) %>%
        collect()  %>%
    mutate(Group = case_match(studentgroup,
                              "HOM" ~ "Homeless",
                              "SWD" ~ "Students with \nDisabilities",
                              "SED" ~ "Socio-Economically \nDisadvantaged",
                              "HI" ~ "Latino",
                              "EL" ~ "English Learner",
                              "LTEL" ~ "Long Term\nEnglish\nLearner",
                              
                              "AI" ~ "American\nIndian/\nAlaska\nNative",
                              "MR" ~ "Multiple \nRaces",
                              "AS" ~ "Asian",
                              "FI" ~ "Filipino",
                              "WH" ~ "White",
                              "ALL" ~ "All",
                              .default = studentgroup
    ))

}
    
temp <- dash.school(school.list[2])

dash.school(27754406026678, yer = "2022")




dfs.school.graph <- function(df) {
    
  thisyear <- as.character(yr)
    
    work.group <-   df %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    skul <- df$SchoolName[1]
    ass <- df$Test[1]
    
    df %>%
        filter(!is.na(DFS.x)) %>%
        ggplot(aes(x = Group, y = DFS)) +
        #        ggplot(aes(x = fct_reorder(Group,DFS), y = DFS)) +
        geom_col(aes(fill = EstimatedColor,
                     color = "black"),
                 position = "dodge2") +
        {if(length(work.group) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
        mcoe_theme +
        scale_fill_identity() +
        scale_color_identity() +
        labs(y = "Distance from Standard",
             title = paste0(skul, " - ", ass," CAASPP Student Group Estimates ", thisyear ,""),
  #           subtitle = "Gray is 2023 results and Colored bars are 2024 with the estimated Dashboard color"
             )
    
    
    ggsave(here("output",save.folder ,paste0(skul, " - ",ass," CAASPP Student Group Results ", thisyear ," ", Sys.Date(),".png")), width = 8, height = 5)
    
    
} 




    
dfs.comp.school <- function(df, cds, assessment = "ELA", limit.case.count = TRUE, old.colors = FALSE ) {
    

    
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
        filter(CDS == cds,
               Test == assessment,
    #           count >= 30
               ) %>%
        filter(if(limit.case.count == TRUE )count >= 30 else count >= 1) %>%
   #     {if(limit.case.count == TRUE )filter(count >= 30 ) } %>%
        ungroup() %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    
 #   print(work.group)
    
    ass2 <- str_to_upper(assessment)

    dash2 <- dash.school( cds ) %>%
        filter(# str_detect(districtname, dist.name),
               indicator == ass2,
               Group %in% work.group
        ) %>%
        select(districtname, indicator, currstatus, color ,Group) %>%
        mutate( EstimatedColor = case_when(old.colors == FALSE ~ "Light Gray",
                                           color == 1 ~ "Red",
                                           color == 2 ~ "Orange",
                                           color == 3 ~ "Yellow",
                                           color == 4 ~ "Green",
                                           color == 5 ~ "Blue",
                                           TRUE ~ "White") 
        ) %>%
        rename(DFS = currstatus) %>%
        mutate(year = "1old")
    
    
    
    

 df2<-   df %>%
        filter(CDS == cds,
               Test == assessment,
 #              count >= 30
               ) %>%
        filter(if(limit.case.count == TRUE )count >= 30 else count >= 1) %>%
        mutate(DFS = as.numeric(DFS)) %>%
        left_join(dash2, by = c("Group")) %>%
        mutate(change = DFS.x - DFS.y,
               EstimatedColor = case_when(
                                            count < 30 ~ "White",
                    
                                        # High Schools
                                          str_detect(ent,"High School") & Test == "ELA" & DFS.x <=-45.1 & change <= 2.99 ~ "Red",
                                          str_detect(ent,"High School") & Test == "ELA" & DFS.x <=-45.1 & change >= 3.0 ~ "Orange",
                                          str_detect(ent,"High School") & Test == "ELA" & DFS.x <=-0.1 & change <= 2.99 ~ "Orange",
                                          str_detect(ent,"High School") & Test == "ELA" & DFS.x <=-0.1 & change >= 3.0 ~ "Yellow",    
                                          str_detect(ent,"High School") & Test == "ELA" & DFS.x <=29.9 & change <= 2.99 ~ "Yellow",
                                          str_detect(ent,"High School") & Test == "ELA" & DFS.x <=29.9 & change >= 3.0 ~ "Green",
                                          str_detect(ent,"High School") & Test == "ELA" & DFS.x <=74.9 & change <= 14.99 ~ "Green",
                                          str_detect(ent,"High School") & Test == "ELA" & DFS.x <=74.9 & change >= 15.0 ~ "Blue",           
                                          str_detect(ent,"High School") & Test == "ELA" & DFS.x >=75.0 & change <= -3.0 ~ "Green",
                                          str_detect(ent,"High School") & Test == "ELA" & DFS.x >=75.0 & change  >= -3.0 ~ "Blue",
                                          
                                          str_detect(ent,"High School") & Test == "Math" & DFS.x <=-115.1 & change <= 2.99 ~ "Red",
                                          str_detect(ent,"High School") & Test == "Math" & DFS.x <=-115.1 & change >= 3.0 ~ "Orange",
                                          str_detect(ent,"High School") & Test == "Math" & DFS.x <=-60.1 & change <= 2.99 ~ "Orange",
                                          str_detect(ent,"High School") & Test == "Math" & DFS.x <=-60.1 & change >= 3.0 ~ "Yellow",    
                                          str_detect(ent,"High School") & Test == "Math" & DFS.x <=-0.1 & change <= 2.99 ~ "Yellow",
                                          str_detect(ent,"High School") & Test == "Math" & DFS.x <=-0.1 & change >= 3.0 ~ "Green",
                                          str_detect(ent,"High School") & Test == "Math" & DFS.x <=24.9 & change <= 14.99 ~ "Green",
                                          str_detect(ent,"High School") & Test == "Math" & DFS.x <=24.9 & change >= 15.0 ~ "Blue",           
                                          str_detect(ent,"High School") & Test == "Math" & DFS.x >=25.0 & change <= -3.0 ~ "Green",
                                          str_detect(ent,"High School") & Test == "Math" & DFS.x >=25.0 & change  >= -3.0 ~ "Blue",
                                          
                                          # Not High Schools
                                          
                                          Test == "ELA" & DFS.x <=-70.1 & change <= 2.9 ~ "Red",
                                          Test == "ELA" & DFS.x <=-70.1 & change >= 3.0 ~ "Orange",
                                          Test == "ELA" & DFS.x <=-5.1 & change <= 2.9 ~ "Orange",
                                          Test == "ELA" & DFS.x <=-5.1 & change >= 3.0 ~ "Yellow",    
                                          Test == "ELA" & DFS.x <=9.9 & change <= 2.9 ~ "Yellow",
                                          Test == "ELA" & DFS.x <=9.9 & change >= 3.0 ~ "Green",
                                          Test == "ELA" & DFS.x <=44.9 & change <= 14.9 ~ "Green",
                                          Test == "ELA" & DFS.x <=44.9 & change >= 15.0 ~ "Blue",           
                                          Test == "ELA" & DFS.x >=45.0 & change <= -3.0 ~ "Green",
                                          Test == "ELA" & DFS.x >=45.0 & change  >= -3.0 ~ "Blue",
                                          
                                          Test == "Math" & DFS.x <=-95.1 & change <= 2.9 ~ "Red",
                                          Test == "Math" & DFS.x <=-95.1 & change >= 3.0 ~ "Orange",
                                          Test == "Math" & DFS.x <=-25.1 & change <= 2.9 ~ "Orange",
                                          Test == "Math" & DFS.x <=-25.1 & change >= 3.0 ~ "Yellow",    
                                          Test == "Math" & DFS.x <=-0.1 & change <= 2.9 ~ "Yellow",
                                          Test == "Math" & DFS.x <=-0.1 & change >= 3.0 ~ "Green",
                                          Test == "Math" & DFS.x <=34.9 & change <= 14.9 ~ "Green",
                                          Test == "Math" & DFS.x <=34.9 & change >= 15.0 ~ "Blue",           
                                          Test == "Math" & DFS.x >=35.0 & change <= -3.0 ~ "Green",
                                          Test == "Math" & DFS.x >=35.0 & change  >= -3.0 ~ "Blue",
                                          

                                          !is.na(DFS.x) & is.na(DFS.y) ~ "Black",
                                          
                                          TRUE ~ EstimatedColor
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                          
               ),
               DFS = DFS.x
        ) %>%
                                          
        mutate(
               year = "2new") %>%
        bind_rows(dash2) %>%
        mutate(EstimatedColor = factor(EstimatedColor),
            EstimatedColor = fct_relevel(EstimatedColor,"Light Gray" ) ,
            year = factor(year),
            year = fct_relevel(year,"1old" ) ,
            
            )
    
 # Posts to google sheet
 sheet_append(ss = sheet,
              sheet = "DFS Comp School",
              data = df2 )
 
 
 
 
 df2
    
    
}


dfs.comp.school.graph <- function(df, old.colors = FALSE) {
    
  thisyear <- as.character(yr)
  lastyear <- as.character(yr - 1)
    
    work.group <-   df %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    skul <- df$SchoolName[1]
    ass <- df$Test[1]
    
    df %>%
        ggplot(aes(x = Group, y = DFS, group = year)) +
#        ggplot(aes(x = fct_reorder(Group,DFS), y = DFS)) +
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
        
        labs(y = "Distance from Standard",
             title = paste0(skul, " - ", ass," CAASPP Student Group Estimates ", thisyear ,""),
             subtitle = if_else(old.colors == FALSE,
                                paste0("Gray is ", lastyear, " results and Colored bars are ", thisyear ," with the estimated Dashboard color"),
                                paste0("", lastyear, " results are on the left and ", thisyear ," estimates are on the right for each student group")
                                )
        )
    

    ggsave(here("output",save.folder ,paste0(skul, " - ",ass," CAASPP Student Group Results ", lastyear, " and ", thisyear ," Comparison ", if_else(old.colors == TRUE, "old colors ","") , Sys.Date(),".png")), width = 8, height = 5)
    
    
} 


dfs.comp.school(suhsd.24, cds = 27661592734481,assessment = "ELA", limit.case.count = TRUE, old.colors = TRUE )






temp <- dfs.comp.school(df = holder, cds = 27660680141010, assessment = "Math")

temp %>% dfs.comp.school.graph()

temp %>% dfs.school.graph()


temp <- dfs.comp.school(df = holder, cds = 27660922732253, assessment = "ELA", limit.case.count = FALSE) %>% 
    dfs.comp.school.graph()

### Three year ----


dfs.3yr.school <- function(df, cds, assessment = "ELA", limit.case.count = TRUE, old.colors = FALSE ) {
    
 dfs.2yr <-  dfs.comp.school(df, cds, assessment, limit.case.count, old.colors )
    

    work.group <-   df %>%
        filter(CDS == cds,
               Test == assessment,
               #           count >= 30
        ) %>%
        filter(if(limit.case.count == TRUE )count >= 30 else count >= 1) %>%
        #     {if(limit.case.count == TRUE )filter(count >= 30 ) } %>%
        ungroup() %>%
        select(Group) %>%
        unique() %>%
        flatten()

    ass2 <- str_to_upper(assessment)
    
    dash3 <- dash.school( cds , yr = "2022") %>%
        filter(# str_detect(districtname, dist.name),
            indicator == ass2,
            Group %in% work.group
        ) %>%
        select(districtname, indicator, currstatus, color ,Group) %>%
        rename(DFS = currstatus) %>%
        mutate(year = "2022")
    
    
    
    
    
    df3<-   dfs.2yr %>%
        bind_rows(dash3) %>%
        mutate(year = case_match(year,
                                 "2022" ~ "2022",
                                 "1old" ~ yr-1,
                                 "2new" ~ yr),
            year = factor(year, levels = c("2022","2023","2024", "2025")),
        )

    df3
}



temp <- dfs.3yr.school(df = holder, cds = 27659616025977, assessment = "Math")



dfs.3yr.school.graph <- function(df) {
    
    
    skul <- df$SchoolName[1]
    ass <- df$Test[1]
    
    df %>%
        ggplot(aes(x = year, y = DFS, label = round(DFS,1), color = Group, group = Group)) +
        #        ggplot(aes(x = fct_reorder(Group,DFS), y = DFS)) +
        geom_line(size = 3) +
        geom_point(size = 5) +
        expand_limits(y = 0) +
        geom_text(color = "black",
                  vjust = -1) +
        geom_blank( aes(x=year, y=DFS*1.1, label=DFS)) +
        scale_color_brewer(type = "qual",palette = 2) +
        
        facet_wrap(~Group) +
        mcoe_theme  +
         theme(legend.position = "none") +

        labs(y = "Distance from Standard",
             title = paste0(skul, " - ", ass," CAASPP Over Time"),
             # subtitle = if_else(old.colors == FALSE,
             #                    "Gray is 2023 results and Colored bars are 2024 with the estimated Dashboard color",
             #                    "2023 results are on the left and 2024 estimates are on the right for each student group")
       )
    
    
#    ggsave(here("output",save.folder ,paste0(skul, " - ",ass," CAASPP Student Group Results 2023 and 2024 Comparison ", if_else(old.colors == TRUE, "old colors ","") , Sys.Date(),".png")), width = 8, height = 5)
    
    
} 

dfs.3yr.school.graph(temp)











### All Schools in District ------

# school.list <- c("27662336026702", "27662336092688", "27662336026710")

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
