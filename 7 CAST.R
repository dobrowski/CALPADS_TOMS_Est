


cast.w.change <- function(df, cds, level = "D") {
    
    # Saves dataframe name
    ddff <-     deparse(substitute(df)) 
    
    holder <-  df %>% 
        filter(Subject %in% c("Science"))  %>%
      { if(level == "S") filter(., CALPADSSchoolCode == cds )
        else filter(., Subject == "Science")
      } %>%
         mutate(All = "Yes",
                Science.points = case_when(GradeLevelWhenAssessed == "05" ~  ScaleScore - 150,
                                            GradeLevelWhenAssessed == "08" ~  ScaleScore - 350,
                                            GradeLevelWhenAssessed %in% c("09","10","11","12") ~  ScaleScore - 550
                                                 ) 
                )   %>%
        pivot_longer(cols = c(HispanicOrLatinoEthnicity:All), values_drop_na = TRUE) %>%
        group_by(name) %>%
        mutate(science.points.mean = mean(Science.points),
               count = n()) %>%
        mutate(studentgroup = case_match(name, 
            
                "SED" ~ "SED",
                "SWD" ~ "SWD",
                "White" ~ "WH",
                "ELdash" ~ "EL",                              
                "AmericanIndianorAlaskaNative" ~ "AI",
                "Asian" ~ "AS",
                "Filipino" ~ "FI",
                "HawaiianOrOtherPacificIslander" ~ "PI",
                "TwoorMoreRaces" ~ "MR",
                "BlackorAfricanAmerican" ~ "AA",
                "HispanicOrLatinoEthnicity" ~ "HI",
                "HOM" ~ "HOM",
                "LTELdash" ~ "LTEL",
                "All" ~ "ALL"
        )
        ) %>%
        select(science.points.mean, studentgroup, count) %>%
        distinct() # %>%
        # mutate() # %>%

    # Gets Dashboard data and compares

    dash.LEA  <- # dash.district(cds) 
    
    { if(level == "S") dash.school(cds)
      else dash.district(cds)
    } %>%
        filter(
  #          studentgroup == "ALL",
            indicator == "SCIENCE"
        ) %>%
        select(cds, Subject = indicator, oldDFS = currstatus , oldcolor = color , studentgroup ,Group)  %>%
        mutate(oldDFS = oldDFS +65,
               District = ddff) %>%
        mutate( old.colors = case_when(#old.colors == FALSE ~ "Light Gray",
            oldcolor == 1 ~ "Red",
            oldcolor == 2 ~ "Orange",
            oldcolor == 3 ~ "Yellow",
            oldcolor == 4 ~ "Green",
            oldcolor == 5 ~ "Blue",
            TRUE ~ "White")
        )
    
     holder <- left_join(holder, dash.LEA) %>%
         mutate( change = science.points.mean - oldDFS,
                EstimatedColor = case_when(
                    count < 30 ~ "White",
                    science.points.mean <= 34.9 & change <= 1.9 ~ "Red",
                    science.points.mean <= 34.9 & change >= 2.0 ~ "Orange",
                    science.points.mean <= 44.9 & change <= 1.9 ~ "Orange",
                    science.points.mean <= 44.9 & change >= 2.0 ~ "Yellow",
                    science.points.mean <= 54.9 & change <= 1.9 ~ "Yellow",
                    science.points.mean <= 54.9 & change >= 2.0 ~ "Green",
                    science.points.mean <= 64.9 & change <= 4.9 ~ "Green",
                    science.points.mean <= 64.9 & change >= 5.0 ~ "Blue",
                    science.points.mean >= 65.0 & change <= -1.9 ~ "Green",
                    science.points.mean >= 65.0 & change >= -2.0 ~ "Blue",
                    #  !is.na(mean.dist.stand) & is.na(DFS.y) ~ "Black",
                )
        ) 

     
     # # Posts to the google sheet
    sheet_append(ss = sheet,
                 sheet = "Science",
                 data = holder )
    # 
     holder
    
}

cast.w.change(nmcusd.25, 27738250000000)


cast.w.change(nmcusd.25, 27738252730034, level = "S")


### CAST Graph -------

working <- read_sheet(ss = sheet,
                      sheet = "Science") %>%
  filter(EstimatedColor %in% c("Red","Orange", "Yellow", "Green", "Blue")) 


cast.dash.graph <- function(dist, ccddss ,dist.name ) {
  
  work.group <-   working %>%
    filter(District == dist,
          # cds == ccddss
           ) %>%
    select(Group) %>%
    unique() %>%
    flatten()

  working %>%
    filter(District == dist,
           cds == ccddss) %>%
    #       mutate(DFS = as.numeric(DFS)) %>%
    ggplot(aes(x = Group, y = SciencePoints, )) +
    #        ggplot(aes(x = fct_reorder(Group,PercentChronicAbsent), y = PercentChronicAbsent, )) +
    geom_col(aes(fill = EstimatedColor, 
                 color = "black")) +
    mcoe_theme +
    {if(length(work.group) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
    scale_fill_identity() +
    scale_color_identity() +
    labs(y = "Science Points",
         title = paste0(dist.name," - CAST Science Points Student Group Estimates ", thisyear))
  
  
  ggsave(here("output", save.folder ,paste0(dist.name, " - Science Student Group Estimates ",thisyear , Sys.Date(),".png")), width = 8, height = 5)    
  
}



cast.dash.graph(dist = "nmcusd.25",
                ccddss = 27738250000000,
                dist.name = "North Monterey County"
                )

cast.dash.graph(dist = "nmcusd.25",
                ccddss = 27738252730034,
                dist.name = "North Monterey High"
                )


### Comparison to prior year ----


cast.dash.comp <- function(dist, ccddss , dist.name, old.colors = TRUE ) {
  
  
  work.group <-   working %>%
    filter(District == dist,
           cds == ccddss) %>%
    select(Group) %>%
    unique() %>%
    flatten()
  

  df <-    working %>%
    filter(District == dist,
           cds == ccddss) %>%
    pivot_longer(cols = c(oldScience,SciencePoints)) %>%
    mutate(GraphColor = case_when(name == "oldScience" ~ `Old Color`,
                                  name == "SciencePoints" ~ EstimatedColor
                                  )
    ) %>%
  #   mutate(PercentSusp = as.numeric(PercentSusp)) %>%
    mutate(year = case_when(name == "oldScience" ~ lastyear,
                            name == "SciencePoints" ~ thisyear),
           year = factor(year),
           year = fct_relevel(year,lastyear ) 
    )
  

  df %>%
    ggplot(aes(x = Group, y = value, group = year)) +
    geom_col_pattern(aes(fill = GraphColor,
                         pattern = year,
                         color = "black"),
                     position = "dodge2") +
    {if(old.colors==TRUE)scale_pattern_manual(values=c('stripe', 'wave'))else scale_pattern_manual(values=c('wave', 'wave'))    } +
    mcoe_theme +
    {if(length(unique(df$Group)) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
    scale_fill_identity() +
    scale_color_identity() +
    theme(legend.position = "none") +

    labs(y = "Science Points",
         title = paste0(dist.name," - Science Student Group Estimates ",thisyear),
         subtitle = if_else(old.colors == FALSE,
                            paste0("Gray is ", lastyear, " results and Colored bars are ", thisyear ," with the estimated Dashboard color"),
                            paste0("", lastyear, " results are on the left and ", thisyear ," estimates are on the right for each student group")),
          caption = "Note: Science did not receive colors in 2024"
         )
    


  ggsave(here("output",save.folder ,paste0(dist.name," - Science Student Group Results ", lastyear, " and ", thisyear ," Comparison ",if_else(old.colors == TRUE, "old colors ",""), Sys.Date(),".png")), width = 8, height = 5)
  
}

cast.dash.comp(dist = "nmcusd.25",
               ccddss = 27738250000000,
               dist.name = "North Monterey County"
               , old.colors = TRUE)

cast.dash.comp(dist = "nmcusd.25",
               ccddss = 27738252730034,
               dist.name = "North Monterey High"
               , old.colors = TRUE)



### For all ---


cast.w.change(nmcusd.25, 27738250000000)


cast.w.change(nmcusd.25, 27738252730034, level = "S")


cast.dash.graph(dist = "nmcusd.25",
                ccddss = 27738250000000,
                dist.name = "North Monterey County"
)

cast.dash.graph(dist = "nmcusd.25",
                ccddss = 27738252730034,
                dist.name = "North Monterey High"
)

cast.dash.comp(dist = "nmcusd.25",
               ccddss = 27738250000000,
               dist.name = "North Monterey County"
               , old.colors = TRUE)

cast.dash.comp(dist = "nmcusd.25",
               ccddss = 27738252730034,
               dist.name = "North Monterey High"
               , old.colors = TRUE)


school.cds.list <- nmcusd.25 %>%
  filter(Subject == "Science",
         CALPADSDistrictName == nmcusd.25$CALPADSDistrictName[1]) %>%
  select(CALPADSSchoolName, CALPADSSchoolCode) %>%
  unique()


for (i in 1:nrow(school.cds.list)) {
  
  print(school.cds.list$CALPADSSchoolName[i])
  
  print(school.cds.list$CALPADSSchoolCode[i])
  
  
  cast.w.change(nmcusd.25, school.cds.list$CALPADSSchoolCode[i] , level = "S")
  
  working <- read_sheet(ss = sheet,
                        sheet = "Science") %>%
    filter(EstimatedColor %in% c("Red","Orange", "Yellow", "Green", "Blue")) 
  
  
  cast.dash.graph(dist = "nmcusd.25",
                  ccddss = school.cds.list$CALPADSSchoolCode[i],
                  dist.name = school.cds.list$CALPADSSchoolName[i]
  )
  
  cast.dash.comp(dist = "nmcusd.25",
                 ccddss = school.cds.list$CALPADSSchoolCode[i],
                 dist.name = school.cds.list$CALPADSSchoolName[i]
                 , old.colors = TRUE)
}

