
# Looks at grad rates 



### Fifth year Grads ----


mcoe.5th.grad.joint <- mcoe.2023.grad.24  %>%
    select(SchoolCode, SSID) %>%
    inner_join(mcoe.5th.grad.24) %>%
    mutate(CohortCategory = "HSDiplomaGraduate",
           SchoolCode = as.numeric(SchoolCode),
           EnrollmentStatus = as.numeric(str_sub(EnrollmentStatus, 1,2  )),
           StudentswithDisabilities = if_else(StudentWithDisabilities == "Y", "Yes", "No"),
           Homeless = if_else(HomelessProgramEligible == "Y", "Yes", "No"),
           EnglishLearner = if_else(ELAS == "EL", "Yes", "No"),
           SocioEconomicallyDisadvantaged = if_else(str_detect(SocioEconomicallyDisadvantaged,"Y"), "Yes", "No")
    )



### grad cals -----


mcoe.school.grad <- mcoe.grad.24 %>%
    bind_rows(mcoe.5th.grad.joint) %>%
    filter(CohortCategory != "RemovedFromCohort") %>%
    mutate(All = "Yes",
           Yes = "Yes") %>%
    pivot_wider(names_from = EthnicityRace, values_from = Yes) |>
    pivot_longer(cols = c(All,StudentswithDisabilities:Homeless,Hispanic:`Pacific Islander`)
        # pick out demographics columns based on values; 
        # create new columns with their names and values
       # where(\(x) all(x %in% c('Yes', 'N', NA))) 
    ) |> 
    # retain only values corresponding to actual demographic populations
    filter(value == 'Yes') |> 
    group_by(SchoolCode,SchoolName, name  ) |> 
    # calculate average value of `AbsenceRate2` and total student per group
    reframe( tabyl(CohortCategory), count = n()) |>
    filter(count >10,
           CohortCategory == "HSDiplomaGraduate") |>
    mutate(grad.rate = percent*100,
           Group = case_match(name,
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
                                         .default = name
               ))

#### Make Functions ---------



grad.func <- function(cohort.old, cohort.new, completer.list , level = "D") {

    
    
    ddff <-     deparse(substitute(completer.list)) 
    
grad.5th.joint <- cohort.old  %>%
    { if (level == "S") select(.,SchoolCode, SSID) else select(.,SSID)  }  %>%
    
    inner_join(completer.list) %>%
    mutate(SchoolCode = (SchoolCode) ) %>%
{ if (level == "D") select(.,-SchoolCode, -SchoolName) else . }%>%
    mutate(CohortCategory = "HSDiplomaGraduate",
           EnrollmentStatus = as.numeric(str_sub(EnrollmentStatus, 1,2  )),
           StudentswithDisabilities = if_else(StudentWithDisabilities == "Y", "Yes", "No"),
           Homeless = if_else(HomelessProgramEligible == "Y", "Yes", "No"),
           EnglishLearner = if_else(ELAS == "EL", "Yes", "No"),
#           LTEL = if_else(ELAS == "EL", "Yes", "No"),
           SocioEconomicallyDisadvantaged = if_else(str_detect(SocioEconomicallyDisadvantaged,"Y"), "Yes", "No")
    )


print(grad.5th.joint)

extra.cols <- c(SchoolCode = NA_real_, SchoolName = NA_real_)

school.grad <- cohort.new %>%
    bind_rows(grad.5th.joint) %>%
    filter(CohortCategory %notin% c( "RemovedFromCohort", "OtherTransfers" )) %>%
    mutate(All = "Yes",
           Yes = "Yes") %>%
    pivot_wider(names_from = EthnicityRace, values_from = Yes) |>
    pivot_longer(cols = c(StudentswithDisabilities:Homeless,All:last_col())
                 # pick out demographics columns based on values;
                 # create new columns with their names and values
                 # where(\(x) all(x %in% c('Yes', 'N', NA)))
    )  %>%
    # retain only values corresponding to actual demographic populations
    filter(value %in% c('Y', 'Yes')) %>%
    { if (level == "S")  group_by(.,
         SchoolCode,
         SchoolName,
        name
    ) else  group_by(.,
        # SchoolCode,
        # SchoolName,
        name
    )  }     %>%
    # calculate average value of `AbsenceRate2` and total student per group
    reframe( tabyl(CohortCategory), count = n()) %>%
    filter(count >10,
           CohortCategory == "HSDiplomaGraduate") %>%
    mutate(district = ddff,
           grad.rate = percent*100,
           Group = case_match(name,
                              "All" ~ "All",
                              "Homeless" ~ "Homeless",
                              "StudentswithDisabilities" ~ "Students with \nDisabilities",
                              "SocioEconomicallyDisadvantaged" ~ "Socio-Economically \nDisadvantaged",
                              "Hispanic" ~ "Latino",
                              "EnglishLearner" ~ "English \nLearner",
                              "LongTermEnglishLearner" ~ "Long Term\nEnglish\nLearner",

                              "African American" ~ "Black/\nAfrican Am",
                              "Black/African Am" ~ "Black/\nAfrican Am",
                              "Nat Hwiin/Othr Pac Islndr" ~ "Pacific Islander",
                              "Multiple" ~ "Multiple \nRaces",
                              .default = name
           )) %>%
    add_column(!!!extra.cols[!names(extra.cols) %in% names(.)]) %>%
    select(district, SchoolCode, SchoolName, name, Group ,count, grad.rate)

sheet_append(ss = sheet,
             sheet = "Grad Group",
             data = school.grad )

school.grad

}


temp <- grad.func(cohort.old = nmcusd.2024.grad.25, cohort.new = nmcusd.2025.grad.25, completer.list = nmcusd.list.grad.25,
                  level = "D")



grad.func(cohort.23 = mcoe.2023.grad.24 %>% filter(str_detect(SchoolName,"Salin|Well")),
          cohort.24 = mcoe.grad.24 %>% filter(str_detect(SchoolName,"Salin|Well")), 
          completer.list = mcoe.5th.grad.24 %>% filter(str_detect(SchoolName,"Salin|Well")),
                  level = "D")




#### GRaphs ------

dash.school.grad <- function(cdsCode) {
    
    dash.all %>%
        filter(cds == cdsCode,
               reportingyear == yr - 1, 
               rtype == "S",
               indicator == "GRAD")
    
}


temp <- dash.school.grad(27102722730117)


grad.comp <- function(df, dist.code, limit.case.count = TRUE, old.colors = TRUE ) {
    
    cds_code <- paste0("27",dist.code, "0000000")
    
    
    work.group <-   df %>%
        # filter(SchoolCode == school.code #| SchoolCode == as.numeric(str_pad(school.code, 7, side="left", pad="0"))
        # ) %>%
        filter(if(limit.case.count == TRUE )count >= 30 else count >= 1) %>%
        ungroup() %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    print(work.group)
    
    dash2 <-     dash.all %>%
        filter(cds == cds_code,
               reportingyear == yr - 1, 
               rtype == "D",
               indicator == "GRAD") %>%
        filter(# str_detect(districtname, dist.name),
            Group %in% work.group
        ) %>%
        select(districtname, schoolname ,indicator, currstatus, color, Group) %>%
        mutate( EstimatedColor = case_when(old.colors == FALSE ~ "Light Gray",
                                           color == 1 ~ "Red",
                                           color == 2 ~ "Orange",
                                           color == 3 ~ "Yellow",
                                           color == 4 ~ "Green",
                                           color == 5 ~ "Blue",
                                           TRUE ~ "White") 
        ) %>%
        rename(grad.rate = currstatus)%>%
        mutate(year = "1old")
    
    print(dash2)
    
    
    df %>%
        # filter(SchoolCode == school.code # | SchoolCode == as.numeric(str_pad(school.code, 7, side="left", pad="0"))
        # ) %>%
        filter(if(limit.case.count == TRUE )count >= 30 else count >= 1) %>%
        #      mutate(DFS = as.numeric(DFS)) %>%
        left_join(dash2, by = c("Group")) %>%
        mutate(change = grad.rate.x - grad.rate.y,
               EstimatedColor = case_when(
                   count < 30 ~ "White",
                   
                   # High Schools
                   grad.rate.x <=67.9  ~ "Red",
                   
                   grad.rate.x <=79.9 & change >= 1.0 ~ "Yellow",
                   grad.rate.x <=79.9 & change <= -5 ~ "Red", 
                   grad.rate.x <=79.9 & change < 1.0 ~ "Orange", 
                   
                   grad.rate.x <=90.4 & change >= 1.0 ~ "Green",
                   grad.rate.x <=90.4 & change <= -1.0 ~ "Orange", 
                   grad.rate.x <=90.4 & change < 1.0 ~ "Yellow",
                   
                   grad.rate.x <=94.9 & change >= 5.0 ~ "Blue",
                   grad.rate.x <=94.9 & change > -1.0 ~ "Green",
                   grad.rate.x <=94.9 & change < -5.0 ~ "Orange",    
                   grad.rate.x <=94.9 & change <= -1.0 ~ "Yellow",    
                   
                   grad.rate.x > 95  ~ "Blue",
                   
                   
                   TRUE ~ EstimatedColor
                   
               ),
               grad.rate = grad.rate.x
        ) %>%
        
        mutate(year = "2new") %>%
        
        bind_rows(dash2) %>%
        mutate(EstimatedColor = factor(EstimatedColor),
               EstimatedColor = fct_relevel(EstimatedColor,"Light Gray" ) ,
               year = factor(year),
               year = fct_relevel(year,"1old" ) 
        )
    
    
}





grad.comp.school <- function(df, dist.code, school.code, limit.case.count = TRUE, old.colors = TRUE ) {
    
    cds <- paste0("27",dist.code, str_pad(school.code, 7, side="left", pad="0"))
    
    
    
    work.group <-   df %>%
        filter(SchoolCode == school.code #| SchoolCode == as.numeric(str_pad(school.code, 7, side="left", pad="0"))
        ) %>%
        filter(if(limit.case.count == TRUE )count >= 30 else count >= 1) %>%
        ungroup() %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    print(work.group)
    
    dash2 <- dash.school.grad( cds ) %>%
        filter(# str_detect(districtname, dist.name),
            Group %in% work.group
        ) %>%
        select(districtname, schoolname ,indicator, currstatus, color, Group) %>%
        mutate( EstimatedColor = case_when(old.colors == FALSE ~ "Light Gray",
                                           color == 1 ~ "Red",
                                           color == 2 ~ "Orange",
                                           color == 3 ~ "Yellow",
                                           color == 4 ~ "Green",
                                           color == 5 ~ "Blue",
                                           TRUE ~ "White") 
        ) %>%
        rename(grad.rate = currstatus)%>%
        mutate(year = "1old")
    
    print(dash2)
    
    
    df %>%
        filter(SchoolCode == school.code # | SchoolCode == as.numeric(str_pad(school.code, 7, side="left", pad="0"))
        ) %>%
        filter(if(limit.case.count == TRUE )count >= 30 else count >= 1) %>%
        #      mutate(DFS = as.numeric(DFS)) %>%
        left_join(dash2, by = c("Group")) %>%
        mutate(change = grad.rate.x - grad.rate.y,
               EstimatedColor = case_when(
                   count < 30 ~ "White",
                   
                   # High Schools
                   grad.rate.x <=67.9  ~ "Red",

                   grad.rate.x <=79.9 & change >= 1.0 ~ "Yellow",
                   grad.rate.x <=79.9 & change <= -5 ~ "Red", 
                   grad.rate.x <=79.9 & change < 1.0 ~ "Orange", 
                   
                   grad.rate.x <=90.4 & change >= 1.0 ~ "Green",
                   grad.rate.x <=90.4 & change <= -1.0 ~ "Orange", 
                   grad.rate.x <=90.4 & change < 1.0 ~ "Yellow",
                   
                   grad.rate.x <=94.9 & change >= 5.0 ~ "Blue",
                   grad.rate.x <=94.9 & change > -1.0 ~ "Green",
                   grad.rate.x <=94.9 & change < -5.0 ~ "Orange",    
                   grad.rate.x <=94.9 & change <= -1.0 ~ "Yellow",    
                   
                   grad.rate.x > 95  ~ "Blue",
                   

                   TRUE ~ EstimatedColor
                   
               ),
               grad.rate = grad.rate.x
        ) %>%
        
        mutate(year = "2new") %>%
        
        bind_rows(dash2) %>%
        mutate(EstimatedColor = factor(EstimatedColor),
               EstimatedColor = fct_relevel(EstimatedColor,"Light Gray" ) ,
               year = factor(year),
               year = fct_relevel(year,"1old" ) 
               )
    
    
}



test <- grad.comp.school(mcoe.school.grad, dist.code = 10272, school.code = 2730265)


test <- grad.comp.school(mpusd.school.grad, dist.code = 66092, school.code = "0111435", old.colors = TRUE) %>%
    grad.comp.school.graph(old.colors = TRUE)


test <- grad.comp(mpusd.school.grad, dist.code = 66092,  old.colors = TRUE) %>%
    grad.comp.school.graph(old.colors = TRUE, level = "D")


# Get the graphs to work with district level too.  paste the colors up to the google sheet.  


grad.graph <- function(df) {
    
    
    work.group <-   df %>%
        select(Group) %>%
        unique() %>%
        flatten()
 
    skul <-  unique(df$districtname)[1]
    
    df %>%
        filter(year == "2new") %>%
        ggplot(aes(x = Group, y = grad.rate)) +
        geom_col(aes(fill = EstimatedColor,
                     color = "black"),
                 position = "dodge2") +
        {if(length(work.group) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
        mcoe_theme +
        scale_fill_identity() +
        scale_color_identity() +
        labs(y = "Graduation Rate",
             title = paste0(skul, " Graduation Student Group Estimates ", thisyear),
             #         subtitle = "Gray is 2023 results and Colored bars are 2024 with the estimated Dashboard color"
        )
    
    
    ggsave(here("output",save.folder ,paste0(skul," Graduation Group Estimates ", thisyear, Sys.Date(),".png")), width = 8, height = 5)
    
    
} 




grad.school.graph <- function(df) {
    
    
    work.group <-   df %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    
    skul <- df$schoolname[1]
    
    df %>%
        filter(!is.na(SchoolCode)) %>%
        ggplot(aes(x = Group, y = grad.rate)) +
        geom_col(aes(fill = EstimatedColor,
                     color = "black"),
                 position = "dodge2") +
        {if(length(work.group) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
        mcoe_theme +
        scale_fill_identity() +
        scale_color_identity() +
        labs(y = "Graduation Rate",
             title = paste0(skul, " Graduation Student Group Estimates ",thisyear),
             #         subtitle = "Gray is 2023 results and Colored bars are 2024 with the estimated Dashboard color"
        )
    
    
    ggsave(here("output",save.folder ,paste0(skul," Graduation Group Estimates ", thisyear, Sys.Date(),".png")), width = 8, height = 5)
    
    
} 

grad.comp.school.graph <- function(df, old.colors = TRUE, level = "S") {
    
    
    work.group <-   df %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    
    skul <- if_else(level == "D", unique(df$districtname)[1]  ,df$schoolname[1])
    
    df %>%
        ggplot(aes(x = Group, y = grad.rate, group = year)) + #group makes dodged columns appear in right order
        geom_col_pattern(aes(fill = EstimatedColor,
                             pattern = year,
                     color = "black"),
                 position = "dodge2") +
        # Adds stripes to the old data if using colors
        {if(old.colors==TRUE)scale_pattern_manual(values=c('stripe', 'wave'))else scale_pattern_manual(values=c('wave', 'wave'))    } +
        # If lots of student groups, then moves labels up and down to avoid overlap
        {if(length(work.group) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
        mcoe_theme +
        scale_fill_identity() +
        scale_color_identity() +
        theme(legend.position = "none") +
        
        labs(y = "Graduation Rate",
             title = paste0(skul, " Graduation Student Group Estimates 2024"),
             subtitle = if_else(old.colors == FALSE,
                                paste0("Gray is ", lastyear, " results and Colored bars are ", thisyear ," with the estimated Dashboard color"),
                                paste0("", lastyear, " results are on the left and ", thisyear ," estimates are on the right for each student group")
             )
        )
    
    
    ggsave(here("output",save.folder ,paste0(skul," Graduation Group Results ", lastyear, " and ", thisyear ," Comparison ",  if_else(old.colors == TRUE, "old colors ","") , Sys.Date(),".png")), width = 8, height = 5)
    
    
} 



grad.comp.school(mcoe.school.grad, dist.code = 10272, school.code = 2730265, limit.case.count = FALSE) %>%
    grad.comp.school.graph()




grad.all.schools <- function(df, dist.cd, limit.case.cnt = TRUE) {
    
    school.list <- df$SchoolCode %>% unique()
    
    for (i in 1:length(school.list)) {
        
        grad.df    <-    grad.comp.school(df, dist.code = dist.cd, school.code = school.list[i], limit.case.count = limit.case.cnt, old.colors = FALSE) 
        
        grad.comp.school.graph(grad.df, old.colors = FALSE)
        grad.school.graph(grad.df)
        
        grad.df.old    <-    grad.comp.school(df, dist.code = dist.cd, school.code = school.list[i], limit.case.count = limit.case.cnt, old.colors = TRUE) 
        
        grad.comp.school.graph(grad.df.old, old.colors = TRUE)
        
        
        #       ggsave(here("output",save.folder ,paste0(school.list[i], " - ","Suspension Student Group Results 2023 and 2024 Comparison ", Sys.Date(),".png")), width = 8, height = 5)
        
    }
    

}


grad.all.schools(mcoe.school.grad, dist.cd = 10272, limit.case.cnt = FALSE)




### 2024 executions ------

grad.func(cohort.23 = mpusd.2023.grad.24, cohort.24 = mpusd.2024.grad.24, completer.list = mpusd.list.grad.24,
          level = "S") %>%
    grad.all.schools(dist.cd = 66092, limit.case.cnt = TRUE)


grad.func(cohort.23 = mpusd.2023.grad.24, cohort.24 = mpusd.2024.grad.24, completer.list = mpusd.list.grad.24,
                               level = "D") %>%
    grad.comp(dist.code = 66092,  old.colors = TRUE) %>%
    grad.comp.school.graph(old.colors = TRUE, level = "D")



grad.func(cohort.23 = soledad.2023.grad.24, cohort.24 = soledad.2024.grad.24, completer.list = soledad.list.grad.24,
          level = "S") %>%
    grad.all.schools(dist.cd = 75440, limit.case.cnt = TRUE)


grad.func(cohort.23 = soledad.2023.grad.24, cohort.24 = soledad.2024.grad.24, completer.list = soledad.list.grad.24,
          level = "D") %>%
    grad.comp(dist.code = 75440,  old.colors = TRUE) %>%
    grad.comp.school.graph(old.colors = TRUE, level = "D")


grad.func(cohort.23 = soledad.2023.grad.24, cohort.24 = soledad.2024.grad.24, completer.list = soledad.list.grad.24,
          level = "D") %>%
    grad.comp(dist.code = 75440,  old.colors = TRUE) %>%
    grad.graph()




#


grad.func(cohort.23 = somoco.2023.grad.24, cohort.24 = somoco.2024.grad.24, completer.list = somoco.list.grad.24,
          level = "S") %>%
    grad.all.schools(dist.cd = 66068, limit.case.cnt = TRUE)


grad.func(cohort.23 = somoco.2023.grad.24, cohort.24 = somoco.2024.grad.24, completer.list = somoco.list.grad.24,
          level = "D") %>%
    grad.comp(dist.code = 66068,  old.colors = TRUE) %>%
    grad.comp.school.graph(old.colors = TRUE, level = "D")


grad.func(cohort.23 = somoco.2023.grad.24, cohort.24 = somoco.2024.grad.24, completer.list = somoco.list.grad.24,
          level = "D") %>%
    grad.comp(dist.code = 66068,  old.colors = TRUE) %>%
    grad.graph()



#


grad.func(cohort.23 = suhsd.2023.grad.24, cohort.24 = suhsd.2024.grad.24, completer.list = suhsd.list.grad.24,
          level = "S") %>%
    grad.all.schools(dist.cd = 66159, limit.case.cnt = TRUE)


grad.func(cohort.23 = suhsd.2023.grad.24, cohort.24 = suhsd.2024.grad.24, completer.list = suhsd.list.grad.24,
          level = "D") %>%
    grad.comp(dist.code = 66159,  old.colors = TRUE) %>%
    grad.comp.school.graph(old.colors = TRUE, level = "D")


grad.func(cohort.23 = suhsd.2023.grad.24, cohort.24 = suhsd.2024.grad.24, completer.list = suhsd.list.grad.24,
          level = "D") %>%
    grad.comp(dist.code = 66159,  old.colors = TRUE) %>%
    grad.graph()



# nmcusd ------


grad.func(cohort.old = nmcusd.2024.grad.25, cohort.new = nmcusd.2025.grad.25, completer.list = nmcusd.list.grad.25,
          level = "S") %>%
    grad.all.schools(dist.cd = 73825, limit.case.cnt = TRUE)


grad.func(cohort.old = nmcusd.2024.grad.25, cohort.new = nmcusd.2025.grad.25, completer.list = nmcusd.list.grad.25,
          level = "D") %>%
    grad.comp(dist.code = 73825,  old.colors = TRUE) %>%
    grad.comp.school.graph(old.colors = TRUE, level = "D")


grad.func(cohort.old = nmcusd.2024.grad.25, cohort.new = nmcusd.2025.grad.25, completer.list = nmcusd.list.grad.25,
          level = "D") %>%
    grad.comp(dist.code = 73825,  old.colors = TRUE) %>%
    grad.graph()

