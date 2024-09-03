
# Looks at grad rates 


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
                                         "Black/African Am" ~ "Black/\nAfrican Am",
                                         "Nat Hwiin/Othr Pac Islndr" ~ "Pacific Islander",
                                         "Multiple" ~ "Multiple \nRaces",
                                         .default = name
               ))



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






#### GRaphs ------

dash.school.grad <- function(cdsCode) {
    
    dash %>%
        filter(cds == cdsCode,
               rtype == "S",
               indicator == "GRAD")
    
}


temp <- dash.school.grad(27102722730117)


grad.comp.school <- function(df, dist.code, school.code, limit.case.count = TRUE ) {
    
    cds <- paste0("27",dist.code, str_pad(school.code, 7, side="left", pad="0"))
    
    # ent <- tbl(con,"SCHOOLS") %>%
    #     filter(
    #         CDSCode == cds
    #         #        County == "Monterey"
    #     ) %>%
    #     collect()  %>%
    #     select(EILName) %>%
    #     simplify()
    # 
    # print(ent)
    
    
    
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
        select(districtname, schoolname ,indicator, currstatus, Group) %>%
        mutate(EstimatedColor = "Light Gray") %>%
        rename(grad.rate = currstatus)
    
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
        
        
        bind_rows(dash2) %>%
        mutate(EstimatedColor = factor(EstimatedColor),
               EstimatedColor = fct_relevel(EstimatedColor,"Light Gray" ) )
    
    
}



test <- grad.comp.school(mcoe.school.grad, dist.code = 10272, school.code = 2730265)



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
             title = paste0(skul, " Graduation Student Group Estimates 2024"),
             #         subtitle = "Gray is 2023 results and Colored bars are 2024 with the estimated Dashboard color"
        )
    
    
    ggsave(here("output",save.folder ,paste0(skul," Graduation Group Estimates 2024 ", Sys.Date(),".png")), width = 8, height = 5)
    
    
} 

grad.comp.school.graph <- function(df) {
    
    
    work.group <-   df %>%
        select(Group) %>%
        unique() %>%
        flatten()
    
    
    skul <- df$schoolname[1]
    
    df %>%
        ggplot(aes(x = Group, y = grad.rate)) +
        geom_col(aes(fill = EstimatedColor,
                     color = "black"),
                 position = "dodge2") +
        {if(length(work.group) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
        mcoe_theme +
        scale_fill_identity() +
        scale_color_identity() +
        labs(y = "Graduation Rate",
             title = paste0(skul, " Graduation Student Group Estimates 2024"),
             subtitle = "Gray is 2023 results and Colored bars are 2024 with the estimated Dashboard color")
    
    
    ggsave(here("output",save.folder ,paste0(skul," Graduation Group Results 2023 and 2024 Comparison ", Sys.Date(),".png")), width = 8, height = 5)
    
    
} 



grad.comp.school(mcoe.school.grad, dist.code = 10272, school.code = 2730265, limit.case.count = FALSE) %>%
    grad.comp.school.graph()




grad.all.schools <- function(df, dist.cd, limit.case.cnt = TRUE) {
    
    school.list <- df$SchoolCode %>% unique()
    
    for (i in 1:length(school.list)) {
        
        grad.df    <-    grad.comp.school(df, dist.code = dist.cd, school.code = school.list[i], limit.case.count = limit.case.cnt) 
        
        grad.comp.school.graph(grad.df)
        grad.school.graph(grad.df)
        
        
        #       ggsave(here("output",save.folder ,paste0(school.list[i], " - ","Suspension Student Group Results 2023 and 2024 Comparison ", Sys.Date(),".png")), width = 8, height = 5)
        
    }
    
    
}


grad.all.schools(mcoe.school.grad, dist.cd = 10272, limit.case.cnt = FALSE)
