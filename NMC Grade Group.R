

nmcusd.grade.group.24 <-  nmcusd.24 %>%
    mutate(ScaleScoreAchievementLevel = factor(ScaleScoreAchievementLevel),
           All = "Yes"
    ) %>%
    left_join(reference2) %>%
#    group_by(Subject, GradeLevelWhenAssessed, CALPADSSchoolName) %>%
    mutate(dist.standard = ScaleScore - MeetStandard) %>%
    pivot_longer(
    # pick out demographics columns based on values; 
    # create new columns with their names and values
    where(\(x) all(x %in% c('Yes', 'N', NA))) 
    ) |> 
    # retain only values corresponding to actual demographic populations
    filter(value == 'Yes') |> 
    group_by(name, value, Subject, GradeLevelWhenAssessed, CALPADSSchoolName) |> 
    # calculate average value of `AbsenceRate2` and total student per group
    summarise(mean = mean(dist.standard), total = n()) |>
    filter(total >10) |>
    ungroup() |>
    mutate(mean = round(mean,1),
           name = case_match(name,
                                         "HOM" ~ "Homeless",
                                         "SWD" ~ "Students with \nDisabilities",
                                         "SED" ~ "Socio-Economically \nDisadvantaged",
                                         "HispanicOrLatinoEthnicity" ~ "Latino",
                                         "ELdash" ~ "English Learner",
                                         .default = name
           )) |>
    select(-value)

write_rds(nmcusd.grade.group.24, "nmcusd_grade_group_24.rds")



DT::datatable(temp2,
              rownames = FALSE,
              colnames = c("Student Group",
                           "Assessment" ,
                           "Grade Level" ,
                           "School Name",
                           "Average DFS",
                           "Number of Students"),
              options = list(pageLength = 25)
)




#####




mcoe.grade.group.24 <-  mcoe.24 %>%
    mutate(ScaleScoreAchievementLevel = factor(ScaleScoreAchievementLevel),
           All = "Yes"
    ) %>%
    left_join(reference2) %>%
    #    group_by(Subject, GradeLevelWhenAssessed, CALPADSSchoolName) %>%
    mutate(dist.standard = ScaleScore - MeetStandard) %>%
    pivot_longer(
        # pick out demographics columns based on values; 
        # create new columns with their names and values
        where(\(x) all(x %in% c('Yes', 'N', NA))) 
    ) |> 
    # retain only values corresponding to actual demographic populations
    filter(value == 'Yes') |> 
    group_by(name, value, Subject, GradeLevelWhenAssessed, CALPADSSchoolName) |> 
    # calculate average value of `AbsenceRate2` and total student per group
    summarise(mean = mean(dist.standard), total = n()) |>
    filter(total >10) |>
    ungroup() |>
    mutate(mean = round(mean,1),
           name = case_match(name,
                             "HOM" ~ "Homeless",
                             "SWD" ~ "Students with \nDisabilities",
                             "SED" ~ "Socio-Economically \nDisadvantaged",
                             "HispanicOrLatinoEthnicity" ~ "Latino",
                             "ELdash" ~ "English Learner",
                             .default = name
           )) |>
    select(-value)

write_rds(nmcusd.grade.group.24, "nmcusd_grade_group_24.rds")



DT::datatable(temp2,
              rownames = FALSE,
              colnames = c("Student Group",
                           "Assessment" ,
                           "Grade Level" ,
                           "School Name",
                           "Average DFS",
                           "Number of Students"),
              options = list(pageLength = 25)
)
