

# King City 

save.folder <- "king city"



dr <- read_xlsx(here("data","king city","DR.xlsx")) %>%
    mutate(school = "Del Rey") %>%
    clean_names() %>%
    mutate(teacher = paste0(teacher, " - ", if_else(current_grade == 3, "3rd", paste0(current_grade,"th")), " grade class")   )


kcam <- read_xlsx(here("data","king city","KCAM.xlsx")) %>%
    mutate(school = "Arts Magnet") %>%
    clean_names()%>%
    mutate(teacher = paste0(teacher, " - ", if_else(current_grade == 3, "3rd", paste0(current_grade,"th")), " grade class")   )


sl <- read_xlsx(here("data","king city","Santa Lucia.xlsx")) %>%
    mutate(school = "Santa Lucia") %>%
    clean_names()%>%
    mutate(teacher = paste0(teacher, " - ", if_else(current_grade == 3, "3rd", paste0(current_grade,"th")), " grade class")   )


cp_ela <- read_xlsx(here("data","king city","CPMS ELA.HistorySS teachers.xlsx")) %>%
    mutate(school = "Chalone Peaks",
           `Course Type` = if_else(str_detect(`Course Type`,"History"),"History-Social Studies","Language Arts")
           ) %>%
    mutate(Teacher = paste0(Teacher, " - ", `Course Type`)) %>%
    select(-`Course Type`) %>%
    clean_names()%>%
    mutate(teacher = paste0(teacher, " - ", if_else(current_grade == 3, "3rd", paste0(current_grade,"th")), " grade class")   )


cp_math <- read_xlsx(here("data","king city","CPMS Math by math teacher.xlsx")) %>%
    mutate(school = "Chalone Peaks") %>%
    mutate(Teacher = paste0(Teacher, " - ", `Course Type`)) %>%
    select(-`Course Type`) %>%
    clean_names()%>%
    mutate(teacher = paste0(teacher, " - ", if_else(current_grade == 3, "3rd", paste0(current_grade,"th")), " grade class")   )




percentile <- read_xlsx(here("data","CAASPPpercentile.xlsx")) %>%
    pivot_longer(starts_with("Grade")) %>%
    mutate(grade = str_sub(name, 7,-1))


thresholds <- read_xlsx(here("data","ScaleScoreReference.xlsx")) %>%
    clean_names() %>%
    transmute(subject, minimum, maximum,
        grade_tested = as.numeric(grade),
           thresh = x2) 


# Define a function to find the closest rank for a given score
find_closest_perc <- function(score, subj, grad) {
    percentile %>%
        filter(Subject == subj,
               value <= score,
               grade == grad) %>%
        arrange(desc(value)) %>%
        slice(1) %>%
        pull(Percentile)
}



kc.comp <- bind_rows(dr, kcam,sl, cp_ela, cp_math) 



### refining -------


kc.comp.clean <- kc.comp %>%
    mutate(missing.ela = if_else(is.na(state_local_by_subject_caaspp_ela_literacy_scale_score_2022_2023)|
                                     is.na(state_local_by_subject_caaspp_ela_literacy_scale_score_2023_2024),
                                 TRUE, FALSE),
           missing.math = if_else(is.na(state_local_by_subject_caaspp_math_scale_score_2022_2023)|
                                     is.na(state_local_by_subject_caaspp_math_scale_score_2023_2024),
                                 TRUE, FALSE)
           ) %>% 
    pivot_longer(starts_with("state"))  %>%
    mutate(subject = case_when(str_detect(name, "math") ~ "Math",
                               str_detect(name, "ela") ~ "ELA"),
           year = case_when(str_detect(name, "2024") ~ "2024",
                               str_detect(name, "2022") ~ "2023")
           ) %>%
    filter( (current_grade == 3 & year == 2023)  | 
                (current_grade == 3 & year == 2024 & student_state_id != 4101679664)  | 
            (subject == "Math" & missing.math == FALSE) | 
                (subject == "ELA" & missing.ela == FALSE)     
               
               ) %>%
    na.omit()



kc.comp.long <- kc.comp.clean %>%
    group_by(school, teacher, subject, current_grade, year) %>%
    summarise(mean = mean(value), count = n())


# Step 2: Use dplyr to apply the function to each row and create a new column
students_with_rank <- kc.comp.long %>%
    rowwise() %>%
    mutate(grade_tested = if_else(year == 2024, as.numeric(current_grade), as.numeric(current_grade)-1),
           rank = find_closest_perc(score = mean, 
                                    subj = subject, 
                                    grad = grade_tested)) %>%
    ungroup() %>%
    left_join(thresholds) %>%
    mutate(dfs = round(mean - thresh, 1),
           groupme = teacher)


students_with_rank %>%
    filter(teacher == "Alvarez, Johnathan") %>%
    # { max_val <<- max(.$maximum) 
    # min_val <<- min(.$minimum)
    # . } %>%
    ggplot(aes(x = year, colour = subject)) +
    geom_point(aes(y = thresh), shape = 61, size = 5) +
    geom_point(aes(y = mean), shape = "triangle", size = 3) +
    geom_label(aes(y = mean, label = rank), nudge_x = 0.3) +
    geom_text(aes(y = thresh +(dfs/2), label = dfs), color = "darkgrey") +
#    scale_y_continuous(limits = c(min_val,max_val)) +
#    lims( x = c(2023, 2024), y = c(minimum, maximum)) +
    facet_grid(~subject) +
    scale_color_few() +
    mcoe_theme +
    theme(legend.position = "none") +
    labs(title = paste0("Alvarez, Johnathan", " - CAASPP Class Averages and Thresholds"),
         subtitle = "Triangles show the average score for students with two years of results. \nEquals are the grade and subject level thresholds for Standards met.\nThe label shows the approximate percentile in the state."
)


ggsave(here("output",save.folder, "percentiles", paste0("Alvarez, Jonathan2",".png" ) )  )





### GRaphing ----


two.yr.percentile.graph <- function(df, lab = "temp") {
    
    
    max_val <- max(df$maximum)
    min_val <- min(df$minimum)
    
    lab <- unique(df$groupme)
    
df %>%
    ggplot(aes(x = year, colour = subject)) +
    geom_point(aes(y = thresh), shape = 61, size = 5) +
    geom_point(aes(y = mean), shape = "triangle", size = 3) +
    geom_label(aes(y = mean, label = rank), nudge_x = 0.3) +
    geom_text(aes(y = thresh +(dfs/2), label = dfs), color = "darkgrey") +
    #    scale_y_continuous(limits = c(min_val,max_val)) +
    #    lims( x = c(2023, 2024), y = c(minimum, maximum)) +
    facet_grid(~subject) +
    scale_color_few() +
    mcoe_theme +
    theme(legend.position = "none") +
    labs(title = paste0(lab, " - CAASPP Class Averages"),
         subtitle = "Triangles show the average score for students with two years of results. \nEquals are the grade and subject level for at Standards Met.\nThe label shows the approximate percentile in the state."
    )


ggsave(here("output",save.folder, "percentiles", paste0(lab,".png" ) )  )

}




students_with_rank %>%
    group_split(groupme) %>%
 map(two.yr.percentile.graph)





# school x grade level
kc.comp.clean %>% 
    group_by(school, subject, current_grade, year) %>%
    summarise(mean = mean(value), count = n()) %>%
    rowwise() %>%
    mutate(grade_tested = if_else(year == 2024, as.numeric(current_grade), as.numeric(current_grade)-1),
           rank = find_closest_perc(score = mean, 
                                    subj = subject, 
                                    grad = grade_tested)) %>%
    ungroup() %>%
    left_join(thresholds) %>%
    mutate(dfs = round(mean - thresh, 1),
           groupme = paste0(school, " ", current_grade)) %>%
    group_split(groupme) %>%
    map(two.yr.percentile.graph)


# school x grade level (SWD)
kc.comp.clean %>% 
    filter(swd == "Yes") %>%
    group_by(school, subject, current_grade, year) %>%
    summarise(mean = mean(value), count = n()) %>%
    rowwise() %>%
    mutate(grade_tested = if_else(year == 2024, as.numeric(current_grade), as.numeric(current_grade)-1),
           rank = find_closest_perc(score = mean, 
                                    subj = subject, 
                                    grad = grade_tested)) %>%
    ungroup() %>%
    left_join(thresholds) %>%
    mutate(dfs = round(mean - thresh, 1),
           groupme = paste0(school, " SWD ", current_grade)) %>%
    group_split(groupme) %>%
    map(two.yr.percentile.graph)


# school x grade level (EL)
kc.comp.clean %>% 
    filter(lep == "Yes") %>%
    group_by(school, subject, current_grade, year) %>%
    summarise(mean = mean(value), count = n()) %>%
    rowwise() %>%
    mutate(grade_tested = if_else(year == 2024, as.numeric(current_grade), as.numeric(current_grade)-1),
           rank = find_closest_perc(score = mean, 
                                    subj = subject, 
                                    grad = grade_tested)) %>%
    ungroup() %>%
    left_join(thresholds) %>%
    mutate(dfs = round(mean - thresh, 1),
           groupme = paste0(school, " EL ", current_grade)) %>%
    group_split(groupme) %>%
    map(two.yr.percentile.graph)









# grade level
kc.comp.clean %>% 
    group_by( subject, current_grade, year) %>%
    summarise(mean = mean(value), count = n()) %>%
    rowwise() %>%
    mutate(grade_tested = if_else(year == 2024, as.numeric(current_grade), as.numeric(current_grade)-1),
           rank = find_closest_perc(score = mean, 
                                    subj = subject, 
                                    grad = grade_tested)) %>%
    ungroup() %>%
    left_join(thresholds) %>%
    mutate(dfs = round(mean - thresh, 1),
           groupme = paste0("Grade ", current_grade)) %>%
    group_split(groupme) %>%
    map(two.yr.percentile.graph)





### facet per class ---


kc.students <- kc.comp.clean %>%
 #   filter(teacher == "Alvarez, Johnathan - 5th grade class") %>%
    rowwise() %>%
    
    mutate(grade_tested = if_else(year == 2024, as.numeric(current_grade), as.numeric(current_grade)-1),
           mean = value,
           rank = find_closest_perc(score = mean, 
                                    subj = subject, 
                                    grad = grade_tested)) %>%
    left_join(thresholds) %>%
    mutate(dfs = round(mean - thresh, 1),
           groupme = paste0("Grade ", current_grade)) %>%

        ggplot(aes(x = year, colour = subject)) +
    geom_point(aes(y = thresh), shape = 61, size = 5) +
    geom_point(aes(y = mean), shape = "triangle", size = 3, position = "jitter") +
    geom_label(aes(y = mean, label = rank), nudge_x = 0.3) +
  #  geom_text(aes(y = thresh +(dfs/2), label = dfs), color = "darkgrey") +
 #   facet_wrap(~student_state_id) +
    scale_color_few() +
    mcoe_theme +
    theme(legend.position = "none") +
    labs(title = paste0("Alvarez, Johnathan", " - CAASPP Class Averages and Thresholds"),
         subtitle = "Triangles show the average score for students with two years of results. \nEquals are the grade and subject level thresholds for Standards met.\nThe label shows the approximate percentile in the state."
    )


ggsave(here("output",save.folder, "percentiles", paste0("Alvarez, Jonathan all students 3",".png" ) ), width = 12, height = 9  )

### students

kc.students.share <- kc.students %>%
    select(student_state_id,swd,lep,current_grade, school_of_enrollment, teacher, value, subject,year,  rank, dfs) %>%
    pivot_wider(names_from = c(subject, year), values_from = c(value, rank, dfs))




write_excel_csv(kc.students, "Students with percentile long.csv")
write_excel_csv(kc.students.share, "Students with percentile wide.csv")

write_excel_csv(students_with_rank, "teacher by school by grade.csv")


#. school x grade 

kc.comp.clean %>% 
    group_by(school, subject, current_grade, year) %>%
    summarise(mean = mean(value), count = n()) %>%
    rowwise() %>%
    mutate(grade_tested = if_else(year == 2024, as.numeric(current_grade), as.numeric(current_grade)-1),
           rank = find_closest_perc(score = mean, 
                                    subj = subject, 
                                    grad = grade_tested)) %>%
    ungroup() %>%
    left_join(thresholds) %>%
    mutate(dfs = round(mean - thresh, 1),
           groupme = paste0(school, " ", current_grade)) %>%
    
    write_excel_csv( "School by grade.csv")
    


kc.comp.clean %>% 
    group_by( subject, current_grade, year) %>%
    summarise(mean = mean(value), count = n()) %>%
    rowwise() %>%
    mutate(grade_tested = if_else(year == 2024, as.numeric(current_grade), as.numeric(current_grade)-1),
           rank = find_closest_perc(score = mean, 
                                    subj = subject, 
                                    grad = grade_tested)) %>%
    ungroup() %>%
    left_join(thresholds) %>%
    mutate(dfs = round(mean - thresh, 1),
           groupme = paste0("Grade ", current_grade))%>%
    
    write_excel_csv( " by grade.csv")
