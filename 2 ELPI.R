# Take TOMS ELPAC File and calculates ELPI


nmcusd.elpac.24 <- read_xlsx(here("data","nmcusd","27738250000000_Summative_ELPAC_and_Summative_Alternate_ELPAC_Student_Score_Data_File_TestedStudentScoreData_2024.xlsx"),
                             skip = 1
                             )


elpi.level <- function(GradeLevelWhenAssessed, ScaleScore) {

        case_when(
             GradeLevelWhenAssessed == "KN" ~ cut(ScaleScore,  c(1149, 1373, 1397, 1421, 1447, 1473, 1700),
                                                                      labels=1:6),
             GradeLevelWhenAssessed == "01" ~ cut(ScaleScore,  c(1149, 1410, 1432 , 1454, 1480 , 1506, 1700),
                                                                      labels=1:6),
             GradeLevelWhenAssessed == "02" ~ cut(ScaleScore,  c(1149, 1423, 1446 , 1470, 1500 , 1531, 1700),
                                                                      labels=1:6),
             GradeLevelWhenAssessed == "03" ~ cut(ScaleScore,  c(1149, 1447, 1467 , 1487, 1510 , 1534, 1800),
                                                                      labels=1:6),
            GradeLevelWhenAssessed == "04" ~ cut(ScaleScore, c(1149, 1458, 1478 , 1498, 1523  , 1548, 1800),
                                                                      labels=1:6),
             GradeLevelWhenAssessed == "05" ~ cut(ScaleScore, c(1149, 1466, 1489 , 1513, 1536 , 1559, 1800),
                                                                      labels=1:6),
            GradeLevelWhenAssessed == "06" ~ cut(ScaleScore,  c(1149, 1474, 1495 , 1516, 1541  , 1566, 1900),
                                                                      labels=1:6),
            GradeLevelWhenAssessed == "07" ~ cut(ScaleScore, c(1149, 1480, 1503 , 1526, 1550  , 1575, 1900),
                                                                      labels=1:6),
            GradeLevelWhenAssessed == "08" ~ cut(ScaleScore, c(1149, 1485,1509  , 1533, 1561  , 1589, 1900),
                                                                      labels=1:6),
             GradeLevelWhenAssessed == "09" ~ cut(ScaleScore, c(1149, 1492, 1518 , 1544, 1574 , 1605, 1950),
                                                                      labels=1:6),
             GradeLevelWhenAssessed == "10" ~ cut(ScaleScore, c(1149, 1492, 1518 , 1544, 1574 , 1605, 1950),
                                                                      labels=1:6),
             GradeLevelWhenAssessed == "11" ~ cut(ScaleScore, c(1149, 1499,  1526  , 1554, 1584 , 1614, 1950),
                                                                      labels=1:6),
             GradeLevelWhenAssessed == "12" ~ cut(ScaleScore,c(1149, 1499, 1526 , 1554, 1584 , 1614, 1950),
                                                                      labels=1:6)
            
        )
    
}


elpi.calc <- function(df, level = "D") {
    
    
    ddff <-     deparse(substitute(df)) 
    
temp.elpi <- df %>%
    select(SSID, TestedSchoolName1, GradeAssessed, OverallScaleScore, GradeAssessedMinus1 ,OverallScaleScoreMinus1) %>%
    mutate(OverallScaleScore = as.numeric(OverallScaleScore),
           OverallScaleScoreMinus1 = as.numeric(OverallScaleScoreMinus1)
           ) %>%
    mutate(new.elpi.level = elpi.level(GradeAssessed,OverallScaleScore),
           old.elpi.level = elpi.level(GradeAssessedMinus1,OverallScaleScoreMinus1)
              ) %>%
    na.omit() %>%
    mutate(elpi.change = as.numeric(new.elpi.level) - as.numeric(old.elpi.level),
           elpi.pos = case_when( elpi.change > 0 ~ TRUE,
                                 new.elpi.level == 6 ~ TRUE,
                                 TRUE ~ FALSE))


elpi.perc <- mean(temp.elpi$elpi.pos) 

namers <- if_else(level == "D", df$TestedDistrictName1[1] ,  temp.elpi$TestedSchoolName1[1] )

print(namers)

holder <- tibble_row(elpi.perc, namers, df$TestedDistrictName1[1] ,  nrow(temp.elpi))

# Saves the overall rate to the google sheet
sheet_append(ss = sheet,
             sheet = "ELPI",
             data = holder )

elpi.perc

}



elpi.calc(nmcusd.elpac.24 #%>%
           #   filter(str_detect(TestedSchoolName1,"Prune"))
          , "D"
          )


school.list <- unique(nmcusd.elpac.24$TestedSchoolName1)

for (i in school.list) {
    elpi.calc(nmcusd.elpac.24%>%
                  filter(str_detect(TestedSchoolName1,i)),
              "S"
    )
    
}





####  ELPAC by School -----


elpac.school <- function(df) {
    
    df %>% 
        filter( Subject =="ELPAC" ) %>%
        mutate(ScaleScoreAchievementLevel = factor(ScaleScoreAchievementLevel),
               GradeLevelWhenAssessed2 = factor(GradeLevelWhenAssessed, levels = c("KG",1,2,3,4,5,6,7,8,11)),
               AssessmentName = case_when(AssessmentName == "Kindergarten ELPAC Summative" ~ "Grade  KG ELPAC Summative",
                                          AssessmentName == "Grade 11 ELA Summative" ~ "Grade11 ELA Summative",
                                          AssessmentName == "Grade 11 Math Summative" ~ "Grade11 Math Summative",
                                          TRUE ~AssessmentName)
        ) %>%
        ggplot( aes( y = GradeLevelWhenAssessed, fill = ScaleScoreAchievementLevel)) +
        geom_bar(color = "black") +
        facet_wrap(vars(SchoolName),
                   # vars(GradeLevelWhenAssessed),
                   #    scales = "free"
        ) +
        geom_text(    stat = "count",
                      aes(label = ..count..), 
                      position = position_stack(vjust = 0.5), size = 2) +
        theme_hc() +
        scale_fill_brewer() + 
        labs(y = "",
             x = "",
             fill = "Achievement Level",
             title = "ELPAC Count of Students at each Achievement Level")
    
}

king.city %>% 
    filter(str_detect(DistrictName,"King City")) %>%
    elpac.school()  

ggsave(here("output",paste0("King City", " ELPAC by School ", Sys.Date(),".png")), width = 12, height = 7)

alisal %>% 
    filter(str_detect(DistrictName,"Alisal")) %>%
    elpac.school()  

ggsave(here("output",paste0("Alisal", " ELPAC by School ", Sys.Date(),".png")), width = 12, height = 7)


salinas.city %>% 
    filter(str_detect(DistrictName,"Salinas City") ,
           Subject =="ELPAC" ) %>%
    mutate(ScaleScoreAchievementLevel = factor(ScaleScoreAchievementLevel),
           GradeLevelWhenAssessed2 = factor(GradeLevelWhenAssessed, levels = c("KG",1,2,3,4,5,6,7,8,11)),
           AssessmentName = case_when(AssessmentName == "Kindergarten ELPAC Summative" ~ "Grade  KG ELPAC Summative",
                                      AssessmentName == "Grade 11 ELA Summative" ~ "Grade11 ELA Summative",
                                      AssessmentName == "Grade 11 Math Summative" ~ "Grade11 Math Summative",
                                      TRUE ~AssessmentName)
    ) %>%
    ggplot( aes( y = GradeLevelWhenAssessed, fill = ScaleScoreAchievementLevel)) +
    geom_bar(color = "black") +
    # facet_grid(vars(SchoolName),
    #            vars(GradeLevelWhenAssessed),
    #            scales = "free") +
    facet_wrap(vars(SchoolName),
               # vars(GradeLevelWhenAssessed),
               #    scales = "free"
    ) +
    geom_text(    stat = "count",
                  aes(label = ..count..),
                  position = position_stack(vjust = 0.5), size = 2) +
    theme_hc() +
    scale_fill_brewer() + 
    labs(y = "",
         x = "",
         fill = "Achievement Level",
         title = "Count of Students at each Achievement Level")


ggsave(here("output",paste0("Salinas City", " ELPAC by School ", Sys.Date(),".png")), width = 12, height = 7)


soledad %>% 
    filter(str_detect(DistrictName,"Soledad")) %>%
    elpac.school()  

ggsave(here("output",paste0("Soledad", " ELPAC by School ", Sys.Date(),".png")), width = 12, height = 7)




#### ELPI ----

# Calculates elpi levels 
elpi.levels <- function(df,dist) {
    
    
    df %>% 
        filter(Subject =="ELPAC" ,
               str_detect(DistrictName,dist)) %>%
        mutate(elpi_level = case_when(
            Subject == "ELPAC" & GradeLevelWhenAssessed == "KG" ~ cut(ScaleScore,
                                                                      c(1149, 1373, 1397, 1421, 1447, 1473, 1700),
                                                                      labels=1:6),
            Subject == "ELPAC" & GradeLevelWhenAssessed == "01" ~ cut(ScaleScore,
                                                                      c(1149, 1410, 1432 , 1454, 1480 , 1506, 1700),
                                                                      labels=1:6),
            Subject == "ELPAC" & GradeLevelWhenAssessed == "02" ~ cut(ScaleScore,
                                                                      c(1149, 1423, 1446 , 1470, 1500 , 1531, 1700),
                                                                      labels=1:6),
            Subject == "ELPAC" & GradeLevelWhenAssessed == "03" ~ cut(ScaleScore,
                                                                      c(1149, 1447, 1467 , 1487, 1510 , 1534, 1800),
                                                                      labels=1:6),
            Subject == "ELPAC" & GradeLevelWhenAssessed == "04" ~ cut(ScaleScore,
                                                                      c(1149, 1458, 1478 , 1498, 1523  , 1548, 1800),
                                                                      labels=1:6),
            Subject == "ELPAC" & GradeLevelWhenAssessed == "05" ~ cut(ScaleScore,
                                                                      c(1149, 1466, 1489 , 1513, 1536 , 1559, 1800),
                                                                      labels=1:6),
            Subject == "ELPAC" & GradeLevelWhenAssessed == "06" ~ cut(ScaleScore,
                                                                      c(1149, 1474, 1495 , 1516, 1541  , 1566, 1900),
                                                                      labels=1:6),
            Subject == "ELPAC" & GradeLevelWhenAssessed == "07" ~ cut(ScaleScore,
                                                                      c(1149, 1480, 1503 , 1526, 1550  , 1575, 1900),
                                                                      labels=1:6),
            Subject == "ELPAC" & GradeLevelWhenAssessed == "08" ~ cut(ScaleScore,
                                                                      c(1149, 1485,1509  , 1533, 1561  , 1589, 1900),
                                                                      labels=1:6),
            Subject == "ELPAC" & GradeLevelWhenAssessed == "09" ~ cut(ScaleScore,
                                                                      c(1149, 1492, 1518 , 1544, 1574 , 1605, 1950),
                                                                      labels=1:6),
            Subject == "ELPAC" & GradeLevelWhenAssessed == "10" ~ cut(ScaleScore,
                                                                      c(1149, 1492, 1518 , 1544, 1574 , 1605, 1950),
                                                                      labels=1:6),
            Subject == "ELPAC" & GradeLevelWhenAssessed == "11" ~ cut(ScaleScore,
                                                                      c(1149, 1499,  1526  , 1554, 1584 , 1614, 1950),
                                                                      labels=1:6),
            Subject == "ELPAC" & GradeLevelWhenAssessed == "12" ~ cut(ScaleScore,
                                                                      c(1149, 1499, 1526 , 1554, 1584 , 1614, 1950),
                                                                      labels=1:6)
            
        ))
}

# Compares a district across years to calculate estimated ELPI indicator level

elpi.change <- function(dist, df.old, df.new, filename) {
    
    
    
    ddff <-     deparse(substitute(df.new)) 
    
    
    temp.elpi.new <- elpi.levels(df.new, dist) %>%
        select(StudentIdentifier,
               elpac.new = elpi_level)
    temp.elpi.old <- elpi.levels(df.old, dist) %>%
        select(StudentIdentifier,
               elpac.old = elpi_level)
    
    temp.elpi <- full_join(temp.elpi.old,temp.elpi.new) %>%
        na.omit() %>%
        mutate(elpi.change = as.numeric(elpac.new) - as.numeric(elpac.old),
               elpi.pos = case_when( elpi.change > 0 ~ TRUE,
                                     elpac.new == 6 ~ TRUE,
                                     TRUE ~ FALSE))
    
    
    # Saves list with students to see which are included in progress calculation
    write_csv(temp.elpi, here("elpi" ,paste0(filename,".csv")))
    
    elpi.perc <- mean(temp.elpi$elpi.pos) 
    
    
    
    
    
    
    holder <- tibble_row(elpi.perc, ddff)
    
    # Saves the overall rate to the google sheet
    sheet_append(ss = sheet,
                 sheet = "ELPI",
                 data = holder )
    
    elpi.perc
    
    
}



elpi.change("King City", king.city.22, king.city.23, "King City ELPI 2023")



### ELPI by School calculations ----


school.list <- unique(alisal.23$SchoolName)

for (i in school.list) {
    elpi.change("Alisal",
                alisal.22,
                alisal.23 %>%
                    filter(str_detect(SchoolName,i)),
                i)
    
}





school.list <- unique(king.city.23$SchoolName)

for (i in school.list) {
    elpi.change("King City",
                king.city.22,
                king.city.23 %>%
                    filter(str_detect(SchoolName,i)),
                i)
    
}


king.city.23.k5 <-  king.city.23 %>%
    filter(GradeLevelWhenAssessed %in% c("KG","01","02","03","04","05"))

elpi.change("King City",
            king.city.22,
            king.city.23.k5
            ,
            "King City KG to 5th")





