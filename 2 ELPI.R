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


elpi.calc <- function(df, level = "D", el.group = "EL") {
    
    
    ddff <-     deparse(substitute(df)) 
    
temp.elpi <- df %>%
    select(TestedSchoolName1, GradeAssessed, OverallScaleScore, GradeAssessedMinus1 ,OverallScaleScoreMinus1, ELEntryDate, ELASforTesting) %>%
    mutate(studentgroup = case_when(ELEntryDate <= ymd( paste0(yr -7,"-06-15")) ~ "LTEL",
                          TRUE ~ "EL")    ) %>%
    filter(if(el.group == "LTEL") studentgroup == "LTEL" else ELASforTesting == "EL") %>%
    
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


elpi.perc <- mean(temp.elpi$elpi.pos) %>% round2(3)

namers <- if_else(level == "D", df$TestedDistrictName1[1] ,  temp.elpi$TestedSchoolName1[1] )

print(namers)

holder <- tibble_row(elpi.perc, 
                     schoolname = namers, 
                     districtname = df$TestedDistrictName1[1] ,  
                     nrow(temp.elpi), 
                     studentgroup = el.group,
                     ddff)


# Compares to last year 

local.dash <- dash.all %>%
    filter(indicator == "ELPI",
           countyname == "Monterey",
           districtname == df$TestedDistrictName1[1],
   #               cds == cdsCode,
           rtype == level,
           reportingyear == as.character(yr-1) ) %>%
    select(schoolname, districtname, studentgroup, currstatus, color) %>%
    mutate(schoolname = if_else(is.na(schoolname),districtname,schoolname),
           currstatus = currstatus/100) %>%
    mutate( old.color = case_when(#old.colors == FALSE ~ "Light Gray",
        color == 1 ~ "Red",
        color == 2 ~ "Orange",
        color == 3 ~ "Yellow",
        color == 4 ~ "Green",
        color == 5 ~ "Blue",
        TRUE ~ "White") 
    ) 


print(local.dash)

holder <- left_join(holder, local.dash) %>%
    mutate(change = elpi.perc - currstatus,
           estimated.color = case_when(
               elpi.perc <=.349 & change <= .019 ~ "Red",
               elpi.perc <=.349 & change >= .1 ~ "Yellow",
               elpi.perc <=.349 & change >= .02 ~ "Orange",
               
               elpi.perc <=.449 & change <= -.101 ~ "Red",
               elpi.perc <=.449 & change >= .02 ~ "Yellow",   
               elpi.perc <=.449 & change <= .19 ~ "Orange",
               
               elpi.perc <=.549 & change <= -.02 ~ "Orange",
               elpi.perc <=.549 & change >= .02 ~ "Green",
               elpi.perc <=.549 & change >= -.019 ~ "Yellow",
               
               elpi.perc <=.649 & change <= -.101 ~ "Orange",
               elpi.perc <=.649 & change <= -.02 ~ "Yellow",
               elpi.perc <=.649 & change <= .099 ~ "Green",
               elpi.perc <=.649 & change >= .10 ~ "Blue",

               elpi.perc >=.65 & change <= -.101 ~ "Yellow",
               elpi.perc >=.65 & change <= -.02 ~ "Green",
               elpi.perc >=.65 & change >= -.019 ~ "Blue"
           )
           )

# Saves the overall rate to the google sheet
sheet_append(ss = sheet,
             sheet = "ELPI",
             data = holder )

elpi.perc

}



elpi.calc(mpusd.elpac.25 #%>%
           #   filter(str_detect(TestedSchoolName1,"Prune"))
          , "D" # , "LTEL"
          )


school.list <- unique(nmcusd.elpac.24$TestedSchoolName1)

for (i in school.list) {
    elpi.calc(nmcusd.elpac.24%>%
                  filter(str_detect(TestedSchoolName1,i)),
              "S"
    )
    
}




temp <- dash |>
    filter(indicator == "ELPI",
           str_detect(districtname, "Peninsula")) %>%
    select(schoolname, currstatus)



### ELPI calc for all the levels ;  Used to make graphs for progress etc. -----





temp.elpi <- soledad.elpac.24 %>%
    filter(str_detect(TestedDistrictName1, "Soledad" ) ) %>%
    select(SSID, TestedSchoolName1, GradeAssessed, OverallScaleScore, GradeAssessedMinus1 ,OverallScaleScoreMinus1) %>%
    mutate(OverallScaleScore = as.numeric(OverallScaleScore),
           OverallScaleScoreMinus1 = as.numeric(OverallScaleScoreMinus1)
    ) %>%
    mutate(new.elpi.level = elpi.level(GradeAssessed,OverallScaleScore),
           old.elpi.level = elpi.level(GradeAssessedMinus1,OverallScaleScoreMinus1)
    ) %>%
    na.omit() %>%
    mutate(elpi.change = as.numeric(new.elpi.level) - as.numeric(old.elpi.level),
           elpi.cat = case_when( elpi.change > 0 ~ "Progressed",
                                 elpi.change == 0 & new.elpi.level == 6 ~ "Maintained at Top",
                                 elpi.change == 0  ~ "Maintained",
                                 elpi.change < 0 ~ "Decreased"),
           elpi.pos = case_when( elpi.change > 0 ~ TRUE,
                                 new.elpi.level == 6 ~ TRUE,
                                 TRUE ~ FALSE)) %>%
    group_by(TestedSchoolName1, elpi.cat) %>%
    summarise(n = n()) %>%
    group_by(TestedSchoolName1) %>%
    mutate(perc = n / sum(n),
           reportingyear = 2024) %>%
    rename(schoolname = TestedSchoolName1)



temp.elpi.wide <- temp.elpi %>%
    pivot_wider(id_cols = schoolname, names_from = elpi.cat, values_from = c(n, perc) )


dash.elpi <- dash.all %>% 
    filter(indicator == "ELPI",
           str_detect(districtname,"Soledad"),
           reportingyear == 2023) %>%
    select(schoolname, reportingyear, currdenom, currprogressed, currmaintain_pl4, currmaintain_oth, currdeclined , currprogressed_alternate, currmaintain_pl3_alternate, currnotprognotmain_alternate) %>%
    mutate(pctprogressed = (currprogressed + currprogressed_alternate )/currdenom,
           pctmaintaintop = (currmaintain_pl4 + currmaintain_pl3_alternate )/currdenom,
           pctmaintain = (currmaintain_oth + currnotprognotmain_alternate )/currdenom,
           pctdecline = (currdeclined  )/currdenom,
           summed = currprogressed+ currmaintain_pl4+ currmaintain_oth+currdeclined + currprogressed_alternate+currmaintain_pl3_alternate+ currnotprognotmain_alternate) %>%
    select(schoolname, reportingyear, starts_with("pct")) %>%
    pivot_longer(cols = starts_with("pct") , values_to = "perc"   ) %>%
    mutate(elpi.cat = case_match(name, "pctprogressed" ~ "Progressed",
                                 "pctmaintaintop" ~ "Maintained at Top",
                                 "pctmaintain" ~ "Maintained",
                                 "pctdecline" ~"Decreased"
                                     )) %>%
    select(-name)


elpi.graph.df <- temp.elpi %>%
    bind_rows(dash.elpi) %>%
    mutate(reportingyear = factor(reportingyear),
           elpi.cat = fct_rev(elpi.cat))


elpi.graph.dist.df <- temp.elpi %>%
    ungroup() %>%
    group_by(elpi.cat) %>%
    summarise(n2 = sum(n))  %>%
    mutate(perc = n2 / sum(n2),
           reportingyear = 2024) %>%
    bind_rows(dash.elpi %>%
                  filter(is.na(schoolname))) %>%
    mutate(reportingyear = factor(reportingyear),
           elpi.cat = fct_rev(elpi.cat))


elpi.graph.dist.df %>%
    ggplot(aes(x = reportingyear, y = perc, group = ( elpi.cat), fill = elpi.cat, label = paste0( round(100* perc, 1), "%"))) +
    geom_col(width = .6) +
    geom_label(position = position_stack(vjust = 0.5), show.legend = FALSE) +
    mcoe_theme +
    theme(axis.text.y = element_blank()) +
    labs(title = paste0( "Soledad Two Year Comparison of English Learners"),
         subtitle = "English Language Progress Indicator based on ELPAC scores",
         caption = "Note: 2023 includes Alternative ELPAC results but 2024 does not")

ggsave(here("output", save.folder, "elpi" , paste0("Soledad ELPI two year comparison ", Sys.Date(),".png")), width = 8, height = 6)





elpi.graph <- function(df, skul) {
    


df %>%
    filter( str_detect(schoolname,skul)) %>%
    ggplot(aes(x = reportingyear, y = perc, group = ( elpi.cat), fill = elpi.cat, label = paste0( round(100* perc, 1), "%"))) +
    geom_col(width = .6) +
    geom_label(position = position_stack(vjust = 0.5), show.legend = FALSE) +
        mcoe_theme +
        theme(axis.text.y = element_blank()) +
        labs(title = paste0(skul, " Two Year Comparison of English Learners"),
             subtitle = "English Language Progress Indicator based on ELPAC scores",
             caption = "Note: 2023 includes Alternative ELPAC results but 2024 does not")

ggsave(here("output", save.folder, "elpi" , paste0(skul, " ELPI two year comparison ", Sys.Date(),".png")), width = 8, height = 6)

    
}




elpi.graph(elpi.graph.df, "Jack")


school.list <- unique(elpi.graph.df$schoolname)

for (i in school.list) {
    
elpi.graph(elpi.graph.df, i)

}


temp <- elpi.graph.df %>%
    group_by(schoolname, reportingyear) %>%
    summarise(sum(n))


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





