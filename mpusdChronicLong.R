

# Chronic graphs for all years, schools and student groups 




chronic.mpusd <- tbl(con,"CHRONIC") %>%
    filter(district_code == "66092",
     #      charter_school == "No" #| charter_yn == "No",
     #      dass == "All"
           #  rtype == "D",
           #        indicator == "ela" | indicator == "math",
           # reportingyear == "2023"
           ) %>%
  #  head(10000) %>%
    collect() # %>%
    mutate(Group = case_match(studentgroup,
                              "HOM" ~ "Homeless",
                              "SWD" ~ "Students with \nDisabilities",
                              "SED" ~ "Socio-Economically \nDisadvantaged",
                              "HI" ~ "Latino",
                              "EL" ~ "English \nLearner",
                              "AS" ~ "Asian",
                              "FI" ~ "Filipino",
                              "WH" ~ "White",
                              "ALL" ~ "All",
                              "AA" ~ "Black/\nAfrican Am",
                              "PI" ~ "Pacific Islander",
                              "MR" ~ "Multiple \nRaces",
                              .default = studentgroup
    ))


    chronic.mpusd2 <-    chronic.mpusd %>% 
        filter(!str_detect(reporting_category, "G")) %>%
        mutate(charter = case_when(
            academic_year >= "2020-21" & charter_school == "No" ~ "Yes",
            academic_year < "2020-21" & aggregate_level == "S" ~ "Yes",
            academic_year < "2020-21" & charter_school == "No" ~ "Yes"
            
            
        )
               ) %>%
        filter(charter == "Yes") %>%
        left_join_codebook("CHRONIC","reporting_category") %>%
        filter(!str_detect(definition, "Not Report")) %>%
        filter(!(str_detect(definition, "Indian") & str_detect(school_name, "Monterey High") ) )

    
    
fake.2020 <-  chronic.mpusd2 %>%
    filter(!is.na(chronic_absenteeism_rate)) %>%
    
        filter(academic_year <= "2020-21") %>%
        mutate(chronic_absenteeism_rate = NA,
               academic_year = "2019-20")
    
    
    
mpusd.chron.24 <- mpusd.abs.school.joint %>%
    pivot_longer(
        # pick out demographics columns based on values; 
        # create new columns with their names and values
        where(\(x) all(x %in% c('Yes', 'N', NA))) 
    ) |> 
    # retain only values corresponding to actual demographic populations
    filter(value == 'Yes') |> 
    group_by(name, value, SchoolName) |> 
    # calculate average value of `AbsenceRate2` and total student per group
    summarise(
        academic_year = "2023-24",
        chronic_absenteeism_rate = round(100*mean(chronic),1) ,
              total = n()
              ) %>%
    filter(total >=11,
           !str_detect(name, "Missing")
           ) |>
    rename(        school_name = SchoolName) %>%
    mutate(definition = case_match(name, 
        "All"                          ~ "Total",
        "Am Indian/Alskn Nat"         ~"American Indian or Alaska Native",
        # "Asian"                         =
         "Black/African Am" ~"African American",
         "EnglishLearner" ~ "English Learners"    ,
        # "Filipino"                      =
         "Hispanic" ~ "Hispanic or Latino" ,
        # "Homeless"                      =
        # "Missing"                       =
         "Multiple" ~ "Two or More Races" ,
         "Nat Hwiin/Othr Pac Islndr"    ~ "Pacific Islander"  ,
         "SocioEconomicallyDisadvantaged"~"Socioeconomically Disadvantaged" ,
         "StudentswithDisabilities"      ~ "Students with Disabilities"  ,
        # "White"                        =
        .default =  name
        
    ))









# [1] "Asian"                           
# [2] "African American"                
# [3] "Filipino"                        
# [4] "Hispanic or Latino"              
# [5] "American Indian or Alaska Native"
# [6] "Pacific Islander"                
# [7] "Two or More Races"               
# [8] "White"                           
# [9] "Students with Disabilities"      
# [10] "English Learners"                
# [11] "Foster"                          
# [12] "Homeless"                        
# [13] "Socioeconomically Disadvantaged" 
# [14] "Total"          



mpusd.chron.graph <-  function(school.name) {
 

    
 chronic.df <- chronic.mpusd2 %>%
   filter(!is.na(chronic_absenteeism_rate)) %>%
   bind_rows(fake.2020) %>%
    bind_rows(mpusd.chron.24) %>%
        filter(str_detect(school_name, school.name)) %>%
    mutate(labby = if_else(academic_year == max(academic_year)|academic_year == "2018-19", definition ,"" ),
           chronic_absenteeism_rate = if_else(academic_year == "2020-21", NA ,chronic_absenteeism_rate ),
           definition = factor(definition),
           definition = fct_relevel(definition, "Total")
           ) 
 
 
        ggplot(chronic.df, aes(x= academic_year, 
                   y = chronic_absenteeism_rate, 
                   group = definition, 
                   colour = definition,
                   label = chronic_absenteeism_rate
                  # label = labby
                   )
               ) +
        geom_line(size = 3) +
        geom_point(size = 5) +
    expand_limits(y = 0) +
        geom_text(color = "black",
                  vjust = -1) +
    geom_blank( aes(x=academic_year, y=chronic_absenteeism_rate*1.1, label=chronic_absenteeism_rate)) +
    scale_color_manual(values = colors_vec) +
     mcoe_theme +
     scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    facet_wrap(~definition) +
     
     theme(panel.border = element_rect(fill=NA,color="darkgrey", size=0.5, 
                                         linetype="solid"),
           strip.background = element_rect(color="black", size=0.5, linetype="solid"  )
           
           )+  # Adds a border around each panel
    theme(legend.position = "none") +
    labs(title = paste0(school.name, " Chronic Absenteeism Rates by Student Group Over Time"),
         subtitle = "From 2016-17 through 2023-24 school years",
        caption = "Note 2019-20 and 2020-21 are removed because the reliability of absenteeism data was significantly impacted by the COVID-19 pandemic and distance learning.",
        y = "Percentage Chronically Absent"
    )

    

ggsave(here("output","mpusd","chronic",paste0(school.name, " Chronic Over Time ",  Sys.Date(),".png")), width = 15, height = 9  )
    
} 
    
   
mpusd.chron.graph("Central Coast High")

mpusd.list <- c(
     "Crumpton"                ,                      
     "La Mesa"                  ,                                      
     "Marina Vista"              ,                          
     "Ord Terrace"                ,                         
     "Los Arboles"                 ,                            
     "Martin Luther King"           ,                                  
     "Seaside Middle"                ,                                 
     "Marina High"                    ,                                
     "Colton"                          ,                        
     "Monte Vista"                      ,                              
     "Dual Language Academy"             ,   
     "Central Coast High"                 ,                            
     "Monterey High"                       ,                           
     "Seaside High"                         ,                          
     "Del Rey Woods"                         ,              
     "Marshall"                               ,   
     "Olson"                                   ,       
    "Foothill"                                  ,          
     "Highland"                                  
)

for (variable in mpusd.list) {
    
    mpusd.chron.graph(variable)
    
}






 
    
    library(RColorBrewer)
    
    # Create a palette with 14 colors
    palette_14 <- colorRampPalette(brewer.pal(12, "Set1"))(14)
    
    # Example usage with a barplot
    barplot(rep(1, 14), col = palette_14, border = NA)    
    
    # 
    # [1] "#66C2A5" "#B6A580" "#F38E6A" "#B798A2"
    # [5] "#9A9CC9" "#CB90C5" "#D79CA9" "#B4C66D"
    # [9] "#C1D848" "#F1D834" "#F4D055" "#E6C58C"
    # [13] "#CDBCA2" "#B3B3B3"
    # 
    colors_vec <- c(
        "Asian"                           = "#E41A1C",
        "African American"                = "#79577C", 
        "Filipino"                        = "#3C899E",
        "Hispanic or Latino"              =  "#49A75A",
        "American Indian or Alaska Native"= "#6F8273",
        "Pacific Islander"                =  "#9F5196",
        "Two or More Races"               =  "#DF6F32",
        "White"                           =  "#FFA60F",
        "Students with Disabilities"      = "#FFF52F",
        "English Learners"                =  "#CFA42D",
        "Foster"                          =  "#B25C3F" ,
        "Homeless"                        = "#E4779C",
        "Socioeconomically Disadvantaged" = "#D28AB0" ,
        "Total"                           = "#999999"
    )
    
