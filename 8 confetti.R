

#### Colors --------


colors_vec <- c(
    "Asian"                           = "#E41A1C",
    "Black/\nAfrican Am"                = "#CFA42D", 
    "Filipino"                        = "#D28AB0",
    "Latino"                          =  "#49A75A",
    "American Indian or Alaska Native"= "#6F8273",
    "Pacific Islander"                =  "#9F5196",
    "Two or More Races"               =  "#DF6F32",
    "White"                           =  "#FFA60F",
    "Students with \nDisabilities"      = "#FFF52F",
    "English \nLearner"                =   "#79577C",
    "Foster"                          =  "#B25C3F" ,
    "Homeless"                        = "#E4779C",
    "Socio-Economically \nDisadvantaged" =   "#3C899E", 
    "All"                           = "#999999"
)



#### Get Data -------

soledad.dash <- tbl(con,"DASH_ALL") %>%
     filter(cds == "27754400000000",
    #        indicator %in% c("MATH", "ELA", "CHRO")
    #        #      charter_school == "No" #| charter_yn == "No",
    #        #      dass == "All"
    #        #  rtype == "D",
    #        #        indicator == "ela" | indicator == "math",
    #        # reportingyear == "2023"
     ) %>%
      head(10000) %>%
    collect() %>%
mutate(Group = case_match(studentgroup,
                          "HOM" ~ "Homeless",
                          "SWD" ~ "Students with \nDisabilities",
                          "SED" ~ "Socio-Economically \nDisadvantaged",
                          "HI" ~ "Latino",
                          "EL" ~ "English \nLearner",
                          "AS" ~ "Asian",
                          "FOS" ~ "Foster",
                          "FI" ~ "Filipino",
                          "WH" ~ "White",
                          "ALL" ~ "All",
                          "AA" ~ "Black/\nAfrican Am",
                          "PI" ~ "Pacific Islander",
                          "MR" ~ "Multiple \nRaces",
                          .default = studentgroup
))  %>%
    mutate(DFS = currstatus,
           PercentChronicAbsent = currstatus)




working <- read_sheet(ss = sheet,
                      sheet = "Distance from Standard Group") %>%
    mutate(Group = case_match(StudentGroup,
                              "HOM" ~ "Homeless",
                              "SWD" ~ "Students with \nDisabilities",
                              "SED" ~ "Socio-Economically \nDisadvantaged",
                              "HispanicOrLatinoEthnicity" ~ "Latino",
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
    bind_rows(working.plus) %>%
    filter(str_starts( District, "soledad"  )) %>%
    mutate(reportingyear = 2024,
           indicator = str_to_upper( Test) )



soledad.caaspp.hist <- working %>%
    bind_rows(soledad.dash) %>%
    select(Group, DFS, reportingyear, indicator) %>%
    na.omit() %>%
    filter(Group %notin% c("CAA", "ELO", "EO", "RFP", "SBA")) %>%
    mutate(DFS = round(DFS,1))


### Graph ------

soledad.caaspp.hist %>%
filter(indicator == "ELA") %>%
    
    ggplot( aes(x= reportingyear, 
                           y = DFS, 
                           group = Group, 
                           colour = Group,
                           label = DFS
                           # label = labby
    )
    ) +
        geom_line(size = 3) +
        geom_point(size = 5) +
        expand_limits(y = 0) +
        geom_text(color = "black",
                  vjust = -1) +
        geom_blank( aes(x=reportingyear, y=DFS*1.1, label=DFS)) +
        scale_color_manual(values = colors_vec) +
        mcoe_theme +
        scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
        facet_wrap(~Group) +
        
        theme(panel.border = element_rect(fill=NA,color="darkgrey", size=0.5, 
                                          linetype="solid"),
              strip.background = element_rect(color="black", size=0.5, linetype="solid"  )
              
        )+  # Adds a border around each panel
        theme(legend.position = "none") +
        labs(title = paste0( " Soledad Union ELA Rates by Student Group Over Time"),
             subtitle = "From 2021-22 through 2023-24 school years",
             y = "Distance from Standard"
        )
    
    
    
    ggsave(here("output","soledad",paste0("Soledad Union ELA Over Time ",  Sys.Date(),".png")), width = 15, height = 9  )
    




#### 
    
    
    
    working <- read_sheet(ss = sheet,
                          sheet = "Chronic Group") %>%
        mutate(Group = case_match(StudentGroupCategory,
                                  "HOM" ~ "Homeless",
                                  "SWD" ~ "Students with \nDisabilities",
                                  "SED" ~ "Socio-Economically \nDisadvantaged",
                                  "HispanicOrLatinoEthnicity" ~ "Latino",
                                  "ELdash" ~ "English \nLearner",
                                  "BlackorAfricanAmerican" ~ "Black/\nAfrican Am",
                                  "HawaiianOrOtherPacificIslander" ~ "Pacific Islander",
                                  "TwoorMoreRaces" ~ "Multiple \nRaces",
                                  "HOM" ~ "Homeless",
                                  "FOS" ~ "Foster",
                                  "StudentswithDisabilities" ~ "Students with \nDisabilities",
                                  "SocioEconomicallyDisadvantaged" ~ "Socio-Economically \nDisadvantaged",
                                  "Hispanic" ~ "Latino",
                                  "EnglishLearner" ~ "English \nLearner",
                                  "BlackorAfricanAmerican" ~ "Black/\nAfrican Am",
                                  "HawaiianOrOtherPacificIslander" ~ "Pacific Islander",
                                  "TwoorMoreRaces" ~ "Multiple \nRaces",
                                  
                                  
                                  .default = StudentGroupCategory
        )) %>% 
        filter(!is.na(EstimatedColor),
               str_starts( District, "soledad"  )) %>%
        mutate(reportingyear = 2024,
               indicator = "CHRO" )
    
    
    
working %>%
        bind_rows(soledad.dash) %>%
        filter(indicator == "CHRO") %>%
        select(Group, PercentChronicAbsent, reportingyear, indicator) %>%
    mutate(PercentChronicAbsent = round(PercentChronicAbsent,1)) %>%
        na.omit()   %>%
        
        ggplot( aes(x= reportingyear, 
                    y = PercentChronicAbsent, 
                    group = Group, 
                    colour = Group,
                    label = PercentChronicAbsent
                    # label = labby
        )
        ) +
        geom_line(size = 3) +
        geom_point(size = 5) +
        expand_limits(y = 0) +
        geom_text(color = "black",
                  vjust = -1) +
        geom_blank( aes(x=reportingyear, y=PercentChronicAbsent*1.1, label=PercentChronicAbsent)) +
        scale_color_manual(values = colors_vec) +
        mcoe_theme +
        scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
        facet_wrap(~Group) +
        
        theme(panel.border = element_rect(fill=NA,color="darkgrey", size=0.5, 
                                          linetype="solid"),
              strip.background = element_rect(color="black", size=0.5, linetype="solid"  )
              
        )+  # Adds a border around each panel
        theme(legend.position = "none") +
        labs(title = paste0( " Soledad Union Chronic Absenteeism Rates by Student Group Over Time"),
             subtitle = "From 2021-22 through 2023-24 school years",
             y = "Percentage Chronically Absent"
        )
    
    
    
    ggsave(here("output","soledad",paste0("Soledad Union Chronic Over Time ",  Sys.Date(),".png")), width = 15, height = 9  )
    
    
    
#### Functions for other districts ---------
    
    
    
    #### Get Data -------
    
dash.all <- tbl(con,"DASH_ALL") %>%
        filter(countyname == "Monterey",
            # cds == "27754400000000",
               #        indicator %in% c("MATH", "ELA", "CHRO")
               #        #      charter_school == "No" #| charter_yn == "No",
               #        #      dass == "All"
               #        #  rtype == "D",
               #        #        indicator == "ela" | indicator == "math",
                reportingyear >= "2022"
        ) %>%
      #  head(10000) %>%
        collect() %>%
        mutate(Group = case_match(studentgroup,
                                  "HOM" ~ "Homeless",
                                  "SWD" ~ "Students with \nDisabilities",
                                  "SED" ~ "Socio-Economically \nDisadvantaged",
                                  "HI" ~ "Latino",
                                  "EL" ~ "English \nLearner",
                                  "AS" ~ "Asian",
                                  "FOS" ~ "Foster",
                                  "FI" ~ "Filipino",
                                  "WH" ~ "White",
                                  "ALL" ~ "All",
                                  "AA" ~ "Black/\nAfrican Am",
                                  "PI" ~ "Pacific Islander",
                                  "MR" ~ "Multiple \nRaces",
                                  .default = studentgroup
        ))  %>%
        mutate(DFS = currstatus,
               PercentChronicAbsent = currstatus)
    
dash.all <-     dash.all %>%
      mutate(DFS = currstatus,
             PercentChronicAbsent = currstatus)
    
    
    working <- read_sheet(ss = sheet,
                          sheet = "Distance from Standard Group") %>%
        mutate(Group = case_match(StudentGroup,
                                  "HOM" ~ "Homeless",
                                  "SWD" ~ "Students with \nDisabilities",
                                  "SED" ~ "Socio-Economically \nDisadvantaged",
                                  "HispanicOrLatinoEthnicity" ~ "Latino",
                                  "ELdash" ~ "English \nLearner",
                                  "LTELdash" ~ "Long Term\nEnglish\nLearner",
                                  
                                  "BlackorAfricanAmerican" ~ "Black/\nAfrican Am",
                                  "HawaiianOrOtherPacificIslander" ~ "Pacific Islander",
                                  "TwoorMoreRaces" ~ "Multiple \nRaces",
                                  
                                  .default = StudentGroup
        ))
    
    working.plus <- read_sheet(ss = sheet,
                               sheet = "Distance from Standard") %>%
        mutate(Group = "All")
    
    
    
confetti  <- function(google.sheet.dist = "soledad", ccddss, indi) {


    working <- working %>%
        bind_rows(working.plus) %>%
        filter(str_starts( District, google.sheet.dist  )) %>%
        mutate(reportingyear = as.numeric(thisyear),
               indicator = str_to_upper( Test) )
    
    
    
dash.small <- dash.all %>%
        filter(cds == ccddss,
               indicator %in% c("MATH", "ELA", "CHRO")
        ) 

dist.name <- dash.small$districtname[1]

  
  lea.caaspp.hist <- dash.small %>% 
     bind_rows(working) %>%
        select(Group, DFS, reportingyear, indicator) %>%
        na.omit() %>%
        filter(Group %notin% c("CAA", "ELO", "EO", "RFP", "SBA")) %>%
        mutate(DFS = round(DFS,1))

# Graph 
    
    lea.caaspp.hist %>%
        mutate(reportingyear = factor(reportingyear)) %>%
        
        filter(indicator == indi) %>%
        
        ggplot( aes(x= reportingyear, 
                    y = DFS, 
                    group = Group, 
                    colour = Group,
                    label = DFS
                    # label = labby
        )
        ) +
        geom_line(size = 3) +
        geom_point(size = 5) +
        expand_limits(y = 0) +
        geom_text(color = "black",
                  vjust = -1) +
        geom_blank( aes(x=reportingyear, y=DFS*1.2, label=DFS)) +
        scale_color_manual(values = colors_vec) +
        mcoe_theme +
 #       scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
        facet_wrap(~Group) +
        
        theme(panel.border = element_rect(fill=NA,color="darkgrey", size=0.5, 
                                          linetype="solid"),
              strip.background = element_rect(color="black", size=0.5, linetype="solid"  )
              
        )+  # Adds a border around each panel
        theme(legend.position = "none") +
        labs(title = paste0(dist.name," ",indi, " Rates by Student Group Over Time"),
             subtitle = paste0("From ", as.numeric(thisyear) - 2 ," through ", thisyear," school years"),
             y = "Distance from Standard"
        )
    
    
    
    ggsave(here("output",save.folder, paste0(dist.name," ",indi," Over Time ",  Sys.Date(),".png")), width = 12, height = 7  )
    
}   
    
  
confetti("greenfield", 27660350000000, "ELA")  


confetti("alisal", 27659610000000, "ELA") 


# do the holder by school first

confetti.school  <- function(ccddss, indi) {
    
    
    working <- holder %>%
        filter(CDS == ccddss,
               count >=10) %>%
        mutate(reportingyear = 2024,
               indicator = str_to_upper( Test) )
    
    dash.small <- dash.all %>%
        filter(cds == ccddss,
               indicator == indi
        ) 
    
    school.name <- dash.small$schoolname[1]
    
    
    lea.caaspp.hist <- dash.small %>% 
        bind_rows(working) %>%
        select(Group, DFS, reportingyear, indicator) %>%
        na.omit() %>%
        filter(Group %notin% c("CAA", "ELO", "EO", "RFP", "SBA")) %>%
        mutate(DFS = round(DFS,1))
    
    # Graph 
    
    lea.caaspp.hist %>%
        mutate(reportingyear = factor(reportingyear)) %>%
        
        filter(indicator == indi) %>%
        
        ggplot( aes(x= reportingyear, 
                    y = DFS, 
                    group = Group, 
                    colour = Group,
                    label = DFS
                    # label = labby
        )
        ) +
        geom_line(size = 3) +
        geom_point(size = 5) +
        expand_limits(y = 0) +
        geom_text(color = "black",
                  vjust = -1) +
        geom_blank( aes(x=reportingyear, y=DFS*1.2, label=DFS)) +
        scale_color_manual(values = colors_vec) +
        mcoe_theme +
        #       scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
        facet_wrap(~Group) +
        
        theme(panel.border = element_rect(fill=NA,color="darkgrey", size=0.5, 
                                          linetype="solid"),
              strip.background = element_rect(color="black", size=0.5, linetype="solid"  )
              
        )+  # Adds a border around each panel
        theme(legend.position = "none") +
        labs(title = paste0(school.name," CAASPP ",indi, " Meeting or Exceeding Standard"),
             subtitle = paste0("Three Year Trend (2022-24)"),
             y = "Distance from Standard"
        )
    
    
    
    ggsave(here("output",save.folder, paste0(school.name," ",indi," Over Time ",  Sys.Date(),".png")), width = 12, height = 7  )
    
}   

confetti.school( 27660350136796, "MATH")

dash.all %>%
    filter(str_detect(districtname, "Greenfield")) %>%
    distinct(cds)

school.list <- c( "27660350136796",
 "27660356026116",
 "27660356089387",
 "27660356111637",
 "27660356111645"
)

for (i in school.list) {
    for (j in c("ELA","MATH")) {
        confetti.school( i, j)
    }
}



    
    #### 
    
confetti.chron  <- function(google.sheet.dist = "soledad", ccddss) {
    
    
    working <- read_sheet(ss = sheet,
                          sheet = "Chronic Group") %>%
        mutate(Group = case_match(StudentGroupCategory,
                                  "HOM" ~ "Homeless",
                                  "SWD" ~ "Students with \nDisabilities",
                                  "SED" ~ "Socio-Economically \nDisadvantaged",
                                  "HispanicOrLatinoEthnicity" ~ "Latino",
                                  "ELdash" ~ "English \nLearner",
                                  "BlackorAfricanAmerican" ~ "Black/\nAfrican Am",
                                  "HawaiianOrOtherPacificIslander" ~ "Pacific Islander",
                                  "TwoorMoreRaces" ~ "Multiple \nRaces",
                                  "HOM" ~ "Homeless",
                                  "FOS" ~ "Foster",
                                  "StudentswithDisabilities" ~ "Students with \nDisabilities",
                                  "SocioEconomicallyDisadvantaged" ~ "Socio-Economically \nDisadvantaged",
                                  "Hispanic" ~ "Latino",
                                  "EnglishLearner" ~ "English \nLearner",
                                  "BlackorAfricanAmerican" ~ "Black/\nAfrican Am",
                                  "HawaiianOrOtherPacificIslander" ~ "Pacific Islander",
                                  "TwoorMoreRaces" ~ "Multiple \nRaces",
                                  
                                  
                                  .default = StudentGroupCategory
        )) %>% 
        filter(!is.na(EstimatedColor),
               str_starts( District, google.sheet.dist  )) %>%
        mutate(reportingyear = 2024,
               indicator = "CHRO" )
    
    
    
    dash.small <- dash.all %>%
        filter(cds == ccddss,
      #         indicator %in% c("CHRO")
        ) 
    
    dist.name <- dash.small$districtname[1]
    
    
    lea.chro.hist <- dash.small %>% 
        bind_rows(working) %>%
   #     filter(indicator == "CHRO") %>%
        select(Group, PercentChronicAbsent, reportingyear, indicator) %>%
        mutate(PercentChronicAbsent = round(PercentChronicAbsent,1)) %>%
        na.omit()  
    
    lea.chro.hist
    
    
    lea.chro.hist    %>%

        ggplot( aes(x= reportingyear,
                    y = PercentChronicAbsent,
                    group = Group,
                    colour = Group,
                    label = PercentChronicAbsent
                    # label = labby
        )
        ) +
        geom_line(size = 3) +
        geom_point(size = 5) +
        expand_limits(y = 0) +
        geom_text(color = "black",
                  vjust = -1) +
        geom_blank( aes(x=reportingyear, y=PercentChronicAbsent*1.1, label=PercentChronicAbsent)) +
        scale_color_manual(values = colors_vec) +
        mcoe_theme +
 #       scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
        facet_wrap(~Group) +

        theme(panel.border = element_rect(fill=NA,color="darkgrey", size=0.5,
                                          linetype="solid"),
              strip.background = element_rect(color="black", size=0.5, linetype="solid"  )

        )+  # Adds a border around each panel
        theme(legend.position = "none") +
        labs(title = paste0( paste0(dist.name," Chronic Absenteeism Rates by Student Group Over Time")),
             subtitle = "From 2021-22 through 2023-24 school years",
             y = "Percentage Chronically Absent"
        )


        ggsave(here("output",save.folder, paste0(dist.name," Chronic Over Time ",  Sys.Date(),".png")), width = 12, height = 7  )
        
}


confetti.chron("greenfield", 27660350000000)      






### Confetti by indicator with dashboard colors for dots and using estimates 


extra.cols.confetti <- c(DFS = NA_real_, 
                         PercentSusp = NA_real_, 
                         PercentGrad = NA_real_, 
                         PercentChronicAbsent = NA_real_, 
                         `Estimated ELPI`= NA_real_ 
                         )


confetti.district.colors  <- function(google.sheet.dist = "soledad", ccddss, indi, labl.me = TRUE) {
  
  
  
  tit <- case_when(indi == "MATH" ~ "Math",
                   indi == "CHRO" ~ "Chronic Absenteeism",
                #   indi == "CCI" ~ "College Career Readiness",
                   indi == "GRAD" ~ "Graduation Rate",
                   indi == "ELPI" ~ "English Language Progress",
                   indi == "ELA" ~ "ELA",
                   indi == "SCIENCE" ~ "Science",
                   indi == "SUSP" ~ "Suspension",
                   TRUE ~ indi) 
  
  sheeeet <- case_when(indi == "MATH" ~ "Distance from Standard Group",
                   indi == "CHRO" ~ "Chronic Group",
               #    indi == "CCI" ~ "College Career Readiness",
                   indi == "GRAD" ~ "Grad Group",
                   indi == "ELPI" ~ "ELPI",
                   indi == "ELA" ~ "Distance from Standard Group",
                   indi == "SCIENCE" ~ "Science",
                   indi == "SUSP" ~ "Susp Group",
                   TRUE ~ indi) 
  
  print(sheeeet)
  
  working.all <- read_sheet(ss = sheet,
                        sheet = "Distance from Standard") %>%
    rename(StudentGroup = Group) %>%
    { if(indi %in% c("ELA","MATH")) filter(., str_to_upper(Test) == indi )
      else filter(., is.na(EstimatedColor))
    }
  
  
  working <- read_sheet(ss = sheet,
                        sheet = sheeeet) %>%
    if (indi == "ELPI") rename(
      GroupUsed = studentgroup#,
       #   new_name2 = old_name2
          # add more renames here
        ) %>%
    bind_rows(working.all) # %>%


  print(working)
  
  working <- working %>%

    mutate(GroupUsed = if ("Group" %in% names(.)) Group else StudentGroup) %>%

    
    
    mutate(Group = case_match(GroupUsed,
                              "HOM" ~ "Homeless",
                              "SWD" ~ "Students with \nDisabilities",
                              "SED" ~ "Socio-Economically \nDisadvantaged",
                              "HispanicOrLatinoEthnicity" ~ "Latino",
                              "ELdash" ~ "English \nLearner",
                              "LTELdash" ~ "Long Term\nEnglish\nLearner",

                              "BlackorAfricanAmerican" ~ "Black/\nAfrican Am",
                              "HawaiianOrOtherPacificIslander" ~ "Pacific Islander",
                              "TwoorMoreRaces" ~ "Multiple \nRaces",

                              .default = GroupUsed
    )) %>%
     filter(str_starts( District, google.sheet.dist  )) %>%

    { if(indi %in% c("ELA","MATH")) filter(., str_to_upper(Test) == indi )
      else filter(., !is.na(EstimatedColor))
      } %>%
    add_column(!!!extra.cols.confetti[!names(extra.cols.confetti) %in% names(.)]) %>%

     mutate(reportingyear = as.numeric(thisyear),
            indicator = indi,
            currstatus = case_when(
              indi %in% c("ELA","MATH") ~ DFS,
              indi == "SUSP" ~ PercentSusp,
              indi == "GRAD" ~ PercentGrad,
              indi == "CHRO" ~ PercentChronicAbsent,
              indi == "ELPI" ~ `Estimated ELPI`
              
            )
            )

print(working)


  dash.small <- dash.all %>%
    filter(cds == ccddss,
           indicator == indi,
           reportingyear >= (as.numeric(thisyear)-2)
    )

  dist.name <- dash.small$districtname[1]


   lea.caaspp.hist <- dash.small %>%
     bind_rows(working) %>%
     select(Group, currstatus, reportingyear, indicator, color, EstimatedColor) # %>%
   #  na.omit() %>%
   
   print(lea.caaspp.hist)
   
   lea.caaspp.hist <- lea.caaspp.hist %>%
     filter(Group %notin% c("CAA", "ELO", "EO", "RFP", "SBA"),
            !is.na(currstatus))   %>%
     mutate(currstatus = round(currstatus,1),
           GraphColor =  case_when(
                     color == 0 ~ "grey60",
                     color == 1 ~ "firebrick",
                     color == 2 ~ "chocolate1",
                     color == 3 ~ "gold1",
                     color == 4 ~ "springgreen3",
                     color == 5 ~ "royalblue3",
                     EstimatedColor == "White" ~ "grey60",
                     EstimatedColor == "Red" ~ "firebrick",
                     EstimatedColor == "Orange" ~ "chocolate1",
                     EstimatedColor == "Yellow" ~ "gold1",
                     EstimatedColor == "Green" ~ "springgreen3",
                     EstimatedColor == "Blue" ~ "royalblue3",

                   )
            ) %>%
     group_by(Group) %>%
     filter(any(GraphColor != "grey60")) %>%
     ungroup()


   print(lea.caaspp.hist)
   
  # Graph

  lea.caaspp.hist %>%
    mutate(reportingyear = factor(reportingyear)) %>%

    filter(indicator == indi) %>%

    ggplot( aes(x= reportingyear,
                y = currstatus,
                group = Group,
                colour = Group,
                label = currstatus
                # label = labby
    )
    ) +
    geom_line(size = 2,
              color = "grey40") +
    geom_point(size = 5, aes(color = GraphColor)) +
    expand_limits(y = 0) +
    { if(labl.me == TRUE) geom_text(color = "black",
                                    vjust = -1)
   #   else filter(., is.na(EstimatedColor))
    } +
    geom_blank( aes(x=reportingyear, 
                    y=currstatus,#*1.2, 
                    label=currstatus)) +
    scale_color_identity() +

 #   scale_color_manual(values = colors_vec) +
    mcoe_theme +
    #       scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    facet_wrap(~Group) +

    theme(panel.border = element_rect(fill=NA,color="darkgrey", size=0.5,
                                      linetype="solid"),
          strip.background = element_rect(color="black", size=0.5, linetype="solid"  )

    )+  # Adds a border around each panel
    theme(legend.position = "none") +
    labs(title = paste0(dist.name," - ",tit, " by Student Group Over Time"),
         subtitle = paste0("From ", as.numeric(thisyear) - 2 ," through ", thisyear," school years"),
         caption = paste0(thisyear, " is estimated based on CAASPP and CALPADS files from the district and is expected to change at Dashboard release")
 #        y = "Distance from Standard"
    )

  #
   ggsave(here("output",save.folder, paste0(dist.name," ",indi," Over Time ",  Sys.Date(),".png")), width = 12, height = 7  )
  
}   



cds.code <- nmcusd.25$CALPADSDistrictCode[1]
print(cds.code)

confetti.district.colors(google.sheet.dist = "gonz",
                         ccddss = cds.code, 
                         indi = "MATH",
                         labl.me = FALSE
                         )


confetti.district.colors(google.sheet.dist = "nmcusd",
                         ccddss = cds.code, 
                         indi = "ELPI",
                         labl.me = FALSE
)

### version with all.sheets ----



confetti.district.colors.all.sheets  <- function(which.sheet = all.sheets, google.sheet.dist = "soledad", ccddss, indi, labl.me = TRUE, el.onlys = FALSE) {
  
  
  
  tit <- case_when(indi == "MATH" ~ "Math",
                   indi == "CHRO" ~ "Chronic Absenteeism",
                   #   indi == "CCI" ~ "College Career Readiness",
                   indi == "GRAD" ~ "Graduation Rate",
                   indi == "ELPI" ~ "English Language Progress",
                   indi == "ELA" ~ "ELA",
                   indi == "SCIENCE" ~ "Science",
                   indi == "SUSP" ~ "Suspension",
                   TRUE ~ indi) 
  
  y.labl <- case_when(indi == "MATH" ~ "Average distance from standard",
                      indi == "CHRO" ~ "Percent of students",
                      #   indi == "CCI" ~ "College Career Readiness",
                      indi == "GRAD" ~ "Percent of students",
                      indi == "ELPI" ~ "Percent of students",
                      indi == "ELA" ~ "Average distance from standard",
                      indi == "SCIENCE" ~ "Average number of science points",
                      indi == "SUSP" ~ "Percent of students")

  
  working <- which.sheet %>%
    filter(str_detect(str_to_lower(indicator), str_to_lower(indi)) 
           ) %>%
    filter(str_starts( District, google.sheet.dist  )) %>%
  
    mutate(reportingyear = as.numeric(thisyear),
 #          indicator = indi,
           currstatus = case_when(
             indi %in% c("ELA","MATH") ~ DFS,
             indi == "SUSP" ~ PercentSusp,
             indi == "GRAD" ~ PercentGrad,
             indi == "CHRO" ~ PercentChronicAbsent,
             indi == "ELPI" ~ `Estimated ELPI`*100
             
           )
    )
  
  print(working)
  
  
  dash.small <- dash.all %>%
    {if(el.onlys==TRUE)filter(.,str_detect(Group,"Learner|All" ) )else filter(.,str_detect(cds,"0" ) )    } %>%
    filter(cds == ccddss,
           indicator == indi,
           reportingyear >= (as.numeric(thisyear)-2)
    ) 
    
  
  dist.name <- dash.small$districtname[1]
  
  
  lea.caaspp.hist <- dash.small %>%
    bind_rows(working) %>%
    select(Group, currstatus, reportingyear, indicator, color, EstimatedColor) # %>%
  #  na.omit() %>%
  
  print(lea.caaspp.hist)
  
  lea.caaspp.hist <- lea.caaspp.hist %>%
    filter(Group %notin% c("CAA", "ELO", "EO", "RFP", "SBA"),
           !is.na(currstatus))   %>%
    mutate(currstatus = round(currstatus,1),
           GraphColor =  case_when(
             color == 0 ~ "grey60",
             color == 1 ~ "firebrick",
             color == 2 ~ "chocolate1",
             color == 3 ~ "gold1",
             color == 4 ~ "springgreen3",
             color == 5 ~ "royalblue3",
             EstimatedColor == "White" ~ "grey60",
             EstimatedColor == "Red" ~ "firebrick",
             EstimatedColor == "Orange" ~ "chocolate1",
             EstimatedColor == "Yellow" ~ "gold1",
             EstimatedColor == "Green" ~ "springgreen3",
             EstimatedColor == "Blue" ~ "royalblue3",
             
           )
    ) %>%
    group_by(Group) %>%
    filter(any(GraphColor != "grey60")) %>%
    ungroup()
  
  
  print(lea.caaspp.hist)
  
  # Graph
  
  lea.caaspp.hist %>%
    mutate(reportingyear = factor(reportingyear)) %>%
    
  #  filter(indicator == indi) %>%
    
    ggplot( aes(x= reportingyear,
                y = currstatus,
                group = Group,
                colour = Group,
                label = currstatus
                # label = labby
    )
    ) +
    geom_line(size = 2,
              color = "grey40") +
    geom_point(size = 5, aes(color = GraphColor)) +
    expand_limits(y = 0) +
    { if(labl.me == TRUE) geom_text(color = "black",
                                    vjust = -1)
      #   else filter(., is.na(EstimatedColor))
    } +
    geom_blank( aes(x=reportingyear, 
                    y=currstatus,#*1.2, 
                    label=currstatus)) +
    scale_color_identity() +
    
    #   scale_color_manual(values = colors_vec) +
    mcoe_theme +
    #       scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    facet_wrap(~Group) +
    
    theme(panel.border = element_rect(fill=NA,color="darkgrey", size=0.5,
                                      linetype="solid"),
          strip.background = element_rect(color="black", size=0.5, linetype="solid"  )
          
    )+  # Adds a border around each panel
    theme(legend.position = "none") +
    labs(title = paste0(dist.name," - ",tit, " by Student Group Over Time"),
         subtitle = paste0("From ", as.numeric(thisyear) - 2 ," through ", thisyear," school years"),
         y = y.labl,
         caption = paste0(thisyear, " is estimated based on CAASPP and CALPADS files from the district and is expected to change at Dashboard release")
         #        y = "Distance from Standard"
    )
  
  #
#  ggsave(here("output",save.folder, paste0(dist.name," ",indi," Over Time ",  Sys.Date(),".png")), width = 12, height = 7  )
  
}   


cds.code <- nmcusd.25$CALPADSDistrictCode[1]


confetti.district.colors.all.sheets(google.sheet.dist = "gonz",
                         ccddss = cds.code, 
                         indi = "MATH",
                         labl.me = FALSE
)


cds.code <- mpusd.25$CALPADSDistrictCode[1]


confetti.district.colors.all.sheets(google.sheet.dist = "mpusd",
                                    ccddss = cds.code, 
                                    indi = "CHRO",
                                    labl.me = FALSE
)

ggsave(here("output",save.folder, paste0("MPUSD - ","Chronic", " three years.png")), width = 12, height = 7  )







el.sheet <- all.sheets %>%
  filter(str_detect(Group,"Learner|All" ) )

confetti.district.colors.all.sheets(
                         which.sheet = el.sheet,
                         el.onlys = TRUE,
                          google.sheet.dist = "nmcusd",
                         ccddss = cds.code, 
                         indi = "SUSP",
                         labl.me = FALSE
) +
  scale_y_continuous(limits = c(0,100))

# have ELPI y axis go to 100% 


ltel.tribble <- tribble(~cds,~district,
                        nmcusd.25$CALPADSDistrictCode[1], "nmcusd",
                        scesd.25$CALPADSDistrictCode[1], "scesd" ,
                        alisal.25$CALPADSDistrictCode[1], "alisal",
                        mpusd.25$CALPADSDistrictCode[1], "mpusd",
                        soledad.25$CALPADSDistrictCode[1], "soledad")
                        
for (i in 1:5) {
  
  for (inder in c("ELA","MATH","CHRO","SUSP")) {
    
  
  confetti.district.colors.all.sheets(
    which.sheet = el.sheet,
    el.onlys = TRUE,
    google.sheet.dist = pull(ltel.tribble[i,2]),
    ccddss = pull(ltel.tribble[i,1]), 
    indi = inder,
    labl.me = FALSE
  )

  ggsave(here("output",save.folder, paste0(pull(ltel.tribble[i,2])," - ",inder, " three years.png")), width = 12, height = 7  )

    }
  
  confetti.district.colors.all.sheets(
    which.sheet = el.sheet,
    el.onlys = TRUE,
    google.sheet.dist = pull(ltel.tribble[i,2]),
    ccddss = pull(ltel.tribble[i,1]), 
    indi = "ELPI",
    labl.me = FALSE
  ) +
    scale_y_continuous(limits = c(0,100))
  
  ggsave(here("output",save.folder, paste0(pull(ltel.tribble[i,2])," - ","ELPI", " three years.png")), width = 12, height = 7  )
  
  
  
  
}




#### For Math CoP -------




