

# Combines the color estimates for student groups to predict DA 



summ.sus <- read_sheet(ss = sheet,
           sheet = "Susp Group") %>%
    mutate(indicator = "Suspensions")

summ.chron <- read_sheet(ss = sheet,
                       sheet = "Chronic Group")%>%
    mutate(indicator = "Chronic")


summ.grad <- read_sheet(ss = sheet,
                         sheet = "Grad Group")%>%
    mutate(indicator = "Graduation") %>%
    filter(is.na(school.name))

summ.acad <- read_sheet(ss = sheet,
                       sheet = "Distance from Standard Group")%>%
    mutate(indicator = Test)

summ.elpi <- read_sheet(ss = sheet,
                        sheet = "ELPI")%>%
    mutate(indicator = "elpi",
           StudentGroupCategory = "EnglishLearner" ) %>%
    filter(School == District)






summ.compile <- list(summ.sus, summ.chron, summ.grad ,summ.acad, summ.elpi) %>%
    bind_rows() %>%
    filter(!is.na(EstimatedColor)) %>%
    transmute(EstimatedColor, indicator,
              StudentGroupCategory = coalesce(StudentGroupCategory, StudentGroup),
           dist = str_extract(District,"^[^.]+")) %>%
    mutate(StudentGroupCategory = case_when(str_detect(StudentGroupCategory, "Hispan") ~ "Hispanic",
                                            str_detect(StudentGroupCategory, "Two") ~ "Multiple",
                                            str_detect(StudentGroupCategory, "ELdash") ~ "EnglishLearner",
                                            str_detect(StudentGroupCategory, "Black") ~ "Black",
                                            str_detect(StudentGroupCategory, "Pac") ~ "Pacific Islander",
                                            str_detect(StudentGroupCategory, "SED") ~ "SocioEconomicallyDisadvantaged",
                                            str_detect(StudentGroupCategory, "SWD") ~ "StudentswithDisabilities",
                                            str_detect(StudentGroupCategory, "HOM") ~ "Homeless",
                                             .default = StudentGroupCategory)) %>%
    pivot_wider(names_from = indicator, values_from = EstimatedColor) %>%
    filter(StudentGroupCategory != "All")


DA.est <- summ.compile %>%
    mutate(academic = case_when((ELA == "Red" & Math == "Orange") |
                                    (ELA == "Orange" & Math == "Red") |
                                    (ELA == "Red" & Math == "Red") ~ TRUE,
                                TRUE ~ FALSE),
           chron = if_else(Chronic == "Red", TRUE, FALSE),
           susp = if_else(Suspensions == "Red", TRUE, FALSE),
           grad = if_else(Graduation == "Red", TRUE, FALSE)
           
           ) %>%
    rowwise() %>%
    mutate(count.reds = sum(academic,chron,susp,grad, na.rm = TRUE )
    )



####


summ.compile %>%
    filter(EstimatedColor == "Red") %>%
    tabyl(StudentGroupCategory)
