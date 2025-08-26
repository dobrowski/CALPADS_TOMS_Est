

### pull the data from the difference district tabs, label with correct indicator




acad <- read_sheet(ss = sheet,
                      sheet = "Distance from Standard Group") %>%
    mutate(indicator = Test,
           Group = OldStudentGroup) %>%
    mutate(Group = case_match(Group,
                              "English Learner" ~ "English \nLearner",
                              "LTEL" ~ "Long Term\nEnglish\nLearner",
                              .default = Group
    ) ) 

acad.plus <- read_sheet(ss = sheet,
                           sheet = "Distance from Standard") %>%
    mutate(Group = "All") %>%
    mutate(indicator = Test)


elpi <- read_sheet(ss = sheet,
                   sheet = "ELPI") %>%
    mutate(indicator = "ELPI",
           EstimatedColor = `Estimated Color`) %>%
    filter(District == School,
   #        (count >=15 & studentgroup == "LTEL") |  (count >= 30) 
           ) %>%
    mutate(District = district.df) %>%
    mutate(Group = case_match(studentgroup,
                         "EL" ~ "English \nLearner",
                         "LTEL" ~ "Long Term\nEnglish\nLearner"
    ) ) 
                                                                 


chro <- read_sheet(ss = sheet,
                   sheet = "Chronic Group") %>%
    mutate(indicator = "Chronic") %>%
    filter( if_else(StudentGroupCategory %in% c("Homeless","LTEL"), NumberStudents >=15,   NumberStudents >=30  ) )


susp <- read_sheet(ss = sheet,
                   sheet = "Susp Group") %>%
    mutate(indicator = "Suspension") %>%
    filter( if_else(StudentGroupCategory %in% c("Homeless","LTEL"), NumberStudents >=15,   NumberStudents >=30  ) )


grad <- read_sheet(ss = sheet,
                       sheet = "Grad Group") %>%
    mutate(indicator = "Graduation") %>%
    filter( is.na(School) ,
            if_else(StudentGroupCategory %in% c("Homeless","LTEL"), NumberStudents >=15,   NumberStudents >=30  ) )


science <- read_sheet(ss = sheet,
                   sheet = "Science") %>%
    mutate(indicator = "Science") %>%
    filter( str_ends(cds, "000000") ,
            if_else(StudentGroupCategory %in% c("Homeless","LTEL"), NumberStudents >=15,   NumberStudents >=30  ) )


### transform to standard combined data set 

all.sheets <- list(acad, acad.plus, elpi, chro, susp, grad, science) %>%
    bind_rows() %>%
    mutate(indicator = factor(indicator, levels = c("ELA","Math","Science","ELPI", "Chronic", "Graduation" ,"Suspension") 
                                 ) 
    )
           

### graph building on the dashboard matrix code 

library(ggtext)

dash.graph <- function(df, dist, grouping = "D") {
    
    tit <- df %>%
        filter(str_detect(District,dist)) %>%
        select(School) %>%
        na.omit() %>%
        slice(1) %>%
        pull()
    
    print(tit)
    
    df %>%
        filter(           (indicator == "ELPI" & count >=15 & studentgroup == "LTEL") |  (indicator == "ELPI" & count >= 30) | indicator != "ELPI"
) %>%
        
        { if(grouping == "S" ) filter(., str_detect(schoolname,dist)) else filter(., str_detect(District,dist))} %>%
        ggplot() +
        geom_tile(aes(y = reorder(Group, desc(Group)),  # Student group
                      x = indicator,  # Indicator2
                      fill = EstimatedColor, #factor(color, levels = c("1","2","3","4","5")),   # Color
                      # color = "black",  # as.factor(`DA Eligible`), 
                      width=0.95, # width and heigth are adjusted to allow the color borders to go fully around
                      height=0.95
        ),
        lwd = .75,
        color = "black"
        )  +
        ggthemes::theme_hc() +
        #           geom_text(size = 2, position = position_dodge(width = 1)),
        ggplot2::theme(plot.title.position = "plot")    +
        
        theme(axis.text.x = element_markdown(color = "black", size = 11) ) +
        scale_fill_identity(#values = color.pal,
                          drop = FALSE) +
        #      scale_color_manual( values = da.pal) +
        
        labs(title = paste0(tit," Estimated Student Group Status"),
             x = "",
             y = "",
             caption = paste0("Source: CAASPP and CALPADS files provided by LEA")
        )  +
 #        theme(plot.title = element_markdown(family = "Rockwell", hjust=0.5)
 #        ) +
        
        guides(#color = guide_legend(title = "DA Eligible",   # Prettify the legends
            #                             title.position = "top",
            #                             label.position = "bottom"
            # ),
            fill = "none" #guide_legend(title = "Dashboard Colors",
            # title.position = "top",
            # title.hjust = .5,
            # label.position = "bottom",
            # nrow = 1
            # )
        ) #+
    
    
    #   theme(legend.key.size = unit(2, 'cm' ))#+
    # theme(axis.text.y = element_markdown())   # Used to make the axis labels red for DA groups
    
     ggsave(here("output",save.folder , paste0(tit, " Dashboard Estimates 2025 - ",Sys.Date()  ,".png")),
            width = 8, height = 5)
    # 
}

save.folder <- "grid"

dash.graph(all.sheets,"scesd")

