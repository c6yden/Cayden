# Cayden  / Lab 2 / May 2026

install.packages("tidycensus")
install.packages("tidyverse" )
library(tidycensus           )
library(tidyverse            )
library(tigris               )
library(scales               )
library(sf                   )
options(scipen = 999         )


# change these and nothing else
census_api_key("", install = TRUE, overwrite = TRUE)
pathName      = "/Users/cayden/Desktop/all_labs/images"
county        = "Lane County"
state         = "OR" 
year          = 2023


#-------------------------------------------------------------------------------
#Population pyramid | single county | age and sex


# produces a table of 5 year age groups populations by sex
ageSexFiltered <- get_estimates(
  geography        = "county"             ,
  county           = county               ,
  state            = state                ,
  product          = "characteristics"    ,
  breakdown        = c("SEX", "AGEGROUP") ,
  breakdown_labels = TRUE                 ,
  year             = year                 )                 |> 
  filter(str_detect(AGEGROUP, "^Age"), SEX != "Both sexes") |>
  mutate(value = ifelse(SEX == "Male", -value, value        ))


# collects max value for the pop pyramid
max_val <- (max(abs(ageSexFiltered$value))) * 1.1


# produces the pyramid
pyramid <- 
  ggplot(ageSexFiltered,aes(x = value, y = AGEGROUP, fill = SEX))      +
  geom_col          (width = 0.8, alpha = 0.95)                        +
  scale_y_discrete  (labels = ~ str_remove_all(.x, "Age\\s|\\syears")) +
  scale_fill_manual (values = c("red", "blue"))                        +
  theme_minimal     (base_family = "Verdana", base_size = 12)          +             
  scale_x_continuous(limits = c(-max_val, max_val), labels = abs)      +
  
  theme( 
    legend.position    = "none",
    panel.grid.minor.y = element_blank(               ),
    panel.grid.minor.x = element_blank(               ),
    panel.grid.major.y = element_blank(               ),
    plot.background    = element_rect (fill  = "grey" ),
    panel.background   = element_rect (fill  = "grey" ),            
    legend.background  = element_rect (fill  = "grey" ),
    legend.key         = element_rect (fill  = "grey" ),
    legend.text        = element_text (color = "black"),
    legend.title       = element_text (color = "black"),
    axis.text          = element_text (color = "black"),
    axis.title         = element_text (color = "black"),
    axis.ticks         = element_line (color = "black"),
    plot.title         = element_text (color = "black"),
    plot.caption       = element_text (color = "black"),
    plot.subtitle      = element_text (color = "black"),
    panel.grid.major.x = element_line (color = "black"))+
  
  labs(
    fill     = "",
    y        = "",
    x        = "Count of people",
    title    = paste ("Population structure in", county, year            ),
    subtitle = paste ("Census Bureau population estimate by age and sex" ),
    caption  = "Sources: US Census Bureau PEP / tidycensus R package / Cayden / Walker 2023") 

pyramid


# save it!
ggsave(
  filename = paste0(county, "_pyramid.png"),
  plot     = pyramid, path   = pathName    ,
  width    = 8      , height = 5           ,
  units    = "in"   , dpi    = 300         ) 


#-------------------------------------------------------------------------------
#Population margin of error | all counties in selected state 


# collect the data and keep only the top 10 counties
swedCount <- get_acs(
  year      = year                              ,
  state     = state                             ,
  survey    = "acs5"                            ,
  geography = "county"                          ,
  variables = "B04006_089"                    )|>
  mutate(NAME = str_remove(NAME, " County,.*"))|>
  slice_max(order_by = estimate, n = 10)


# make the plot
popMOE <- 
  ggplot(swedCount, aes(x = estimate, y = reorder(NAME, estimate)))   +
    geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +                  
    geom_point(size = 3, color = "black")                             +
                  
  theme( 
    plot.background    = element_rect (fill  = "grey" ),
    panel.background   = element_rect (fill  = "grey" ),            
    legend.background  = element_rect (fill  = "grey" ),
    legend.key         = element_rect (fill  = "grey" ))+

    labs(
      x        = "",
      y        = "",
      title    = paste ("Count of Swedish People in", state),
      subtitle = "Visualizing margin of error, top 10 counties",
      caption  = "Sources: ACS / tidycensus R package / Cayden / Walker 2023") 
popMOE


# save it!
ggsave(
  filename = paste0(county, "_popMOE.png"),
  plot     = popMOE, path   = pathName    ,
  width    = 8     , height = 5           ,
  units    = "in"  , dpi    = 300         ) 
 




#-------------------------------------------------------------------------------
# counties with highest and lowest percent of pop with a masters degree


# collect the data
masters <- get_acs(
  year      = year                           ,
  state     = state                          ,
  survey    = "acs5"                         ,
  geography = "county"                       ,
  variables = "B15003_023"                   ) |>
  mutate(NAME = str_remove(NAME, " County,.*"))|>
  select(NAME, masters = estimate)


# collect the data
population <- get_acs(
  year      = year                           ,
  state     = state                          ,
  survey    = "acs5"                         ,
  geography = "county"                       ,
  variables = "B01003_001"                   ) |> 
  mutate(NAME = str_remove(NAME, " County,.*"))|>
  select(NAME, population = estimate)


# add the percentage column
mastersPercent <- masters                        |>
  inner_join(population, by = "NAME")            |>
  mutate(masters_percent = masters / population) |>
  select(NAME, masters_percent)                 


# collect the unweighted average of all counties
average <- round( mean(mastersPercent$masters_percent, na.rm = TRUE) * 100)


# collect the top 10 counties
mastersTop <- mastersPercent |>
  slice_max(order_by = masters_percent, n = 5)


# collect the bottom 5 counties
mastersBot <- mastersPercent |>
  slice_min(order_by = masters_percent, n = 5)


# make the graph
mastersTopGraph <-
  ggplot(mastersTop, aes(x = masters_percent, y = reorder(NAME, masters_percent))) +
  geom_point(size = 3, color = "black")                                            +
  theme_minimal(base_size = 12.5)                                                  +
  scale_x_continuous(labels = label_percent())                                     +
  
  theme( 
    plot.background    = element_rect (fill  = "grey" ),
    panel.background   = element_rect (fill  = "grey" ),            
    legend.background  = element_rect (fill  = "grey" ),
    legend.key         = element_rect (fill  = "grey" ))+

  labs(
    x        = ""                                       ,
    y        = ""                                       ,
    title    = paste ("Percent of People With a Masters Degree in", state),
    subtitle = paste0("State-county average (unweighted): ", average, "%\n", "Top 5 counties"),
    caption  = "Sources: ACS / tidycensus R package / Cayden  / Walker 2023") 
mastersTopGraph

# save it!
ggsave(
  filename = paste0(county, "_mastersTopGraph.png"),
  plot     = mastersTopGraph, path   = pathName    ,
  width    = 8              , height = 5           ,
  units    = "in"           , dpi    = 300         ) 

#----------------------------


# make the graph
mastersBotGraph <-
  ggplot(mastersBot, aes(x = masters_percent, y = reorder(NAME, masters_percent))) +
  geom_point(size = 3, color = "black")                                            +
  theme_minimal(base_size = 12.5)                                                  +
  scale_x_continuous(labels = label_percent())                                     +
  
  theme( 
    plot.background    = element_rect (fill  = "grey" ),
    panel.background   = element_rect (fill  = "grey" ),            
    legend.background  = element_rect (fill  = "grey" ),
    legend.key         = element_rect (fill  = "grey" ))+

  labs(
    x        = ""                                       ,
    y        = ""                                       ,
    title    = paste ("Percent of People With a Masters Degree in", state),
    subtitle = paste0("State-county average (unweighted): ", average, "%\n", "Bottom 5 counties"),
    caption  = "Sources: ACS / tidycensus R package / Cayden / Walker 2023") 
mastersBotGraph

# save it!
ggsave(
  filename = paste0(county, "_mastersBotGraph.png"),
  plot     = mastersBotGraph, path   = pathName    ,
  width    = 8              , height = 5           ,
  units    = "in"           , dpi    = 300         ) 






