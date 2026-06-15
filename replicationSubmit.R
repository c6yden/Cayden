
#necessary packages
install.packages("tidycensus")
install.packages("tidyverse" )
library(tidycensus           )
library(tidyverse            )
library(tigris               )
library(scales               )
library(sf                   )
options(scipen = 999         )


# change these and nothing else
census_api_key(".........", install = TRUE)

state         = "CA" 
county        = "San Diego"
year          = 2023
pathName      = "/User/......"


# ------------------------------------------------------------------------------
# makes a bar chart of race proportion



# collect total population for proportions later
totalPop   <- get_estimates(
  geography = "county"     ,
  state     = state        ,       
  county    = county       ,
  variables = "POPESTIMATE",
  vintage   = year       )|>
  dplyr     ::pull   (value)

# produced a graph of race by population
options(tigris_use_cache = TRUE)

# save race variables as a list
race_vars <- c(
  White    = "B03002_003",
  Black    = "B03002_004",
  Native   = "B03002_005",
  Asian    = "B03002_006",
  HIPI     = "B03002_007",
  Hispanic = "B03002_012"
)

# produces a summarized table of each race group with its population
race_totals <- get_acs(
  geography   = "county"     ,
  state       = state        ,
  county      = county       ,
  variables   = race_vars    ,
  summary_var = "B03002_001" ,
  year        = year         ) |>
  group_by (variable)          |>
  summarise(population = sum(estimate)) 

#makes the graph
race_graph <- race_totals %>%
  ggplot(aes(y = reorder(variable, population), x = population, fill = variable)) +
  geom_col(width = 0.80, alpha = 0.97, fill = "grey")                             +
  scale_x_continuous(labels = scales::label_comma())                              +
  
  theme(
    legend.position    = "none"                       ,
    panel.grid.major.y = element_blank()              ,
    panel.grid.minor.y = element_blank()              ,
    panel.grid.minor.x = element_blank()              ,
    panel.grid.major.x = element_line(color = "grey" ),
    plot.background    = element_rect(fill  = "black"),
    panel.background   = element_rect(fill  = "black"),
    text               = element_text(color = "white"),
    axis.text          = element_text(color = "white"),
    axis.title         = element_text(color = "white"),
    plot.title         = element_text(color = "white"),
    plot.caption       = element_text(color = "white"))+
  
  labs(
    y        = ""            ,
    x        = "ACS estimate",
    title    = paste(county, "Race Share"),
    subtitle = paste(year,  "ACS estimates | Total population: ", totalPop),
    caption  = "Source: ACS Data Profile / PEP / tidycensus R package / Cayden Doherty / Walker 2023"
)
race_graph

#save it!
ggsave(
  filename = paste0(county, "_race_graph.png"),
  plot     = race_graph   , path   = pathName ,
  width    = 8            , height = 5        ,
  units    = "in"         , dpi    = 300
) 



# ------------------------------------------------------------------------------
# makes an age pyramid by sex



# produces a table of 5 year age groups populations by sex
ageSexFiltered <- get_estimates(
  geography        = "county"             ,
  county           = county               ,
  state            = state                ,
  product          = "characteristics"    ,
  breakdown        = c("SEX", "AGEGROUP") ,
  breakdown_labels = TRUE                 ,
  year             = year
  ) |> filter(str_detect(AGEGROUP, "^Age"),
  SEX != "Both sexes") |>
  mutate(value = ifelse(SEX == "Male", -value, value))

# find limit for graph here, highest value + buffer
# not in use currently 
max_val <- (max(abs(ageSexFiltered$value))) * 1.25

# produces the pyramid
pyramid <- 
  ggplot(ageSexFiltered,aes(x = value, y = AGEGROUP, fill = SEX))     +
  geom_col         (width = 0.8, alpha = 0.95)                        +
  scale_y_discrete (labels = ~ str_remove_all(.x, "Age\\s|\\syears")) +
  scale_fill_manual(values = c("red", "blue"))                        +
  theme_minimal    (base_family = "Verdana", base_size = 12)          +             
  
  theme(
    legend.position    = "none",
    plot.background    = element_rect(fill = "black") ,
    panel.background   = element_rect(fill = "black") ,            
    panel.grid.minor.y = element_blank()              ,
    panel.grid.minor.x = element_blank()              ,
    panel.grid.major.y = element_blank()              ,
    legend.background  = element_rect(fill  = "black"),
    legend.key         = element_rect(fill  = "black"),
    legend.text        = element_text(color = "white"),
    legend.title       = element_text(color = "white"),
    axis.text          = element_text(color = "white"),
    axis.title         = element_text(color = "white"),
    axis.ticks         = element_line(color = "white"),
    plot.title         = element_text(color = "white"),
    plot.caption       = element_text(color = "white"),
    plot.subtitle      = element_text(color = "white"))+
  
  labs(
    x        = "",
    y        = paste(year, "Census Bureau population estimate"),
    title    = paste("Population structure in", county),
    subtitle = paste("Count of people, by age group and sex"),
    fill     = "",
    caption  = "Sources: US Census Bureau PEP / tidycensus R package / Cayden Doherty / Walker 2023"
) 
pyramid

# save it!
ggsave(
  filename = paste0(county, "_pyramid.png"),
  plot     = pyramid, path   = pathName    ,
  width    = 8      , height = 5           ,
  units    = "in"   , dpi    = 300
) 



# ------------------------------------------------------------------------------
# plots a map of tracts by density



# makes a table of tracts and adds a column for population density
tractPopDensity <-   get_acs (
  geography   = "tract"      ,
  state       = state        ,
  county      = county       ,
  variables   = "B03002_001" ,
  year        = year         , 
  geometry    = TRUE)        |>
  st_transform(5070)         |> 
  mutate(pop_density = estimate / (as.numeric(st_area(geometry)) / 1e6))

# makes the plot and uses a square root normalization 
densityMap <- tractPopDensity %>%
  ggplot ()                                                           +
  scale_fill_viridis_c (option = "turbo", trans = "sqrt")             +
  geom_sf (aes (fill = pop_density), color = "grey", linewidth = .05) +
  
  theme (
    axis.text         = element_blank(),
    axis.ticks        = element_blank(),
    panel.grid        = element_blank(),
    plot.background   = element_rect(fill  = "black"),
    panel.background  = element_rect(fill  = "black"),
    text              = element_text(color = "white"),
    legend.background = element_rect(fill  = "black"),
    legend.key        = element_rect(fill  = "black"),
    legend.text       = element_text(color = "white"),
    legend.title      = element_text(color = "white"),
    plot.caption      = element_text(hjust = -2)   )+

  labs (
    fill     = "Population Density",
    title    = paste(county, "Population Density"),
    subtitle = "People per square km",
    caption  = "Source: ACS Data Profile / tidycensus R package / Cayden Doherty / Walker 2023"
)
densityMap

# save it!
ggsave(
  filename = paste0(county, "_densityMap.png"),
  path     = pathName, plot   = densityMap    ,
  width    = 8       , height = 5             ,
  units    = "in"    , dpi    = 300
) 



# ------------------------------------------------------------------------------
# sorts tracts into density groups seen in variables below. plots on map



# upper bounds of each, urban high is anything above 4500/km^2, 4 category
exurban      <-  550  # 0 category
subUrbanLow  <- 1000  # 1 category
subUrbanHigh <- 1900  # 2 category
urbanLow     <- 4500  # 3 category

# adds a new column for density category
reclassify <- tractPopDensity     |>
  filter (!is.na(pop_density))    |>
  arrange(desc  (pop_density))    |> 
  mutate (class = case_when       (
    pop_density < exurban       ~ 0,
    pop_density < subUrbanLow   ~ 1,
    pop_density < subUrbanHigh  ~ 2,
    pop_density < urbanLow      ~ 3,
    pop_density > urbanLow      ~ 4)
    )                             |>
  mutate( class = factor(class, levels = c(0,1,2,3,4)))

#makes the map, uses that new category for coloring
reclassMap <- reclassify |>
  ggplot()                                                   +
  geom_sf(aes(fill = class), color ="grey", linewidth = .05) +
  
  scale_fill_manual(
    values = c(
      "0" = "blue"  , "1" = "green" ,
      "2" = "yellow", "3" = "orange",
      "4" = "red")  ,
    labels = c(
      "Exurban    <   550" , "Sub. low    < 1000" ,
      "Sub. high   < 1900" , "Urban low  < 4500"  ,
      "Urban high > 4500"))+
  
  theme (
    axis.text         = element_blank(),
    axis.ticks        = element_blank(),
    panel.grid        = element_blank(),
    
    plot.background   = element_rect(fill  = "black"),
    panel.background  = element_rect(fill  = "black"),
    text              = element_text(color = "white"),
    
    legend.background = element_rect(fill  = "black"),
    legend.key        = element_rect(fill  = "black"),
    legend.text       = element_text(color = "white"),
    legend.title      = element_text(color = "white")) +
  
  labs(
    fill     = "Catagory",
    title    = paste(county, "Population Density"),
    subtitle = "Tracts sorted by Hanberry urban classifications",
    caption  = "Source: ACS Data Profile / tidycensus R package / Cayden Doherty / Walker 2023"
)
reclassMap

# save it!
ggsave(
  filename = paste0(county, "_reclassMap.png"),
  plot     = reclassMap, path   = pathName    ,
  width    = 8         , height = 5           ,
  units    = "in"      , dpi    = 300
) 



# ------------------------------------------------------------------------------
# shows the quantity of tracts within each class 



countTable <- reclassify |>
  group_by(class)        |>
  summarise(total_estimate = sum(estimate, na.rm = TRUE), n = n()) |>
  mutate (class = case_when   (
    class == 0  ~ "Exurban"   ,
    class == 1  ~ "Sub. Low"  ,
    class == 2  ~ "Sub. High" ,
    class == 3  ~ "Urban Low" ,
    class == 4  ~ "Urban High")
  )   

countGraph <- countTable %>%
  ggplot(aes(y = class, x = n, fill = class)) +
  geom_col(width = 0.80, alpha = 0.97, fill = "grey")     +
  
  theme(
    legend.position    = "none"                       ,
    panel.grid.major.y = element_blank()              ,
    panel.grid.minor.y = element_blank()              ,
    panel.grid.minor.x = element_blank()              ,
    panel.grid.major.x = element_line(color = "grey" ),
    plot.background    = element_rect(fill  = "black"),
    panel.background   = element_rect(fill  = "black"),
    text               = element_text(color = "white"),
    axis.text          = element_text(color = "white"),
    axis.title         = element_text(color = "white"),
    plot.title         = element_text(color = "white"),
    plot.caption       = element_text(color = "white"))+
  
  labs(
    y        = ""            ,
    x        = "Tract count",
    title    = paste(county, "Tracts by Urban Classification"),
    caption  = "Source: ACS Data Profile / tidycensus R package / Cayden Doherty / Walker 2023"
  )
countGraph

#save it!
ggsave(
  filename = paste0(county, "_tractCountgraph.png"),
  plot     = countGraph   , path   = pathName ,
  width    = 8            , height = 5        ,
  units    = "in"         , dpi    = 300
) 



# ------------------------------------------------------------------------------
# population by tracts




tractPopGraph <- countTable |>
  ggplot(aes(y = class, x = total_estimate, fill = class)) +
  geom_col(width = 0.80, alpha = 0.97, fill = "grey")      +
  
  theme(
    legend.position    = "none"                       ,
    panel.grid.major.y = element_blank()              ,
    panel.grid.minor.y = element_blank()              ,
    panel.grid.minor.x = element_blank()              ,
    panel.grid.major.x = element_line(color = "grey" ),
    plot.background    = element_rect(fill  = "black"),
    panel.background   = element_rect(fill  = "black"),
    text               = element_text(color = "white"),
    axis.text          = element_text(color = "white"),
    axis.title         = element_text(color = "white"),
    plot.title         = element_text(color = "white"),
    plot.caption       = element_text(color = "white"))+
  
  labs(
    y        = ""            ,
    x        = "Person count",
    title    = paste(county, "Population by Urban Classification"),
    caption  = "Source: ACS Data Profile / tidycensus R package / Cayden Doherty / Walker 2023"
  )
tractPopGraph

#save it!
ggsave(
  filename = paste0(county, "_tractPopgraph.png"),
  plot     = tractPopGraph, path   = pathName ,
  width    = 8            , height = 5        ,
  units    = "in"         , dpi    = 300
) 



# ------------------------------------------------------------------------------
# Pop. pyramid of people only within urban tracts



# collect tracts
urbanTracts <- reclassify       |>
  filter(class %in% c(3, 4))    |>
  st_drop_geometry()            |> 
  select(GEOID)                 
  
# collect full tracts age sex data
sexTractClass <- get_acs(
  geography   = "tract"      ,
  state       = state        ,
  county      = county       ,
  table       = "B01001"     ,
  year        = year         ) 

age_labels <- c(
  "Under 5"       ,
  "5–9"  ,"10–14" ,
  "15–17","18–19" ,
  "20"   ,"21"    ,
  "22–24","25–29" ,
  "30–34","35–39" ,
  "40–44", "45–49",
  "50–54", "55–59",
  "60–61", "62–64",
  "65–66", "67–69",
  "70–74", "75–79",
  "80–84", "85+"
)

# list of variables with summed populations for urban areas
urbanTracts <- sexTractClass                                       |>
  inner_join(urbanTracts, by = "GEOID")                            |>                         
  group_by(variable)                                               |>
  summarise(total = sum(estimate), .groups = "drop")               |>
  filter(!variable %in% c("B01001_001","B01001_002","B01001_026")) |>
  mutate(total = if_else(row_number() <= 23, -total, total))       |>
  mutate(sex   = if_else(row_number() <= 23, "Male", "female"))    |>
  mutate(age_group = rep(age_labels, times = 2))
 
ageSexUrbanPyramid <- ggplot(urbanTracts, aes(x = age_group, y = total, fill = sex)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = c("red", "blue"))+

  theme(
    legend.position    = "none",
    plot.background    = element_rect(fill = "black") ,
    panel.background   = element_rect(fill = "black") ,            
    panel.grid.minor.y = element_blank()              ,
    panel.grid.minor.x = element_blank()              ,
    panel.grid.major.y = element_blank()              ,
    legend.background  = element_rect(fill  = "black"),
    legend.key         = element_rect(fill  = "black"),
    legend.text        = element_text(color = "white"),
    legend.title       = element_text(color = "white"),
    axis.text          = element_text(color = "white"),
    axis.title         = element_text(color = "white"),
    axis.ticks         = element_line(color = "white"),
    plot.title         = element_text(color = "white"),
    plot.caption       = element_text(color = "white"),
    plot.subtitle      = element_text(color = "white"))+
    
    labs(
      x        = "",
      y        = paste(year, "Census Bureau population estimate"),
      title    = paste("Population structure in", county),
      subtitle = paste("Count of people, by age group and sex, in urban Tracts"),
      fill     = "",
      caption  = "Sources: US Census Bureau PEP / tidycensus R package / Cayden Doherty / Walker 2023"
    ) 
ageSexUrbanPyramid

# save it!
ggsave(
  filename = paste0(county, "_ageSexUrbanPyramid.png"),
  plot     = ageSexUrbanPyramid, path   = pathName    ,
  width    = 8                 , height = 5           ,
  units    = "in"              , dpi    = 300
) 



# ------------------------------------------------------------------------------
# Pop. pyramid of people only within sub urban tracts





# collect tracts
subanTracts <- reclassify       |>
  filter(class %in% c(1, 2, 3)) |>
  st_drop_geometry()            |> 
  select(GEOID)                 

# collect full tracts age sex data
sexTractClass <- get_acs(
  geography   = "tract"      ,
  state       = state        ,
  county      = county       ,
  table       = "B01001"     ,
  year        = year         ) 

age_labels <- c(
  "Under 5"       ,
  "5–9"  ,"10–14" ,
  "15–17","18–19" ,
  "20"   ,"21"    ,
  "22–24","25–29" ,
  "30–34","35–39" ,
  "40–44", "45–49",
  "50–54", "55–59",
  "60–61", "62–64",
  "65–66", "67–69",
  "70–74", "75–79",
  "80–84", "85+"
)

# list of variables with summed populations for urban areas
suburbanTracts <- sexTractClass                                       |>
  inner_join(subanTracts, by = "GEOID")                            |>                         
  group_by(variable)                                               |>
  summarise(total = sum(estimate), .groups = "drop")               |>
  filter(!variable %in% c("B01001_001","B01001_002","B01001_026")) |>
  mutate(total = if_else(row_number() <= 23, -total, total))       |>
  mutate(sex   = if_else(row_number() <= 23, "Male", "female"))    |>
  mutate(age_group = rep(age_labels, times = 2))

ageSexsubUrbanPyramid <- ggplot(urbanTracts, aes(x = age_group, y = total, fill = sex)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = c("red", "blue"))+
  
  theme(
    legend.position    = "none",
    plot.background    = element_rect(fill = "black") ,
    panel.background   = element_rect(fill = "black") ,            
    panel.grid.minor.y = element_blank()              ,
    panel.grid.minor.x = element_blank()              ,
    panel.grid.major.y = element_blank()              ,
    legend.background  = element_rect(fill  = "black"),
    legend.key         = element_rect(fill  = "black"),
    legend.text        = element_text(color = "white"),
    legend.title       = element_text(color = "white"),
    axis.text          = element_text(color = "white"),
    axis.title         = element_text(color = "white"),
    axis.ticks         = element_line(color = "white"),
    plot.title         = element_text(color = "white"),
    plot.caption       = element_text(color = "white"),
    plot.subtitle      = element_text(color = "white"))+
  
  labs(
    x        = "",
    y        = paste(year, "Census Bureau population estimate"),
    title    = paste("Population structure in", county),
    subtitle = paste("Count of people, by age group and sex, in suburban Tracts"),
    fill     = "",
    caption  = "Sources: US Census Bureau PEP / tidycensus R package / Cayden Doherty / Walker 2023"
  ) 
ageSexsubUrbanPyramid

# save it!
ggsave(
  filename = paste0(county, "_ageSexsubUrbanPyramid.png"),
  plot     = ageSexsubUrbanPyramid, path   = pathName    ,
  width    = 8                 , height = 5           ,
  units    = "in"              , dpi    = 300
) 




# ------------------------------------------------------------------------------
# Race breakdown of urban tracts


raceTractClass <- get_acs(
  geography   = "tract"      ,
  state       = state        ,
  county      = county       ,
  variables   = race_vars    ,
  summary_var = "B03002_001" ,
  year        = year         )            |>
  select("GEOID", "variable", "estimate") |>
  st_drop_geometry(df)                    |>
  left_join(simpleClasses, by = "GEOID")

urbanRace <- raceTractClass          |>
  filter   (class %in% c(3, 4))      |>
  group_by (variable)                |>
  summarise(population = sum(estimate)) 

#makes the graph
urbanRaceGraph <- urbanRace %>%
  ggplot(aes(y = variable, x = population, fill = variable)) +
  geom_col(width = 0.80, alpha = 0.97, fill = "grey")        +
  scale_x_continuous(labels = scales::label_comma())         +
  
  theme(
    legend.position    = "none"                       ,
    panel.grid.major.y = element_blank()              ,
    panel.grid.minor.y = element_blank()              ,
    panel.grid.minor.x = element_blank()              ,
    panel.grid.major.x = element_line(color = "grey" ),
    plot.background    = element_rect(fill  = "black"),
    panel.background   = element_rect(fill  = "black"),
    text               = element_text(color = "white"),
    axis.text          = element_text(color = "white"),
    axis.title         = element_text(color = "white"),
    plot.title         = element_text(color = "white"),
    plot.caption       = element_text(color = "white"))+
  
  labs(
    y        = ""            ,
    x        = "ACS estimate",
    title    = paste(county, "Race Share in Urban Tracts"),
    caption  = "Source: ACS Data Profile  / tidycensus R package / Cayden Doherty / Walker 2023"
  )
urbanRaceGraph

ggsave(
  filename = paste0(county , "_urbanRaceGraph.png"),
  plot     = urbanRaceGraph, path   = pathName     ,
  width    = 8             , height = 5            ,
  units    = "in"          , dpi    = 300
) 



# ------------------------------------------------------------------------------
# Race breakdown of sub tracts



subUrbanRace <- raceTractClass       |>
  filter   (class %in% c(1, 2, 3))   |>
  group_by (variable)                |>
  summarise(population = sum(estimate)) 

subUrbanRaceGraph <- subUrbanRace %>%
  ggplot(aes(y = variable, x = population, fill = variable)) +
  geom_col(width = 0.80, alpha = 0.97, fill = "grey")        +
  scale_x_continuous(labels = scales::label_comma())         +
  
  theme(
    legend.position    = "none"                       ,
    panel.grid.major.y = element_blank()              ,
    panel.grid.minor.y = element_blank()              ,
    panel.grid.minor.x = element_blank()              ,
    panel.grid.major.x = element_line(color = "grey" ),
    plot.background    = element_rect(fill  = "black"),
    panel.background   = element_rect(fill  = "black"),
    text               = element_text(color = "white"),
    axis.text          = element_text(color = "white"),
    axis.title         = element_text(color = "white"),
    plot.title         = element_text(color = "white"),
    plot.caption       = element_text(color = "white"))+
  
  labs(
    y        = ""            ,
    x        = "ACS estimate",
    title    = paste(county, "Race Share in suburban Tracts"),
    caption  = "Source: ACS Data Profile  / tidycensus R package / Cayden Doherty / Walker 2023"
  )
subUrbanRaceGraph

ggsave(
  filename = paste0(county, "_subUrbanRaceGraph.png"),
  plot     = subUrbanRaceGraph, path   = pathName    ,
  width    = 8                , height = 5           ,
  units    = "in"             , dpi    = 300
) 



# ------------------------------------------------------------------------------
# Combine all graphs



install.packages("patchwork")
library(patchwork)

infoGraphic <- 
  (race_graph        | pyramid       ) /
  (densityMap        | reclassMap    ) /
  (tractPopGraph     |countGraph     ) /
  (subUrbanRaceGraph | urbanRaceGraph)
     
ggsave(
  filename = paste0(county, "_info.png")   ,
  plot     = infoGraphic, path   = pathName,
  width    = 16         , height = 15      ,   
  units    = "in"       , dpi    = 300
) 

 
       
