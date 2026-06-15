
# nolint start


###########
############
census_api_key("", install = TRUE, overwrite = TRUE)
############
###########



install.packages("tidycensus")
install.packages("tidyverse" )
install.packages("tmap"      )
library(tidycensus           )
library(tidyverse            )
library(tigris               )
library(scales               )
library(sf                   )
library(tmap                 )
install.packages("spdep"     )
library(spdep)





state         = "CA"
county        = "San Diego"
year          = 2023


options(tigris_use_cache = TRUE)
options(scipen           = 999 )


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
  geography   = "county"             ,
  state       = state                ,
  county      = county               ,
  variables   = race_vars            ,
  summary_var = "B03002_001"         ,
  year        = year                )|>
  group_by (variable)                |>
  summarise(population = sum(estimate)
  ) 


#makes the graph
figure1 <- race_totals |>
  ggplot(aes(y = reorder(variable, population), x = population, fill = variable))+
  geom_col(width = 0.80, alpha = 0.97, fill = "grey")                          +
  scale_x_continuous(labels = scales::label_comma())                             +
  
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
    caption  = "Source: ACS Data Profile / PEP / tidycensus R package / Cayden / Walker 2023"
  )


# ------------------------------------------------------------------------------
# makes an age pyramid by sex



# produces a table of 5 year age groups populations by sex
ageSexFiltered <- get_estimates(
  geography        = "county",
  county           = county,
  state            = state,
  product          = "characteristics",
  breakdown        = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  vintage          = year
) |>
  filter(
    str_detect(AGEGROUP, "^Age"),
    SEX != "Both sexes"
  ) |>
  mutate(value = ifelse(SEX == "Male", -value, value))

# find limit for graph here, highest value + buffer
# not in use currently 
max_val <- (max(abs(ageSexFiltered$value))) * 1.25

# produces the pyramid
figure2 <- 
  ggplot(ageSexFiltered,aes(x = value, y = AGEGROUP, fill = SEX))    +
  geom_col         (width = 0.8, alpha = 0.95)                       +
  scale_y_discrete (labels = ~ str_remove_all(.x, "Age\\s|\\syears"))+
  scale_fill_manual(values = c("red", "blue"))                   +
  theme_minimal    (base_family = "Verdana", base_size = 12)         +             
  
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
    fill     = "",
    title    = paste("Population structure in", county)        ,
    subtitle = paste("Count of people, by age group and sex")  ,
    y        = paste(year, "Census Bureau population estimate"),
    caption  = "Sources: US Census Bureau PEP / tidycensus R package / Cayden / Walker 2023"
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
figure3 <- tractPopDensity %>%
  ggplot ()                                                            +
  scale_fill_viridis_c (option = "turbo", trans = "sqrt")              +
  geom_sf (aes (fill = pop_density), color = "grey", linewidth = .05)+
  
  theme (
    axis.text         = element_blank()                ,
    axis.ticks        = element_blank()                ,
    panel.grid        = element_blank()                ,
    plot.background   = element_rect(fill  = "black"),
    panel.background  = element_rect(fill  = "black"),
    text              = element_text(color = "white"),
    legend.background = element_rect(fill  = "black"),
    legend.key        = element_rect(fill  = "black"),
    legend.text       = element_text(color = "white"),
    legend.title      = element_text(color = "white"),
    plot.caption      = element_text(hjust =      -2))+
  
  labs (
    fill     = "Population Density"               ,
    title    = paste(county, "Population Density"),
    subtitle = "People per square km"             ,
    caption  = "Source: ACS Data Profile / tidycensus R package / Cayden / Walker 2023"
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
  mutate(class = factor(class, levels = c(0,1,2,3,4))
  )

#makes the map, uses that new category for coloring
figure4 <- reclassify |>
  ggplot()                                                    +
  geom_sf(aes(fill = class), color ="grey", linewidth = .05)+
  
  scale_fill_manual(
    values = c(
      "0" = "blue"  , "1" = "green" ,
      "2" = "yellow", "3" = "orange",
      "4" = "red")                    ,
    labels = c(
      "Exurban    <   550" , "Sub. low    < 1000" ,
      "Sub. high   < 1900" , "Urban low  < 4500"  ,
      "Urban high > 4500"  )                      )+
  
  theme (
    axis.text         = element_blank()                ,
    axis.ticks        = element_blank()                ,
    panel.grid        = element_blank()                ,
    
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
    caption  = "Source: ACS Data Profile / tidycensus R package / Cayden / Walker 2023"
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

figure5 <- countTable %>%
  ggplot(aes(y = class, x = n, fill = class)) +
  geom_col(width = 0.80, alpha = 0.97, fill = "grey")     +
  
  theme(
    legend.position    = "none"                         ,
    panel.grid.major.y = element_blank()                ,
    panel.grid.minor.y = element_blank()                ,
    panel.grid.minor.x = element_blank()                ,
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
    caption  = "Source: ACS Data Profile / tidycensus R package / Cayden / Walker 2023"
  )


# ------------------------------------------------------------------------------
# population by tracts


figure6 <- countTable |>
  ggplot(aes(y = class, x = total_estimate, fill = class)) +
  geom_col(width = 0.80, alpha = 0.97, fill = "grey")      +
  
  theme(
    legend.position    = "none"                         ,
    panel.grid.major.y = element_blank()                ,
    panel.grid.minor.y = element_blank()                ,
    panel.grid.minor.x = element_blank()                ,
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
    caption  = "Source: ACS Data Profile / tidycensus R package / Cayden / Walker 2023"
  )


# ------------------------------------------------------------------------------




youngPeople <-  get_acs(
  variables = "B01001A_008",
  geography = "tract"      ,
  county    = county       ,
  state     = state        ,
  year      = year         ,
  geometry  = TRUE         )|>
  filter(!is.na(estimate)  )


# plot with embedded legend and red coloration
figure7 <- tm_shape(youngPeople)+
  tm_polygons()               +
  
  tm_bubbles(
    size       = "estimate",
    fill_alpha = 0.8       ,
    col        = "red"     ,
    title.size = paste(year, "ACS"))+
  
  tm_layout(
    title             = paste0("White Men Age 20-24 by tract in ", county,", ", state),
    inner.margins     = c(0.05, 0.05, 0.1, 0.05),
    title.position    = c("right", "top"       ),
    frame             = TRUE     ,
    legend.width      = 10       ,
    bg.color          = "grey70" ,
    text.fontfamily   = "Verdana")+
  
  tm_scalebar(breaks = c(0, 5, 10), position = c("left", "top")          )+
  tm_legend  (position = c("left", "bottom"), title = "Count", width = 10)+
  
  tm_credits(
    "Walker 2023, Cayden, ACS"  ,
    bg.color = "white"             ,
    position = c("right", "bottom")
    )


#------------------------------


income <- get_acs(
  variables = "B19013_001",
  geography = "tract"     ,
  county    = county      ,
  state     = state       ,
  year      = year        ,
  geometry  = TRUE        )|>
  filter(!is.na(estimate) )

# Make the plot using a red to green color scale and 10 bins






year   = 2020
state  = "CA"
place  = "San Diego"
var    = "B19013_001" # median household income
  

county <- get_acs(
  geography = "tract",
  variables = var    , 
  state     = state  ,
  county    = place  ,
  year      = year   ,
  geometry  = TRUE   )|>
  erase_water()


countyWater <- get_acs(
  geography = "tract",
  variables = var    , 
  state     = state  ,
  county    = place  ,
  year      = year   ,
  geometry  = TRUE   )



figure8 <- ggplot(county)                                           +
  geom_sf(data = countyWater, fill = "blue", color = NA) +
  geom_sf(aes(fill = estimate))                          +
  
  scale_fill_viridis_c(labels = scales::label_dollar())  +
  theme_void()                                           +
  
  scale_fill_distiller(palette = "RdYlGn", direction = 1)+
 
  theme(plot.background = element_rect (fill  = "grey" ))+
  
  labs(
    title   = "San Diego County Income With Water Filled",
    fill    = "Median household\nincome"                   ,
    caption = "Walker 2023, Cayden, ACS"                  )
  


pathName <- "figures"

ggsave(
  filename = "figure1.png",
  plot     = figure1      , path   = pathName,
  width    = 8            , height = 5       ,
  units    = "in"         , dpi    = 300      
) 

ggsave(
  filename = "figure2.png",
  plot     = figure2      , path   = pathName,
  width    = 8            , height = 5       ,
  units    = "in"         , dpi    = 300      
) 

ggsave(
  filename = "figure3.png",
  plot     = figure3      , path   = pathName,
  width    = 8            , height = 5       ,
  units    = "in"         , dpi    = 300      
) 

ggsave(
  filename = "figure4.png",
  plot     = figure4   , path   = pathName ,
  width    = 8            , height = 5        ,
  units    = "in"         , dpi    = 300      
) 





ggsave(
  filename = "figure6.png",
  plot     = figure6   , path   = pathName ,
  width    = 8            , height = 5        ,
  units    = "in"         , dpi    = 300      
) 




ggsave(
  filename = "figure8.png",
  plot     = figure8   , path   = pathName ,
  width    = 8            , height = 5        ,
  units    = "in"         , dpi    = 300      
) 











































# nolint endsimple