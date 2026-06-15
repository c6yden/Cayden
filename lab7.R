# / Lab 7 / May 2026

install.packages("tidycensus"  )
install.packages("tidyverse"   )
install.packages("segregation" )
library(segregation            )
library(tidycensus             )
library(tidyverse              )
library(tigris                 )
library(scales                 )
library(sf                     )
options(scipen = 999           )

census_api_key("", install = TRUE, overwrite = TRUE)




#-------------------------------------------------------------------------------
# Multigroup segregation index


ca_acs_data <- get_acs(
  geography  = "tract"      ,
  variables  = c( 
    white    = "B03002_003" ,
    black    = "B03002_004" ,
    asian    = "B03002_006" ,
    hispanic = "B03002_012"),
  state      = "CA"         ,
  geometry   = TRUE         ,
  year       = 2019         )

us_urban_areas <- get_acs(
  geography = "urban area",
  variables = "B01001_001",
  geometry  = TRUE        ,
  year      = 2019        ,
  survey    = "acs1"      )  %>%
  filter(estimate >= 750000) %>%
  transmute(urban_name = str_remove(NAME, fixed(", CA Urbanized Area (2010)")))

ca_urban_data <- ca_acs_data            %>%
  st_join(us_urban_areas, left = FALSE) %>%
  select(-NAME)                         %>%
  st_drop_geometry()

ca_urban_data                                  %>%
  filter(variable %in% c("white", "hispanic")) %>%
  group_by(urban_name)                         %>%
  group_modify(~dissimilarity(.x,
     group  = "variable",
     unit   = "GEOID"   ,  
     weight = "estimate"))                     %>%
  arrange(desc(est))

mutual_within(
  data   = ca_urban_data,
  group  = "variable"   ,
  unit   = "GEOID"      ,
  weight = "estimate"   ,
  within = "urban_name" ,
  wide   = TRUE         )

la_local_seg <- ca_urban_data       %>%
  filter(urban_name == "San Diego") %>%
  mutual_local(
    group  = "variable",
    unit   = "GEOID"   ,
    weight = "estimate",
    wide   = TRUE      )

la_tracts_seg <- tracts("CA", cb = TRUE, year = 2019) %>%
  inner_join(la_local_seg, by = "GEOID")

install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()

la_tracts_seg %>%
  ggplot  (aes(fill = ls))                +
  geom_sf (color = NA)                    + 
  coord_sf(crs = 26946)                   + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1)+
  theme_void()                            +
  theme( 
    plot.margin        = margin(.05, .05, .05, .05)    ,
    plot.background    = element_rect (fill  = "grey" ),
    panel.background   = element_rect (fill  = "grey" ),            
    legend.background  = element_rect (fill  = "grey" ),
    legend.key         = element_rect (fill  = "grey" ),
    legend.text        = element_text (color = "black"),
    legend.title       = element_text (color = "black"),
    
    plot.title         = element_text (color = "black"),
    plot.caption       = element_text (color = "black"),
    plot.subtitle      = element_text (color = "black"))+
  
  
  labs(
    title    = "San Diego Urban Area Segregation"  ,
    fill     = "Local\nsegregation index"          ,
    caption  = "ACS / Cayden  / Walker 2023")




#-------------------------------------------------------------------------------
# Location quotient

insal

install.packages("rmapshaper")
install.packages("flextable")
library(rmapshaper)
library(flextable)
install.packages("tmap")
library(tmap)


ca.tracts <- get_acs(
  geography = "tract",
  year      = 2023,
  variables = c(
    tpop    = "B03002_001",
    white   = "B03002_003", 
    black   = "B03002_004",
    asian   = "B03002_006", 
    hisp    = "B03002_012"),
  state     = "CA",
  survey    = "acs5",
  output    = "wide",
  geometry  = TRUE)


ca.tracts <- ca.tracts               %>%
  mutate(pwhite = 100*(whiteE/tpopE),
         pasian = 100*(asianE/tpopE),
         pblack = 100*(blackE/tpopE),
         phisp  = 100*(hispE/tpopE)) %>%
  
  select(GEOID,tpopE, pwhite, pasian, pblack, phisp,
         whiteE, asianE, blackE, hispE)



pl <- places(state = "CA", year = 2023, cb = TRUE)


sanDiegoCity <- pl %>%
  filter(NAME == "San Diego")
         
#Clip tracts in large cities
sanDiegoTracts <- ms_clip(target = ca.tracts,
                        clip = sanDiegoCity,
                        remove_slivers = TRUE)


sumCounts <- sanDiegoTracts %>%
  summarise(
    whiteSUM = sum(whiteE, na.rm = TRUE),
    asianSUM = sum(asianE, na.rm = TRUE),
    blackSUM = sum(blackE, na.rm = TRUE),
    hispSUM  = sum(hispE,  na.rm = TRUE),
    tpopSUM  = sum(tpopE,  na.rm = TRUE)
  )




sanDiegoTracts <- sanDiegoTracts %>%
  mutate(hisplq = (hispE/tpopE)/(438000/1480000))


sanDiegoTracts %>%
  ggplot() +
  geom_histogram(mapping = aes(x=hisplq), na.rm=TRUE) +
  xlab("Hispanic Location Quotient")+
  labs(
    title =  "San Diego"
  )


tm_shape(sanDiegoTracts, unit = "mi") +
  tm_polygons(fill = "hisplq",
              fill.scale = tm_scale(style = "quantile",
                                    values = "reds"),
              fill.legend = tm_legend(title = "San Diego Hispanic Location Quotient"))









































































































































































































































































































































































































































































































































































































































































































































































































































































































































