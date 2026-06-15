# Cayden  / Lab 3 / May 2026

install.packages("tidycensus")
install.packages("tidyverse" )
install.packages("tmap"      )
library(tidycensus           )
library(tidyverse            )
library(tigris               )
library(scales               )
library(tmap                 )
library(sf                   )
options(scipen = 999         )
options(tigris_use_cache = TRUE)


# change these and nothing else
census_api_key("", install = TRUE, overwrite = TRUE)
pathName      = "/Users/cayden/Desktop/all_labs/images3"
county        = "Santa Clara"
state         = "CA" 
year          = 2020


# ------------------------------------------------------------------------------
# Race dot density map | american Indian and Alaskan native

# concatenate race vars
raceVars  = c(
  Hispanic = "P2_002N",
  White    = "P2_005N",
  Asian    = "P2_008N")


# call for decennial census data
raceCounts <- get_decennial  (
  variables   = raceVars ,
  geography   = "tract"  ,
  county      = county   ,
  state       = state    ,
  year        = year     ,
  geometry    = TRUE     )


# create all dots with locations
raceDots <- raceCounts |>
  as_dot_density(
    value          = "value",
    values_per_dot = 300,
    group          = "variable"
  )


# make the plot
dotPlot <-tm_shape(filter(raceCounts, variable == "White"))+
  
  tm_polygons(col = "white", border.col = "grey")+
  tm_shape(raceDots)                             + 
  
  tm_layout(
    inner.margins     = c(0.1, 0.05, 0.2, 0.05)                          ,
    title.position    = c("right", "top")                                ,
    frame             = TRUE                                             ,
    bg.color          = "grey70"                                         ,
    text.fontfamily   = "Verdana"                                        )+
  
  tm_title   (paste0("Income by tract in ", county,", ", state, ", 1:300"))+
 
  
  tm_compass (position = c("left", "top"   )                             )+
  
  tm_legend  (
    position = c("left", "bottom") , 
    width    = 10                  ,
    size     = 1                   )+
  
  tm_dots(
    col     = "variable"          ,
    palette = "Set1"              ,
    size    = .05                 )+
  
  tm_credits(
    "Walker 2023, , Census bureau",
    bg.color = "white"                   ,
    position = c("right", "bottom"      ))
dotPlot




# ------------------------------------------------------------------------------
# Income chloroplast map  


# Collect income data by tract and remove empty values

income <- get_acs(
  variables = "B19013_001",
  geography = "tract"     ,
  county    = "San Diego"     ,
  state     = "CA"       ,
  year      = year        ,
  geometry  = TRUE        )|>
  filter(!is.na(estimate) )

# Make the plot using a red to green color scale and 10 bins
incomeMap <- tm_shape(income)+
  tm_polygons(
    fill.scale = tm_scale_intervals(),
    col         = "estimate"         ,
    title       = paste(year, "ACS") ,
    palette     = "RdYlGn"           ,
    colorNA     = "white"            ,
    n           = 10                 ,
    legend.hist = TRUE               )+
  
  tm_layout(
    title             = paste0("Income by tract in ", county,", ", state),
    inner.margins     = c(0.05, 0.05, 0.2, 0.05)                         ,
    title.position    = c("right", "top")                                ,
    frame             = TRUE     ,
    legend.outside    = TRUE     ,
    legend.hist.width = 5        ,
    bg.color          = "grey70" ,
    text.fontfamily   = "Verdana")+

  tm_compass (position = c("left", "top"   ))+
  tm_scalebar(breaks = c(0, 10, 20), position = c("left", "bottom"))+
    
  tm_credits(
    "Walker 2023, Cayden, ACS"  ,
    bg.color = "white"             ,
    position = c("right", "bottom"))
incomeMap


# ------------------------------------------------------------------------------
# Graduated symbol map | white men age 20-24


# use acs men age 20-24 data by tract
youngPeople <-  get_acs(
  variables = "B01001A_008",
  geography = "tract"      ,
  county    = county       ,
  state     = state        ,
  year      = year         ,
  geometry  = TRUE         )|>
  filter(!is.na(estimate)  )


# plot with embedded legend and red coloration
young <- tm_shape(youngPeople)+
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
    "Walker 2023, , ACS"  ,
    bg.color = "white"             ,
    position = c("right", "bottom"))
young































































































































































































































































































































































































































































































































































































































































































































































































































































































































  
  
  