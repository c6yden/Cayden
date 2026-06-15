# Cayden  / Lab 5 / May 2026

install.packages("tidycensus")
install.packages("tidyverse" )
install.packages("spdep"     )
library(tidycensus           )
library(tidyverse            )
library(tigris               )
library(scales               )
library(spdep)
library(sf                   )
options(scipen = 999         )


# change these and nothing else
census_api_key("", install = TRUE, overwrite = TRUE)
pathName      = "/Users/cayden/Desktop/all_labs/images5"


#-------------------------------------------------------------------------------
# Split metropolitan area
statArea       = "Bristol"
state1         = "TN" 
state2         = "VA" 
year           = 2020
#collect all tracts within each state
bristolTracts <- map_dfr(c(state1, state2),~{
  tracts(.x, cb = TRUE, year = 2020)        })|>
  st_filter(metro, .predicate = st_within) # delete here for the whole states


# statistical area polygon
metro <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  filter(str_detect(NAME, statArea))


# plotting the two together
ggplot() +
  geom_sf(data = bristolTracts, aes(fill = STATEFP), color = "black")+
  geom_sf(data = metro, fill = NA, color = "red"                    )+
  scale_fill_manual(values = c("51" = "green", "47" = "orange" )    )+

  theme( 
    legend.position    = "none"                        ,
    plot.background    = element_rect (fill  = "grey" ),
    panel.background   = element_rect (fill  = "grey" ),            
  
    
    plot.title         = element_text (color = "black"),
    plot.caption       = element_text (color = "black"),
    plot.subtitle      = element_text (color = "black"),
  
    axis.text         = element_blank(                ),
    axis.title        = element_blank(                ),
    axis.ticks        = element_blank(                ),
    panel.grid.major  = element_blank(                ),
    panel.grid.minor  = element_blank(                ))+
  
  labs(
    fill     = "",
    y        = "",
    x        = "Count of people",
    title    = paste0(statArea, ", ", state1,", ", state2,", "  ),
    subtitle = paste ("Virginia in green (26 tracts) | Tennessee in Orange (53 tracts)" ), # tracts hand counted but can be automated
    caption  = "US Census Bureau / Cayden  / Walker 2023") 
  
  
#-------------------------------------------------------------------------------
# Erasing water

year   = 2020
state  = "MN"
place  = "Otter Tail"
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




ggplot(countyWater)                                      +
  geom_sf(aes(fill = estimate))                          +
  
  scale_fill_viridis_c(labels = scales::label_dollar())  +
  theme_void()                                           +
  
  scale_fill_distiller(palette = "RdYlGn", direction = 1)+
  
  theme(plot.background = element_rect (fill  = "grey" ))+
  
  labs(
    title   = "Otter Tail County Income"                 ,
    fill    = "Median household\nincome"                 ,
    caption = "Walker 2023, , ACS"                )


ggplot(county)                                           +
  geom_sf(data = countyWater, fill = "blue", color = NA) +
  geom_sf(aes(fill = estimate))                          +
  
  scale_fill_viridis_c(labels = scales::label_dollar())  +
  theme_void()                                           +
  
  scale_fill_distiller(palette = "RdYlGn", direction = 1)+
 
  theme(plot.background = element_rect (fill  = "grey" ))+
  
  labs(
    title   = "Otter Tail County Income With Lakes Filled",
    fill    = "Median household\nincome"                   ,
    caption = "Walker 2023, , ACS"                  )
  
  
  
#-------------------------------------------------------------------------------
# Getis-Ord hot spot analysis
  
  
# median age
sanDiegoTracts <- get_acs (
  county    = "San Diego" ,
  geography = "tract"     ,
  variables = "B06011PR_001",
  state     = "CA"        ,
  year      = 2020        ,
  geometry  = TRUE        )

acs_vars <- load_variables(2020, "acs5", cache = TRUE)





sanDiegoTracts <- st_buffer(sanDiegoTracts, 0)


neighbors <- poly2nb(sanDiegoTracts, queen = TRUE)
  
sd_coords <- sanDiegoTracts %>%
  st_centroid() %>%
  st_coordinates()

plot(sanDiegoTracts$geometry)
plot(neighbors,
     coords = sd_coords,
     add    = TRUE,
     col    = "blue",
     points = FALSE)
  
neighbors[[1]]
  
weights <- nb2listw(neighbors, style = "W")
weights$weights[[1]]
  
sanDiegoTracts$lag_estimate <- lag.listw(weights, sanDiegoTracts$estimate)

  
ggplot(sanDiegoTracts, aes(x = estimate, y = lag_estimate)) +
  geom_point(alpha = 0.3) +
  geom_abline(color = "red") +
  theme_minimal() +
  labs(
    title = "Median Income by Census tract, San Diego, CA.",
    x = "Median Income",
    y = "Spatial lag, median income",
    caption = "Data source: 2016-2020 ACS via the tidycensus R package. Walker 2023") 
  

moran.test(sanDiegoTracts$estimate, weights)
  
  
localg_weights <- nb2listw(include.self(neighbors))

sanDiegoTracts$localG <- localG(sanDiegoTracts$estimate, localg_weights)

sanDiegoTracts$localG <- as.numeric(localG(sanDiegoTracts$estimate, localg_weights))


ggplot(sanDiegoTracts) +
  geom_sf(aes(fill = localG), color = NA) +
  scale_fill_distiller(palette = "RdYlBu") +
  theme_void() +
  labs(fill = "Local Gi* statistic",
       title =  "San Diego, CA.",
       caption = "Data source: 2016-2020 ACS via the tidycensus R package. Walker 2023")
  
  
  
  
sanDiegoTracts <- sanDiegoTracts %>%
  mutate(hotspot = case_when(
    localG >= 2.56 ~ "High cluster",
    localG <= -2.56 ~ "Low cluster",
    TRUE ~ "Not significant"
  ))
  
  
  
ggplot(sanDiegoTracts) +
  geom_sf(aes(fill = hotspot), color = "grey90", size = 0.1) +
  scale_fill_manual(values = c("red", "blue", "grey")) +
  theme_void()+
  labs(
     title =  "San Diego, CA. Median Income Hotspots",
     caption = "Data source: 2016-2020 ACS via the tidycensus R package. Walker 2023")
  
  
  
  
  


sanDiegoTracts$scaled_estimate <- as.numeric(scale(sanDiegoTracts$estimate))
dfw_lisa <- localmoran_perm(
  sanDiegoTracts$scaled_estimate,
  weights,
  nsim = 999L,
  alternative = "two.sided"
) %>%
  as_tibble() %>%
  set_names(c("local_i", "exp_i", "var_i", "z_i", "p_i",
              "p_i_sim", "pi_sim_folded", "skewness", "kurtosis"))
dfw_lisa_df <- sanDiegoTracts %>%
  select(GEOID, scaled_estimate) %>%
  mutate(lagged_estimate = lag.listw(weights, scaled_estimate)) %>%
  bind_cols(dfw_lisa)
  

dfw_lisa_clusters <- dfw_lisa_df %>%
  mutate(lisa_cluster = case_when(
    p_i >= 0.05 ~ "Not significant",
    scaled_estimate > 0 & local_i > 0 ~ "High-high",
    scaled_estimate > 0 & local_i < 0 ~ "High-low",
    scaled_estimate < 0 & local_i > 0 ~ "Low-low",
    scaled_estimate < 0 & local_i < 0 ~ "Low-high"
  ))
  
  
  
  
  
color_values <- c(`High-high`
                  = "red",
                  `High-low`
                  = "pink",
                  `Low-low`
                  = "blue",
                  `Low-high`
                  = "lightblue",
                  `Not significant`
                  = "white")
ggplot(dfw_lisa_clusters, aes(x = scaled_estimate,
                              y = lagged_estimate,
                              fill = lisa_cluster)) +
  geom_point(color = "black", shape = 21, size = 2) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = color_values) +
  labs(x = "Median income (z-score)",
       y = "Spatial lag of median income (z-score)",
       fill = "Cluster type",
       title = "San Diego, CA.",
       caption = "Data source: 2016-2020 ACS via the tidycensus R package. Walker 2023")
  
  
  
  
ggplot(dfw_lisa_clusters, aes(fill = lisa_cluster)) +
  geom_sf(size = 0.1) +
  theme_void() +
  scale_fill_manual(values = color_values) +
  labs(fill = "Cluster type",
         title =  "San Diego, CA. Median Income Hotspots",
         caption = "Data source: 2016-2020 ACS via the tidycensus R package. Walker 2023")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  