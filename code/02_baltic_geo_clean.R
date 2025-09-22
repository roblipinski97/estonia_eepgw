
#
# SET-UP --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#

# rm(list=ls())

### Source the '00_global.R' script with required packages and functions
source(file.path(dirname( rstudioapi::getSourceEditorContext()$path), '00_baltic_global.R'))


### Re-set working directory (if necessary)
# main_dir = dirname(dirname(rstudioapi::getSourceEditorContext()$path))
# setwd(main_dir)

### make a copy of the file    ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
file.copy(rstudioapi::getSourceEditorContext()$path,
          gsub('code', 'code/00_archive', gsub('\\.R', ' - copy.R', rstudioapi::getSourceEditorContext()$path)),
          overwrite = T, copy.date = T)


# '----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# NOTES --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#

### > use provided lat-long AND/OR IP to add/check geo-location?

### read cleaned survey data  ------------------------------------------------------------------------------------------------------------------------------------

estonia = fread(file.path('data', 'clean', 'baltic_way_survey_no_geo.csv'))  # read raw .csv file downloaded from Qualtrics


# '----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1. Geo-variables ------------------------------------------------------------------------------------------------------------------------------------


#### 1.1. LG boundaries  ------------------------------------------------------------------------------------------------------------------------------------
estonia_lg_sf = read_sf(file.path(main_dir, 'data', 'spatial', 'estonia_lg_2023.shp')) %>% clean_names() %>% 
  rename(county = mnimi, lg = onimi) %>% 
  select(c(county,lg)) %>% 
  mutate(across(c(county, lg), ~tolower(.)))


# mapa(estonia_county_sf, 'county')

###### + population -----------------------------------------------------------------------------------------------------------------------------------------------
population <- fread(file.path(main_dir, 'data', 'raw', 'population_2016_2023.csv')) %>% 
  filter(year == 2023) %>% 
  select(lg, population) %>% 
  mutate(across(c(lg), ~tolower(.)))

estonia_lg_sf = left_join(estonia_lg_sf, population) %>% st_as_sf()

##### + county union ------------------------------------------------------------------------------------------------------------------------------------------
# estonia_county_sf <- estonia_lg_sf %>%
#   group_by(county) %>%  
#   summarise(geometry = st_union(geometry)) %>%
#   ungroup()

##### + country union ------------------------------------------------------------------------------------------------------------------------------------------
# estonia_country_sf <- estonia_lg_sf %>%
#   mutate(country = 'estonia') %>% 
#   group_by(country) %>%  
#   summarise(geometry = st_union(geometry)) %>%
#   ungroup()



### 1.2. locality (from post codes)  ------------------------------------------------------------------------------------------------------------------------------------
anew_postal = F

if(anew_postal == T | !file.exists(file.path('data', 'clean', 'estonia_postal.csv'))){ # takes a while to read the full Excel, so only do so if one doesn't exist yet
  estonia_postal_all = read.xlsx(file.path(main_dir, 'data', 'raw', 'postal_codes.xlsx')) %>% clean_names()
  
  estonia_postal = estonia_postal_all  %>% 
    select(4:6, 11:12) %>% distinct %>% 
    rename(county = 1, lg = 2, settlement = 3, lat = coordinate_x, lon = coordinate_y) %>%
    
    mutate(# if city, then it's good enough and proceed 
           settlement = ifelse(grepl('linn', lg), lg, settlement),
            
           # clean incorrectly read diacritics...
           across(where(is.character), ~gsub( "Ã¼", "ü",   .)),
           across(where(is.character), ~gsub( "Ã¤", "ä",  .)),
           across(where(is.character), ~gsub( "Ãµ", "õ",   .)),
           across(where(is.character), ~gsub( "Ã¶", "ö",   .)),
           across(where(is.character), ~gsub( "Ã¥", "å",   .)), # very rare (few instances) but still
           
           across(where(is.character), ~gsub( "Ãœ", "Ü",   .)),
           across(where(is.character), ~gsub( "Ã„", "Ä",   .)),
           across(where(is.character), ~gsub( "Ã•", "Õ",   .)),
           across(where(is.character), ~gsub( "Ã–", "Ö",   .)),
           
           across(everything(), ~trimws(.)), # remove empty spaces
           across(where(is.character), ~tolower(.)), # character to lowercase
           across(c(lat, lon), ~as.numeric(.)) # coordinates as numeric
          
    ) %>% 
    # coordinates are there per postal code - average them for at settlement level (take median as with mean 1 mislabelled post
    # code would skew the result?? -> doesn't really make a difference, as non-linn settlements have a single unique coordinate)
    fgroup_by(county, lg, settlement) %>% 
    fsummarise(lat = fmedian(lat), lon = fmedian(lon)
              # lat_n = fdistinct(lat), lon_n = fdistinct(lon)
              )
  
  # save to avoid re-reading the .xlsx
  write_flex(estonia_postal, file.path('data', 'clean', 'estonia_postal.csv'))
  
}else{
  estonia_postal = fread(file.path('data', 'clean', 'estonia_postal.csv'))
}







### 1.3. BW pathway -------------------------------------------------------------------------------------------------------------------------------------------------------------------------

### read pathway coordinates
baltic_way_points = jsonlite::fromJSON(file.path(main_dir, 'data', 'spatial', 'Baltic_way.json'), flatten=TRUE)
# baltic_way$jsondata$data$features$geometry.coordinates

baltic_way_points <- st_as_sf(x = data.frame(baltic_way_points$jsondata$data$features$geometry.coordinates),
                       coords = c("X1", "X2"),
                       crs = 4326)


### treat as line
baltic_way <- st_as_sf(baltic_way_points) %>% 
  summarise(geometry = st_combine(geometry)) %>% 
  st_cast("LINESTRING") 

### ensure common projection
baltic_way <- st_transform(baltic_way, 4326) # 3301 - should be in metres; 4326 - should be in degrees
estonia_lg_sf <- st_transform(estonia_lg_sf, 4326)

st_crs(baltic_way) = 4326
st_crs(estonia_lg_sf) = 4326

baltic_way  = st_make_valid(baltic_way)
estonia_lg_sf  = st_make_valid(estonia_lg_sf)

### leave only Estonian segment of the BW line
baltic_way <- st_intersection(baltic_way, estonia_lg_sf)


# '  --------------------------------------------------------------------------------------------------------------------------------------------------------
# 2. Distances --------------------------------------------------------------------------------------------------------------------------------------------------------
#

### linear distance ------------------------------------------------------------------------------------------------------------------------------------

#### LG-level
dist_matrix <- st_distance(estonia_lg_sf %>% st_centroid(), baltic_way)
estonia_lg_sf$dist_lg_linear = apply(dist_matrix, 1, min)/1e3

### *checks - visualize the pathway and the distance
# ggplot()+
#   geom_sf(data = estonia_lg_sf, aes(fill = dist_bw_lg))+
#   # geom_sf(data = baltic_way)+
#   geom_sf(data = estonia_lg_sf[estonia_lg_sf$lg %in% baltic_way$lg,], color = 'red', fill = NA) +
#   geom_sf(data = baltic_way, color = 'red', linewidth = 1.8) + 
#   map_theme


## Locality-level
estonia_postal <- st_as_sf(estonia_postal, coords = c("lat", "lon"), crs = 3301)
estonia_postal <- st_transform(estonia_postal, 4326)

dist_matrix <- st_distance(estonia_postal, baltic_way)
estonia_postal$dist_locality_linear = apply(dist_matrix, 1, min)/1e3

### *checks - visualize distance
# mapa(estonia_lg_sf, 'dist_bw_lg')
# 
# ggplot()+
#   geom_sf(data = estonia_postal, aes(fill = dist_bw_locality),
#           shape = 21) + 
#   geom_sf(data = baltic_way, color = 'red', linewidth = 1.8) + 
#   map_theme


### travel distance/duration ---------------------------------------------------------------------------------------------------------------------

### use osmdata and/or gmapdistance instead? xxx The distance seems ok here, but travel time off compared to Google Maps

### ensure common projection
baltic_way_points = st_transform(baltic_way_points, 4326) # 3301 - should be in metres; 4326 - should be in degrees
baltic_way_points = st_intersection(baltic_way_points, estonia_lg_sf)


#### LG-level  ---------------------------------------------------------------------------------------------------------------------
estonia_lg_centroid = estonia_lg_sf %>% st_centroid()

### xxx still not ideal? The nearest point is still linearly defined...
nearest_lg = st_nearest_feature(estonia_lg_centroid, baltic_way_points)
nearest_lg = baltic_way_points[nearest_lg, ]
nearest_lg$id <- seq_len(nrow(nearest_lg))

for(i in 1:nrow(nearest_lg)){
  
  rt1 = osrmRoute(
    src = nearest_lg[i,],
    dst = estonia_lg_centroid[i,])
  
  if(i==1){
    routes_lg = rt1
  }else{
    routes_lg = rbind(routes_lg, rt1)
  }
}

### assign to estonia_lg_sf data.frame
estonia_lg_sf$dist_lg_travel = routes_lg$distance
estonia_lg_sf$dist_lg_minuteute    = routes_lg$duration

# fine to exclude Ruhnu and Kihnu as no respondent chose those places

### *checks - visualize the pathway and the distances to it
g1 = ggplot()+
  geom_sf(data = estonia_lg_sf, fill = 'grey90', color = 'grey80', linewidth = .1)+
  # geom_sf(data = baltic_way)+
  # geom_sf(data = estonia_lg_sf[estonia_lg_sf$lg %in% baltic_way$lg,], color = 'red', fill = NA) +
  geom_sf(data = routes_lg %>% filter(duration != max(duration)), aes(color = duration),
          linewidth = .8) + 
  geom_sf(data = baltic_way, color = 'red', linewidth = 1.5) +
  geom_sf(data = estonia_lg_centroid, fill = 'grey60')+
  # scale_color_gradient(low = "red", high = "#ffffff")+
  
  scale_color_viridis(name = 'Travel duration (min)',  option = 'magma', direction=(-1))+
  # scale_color_gradient(name = 'Travel duration (min)', # 'share',
  #                      low = "darkblue", high = "#EC5F06") +
                       
  guides(color = guide_colorbar(title.position = 'top')) +
  labs(title = '<b>Municipality centroids</b>') +
  
  map_theme +
  theme(
    plot.title = element_markdown(size = 14),
    plot.caption = element_textbox_simple(size = 12),
    
    legend.title = element_markdown(size = 15, angle = 00),
    legend.text = element_text(face = 'plain', size = 14),
    legend.key.width = unit(1.9, 'cm'),
    legend.key.height = unit(.5, 'cm'),
    
  )



#### Locality-level  ---------------------------------------------------------------------------------------------------------------------

estonia_localities_sf <- st_as_sf(estonia_postal %>% 
                         filter(settlement %in% unique(c(
                           estonia$locality,
                           estonia$bw5_locality_you,
                           estonia$bw5_locality_parent
                         ))), 
                       coords = c('lat', 'lon'), crs=4326)

nearest_locality = st_nearest_feature(estonia_localities_sf, baltic_way_points)
nearest_locality = baltic_way_points[nearest_locality, ]
nearest_locality$id <- seq_len(nrow(nearest_locality))

for(i in 1:nrow(nearest_locality)){

  if(i %% 25 == 0){print(i)}
    
  rt1 = osrmRoute(
    src = nearest_locality[i,],
    dst = estonia_localities_sf[i,])
  
  if(i==1){
    routes_locality = rt1
  }else{
    routes_locality = rbind(routes_locality, rt1)
  }
}

### assign to estonia_lg_sf data.frame
estonia_localities_sf$dist_locality_travel = routes_locality$distance
estonia_localities_sf$dist_locality_minuteute    = routes_locality$duration

plot(estonia_localities_sf$dist_locality_linear, estonia_localities_sf$dist_lg_travel)


### *checks - visualize the pathway and the distances to it
g2 = ggplot()+
  geom_sf(data = estonia_lg_sf, fill = 'grey90', color = 'grey80', linewidth = .1)+
  # geom_sf(data = baltic_way)+
  # geom_sf(data = estonia_lg_sf[estonia_lg_sf$lg %in% baltic_way$lg,], color = 'red', fill = NA) +
  geom_sf(data = estonia_localities_sf, fill = 'grey60', size = 1)+
  geom_sf(data = routes_locality, aes(color = duration),
          linewidth = .65) + 
  geom_sf(data = baltic_way, color = 'red', linewidth = 1.5) +
  # scale_color_gradient(low = "red", high = "#ffffff")+
  
  scale_color_viridis(name = 'Travel duration (min)',  option = 'magma', direction=(-1))+
  # scale_color_gradient(name = 'Travel duration (min)', # 'share',
  #                      low = "darkblue", high = "#EC5F06") +
  
  guides(color = guide_colorbar(title.position = 'top')) +
  labs(title = '<b>Localities</b>') +
  
  map_theme +
  theme(
    plot.title = element_markdown(size = 14),
    plot.caption = element_textbox_simple(size = 12),
    
    legend.title = element_markdown(size = 15, angle = 00),
    legend.text = element_text(face = 'plain', size = 14),
    legend.key.width = unit(1.9, 'cm'),
    legend.key.height = unit(.5, 'cm'),
    
  )

### map | shortest routes ---------------------------------------------------------------------------------------------------------------------------------

g_all = g1 + g2 + plot_layout(guides = "collect")

ggsave(g_all, width = 23, height = 12, unit = 'cm',
       # file = file.path('figures', 'aggregate', 'Finnish TV watchers by referent.png'))
       file = file.path('figures', 'Travel routes LG and locality.png'))



### plot | distance correlations ---------------------------------------------------------------------------------------------------------------------------------
names(estonia_lg_sf)

g1 = ggplot(estonia_lg_sf %>% filter(!lg %in% c('ruhnu vald')), # no respondent from Ruhnu anyway
            aes(x = dist_lg_minuteute, y = dist_lg_travel))+
  geom_point(fill = 'grey', size = 4)+
  geom_smooth(method = 'lm', se = F, color = 'black', linetype = 'dashed')+
  labs(
    title = paste0('<b>Municipality centroids</b> (r=', round(cor(estonia_lg_sf$dist_lg_minuteute,
                                                                   estonia_lg_sf$dist_lg_travel), 2), ')'),
    x = 'Travel duration (minutes)',
    y = 'Travel distance (kilometres)'
  )   +
  theme(
    plot.title = element_markdown(size = 23),
  )


g2 = ggplot(estonia_lg_sf %>% filter(!lg %in% c('ruhnu vald')), # no respondent from Ruhnu anyway
             aes(x = dist_lg_minuteute, y = dist_lg_linear))+
  geom_point(fill = 'grey', size = 4)+
  geom_smooth(method = 'lm', se = F, color = 'black', linetype = 'dashed')+
  labs(
    title = paste0('<b>Municipality centroids</b> (r=', round(cor(estonia_lg_sf$dist_lg_minuteute,
                                                                   estonia_lg_sf$dist_lg_linear), 2), ')'),
    x = 'Travel duration (minutes)',
    y = 'Linear distance (kilometres)',
  ) +
  theme(
    plot.title = element_markdown(size = 23),
  )


g3 = ggplot(estonia_localities_sf,
            aes(x = dist_locality_minuteute, y = dist_locality_travel))+
  geom_point(fill = 'grey', size = 2.5)+
  geom_smooth(method = 'lm', se = F, color = 'black', linetype = 'dashed')+
  labs(
    title = paste0('<b>Localities</b> (r=', round(cor(estonia_localities_sf$dist_locality_minuteute,
                                                       estonia_localities_sf$dist_locality_travel), 2), ')'), 
    x = 'Travel duration (minutes)',
    y = 'Travel distance (kilometres)'
  )   +
  theme(
    plot.title = element_markdown(size = 23),
  )


g4 = ggplot(estonia_localities_sf,
            aes(x = dist_locality_minuteute, y = dist_locality_linear))+
  geom_point(fill = 'grey', size = 2.5)+
  geom_smooth(method = 'lm', se = F, color = 'black', linetype = 'dashed')+
  labs(
    title = paste0('<b>Localities</b> (r=', round(cor(estonia_localities_sf$dist_locality_minuteute,
                                                estonia_localities_sf$dist_locality_linear), 2), ')'),
    x = 'Travel duration (minutes)',
    y = 'Linear distance (kilometres)',
  ) +
  theme(
    plot.title = element_markdown(size = 23),
  )

g_all = g1+g2+g3+g4

ggsave(g_all, width = 40, height = 25, unit = 'cm',
       file = file.path('figures', 'Distance alternatives scaterplots.png'))




estonia_temp = estonia



  
### <> add to survey ----------------------------------------------------------------------------------------------------------------------------------------
estonia = estonia_temp


### treat NAs as string ('missing') for easier merging
estonia = estonia %>% mutate(across(c(county, lg, locality, matches('bw5_lg|bw5_locality')), ~ifelse(is.na(.), 'missing', .)))


#### LG ---------------------------------------------------------------------------------------------------------------------------------------
estonia = left_join(estonia, 
                    estonia_lg_sf %>% st_drop_geometry() %>% select(-c(county, population)) %>% 
                      rename_with(~ paste0("bw5_", ., "_you")))

estonia = left_join(estonia, 
                    estonia_lg_sf %>% st_drop_geometry() %>% select(-c(county, population)) %>% 
                      rename_with(~ paste0("bw5_", ., "_parent")))

### * checks - both equations should give the same distribution
sf(estonia$bw5_lg_you == 'missing') # 146
sf(is.na(estonia$bw5_dist_lg_linear_you)) # 146


#### Locality ---------------------------------------------------------------------------------------------------------------------------------------

estonia = left_join(
  estonia,
  estonia_localities_sf %>% st_drop_geometry() %>% select(-c(county)) %>% rename(locality = settlement) %>% 
    rename_with(~ paste0("bw5_", ., "_you")))

estonia = left_join(
  estonia,
  estonia_localities_sf %>% st_drop_geometry() %>% select(-c(county)) %>% rename(locality = settlement) %>% 
    rename_with(~ paste0("bw5_", ., "_parent")))


### * checks - both equations should give the same distribution
sf(estonia$bw5_locality_parent == 'missing') # 267
sf(is.na(estonia$bw5_dist_locality_minute_parent)) # 267



### replace NA locality with LG centroids -------------------------------------------------------------------------------------------------------------------------------------
table(estonia$bw5_locality_you == 'missing',
      estonia$bw5_lg_you == 'missing'
)

estonia = estonia %>% 
  mutate(
    across(c(matches('bw5_dist_locality')), ~ifelse(test = . == 'missing' | is.na(.),
                                                    yes  = estonia[[sub("locality", "lg", cur_column())]],
                                                    no   = .),
           .names = "{.col}_temp")   # new column name)
    )



### revert from 'missing' to NAs
estonia = estonia %>% mutate(across(c(county, lg, locality, matches('bw5_lg|bw5_locality')), 
                                    ~ifelse(. == 'missing', NA, .)))


# ' --------------------------------------------------------------------------------------------------------------------------------------------------------------
# 3. SAVE ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 
write_flex(estonia, file.path('data', 'clean', 'baltic_way_survey.csv'), format = format1)


#### > save spatial ------------------------------------------------------------------------------------------------------------------------------

### 'baltic_way' object (Estonia only + LGs crossed)
write_sf(baltic_way, file.path('data', 'spatial', 'baltic_way_estonia_clean.geojson'))

### LG shp with the distances from BW
write_sf(estonia_lg_sf, file.path('data', 'spatial', 'estonia_lg_2023_dist.geojson'))

### all localities in the survey the distances from BW
write_sf(estonia_localities_sf, file.path('data', 'spatial', 'estonia_localities_sf.geojson'))


### routes -> for the vein map? (currently done above)
# write_sf(routes_lg, file.path('data', 'spatial', 'routes_lg.geojson'))
# write_sf(routes_locality, file.path('data', 'spatial', 'routes_locality.geojson'))



### (*)checks -------------------------------------------------------------------------------------------------------------------------------------------------
### -> basic correlations with Finnish TV 
# sf(estonia$bw2_finnish_tv)
# pr_na(estonia$bw2_finnish_tv)
# 
# prop.table(table( estonia$bw1_soviet,  is.na(estonia$bw2_finnish_tv), useNA='ifany'), 2)
# prop.table(table( is.na(estonia$bw2_ref91),  is.na(estonia$bw2_finnish_tv), useNA='ifany'), 2)
# (table( is.na(estonia$bw2_ref91),  is.na(estonia$bw2_finnish_tv), useNA='ifany'))
# 
# 
# table(grepl("nobody|prefer|don't know|^$", estonia$bw2_ref91), 
#       grepl("nobody|prefer|don't know|^$", estonia$bw2_finnish_tv), useNA = 'ifany')
# 
# 
# ### duration
# summary(estonia$duration_in_seconds/60)
# sf(estonia$duration_in_seconds > 60 & estonia$duration_in_seconds < 15*60)
# hist(estonia$duration_in_seconds/60)
# 
# 
# 
# ### lat-lon
# plot(estonia$location_latitude[estonia$location_latitude >55 & estonia$location_latitude < 65],
#      estonia$location_longitude[estonia$location_latitude >55 & estonia$location_latitude < 65])


# ' --------------------------------------------------------------------------------------------------------------------------------------------
# **SCRAPBOOK -----------------------------------------------------------------------------------------------------------------------------------
#


### read questionnaire? -------------------------------------------------------------------------------------------------
# library(jsonlite)
# temp = fromJSON(file.path('data', 'qualtrics_questionnaire.qsf'), flatten = TRUE)$SurveyElements %>% clean_names()
# dim(temp)
# names(temp)
# 
# temp$element %>% sf
# temp2 = temp %>%  filter(element == "SQ")
# 
# temp2$payload[36]%>% unlist
# 
# survey_qs =  temp %>%  filter(element == "SQ") %>% pull(secondary_attribute)


# ' --------------------------------------------------------------------------------------------------------------------------------------------
# ACTA EST FABULA -------------------------------------------------------------------------------------------------------------------------
#