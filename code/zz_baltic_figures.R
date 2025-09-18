
#
# SET-UP --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#
rm(list=ls())

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


# '----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### read data --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#

#### clean survey --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
estonia = fread(file.path('data', 'clean', 'baltic_way_survey.csv')) 

#### LG .shp  --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
estonia_lg_sf = read_sf(file.path(main_dir, 'data', 'spatial', 'estonia_lg_2023.shp')) %>% clean_names() %>% 
  rename(county = mnimi, lg = onimi) %>% 
  select(c(county,lg)) %>% 
  mutate(across(c(county, lg), ~tolower(.)))


##### + population -----------------------------------------------------------------------------------------------------------------------------------------------
population <- fread(file.path(main_dir, 'data', 'raw', 'population_2016_2023.csv')) %>% 
  filter(year == 2023) %>% 
  select(lg, population) %>% 
  mutate(across(c(lg), ~tolower(.)))

estonia_lg_sf = left_join(estonia_lg_sf, population) %>% st_as_sf()


### map | BW2 pol. activity by 89' you/parents ----------------------------------------------------------------------------------------------------------------------------------------

estonia$bw5_locality_any


fdistinct(estonia$bw5_locality_any)
fdistinct(estonia$bw5_lon_any)



### *checks
pr_na(estonia$bw2_finnish_tv_you) # % watching Finnish TV
pr_na(estonia$bw2_finnish_tv_you[estonia$yob < 1985]) # % watching Finnish TV among born <1985 (33%)
prop.table(table(estonia$bw2_finnish_tv_you,estonia$yob < 1985, useNA = 'ifany'), 2) # % watching Finnish TV <1985 vs >1985 -> no one born >1985 admits watching

### select variables to run the mapping loop for
bw2_vars = names(estonia)[grepl('^bw2', names(estonia)) & grepl('_multi', names(estonia))]


### map across selected variables
for(var1 in bw2_vars){
  
  print(var1) # control the loop
  
  ### assign title to the map
  if(grepl('communist_', var1)){title1 = 'Became a member of the Communist Party'}
  if(grepl('pop_front_', var1)){title1 = 'Became member of the Popular Front of Estonia [Eestimaa Rahvarinne]'}
  if(grepl('forest_', var1)){title1 = 'Participated in activities of Estonian partisans (e.g., Forest Brothers) against Soviet rule'}
  if(grepl('exit_', var1)){title1 = 'Applied for an exit visa'}
  if(grepl('radio_', var1)){title1 = 'Listened to Radio Free Europe or Radio Liberty regularly (at least weekly)'}
  if(grepl('finnish_tv', var1)){title1 = 'Watched Finnish TV regularly (at least weekly)'}
  if(grepl('russian_tv', var1)){title1 = 'Watched Russian TV regularly (at least weekly)'}
  if(grepl('protest_pre', var1)){title1 = 'Took part in any public demonstration before the Baltic Way demonstration of 23rd August 1989'}
  if(grepl('protest_bw', var1)){title1 = 'Took part in the Baltic Way demonstration of 23rd August 1989'}
  if(grepl('ref91_', var1)){title1 = 'Voting ‘Yes’ in the Independence Referendum of 1991'}
  
 

  ### leave data for plotting
  dta_plot = estonia %>% 
    mutate(var_multi = !!rlang::ensym(var1)) %>% 
    
    # rename(lg_plot = lg) %>% # (2025 OR 1989 residence?)
    rename(lg_plot = bw5_lg_any) %>% # (2025 OR 1989 residence?)
    
    # filter(yob < 1985) %>% # leave only people born <1985
    # filter missing obs
    filter(!is.na(lg_plot)) %>% 
    group_by(lg_plot) %>% 
    filter(!all(is.na(var_multi))) %>% 
    
    select(lg_plot, # leave only necessary columns
           matches(str_remove(var1, '_multi')),  -matches('_multi$|_na$')) %>% 
    
    summarise_all(., mean_miss) %>% # summarise
    mutate(across(everything(), ~replace(.x, is.nan(.x), NA))) # NaN -> NA
  # complete(lg = factor(estonia$lg), fill = list(lg=NA))
  
  
  ### to long format
  dta_plot = dta_plot %>% gather(var, value, -lg_plot)
  
  
  # add geo-coordinates
  dta_plot = dta_plot %>% left_join(., estonia_lg_sf %>% rename(lg_plot = lg)) %>% st_as_sf() 
  
  
  ### create labels (for full matrix)
  dta_plot$var = gsub(str_remove(var1, '_multi'), '', dta_plot$var)
  dta_plot$var = gsub('^_', '', dta_plot$var)
  unique(dta_plot$var)
  
  labels_you =  c(
    you   = "You",
    immediate_family_parents_or_siblings   = "Parents or siblings",
    grandparents   = "Grandparents",
    another_family_member   = "Another family member",
    nobody   = "Nobody"
  )
  
  ### order labels
  dta_plot$var =  factor(dta_plot$var, levels = c("you","immediate_family_parents_or_siblings",
                                                  "grandparents",  'another_family_member', 'nobody'))
  
  
  
  
  ### plot
  g1= ggplot()+
    
    geom_sf(data = estonia_lg_sf, fill = 'grey30')+
    geom_sf(data = dta_plot, aes(fill = value),
            color = 'black') +
    
    facet_wrap(~var, labeller = labeller(var = labels_you))+ 
    
    scale_fill_gradient2(name = '', # 'share',
                         low = "darkblue", high = "#EC5F06",
                         mid = "#fffff0", 
                         midpoint = mean_miss(dta_plot$value[dta_plot$var == 'nobody']),
                         # midpoint = .25,
                         labels = percent_format(),
                         breaks = round(seq(0, 1, length.out = 11), 2)
    ) +
    
    labs(
      title = 'Before 1991, did you or any of your family members do the following...',
      subtitle = str_wrap_br(paste0(title1, '<br>'), 80)
    ) +
    
    map_theme +
    theme(
      plot.title = element_markdown(size = 25),
      plot.subtitle = element_markdown(size = 24, face = 'bold'),
      
      legend.title = element_markdown(size = 20, angle = 90),
      legend.text = element_text(face = 'plain', size = 16),
      legend.ticks = element_line(color = 'grey70', size = .9),
      legend.key.width = unit(3.8, 'cm'),
      legend.key.height = unit(.6, 'cm'),
      
      strip.text = element_markdown(size = 18, color = 'black')
    )
  
  ggsave(g1, width = 35, height = 21, unit = 'cm',
         # file = file.path('figures', 'aggregate', 'Finnish TV watchers by referent.png'))
         file = file.path('figures', 'aggregate', 'BW2 pre-1989 activity',
                          paste0("BW2 ", title1, ".png")))
  
  
  if(grepl('protest_bw', var1)){assign('dta_bw_lg', dta_plot)}
  
  
  
}



### xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx



### plot | BW distance vs participation ----------------------------------------------------------------------------------------------------------------------------------------


dta_plot = left_join(dta_bw_lg %>% rename(lg = lg_plot),
                     estonia %>% select(c(lg, dist_bw_lg_current)) %>% distinct)

labels_you =  c(
  you   = "You",
  immediate_family_parents_or_siblings   = "Parents or siblings",
  grandparents   = "Grandparents",
  another_family_member   = "Another family member",
  nobody   = "Nobody"
)


g1 = ggplot(dta_plot, aes(x=dist_bw_lg_current, y=value))+
  geom_point(size = 4, fill = 'grey')+
  geom_smooth(method='lm', se=F, color='red', linetype='dashed')+
  facet_wrap(~var, labeller = labeller(var = labels_you))+

  scale_y_continuous(expand = expansion(mult = c(0.04, 0.1)),
                     labels = percent_format()) +
  labs(
    title = 'Baltic Way participation',
    x = 'Distance from the municipality of residence (in 1989) from the Baltic Way pathway (km)',
    y = str_wrap_br('Share of respondents declaring participation for a given referent', 40)
  )
  
g1

ggsave(g1, width = 35, height = 21, unit = 'cm',
       # file = file.path('figures', 'aggregate', 'Finnish TV watchers by referent.png'))
       file = file.path('figures', 'BW participation vs distance (LG).png'))



#### reg ----------------------------------------------------------------------------------------------------------------------------------------------
names(estonia)[1:50]
estonia[, ethnic_estonia := fifelse(ethnic == 'estonian', 'Estonian', 'Other')]


m1 = lm(bw2_protest_bw_you ~ gender + age + edu + ethnic + dist_bw_lg_1989,
        data = estonia)
summary(m1)

        
m2 = glm(bw2_protest_bw_you ~ gender + age + edu + citizenship + 
           dist_bw_lg_1989,
         family = binomial(link = 'logit'),
         data = estonia[estonia$yob < 1985,])

summary(m2)


### map | languages spoken ----------------------------------------------------------------------------------------------------------------------------------------

## NOTE: Finnish knowledge not really showing, even for those born 1960-1985, at least when any knowledge of language treated as one variable

var1 = 'lang_home_now_multi'

dta_plot = estonia %>% 
  mutate(var_multi = lang_home_now_multi) %>% 
  
  rename(lg_plot = lg) %>% # (2025 OR 1989 residence?)
  # rename(lg_plot = bw5_lg_any) %>% # (2025 OR 1989 residence?)
  
  filter(yob < 1985) %>% # leave only people born <1985
  filter(yob > 1960) %>% # leave only people born >1960
  # filter missing obs
  filter(!is.na(lg_plot)) %>% 
  group_by(lg_plot) %>% 
  filter(!all(is.na(var_multi))) %>% 
  
  select(lg_plot, # leave only necessary columns
         matches(str_remove(var1, '_multi')),  -matches('_multi$|_na$')) %>% 
  
  summarise_all(., mean_miss) %>% # summarise
  mutate(across(everything(), ~replace(.x, is.nan(.x), NA))) # NaN -> NA
# complete(lg = factor(estonia$lg), fill = list(lg=NA))


### to long format
dta_plot = dta_plot %>% gather(var, value, -lg_plot)


# add geo-coordinates
dta_plot = dta_plot %>% left_join(., estonia_lg_sf %>% rename(lg_plot = lg)) %>% st_as_sf() 


### create labels (for full matrix)
dta_plot$var = gsub(str_remove(var1, '_multi'), '', dta_plot$var)
dta_plot$var = gsub('^_', '', dta_plot$var)
dta_plot$var = gsub('_', ' ', dta_plot$var)
dta_plot$var = str_to_sentence(dta_plot$var)
unique(dta_plot$var)


### order labels
dta_plot$var =  factor(dta_plot$var, levels = c('Estonian', 'Russian', 'English',
                                                'Finnish', 'Ukrainian', 'Other', 'Not applicable'))
### plot
g1= ggplot()+
  
  geom_sf(data = estonia_lg_sf, fill = 'grey30')+
  geom_sf(data = dta_plot %>% filter(var %in% c('Finnish')),
          aes(fill = value),
          color = 'black') +
  
  facet_wrap(~str_to_title(var), nrow = 2)+ 
  
  scale_fill_gradient2(name = '',# 'share',
    low = "darkblue", high = "#EC5F06",
    mid = "#fffff0", 
    # midpoint = mean_miss(dta_plot$value[dta_plot$var == 'nobody']),
    midpoint = .1,
    labels = percent_format(),
    breaks = round(seq(0, 1, length.out = 11), 2)
  ) +
  
  labs(
    # title = 'Before 1991, did you or any of your family members do the following...',
    # subtitle = str_wrap_br(paste0(title1, '<br>'), 80)
  ) +
  
  map_theme +
  theme(
    plot.title = element_markdown(size = 25),
    plot.subtitle = element_markdown(size = 24, face = 'bold'),
    
    legend.title = element_markdown(size = 20, angle = 90),
    legend.text = element_text(face = 'plain', size = 16),
    legend.ticks = element_line(color = 'grey70', size = .9),
    legend.key.width = unit(3.8, 'cm'),
    legend.key.height = unit(.6, 'cm'),
    
    strip.text = element_markdown(size = 18, color = 'black')
  )


ggsave(g1, width = 35, height = 21, unit = 'cm',
       # file = file.path('figures', 'aggregate', 'Finnish TV watchers by referent.png'))
       file = file.path('figures', 'aggregate',
                        paste0("Languages spoken (anywhere)", ".png")))


### map | respondents by LG ----------------------------------------------------------------------------------------------------------------------------------------
dta_plot = estonia %>% count(lg) %>% left_join(., estonia_lg_sf) %>% st_as_sf() %>% 
  mutate(n_rel = 1000*n/population)


median1 = median_miss(dta_plot$n_rel)
min1 = min_miss(dta_plot$n_rel)
max1 = max_miss(dta_plot$n_rel)

g1 = ggplot(dta_plot, # %>% filter(!lg %in% c('tallinn', 'tartu linn')), 
            aes(fill = n_rel))+
  geom_sf(color = 'black')+
  # guides(fill = 'none')+
  
  labs(
    title = str_wrap_br('<b>Distribution of survey respondents by municipality</b>', 50)
  ) +
  scale_fill_gradient2(name = 'Respondents per 1,000 inhabitants',
                       low = "darkblue", high = "#EC5F06",
                       mid = "white", midpoint = median1,
                       breaks = round(seq(min1, max1, length.out = 7), 2)
  ) +
  
  guides(fill = guide_colorbar(title.position = 'left')) +
  
  map_theme +
  theme(
    plot.title = element_markdown(size = 38),
    
    legend.position = 'left',
    legend.direction = 'vertical',
    legend.title = element_markdown(size = 20, angle = 90),
    legend.text = element_text(face = 'plain', size = 20),
    legend.ticks = element_line(color = 'grey70', size = .9),
    legend.key.width = unit(.8, 'cm'),
    legend.key.height = unit(3.6, 'cm')
  )


ggsave(g1, width = 35, height = 28, unit = 'cm',
       file = file.path('figures', 'aggregate', 'respondents by lg.png'))


### map | respondents by 89' LG ----------------------------------------------------------------------------------------------------------------------------------------
dta_plot = estonia %>% count(bw5_lg_any) %>% rename(lg = bw5_lg_any) %>% left_join(., estonia_lg_sf) %>% st_as_sf() %>% 
  mutate(n_rel = 1000*n/population)

mean = mean_miss(dta_plot$n)
min1 = min_miss(dta_plot$n)
max1 = max_miss(dta_plot$n)

dta_plot = dta_plot %>%  filter(!lg %in% c('tallinn', 'tartu linn')) %>% filter(!is.na(lg))

g1 = ggplot(dta_plot, 
            aes(fill = n))+
  geom_sf(color = 'black')+
  # guides(fill = 'none')+
  
  labs(
    title = str_wrap_br('<b>Distribution of survey respondents by municipality</b>', 50)
  ) +
  scale_fill_gradient(
    name = "Variable",
    low = "white",
    high = "#EC5F06",
    # limits = c(0, 100),           # optional: set min/max
    breaks = c(0,1,2,3,4,5,6,7,8,9,10,20, 50, max1) # custom tick marks
  ) +
  
  guides(fill = guide_colorbar(title.position = 'left')) +
  
  map_theme +
  theme(
    plot.title = element_markdown(size = 38),
    
    legend.position = 'left',
    legend.direction = 'vertical',
    legend.title = element_markdown(size = 20, angle = 90),
    legend.text = element_text(face = 'plain', size = 20),
    legend.ticks = element_line(color = 'grey70', size = .9),
    legend.key.width = unit(.8, 'cm'),
    legend.key.height = unit(3.6, 'cm')
  )


ggsave(g1, width = 35, height = 28, unit = 'cm',
       file = file.path('figures', 'aggregate', 'respondents residence 1989.png'))



### map | party thermometers -----------------------------------------------------------------------------------------------------------------
dta_plot = estonia %>% 
  select(c(starts_with("party_th_"))) %>% 
  pivot_longer(
    cols = starts_with("party_th_"),
    names_to = "party",
    values_to = "thermometer"
  ) %>% 
  count(party, thermometer)

# Make nice labels for facets
party_labels <- c(
  party_th_isamaa = "Isamaa (Fatherland)",
  party_th_kesk   = "Keskerakond (Centre)",
  party_th_ekre   = "EKRE",
  party_th_sde    = "SDE (Social Democrats)",
  party_th_parem  = "Parem",
  party_th_eer    = "Greens",
  party_th_ee200  = "Eesti 200",
  party_th_euvp   = "EUVP (United Left)",
  party_th_reform = "Reform"
)

# Plot
g1 = ggplot(dta_plot %>% filter(!is.na(thermometer)), aes(x = factor(thermometer), y = n, fill = party)) +
  geom_bar(stat = 'identity', position = position_dodge(0.9), color = "black") +
  
  facet_wrap(~party, labeller = labeller(party = party_labels), scales = 'fixed') +
  
  # scale_x_continuous(breaks = 0:10, limits = c(-1, 11)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  
  scale_fill_manual(
    name = '',  
    values = c(
      party_th_isamaa = "#77BDD9",       # Dark Blue, or choose a blue you prefer for Isamaa
      party_th_kesk   = "#008000",       # Green for Centre Party
      party_th_ekre   = "#241C9C",       # Black (common for EKRE in polls)
      party_th_sde    = "#FF0000",       # Red for SDE
      party_th_parem  = "#FFA500",       # Orange for Parempoolsed
      party_th_eer    = "#00CC00",       # Green-like (but distinct from Centre) for Greens
      party_th_ee200  = "#073342",       # Indigo for Eesti 200
      party_th_euvp   = "#800080",       # Purple-ish for United Left (or pick something distinct)
      party_th_reform = "#FFFF00"        # Yellow for Reform
    )) +
  guides(fill = 'none') +
  
  labs(
    x = "Thermometer (0–10)",
    y = "Count",
    title = "<br>Party Thermometer Ratings<br>"
  )

ggsave(g1, width = 35, height = 28, unit = 'cm',
       file = file.path('figures', 'party_thermometers.png'))



#

# ' --------------------------------------------------------------------------------------------------------------------------------------------------
#  SCRAPBOOK ------------------------------------------------------------------------------------------------------------------------------------


### map | BW part. by 89' LG ----------------------------------------------------------------------------------------------------------------------------------------
estonia[, bw2_protest_bw_dummy := fcase(
  str_detect(bw2_protest_bw, '^nobody'), 'no one in the family',
  is.na(bw2_protest_bw), 'no response',
  default = 'anyone in the family'
)]

pr_na(estonia$bw2_protest_bw_dummy) # nice confirmation that 1 in 4 (27% here) of Estonians took part in the BW (I appreciate it's wide net, as it's not only 'you;)


dta_plot = estonia %>% count(bw5_lg_any, bw5_locality_any, bw2_protest_bw_dummy) %>% 
  rename(lg = bw5_lg_any) %>% left_join(., estonia_lg_sf) %>% st_as_sf() %>% 
  mutate(n_rel = 1000*n/population)


g1 = ggplot(dta_plot  %>% filter(!bw2_protest_bw_dummy %in% c('no response')), 
            aes(fill = n_rel))+
  geom_sf(color = 'black')+
  facet_wrap(~bw2_protest_bw_dummy) +
  # guides(fill = 'none')+
  
  labs(
    title = str_wrap_br('<b>Baltic Way participation</b>', 50)
  ) +
  # scale_fill_gradient2(name = 'Respondents per 1,000 inhabitants',
  #                      low = "darkblue", high = "#EC5F06",
  #                      mid = "white", midpoint = median1,
  #                      breaks = round(seq(min1, max1, length.out = 7), 2)
  # ) +
  # 
  guides(fill = guide_colorbar(title.position = 'left')) +
  
  map_theme +
  theme(
    plot.title = element_markdown(size = 38),
    
    legend.position = 'left',
    legend.direction = 'vertical',
    legend.title = element_markdown(size = 20, angle = 90),
    legend.text = element_text(face = 'plain', size = 20),
    legend.ticks = element_line(color = 'grey70', size = .9),
    legend.key.width = unit(.8, 'cm'),
    legend.key.height = unit(3.6, 'cm')
  )


ggsave(g1, width = 35, height = 18, unit = 'cm',
       file = file.path('figures', 'aggregate', 'Baltic Way participation.png'))




### map | Finnish TV by 89' LG ----------------------------------------------------------------------------------------------------------------------------------------
estonia[, bw2_finnish_tv_dummy := fcase(
  str_detect(bw2_finnish_tv, '^nobody'), 'no one in the family',
  is.na(bw2_finnish_tv), 'no response',
  default = 'anyone in the family'
)]


dta_plot = estonia %>% count(bw5_lg_any, bw5_locality_any, bw2_finnish_tv_dummy) %>% 
  rename(lg = bw5_lg_any) %>% left_join(., estonia_lg_sf) %>% st_as_sf() %>% 
  mutate(n_rel = 1000*n/population)



g1 = ggplot(dta_plot %>% filter(!bw2_finnish_tv_dummy %in% c('no response')), 
            aes(fill = n_rel))+
  geom_sf(color = 'black')+
  facet_wrap(~bw2_finnish_tv_dummy) +
  # guides(fill = 'none')+
  
  labs(
    title = str_wrap_br('<b>Finnish TV watching patterns </b>', 50)
  ) +
  # scale_fill_gradient2(name = 'Respondents per 1,000 inhabitants',
  #                      low = "darkblue", high = "#EC5F06",
  #                      mid = "white", midpoint = median1,
  #                      breaks = round(seq(min1, max1, length.out = 7), 2)
  # ) +
  # 
  guides(fill = guide_colorbar(title.position = 'left')) +
  
  map_theme +
  theme(
    plot.title = element_markdown(size = 38),
    
    legend.position = 'left',
    legend.direction = 'vertical',
    legend.title = element_markdown(size = 20, angle = 90),
    legend.text = element_text(face = 'plain', size = 20),
    legend.ticks = element_line(color = 'grey70', size = .9),
    legend.key.width = unit(.8, 'cm'),
    legend.key.height = unit(3.6, 'cm')
  )


ggsave(g1, width = 35, height = 18, unit = 'cm',
       file = file.path('figures', 'aggregate', 'Finnish TV watchers.png'))



# ' --------------------------------------------------------------------------------------------------------------------------------------------------
# ACTA EST FABULA -------------------------------------------------------------------------------------------------------------------------
#