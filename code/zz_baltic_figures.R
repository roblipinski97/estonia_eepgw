
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
estonia_lg_sf = read_sf(file.path(main_dir, 'data', 'spatial', 'estonia_lg_2023_dist.shp')) %>% clean_names()


##### + population -----------------------------------------------------------------------------------------------------------------------------------------------
population <- fread(file.path(main_dir, 'data', 'raw', 'population_2016_2023.csv')) %>% 
  filter(year == 2023) %>% 
  select(lg, population) %>% 
  mutate(across(c(lg), ~tolower(.)))

estonia_lg_sf = left_join(estonia_lg_sf, population) %>% st_as_sf()

#### pathway BW ------------------------------------------------------------------------------------------------------------------------------------------------------
baltic_way = read_sf(file.path('data', 'spatial', 'baltic_way_estonia_clean.geojson'))



### map | Baltic Way participation ----------------------------------------------------------------------------------------------------------------------------------------


### > summarise --------------------------------------------------------------------------------------------------------------------------------------------
dta_plot = estonia %>% select(c(matches('yob|bw5|bw3'))) %>% 
  filter(yob <= 1989) %>% 
  mutate(lg_plot = bw5_lg_you) %>% 
  select(c(lg_plot, matches('bw3_me_dummy'))) %>% 
  group_by(lg_plot) %>% 
  summarise_all(., mean_miss) %>% # summarise
  mutate(across(everything(), ~replace(.x, is.nan(.x), NA))) # NaN -> NA

# find nobs used for plotting
n1 = estonia %>% 
  filter(yob <= 1989) %>% 
  filter(!is.na(bw3_me)) %>% 
  filter(!is.na(bw5_lg_you)) %>% 
  nrow()

# average participation?
estonia %>% filter(yob <= 1989) %>% mutate(temp = grepl('took part', bw3_me)) %>% pull(temp) %>% pr

### to long format
dta_plot = dta_plot %>% gather(var, value, -lg_plot)

# add geo-coordinates
dta_plot = dta_plot %>% left_join(., estonia_lg_sf %>% rename(lg_plot = lg)) %>% st_as_sf() 

### create labels (for full matrix)
labels_you =  c(
  bw3_me_dummy_part   = "Participated or witnessed",
  bw3_me_dummy_watch   = "Watched/listened in the media",
  bw3_me_dummy_none   = "Not involved"
  )

### order labels
dta_plot$var =  factor(dta_plot$var, levels = c('bw3_me_dummy_part',  'bw3_me_dummy_watch', 'bw3_me_dummy_none'))



### > plot ---------------------------------------------------------------------------------------------------------------------------------------------------
g1= ggplot()+
  
  geom_sf(data = estonia_lg_sf, fill = 'grey30')+
  geom_sf(data = dta_plot, aes(fill = value),
          color = 'black') +
  geom_sf(data = baltic_way, color = 'red', linewidth = 1.1) +
  
  facet_wrap(~var, labeller = labeller(var = labels_you))+ 
  
  scale_fill_gradient2(name = 'Share of respondents', # 'share',
                       low = "darkblue", high = "#EC5F06",
                       mid = "#F2E5E1", 
                       # midpoint = mean_miss(dta_plot$value[dta_plot$var == 'nobody']),
                       midpoint = .25,
                       labels = percent_format(),
                       breaks = round(seq(0, 1, length.out = 11), 2)
  ) +
  guides(fill = guide_colorbar(title.position = 'top')) +
  labs(
    # title = str_wrap_br('Thinking about the Baltic Way demonstration on 23rd August 1989,
    #                     please indicate which of the following options best describes the form of participation of...<br>', 90),
    # subtitle = str_wrap_br(paste0('<b>You personally</b>', '<br>'), 80),
    caption = paste0("<br><b>Note:</b> Based on an online EEPGW survey conducted on a sample of 1,222 adult Estonians in September 2025. 
    Only respondents born before 1990 are included in the sample used for those calculations (n =",
    prettyNum(n1, big.mark = ',') ,  
    
    "). Values calculated per self-declared municipality <b>residence in 1989</b>. 
    Pathway of the <b><span style='color:red;'>Baltic Way</span></b> is marked in red.")
  ) +
  
  map_theme +
  theme(
    plot.title = element_markdown(size = 19),
    plot.subtitle = element_markdown(size = 20),
    plot.caption = element_textbox_simple(size = 12),
    
    legend.title = element_markdown(size = 15, angle = 00),
    legend.text = element_text(face = 'plain', size = 14),
    legend.key.width = unit(2.9, 'cm'),
    legend.key.height = unit(.5, 'cm'),
    
    strip.text = element_markdown(size = 15, color = 'black')
  )


ggsave(g1, width = 30, height = 14, unit = 'cm',
       file = file.path('figures', 'BW participation (you).png'))




### scatterplot | BW distance vs participation ----------------------------------------------------------------------------------------------------------------------------------------




g2 = ggplot(dta_plot %>% st_drop_geometry(),
       aes(x = dist_bw_lg, y = value))+
  geom_hline(yintercept = 0)+
  geom_point(fill = 'grey', size = 4)+
  geom_smooth(method = 'lm', se = F, color = 'black', linetype = 'dashed') +
  
  facet_wrap(~var, labeller = labeller(var = labels_you)) +
  
  scale_y_continuous(expand = expansion(mult = c(0.00, 0.1)),
                     labels = percent_format()) +
  
  
  labs(
    # title = str_wrap_br('Thinking about the Baltic Way demonstration on 23rd August 1989,
    #                     please indicate which of the following options best describes the form of participation of...<br>', 90),
    # subtitle = str_wrap_br(paste0('<b>You personally</b>', '<br>'), 80),
    x = '<br>Distance from the self-declared municipality <b>residence in 1989</b> to the Baltic Way pathway (km)<br>',
    y = str_wrap_br('Share of respondents', 40),
    caption = paste0("<br><b>Note:</b> Based on an online EEPGW survey conducted on a sample of 1,222 adult Estonians in September 2025. 
    Only respondents born before 1990 are included in the sample used for those calculations (n =",
      prettyNum(n1, big.mark = ',') , 
      "). Black dashed line shows the linear line of best fit.")
  ) +
  theme(
    axis.line.x = element_blank(),
    plot.caption = element_textbox_simple(size = 12)
  )

ggsave(g2, width = 40, height = 15, unit = 'cm',
       file = file.path('figures', 'BW participation (you) vs distance (LG).png'))





# ' ----------------------------------------------------------------------------------------------------------------------------------------
### map | BW2 pol. activity by 89' you/parents ----------------------------------------------------------------------------------------------------------------------------------------

### *checks
pr_na(estonia$bw2_finnish_tv_you) # % watching Finnish TV
pr_na(estonia$bw2_finnish_tv_you[estonia$yob < 1981]) # % watching Finnish TV among born <1985 (33%)
prop.table(table(estonia$bw2_finnish_tv_you,estonia$yob < 1981, useNA = 'ifany'), 2) # % watching Finnish TV <1985 vs >1985 -> no one born >1985 admits watching

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
  if(grepl('bw2_protest_post', var1)){title1 = 'Took part in any public demonstration after the Baltic Way demonstration of 23rd August 1989'}
  if(grepl('ref91_', var1)){title1 = 'Voting ‘Yes’ in the Independence Referendum of 1991'}
  
 

  ### leave data for plotting
  dta_plot = estonia %>% 
    mutate(var_multi = !!rlang::ensym(var1)) %>% 
    
    # rename(lg_plot = lg) %>% # (2025 OR 1989 residence?)
    rename(lg_plot = bw5_lg_any) %>% # (2025 OR 1989 residence?)
    
    filter(yob < 1981) %>% # leave only people born <1985
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
                         mid = "#F2E5E1", 
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
      legend.key.width = unit(3.8, 'cm'),
      legend.key.height = unit(.6, 'cm'),
      
      strip.text = element_markdown(size = 18, color = 'black')
    )
  
  ggsave(g1, width = 35, height = 21, unit = 'cm',
         # file = file.path('figures', 'aggregate', 'Finnish TV watchers by referent.png'))
         file = file.path('figures', 'aggregate', 'BW2 pre-1989 activity',
                          paste0("BW2 ", title1, ".png")))
  
  
  
  
}




### map | repressions you/parents ----------------------------------------------------------------------------------------------------------------------------------------



### *checks
pr_na(estonia$repress_discriminate_you) # 5.8% repressed personnaly?
pr_na(estonia$repress_discriminate_you[estonia$yob < 1985]) # 8.1% if born <1985
prop.table(table(estonia$repress_discriminate_you, estonia$yob < 1985, useNA = 'ifany'), 2)


### select variables to run the mapping loop for
repress_vars = names(estonia)[grepl('^repress_', names(estonia)) & grepl('_multi', names(estonia))]


### map across selected variables
for(var1 in repress_vars){
  
  print(var1) # control the loop

  
  ### assign title to the map
  if(grepl('repress_ww2_multi', var1)){title1 = 'Killed by Nazi or Nazi-allied forces during WWII'}
  if(grepl('repress_deport_multi', var1)){title1 = 'Deportation or forced resettlement to another part of the USSR'}
  if(grepl('repress_prison_multi', var1)){title1 = 'Sent to labor camp or prison for a political reason '}
  if(grepl('repress_killed_multi', var1)){title1 = 'Killed by state authorities (e.g., shot, disappeared)'}
  if(grepl('repress_torture_multi', var1)){title1 = 'Subject to torture or some other form of physical violence by the state (not killed) '}
  if(grepl('repress_discriminate_multi', var1)){title1 = 'Discrimination in education or professional life'}
  if(grepl('repress_seizure_multi', var1)){title1 = 'Unlawful seizure of property during collectivization'}
  if(grepl('repress_religion_multi', var1)){title1 = 'Religious discrimination'}
  if(grepl('repress_surveillance_multi', var1)){title1 = 'Surveillance from state agents'}
  
  
  
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
                         mid = "#F2E5E1", 
                         midpoint = mean_miss(dta_plot$value[dta_plot$var == 'nobody']),
                         # midpoint = .25,
                         labels = percent_format(),
                         breaks = round(seq(0, 1, length.out = 11), 2)
    ) +
    
    labs(
      title = 'Before 1991, did you or any of your family members experience the following… ',
      subtitle = str_wrap_br(paste0(title1, '<br>'), 80)
    ) +
    
    map_theme +
    theme(
      plot.title = element_markdown(size = 25),
      plot.subtitle = element_markdown(size = 24, face = 'bold'),
      
      legend.title = element_markdown(size = 20, angle = 90),
      legend.text = element_text(face = 'plain', size = 16),
      legend.key.width = unit(3.8, 'cm'),
      legend.key.height = unit(.6, 'cm'),
      
      strip.text = element_markdown(size = 18, color = 'black')
    )
  
  ggsave(g1, width = 35, height = 21, unit = 'cm',
         # file = file.path('figures', 'aggregate', 'Finnish TV watchers by referent.png'))
         file = file.path('figures', 'aggregate', 'Repressions',
                          paste0("Repressions ", title1, ".png")))
  
  
}







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
    legend.key.width = unit(.8, 'cm'),
    legend.key.height = unit(3.6, 'cm')
  )


ggsave(g1, width = 35, height = 28, unit = 'cm',
       file = file.path('figures', 'aggregate', 'respondents residence 1989.png'))



### xxx | protests pre-BW-post -----------------------------------------------------------------------------------------------------------------

### create dummy for ANY protest at ANY point post-1991
estonia <- estonia %>%
  mutate(
    pol_past_protest_any_post1991 = if_else(
      rowSums(select(., 
                     pol_past_protest_elections_1991_post_independence,
                     pol_past_protest_elections_2022_last_3_5_years,
                     pol_past_protest_other_1991_post_independence,
                     pol_past_protest_other_2022_last_3_5_years
      ) == 1, na.rm = TRUE) > 0,
      1, 0
    )
  )

estonia$pol_past_protest_any_post1991 %>% sf


### add dummy if LG actually crossed by BW pathway
baltic_way = read_sf(file.path('data', 'spatial', 'baltic_way_estonia_clean.geojson'))


### calculate plot data


dta_plot = estonia %>% 
  filter(yob < 1981) %>% 
  filter(!bw5_lg_you %in% c('tallinn')) %>% 
  mutate(bw_cross = ifelse(bw5_lg_you %in% baltic_way$lg, 'Crossed', 'Not crossed')) %>% 
  select(c(bw_cross,
           bw2_protest_pre_you, bw2_protest_post_you,
           bw3_me_took_part_in_it_by_being_part_of_the_human_chain_formed,
           pol_past_protest_any_post1991
           )) %>% 
  group_by(bw_cross) %>% summarise_all(.,mean_miss) %>% 
  gather(var, value, -bw_cross) %>% 
  mutate(var = case_when(
    str_detect(var, 'pre_you') ~ 'Soviet era (pre-BW)',
    str_detect(var, 'took_part') ~ 'Baltic Way (1989)',
    str_detect(var, 'post_you') ~ 'Soviet era (post-BW)',
    str_detect(var, 'post1991') ~ 'Post-1991'
  ))

dta_plot$var = factor(dta_plot$var, levels = c('Soviet era (pre-BW)', 'Baltic Way (1989)',
                                               'Soviet era (post-BW)', 'Post-1991'))

g1=ggplot(dta_plot, aes(x = var, y = value, fill = bw_cross, group = bw_cross))+
  geom_bar(stat = 'identity', position = position_dodge(.9))+  
  geom_text(aes(label=paste0(round(value*100, 0), '%')), , position = position_dodge(.9),
            size = 7, vjust = -.5)+
  labs(
      # title = '<b>Municipality-level protest trends before and after the Baltic Way<b>',
       x = '', y = 'Share of respondents\ndeclaring protest participation\n',
       caption = '<br><b>Note:</b> Only respondents born before 1981 and those living outside Tallinn are
       included in the sample used for those calculations.<br>'
       ) +
  scale_fill_manual(name = 'BW crossing the municipality: ',
                    labels = c('Yes', 'No'),
                    values = c('#0072ce','#CE5D00')) + 
  scale_y_continuous(expand = expansion(mult=c(0,.1), add=c(0,0)),
                     labels=percent_format())+
  guides(fill = guide_legend(title.position = 'top')) +
  bar_theme+
  theme(
    legend.title = element_markdown(size = 26, face='bold'),
    legend.text = element_markdown(size = 26),
    axis.text.x = element_text(size = 20),
    legend.key.spacing.x = unit(.5,'cm')
  )

g1

# save
ggsave(plot = g1, file.path(main_dir, 'Figures', 'Protest dynamic pre-post BW.png'),
       width = 37, height = 20, unit = 'cm')





### plot | demographic distribution -----------------------------------------------------------------------------------------------------------------

names(estonia)[1:50]
var1 = 'gender'
temp = estonia %>% 
  mutate(gender = replace_na('missing')) 

# gender
g1 = estonia %>% 
  mutate(gender = ifelse(is.na(gender), 'Missing', str_to_sentence(gender))) %>% 
  ggplot()+
  geom_histogram(aes(x = gender), 
                 # stat = 'count', fill = c('#0072ce','#000000',  '#fffff0'), color = 'black') +
                 stat = 'count', fill = c('#0072ce','#CE5D00',  'grey'), color = 'black') +
  labs(title = '<b>Gender<b><br>', x = '', y = 'No. of respondents') +
  scale_y_continuous(expand = expansion(mult=c(0,.1), add=c(0,0)),
                     labels=comma)+
  bar_theme

estonia$edu %>% pr_na


# education
g2 = estonia %>% 
  mutate(edu = ifelse(is.na(edu), 'Missing', str_to_sentence(edu)),
         edu = factor(edu, levels = c("Primary or less",
                                      "Secondary",
                                      "Secondary (vocational)",
                                      "Bachelor's",
                                      "Master's or phd",
                                      'Missing'
                                ))) %>% 
  ggplot()+
  geom_histogram(aes(x = edu), 
                 stat = 'count', fill = c(rep('#0072ce', fdistinct(estonia$edu)-1), 'grey'), color = 'black') +
  labs(title = '<b>Education<b><br>', x = '', y = '') +
  scale_y_continuous(expand = expansion(mult=c(0,.1), add=c(0,0)),
                     labels=comma) +
  bar_theme  +
  theme(axis.text.x = element_text(size = 22, angle = 45, hjust = 1))



# ethniciy
table(estonia$ethnic, estonia$citizenship, useNA = 'ifany')

g3 = estonia %>% 
  mutate(ethnic = ifelse(ethnic %in% c('estonian', 'russian') & !is.na(ethnic), ethnic, 'other'),
         ethnic = ifelse(is.na(ethnic), 'Missing', str_to_sentence(ethnic)),
         ethnic = factor(ethnic, levels = c("Estonian",
                                      'Russian',
                                      'Missing'
         ))) %>% 
  ggplot()+
  geom_histogram(aes(x = ethnic), 
                 stat = 'count', fill =  c('#0072ce','#CE5D00',  'grey'), color = 'black') +
  labs(title = '<b>Ethnicity<b><br>', x = '', y = '') +
  scale_y_continuous(expand = expansion(mult=c(0,.1), add=c(0,0)),
                     labels=comma) +
  bar_theme


g3


### yob
estonia[, yob:=as.numeric(yob)]
breaks <- c(min(estonia$yob), 1950, 1960, 1970, 1980, 1990, 2000, max(estonia$yob))
labels <- c('<1950', '1950s', '1960s', '1970s', '1980s', '1990s', '2000s')

estonia$yob_group <- cut(estonia$yob, breaks = breaks, labels = labels, include.lowest = TRUE)

g4 = estonia %>% 
  ggplot()+
  geom_histogram(aes(x = yob_group), 
                 stat = 'count', fill = c(rep('#0072ce', fdistinct(estonia$yob_group)-1), 'grey'), color = 'black') +
  labs(title = '<b>Year of birth<b><br>', x = '', y = '') +
  scale_y_continuous(expand = expansion(mult=c(0,.1), add=c(0,0)),
                     labels=comma) +
  bar_theme 

g4


### satisf-financial
pr_na(estonia$satisf_financial)

g5=estonia %>% 
  mutate(satisf_financial = case_when(satisf_financial == 1 ~ '1 - very\nsatisfactory',
                                      satisf_financial == 2 ~ '2 - rather\nsatisfactory',
                                      satisf_financial == 3 ~ '3 - neither\nsatisfactory nor\nunsatisfactory',
                                      satisf_financial == 4 ~ '4 - rather\nunsatisfactory',
                                      satisf_financial == 5 ~ '5 - very\nunsatisfactory',
                                      .default = NA)) %>% 
  ggplot()+
  geom_histogram(aes(x = satisf_financial), 
                 stat = 'count', fill = c(rep('#0072ce', fdistinct(estonia$satisf_financial)-1), 'grey'), color = 'black') +
  labs(title = '<b>Satisfaction with fin. situation<b><br>', x = '', y = '') +
  scale_y_continuous(expand = expansion(mult=c(0,.1), add=c(0,0)),
                     labels=comma) +
  bar_theme +
  theme(axis.text.x = element_text(size = 22, angle = 45, hjust = 1))


### county


g6 = estonia %>% 
  ggplot()+
  geom_histogram(aes(x = gsub(' maakond', '', county)), 
                 stat = 'count', fill = c(rep('#0072ce', fdistinct(estonia$county)-1), 'grey'), color = 'black') +
  labs(title = '<b>County<b><br>', x = '', y = '') +
  scale_y_continuous(expand = expansion(mult=c(0,.1), add=c(0,0)),
                     labels=comma) +
  bar_theme +
  theme(axis.text.x = element_text(size = 22, angle = 45, hjust = 1))

g6




# combine
g_all = g1 + g4 + g3 + g2 + g5 + g6 +  
  plot_layout(ncol = 3) &
  plot_annotation('\nNumber of respondents by:\n',
                  caption = paste0("<br><b>Notes: </b> Based on ", 
                                   prettyNum(nrow(estonia), big.mark = ','),
                                   " survey responses collected in September 2025 by Norstat Estonia</b>.<br>"),
                  theme=theme(plot.title=element_markdown(size = 40, face = 'bold', hjust = .5),
                              plot.caption=element_textbox_simple(size = 20, hjust = 0)))


# save
ggsave(plot = g_all, file.path(main_dir, 'Figures', 'Demographics.png'),
       width = 67, height = 40, unit = 'cm')


### xxx plot | outcomes by key demo --------------------------------------------------------------------------------------------------------

for(var1 in c('county', 'gender', 'age', 'edu', 'ethnic', 'satisf_financial')){

  estonia = fread(file.path('data', 'clean', 'baltic_way_survey.csv')) %>% 
    mutate(value = !!rlang::ensym(var1))
    
    
  gender_share <- estonia %>%
    filter(!is.na(gender) & !is.na(value)) %>% 
    group_by(gender) %>% mutate(N = n()) %>% ungroup() %>%  # count all respondents 
    group_by(gender, N, value) %>% count %>% ungroup() %>% # count each response
    mutate(share = n/N, # calculate shares
           category = 'Gender')  # define category
  ### check if works 
  #sf(estonia_save$dem_04_01[estonia_save$mot_10_2a %in% c(1,2,3,4,5)])
  
  
  age_share <- estonia %>%
    filter(!is.na(age_group) & !is.na(value)) %>% 
    group_by(age_group) %>% mutate(N = n()) %>% ungroup() %>%  # count all respondents 
    group_by(age_group, N, value) %>% count %>% ungroup() %>% # count each response
    mutate(share = n/N, # calculate shares
           category = 'Age group')  # define category
  
  tenure_share <- estonia %>%
    filter(!is.na(tenure_group) & !is.na(value)) %>% 
    group_by(tenure_group) %>% mutate(N = n()) %>% ungroup() %>%  # count all respondents 
    group_by(tenure_group, N, value) %>% count %>% ungroup() %>% # count each response
    mutate(share = n/N, # calculate shares
           category = 'Tenure group')  # define category
  
  education_share <- estonia %>%
    filter(!is.na(education) & !is.na(value)) %>% 
    group_by(education) %>% mutate(N = n()) %>% ungroup() %>%  # count all respondents 
    group_by(education, N, value) %>% count %>% ungroup() %>% # count each response
    mutate(share = n/N, # calculate shares
           category = 'Education')  # define category
  
  manager_share <- estonia %>%
    filter(!is.na(manager) & !is.na(value)) %>% 
    group_by(manager) %>% mutate(N = n()) %>% ungroup() %>%  # count all respondents 
    group_by(manager, N, value) %>% count %>% ungroup() %>% # count each response
    mutate(share = n/N, # calculate shares
           category = 'Manager status')  # define category
  
  inst_share <- estonia %>%
    filter(!is.na(inst_level_labels) & !is.na(value)) %>% 
    group_by(inst_level_labels) %>% mutate(N = n()) %>% ungroup() %>%  # count all respondents 
    group_by(inst_level_labels, N, value) %>% count %>% ungroup() %>% # count each response
    mutate(share = n/N, # calculate shares
           category = 'Institution level',  # define category
           inst_level_labels = dplyr::case_when( # re-label (helps with plot ordering)
             inst_level_labels == 'Central' ~ 'ADM-0 (central)',
             inst_level_labels == 'Voivodeship (adm-1)' ~ 'ADM-1 (voivodeship)',
             inst_level_labels == 'Powiat (adm-2)' ~ 'ADM-2 (powiat)',
             inst_level_labels == 'Gmina (adm-3)' ~ 'ADM-3 (gmina)',
             .default = inst_level_labels))
  
  
  
  # Combine
  estonia_plot = rbindlist(list(gender_share, age_share, tenure_share,
                               education_share, manager_share, inst_share)) %>% 
    rename(label = gender) %>% 
    filter(label != 'missing') %>% 
    mutate(label = str_wrap(label, 15))
  
  # Re-order
  estonia_plot$category = factor(estonia_plot$category,
                                levels = c('Gender', 'Education', 'Manager status',
                                           'Institution level', 'Age group', 'Tenure group'))
  
  
  # Plot
  g1 = ggplot() +
    
    geom_bar(data = estonia_plot,
             aes(x = label, y = share, group = as.character(value), fill = as.character(value)),
             col = 'black',
             stat = 'identity', position = position_dodge(.77), width = .77) +
    
    facet_wrap(~category, scales = 'free') +
    
    scale_fill_manual(name = '', 
                      values = c('#CFF7FA','#B4DDFA','#98CEF5','#5DB5F5','#209BF5')) +
    guides(fill = 'none') +
    
    scale_y_continuous(expand = expansion(mult=c(0,.2), add=c(0,0)),
                       labels = percent_format()) + 
    
    labs(x = '', y = 'Share of respondents\n',
         title = paste0('<br>Responses to <b>"',
                        gsub('\\.', '', unique(dict1$question_text[dict1$question_var == var1])),
                        '"</b>  question<br><br>'),
         caption = paste0("<br><b>Notes:</b> All response categories are measured using 1-5 Likert scale
                        from <b>1 - 'strongly disagree'</b> to <b>5 - 'strongly agree</b>'.
                        Question only asked to respondents assigned to Track 2 (N = 2,300) of the questionnaire.
                        The shares are calculated excluding Track 1 respondents and 
                        the missing responses for Track 2")) +
    bar_theme +
    theme(
      strip.text = element_text(size = 28, color = 'grey15'),
      plot.title = element_markdown(color = 'black', size = 31, hjust = 0.5),
      plot.caption= element_textbox_simple(face = 'plain', color = 'grey30', size = 12, hjust = 0),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 17)
    )
  
  
  # Save
  ggsave(plot = g1, file.path(main_dir, 'Figures', paste0('GSPS core demo (', var1,').png')),
         width = 50, height = 27, unit = 'cm')
}




### plot | left-right by party -----------------------------------------------------------------------------------------------------------------


g1=ggplot(estonia, aes(x=lr))+
  geom_histogram()+
  facet_wrap(~party2023, scales='free_y')


# save
ggsave(plot = g1, file.path(main_dir, 'Figures', 'Left-right by party (draft).png'),
       width = 67, height = 40, unit = 'cm')




### plot | party thermometers -----------------------------------------------------------------------------------------------------------------
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




# ' ----------------------------------------------------------------------------------------------------------------------------------------
### map | EE pop density ---------------------------------------------------------------------------------------------------------------------
pop_estonia = read_sf(file.path(main_dir, 'data', 'spatial', 'pop_grid_1km_baltic.gpkg')) %>% 
  clean_names() %>% 
  filter(str_detect(nuts2021_3, 'EE'))

names(pop_estonia)

mid1 = mean_miss(log(pop_estonia$tot_p_2006[pop_estonia$tot_p_2006>0]))

pop_estonia <- st_transform(pop_estonia, 4326)
# pop_estonia = st_intersection(pop_estonia, estonia_lg_sf) # takes too long, leave Peipus for now


g1 = ggplot()+
  geom_sf(data = pop_estonia, aes(fill = log(tot_p_2006+.01)),
          color = NA)+
  
  scale_fill_viridis(name = 'Population 2006 (log)',
                     option = 'magma',
                     breaks = round(seq(from = min(log(pop_estonia$tot_p_2006+.01)),
                                        to   = max(log(pop_estonia$tot_p_2006+.01)),
                                        length.out=10), 1))+
  guides(fill = guide_colorbar(title.position = 'top')) +
  # scale_fill_gradient2(name = '', # 'share',
  #                      low = "darkblue", high = "#EC5F06",
  #                      mid = "#F2E5E1", 
  #                      midpoint = 3,
  #                      # midpoint = .25
  # ) +
  labs(
    # title = str_wrap_br('Population density in Estonia (2006)', 60),
    caption = '<br><b>Note:</b> Full geographic boundaries, including internal waters included. Logarithm of zero values
    taken by adding 0.1 constant.<br>
    <br><b>Source:</b> Eurostat (Population);.<br>'
  )+
  map_theme+
  theme(
    plot.caption = element_textbox_simple(size = 12),
    
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 18),
    legend.key.width = unit(3.8, 'cm'),
    legend.key.height = unit(.6, 'cm')
  )


ggsave(g1, width = 30, height = 20, unit = 'cm',
       # file = file.path('figures', 'aggregate', 'Finnish TV watchers by referent.png'))
       file = file.path('figures', 'Estonia pop density (2006).png'))



# ' --------------------------------------------------------------------------------------------------------------------------------------------------
#  SCRAPBOOK ------------------------------------------------------------------------------------------------------------------------------------


### map | BW part. by 89' LG ----------------------------------------------------------------------------------------------------------------------------------------
estonia[, bw2_bw2_protest_post_dummy := fcase(
  str_detect(bw2_bw2_protest_post, '^nobody'), 'no one in the family',
  is.na(bw2_bw2_protest_post), 'no response',
  default = 'anyone in the family'
)]

pr_na(estonia$bw2_bw2_protest_post_dummy) # nice confirmation that 1 in 4 (27% here) of Estonians took part in the BW (I appreciate it's wide net, as it's not only 'you;)


dta_plot = estonia %>% count(bw5_lg_any, bw5_locality_any, bw2_bw2_protest_post_dummy) %>% 
  rename(lg = bw5_lg_any) %>% left_join(., estonia_lg_sf) %>% st_as_sf() %>% 
  mutate(n_rel = 1000*n/population)


g1 = ggplot(dta_plot  %>% filter(!bw2_bw2_protest_post_dummy %in% c('no response')), 
            aes(fill = n_rel))+
  geom_sf(color = 'black')+
  facet_wrap(~bw2_bw2_protest_post_dummy) +
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
    legend.key.width = unit(.8, 'cm'),
    legend.key.height = unit(3.6, 'cm')
  )


ggsave(g1, width = 35, height = 18, unit = 'cm',
       file = file.path('figures', 'aggregate', 'Finnish TV watchers.png'))



# ' --------------------------------------------------------------------------------------------------------------------------------------------------
# ACTA EST FABULA -------------------------------------------------------------------------------------------------------------------------
#