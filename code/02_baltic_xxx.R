
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
# CLEAN --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#

estonia = fread(file.path('data', 'raw', 'qualtrics_nobs1281.csv')) %>% # read raw .csv file downloaded from Qualtrics
  clean_names() %>% slice(-c(1,2)) %>% # clean column names
  
  setDT %>%  # set as data.table
  
  # cleaning across all columns
  mutate(across(where(~ all(grepl("^\\d*\\.?\\d*$", .x[!is.na(.x)]))), as.numeric), # treat numeric columns as such
         across(where(is.character), ~tolower(.)), # all to lowercase
         across(everything(), ~replace(.x, .x %in% c("" ,"don't know", "prefer not to say"), NA)) # empty string + DK + prefer not to say = NA
         ) %>%  
  
  # rename columns
  rename(
    consent = q2b,
    # q_language = q_language,
    
    gender = q3,
    yob    = q4,
    
    count = residence_county_1,
    lg    = residence_locality_1,
    locality = residence_locality_2,
    
    edu    = q7,
    ethnic = q8,
    ethnic_other = q8_6_text,
    lang_home_now = q9_1,
    lang_home_past = q9_2,
    lang_home_work = q9_3,
    lang_home_other = q9_4,
    satisf_financial = q10,
    satisf_lead_econ = q11_1,
    satisf_lead_eu = q11_2,
    satisf_lead_russia = q11_3,
    satisf_lead_pub_serv = q11_4,
    satisf_lead_ukraine_ref = q11_5,
    satisf_lead_ethnic = q11_6,
    pol_interest_understand = q12_1,
    pol_interest_qualified = q12_2,
    pol_interest_good_job = q12_3,
    pol_interest_have_say = q12_4,
    pol_interest_demo_best = q12_5,
    pol_interest_capt_best = q12_6,
    lr = q13_1,
    party2023 = q14,
    
    fc_party = formal_civic_1,
    fc_religion = formal_civic_2,
    fc_recreation = formal_civic_3,
    fc_environ = formal_civic_4,
    fc_assoc = formal_civic_5,
    fc_charity = formal_civic_6,
    
    ic_talk_friend = informal_civic_1,
    ic_talk_neighbor = informal_civic_2,
    ic_talk_stranger = informal_civic_3,
    ic_go_public = informal_civic_4,
    ic_go_event = informal_civic_5,
    ic_talk_inperson = informal_civic_6,
    
    child_obedience = children_char_1,
    child_hardwork = children_char_2,
    child_responsible = children_char_3,
    child_imagination = children_char_4,
    child_tolerant = children_char_5,
    child_independent = children_char_6,
    child_determine = children_char_7,
    child_religion = children_char_8,
    child_kind = children_char_9,
    
    repress_ww2 = qe_repressions_1_1,
    repress_deport = qe_repressions_1_2,
    repress_prison = qe_repressions_1_3,
    repress_killed = qe_repressions_1_4,
    repress_torture = qe_repressions_1_5,
    repress_discriminate = qe_repressions_1_6,
    repress_seizure = qe_repressions_1_7,
    repress_religion = qe_repressions_1_8,
    repress_surveillance = qe_repressions_1_9,
    
    repress2_open = qe_repressions_2_1,
    
    pol_letter = political_activity_1,
    pol_contact = political_activity_2,
    pol_protest_elections = political_activity_3,
    pol_protest_other = political_activity_4,
    pol_ngo = political_activity_5,
    pol_charity = political_activity_6,
    
    ukr_comment = qe_future_1_1,
    ukr_contact = qe_future_1_2,
    ukr_protest = qe_future_1_3,
    ukr_donate = qe_future_1_4,
    ukr_house = qe_future_1_5,
    ukr_buy = qe_future_1_6,
    ukr_culture = qe_future_1_7,
    ukr_flag = qe_future_1_8,
    ukr_wear = qe_future_1_9,
    ukr_peacekeeping = qe_future_1_10,
    
    
    bw1_proud = qbw_1_1,
    bw1_trust_gov = qbw_1_2,
    bw1_trust_people = qbw_1_3,
    bw1_leave_past = qbw_1_4,
    bw1_soviet = qbw_1_5,
    bw1_eu = qbw_1_6,
    bw1_ukraine_people = qbw_1_7,
    bw1_ukraine_war = qbw_1_8,
    bw1_ref25 = qbw_1_9,
    
    bw2_communist = qbw_2_1,
    bw2_pop_front = qbw_2_2,
    bw2_forest = qbw_2_3,
    bw2_exit = qbw_2_4,
    bw2_radio = qbw_2_5,
    bw2_finnish_tv = qbw_2_6,
    bw2_russian_tv = qbw_2_7,
    bw2_protest_pre = qbw_2_8,
    bw2_protest_bw = qbw_2_9,
    bw2_ref91 = qbw_2_10,
    
    bw3_me = qbw_3_1,
    bw3_father = qbw_3_2,
    bw3_mother = qbw_3_3,
    
    bw4_pos_neg = qbw_4_1,
    bw4_personal = qbw_4_2,
    bw4_discuss = qbw_4_3,
    bw4_unity = qbw_4_4,
    bw4_commemorate = qbw_4_5,
    
    bw5_lg_you = qbw_5a_1,
    bw5_locality_you = qbw_5a_2,
    bw5_lg_parent = qbw_5b_1,
    bw5_locality_parent = qbw_5b_2,
    
    party_th_isamaa = q12_1_2, # Fatherland, conservative, currently leading the pools
    party_th_kesk = q12_2_2, # Centre Part, 3rd in 2023 elections with 15%
    party_th_ekre = q12_3_2, # Conservative People's Party of Estonia - nationalist, right-wing, 2nd in 2023 elections with 16%
    party_th_sde = q12_4_2, # Social Democratic Party; founded 1990 as ESDP under Marju Lauristin, 
    party_th_parem = q12_5_2, # the Right, economic liberals, currently no MPs (founded 2022)
    party_th_eer = q12_6_2, # Greens
    party_th_ee200 = q12_7, # current coalition partner
    party_th_euvp = q12_8, # United Left, pro-EU and progressive
    party_th_reform = q12_9, # Reform, current ruling party, 31% in 2023 elections
    
    ngo_policy1 = outcomes_1_1,
    ngo_policy2 = outcomes_1_2,
    ngo_policy3 = outcomes_1_3,
    ngo_policy4 = outcomes_1_4,
    ngo_policy5 = outcomes_1_5,
    
    ngo_part1 = outcomes_2_1,
    ngo_part2 = outcomes_2_2,
    ngo_part3 = outcomes_2_3,
    ngo_part4 = outcomes_2_4,
    
    donation = donation_1,
    
    ngo_real1 = realistic_outcomes_1,
    ngo_real2 = realistic_outcomes_2,
    ngo_real3 = realistic_outcomes_3,
    ngo_real4 = realistic_outcomes_4,
    ngo_real5 = realistic_outcomes_5,
    
    # treatment_is = treatment_is,
    # treatment_fc = treatment_fc
    
  ) %>% 
  filter(consent == 'yes') %>% 
  filter(attn_check1 == 'red') %>% 
  filter(attn_check2 == 'red') %>% 
  mutate(age = 2025-as.numeric(yob))

names(estonia)[150:170]

table(estonia$bw1_soviet, estonia$bw1_eu, useNA='ifany')

estonia$q12_1_2 %>% hist





### duration
summary(estonia$duration_in_seconds/60)
sf(estonia$duration_in_seconds > 60 & estonia$duration_in_seconds < 15*60)
hist(estonia$duration_in_seconds/60)



### lat-lon
plot(estonia$location_latitude[estonia$location_latitude >55 & estonia$location_latitude < 65],
     estonia$location_longitude[estonia$location_latitude >55 & estonia$location_latitude < 65])


### gender


### age 
estonia$q4 %>% hist



### save --------------------------------------------------------------------------------------------------------------------------------
write_flex(estonia, file.path('data', 'clean', 'baltic_way_survey.csv'), format = format1)


# ' --------------------------------------------------------------------------------------------------------------------------------------------
# SCRAPBOOK -----------------------------------------------------------------------------------------------------------------------------------
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



### plot | part thermometers -----------------------------------------------------------------------------------------------------------------
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
       file = file.path('figures', '00_other', 'party_thermometers.png'))


### *checks -> basic correlations with Finnish TV 
sf(estonia$bw2_finnish_tv)
pr_na(estonia$bw2_finnish_tv)

prop.table(table( estonia$bw1_soviet,  is.na(estonia$bw2_finnish_tv), useNA='ifany'), 2)
prop.table(table( is.na(estonia$bw2_ref91),  is.na(estonia$bw2_finnish_tv), useNA='ifany'), 2)
(table( is.na(estonia$bw2_ref91),  is.na(estonia$bw2_finnish_tv), useNA='ifany'))


table(grepl("nobody|prefer|don't know|^$", estonia$bw2_ref91), 
      grepl("nobody|prefer|don't know|^$", estonia$bw2_finnish_tv), useNA = 'ifany')


#
# ACTA EST FABULA -------------------------------------------------------------------------------------------------------------------------
#