
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





# ' ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# QUALTRICS AREA NAMES --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#

## NOTE: explore which datasets can be used as inputs into Baltic Way Qualtrics survey for respondents to select places of residence
## from county > municipality > locality drop-down

### read the data ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## NOTE: needs to be .xlsx as .csv also reads Estonian diacritics incorrectly, but all as ???
estonia_save = read.xlsx(file.path(main_dir, 'data', 'raw', 'postal_codes.xlsx')) %>% clean_names()




### clean
estonia = estonia_save  %>% 
  select(4:6) %>% distinct %>% rename(county = 1, lg = 2, settlement = 3) %>%
  mutate(across(everything(), ~trimws(.)))

### clean incorrectly read Estonian diacritics  ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
estonia = estonia %>% 
  mutate(across(everything(), ~gsub( "Ã¼", "ü",   .)),
         across(everything(), ~gsub( "Ã¤", "ä",  .)),
         across(everything(), ~gsub( "Ãµ", "õ",   .)),
         across(everything(), ~gsub( "Ã¶", "ö",   .)),
         across(everything(), ~gsub( "Ã¥", "å",   .)), # very rare (few instances) but still
         
         across(everything(), ~gsub( "Ãœ", "Ü",   .)),
         across(everything(), ~gsub( "Ã„", "Ä",   .)),
         across(everything(), ~gsub( "Ã•", "Õ",   .)),
         across(everything(), ~gsub( "Ã–", "Ö",   .)),
         )


# if city, then it's good enough and proceed 
estonia$settlement = ifelse(grepl('linn', estonia$lg), estonia$lg, estonia$settlement)



### save county ---------------------------------------------------------------------------------------------------------------------------------------------------
estonia_county = estonia %>%
  select(c(county)) %>% distinct %>% 
  add_row(county = "-EELISTAN MITTE VASTATA / Я ПРЕДПОЧИТАЮ НЕ ОТВЕЧАТЬ / PREFER NOT TO SAY", .before = 1)

fwrite(estonia_county,
       col.names = F, # don't include header for Qualtrics
       file.path(main_dir, 'data', 'estonia_counties.csv'),
       encoding = 'UTF-8', row.names=F)


### leave only clean of LG + settlement ----------------------------------------------------------------------------------------------------------------------------
estonia = estonia %>% select(c(lg, settlement)) %>% distinct # no county as Qualtrics limits cell count to <10,000 

# order alphabetically (IMPORTANT!)
estonia = estonia %>% arrange(lg, settlement)

#### + PF options on top of each group  ------------------------------------------------------------------------------------------------------------------

estonia_now = estonia %>%
  group_by(lg) %>%
  summarise(settlement = c("-PREFER NOT TO SAY", settlement), .groups = "drop") %>% 
  add_row(lg = "-PREFER NOT TO SAY", settlement = "-PREFER NOT TO SAY", .before = 1)

estonia_now

### version for Baltic Way reside ----------------------------------------------------------------------------------------------------------------
# Don't Know + Not alive at the time (this one only for LG - you either are or aren't alive, but you can remember LG but not locality)
estonia_bw = estonia %>%
  group_by(lg) %>%
  summarise(settlement = c('-PREFER NOT TO SAY',"-DON'T KNOW", settlement), .groups = "drop") %>% 
  add_row(lg = "-NOT ALIVE AT THE TIME", settlement = "-NOT ALIVE AT THE TIME", .before = 1) %>% 
  add_row(lg = "-DON'T KNOW", settlement = "-DON'T KNOW", .before = 1) %>% 
  add_row(lg = "-PREFER NOT TO SAY", settlement = "-PREFER NOT TO SAY", .before = 1)
  

### save (ENG) ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fwrite(estonia_now,
       col.names = F, # don't include header for Qualtrics
       file.path(main_dir, 'data', 'clean', 'qualtrics_input', 'estonia_lgs_qualtrics_ENG.csv'),
       encoding = 'UTF-8', row.names=F)

fwrite(estonia_bw,
       col.names = F, # don't include header for Qualtrics
       file.path(main_dir, 'data',  'clean', 'qualtrics_input', 'estonia_lgs_1991_qualtrics_ENG.csv'),
       encoding = 'UTF-8', row.names=F)


### save (EST/RUS) ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# NOTE: since there are just few cells with actual text, save common EST/RUS version to save space

estonia_now_est = estonia_now %>%
  mutate(across(everything(), ~ ifelse(grepl("-PREFER NOT TO SAY", ., ignore.case = TRUE), "-EELISTAN MITTE VASTATA / Я ПРЕДПОЧИТАЮ НЕ ОТВЕЧАТЬ / PREFER NOT TO SAY", .)))
  

estonia_bw_est = estonia_bw %>% 
  mutate(across(everything(), ~ ifelse(grepl("-DON'T KNOW", ., ignore.case = TRUE), "-EI TEA / НЕ ЗНАЮ / DON'T KNOW", .)),
         across(everything(), ~ ifelse(grepl("-PREFER NOT TO SAY", ., ignore.case = TRUE), "-EELISTAN MITTE VASTATA / Я ПРЕДПОЧИТАЮ НЕ ОТВЕЧАТЬ / PREFER NOT TO SAY", .)),
         across(everything(), ~ ifelse(grepl("-NOT ALIVE AT THE TIME", ., ignore.case = TRUE), "-SEL AJAL ELUS EI OLNUD / НЕ ЖИЛ В(А) ТО ВРЕМЯ / NOT ALIVE AT THE TIME", .))) 


fwrite(estonia_now_est,
       col.names = F, # don't include header for Qualtrics
       file.path(main_dir, 'data', 'clean', 'qualtrics_input', 'estonia_lgs_qualtrics_EST_RUS.csv'),
       encoding = 'UTF-8', row.names=F)

fwrite(estonia_bw_est,
       col.names = F, # don't include header for Qualtrics
       file.path(main_dir, 'data', 'clean', 'qualtrics_input', 'estonia_lgs_1991_qualtrics_EST_RUS.csv'),
       encoding = 'UTF-8', row.names=F)



### (*) checks -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

### checks > how many units per LG
## NOTE: too many - Saaremaa > 400, a few > 100, many >50
tapply(estonia$settlement, estonia$lg, funique)
temp = estonia %>% group_by(lg) %>% summarise(n = fdistinct(settlement))
hist(temp$n)

temp$n %>% sf

### checks > only normal letters + standard interpunction?
# all_text <- paste(temp$settlement, collapse = "")
# chars <- strsplit(all_text, "")[[1]]
# unique_chars <- sort(unique(tolower(chars)))
# unique_chars







#
# ACTA EST FABULA ----------------------------------------------------------------------------------------------------
#