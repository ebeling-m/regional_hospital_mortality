# Load external data linked to DRG Stat and prepare region codes
setwd("data_input/")
dat_names <- dir() 
fdz <- 
  dat_names[substr(dat_names, 1, 8) == "external"][1:8] %>% 
  map_df(~read_delim(.)) %>% 
  select(ags2012, ags2013,
         ags2014, ags2015, ags2016, ags2017, 
         ags2018, ags2017kreis)
setwd("..")

nuts_codes_harm <- 
  fdz %>% 
  pivot_longer(cols = c(ags2012, ags2013,
                        ags2014, ags2015, ags2016)) %>% 
  mutate(Year = substr(name, 4, 7)) %>% 
  filter(!is.na(value)) %>% 
  mutate(NUTS_3_17 = case_when(
    nchar(as.integer(ags2017)) == 7 ~ substr(ags2017, 1,4), 
    nchar(as.integer(ags2017)) == 8 ~ substr(ags2017, 1,5)),
    NUTS_3_18 = case_when(
      nchar(as.integer(ags2018)) == 7 ~ substr(ags2018, 1,4), 
      nchar(as.integer(ags2018)) == 8 ~ substr(ags2018, 1,5)),
    NUTS_3 = case_when(
      nchar(as.integer(value)) == 7 ~ substr(value, 1,4), 
      nchar(as.integer(value)) == 8 ~ substr(value, 1,5))) %>% 
  mutate(NUTS_3 = as.numeric(NUTS_3),
         NUTS_3_17 = as.numeric(NUTS_3_17), 
         # Berlin and Hamburg extra coding
         NUTS_3 = case_when(ags2017kreis == 2000 ~ 2000,
                            ags2017kreis == 11000 ~ 11000,
                            !ags2017kreis %in% c(2000, 11000) ~ NUTS_3)) %>% 
  distinct(NUTS_3, ags2017kreis)

