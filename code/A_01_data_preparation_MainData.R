# frequency of hospitalizations
source("code/packages_functions.R")

# Load Hopsital case data
h_case <- 
  read_sas("data_input/ebeling_casesops_0622_anonym.sas7bdat") %>% 
  filter(!is.na(ags2017kreis)) %>%
  rename(Region = ags2017kreis, dia = icd_hd3NEW, 
         Age = alterGRP_5) %>% 
  # Adjust for Eisenach\Wartburgkreis
  mutate(Region = ifelse(Region == 16056, 16063, Region)) %>%  
  select(!yearGRP) %>% 
  group_by(sex, Age, Region, dia, OPS) %>% 
  summarise(Cases = sum(Cases))

h_case_adj <- 
  expand_grid(Region = unique(h_case$Region), sex = c("m", "w"), 
              Age = seq(65, 90, by = 5), dia = 1:3, OPS = 0:1) %>% 
  left_join(h_case) %>% 
  rowwise() %>%  
  mutate(C_adj = ifelse(is.na(Cases), sample(size = 1, c(0,1,2)), Cases)) %>% 
  # Calculate total cases
  filter(dia == 1) %>% 
  group_by(Region, Age, sex) %>% 
  summarise(C_hosp_MI = sum(C_adj))

# Load death data
# Load hospital deaths with MI as primary diagnosis
h_dx <- 
  read_sas("data_input/ebeling_deathsops_0622_anonym.sas7bdat") %>% 
  filter(!is.na(ags2017kreis)) %>%
  rename(Region = ags2017kreis, dia = icd_hd3NEW, 
         Age = alterGRP_5) %>% 
  # Adjust for Eisenach\Wartburgkreis
  mutate(Region = ifelse(Region == 16056, 16063, Region)) %>%  
  select(!yearGRP) %>% 
  group_by(sex, Age, Region, dia, OPS) %>% 
  summarise(Cases = sum(Cases))


# Create data sets with all possible combinations
h_d <- 
  expand_grid(Region = unique(h_dx$Region), Age = seq(65, 90, by = 5),
              dia = 1:3, sex = c("m", "w"), OPS = 0:1) %>% 
  left_join(h_dx) %>% 
  group_by(Region, Age, sex, dia) %>% 
  mutate(Dx_unadj_dia = sum(Cases, na.rm = TRUE)) %>%
  rowwise() %>%
  # Calculate maximum possible deaths, 
  # given that n < 3 are deleted from data
  mutate(Case_sim = case_when(is.na(Cases) ~ sample(size = 1, x = c(0,1,2)),
                              !is.na(Cases) ~ Cases)) %>% 
  group_by(Region, Age, sex, dia) %>% 
  mutate(Dx_adj_dia = sum(Case_sim, na.rm = TRUE)) %>% 
  group_by(Region, Age, sex) %>%
  mutate(Dx_unadj_t = sum(Cases, na.rm = TRUE),
         Dx_adj_t = sum(Case_sim, na.rm = TRUE)) %>% 
  filter(dia == 1) %>% 
  distinct(Region, Age, sex, Dx_adj_dia) %>% 
  rename(Dx_MI_hosp_prim = Dx_adj_dia)


# Load death data
load("data_inter/death_rates_data_region.RData")

# prepare data
Dx_data <- 
  death_dat %>% 
  group_by(Age, sex, Region) %>% 
  mutate(PD = bev17/area,
         PD_cat = cut(PD, 
                      breaks = c(0,300, 1000, 20000), 
                      labels = 1:3, 
                      include.lowest = TRUE))

h_d_cod <- 
  h_d %>% 
  left_join(Dx_data)

# Merge both datasets
hosp <- 
  h_case_adj %>% 
  left_join(h_d_cod)


save(hosp, file = "data_inter/hospCase_deaths.RData")

