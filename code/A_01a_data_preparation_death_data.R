# This code prepares the death data for estimating average death rates between 
# rural and urban areas
# Load packages
source("code/packages_functions.R")
source("code/A_01b_NUTS_translation.R")

# Load cause of death data
d_cod <- 
  read_excel("data_input/Todesursachen_HKE_AlterG_Geschl_Kreis_2012_2018e_anonymisiert_0622.xlsx",
             range = "A1:E22401") %>%
  filter(AlterG90 > 60) %>% 
  rename(Age = AlterG90, Region = RegKreis, dia = HKE) %>%
  mutate(sex = ifelse(Geschl == 1, "m", "w"),
         D = as.numeric(D)) %>%
  rowwise() %>% 
  mutate(D_mis = ifelse(is.na(D), 1, 0),
         D = ifelse(is.na(D), 
                    sample(x = c(0,1,2), size = 1), 
                    D)) %>%
  group_by(Age, sex, Region) %>%
  mutate(Dx_tot = sum(D)) %>% 
  filter(dia == 1) %>% 
  rename(Dx_MI = D) %>% 
  select(Age, sex, Region, Dx_MI, Dx_tot) 

# Load CVD cause of death data
d_CVD <- 
  read_excel("data_input/DE_Kreise_HKE_AlterG_Geschl_2012-2018.xlsx",
             range = "A1:D4801") %>%
  rename(Age = Alter, Region = Kreis, sex = Geschl, Dx_CVD = D) %>%
  mutate(sex = ifelse(sex == 1, "m", "w"))

# Add in-hospital deaths (first and secondary diagnosis)
# Load hospital deaths based on primary and secondary
h_dx_sec <- 
  read_sas("data_input/ebeling_deathsops_sec_0822_a.sas7bdat") %>% 
  filter(!is.na(ags2017kreis)) %>%
  rename(Region = ags2017kreis, dia = icd_hd3NEW, 
         Age = alterGRP_5) %>% 
  # Adjust for Eisenach\Wartburgkreis
  mutate(Region = ifelse(Region == 16056, 16063, Region)) %>%  
  select(!yearGRP) %>% 
  group_by(sex, Age, Region, dia, OPS) %>% 
  summarise(Cases = sum(Cases))


# Create data sets with all possible combinations
h_d_sec <- 
  expand_grid(Region = unique(h_dx_sec$Region), Age = seq(65, 90, by = 5),
              dia = 0:1, sex = c("m", "w"), OPS = 0:1) %>% 
  left_join(h_dx_sec) %>% 
  group_by(Region, Age, sex, dia) %>% 
  mutate(Dx_unadj_dia = sum(Cases, na.rm = TRUE)) %>%
  rowwise() %>%
  filter(dia == 1) %>% 
  # Calculate maximum possible deaths, 
  # given that n < 3 are deleted from data
  mutate(Case_sim = case_when(is.na(Cases) ~ sample(size = 1, x = c(0,1,2)),
                              !is.na(Cases) ~ Cases)) %>% 
  group_by(Region, Age, sex) %>% 
  summarise(Dx_hosp_MI_sec = sum(Case_sim, na.rm = TRUE))

# Merge death data 
d_cod <- 
  d_cod %>% 
  left_join(d_CVD) %>% 
  left_join(h_d_sec)


# Load and prepare data for average population
# Load Population Data
# Stadt Eisenach 16056 included in Wartburgkreis 	16063
pop <- read_delim("data_input/population_counts2011_2018.csv", 
                  delim = ";", skip = 7, col_names = FALSE, 
                  locale = locale(encoding = 'ISO-8859-1'), n_max = 90391-7)

pop1 <- 
  pop %>% 
  rename(Year = X1, Code = X2, Region = X3, Age = X4, 
         Total = X5, Men = X6, Women = X7) %>%
  select(Year, Age, Code, Men, Women) %>% 
  mutate(Year = as.numeric(substr(Year,7, 10)))

ageRecode <- cbind(unique(pop1$Age),
                   c(seq(0, 90, by = 5), 90, NA))


# test <- 
#   pop1 %>% 
#   mutate(Age1 = mapvalues(Age, 
#                           from = ageRecode[,1],
#                           to = ageRecode[,2]), 
#          NUTS_3 = as.numeric(Code), 
#          NUTS_3 = case_when(NUTS_3 == 2 ~ 2000,
#                             NUTS_3 == 11 ~ 11000,
#                             !NUTS_3 %in% c(2,11) ~ NUTS_3), 
#          Men1 = as.numeric(Men),
#          Women1 = as.numeric(Women))
# 
# as.data.frame(test %>% distinct(Code, NUTS_3))
# as.data.frame(test %>% filter(is.na(Men1)))
# 

pop1 <- 
  pop1 %>% 
  mutate(Age1 = mapvalues(Age, 
                          from = ageRecode[,1],
                          to = ageRecode[,2]), 
         NUTS_3 = as.numeric(Code), 
         NUTS_3 = case_when(NUTS_3 == 2 ~ 2000,
                            NUTS_3 == 11 ~ 11000,
                            !NUTS_3 %in% c(2,11) ~ NUTS_3), 
         Men = as.numeric(Men),
         Women = as.numeric(Women)) %>% 
  filter(!is.na(NUTS_3) & Age1 >= 65 & !is.na(Women)) %>% 
  left_join(nuts_codes_harm) %>%
  # Merge Eisenach (16056) to Wartburgkreis (16063)
  mutate(ags2017kreis = ifelse(ags2017kreis == 16056, 16063, ags2017kreis)) %>% 
  # Data incldues also state level, Regierungsbezirk, and some historical NUTS-3 regions
  # Excluded by condition !is.na(ags2017kreis)
  # test <- pop1 %>% filter(is.na(ags2017kreis))
  filter(!is.na(ags2017kreis)) %>% 
  group_by(Year, Age1, ags2017kreis) %>% 
  summarise(Men = sum(Men), 
            Women = sum(Women))

# Calculate average population for years 2012 to 2018
# x <- pop_mean %>% filter(Age1 == 65 & Sex == "Men", ags2017kreis == 1001)

average_pop <- function(x){
  p <- x$P
  Nx <- sum((p[-1]+p[-length(p)])/2)
  out <- tibble(sex = x$Sex[1], ags2017kreis = x$ags2017kreis[1], 
                Age = as.numeric(x$Age1[1]), Nx)
  return(out)
}

pop_mean <- 
  pop1 %>% 
  pivot_longer(cols = c(Men, Women), names_to = "Sex", values_to = "P") %>% 
  group_by(Age1,ags2017kreis, Sex) %>% 
  group_map(~average_pop(x=.), .keep = TRUE) %>% 
  bind_rows() %>% 
  mutate(sex = case_when(sex == "Men" ~ "m",
                         sex == "Women" ~ "w")) %>% 
  rename(Region = ags2017kreis)


# Merge death data and population data
death_dat <- d_cod %>% left_join(pop_mean)


# Load data with regional characteristics 
nuts3 <- 
  read_excel("data_input/kreis-kreisregionen-2017.xlsx",
             range = "A3:AK404") %>%
  select(krs17, fl17, bev17, ktyp4...16, slraum...18, raumt2010lage_kreis) %>% 
  rename(Region1 = krs17, area = fl17, kreis_t_sett = ktyp4...16, 
         RurUrb = slraum...18, kreis_t_pos = raumt2010lage_kreis) %>%
  # Edit Berlin, Hamburg and Eisenach/Wartburgkreis
  mutate(Region = case_when(nchar(Region1) == 8 ~ as.numeric(substr(Region1, 1,5)),
                            nchar(Region1) == 7  & Region1 !=  11000000 ~ as.numeric(substr(Region1, 1,4)),
                            nchar(Region1) == 5 ~ 2000,
                            as.integer(Region1) == 11000000 ~ 11000)) %>% 
  mutate(Region = ifelse(Region == 16056, 16063, Region)) %>% 
  group_by(Region, kreis_t_sett, kreis_t_pos, RurUrb) %>% 
  summarise(area = sum(area),
            bev17 = sum(bev17))

# Merge Regional data to death data
death_dat <- death_dat %>% left_join(nuts3)
save(death_dat, file = "data_inter/death_rates_data_region.RData")

