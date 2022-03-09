# This code prepares the death data for estimating average death rates between 
# rural and urban areas
# Load packages
source("code/A_mx_00_packages_functions.R")
source("code/A_mx_01.a_NUTS_Translation.R")

# Load cause of death data
d_cod <- 
  read_excel("data_input/Todesursachen_HKE_AlterG_Geschl_Kreis_2012_2018e_anonymisiert.xlsx",
             range = "A1:E22401") %>%
  filter(AlterG90 > 60) %>% 
  rename(Age = AlterG90, Region = RegKreis, dia = HKE) %>%
  mutate(sex = ifelse(Geschl == 1, "m", "w"),
         D = as.numeric(D)) %>%
  rowwise() %>% 
  mutate(D = ifelse(is.na(D), 
                    sample(x = c(0,1,2), size = 1), 
                    D)) %>% 
  select(Age, sex, Region, dia, D)
unique(d_cod$Region)

# Stadt Eisenach 16056 included in Wartburgkreis 	16063

# Load and prepare data for average population
# Load Population Data
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
                   c(seq(0, 95, by = 5), NA))

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

