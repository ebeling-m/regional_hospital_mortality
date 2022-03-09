# Calculate average death rates total and by cause of death
source("code/A_mx_00_packages_functions.R")
load("data_inter/death_rates_data_region.RData")

# Calculate total number of deaths
# prepare data for total death rates estimation
mx_tot <- 
  death_dat %>% 
  group_by(Age, sex, Region) %>% 
  mutate(Dx_t = sum(D),
         mx = Dx_t/Nx) %>% 
  group_by(sex, Region) %>% 
  distinct(Age, sex, Region, Dx_t, Nx, kreis_t_sett, RurUrb, mx) %>% 
  mutate(e65 = lifetable(mx)) %>% 
  group_by(sex) %>% 
  mutate(e65_cat = cut(e65, 
                       breaks = quantile(e65, probs = c(0,1/3, 2/3, 1)), 
                       labels = 1:3, 
                       include.lowest = TRUE))

# Function performing bootstrap and difference calculation
# x <- mx_tot %>% filter(Age == 65 & sex == "w")
boot_mx_kr_sett <- function(x){
  
  # Select rural urban data
  ru <- x %>% filter(kreis_t_sett == 4)
  ur <- x %>% filter(kreis_t_sett == 1)
  
  # Create set of boootstraped death counts and death rates
  # Calculate mean
  mean_ru <- unlist(Map(1:10000, f = function(x) 
    mean(rpois(n = length(ru$Dx_t), lambda = ru$Dx_t)/ru$Nx)))
  
  mean_ur <- unlist(Map(1:10000, f = function(x) 
    mean(rpois(n = length(ur$Dx_t), lambda = ur$Dx_t)/ur$Nx)))
  
  # Quantile values across relative differences
  distDiff <- quantile((mean_ru/mean_ur)-1, probs = c(0.025, 0.5, 0.975))
  
  # Function
  out <- tibble(Age = x$Age[1], sex = x$sex[1], e65_cat = x$e65_cat[1], 
                mx_low = distDiff["2.5%"],
                mx_med = distDiff["50%"], mx_up = distDiff["97.5%"])
  return(out)
}


mean_mx <- 
  mx_tot %>% 
  group_by(sex, Age, e65_cat) %>% 
  group_map(~boot_mx_kr_sett(x=.), .keep = TRUE) %>% 
  bind_rows()

# Calculate difference without deaths from MI and stroke
mx_wo_cvd <- 
  death_dat %>% 
  group_by(Age, sex, Region) %>% 
  mutate(Dx_t = sum(D),
         mx = Dx_t/Nx) %>%
  filter(dia %in% 3) %>% 
  group_by(sex, Age, Region) %>%
  mutate(D_CVD = sum(D), 
         D_wo_CVD = Dx_t - D_CVD) %>% 
  distinct(Age, sex, Region, Dx_t, Nx, kreis_t_sett, RurUrb, mx, D_CVD, D_wo_CVD) %>% 
  group_by(sex, Region) %>%
  mutate(e65 = lifetable(mx)) %>% 
  group_by(sex) %>% 
  mutate(e65_cat = cut(e65, 
                       breaks = quantile(e65, probs = c(0,1/3, 2/3, 1)), 
                       labels = 1:3, 
                       include.lowest = TRUE))


boot_mx_kr_sett_CVD <- function(x){
  
  # Select rural urban data
  ru <- x %>% filter(kreis_t_sett == 4)
  ur <- x %>% filter(kreis_t_sett == 1)
  
  # Create set of boootstraped death counts and death rates
  # Calculate mean
  mean_ru <- unlist(Map(1:10000, f = function(x) 
    mean(rpois(n = length(ru$D_CVD), lambda = ru$D_CVD)/ru$Nx)))
  
  mean_ur <- unlist(Map(1:10000, f = function(x) 
    mean(rpois(n = length(ur$D_CVD), lambda = ur$D_CVD)/ur$Nx)))
  
  # Quantile values across relative differences
  distDiff <- quantile((mean_ru/mean_ur)-1, probs = c(0.025, 0.5, 0.975))
  
  # Function
  out <- tibble(Age = x$Age[1], sex = x$sex[1], e65_cat = x$e65_cat[1], 
                mx_low = distDiff["2.5%"],
                mx_med = distDiff["50%"], mx_up = distDiff["97.5%"])
  return(out)
}

mean_mx_CVD <- 
  mx_wo_cvd %>% 
  group_by(sex, Age, e65_cat) %>% 
  group_map(~boot_mx_kr_sett_CVD(x=.), .keep = TRUE) %>% 
  bind_rows()

plot1 <- 
  mean_mx %>% 
  filter(sex == "m" & e65_cat == 1)

plot2 <- 
  mean_mx_CVD %>% 
  filter(sex == "m" & e65_cat == 1)

plot(x=plot1$mx_med, y=1:6, typ = "l", lwd = 3, xlim = c(-0.2, 0.2))
abline(v = 0)
lines(x=plot2$mx_med, y=1:6, typ = "l", lwd = 3, col = "blue")
