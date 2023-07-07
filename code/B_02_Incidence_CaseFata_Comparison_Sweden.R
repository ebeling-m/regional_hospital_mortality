# Comparison to Sweden total population 
source("code/packages_functions.R")

# German dataset
load("data_inter/hospCase_deaths.RData")

# Calculate total number of cases without considering OPS
# Dx_adj_dia: Deaths from MI in hospital
# Dx_CoD: deaths from MI as underlying cause of death
# C_adj: Hospitalizations from MI

h_adj <- 
  hosp %>%
  mutate(Age = ifelse(Age >= 85, 85, Age)) %>% 
  group_by(Age, sex) %>% 
  summarise(Dx_MI = sum(Dx_MI),
            Dx_MI_hosp_prim = sum(Dx_MI_hosp_prim),
            Dx_hosp_MI_sec = sum(Dx_hosp_MI_sec),
            C_hosp_MI = sum(C_hosp_MI),
            Nx = sum(Nx)) %>% 
  ungroup() %>% 
  # incidence, case fatality
  mutate(cfat = (Dx_MI-Dx_MI_hosp_prim+Dx_hosp_MI_sec) / (C_hosp_MI+(Dx_MI-Dx_MI_hosp_prim)),
         inc = (C_hosp_MI+(Dx_MI-Dx_MI_hosp_prim)) / Nx,
         mx = Dx_MI / Nx)
  
# Load Swedish data
swe <- 
  read_delim("data_input/comparison_estimates_Sweden.csv", delim = ";", 
             skip = 1) %>% 
  mutate(Age = mapvalues(Age,
                         from = unique(Age), 
                         to = c(seq(65, 85, by = 5), 99))) %>% 
  filter(Age < 99) %>%
  pivot_longer(cols = `2012`:`2018`, names_to = "Year")

recodeIndi <- cbind(unique(swe$Indicator),
                    c("cfat_28", "cfat_365",
                      "IR", "mx"))
swe <- 
  swe %>% 
  mutate(Indicator = mapvalues(Indicator, 
                               from = recodeIndi[,1],
                               to = recodeIndi[,2])) %>% 
  group_by(Indicator, Sex, Age) %>% 
  mutate(mean_value = mean(value))

# Calculate rates for hospitalized cases with AMI as principal diagnosis 
swe_pri <-   
  read_delim("data_input/comparison_swe_principalDiagnosis.csv", delim = ";", 
                        skip = 1) %>% 
  mutate(Age = mapvalues(Age,
                         from = unique(Age), 
                         to = c(seq(65, 85, by = 5), 99))) %>% 
  filter(Age < 99) %>% 
  rename(cases = `Entire Sweden`)

swe_pop <- 
  as_tibble(read.table("data_input/averagePopulation_Sweden_singleAges.txt", 
             skip = 2, header = TRUE, stringsAsFactors = FALSE)) %>% 
  filter(Year %in% c(2012:2018) & Age %in% c(65:109, "110+")) %>% 
  mutate(Age2 = mapvalues(Age, 
                         from = c(65:109, "110+"),
                         to = c(rep(seq(65, 80, by = 5), each = 5), 
                                rep(85, length(85:110))))) %>% 
  group_by(Year, Age2) %>% 
  summarise(Men = sum(Male),
            Women = sum(Female)) %>% 
  rename(Age = Age2) %>% 
  pivot_longer(cols = c(Men, Women), names_to = "Sex", values_to = "Pop")

swe_pri <- 
  swe_pri %>% 
  left_join(swe_pop) %>% 
  group_by(Sex, Age) %>% 
  summarise(hosp_pri = sum(cases)/sum(Pop))


# Plot results for incidence and cfat

# dev.off()
pdf("figures/MI_Median_incidence_CaseFatality_Sweden.pdf", family = "Times", 
    width = 14, pointsize = 17, height = 14)
layout(cbind(c(1,3), c(2,4)))

# Women
# MI events
par(mar = c(4, 4, 5, 2) + 0.1, xpd = TRUE)

plot(x=1, y=1, typ = "n", ylim = c(0.002, 0.036), xlim = c(0, 5), 
     xlab = NA, ylab = NA, bty = "n", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i", log = "y")

par(las = 1)
# yy <- seq(0.002, 0.036, by = 0.006)
yy <- c(0.002, 0.006, 0.01, 0.02, 0.036)
axis(2, at = yy, 
     labels = round(yy*10000,0), lwd = 0, pos = 0.08, cex.axis = 1.2)

segments(x0=0:5, y0=0.002,x1=0:5, y1=0.04, 
         col = "lightgray")
segments(x0=0, y0=yy, x1=5, y1=yy, 
         col = "lightgray")
par(las = 0)
mtext("Rate per 10000 PY", 2, cex = 1.3, line = 2.8)

# mtext("Age group", 1, cex = 1.3, line = 3)
axis(1, at = seq(0.5, 4.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84",
                "85+"), lwd = 0, cex.axis = 1.2)
mtext("Women", 3, line = 1, cex = 1.4, adj = 0.01)
# Women
swe %>% 
  filter(Sex == "Women" & Indicator == "IR") %>%
  distinct(Age, mean_value) %>%
  group_by(Sex) %>% 
  group_map(~lines(x=seq(0.5, 4.5, by=1), y=.$mean_value/100000,
                   typ = "l", lwd = 3, col = 'darkgray', 
                   lty = 1))

swe %>% 
  filter(Sex == "Women" & Indicator == "IR") %>%
  distinct(Age, mean_value) %>% 
  group_by(Sex) %>% 
  group_map(~lines(x=seq(0.5, 4.5, by=1), y=.$mean_value/100000,
                   typ = "p", lwd = 2, col = 'darkgray', 
                   pch = 16, cex = 1.5))

swe_pri %>% 
  filter(Sex == "Women") %>%
  group_by(Sex) %>% 
  group_map(~lines(x=seq(0.5, 4.5, by=1), y=.$hosp_pri,
                   typ = "l", lwd = 3, col = 'steelblue', 
                   lty = 1))

swe_pri %>% 
  filter(Sex == "Women") %>%
  group_by(Sex) %>% 
  group_map(~lines(x=seq(0.5, 4.5, by=1), y=.$hosp_pri,
                   typ = "p", lwd = 2, col = 'steelblue', 
                   pch = 16, cex = 1.5))

wom <- h_adj %>% filter(sex == "w")
lines(x=seq(0.5, 4.5, by=1), y=wom$inc, typ = "p", 
      pch = 16, cex = 1.5, col = 'black')
lines(x=seq(0.5, 4.5, by=1), y=wom$inc, typ = "l", 
      lwd = 3, col = 'black', lty = 1)

legend("topleft", legend = c("Incidence rate Sweden", 
                              "Rate hosp. AMI (primary diag.) Sweden",
                              "Rate of incident AMI cases Germany"), 
       col = c("darkgray", "steelblue", "black"), pch = 15, cex =1.3, 
       bty = "n")


# MI deaths
plot(x=1, y=1, typ = "n", ylim = c(0, 0.8), xlim = c(0, 5), 
     xlab = NA, ylab = NA, bty = "n", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i")
par(las = 1)
yy <- seq(0, 0.8, by = 0.1)
axis(2, at = yy, 
     labels = yy, lwd = 0, pos = 0.08, cex.axis = 1.2)

segments(x0=0:5, y0=0,x1=0:5, y1=0.8, 
         col = "lightgray")
segments(x0=0, y0=yy,x1=5, y1=yy, 
         col = "lightgray")

par(las = 0)
mtext("Proportion of deaths", 2, cex = 1.3, line = 3)
# mtext("Age group", 1, cex = 1.3, line = 3)
axis(1, at = seq(0.5, 4.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84",
                "85+"), lwd = 0, cex.axis = 1.2)
# Women

swe %>% 
  filter(Sex == "Women" & Indicator == "cfat_28") %>%
  distinct(Age, mean_value) %>%
  group_by(Sex) %>% 
  group_map(~lines(x=seq(0.5, 4.5, by=1), y=.$mean_value/100,
                   typ = "l", lwd = 3, col = 'darkgray', 
                   lty = 1))

swe %>% 
  filter(Sex == "Women" & Indicator == "cfat_28") %>%
  distinct(Age, mean_value) %>% 
  group_by(Sex) %>% 
  group_map(~lines(x=seq(0.5, 4.5, by=1), y=.$mean_value/100,
                   typ = "p", lwd = 2, col = 'darkgray', 
                   pch = 16, cex = 1.5))


swe %>% 
  filter(Sex == "Women" & Indicator == "cfat_365") %>%
  distinct(Age, mean_value) %>%
  group_by(Sex) %>% 
  group_map(~lines(x=seq(0.5, 4.5, by=1), y=.$mean_value/100,
                   typ = "l", lwd = 3, col = 'steelblue', 
                   lty = 1))

swe %>% 
  filter(Sex == "Women" & Indicator == "cfat_365") %>%
  distinct(Age, mean_value) %>% 
  group_by(Sex) %>% 
  group_map(~lines(x=seq(0.5, 4.5, by=1), y=.$mean_value/100,
                   typ = "p", lwd = 2, col = 'steelblue', 
                   pch = 16, cex = 1.5))


wom <- h_adj %>% filter(sex == "w")
lines(x=seq(0.5, 4.5, by=1), y=wom$cfat, typ = "p", 
      pch = 16, cex = 1.5, col = 'black')
lines(x=seq(0.5, 4.5, by=1), y=wom$cfat, typ = "l", 
      lwd = 3, col = 'black', lty = 1)

legend("topleft", legend = c("Case fatality 28 days Sweden", 
                             "Case fatality 365 days Sweden",
                             "Proportion of deaths Germany"), 
       col = c("darkgray", "steelblue", "black"), pch = 15, cex =1.3, 
       bty = "n")


# Men PLot
par(mar = c(7, 4, 2, 2) + 0.1)

plot(x=1, y=1, typ = "n", ylim = c(0.002, 0.036), xlim = c(0, 5), 
     xlab = NA, ylab = NA, bty = "n", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i", log = "y")

par(las = 1)
# yy <- seq(0.002, 0.036, by = 0.006)
yy <- c(0.002, 0.006, 0.01, 0.02, 0.036)
axis(2, at = yy, 
     labels = round(yy*10000,0), lwd = 0, pos = 0.08, cex.axis = 1.2)

segments(x0=0:5, y0=0.002,x1=0:5, y1=0.04, 
         col = "lightgray")
segments(x0=0, y0=yy, x1=5, y1=yy, 
         col = "lightgray")
par(las = 0)
mtext("Rate per 10000 PY", 2, cex = 1.3, line = 2.8)

# mtext("Age group", 1, cex = 1.3, line = 3)
axis(1, at = seq(0.5, 4.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84",
                "85+"), lwd = 0, cex.axis = 1.2)
mtext("Men", 3, line = 1, cex = 1.4, adj = 0.01)
# Men
swe %>% 
  filter(Sex == "Men" & Indicator == "IR") %>%
  distinct(Age, mean_value) %>%
  group_by(Sex) %>% 
  group_map(~lines(x=seq(0.5, 4.5, by=1), y=.$mean_value/100000,
                   typ = "l", lwd = 3, col = 'darkgray', 
                   lty = 1))

swe %>% 
  filter(Sex == "Men" & Indicator == "IR") %>%
  distinct(Age, mean_value) %>% 
  group_by(Sex) %>% 
  group_map(~lines(x=seq(0.5, 4.5, by=1), y=.$mean_value/100000,
                   typ = "p", lwd = 2, col = 'darkgray', 
                   pch = 16, cex = 1.5))

swe_pri %>% 
  filter(Sex == "Men") %>%
  group_by(Sex) %>% 
  group_map(~lines(x=seq(0.5, 4.5, by=1), y=.$hosp_pri,
                   typ = "l", lwd = 3, col = 'steelblue', 
                   lty = 1))

swe_pri %>% 
  filter(Sex == "Men") %>%
  group_by(Sex) %>% 
  group_map(~lines(x=seq(0.5, 4.5, by=1), y=.$hosp_pri,
                   typ = "p", lwd = 2, col = 'steelblue', 
                   pch = 16, cex = 1.5))

wom <- h_adj %>% filter(sex == "m")
lines(x=seq(0.5, 4.5, by=1), y=wom$inc, typ = "p", 
      pch = 16, cex = 1.5, col = 'black')
lines(x=seq(0.5, 4.5, by=1), y=wom$inc, typ = "l", 
      lwd = 3, col = 'black', lty = 1)

# MI deaths
plot(x=1, y=1, typ = "n", ylim = c(0, 0.8), xlim = c(0, 5), 
     xlab = NA, ylab = NA, bty = "n", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i")
par(las = 1)
yy <- seq(0, 0.8, by = 0.1)
axis(2, at = yy, 
     labels = yy, lwd = 0, pos = 0.08, cex.axis = 1.2)

segments(x0=0:5, y0=0,x1=0:5, y1=0.8, 
         col = "lightgray")
segments(x0=0, y0=yy,x1=5, y1=yy, 
         col = "lightgray")

par(las = 0)
mtext("Proportion of deaths", 2, cex = 1.3, line = 3)
# mtext("Age group", 1, cex = 1.3, line = 3)
axis(1, at = seq(0.5, 4.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84",
                "85+"), lwd = 0, cex.axis = 1.2)
# Men

swe %>% 
  filter(Sex == "Men" & Indicator == "cfat_28") %>%
  distinct(Age, mean_value) %>%
  group_by(Sex) %>% 
  group_map(~lines(x=seq(0.5, 4.5, by=1), y=.$mean_value/100,
                   typ = "l", lwd = 3, col = 'darkgray', 
                   lty = 1))

swe %>% 
  filter(Sex == "Men" & Indicator == "cfat_28") %>%
  distinct(Age, mean_value) %>% 
  group_by(Sex) %>% 
  group_map(~lines(x=seq(0.5, 4.5, by=1), y=.$mean_value/100,
                   typ = "p", lwd = 2, col = 'darkgray', 
                   pch = 16, cex = 1.5))


swe %>% 
  filter(Sex == "Men" & Indicator == "cfat_365") %>%
  distinct(Age, mean_value) %>%
  group_by(Sex) %>% 
  group_map(~lines(x=seq(0.5, 4.5, by=1), y=.$mean_value/100,
                   typ = "l", lwd = 3, col = 'steelblue', 
                   lty = 1))

swe %>% 
  filter(Sex == "Men" & Indicator == "cfat_365") %>%
  distinct(Age, mean_value) %>% 
  group_by(Sex) %>% 
  group_map(~lines(x=seq(0.5, 4.5, by=1), y=.$mean_value/100,
                   typ = "p", lwd = 2, col = 'steelblue', 
                   pch = 16, cex = 1.5))

wom <- h_adj %>% filter(sex == "m")
lines(x=seq(0.5, 4.5, by=1), y=wom$cfat, typ = "p", 
      pch = 16, cex = 1.5, col = 'black')
lines(x=seq(0.5, 4.5, by=1), y=wom$cfat, typ = "l", 
      lwd = 3, col = 'black', lty = 1)

dev.off()



