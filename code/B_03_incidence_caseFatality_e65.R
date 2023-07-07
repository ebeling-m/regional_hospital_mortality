# Calculate incidence and plot
# Fkt and packages
source("code/packages_functions.R")

# Load dataset
load("data_inter/hospCase_deaths.RData")

# Calculate total number of cases without considering OPS
# Dx_adj_dia: Deaths from MI in hospital
# Dx_CoD: deaths from MI as underlying cause of death
# C_adj: Hospitalizations from MI

# Calculate life expectancy and respective categories
h_adj <- 
  hosp %>%
  ungroup() %>%
  mutate(mx = Dx_tot / Nx) %>% 
  group_by(Region, sex) %>% 
  mutate(e65 = lifetable(mx=mx)) %>% 
  group_by(sex) %>%
  mutate(e65_cat = cut(e65, 
                       breaks = quantile(e65, probs = c(0, 1/3, 2/3, 1)), 
                       labels = 1:3, 
                       include.lowest = TRUE)) %>% 
  ungroup() %>% 
  # incidence, case fatality
  mutate(cfat = (Dx_MI-Dx_MI_hosp_prim+Dx_hosp_MI_sec) / (C_hosp_MI+(Dx_MI-Dx_MI_hosp_prim)),
         inc = (C_hosp_MI+(Dx_MI-Dx_MI_hosp_prim)) / Nx) %>%
  select(Region, Age, sex, cfat, inc, kreis_t_sett, e65_cat) %>% 
  filter(kreis_t_sett %in% c(1,4))

# # Get number of rural/urban districts by e65_cat 
h_adj %>%
  distinct(Region, e65_cat, kreis_t_sett) %>%
  count(e65_cat, kreis_t_sett)

# Plot incidence
inc_median <- 
  h_adj %>% 
  group_by(Age, sex, kreis_t_sett, e65_cat) %>% 
  summarise(inc_1 = quantile(inc, probs = 0.25),
            inc_5 = quantile(inc, probs = 0.5),
            inc_9 = quantile(inc, probs = 0.75),
            cfat_1 = quantile(cfat, probs = 0.25),
            cfat_5 = quantile(cfat, probs = 0.5),
            cfat_9 = quantile(cfat, probs = 0.75))

inc_median_t <- 
  h_adj %>% 
  group_by(Age, sex, kreis_t_sett) %>% 
  summarise(inc_1 = quantile(inc, probs = 0.25),
            inc_5 = quantile(inc, probs = 0.5),
            inc_9 = quantile(inc, probs = 0.75),
            cfat_1 = quantile(cfat, probs = 0.25),
            cfat_5 = quantile(cfat, probs = 0.5),
            cfat_9 = quantile(cfat, probs = 0.75))


# dev.off()
pdf("figures/MI_Median_incidence_CaseFatality_settlement_e65.pdf", family = "Times", 
    width = 14, pointsize = 17, height = 14)
layout(cbind(c(1,3), c(2,4)))

# Women
# MI events
par(mar = c(4, 4, 5, 2) + 0.1)

plot(x=1, y=1, typ = "n", ylim = c(-0.004, 0.004), xlim = c(0, 6), 
     xlab = NA, ylab = NA, bty = "n", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i")
par(las = 1)
yy <- seq(-0.004, 0.004, by = 0.001)
axis(2, at = yy, 
     labels = round(yy*10000,0), lwd = 0, pos = 0.08, cex.axis = 1.2)

segments(x0=0:6, y0=-0.004,x1=0:6, y1=0.004, 
         col = "lightgray")
segments(x0=0, y0=yy,x1=6, y1=yy, 
         col = "lightgray")
segments(x0=0, y0=0,x1=6, y1=0, 
         col = "lightgray", lwd = 4)

par(las = 0)
mtext("Absolute difference per 10000 PY", 2, cex = 1.3, line = 2.8)

# mtext("Age group", 1, cex = 1.3, line = 3)
axis(1, at = seq(0.5, 5.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84",
                "85-89", "90+"), lwd = 0, cex.axis = 1.2)
# Women
# Total estimates
rur_t <- inc_median_t %>% filter(sex == "w" & 
                                 kreis_t_sett == 4)

urb_t <- inc_median_t %>% filter(sex == "w" & 
                                   kreis_t_sett == 1)

diff_t <- (rur_t$inc_5-urb_t$inc_5)
lines(x=seq(0.5, 5.5, by=1), y=diff_t, typ = "p", 
      pch = 16, cex = 1.5, col = 'black')
lines(x=seq(0.5, 5.5, by=1), y=diff_t, typ = "l", 
      lwd = 3, col = 'black', lty = 2)

# Rural-Urban
rur_1 <- inc_median %>% filter(sex == "w" & 
                                 kreis_t_sett == 4 &
                                 e65_cat == 1)
rur_2 <- inc_median %>% filter(sex == "w" & 
                                 kreis_t_sett == 4 &
                                 e65_cat == 2)
rur_3 <- inc_median %>% filter(sex == "w" & 
                                 kreis_t_sett == 4 &
                                 e65_cat == 3)


urb_1 <- inc_median %>% filter(sex == "w" & 
                                 kreis_t_sett == 1 &
                                 e65_cat == 1)
urb_2 <- inc_median %>% filter(sex == "w" & 
                                 kreis_t_sett == 1 &
                                 e65_cat == 2)
urb_3 <- inc_median %>% filter(sex == "w" & 
                                 kreis_t_sett == 1 &
                                 e65_cat == 3)

lines(x=seq(0.5, 5.5, by=1), y=rur_1$inc_5-urb_1$inc_5, typ = "p", 
      pch = 16, cex = 1.5, col = '#6baed6')
lines(x=seq(0.5, 5.5, by=1), y=rur_1$inc_5-urb_1$inc_5, typ = "l", 
      lwd = 3, col = '#6baed6', lty = 2)

lines(x=seq(0.5, 5.5, by=1), y=rur_2$inc_5-urb_2$inc_5, typ = "p", 
      pch = 16, cex = 1.5, col = '#3182bd')
lines(x=seq(0.5, 5.5, by=1), y=rur_2$inc_5-urb_2$inc_5, typ = "l", 
      lwd = 3, col = '#3182bd', lty = 2)

lines(x=seq(0.5, 5.5, by=1), y=rur_3$inc_5-urb_3$inc_5, typ = "p", 
      pch = 16, cex = 1.5, col = '#08519c')
lines(x=seq(0.5, 5.5, by=1), y=rur_3$inc_5-urb_3$inc_5, typ = "l", 
      lwd = 3, col = '#08519c', lty = 2)


mtext("Women", 3, line = -1.5, cex = 1.4, adj = 0.01)
mtext("Rural-Urban difference in\nmedian rate of MI events", 3, line = 1, cex = 1.5)
legend("bottomleft", legend = c("Total",
                              "Low LE",
                              "Mid LE", 
                              "High LE"), 
       col = c("black", '#6baed6', '#3182bd', '#08519c'), pch = 15, cex =1.3, 
       bty = "n")

# MI deaths

plot(x=1, y=1, typ = "n", ylim = c(-0.06, 0.06), xlim = c(0, 6), 
     xlab = NA, ylab = NA, bty = "n", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i")
par(las = 1)
yy <- seq(-0.06, 0.06, by = 0.02)
axis(2, at = yy, 
     labels = yy, lwd = 0, pos = 0.08, cex.axis = 1.2)

segments(x0=0:6, y0=-0.06,x1=0:6, y1=0.06, 
         col = "lightgray")
segments(x0=0, y0=yy,x1=6, y1=yy, 
         col = "lightgray")

par(las = 0)
mtext("Absolute difference", 2, cex = 1.3, line = 3)
mtext("Rural-Urban difference in\nmedian prop. of MI-related deaths", 
      3, line = 1, cex = 1.5)
# mtext("Age group", 1, cex = 1.3, line = 3)
axis(1, at = seq(0.5, 5.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84",
                "85-89", "90+"), lwd = 0, cex.axis = 1.2)
# Women
# Case fatality
diff_t <- (rur_t$cfat_5-urb_t$cfat_5)
lines(x=seq(0.5, 5.5, by=1), y=diff_t, typ = "p", 
      pch = 16, cex = 1.5, col = 'black')
lines(x=seq(0.5, 5.5, by=1), y=diff_t, typ = "l", 
      lwd = 3, col = 'black', lty = 2)

lines(x=seq(0.5, 5.5, by=1), y=rur_1$cfat_5-urb_1$cfat_5, typ = "p", 
      pch = 16, cex = 1.5, col = '#6baed6')
lines(x=seq(0.5, 5.5, by=1), y=rur_1$cfat_5-urb_1$cfat_5, typ = "l", 
      lwd = 3, col = '#6baed6', lty = 2)

lines(x=seq(0.5, 5.5, by=1), y=rur_2$cfat_5-urb_2$cfat_5, typ = "p", 
      pch = 16, cex = 1.5, col = '#3182bd')
lines(x=seq(0.5, 5.5, by=1), y=rur_2$cfat_5-urb_2$cfat_5, typ = "l", 
      lwd = 3, col = '#3182bd', lty = 2)

lines(x=seq(0.5, 5.5, by=1), y=rur_3$cfat_5-urb_3$cfat_5, typ = "p", 
      pch = 16, cex = 1.5, col = '#08519c')
lines(x=seq(0.5, 5.5, by=1), y=rur_3$cfat_5-urb_3$cfat_5, typ = "l", 
      lwd = 3, col = '#08519c', lty = 2)


# Men
par(mar = c(7, 4, 2, 2) + 0.1)

plot(x=1, y=1, typ = "n", ylim = c(-0.004, 0.004), xlim = c(0, 6), 
     xlab = NA, ylab = NA, bty = "n", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i")
par(las = 1)
yy <- seq(-0.004, 0.004, by = 0.001)
axis(2, at = yy, 
     labels = round(yy*10000,0), lwd = 0, pos = 0.08, cex.axis = 1.2)

segments(x0=0:6, y0=-0.004,x1=0:6, y1=0.004, 
         col = "lightgray")
segments(x0=0, y0=yy,x1=6, y1=yy, 
         col = "lightgray")
segments(x0=0, y0=0,x1=6, y1=0, 
         col = "lightgray", lwd = 4)

par(las = 0)
mtext("Absolute difference per 10000 PY", 2, cex = 1.3, line = 2.8)

# mtext("Age group", 1, cex = 1.3, line = 3)
axis(1, at = seq(0.5, 5.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84",
                "85-89", "90+"), lwd = 0, cex.axis = 1.2)
mtext("Age group", 1, cex = 1.3, line = 3)

# Total estimates
rur_t <- inc_median_t %>% filter(sex == "m" & 
                                   kreis_t_sett == 4)

urb_t <- inc_median_t %>% filter(sex == "m" & 
                                   kreis_t_sett == 1)

diff_t <- (rur_t$inc_5-urb_t$inc_5)
lines(x=seq(0.5, 5.5, by=1), y=diff_t, typ = "p", 
      pch = 16, cex = 1.5, col = 'black')
lines(x=seq(0.5, 5.5, by=1), y=diff_t, typ = "l", 
      lwd = 3, col = 'black', lty = 2)

# Rural-Urban
rur_1 <- inc_median %>% filter(sex == "m" & 
                                 kreis_t_sett == 4 &
                                 e65_cat == 1)
rur_2 <- inc_median %>% filter(sex == "m" & 
                                 kreis_t_sett == 4 &
                                 e65_cat == 2)
rur_3 <- inc_median %>% filter(sex == "m" & 
                                 kreis_t_sett == 4 &
                                 e65_cat == 3)


urb_1 <- inc_median %>% filter(sex == "m" & 
                                 kreis_t_sett == 1 &
                                 e65_cat == 1)
urb_2 <- inc_median %>% filter(sex == "m" & 
                                 kreis_t_sett == 1 &
                                 e65_cat == 2)
urb_3 <- inc_median %>% filter(sex == "m" & 
                                 kreis_t_sett == 1 &
                                 e65_cat == 3)

lines(x=seq(0.5, 5.5, by=1), y=rur_1$inc_5-urb_1$inc_5, typ = "p", 
      pch = 16, cex = 1.5, col = '#6baed6')
lines(x=seq(0.5, 5.5, by=1), y=rur_1$inc_5-urb_1$inc_5, typ = "l", 
      lwd = 3, col = '#6baed6', lty = 2)

lines(x=seq(0.5, 5.5, by=1), y=rur_2$inc_5-urb_2$inc_5, typ = "p", 
      pch = 16, cex = 1.5, col = '#3182bd')
lines(x=seq(0.5, 5.5, by=1), y=rur_2$inc_5-urb_2$inc_5, typ = "l", 
      lwd = 3, col = '#3182bd', lty = 2)

lines(x=seq(0.5, 5.5, by=1), y=rur_3$inc_5-urb_3$inc_5, typ = "p", 
      pch = 16, cex = 1.5, col = '#08519c')
lines(x=seq(0.5, 5.5, by=1), y=rur_3$inc_5-urb_3$inc_5, typ = "l", 
      lwd = 3, col = '#08519c', lty = 2)
mtext("Men", 3, line = -1.5, cex = 1.4, adj = 0.01)


# Men
plot(x=1, y=1, typ = "n", ylim = c(-0.06, 0.06), xlim = c(0, 6), 
     xlab = NA, ylab = NA, bty = "n", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i")
par(las = 1)
yy <- seq(-0.06, 0.06, by = 0.02)
axis(2, at = yy, 
     labels = yy, lwd = 0, pos = 0.08, cex.axis = 1.2)

segments(x0=0:6, y0=-0.06,x1=0:6, y1=0.06, 
         col = "lightgray")
segments(x0=0, y0=yy,x1=6, y1=yy, 
         col = "lightgray")

par(las = 0)
mtext("Asolute difference", 2, cex = 1.3, line = 3)
mtext("Age group", 1, cex = 1.3, line = 3)
axis(1, at = seq(0.5, 5.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84",
                "85-89", "90+"), lwd = 0, cex.axis = 1.2)

diff_t <- (rur_t$cfat_5-urb_t$cfat_5)
lines(x=seq(0.5, 5.5, by=1), y=diff_t, typ = "p", 
      pch = 16, cex = 1.5, col = 'black')
lines(x=seq(0.5, 5.5, by=1), y=diff_t, typ = "l", 
      lwd = 3, col = 'black', lty = 2)

lines(x=seq(0.5, 5.5, by=1), y=rur_1$cfat_5-urb_1$cfat_5, typ = "p", 
      pch = 16, cex = 1.5, col = '#6baed6')
lines(x=seq(0.5, 5.5, by=1), y=rur_1$cfat_5-urb_1$cfat_5, typ = "l", 
      lwd = 3, col = '#6baed6', lty = 2)

lines(x=seq(0.5, 5.5, by=1), y=rur_2$cfat_5-urb_2$cfat_5, typ = "p", 
      pch = 16, cex = 1.5, col = '#3182bd')
lines(x=seq(0.5, 5.5, by=1), y=rur_2$cfat_5-urb_2$cfat_5, typ = "l", 
      lwd = 3, col = '#3182bd', lty = 2)

lines(x=seq(0.5, 5.5, by=1), y=rur_3$cfat_5-urb_3$cfat_5, typ = "p", 
      pch = 16, cex = 1.5, col = '#08519c')
lines(x=seq(0.5, 5.5, by=1), y=rur_3$cfat_5-urb_3$cfat_5, typ = "l", 
      lwd = 3, col = '#08519c', lty = 2)

dev.off()

