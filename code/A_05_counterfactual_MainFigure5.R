# Calculate incidence and plot
# Fkt and packages
source("code/packages_functions.R")

# Load dataset
load("data_inter/hospCase_deaths.RData")

# Calculate total number of cases without considering OPS
# Dx_adj_dia: Deaths from MI in hospital
# Dx_CoD: deaths from MI as underlying cause of death
# C_adj: Hospitalizations from MI

h_adj <- 
  hosp %>% 
  ungroup() %>% 
  # incidence, case fatality
  mutate(cfat = (Dx_MI-Dx_MI_hosp_prim+Dx_hosp_MI_sec) / (C_hosp_MI+(Dx_MI-Dx_MI_hosp_prim)),
         inc = (C_hosp_MI+(Dx_MI-Dx_MI_hosp_prim)) / Nx, 
         inc_inHosp = C_hosp_MI / Nx, 
         inc_outHosp = (Dx_MI-Dx_MI_hosp_prim) / Nx) %>%
  filter(kreis_t_sett %in% c(1,4)) %>% 
  select(Region, Age, sex, C_hosp_MI, Dx_MI_hosp_prim, Dx_MI, Dx_hosp_MI_sec, 
         Dx_CVD, Dx_tot, Nx, kreis_t_sett, cfat, inc, inc_inHosp, inc_outHosp)

  
# Calculate median rates
median_est <- 
  h_adj %>% 
  group_by(Age, sex, kreis_t_sett) %>% 
  summarise(cfat_5 = quantile(cfat, probs = 0.5),
            inc_5 = quantile(inc, probs = 0.5),
            inH_5 = quantile(inc_inHosp, probs = 0.5),
            outH_5 = quantile(inc_outHosp, probs = 0.5))


# Which death rates are needed?
# Initial death rate
# mx_tot
# mx_CVD
# mx_MI
# mx_MIHosp

# Stepwise change rural values to median urban value 
# (inHosp, outHosp, case fatality)

# Step 1: Calculate intial death rates and median values
Dx_data <- 
  hosp %>% 
  group_by(Age, sex, Region) %>% 
  mutate(mx_all = Dx_tot / Nx,
         mx_CVD = Dx_CVD / Nx,
         mx_MI = (Dx_MI-Dx_MI_hosp_prim+Dx_hosp_MI_sec) / Nx,
         mx_hosp = Dx_hosp_MI_sec / Nx) %>%
  group_by(Age, sex, kreis_t_sett) %>% 
  summarise(mx_all = quantile(mx_all, probs = 0.5),
         mx_CVD = quantile(mx_CVD, probs = 0.5),
         mx_MI = quantile(mx_MI, probs = 0.5),
         mx_hosp = quantile(mx_hosp, probs = 0.5)) %>% 
  filter(kreis_t_sett %in% c(1,4)) %>% 
  pivot_wider(id_cols = c(Age, sex),
              names_from = c(kreis_t_sett), 
              values_from = c(mx_all:mx_hosp)) %>% 
  summarise(diff_all = (mx_all_4/mx_all_1)-1,
            diff_CVD = (mx_CVD_4/mx_CVD_1)-1,
            diff_MI = (mx_MI_4/mx_MI_1)-1,
            diff_hosp = (mx_hosp_4/mx_hosp_1)-1)

# Step 2: Link urban median values
median_est_urban <- 
  median_est %>% filter(kreis_t_sett == 1) %>% select(!kreis_t_sett)

h_adj <- 
  hosp %>% 
  ungroup() %>% 
  # incidence, case fatality
  mutate(cfat = (Dx_MI-Dx_MI_hosp_prim+Dx_hosp_MI_sec) / (C_hosp_MI+(Dx_MI-Dx_MI_hosp_prim)),
         inc = (C_hosp_MI+(Dx_MI-Dx_MI_hosp_prim)) / Nx, 
         inc_inHosp = C_hosp_MI / Nx, 
         inc_outHosp = (Dx_MI-Dx_MI_hosp_prim) / Nx) %>%
  filter(kreis_t_sett %in% c(1,4)) %>% 
  select(Region, Age, sex, C_hosp_MI, Dx_MI_hosp_prim, Dx_MI, Dx_hosp_MI_sec, 
         Dx_CVD, Dx_tot, Nx, kreis_t_sett, cfat, inc, inc_inHosp, inc_outHosp) %>% 
  left_join(median_est_urban)

# Step 3: estimate MI-related deaths under the different scenarios
h_adj_scenario <- 
  h_adj %>%
  rowwise() %>% 
  mutate(inc_inHosp_A = 
           ifelse(inH_5 < inc_inHosp & kreis_t_sett == 4, 
                  inH_5, inc_inHosp),
         inc_outHosp_A = 
           ifelse(outH_5 < inc_outHosp & kreis_t_sett == 4, 
                  outH_5, inc_outHosp),
        cfat_A = 
           ifelse(cfat_5 < cfat & kreis_t_sett == 4, 
                  cfat_5, cfat), 
        inc_A =
          ifelse(inc_5 < inc & kreis_t_sett == 4, 
                 inc_5, inc)) %>% 
  mutate(Dx_MI_related = (Dx_MI-Dx_MI_hosp_prim+Dx_hosp_MI_sec),
         Dx_inH = (Nx*inc_inHosp_A + Nx*inc_outHosp)*cfat,
         Dx_outH =(Nx*inc_A)*cfat,
         Dx_cfat = (Nx*inc_inHosp + Nx*inc_outHosp)*cfat_A,
         Dx_full = (Nx*inc_inHosp_A + Nx*inc_outHosp_A)*cfat_A)


# Step 4: Calculate death rates and the corresponding median values 
mx_scenario <- 
  h_adj_scenario %>%
  mutate(mx_inH = Dx_inH / Nx,
         mx_outH = Dx_outH / Nx, 
         mx_cfat = Dx_cfat / Nx,
         mx_full = Dx_full / Nx, 
         mx_MI = Dx_MI_related / Nx)
# test <- 
#   mx_scenario %>% 
#   filter(Age == 65 & sex == "w") %>% 
#   mutate(col_p = ifelse(kreis_t_sett == 1, "black", "blue"))
# 
# test2 <- 
#   median_mx_scenario %>% 
#   filter(Age == 65 & sex == "w")
# 
# 
# plot(x=test$mx_MI, y=test$mx_full, col = test$col_p, pch = 16, typ = "n")
# lines(x=c(0, 8e-04), y=c(0, 8e-04), col = "lightgray", lwd = 5)
# lines(x=test$mx_MI, y=test$mx_full, col = test$col_p, pch = 16, typ = "p")
# 

# Calculate median rates and mortality differences
median_mx_scenario <- 
  mx_scenario %>% 
  group_by(Age, sex, kreis_t_sett) %>% 
  summarise(mx_inH_med = quantile(mx_inH, probs = 0.5),
            mx_outH_med = quantile(mx_outH, probs = 0.5),
            mx_cfat_med = quantile(mx_cfat, probs = 0.5),
            mx_full_med = quantile(mx_full, probs = 0.5),
            mx_MI_med = quantile(mx_MI, probs = 0.5)) %>% 
  pivot_wider(id_cols = c(Age, sex),
              names_from = c(kreis_t_sett), 
              values_from = c(mx_inH_med:mx_MI_med)) %>% 
  summarise(diff_inH = (mx_inH_med_4/mx_inH_med_1)-1,
            diff_outH = (mx_outH_med_4/mx_outH_med_1)-1,
            diff_cfat = (mx_cfat_med_4/mx_cfat_med_1)-1,
            diff_full = (mx_full_med_4/mx_full_med_1)-1,
            diff_MI = (mx_MI_med_4/mx_MI_med_1)-1)



# Caluclate deaths counts
Dx_scenario <- 
  mx_scenario %>% 
  group_by(sex, Age, kreis_t_sett) %>% 
  summarise(across(c(Dx_MI_related,Dx_inH:Dx_full), ~ sum(.x)))

Dx_scenario_tot <- 
  mx_scenario %>% 
  group_by(sex, kreis_t_sett) %>% 
  summarise(across(c(Dx_MI_related,Dx_inH:Dx_full), ~ sum(.x)))

Dx_total <- 
  Dx_scenario_tot %>%
  group_by(kreis_t_sett) %>% 
  summarise(Dx_MI_related = sum(Dx_MI_related),
            Dx_inH = sum(Dx_inH),
            Dx_outH = sum(Dx_outH),
            Dx_cfat = sum(Dx_cfat),
            Dx_full = sum(Dx_full))

# Plot results
basic_bar_mx <- function(){
  plot(x=1, y=1, typ = "n", lwd = 3, xlim = c(-0.25, 0.25), ylim = c(0,6), 
       bty = "n", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", 
       xlab = NA, ylab = NA)
  segments(x0=seq(-0.25, 0.25, by = 0.05), y0=0,
           x1=seq(-0.25, 0.25, by = 0.05), y1=6, 
           col = "lightgray")
  segments(x0=0, y0=0,
           x1=0, y1=6, 
           col = "lightgray", lwd = 5)
  
  axis(1, at = seq(-0.25, 0.25, by = 0.05), 
       labels = c("25%", "20%", "15%", "10%", "5%", "0%", "5%", "10%", "15%", "20%", "25%"), 
       lwd = 3, cex.axis = 1.4, pos = -0.08)
  axis(1, at = seq(-0.225, 0.225, by = 0.05), 
       labels = FALSE, 
       lwd = 1, pos = -0.08)
  par(las = 1)
  axis(2, at = seq(0.5, 5.5, by = 1), 
       labels = c("65-69", "70-74", "75-79",
                  "80-84", "85-89", "90+"), 
       lwd = 0, cex.axis = 1.5, pos = -0.245)
  
  # mtext("Relative mortality difference", 1,  line = 3.5, cex = 1.4)
  mtext("Urban excess", 1, line = -1, cex = 1.1, adj = 0.01)
  mtext("Rural excess", 1, line = -1, cex = 1.1, adj = 0.99)
}

basic_bar_counts <- function(){
  plot(x=1, y=1, typ = "n", lwd = 3, xlim = c(0, 9000), ylim = c(0,6), 
       bty = "n", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", 
       xlab = NA, ylab = NA)
  segments(x0=seq(0, 9000, by = 1000), y0=0,
           x1=seq(0, 9000, by = 1000), y1=6, 
           col = "lightgray")
  
  axis(1, at = seq(0, 9000, by = 1000), 
       labels = TRUE, lwd = 3, cex.axis = 1.4, pos = -0.08)
  axis(1, at = seq(500, 8500, by = 1000), 
       labels = FALSE, lwd = 1, pos = -0.08)
  par(las = 1)
  axis(2, at = seq(0.5, 5.5, by = 1), 
       labels = c("65-69", "70-74", "75-79",
                  "80-84", "85-89", "90+"), 
       lwd = 0, cex.axis = 1.5)
    # mtext("MI-related death counts", 3,  line = 1, cex = 1.4)
}

pdf("figures/counterfactual_settlement.pdf", family = "Times",
    width = 14, pointsize = 14)
# layout(cbind(c(1,2),c(3,4)))
layout(rbind(c(1,2), c(3,3)), height = c(0.9, 0.1))

# Women
# MI_related
par(mar = c(3, 4, 3, 1.5) + 0.1)
basic_bar_mx()
wom <- median_mx_scenario %>% filter(sex == "w")

yy_inH <- seq(0.3, 5.3, by = 1)
yy_cfat <- seq(0.6, 5.6, by = 1)
yy_full <- seq(0.9, 5.9, by = 1)

# mtext("MI-related mortality", 3, line = 1, cex = 1.3)
mtext("Women", 3, line = 1, cex = 1.3, adj = 0.01)
mtext("Relative mortality difference", 1, line = 3, cex = 1.3)
for(i in 1:6){
  # segments(x0=wom$diff_MI[i], y0=yy_inH[i], x1=wom$diff_inH[i],
  #          y1=yy_inH[i], col = '#33a02c', lwd = 5)
  segments(x0=wom$diff_MI[i], y0=yy_inH[i], x1=wom$diff_outH[i],
           y1=yy_inH[i], col = '#b2df8a', lwd = 5)
  # lines(x=wom$diff_inH[i], y=yy_inH[i], col = '#33a02c', pch = 16, typ = "p", cex = 2)
  lines(x=wom$diff_outH[i], y=yy_inH[i], col = '#b2df8a', pch = 16, typ = "p", cex = 2)
  
  segments(x0=wom$diff_MI[i], y0=yy_cfat[i], x1=wom$diff_cfat[i],
           y1=yy_cfat[i], col = '#1f78b4', lwd = 5)
  lines(x=wom$diff_cfat[i], y=yy_cfat[i], col = '#1f78b4', pch = 16, typ = "p", cex = 2)
  
  segments(x0=wom$diff_MI[i], y0=yy_full[i], x1=wom$diff_full[i],
           y1=yy_full[i], col = '#a6cee3', lwd = 5)
  lines(x=wom$diff_full[i], y=yy_full[i], col = '#a6cee3', pch = 16, typ = "p", cex = 2)
}

segments(x0=wom$diff_MI, y0=seq(0, 5, 1),
         x1=wom$diff_MI, y1=seq(1, 6, 1), 
         lwd = 7, col = "darkgray")

# Men
# MI_related
basic_bar_mx()
wom <- median_mx_scenario %>% filter(sex == "m")

yy_inH <- seq(0.3, 5.3, by = 1)
yy_cfat <- seq(0.6, 5.6, by = 1)
yy_full <- seq(0.9, 5.9, by = 1)

# mtext("MI-related mortality", 3, line = 1, cex = 1.3)
mtext("Men", 3, line = 1, cex = 1.3, adj = 0.01)
mtext("Relative mortality difference", 1, line = 3, cex = 1.3)
for(i in 1:6){
  # segments(x0=wom$diff_MI[i], y0=yy_inH[i], x1=wom$diff_inH[i],
  #          y1=yy_inH[i], col = '#33a02c', lwd = 5)
  segments(x0=wom$diff_MI[i], y0=yy_inH[i], x1=wom$diff_outH[i],
           y1=yy_inH[i], col = '#b2df8a', lwd = 5)
  # lines(x=wom$diff_inH[i], y=yy_inH[i], col = '#33a02c', pch = 16, typ = "p", cex = 2)
  lines(x=wom$diff_outH[i], y=yy_inH[i], col = '#b2df8a', pch = 16, typ = "p", cex = 2)
  
  segments(x0=wom$diff_MI[i], y0=yy_cfat[i], x1=wom$diff_cfat[i],
           y1=yy_cfat[i], col = '#1f78b4', lwd = 5)
  lines(x=wom$diff_cfat[i], y=yy_cfat[i], col = '#1f78b4', pch = 16, typ = "p", cex = 2)
  
  segments(x0=wom$diff_MI[i], y0=yy_full[i], x1=wom$diff_full[i],
           y1=yy_full[i], col = '#a6cee3', lwd = 5)
  lines(x=wom$diff_full[i], y=yy_full[i], col = '#a6cee3', pch = 16, typ = "p", cex = 2)
}

segments(x0=wom$diff_MI, y0=seq(0, 5, 1),
         x1=wom$diff_MI, y1=seq(1, 6, 1), 
         lwd = 7, col = "darkgray")

# Legend
par(mar=c(1,1,1,1))
plot(x=1, y=1, bty = "n", xlab = NA, ylab = NA, xaxs = "i", yaxs = "i", 
     xaxt = "n", yaxt = "n", typ = "n")
legend("center", 
       legend = c("Observed difference", 
                  "Rate of MI events (I)", 
                  "Proportion of deaths (II)", 
                  "All components (III)"),
       bty = "n", col =c("darkgray", '#b2df8a', '#1f78b4', '#a6cee3'), 
       cex = 1.3, pch = 15, horiz = TRUE, pt.cex = 2)

dev.off()














# 
# Additional figures
pdf("figures/counterfactual_settlement_w_hopsitalization.pdf", family = "Times",
    width = 14, pointsize = 14)
# layout(cbind(c(1,2),c(3,4)))
layout(rbind(c(1,2), c(3,3)), height = c(0.9, 0.1))

# Women
# MI_related
par(mar = c(3, 4, 3, 1.5) + 0.1)
basic_bar_mx()
wom <- median_mx_scenario %>% filter(sex == "w")

yy_inH <- seq(0.3, 5.3, by = 1)
yy_cfat <- seq(0.6, 5.6, by = 1)
yy_full <- seq(0.9, 5.9, by = 1)

# mtext("MI-related mortality", 3, line = 1, cex = 1.3)
mtext("Women", 3, line = 1, cex = 1.3, adj = 0.01)
mtext("Relative mortality difference", 1, line = 3, cex = 1.3)
for(i in 1:6){
  segments(x0=wom$diff_MI[i], y0=yy_inH[i], x1=wom$diff_inH[i],
           y1=yy_inH[i], col = '#33a02c', lwd = 5)
  segments(x0=wom$diff_inH[i], y0=yy_inH[i], x1=wom$diff_outH[i],
           y1=yy_inH[i], col = '#b2df8a', lwd = 5)
  lines(x=wom$diff_inH[i], y=yy_inH[i], col = '#33a02c', pch = 16, typ = "p", cex = 2)
  lines(x=wom$diff_outH[i], y=yy_inH[i], col = '#b2df8a', pch = 16, typ = "p", cex = 2)
  
  segments(x0=wom$diff_MI[i], y0=yy_cfat[i], x1=wom$diff_cfat[i],
           y1=yy_cfat[i], col = '#1f78b4', lwd = 5)
  lines(x=wom$diff_cfat[i], y=yy_cfat[i], col = '#1f78b4', pch = 16, typ = "p", cex = 2)
  
  segments(x0=wom$diff_MI[i], y0=yy_full[i], x1=wom$diff_full[i],
           y1=yy_full[i], col = '#a6cee3', lwd = 5)
  lines(x=wom$diff_full[i], y=yy_full[i], col = '#a6cee3', pch = 16, typ = "p", cex = 2)
}

segments(x0=wom$diff_MI, y0=seq(0, 5, 1),
         x1=wom$diff_MI, y1=seq(1, 6, 1), 
         lwd = 7, col = "darkgray")

# Men
# MI_related
basic_bar_mx()
wom <- median_mx_scenario %>% filter(sex == "m")

yy_inH <- seq(0.3, 5.3, by = 1)
yy_cfat <- seq(0.6, 5.6, by = 1)
yy_full <- seq(0.9, 5.9, by = 1)

# mtext("MI-related mortality", 3, line = 1, cex = 1.3)
mtext("Men", 3, line = 1, cex = 1.3, adj = 0.01)
mtext("Relative mortality difference", 1, line = 3, cex = 1.3)
for(i in 1:6){
  segments(x0=wom$diff_MI[i], y0=yy_inH[i], x1=wom$diff_inH[i],
           y1=yy_inH[i], col = '#33a02c', lwd = 5)
  segments(x0=wom$diff_inH[i], y0=yy_inH[i], x1=wom$diff_outH[i],
           y1=yy_inH[i], col = '#b2df8a', lwd = 5)
  lines(x=wom$diff_inH[i], y=yy_inH[i], col = '#33a02c', pch = 16, typ = "p", cex = 2)
  lines(x=wom$diff_outH[i], y=yy_inH[i], col = '#b2df8a', pch = 16, typ = "p", cex = 2)
  
  segments(x0=wom$diff_MI[i], y0=yy_cfat[i], x1=wom$diff_cfat[i],
           y1=yy_cfat[i], col = '#1f78b4', lwd = 5)
  lines(x=wom$diff_cfat[i], y=yy_cfat[i], col = '#1f78b4', pch = 16, typ = "p", cex = 2)
  
  segments(x0=wom$diff_MI[i], y0=yy_full[i], x1=wom$diff_full[i],
           y1=yy_full[i], col = '#a6cee3', lwd = 5)
  lines(x=wom$diff_full[i], y=yy_full[i], col = '#a6cee3', pch = 16, typ = "p", cex = 2)
}

segments(x0=wom$diff_MI, y0=seq(0, 5, 1),
         x1=wom$diff_MI, y1=seq(1, 6, 1), 
         lwd = 7, col = "darkgray")

# Legend
par(mar=c(1,1,1,1))
plot(x=1, y=1, bty = "n", xlab = NA, ylab = NA, xaxs = "i", yaxs = "i", 
     xaxt = "n", yaxt = "n", typ = "n")
legend("center", 
       legend = c("Initial difference", 
                  "Only hospitalized MI (I)", 
                  "Rate of MI events (II)", 
                  "Proportion of deaths (III)", 
                  "All components (IV)"),
       bty = "n", col =c("darkgray", '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3'), 
       cex = 1.3, pch = 15, horiz = TRUE, pt.cex = 2)

dev.off()


pdf("figures/counterfactual_settlement_withCountBar.pdf", family = "Times",
    width = 14, pointsize = 14, height = 14)
# layout(cbind(c(1,2),c(3,4)))
layout(rbind(cbind(c(1,2), c(3,4)), c(5,5)), height = c(0.45, 0.45, 0.1))

# Women
# MI_related
par(mar = c(3, 4, 3, 1.5) + 0.1)
basic_bar_mx()
wom <- median_mx_scenario %>% filter(sex == "w")

yy_inH <- seq(0.3, 5.3, by = 1)
yy_cfat <- seq(0.6, 5.6, by = 1)
yy_full <- seq(0.9, 5.9, by = 1)

# mtext("MI-related mortality", 3, line = 1, cex = 1.3)
mtext("Women", 3, line = 1, cex = 1.3, adj = 0.01)
# mtext("Relative mortality difference", 1, line = 2.5, cex = 1.3)
for(i in 1:6){
  segments(x0=wom$diff_MI[i], y0=yy_inH[i], x1=wom$diff_inH[i],
         y1=yy_inH[i], col = '#33a02c', lwd = 5)
  segments(x0=wom$diff_inH[i], y0=yy_inH[i], x1=wom$diff_outH[i],
           y1=yy_inH[i], col = '#b2df8a', lwd = 5)
  lines(x=wom$diff_inH[i], y=yy_inH[i], col = '#33a02c', pch = 16, typ = "p", cex = 2)
  lines(x=wom$diff_outH[i], y=yy_inH[i], col = '#b2df8a', pch = 16, typ = "p", cex = 2)
  
  segments(x0=wom$diff_MI[i], y0=yy_cfat[i], x1=wom$diff_cfat[i],
         y1=yy_cfat[i], col = '#1f78b4', lwd = 5)
  lines(x=wom$diff_cfat[i], y=yy_cfat[i], col = '#1f78b4', pch = 16, typ = "p", cex = 2)
  
  segments(x0=wom$diff_MI[i], y0=yy_full[i], x1=wom$diff_full[i],
         y1=yy_full[i], col = '#a6cee3', lwd = 5)
  lines(x=wom$diff_full[i], y=yy_full[i], col = '#a6cee3', pch = 16, typ = "p", cex = 2)
}

segments(x0=wom$diff_MI, y0=seq(0, 5, 1),
         x1=wom$diff_MI, y1=seq(1, 6, 1), 
         lwd = 5, col = "darkgray")

# Men
# MI_related
basic_bar_mx()
wom <- median_mx_scenario %>% filter(sex == "m")

yy_inH <- seq(0.3, 5.3, by = 1)
yy_cfat <- seq(0.6, 5.6, by = 1)
yy_full <- seq(0.9, 5.9, by = 1)

# mtext("MI-related mortality", 3, line = 1, cex = 1.3)
mtext("Men", 3, line = 1, cex = 1.3, adj = 0.01)
mtext("Relative mortality difference", 1, line = 4, cex = 1.3)
for(i in 1:6){
  segments(x0=wom$diff_MI[i], y0=yy_inH[i], x1=wom$diff_inH[i],
           y1=yy_inH[i], col = '#33a02c', lwd = 5)
  segments(x0=wom$diff_inH[i], y0=yy_inH[i], x1=wom$diff_outH[i],
           y1=yy_inH[i], col = '#b2df8a', lwd = 5)
  lines(x=wom$diff_inH[i], y=yy_inH[i], col = '#33a02c', pch = 16, typ = "p", cex = 2)
  lines(x=wom$diff_outH[i], y=yy_inH[i], col = '#b2df8a', pch = 16, typ = "p", cex = 2)
  
  segments(x0=wom$diff_MI[i], y0=yy_cfat[i], x1=wom$diff_cfat[i],
           y1=yy_cfat[i], col = '#1f78b4', lwd = 5)
  lines(x=wom$diff_cfat[i], y=yy_cfat[i], col = '#1f78b4', pch = 16, typ = "p", cex = 2)
  
  segments(x0=wom$diff_MI[i], y0=yy_full[i], x1=wom$diff_full[i],
           y1=yy_full[i], col = '#a6cee3', lwd = 5)
  lines(x=wom$diff_full[i], y=yy_full[i], col = '#a6cee3', pch = 16, typ = "p", cex = 2)
}

segments(x0=wom$diff_MI, y0=seq(0, 5, 1),
         x1=wom$diff_MI, y1=seq(1, 6, 1), 
         lwd = 5, col = "darkgray")

# Bar plots death counts 
# Women
basic_bar_counts()
wom <- Dx_scenario %>% filter(sex == "w" & kreis_t_sett == 4)
yy <- cbind(seq(0.1, 5.1, by = 1),
            seq(0.4, 5.4, by = 1),
            seq(0.6, 5.6, by = 1),
            seq(0.8, 5.8, by = 1))


yy_inH <- c(0, 0, 0.225, 0.225)
yy_cfat <- c(0.225, 0.225, 0.45, 0.45)
yy_full <- c(0.45, 0.45, 0.675, 0.675)
yy_int <- c(0.675, 0.675, 0.9, 0.9)

for(i in 1:6){
  polygon(x=c(0, rep(wom$Dx_inH[i],2), 0), y=yy_inH+i-1,
         col = adjustcolor('#33a02c', alpha.f = 0.7), border = "white")
  polygon(x=c(wom$Dx_inH[i], rep(wom$Dx_outH[i],2), wom$Dx_inH[i]), y=yy_inH+i-1,
          col = adjustcolor('#b2df8a', alpha.f = 0.7), border = "white")
  polygon(x=c(0, rep(wom$Dx_cfat[i],2), 0), y=yy_cfat+i-1,
          col = adjustcolor('#1f78b4', alpha.f = 0.7), border = "white")
  polygon(x=c(0, rep(wom$Dx_full[i],2), 0), y=yy_full+i-1,
          col = adjustcolor('#a6cee3', alpha.f = 0.7), border = "white")
  polygon(x=c(0, rep(wom$Dx_MI_related[i],2), 0), y=yy_int+i-1,
          col = adjustcolor('darkgray', alpha.f = 0.7), border = "white")
}

# Men
basic_bar_counts()
wom <- Dx_scenario %>% filter(sex == "m" & kreis_t_sett == 4)
yy <- cbind(seq(0.1, 5.1, by = 1),
            seq(0.4, 5.4, by = 1),
            seq(0.6, 5.6, by = 1),
            seq(0.8, 5.8, by = 1))


yy_inH <- c(0, 0, 0.225, 0.225)
yy_cfat <- c(0.225, 0.225, 0.45, 0.45)
yy_full <- c(0.45, 0.45, 0.675, 0.675)
yy_int <- c(0.675, 0.675, 0.9, 0.9)

for(i in 1:6){
  polygon(x=c(0, rep(wom$Dx_inH[i],2), 0), y=yy_inH+i-1,
          col = adjustcolor('#33a02c', alpha.f = 0.7), border = "white")
  polygon(x=c(wom$Dx_inH[i], rep(wom$Dx_outH[i],2), wom$Dx_inH[i]), y=yy_inH+i-1,
          col = adjustcolor('#b2df8a', alpha.f = 0.7), border = "white")
  polygon(x=c(0, rep(wom$Dx_cfat[i],2), 0), y=yy_cfat+i-1,
          col = adjustcolor('#1f78b4', alpha.f = 0.7), border = "white")
  polygon(x=c(0, rep(wom$Dx_full[i],2), 0), y=yy_full+i-1,
          col = adjustcolor('#a6cee3', alpha.f = 0.7), border = "white")
  polygon(x=c(0, rep(wom$Dx_MI_related[i],2), 0), y=yy_int+i-1,
          col = adjustcolor('darkgray', alpha.f = 0.7), border = "white")
}
mtext("MI-related deaths", 1, line = 4, cex = 1.3)

# Legend
par(mar=c(1,1,1,1))
plot(x=1, y=1, bty = "n", xlab = NA, ylab = NA, xaxs = "i", yaxs = "i", 
     xaxt = "n", yaxt = "n", typ = "n")
legend("center", 
       legend = c("Initial difference/deaths", 
                  "Only hospitalized MI events", 
                  "Hospitalized MI events &\nDeaths outside hospital", 
                  "Proportion of deaths", 
                  "All components"),
       bty = "n", col =c("darkgray", '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3'), 
       cex = 1.6, pch = 15, horiz = TRUE)

dev.off()


# For presentation

pdf("figures/counterfactual_settlement_presentation_1.pdf", family = "Times",
    width = 14, pointsize = 14)
# layout(cbind(c(1,2),c(3,4)))
layout(rbind(c(1,2), c(3,3)), height = c(0.85, 0.15))

# Women
# MI_related
par(mar = c(3, 4, 3, 1.5) + 0.1)
basic_bar_mx()
wom <- median_mx_scenario %>% filter(sex == "w")

yy_inH <- seq(0.3, 5.3, by = 1)
yy_cfat <- seq(0.6, 5.6, by = 1)
yy_full <- seq(0.9, 5.9, by = 1)

# mtext("MI-related mortality", 3, line = 1, cex = 1.3)
mtext("Women", 3, line = 1, cex = 1.3, adj = 0.01)
mtext("Relative mortality difference", 1, line = 3, cex = 1.3)
# mtext("Relative mortality difference", 1, line = 2.5, cex = 1.3)
# for(i in 1:6){
#   segments(x0=wom$diff_MI[i], y0=yy_inH[i], x1=wom$diff_inH[i],
#            y1=yy_inH[i], col = '#33a02c', lwd = 5)
#   segments(x0=wom$diff_inH[i], y0=yy_inH[i], x1=wom$diff_outH[i],
#            y1=yy_inH[i], col = '#b2df8a', lwd = 5)
#   lines(x=wom$diff_inH[i], y=yy_inH[i], col = '#33a02c', pch = 16, typ = "p", cex = 2)
#   lines(x=wom$diff_outH[i], y=yy_inH[i], col = '#b2df8a', pch = 16, typ = "p", cex = 2)
#   
#   segments(x0=wom$diff_MI[i], y0=yy_cfat[i], x1=wom$diff_cfat[i],
#            y1=yy_cfat[i], col = '#1f78b4', lwd = 5)
#   lines(x=wom$diff_cfat[i], y=yy_cfat[i], col = '#1f78b4', pch = 16, typ = "p", cex = 2)
#   
#   segments(x0=wom$diff_MI[i], y0=yy_full[i], x1=wom$diff_full[i],
#            y1=yy_full[i], col = '#a6cee3', lwd = 5)
#   lines(x=wom$diff_full[i], y=yy_full[i], col = '#a6cee3', pch = 16, typ = "p", cex = 2)
# }

segments(x0=wom$diff_MI, y0=seq(0, 5, 1),
         x1=wom$diff_MI, y1=seq(1, 6, 1), 
         lwd = 5, col = "darkgray")

# Men
# MI_related
basic_bar_mx()
wom <- median_mx_scenario %>% filter(sex == "m")

yy_inH <- seq(0.3, 5.3, by = 1)
yy_cfat <- seq(0.6, 5.6, by = 1)
yy_full <- seq(0.9, 5.9, by = 1)

# mtext("MI-related mortality", 3, line = 1, cex = 1.3)
mtext("Men", 3, line = 1, cex = 1.3, adj = 0.01)
mtext("Relative mortality difference", 1, line = 3, cex = 1.3)
# for(i in 1:6){
#   segments(x0=wom$diff_MI[i], y0=yy_inH[i], x1=wom$diff_inH[i],
#            y1=yy_inH[i], col = '#33a02c', lwd = 5)
#   segments(x0=wom$diff_inH[i], y0=yy_inH[i], x1=wom$diff_outH[i],
#            y1=yy_inH[i], col = '#b2df8a', lwd = 5)
#   lines(x=wom$diff_inH[i], y=yy_inH[i], col = '#33a02c', pch = 16, typ = "p", cex = 2)
#   lines(x=wom$diff_outH[i], y=yy_inH[i], col = '#b2df8a', pch = 16, typ = "p", cex = 2)
#   
#   segments(x0=wom$diff_MI[i], y0=yy_cfat[i], x1=wom$diff_cfat[i],
#            y1=yy_cfat[i], col = '#1f78b4', lwd = 5)
#   lines(x=wom$diff_cfat[i], y=yy_cfat[i], col = '#1f78b4', pch = 16, typ = "p", cex = 2)
#   
#   segments(x0=wom$diff_MI[i], y0=yy_full[i], x1=wom$diff_full[i],
#            y1=yy_full[i], col = '#a6cee3', lwd = 5)
#   lines(x=wom$diff_full[i], y=yy_full[i], col = '#a6cee3', pch = 16, typ = "p", cex = 2)
# }

segments(x0=wom$diff_MI, y0=seq(0, 5, 1),
         x1=wom$diff_MI, y1=seq(1, 6, 1), 
         lwd = 5, col = "darkgray")

# Legend
par(mar=c(1,1,1,1))
plot(x=1, y=1, bty = "n", xlab = NA, ylab = NA, xaxs = "i", yaxs = "i", 
     xaxt = "n", yaxt = "n", typ = "n")
legend("center", 
       legend = c("Initial difference/deaths", 
                  "Only hospitalized MI events", 
                  "Hospitalized MI events &\nDeaths outside hospital", 
                  "Proportion of deaths", 
                  "All components"),
       bty = "n", col =c("darkgray", '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3'), 
       cex = 1.3, pch = 15, horiz = TRUE, pt.cex = 2)
dev.off()



pdf("figures/counterfactual_settlement_presentation_2.pdf", family = "Times",
    width = 14, pointsize = 14)
# layout(cbind(c(1,2),c(3,4)))
layout(rbind(c(1,2), c(3,3)), height = c(0.85, 0.15))

# Women
# MI_related
par(mar = c(3, 4, 3, 1.5) + 0.1)
basic_bar_mx()
wom <- median_mx_scenario %>% filter(sex == "w")

yy_inH <- seq(0.3, 5.3, by = 1)
yy_cfat <- seq(0.6, 5.6, by = 1)
yy_full <- seq(0.9, 5.9, by = 1)

# mtext("MI-related mortality", 3, line = 1, cex = 1.3)
mtext("Women", 3, line = 1, cex = 1.3, adj = 0.01)
mtext("Relative mortality difference", 1, line = 3, cex = 1.3)
for(i in 1:6){
  segments(x0=wom$diff_MI[i], y0=yy_inH[i], x1=wom$diff_inH[i],
           y1=yy_inH[i], col = '#33a02c', lwd = 5)
  # segments(x0=wom$diff_inH[i], y0=yy_inH[i], x1=wom$diff_outH[i],
  #          y1=yy_inH[i], col = '#b2df8a', lwd = 5)
  lines(x=wom$diff_inH[i], y=yy_inH[i], col = '#33a02c', pch = 16, typ = "p", cex = 2)
  # lines(x=wom$diff_outH[i], y=yy_inH[i], col = '#b2df8a', pch = 16, typ = "p", cex = 2)
#   
#   segments(x0=wom$diff_MI[i], y0=yy_cfat[i], x1=wom$diff_cfat[i],
#            y1=yy_cfat[i], col = '#1f78b4', lwd = 5)
#   lines(x=wom$diff_cfat[i], y=yy_cfat[i], col = '#1f78b4', pch = 16, typ = "p", cex = 2)
#   
#   segments(x0=wom$diff_MI[i], y0=yy_full[i], x1=wom$diff_full[i],
#            y1=yy_full[i], col = '#a6cee3', lwd = 5)
#   lines(x=wom$diff_full[i], y=yy_full[i], col = '#a6cee3', pch = 16, typ = "p", cex = 2)
}

segments(x0=wom$diff_MI, y0=seq(0, 5, 1),
         x1=wom$diff_MI, y1=seq(1, 6, 1), 
         lwd = 5, col = "darkgray")

# Men
# MI_related
basic_bar_mx()
wom <- median_mx_scenario %>% filter(sex == "m")

yy_inH <- seq(0.3, 5.3, by = 1)
yy_cfat <- seq(0.6, 5.6, by = 1)
yy_full <- seq(0.9, 5.9, by = 1)

# mtext("MI-related mortality", 3, line = 1, cex = 1.3)
mtext("Men", 3, line = 1, cex = 1.3, adj = 0.01)
mtext("Relative mortality difference", 1, line = 3, cex = 1.3)
for(i in 1:6){
  segments(x0=wom$diff_MI[i], y0=yy_inH[i], x1=wom$diff_inH[i],
           y1=yy_inH[i], col = '#33a02c', lwd = 5)
#   segments(x0=wom$diff_inH[i], y0=yy_inH[i], x1=wom$diff_outH[i],
#            y1=yy_inH[i], col = '#b2df8a', lwd = 5)
  lines(x=wom$diff_inH[i], y=yy_inH[i], col = '#33a02c', pch = 16, typ = "p", cex = 2)
#   lines(x=wom$diff_outH[i], y=yy_inH[i], col = '#b2df8a', pch = 16, typ = "p", cex = 2)
#   
#   segments(x0=wom$diff_MI[i], y0=yy_cfat[i], x1=wom$diff_cfat[i],
#            y1=yy_cfat[i], col = '#1f78b4', lwd = 5)
#   lines(x=wom$diff_cfat[i], y=yy_cfat[i], col = '#1f78b4', pch = 16, typ = "p", cex = 2)
#   
#   segments(x0=wom$diff_MI[i], y0=yy_full[i], x1=wom$diff_full[i],
#            y1=yy_full[i], col = '#a6cee3', lwd = 5)
#   lines(x=wom$diff_full[i], y=yy_full[i], col = '#a6cee3', pch = 16, typ = "p", cex = 2)
}

segments(x0=wom$diff_MI, y0=seq(0, 5, 1),
         x1=wom$diff_MI, y1=seq(1, 6, 1), 
         lwd = 5, col = "darkgray")

# Legend
par(mar=c(1,1,1,1))
plot(x=1, y=1, bty = "n", xlab = NA, ylab = NA, xaxs = "i", yaxs = "i", 
     xaxt = "n", yaxt = "n", typ = "n")
legend("center", 
       legend = c("Initial difference/deaths", 
                  "Only hospitalized MI events", 
                  "Hospitalized MI events &\nDeaths outside hospital", 
                  "Proportion of deaths", 
                  "All components"),
       bty = "n", col =c("darkgray", '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3'), 
       cex = 1.3, pch = 15, horiz = TRUE, pt.cex = 2)
dev.off()


pdf("figures/counterfactual_settlement_presentation_3.pdf", family = "Times",
    width = 14, pointsize = 14)
# layout(cbind(c(1,2),c(3,4)))
layout(rbind(c(1,2), c(3,3)), height = c(0.85, 0.15))

# Women
# MI_related
par(mar = c(3, 4, 3, 1.5) + 0.1)
basic_bar_mx()
wom <- median_mx_scenario %>% filter(sex == "w")

yy_inH <- seq(0.3, 5.3, by = 1)
yy_cfat <- seq(0.6, 5.6, by = 1)
yy_full <- seq(0.9, 5.9, by = 1)

# mtext("MI-related mortality", 3, line = 1, cex = 1.3)
mtext("Women", 3, line = 1, cex = 1.3, adj = 0.01)
mtext("Relative mortality difference", 1, line = 3, cex = 1.3)
for(i in 1:6){
  segments(x0=wom$diff_MI[i], y0=yy_inH[i], x1=wom$diff_inH[i],
           y1=yy_inH[i], col = '#33a02c', lwd = 5)
  segments(x0=wom$diff_inH[i], y0=yy_inH[i], x1=wom$diff_outH[i],
           y1=yy_inH[i], col = '#b2df8a', lwd = 5)
  lines(x=wom$diff_inH[i], y=yy_inH[i], col = '#33a02c', pch = 16, typ = "p", cex = 2)
  lines(x=wom$diff_outH[i], y=yy_inH[i], col = '#b2df8a', pch = 16, typ = "p", cex = 2)
  #   
  #   segments(x0=wom$diff_MI[i], y0=yy_cfat[i], x1=wom$diff_cfat[i],
  #            y1=yy_cfat[i], col = '#1f78b4', lwd = 5)
  #   lines(x=wom$diff_cfat[i], y=yy_cfat[i], col = '#1f78b4', pch = 16, typ = "p", cex = 2)
  #   
  #   segments(x0=wom$diff_MI[i], y0=yy_full[i], x1=wom$diff_full[i],
  #            y1=yy_full[i], col = '#a6cee3', lwd = 5)
  #   lines(x=wom$diff_full[i], y=yy_full[i], col = '#a6cee3', pch = 16, typ = "p", cex = 2)
}

segments(x0=wom$diff_MI, y0=seq(0, 5, 1),
         x1=wom$diff_MI, y1=seq(1, 6, 1), 
         lwd = 5, col = "darkgray")

# Men
# MI_related
basic_bar_mx()
wom <- median_mx_scenario %>% filter(sex == "m")

yy_inH <- seq(0.3, 5.3, by = 1)
yy_cfat <- seq(0.6, 5.6, by = 1)
yy_full <- seq(0.9, 5.9, by = 1)

# mtext("MI-related mortality", 3, line = 1, cex = 1.3)
mtext("Men", 3, line = 1, cex = 1.3, adj = 0.01)
mtext("Relative mortality difference", 1, line = 3, cex = 1.3)
for(i in 1:6){
  segments(x0=wom$diff_MI[i], y0=yy_inH[i], x1=wom$diff_inH[i],
           y1=yy_inH[i], col = '#33a02c', lwd = 5)
  segments(x0=wom$diff_inH[i], y0=yy_inH[i], x1=wom$diff_outH[i],
             y1=yy_inH[i], col = '#b2df8a', lwd = 5)
  lines(x=wom$diff_inH[i], y=yy_inH[i], col = '#33a02c', pch = 16, typ = "p", cex = 2)
  lines(x=wom$diff_outH[i], y=yy_inH[i], col = '#b2df8a', pch = 16, typ = "p", cex = 2)
  #   
  #   segments(x0=wom$diff_MI[i], y0=yy_cfat[i], x1=wom$diff_cfat[i],
  #            y1=yy_cfat[i], col = '#1f78b4', lwd = 5)
  #   lines(x=wom$diff_cfat[i], y=yy_cfat[i], col = '#1f78b4', pch = 16, typ = "p", cex = 2)
  #   
  #   segments(x0=wom$diff_MI[i], y0=yy_full[i], x1=wom$diff_full[i],
  #            y1=yy_full[i], col = '#a6cee3', lwd = 5)
  #   lines(x=wom$diff_full[i], y=yy_full[i], col = '#a6cee3', pch = 16, typ = "p", cex = 2)
}

segments(x0=wom$diff_MI, y0=seq(0, 5, 1),
         x1=wom$diff_MI, y1=seq(1, 6, 1), 
         lwd = 5, col = "darkgray")

# Legend
par(mar=c(1,1,1,1))
plot(x=1, y=1, bty = "n", xlab = NA, ylab = NA, xaxs = "i", yaxs = "i", 
     xaxt = "n", yaxt = "n", typ = "n")
legend("center", 
       legend = c("Initial difference/deaths", 
                  "Only hospitalized MI events", 
                  "Hospitalized MI events &\nDeaths outside hospital", 
                  "Proportion of deaths", 
                  "All components"),
       bty = "n", col =c("darkgray", '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3'), 
       cex = 1.3, pch = 15, horiz = TRUE, pt.cex = 2)

dev.off()


pdf("figures/counterfactual_settlement_presentation_4.pdf", family = "Times",
    width = 14, pointsize = 14)
# layout(cbind(c(1,2),c(3,4)))
layout(rbind(c(1,2), c(3,3)), height = c(0.85, 0.15))

# Women
# MI_related
par(mar = c(3, 4, 3, 1.5) + 0.1)
basic_bar_mx()
wom <- median_mx_scenario %>% filter(sex == "w")

yy_inH <- seq(0.3, 5.3, by = 1)
yy_cfat <- seq(0.6, 5.6, by = 1)
yy_full <- seq(0.9, 5.9, by = 1)

# mtext("MI-related mortality", 3, line = 1, cex = 1.3)
mtext("Women", 3, line = 1, cex = 1.3, adj = 0.01)
mtext("Relative mortality difference", 1, line = 3, cex = 1.3)
for(i in 1:6){
  segments(x0=wom$diff_MI[i], y0=yy_inH[i], x1=wom$diff_inH[i],
           y1=yy_inH[i], col = '#33a02c', lwd = 5)
  segments(x0=wom$diff_inH[i], y0=yy_inH[i], x1=wom$diff_outH[i],
           y1=yy_inH[i], col = '#b2df8a', lwd = 5)
  lines(x=wom$diff_inH[i], y=yy_inH[i], col = '#33a02c', pch = 16, typ = "p", cex = 2)
  lines(x=wom$diff_outH[i], y=yy_inH[i], col = '#b2df8a', pch = 16, typ = "p", cex = 2)

    segments(x0=wom$diff_MI[i], y0=yy_cfat[i], x1=wom$diff_cfat[i],
             y1=yy_cfat[i], col = '#1f78b4', lwd = 5)
    lines(x=wom$diff_cfat[i], y=yy_cfat[i], col = '#1f78b4', pch = 16, typ = "p", cex = 2)

    # segments(x0=wom$diff_MI[i], y0=yy_full[i], x1=wom$diff_full[i],
    #          y1=yy_full[i], col = '#a6cee3', lwd = 5)
    # lines(x=wom$diff_full[i], y=yy_full[i], col = '#a6cee3', pch = 16, typ = "p", cex = 2)
}

segments(x0=wom$diff_MI, y0=seq(0, 5, 1),
         x1=wom$diff_MI, y1=seq(1, 6, 1), 
         lwd = 5, col = "darkgray")

# Men
# MI_related
basic_bar_mx()
wom <- median_mx_scenario %>% filter(sex == "m")

yy_inH <- seq(0.3, 5.3, by = 1)
yy_cfat <- seq(0.6, 5.6, by = 1)
yy_full <- seq(0.9, 5.9, by = 1)

# mtext("MI-related mortality", 3, line = 1, cex = 1.3)
mtext("Men", 3, line = 1, cex = 1.3, adj = 0.01)
mtext("Relative mortality difference", 1, line = 3, cex = 1.3)
for(i in 1:6){
  segments(x0=wom$diff_MI[i], y0=yy_inH[i], x1=wom$diff_inH[i],
           y1=yy_inH[i], col = '#33a02c', lwd = 5)
  segments(x0=wom$diff_inH[i], y0=yy_inH[i], x1=wom$diff_outH[i],
           y1=yy_inH[i], col = '#b2df8a', lwd = 5)
  lines(x=wom$diff_inH[i], y=yy_inH[i], col = '#33a02c', pch = 16, typ = "p", cex = 2)
  lines(x=wom$diff_outH[i], y=yy_inH[i], col = '#b2df8a', pch = 16, typ = "p", cex = 2)

    segments(x0=wom$diff_MI[i], y0=yy_cfat[i], x1=wom$diff_cfat[i],
             y1=yy_cfat[i], col = '#1f78b4', lwd = 5)
    lines(x=wom$diff_cfat[i], y=yy_cfat[i], col = '#1f78b4', pch = 16, typ = "p", cex = 2)

    # segments(x0=wom$diff_MI[i], y0=yy_full[i], x1=wom$diff_full[i],
    #          y1=yy_full[i], col = '#a6cee3', lwd = 5)
    # lines(x=wom$diff_full[i], y=yy_full[i], col = '#a6cee3', pch = 16, typ = "p", cex = 2)
}

segments(x0=wom$diff_MI, y0=seq(0, 5, 1),
         x1=wom$diff_MI, y1=seq(1, 6, 1), 
         lwd = 5, col = "darkgray")

# Legend
par(mar=c(1,1,1,1))
plot(x=1, y=1, bty = "n", xlab = NA, ylab = NA, xaxs = "i", yaxs = "i", 
     xaxt = "n", yaxt = "n", typ = "n")
legend("center", 
       legend = c("Initial difference/deaths", 
                  "Only hospitalized MI events", 
                  "Hospitalized MI events &\nDeaths outside hospital", 
                  "Proportion of deaths", 
                  "All components"),
       bty = "n", col =c("darkgray", '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3'), 
       cex = 1.3, pch = 15, horiz = TRUE, pt.cex = 2)

dev.off()


pdf("figures/counterfactual_settlement_presentation_5.pdf", family = "Times",
    width = 14, pointsize = 14)
# layout(cbind(c(1,2),c(3,4)))
layout(rbind(c(1,2), c(3,3)), height = c(0.85, 0.15))

# Women
# MI_related
par(mar = c(3, 4, 3, 1.5) + 0.1)
basic_bar_mx()
wom <- median_mx_scenario %>% filter(sex == "w")

yy_inH <- seq(0.3, 5.3, by = 1)
yy_cfat <- seq(0.6, 5.6, by = 1)
yy_full <- seq(0.9, 5.9, by = 1)

# mtext("MI-related mortality", 3, line = 1, cex = 1.3)
mtext("Women", 3, line = 1, cex = 1.3, adj = 0.01)
mtext("Relative mortality difference", 1, line = 3, cex = 1.3)
for(i in 1:6){
  segments(x0=wom$diff_MI[i], y0=yy_inH[i], x1=wom$diff_inH[i],
           y1=yy_inH[i], col = '#33a02c', lwd = 5)
  segments(x0=wom$diff_inH[i], y0=yy_inH[i], x1=wom$diff_outH[i],
           y1=yy_inH[i], col = '#b2df8a', lwd = 5)
  lines(x=wom$diff_inH[i], y=yy_inH[i], col = '#33a02c', pch = 16, typ = "p", cex = 2)
  lines(x=wom$diff_outH[i], y=yy_inH[i], col = '#b2df8a', pch = 16, typ = "p", cex = 2)
  
  segments(x0=wom$diff_MI[i], y0=yy_cfat[i], x1=wom$diff_cfat[i],
           y1=yy_cfat[i], col = '#1f78b4', lwd = 5)
  lines(x=wom$diff_cfat[i], y=yy_cfat[i], col = '#1f78b4', pch = 16, typ = "p", cex = 2)
  
  segments(x0=wom$diff_MI[i], y0=yy_full[i], x1=wom$diff_full[i],
           y1=yy_full[i], col = '#a6cee3', lwd = 5)
  lines(x=wom$diff_full[i], y=yy_full[i], col = '#a6cee3', pch = 16, typ = "p", cex = 2)
}

segments(x0=wom$diff_MI, y0=seq(0, 5, 1),
         x1=wom$diff_MI, y1=seq(1, 6, 1), 
         lwd = 5, col = "darkgray")

# Men
# MI_related
basic_bar_mx()
wom <- median_mx_scenario %>% filter(sex == "m")

yy_inH <- seq(0.3, 5.3, by = 1)
yy_cfat <- seq(0.6, 5.6, by = 1)
yy_full <- seq(0.9, 5.9, by = 1)

# mtext("MI-related mortality", 3, line = 1, cex = 1.3)
mtext("Men", 3, line = 1, cex = 1.3, adj = 0.01)
mtext("Relative mortality difference", 1, line = 3, cex = 1.3)
for(i in 1:6){
  segments(x0=wom$diff_MI[i], y0=yy_inH[i], x1=wom$diff_inH[i],
           y1=yy_inH[i], col = '#33a02c', lwd = 5)
  segments(x0=wom$diff_inH[i], y0=yy_inH[i], x1=wom$diff_outH[i],
           y1=yy_inH[i], col = '#b2df8a', lwd = 5)
  lines(x=wom$diff_inH[i], y=yy_inH[i], col = '#33a02c', pch = 16, typ = "p", cex = 2)
  lines(x=wom$diff_outH[i], y=yy_inH[i], col = '#b2df8a', pch = 16, typ = "p", cex = 2)
  
  segments(x0=wom$diff_MI[i], y0=yy_cfat[i], x1=wom$diff_cfat[i],
           y1=yy_cfat[i], col = '#1f78b4', lwd = 5)
  lines(x=wom$diff_cfat[i], y=yy_cfat[i], col = '#1f78b4', pch = 16, typ = "p", cex = 2)
  
  segments(x0=wom$diff_MI[i], y0=yy_full[i], x1=wom$diff_full[i],
           y1=yy_full[i], col = '#a6cee3', lwd = 5)
  lines(x=wom$diff_full[i], y=yy_full[i], col = '#a6cee3', pch = 16, typ = "p", cex = 2)
}

segments(x0=wom$diff_MI, y0=seq(0, 5, 1),
         x1=wom$diff_MI, y1=seq(1, 6, 1), 
         lwd = 5, col = "darkgray")

# Legend
par(mar=c(1,1,1,1))
plot(x=1, y=1, bty = "n", xlab = NA, ylab = NA, xaxs = "i", yaxs = "i", 
     xaxt = "n", yaxt = "n", typ = "n")
legend("center", 
       legend = c("Initial difference/deaths", 
                  "Only hospitalized MI events", 
                  "Hospitalized MI events &\nDeaths outside hospital", 
                  "Proportion of deaths", 
                  "All components"),
       bty = "n", col =c("darkgray", '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3'), 
       cex = 1.3, pch = 15, horiz = TRUE, pt.cex = 2)

dev.off()

