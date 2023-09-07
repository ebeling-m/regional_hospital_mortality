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
         inc = (C_hosp_MI+(Dx_MI-Dx_MI_hosp_prim)) / Nx) %>%
  select(Region, Age, sex, cfat, inc, kreis_t_sett) %>% 
  filter(kreis_t_sett %in% c(1,4))

# # Get number of rural/urban districts 
# h_adj %>%  
#   distinct(Region, PD_cat) %>%
#   count(PD_cat)

# Plot incidence
inc_median <- 
  h_adj %>% 
  group_by(Age, sex, kreis_t_sett) %>% 
  summarise(inc_1 = quantile(inc, probs = 0.25),
            inc_5 = quantile(inc, probs = 0.5),
            inc_9 = quantile(inc, probs = 0.75),
            cfat_1 = quantile(cfat, probs = 0.25),
            cfat_5 = quantile(cfat, probs = 0.5),
            cfat_9 = quantile(cfat, probs = 0.75))

# dev.off()
pdf("figures/MI_Median_incidence_CaseFatality_settlement.pdf", family = "Times", 
    width = 14, pointsize = 17, height = 14)
layout(cbind(c(1,3), c(2,4)))

# Women
# MI events
par(mar = c(4, 4, 5, 2) + 0.1)

plot(x=1, y=1, typ = "n", ylim = c(0.002, 0.034), xlim = c(0, 6), 
     xlab = NA, ylab = NA, bty = "n", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i")
par(las = 1)
yy <- seq(0.002, 0.034, by = 0.004)
axis(2, at = yy, 
     labels = round(yy*10000,0), lwd = 0, pos = 0.08, cex.axis = 1.2)

segments(x0=0:6, y0=0,x1=0:6, y1=0.04, 
         col = "lightgray")
segments(x0=0, y0=yy,x1=6, y1=yy, 
         col = "lightgray")
par(las = 0)
mtext("Cases per 10000 PY", 2, cex = 1.3, line = 2.8)

# mtext("Age group", 1, cex = 1.3, line = 3)
axis(1, at = seq(0.5, 5.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84",
                "85-89", "90+"), lwd = 0, cex.axis = 1.2)
# Women
rur <- inc_median %>% filter(sex == "w", kreis_t_sett == 4)
xx <- c(seq(0.5, 5.5, by=1), rev(seq(0.5, 5.5, by=1)))
yy <- c(rur$inc_1, rev(rur$inc_9))
polygon(x=xx, y=yy, col = adjustcolor('#5ab4ac', alpha.f = 0.2),
        border = "white")

urb <- inc_median %>% filter(sex == "w", kreis_t_sett == 1)
xx <- c(seq(0.5, 5.5, by=1), rev(seq(0.5, 5.5, by=1)))
yy <- c(urb$inc_1, rev(urb$inc_9))
polygon(x=xx, y=yy, col = adjustcolor('#d8b365', alpha.f = 0.2),
        border = "white")

lines(x=seq(0.5, 5.5, by=1), y=rur$inc_5, typ = "p", 
      pch = 15, cex = 1.5, col = '#5ab4ac')
lines(x=seq(0.5, 5.5, by=1), y=rur$inc_5, typ = "l", 
      lwd = 3, col = '#5ab4ac', lty = 2)

lines(x=seq(0.5, 5.5, by=1), y=urb$inc_5, typ = "p", 
      pch = 15, cex = 1.5, col = '#d8b365')
lines(x=seq(0.5, 5.5, by=1), y=urb$inc_5, typ = "l", 
      lwd = 3, col = '#d8b365', lty = 2)
mtext("Women", 3, line = -1.5, cex = 1.4, adj = 0.01)
mtext("MI incidence", 3, line = 1, cex = 1.5)
legend("topright", legend = c("Rural median with IQR", 
                             "Urban median with IQR"), 
       col = c("#5ab4ac", "#d8b365"), pch = 15, cex =1.3, 
       bty = "n")

# MI deaths

plot(x=1, y=1, typ = "n", ylim = c(0, 0.8), xlim = c(0, 6), 
     xlab = NA, ylab = NA, bty = "n", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i")
par(las = 1)
yy <- seq(0, 0.8, by = 0.1)
axis(2, at = yy, 
     labels = yy, lwd = 0, pos = 0.08, cex.axis = 1.2)

segments(x0=0:6, y0=0,x1=0:6, y1=0.8, 
         col = "lightgray")
segments(x0=0, y0=yy,x1=6, y1=yy, 
         col = "lightgray")

par(las = 0)
mtext("Proportion on MI cases", 2, cex = 1.3, line = 3)
mtext("Case fatality", 3, line = 1, cex = 1.5)
# mtext("Age group", 1, cex = 1.3, line = 3)
axis(1, at = seq(0.5, 5.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84",
                "85-89", "90+"), lwd = 0, cex.axis = 1.2)
# Women
rur <- inc_median %>% filter(sex == "w", kreis_t_sett == 4)
xx <- c(seq(0.5, 5.5, by=1), rev(seq(0.5, 5.5, by=1)))
yy <- c(rur$cfat_1, rev(rur$cfat_9))
polygon(x=xx, y=yy, col = adjustcolor('#5ab4ac', alpha.f = 0.2),
        border = "white")

urb <- inc_median %>% filter(sex == "w", kreis_t_sett == 1)
xx <- c(seq(0.5, 5.5, by=1), rev(seq(0.5, 5.5, by=1)))
yy <- c(urb$cfat_1, rev(urb$cfat_9))
polygon(x=xx, y=yy, col = adjustcolor('#d8b365', alpha.f = 0.2),
        border = "white")

lines(x=seq(0.5, 5.5, by=1), y=rur$cfat_5, typ = "p", 
      pch = 15, cex = 1.5, col = '#5ab4ac')
lines(x=seq(0.5, 5.5, by=1), y=rur$cfat_5, typ = "l", 
      lwd = 3, col = '#5ab4ac', lty = 2 )

lines(x=seq(0.5, 5.5, by=1), y=urb$cfat_5, typ = "p", 
      pch = 15, cex = 1.5, col = '#d8b365')
lines(x=seq(0.5, 5.5, by=1), y=urb$cfat_5, typ = "l", 
      lwd = 3, col = '#d8b365', lty = 2 )



# Case Fatality
par(mar = c(7, 4, 2, 2) + 0.1)

# Men
plot(x=1, y=1, typ = "n", ylim = c(0.002, 0.034), xlim = c(0, 6), 
     xlab = NA, ylab = NA, bty = "n", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i")
par(las = 1)
yy <- seq(0.002, 0.034, by = 0.004)
axis(2, at = yy, 
     labels = round(yy*10000,0), lwd = 0, pos = 0.08, cex.axis = 1.2)

segments(x0=0:6, y0=0,x1=0:6, y1=0.04, 
         col = "lightgray")
segments(x0=0, y0=yy,x1=6, y1=yy, 
         col = "lightgray")

par(las = 0)
mtext("Events per 10000 PY", 2, cex = 1.3, line = 2.8)
mtext("Age group", 1, cex = 1.3, line = 3)
axis(1, at = seq(0.5, 5.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84",
                "85-89", "90+"), lwd = 0, cex.axis = 1.2)

rur <- inc_median %>% filter(sex == "m", kreis_t_sett == 4)
xx <- c(seq(0.5, 5.5, by=1), rev(seq(0.5, 5.5, by=1)))
yy <- c(rur$inc_1, rev(rur$inc_9))
polygon(x=xx, y=yy, col = adjustcolor('#5ab4ac', alpha.f = 0.2),
        border = "white")

urb <- inc_median %>% filter(sex == "m", kreis_t_sett == 1)
xx <- c(seq(0.5, 5.5, by=1), rev(seq(0.5, 5.5, by=1)))
yy <- c(urb$inc_1, rev(urb$inc_9))
polygon(x=xx, y=yy, col = adjustcolor('#d8b365', alpha.f = 0.2),
        border = "white")

lines(x=seq(0.5, 5.5, by=1), y=rur$inc_5, typ = "p", 
      pch = 15, cex = 1.5, col = '#5ab4ac')
lines(x=seq(0.5, 5.5, by=1), y=rur$inc_5, typ = "l", 
      lwd = 3, col = '#5ab4ac', lty = 2)

lines(x=seq(0.5, 5.5, by=1), y=urb$inc_5, typ = "p", 
      pch = 15, cex = 1.5, col = '#d8b365')
lines(x=seq(0.5, 5.5, by=1), y=urb$inc_5, typ = "l", 
      lwd = 3, col = '#d8b365', lty = 2)
mtext("Men", 3, line = -1.5, cex = 1.4, adj = 0.01)


# Men
plot(x=1, y=1, typ = "n", ylim = c(0, 0.8), xlim = c(0, 6), 
     xlab = NA, ylab = NA, bty = "n", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i")
par(las = 1)
yy <- seq(0, 0.8, by = 0.1)
axis(2, at = yy, 
     labels = yy, lwd = 0, pos = 0.08, cex.axis = 1.2)

segments(x0=0:6, y0=0,x1=0:6, y1=0.8, 
         col = "lightgray")
segments(x0=0, y0=yy,x1=6, y1=yy, 
         col = "lightgray")

par(las = 0)
mtext("Proportion on MI events", 2, cex = 1.3, line = 3)
mtext("Age group", 1, cex = 1.3, line = 3)
axis(1, at = seq(0.5, 5.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84",
                "85-89", "90+"), lwd = 0, cex.axis = 1.2)

rur <- inc_median %>% filter(sex == "m", kreis_t_sett == 4)
xx <- c(seq(0.5, 5.5, by=1), rev(seq(0.5, 5.5, by=1)))
yy <- c(rur$cfat_1, rev(rur$cfat_9))
polygon(x=xx, y=yy, col = adjustcolor('#5ab4ac', alpha.f = 0.2),
        border = "white")

urb <- inc_median %>% filter(sex == "m", kreis_t_sett == 1)
xx <- c(seq(0.5, 5.5, by=1), rev(seq(0.5, 5.5, by=1)))
yy <- c(urb$cfat_1, rev(urb$cfat_9))
polygon(x=xx, y=yy, col = adjustcolor('#d8b365', alpha.f = 0.2),
        border = "white")

lines(x=seq(0.5, 5.5, by=1), y=rur$cfat_5, typ = "p", 
      pch = 15, cex = 1.5, col = '#5ab4ac')
lines(x=seq(0.5, 5.5, by=1), y=rur$cfat_5, typ = "l", 
      lwd = 3, col = '#5ab4ac', lty = 2 )

lines(x=seq(0.5, 5.5, by=1), y=urb$cfat_5, typ = "p", 
      pch = 15, cex = 1.5, col = '#d8b365')
lines(x=seq(0.5, 5.5, by=1), y=urb$cfat_5, typ = "l", 
      lwd = 3, col = '#d8b365', lty = 2 )

dev.off()


# PAA Figure


# dev.off()
pdf("figures/MI_Median_incidence_CaseFatality_settlement_PAA.pdf", family = "Times", 
    width = 14, pointsize = 17, height = 14)
layout(cbind(c(1,3), c(2,4)))

# Women
# MI events
par(mar = c(4, 4, 5, 2) + 0.1)

plot(x=1, y=1, typ = "n", ylim = c(0.002, 0.034), xlim = c(0, 6), 
     xlab = NA, ylab = NA, bty = "n", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i")
par(las = 1)
yy <- seq(0.002, 0.034, by = 0.004)
axis(2, at = yy, 
     labels = round(yy*10000,0), lwd = 0, pos = 0.08, cex.axis = 1.2)

segments(x0=0:6, y0=0,x1=0:6, y1=0.04, 
         col = "lightgray")
segments(x0=0, y0=yy,x1=6, y1=yy, 
         col = "lightgray")
par(las = 0)
mtext("Events per 10000 PY", 2, cex = 1.3, line = 2.8)

# mtext("Age group", 1, cex = 1.3, line = 3)
axis(1, at = seq(0.5, 5.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84",
                "85-89", "90+"), lwd = 0, cex.axis = 1.2)
# Women
rur <- inc_median %>% filter(sex == "w", kreis_t_sett == 4)
xx <- c(seq(0.5, 5.5, by=1), rev(seq(0.5, 5.5, by=1)))
yy <- c(rur$inc_1, rev(rur$inc_9))
polygon(x=xx, y=yy, col = adjustcolor('#5ab4ac', alpha.f = 0.2),
        border = "white")

urb <- inc_median %>% filter(sex == "w", kreis_t_sett == 1)
xx <- c(seq(0.5, 5.5, by=1), rev(seq(0.5, 5.5, by=1)))
yy <- c(urb$inc_1, rev(urb$inc_9))
polygon(x=xx, y=yy, col = adjustcolor('#d8b365', alpha.f = 0.2),
        border = "white")

lines(x=seq(0.5, 5.5, by=1), y=rur$inc_5, typ = "p", 
      pch = 15, cex = 1.5, col = '#5ab4ac')
lines(x=seq(0.5, 5.5, by=1), y=rur$inc_5, typ = "l", 
      lwd = 3, col = '#5ab4ac', lty = 2)

lines(x=seq(0.5, 5.5, by=1), y=urb$inc_5, typ = "p", 
      pch = 15, cex = 1.5, col = '#d8b365')
lines(x=seq(0.5, 5.5, by=1), y=urb$inc_5, typ = "l", 
      lwd = 3, col = '#d8b365', lty = 2)
mtext("Women", 3, line = -1.5, cex = 1.4, adj = 0.01)
mtext("MI incidence", 3, line = 1, cex = 1.5)
legend("topright", legend = c("Rural median with IQR", 
                              "Urban median with IQR"), 
       col = c("#5ab4ac", "#d8b365"), pch = 15, cex =1.3, 
       bty = "n")

# MI deaths

plot(x=1, y=1, typ = "n", ylim = c(0, 0.8), xlim = c(0, 6), 
     xlab = NA, ylab = NA, bty = "n", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i")
par(las = 1)
yy <- seq(0, 0.8, by = 0.1)
axis(2, at = yy, 
     labels = yy, lwd = 0, pos = 0.08, cex.axis = 1.2)

segments(x0=0:6, y0=0,x1=0:6, y1=0.8, 
         col = "lightgray")
segments(x0=0, y0=yy,x1=6, y1=yy, 
         col = "lightgray")

par(las = 0)
mtext("Proportion on MI events", 2, cex = 1.3, line = 3)
mtext("MI case fatality", 3, line = 1, cex = 1.5)
# mtext("Age group", 1, cex = 1.3, line = 3)
axis(1, at = seq(0.5, 5.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84",
                "85-89", "90+"), lwd = 0, cex.axis = 1.2)
# Women
rur <- inc_median %>% filter(sex == "w", kreis_t_sett == 4)
xx <- c(seq(0.5, 5.5, by=1), rev(seq(0.5, 5.5, by=1)))
yy <- c(rur$cfat_1, rev(rur$cfat_9))
polygon(x=xx, y=yy, col = adjustcolor('#5ab4ac', alpha.f = 0.2),
        border = "white")

urb <- inc_median %>% filter(sex == "w", kreis_t_sett == 1)
xx <- c(seq(0.5, 5.5, by=1), rev(seq(0.5, 5.5, by=1)))
yy <- c(urb$cfat_1, rev(urb$cfat_9))
polygon(x=xx, y=yy, col = adjustcolor('#d8b365', alpha.f = 0.2),
        border = "white")

lines(x=seq(0.5, 5.5, by=1), y=rur$cfat_5, typ = "p", 
      pch = 15, cex = 1.5, col = '#5ab4ac')
lines(x=seq(0.5, 5.5, by=1), y=rur$cfat_5, typ = "l", 
      lwd = 3, col = '#5ab4ac', lty = 2 )

lines(x=seq(0.5, 5.5, by=1), y=urb$cfat_5, typ = "p", 
      pch = 15, cex = 1.5, col = '#d8b365')
lines(x=seq(0.5, 5.5, by=1), y=urb$cfat_5, typ = "l", 
      lwd = 3, col = '#d8b365', lty = 2 )



# Case Fatality
par(mar = c(7, 4, 2, 2) + 0.1)

# Men
plot(x=1, y=1, typ = "n", ylim = c(0.002, 0.034), xlim = c(0, 6), 
     xlab = NA, ylab = NA, bty = "n", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i")
par(las = 1)
yy <- seq(0.002, 0.034, by = 0.004)
axis(2, at = yy, 
     labels = round(yy*10000,0), lwd = 0, pos = 0.08, cex.axis = 1.2)

segments(x0=0:6, y0=0,x1=0:6, y1=0.04, 
         col = "lightgray")
segments(x0=0, y0=yy,x1=6, y1=yy, 
         col = "lightgray")

par(las = 0)
mtext("Events per 10000 PY", 2, cex = 1.3, line = 2.8)
mtext("Age group", 1, cex = 1.3, line = 3)
axis(1, at = seq(0.5, 5.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84",
                "85-89", "90+"), lwd = 0, cex.axis = 1.2)

rur <- inc_median %>% filter(sex == "m", kreis_t_sett == 4)
xx <- c(seq(0.5, 5.5, by=1), rev(seq(0.5, 5.5, by=1)))
yy <- c(rur$inc_1, rev(rur$inc_9))
polygon(x=xx, y=yy, col = adjustcolor('#5ab4ac', alpha.f = 0.2),
        border = "white")

urb <- inc_median %>% filter(sex == "m", kreis_t_sett == 1)
xx <- c(seq(0.5, 5.5, by=1), rev(seq(0.5, 5.5, by=1)))
yy <- c(urb$inc_1, rev(urb$inc_9))
polygon(x=xx, y=yy, col = adjustcolor('#d8b365', alpha.f = 0.2),
        border = "white")

lines(x=seq(0.5, 5.5, by=1), y=rur$inc_5, typ = "p", 
      pch = 15, cex = 1.5, col = '#5ab4ac')
lines(x=seq(0.5, 5.5, by=1), y=rur$inc_5, typ = "l", 
      lwd = 3, col = '#5ab4ac', lty = 2)

lines(x=seq(0.5, 5.5, by=1), y=urb$inc_5, typ = "p", 
      pch = 15, cex = 1.5, col = '#d8b365')
lines(x=seq(0.5, 5.5, by=1), y=urb$inc_5, typ = "l", 
      lwd = 3, col = '#d8b365', lty = 2)
mtext("Men", 3, line = -1.5, cex = 1.4, adj = 0.01)


# Men
plot(x=1, y=1, typ = "n", ylim = c(0, 0.8), xlim = c(0, 6), 
     xlab = NA, ylab = NA, bty = "n", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i")
par(las = 1)
yy <- seq(0, 0.8, by = 0.1)
axis(2, at = yy, 
     labels = yy, lwd = 0, pos = 0.08, cex.axis = 1.2)

segments(x0=0:6, y0=0,x1=0:6, y1=0.8, 
         col = "lightgray")
segments(x0=0, y0=yy,x1=6, y1=yy, 
         col = "lightgray")

par(las = 0)
mtext("Proportion on MI events", 2, cex = 1.3, line = 3)
mtext("Age group", 1, cex = 1.3, line = 3)
axis(1, at = seq(0.5, 5.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84",
                "85-89", "90+"), lwd = 0, cex.axis = 1.2)

rur <- inc_median %>% filter(sex == "m", kreis_t_sett == 4)
xx <- c(seq(0.5, 5.5, by=1), rev(seq(0.5, 5.5, by=1)))
yy <- c(rur$cfat_1, rev(rur$cfat_9))
polygon(x=xx, y=yy, col = adjustcolor('#5ab4ac', alpha.f = 0.2),
        border = "white")

urb <- inc_median %>% filter(sex == "m", kreis_t_sett == 1)
xx <- c(seq(0.5, 5.5, by=1), rev(seq(0.5, 5.5, by=1)))
yy <- c(urb$cfat_1, rev(urb$cfat_9))
polygon(x=xx, y=yy, col = adjustcolor('#d8b365', alpha.f = 0.2),
        border = "white")

lines(x=seq(0.5, 5.5, by=1), y=rur$cfat_5, typ = "p", 
      pch = 16, cex = 1.5, col = '#5ab4ac')
lines(x=seq(0.5, 5.5, by=1), y=rur$cfat_5, typ = "l", 
      lwd = 3, col = '#5ab4ac', lty = 2 )

lines(x=seq(0.5, 5.5, by=1), y=urb$cfat_5, typ = "p", 
      pch = 16, cex = 1.5, col = '#d8b365')
lines(x=seq(0.5, 5.5, by=1), y=urb$cfat_5, typ = "l", 
      lwd = 3, col = '#d8b365', lty = 2 )

dev.off()


