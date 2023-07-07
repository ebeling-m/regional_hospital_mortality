# frequency of hospitalizations
source("code/packages_functions.R")

# Load data
load("data_inter/hospCase_deaths.RData")

# calculate share of MI_cod deaths on all CVD deaths
hosp <- 
  hosp %>%
  ungroup() %>% 
  mutate(MI_on_tot = (Dx_MI-Dx_MI_hosp_prim+Dx_hosp_MI_sec) / Dx_tot,
         CVD_on_tot = Dx_CVD / Dx_tot)


# Describe distribution of Proportion
p_MI_Dx_dist <- 
  hosp %>% 
  group_by(Age, sex, kreis_t_sett) %>% 
  summarise(p_1 = quantile(MI_on_tot, probs = 0.1),
            p_25 = quantile(MI_on_tot, probs = 0.25),
            p_5 = quantile(MI_on_tot, probs = 0.5),
            p_75 = quantile(MI_on_tot, probs = 0.75),
            p_9 = quantile(MI_on_tot, probs = 0.9)) %>% 
  filter(kreis_t_sett %in% c(1,4))

p_CVD_Dx_dist <- 
  hosp %>% 
  group_by(Age, sex, kreis_t_sett) %>% 
  summarise(p_1 = quantile(CVD_on_tot, probs = 0.1),
            p_25 = quantile(CVD_on_tot, probs = 0.25),
            p_5 = quantile(CVD_on_tot, probs = 0.5),
            p_75 = quantile(CVD_on_tot, probs = 0.75),
            p_9 = quantile(CVD_on_tot, probs = 0.9)) %>% 
  filter(kreis_t_sett %in% c(1,4))



pdf("figures/share_MI_deaths.pdf", family = "Times", 
    width = 14, pointsize = 17, height = 14)
layout(cbind(c(1,2), c(3,4)))

# PLot share of MI deaths
# Women
par(mar = c(4, 4, 5, 2) + 0.1, xpd = TRUE)
plot(x=1, y=1, typ = "n", ylim = c(0, 0.14), xlim = c(0, 6), 
     xlab = NA, ylab = NA, bty = "n", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i")
par(las = 1)
yy <- seq(0, 0.14, by = 0.02)
axis(2, at = yy, 
     labels = yy, lwd = 0, pos = 0.08, cex.axis = 1.2)

segments(x0=0:6, y0=0,x1=0:6, y1=0.14, 
         col = "lightgray")
segments(x0=0, y0=yy,x1=6, y1=yy, 
         col = "lightgray")

par(las = 0)
mtext("Proportion", 2, cex = 1.3, line = 3)
mtext("Women", 3, cex = 1.3, line = -1.2, adj = 0.01)
mtext("MI-related deaths", 3, cex = 1.5, line = 0.5)

# mtext("Age group", 1, cex = 1.3, line = 3)
axis(1, at = seq(0.5, 5.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84",
                "85-89", "90+"), lwd = 0, cex.axis = 1.2)

# rural
rur <- p_MI_Dx_dist %>% filter(sex == "w", kreis_t_sett == 4)
lines(x=seq(0.5, 5.5, by = 1), y=rur$p_5, col = adjustcolor('#5ab4ac', alpha.f = 0.8),
        lwd = 3)
lines(x=seq(0.5, 5.5, by = 1), y=rur$p_5, col = adjustcolor('#5ab4ac', alpha.f = 0.8),
      pch = 16, cex = 1.4, typ = "p")
segments(x0=seq(0.5, 5.5, by = 1), y0=rur$p_1, 
         x1=seq(0.5, 5.5, by = 1), y1=rur$p_9, lwd = 6, 
         col = adjustcolor('#5ab4ac', alpha.f = 0.2))

# urban
urb <- p_MI_Dx_dist %>% filter(sex == "w", kreis_t_sett == 1)
lines(x=seq(0.5, 5.5, by = 1), y=urb$p_5, col = adjustcolor('#d8b365', alpha.f = 0.8),
      lwd = 3)
lines(x=seq(0.5, 5.5, by = 1), y=urb$p_5, col = adjustcolor('#d8b365', alpha.f = 0.8),
      pch = 16, cex = 1.4, typ = "p")
segments(x0=seq(0.5, 5.5, by = 1), y0=urb$p_1, 
         x1=seq(0.5, 5.5, by = 1), y1=urb$p_9, lwd = 6, 
         col = adjustcolor('#d8b365', alpha.f = 0.2))

legend("bottomleft", legend = c("Rural", "Urban"), 
       col = c('#5ab4ac', '#d8b365'), pch = 16, 
       bty = "n", cex = 1.2)


# Men
par(mar = c(7, 4, 2, 2) + 0.1)
plot(x=1, y=1, typ = "n", ylim = c(0, 0.14), xlim = c(0, 6), 
     xlab = NA, ylab = NA, bty = "n", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i")
par(las = 1)
yy <- seq(0, 0.14, by = 0.02)
axis(2, at = yy, 
     labels = yy, lwd = 0, pos = 0.08, cex.axis = 1.2)

segments(x0=0:6, y0=0,x1=0:6, y1=0.14, 
         col = "lightgray")
segments(x0=0, y0=yy,x1=6, y1=yy, 
         col = "lightgray")

par(las = 0)
mtext("Proportion", 2, cex = 1.3, line = 3)
mtext("Men", 3, cex = 1.3, line = -1.2, adj = 0.01)

# mtext("Age group", 1, cex = 1.3, line = 3)
axis(1, at = seq(0.5, 5.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84",
                "85-89", "90+"), lwd = 0, cex.axis = 1.2)

# rural
rur <- p_MI_Dx_dist %>% filter(sex == "m", kreis_t_sett == 4)
lines(x=seq(0.5, 5.5, by = 1), y=rur$p_5, col = adjustcolor('#5ab4ac', alpha.f = 0.8),
      lwd = 3)
lines(x=seq(0.5, 5.5, by = 1), y=rur$p_5, col = adjustcolor('#5ab4ac', alpha.f = 0.8),
      pch = 16, cex = 1.4, typ = "p")
segments(x0=seq(0.5, 5.5, by = 1), y0=rur$p_1, 
         x1=seq(0.5, 5.5, by = 1), y1=rur$p_9, lwd = 6, 
         col = adjustcolor('#5ab4ac', alpha.f = 0.2))

# urban
urb <- p_MI_Dx_dist %>% filter(sex == "m", kreis_t_sett == 1)
lines(x=seq(0.5, 5.5, by = 1), y=urb$p_5, col = adjustcolor('#d8b365', alpha.f = 0.8),
      lwd = 3)
lines(x=seq(0.5, 5.5, by = 1), y=urb$p_5, col = adjustcolor('#d8b365', alpha.f = 0.8),
      pch = 16, cex = 1.4, typ = "p")
segments(x0=seq(0.5, 5.5, by = 1), y0=urb$p_1, 
         x1=seq(0.5, 5.5, by = 1), y1=urb$p_9, lwd = 6, 
         col = adjustcolor('#d8b365', alpha.f = 0.2))

#################################
# CVD deaths
par(mar = c(4, 4, 5, 2) + 0.1, xpd = TRUE)
plot(x=1, y=1, typ = "n", ylim = c(0, 0.7), xlim = c(0, 6), 
     xlab = NA, ylab = NA, bty = "n", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i")
par(las = 1)
yy <- seq(0, 0.7, by = 0.1)
axis(2, at = yy, 
     labels = yy, lwd = 0, pos = 0.08, cex.axis = 1.2)

segments(x0=0:6, y0=0,x1=0:6, y1=0.7, 
         col = "lightgray")
segments(x0=0, y0=yy,x1=6, y1=yy, 
         col = "lightgray")

par(las = 0)
mtext("Proportion", 2, cex = 1.3, line = 3)
# mtext("Age group", 1, cex = 1.3, line = 3)
axis(1, at = seq(0.5, 5.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84",
                "85-89", "90+"), lwd = 0, cex.axis = 1.2)
mtext("CVD deaths", 3, cex = 1.5, line = 0.5)

# rural
rur <- p_CVD_Dx_dist %>% filter(sex == "w", kreis_t_sett == 4)
lines(x=seq(0.5, 5.5, by = 1), y=rur$p_5, col = adjustcolor('#5ab4ac', alpha.f = 0.8),
      lwd = 3)
lines(x=seq(0.5, 5.5, by = 1), y=rur$p_5, col = adjustcolor('#5ab4ac', alpha.f = 0.8),
      pch = 16, cex = 1.4, typ = "p")
segments(x0=seq(0.5, 5.5, by = 1), y0=rur$p_1, 
         x1=seq(0.5, 5.5, by = 1), y1=rur$p_9, lwd = 6, 
         col = adjustcolor('#5ab4ac', alpha.f = 0.2))

# urban
urb <- p_CVD_Dx_dist %>% filter(sex == "w", kreis_t_sett == 1)
lines(x=seq(0.5, 5.5, by = 1), y=urb$p_5, col = adjustcolor('#d8b365', alpha.f = 0.8),
      lwd = 3)
lines(x=seq(0.5, 5.5, by = 1), y=urb$p_5, col = adjustcolor('#d8b365', alpha.f = 0.8),
      pch = 16, cex = 1.4, typ = "p")
segments(x0=seq(0.5, 5.5, by = 1), y0=urb$p_1, 
         x1=seq(0.5, 5.5, by = 1), y1=urb$p_9, lwd = 6, 
         col = adjustcolor('#d8b365', alpha.f = 0.2))

# Men
par(mar = c(7, 4, 2, 2) + 0.1)
plot(x=1, y=1, typ = "n", ylim = c(0, 0.7), xlim = c(0, 6), 
     xlab = NA, ylab = NA, bty = "n", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i")
par(las = 1)
yy <- seq(0, 0.7, by = 0.1)
axis(2, at = yy, 
     labels = yy, lwd = 0, pos = 0.08, cex.axis = 1.2)

segments(x0=0:6, y0=0,x1=0:6, y1=0.7, 
         col = "lightgray")
segments(x0=0, y0=yy,x1=6, y1=yy, 
         col = "lightgray")

par(las = 0)
mtext("Proportion", 2, cex = 1.3, line = 3)
# mtext("Age group", 1, cex = 1.3, line = 3)
axis(1, at = seq(0.5, 5.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84",
                "85-89", "90+"), lwd = 0, cex.axis = 1.2)

# rural
rur <- p_CVD_Dx_dist %>% filter(sex == "m", kreis_t_sett == 4)
lines(x=seq(0.5, 5.5, by = 1), y=rur$p_5, col = adjustcolor('#5ab4ac', alpha.f = 0.8),
      lwd = 3)
lines(x=seq(0.5, 5.5, by = 1), y=rur$p_5, col = adjustcolor('#5ab4ac', alpha.f = 0.8),
      pch = 16, cex = 1.4, typ = "p")
segments(x0=seq(0.5, 5.5, by = 1), y0=rur$p_1, 
         x1=seq(0.5, 5.5, by = 1), y1=rur$p_9, lwd = 6, 
         col = adjustcolor('#5ab4ac', alpha.f = 0.2))

# urban
urb <- p_CVD_Dx_dist %>% filter(sex == "m", kreis_t_sett == 1)
lines(x=seq(0.5, 5.5, by = 1), y=urb$p_5, col = adjustcolor('#d8b365', alpha.f = 0.8),
      lwd = 3)
lines(x=seq(0.5, 5.5, by = 1), y=urb$p_5, col = adjustcolor('#d8b365', alpha.f = 0.8),
      pch = 16, cex = 1.4, typ = "p")
segments(x0=seq(0.5, 5.5, by = 1), y0=urb$p_1, 
         x1=seq(0.5, 5.5, by = 1), y1=urb$p_9, lwd = 6, 
         col = adjustcolor('#d8b365', alpha.f = 0.2))

dev.off()

