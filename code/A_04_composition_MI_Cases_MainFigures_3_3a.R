# frequency of hospitalizations
source("code/packages_functions.R")

# Load Hopsital case data
h_case_adm <- 
  read_sas("data_input/cases_admission_1122_anonym.sas7bdat") %>% 
  filter(!is.na(ags2017kreis)) %>%
  rename(Region = ags2017kreis, dia = icd_hd3NEW, 
         Age = alterGRP_5) %>% 
  # Adjust for Eisenach\Wartburgkreis
  mutate(Region = ifelse(Region == 16056, 16063, Region)) %>%  
  select(!yearGRP) %>% 
  group_by(sex, Age, Region, dia, aufn_anl) %>% 
  summarise(Cases = sum(Cases))

# A = Verlegung/moved to other hospital with treatment up to 24 hours
# N = Emergency cases
# X = Other

h_case_adj_adm <- 
  expand_grid(Region = unique(h_case_adm$Region), sex = c("m", "w"), 
              Age = seq(65, 90, by = 5), dia = 0:1, 
              aufn_anl = c("A", "N", "X")) %>% 
  left_join(h_case_adm) %>% 
  rowwise() %>%  
  mutate(C_adj = ifelse(is.na(Cases), sample(size = 1, c(0,1,2)), Cases)) %>% 
  # Calculate total cases
  filter(dia == 1) %>% 
  group_by(Region, Age, sex, aufn_anl) %>% 
  summarise(C_hosp_MI_adm = sum(C_adj))


# Load hospital data
load("data_inter/hospCase_deaths.RData")
hosp_merge <- 
  hosp %>% 
  select(Region, sex, Age, C_hosp_MI, Dx_MI, 
         Dx_MI_hosp_prim, Nx, kreis_t_sett)

# Merge both datasets
h_case_adj_adm <- 
  h_case_adj_adm %>% 
  left_join(hosp_merge)


# Step 1: Compare total number of cases
num_hosp_case <- 
  h_case_adj_adm %>% 
  group_by(Region, Age, sex) %>% 
  mutate(C_hosp_MI_SumAdm = sum(C_hosp_MI_adm)) %>% 
  distinct(Region, Age, sex, C_hosp_MI_SumAdm, C_hosp_MI) %>% 
  mutate(diff_case = C_hosp_MI_SumAdm/C_hosp_MI)

# Results: No significant difference between num of cases in both datasets
comp_MI_rate <-
  h_case_adj_adm %>%
  # mutate(aufn_anl = ifelse(aufn_anl == "A", "X", aufn_anl)) %>% 
  group_by(Region, Age, sex, Nx, aufn_anl, kreis_t_sett, 
           Dx_MI_hosp_prim, Dx_MI) %>%
  summarise(C_hosp_MI_adm = sum(C_hosp_MI_adm)) %>%
  group_by(Region, Age, sex) %>% 
  ungroup() %>%
  mutate(out_hosp = Dx_MI-Dx_MI_hosp_prim) %>% 
  select(Region, Age, sex, kreis_t_sett, Nx, aufn_anl, C_hosp_MI_adm)

out_hosp <- 
  h_case_adj_adm %>%
  # mutate(aufn_anl = ifelse(aufn_anl == "A", "X", aufn_anl)) %>% 
  group_by(Region, Age, sex, Nx, aufn_anl, kreis_t_sett, 
           Dx_MI_hosp_prim, Dx_MI) %>%
  summarise(C_hosp_MI_adm = sum(C_hosp_MI_adm)) %>%
  group_by(Region, Age, sex) %>% 
  ungroup() %>%
  mutate(out_hosp = Dx_MI-Dx_MI_hosp_prim) %>% 
  distinct(Region, Age, sex, kreis_t_sett, Nx, out_hosp) %>% 
  mutate(aufn_anl = "D") %>% 
  rename(C_hosp_MI_adm = out_hosp)

# Bind data, calculate rates and distribution parameters

# Single diff
comp_MI <- 
  comp_MI_rate %>% 
  bind_rows(out_hosp) %>% 
  arrange(Region, Age, sex) %>% 
  mutate(aufn_anl = ifelse(aufn_anl == "D", "D", "X")) %>% 
  group_by(Region, Age, sex, aufn_anl, kreis_t_sett, Nx) %>%
  summarise(C_hosp_MI_adm = sum(C_hosp_MI_adm)) %>% 
  filter(kreis_t_sett %in% c(1,4)) %>%
  ungroup() %>% 
  # 15 Region-Age-sex combinations with outside deaths being < 0 
  mutate(C_hosp_MI_adm = ifelse(C_hosp_MI_adm < 0, 0, C_hosp_MI_adm),
         inc = C_hosp_MI_adm / Nx) %>%
  group_by(Age, sex, kreis_t_sett, aufn_anl) %>% 
  summarise(inc_1 = quantile(inc, probs = 0.25),
            inc_5 = quantile(inc, probs = 0.5),
            inc_9 = quantile(inc, probs = 0.75))

save(comp_MI, file = "data_inter/median_rates_in_out_hospital.RData")

pdf("figures/in_out_hospital_eventRates_settlement.pdf", family = "Times", 
    width = 14, pointsize = 15)
layout(cbind(1,2))
par(mar = c(4, 3.5, 2, 1) + 0.1, xpd = TRUE)
plot(x=1, y=1, typ = "n", ylim = c(0.0001, 0.024), xlim = c(0, 6), 
     xlab = NA, ylab = NA, bty = "n", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i")
par(las = 1)
# yy <- seq(0.0001, 0.025, length.out = 7)
yy <- c(0.0001, 0.004, 0.008, 0.012, 0.016, 0.02, 0.024)
axis(2, at = yy, 
     labels = round(yy*10000,1), lwd = 0, pos = 0.08, cex.axis = 1.2)

segments(x0=0:6, y0=0.0001,x1=0:6, y1=0.024, 
         col = "lightgray")
segments(x0=0, y0=yy,x1=6, y1=yy, 
         col = "lightgray")
par(las = 0)
mtext("Events per 10000 PY", 2, cex = 1.3, line = 2.5)
mtext("Age group", 1, cex = 1.3, line = 2.5)

# mtext("Age group", 1, cex = 1.3, line = 3)
axis(1, at = seq(0.5, 5.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84",
                "85-89", "90+"), lwd = 0, cex.axis = 1.2)
# Women
rur_X <- 
  comp_MI %>% 
  filter(sex == "w", kreis_t_sett == 4, aufn_anl == "X")
rur_D <- 
  comp_MI %>% 
  filter(sex == "w", kreis_t_sett == 4, aufn_anl == "D")

urb_X <- 
  comp_MI %>% 
  filter(sex == "w", kreis_t_sett == 1, aufn_anl == "X")
urb_D <- 
  comp_MI %>% 
  filter(sex == "w", kreis_t_sett == 1, aufn_anl == "D")

# Hospitalized events
# IQR Values
xx <- c(seq(0.5, 5.5, by=1), rev(seq(0.5, 5.5, by=1)))
yy <- c(rur_X$inc_1, rev(rur_X$inc_9))
polygon(x=xx, y=yy, col = adjustcolor('#5ab4ac', alpha.f = 0.2),
        border = "white")

xx <- c(seq(0.5, 5.5, by=1), rev(seq(0.5, 5.5, by=1)))
yy <- c(urb_X$inc_1, rev(urb_X$inc_9))
polygon(x=xx, y=yy, col = adjustcolor('#d8b365', alpha.f = 0.2),
        border = "white")

# Median Values
lines(x=seq(0.5, 5.5, by=1), y=rur_X$inc_5, typ = "p", 
      pch = 16, cex = 1.5, col = '#5ab4ac')
lines(x=seq(0.5, 5.5, by=1), y=rur_X$inc_5, typ = "l", 
      lwd = 3, col = '#5ab4ac', lty = 2)

lines(x=seq(0.5, 5.5, by=1), y=urb_X$inc_5, typ = "p", 
      pch = 16, cex = 1.5, col = '#d8b365')
lines(x=seq(0.5, 5.5, by=1), y=urb_X$inc_5, typ = "l", 
      lwd = 3, col = '#d8b365', lty = 2)


# Outside hospital deaths
# IQR Values
xx <- c(seq(0.5, 5.5, by=1), rev(seq(0.5, 5.5, by=1)))
yy <- c(rur_D$inc_1, rev(rur_D$inc_9))
polygon(x=xx, y=yy, col = adjustcolor('#5ab4ac', alpha.f = 0.2),
        border = "white")

xx <- c(seq(0.5, 5.5, by=1), rev(seq(0.5, 5.5, by=1)))
yy <- c(urb_D$inc_1, rev(urb_D$inc_9))
polygon(x=xx, y=yy, col = adjustcolor('#d8b365', alpha.f = 0.2),
        border = "white")

# Median Values
lines(x=seq(0.5, 5.5, by=1), y=rur_D$inc_5, typ = "p", 
      pch = 15, cex = 1.5, col = '#5ab4ac')
lines(x=seq(0.5, 5.5, by=1), y=rur_D$inc_5, typ = "l", 
      lwd = 3, col = '#5ab4ac', lty = 1)

lines(x=seq(0.5, 5.5, by=1), y=urb_D$inc_5, typ = "p", 
      pch = 15, cex = 1.5, col = '#d8b365')
lines(x=seq(0.5, 5.5, by=1), y=urb_D$inc_5, typ = "l", 
      lwd = 3, col = '#d8b365', lty = 1)

mtext("Women", 3, line = -1.5, cex = 1.4, adj = 0.01)
legend(x = 0, y=0.023, legend = c("Rural median of MI events with IQR", 
                              "Urban median of MI events with IQR",
                              "Rural median of MI deaths outside hospital with IQR", 
                              "Urban median of MI deaths outside hospital with IQR"), 
       col = c("#5ab4ac", "#d8b365", "#d8b365", "#5ab4ac"), pch = c(16,16, 15, 15), 
       bty = "n", lty = c(2,2,1,1))

# Men
plot(x=1, y=1, typ = "n", ylim = c(0.0001, 0.024), xlim = c(0, 6), 
     xlab = NA, ylab = NA, bty = "n", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i")
par(las = 1)
# yy <- seq(0.0001, 0.025, length.out = 7)
yy <- c(0.0001, 0.004, 0.008, 0.012, 0.016, 0.02, 0.024)
axis(2, at = yy, 
     labels = round(yy*10000,1), lwd = 0, pos = 0.08, cex.axis = 1.2)

segments(x0=0:6, y0=0.0001,x1=0:6, y1=0.024, 
         col = "lightgray")
segments(x0=0, y0=yy,x1=6, y1=yy, 
         col = "lightgray")
par(las = 0)
# mtext("Events per 10000 PY", 2, cex = 1.3, line = 3)
mtext("Age group", 1, cex = 1.3, line = 2.5)

# mtext("Age group", 1, cex = 1.3, line = 3)
axis(1, at = seq(0.5, 5.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84",
                "85-89", "90+"), lwd = 0, cex.axis = 1.2)
# Women
rur_X <- 
  comp_MI %>% 
  filter(sex == "m", kreis_t_sett == 4, aufn_anl == "X")
rur_D <- 
  comp_MI %>% 
  filter(sex == "m", kreis_t_sett == 4, aufn_anl == "D")

urb_X <- 
  comp_MI %>% 
  filter(sex == "m", kreis_t_sett == 1, aufn_anl == "X")
urb_D <- 
  comp_MI %>% 
  filter(sex == "m", kreis_t_sett == 1, aufn_anl == "D")

# Hospitalized events
# IQR Values
xx <- c(seq(0.5, 5.5, by=1), rev(seq(0.5, 5.5, by=1)))
yy <- c(rur_X$inc_1, rev(rur_X$inc_9))
polygon(x=xx, y=yy, col = adjustcolor('#5ab4ac', alpha.f = 0.2),
        border = "white")

xx <- c(seq(0.5, 5.5, by=1), rev(seq(0.5, 5.5, by=1)))
yy <- c(urb_X$inc_1, rev(urb_X$inc_9))
polygon(x=xx, y=yy, col = adjustcolor('#d8b365', alpha.f = 0.2),
        border = "white")

# Median Values
lines(x=seq(0.5, 5.5, by=1), y=rur_X$inc_5, typ = "p", 
      pch = 16, cex = 1.5, col = '#5ab4ac')
lines(x=seq(0.5, 5.5, by=1), y=rur_X$inc_5, typ = "l", 
      lwd = 3, col = '#5ab4ac', lty = 2)

lines(x=seq(0.5, 5.5, by=1), y=urb_X$inc_5, typ = "p", 
      pch = 16, cex = 1.5, col = '#d8b365')
lines(x=seq(0.5, 5.5, by=1), y=urb_X$inc_5, typ = "l", 
      lwd = 3, col = '#d8b365', lty = 2)


# Outside hospital deaths
# IQR Values
xx <- c(seq(0.5, 5.5, by=1), rev(seq(0.5, 5.5, by=1)))
yy <- c(rur_D$inc_1, rev(rur_D$inc_9))
polygon(x=xx, y=yy, col = adjustcolor('#5ab4ac', alpha.f = 0.2),
        border = "white")

xx <- c(seq(0.5, 5.5, by=1), rev(seq(0.5, 5.5, by=1)))
yy <- c(urb_D$inc_1, rev(urb_D$inc_9))
polygon(x=xx, y=yy, col = adjustcolor('#d8b365', alpha.f = 0.2),
        border = "white")

# Median Values
lines(x=seq(0.5, 5.5, by=1), y=rur_D$inc_5, typ = "p", 
      pch = 15, cex = 1.5, col = '#5ab4ac')
lines(x=seq(0.5, 5.5, by=1), y=rur_D$inc_5, typ = "l", 
      lwd = 3, col = '#5ab4ac', lty = 1)

lines(x=seq(0.5, 5.5, by=1), y=urb_D$inc_5, typ = "p", 
      pch = 15, cex = 1.5, col = '#d8b365')
lines(x=seq(0.5, 5.5, by=1), y=urb_D$inc_5, typ = "l", 
      lwd = 3, col = '#d8b365', lty = 1)

mtext("Men", 3, line = -1.5, cex = 1.4, adj = 0.01)
dev.off()


# Distribution of MI events
?arrange

comp_MI_events <-
  h_case_adj_adm %>%
  group_by(kreis_t_sett, Age, sex, aufn_anl) %>%
  summarise(C_hosp_MI_adm = sum(C_hosp_MI_adm),
            Dx_MI = sum(Dx_MI), 
            Dx_MI_hosp_prim = sum(Dx_MI_hosp_prim)) %>%
  group_by(kreis_t_sett, Age, sex) %>%
  mutate(C_hosp_MI_SumAdm = sum(C_hosp_MI_adm)) %>% 
  ungroup() %>%
  mutate(MI_events = C_hosp_MI_SumAdm+(Dx_MI-Dx_MI_hosp_prim),
         out_hosp = Dx_MI-Dx_MI_hosp_prim,
         case_share = C_hosp_MI_adm / MI_events) %>%
  select(Age, sex, kreis_t_sett, case_share, aufn_anl) %>% 
  filter(kreis_t_sett %in% c(1,4)) %>% 
  arrange(sex, Age, match(aufn_anl, c("N", "X", "A"))) %>% 
  group_by(Age, sex, kreis_t_sett) %>% 
  mutate(cu_share = cumsum(case_share))

col_rur <- adjustcolor(c('#238b45', '#74c476', '#bae4b3', '#edf8e9'),
                       alpha.f = 0.85)
col_urb <- adjustcolor(c('#2171b5', '#6baed6', '#bdd7e7', '#eff3ff'),
                       alpha.f = 0.85)

pdf("figures/distribution_MIevents.pdf", family = "Times", 
    width = 14, pointsize = 16)

layout(rbind(c(1,2), rep(3, 2)),
       heights = c(0.9,0.1))

par(mar = c(3, 4, 2, 1) + 0.1)
# Women
plot(x=1, y=1, typ = "n", ylim = c(0, 1), xlim = c(0, 6), 
     xlab = NA, ylab = NA, bty = "n", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i")

segments(x0=0:6, y0=0, 
         x1=0:6, y1=1, col = "lightgray")
segments(x0=0, y0=seq(0,1,by = 0.1), 
         x1=6, y1=seq(0,1,by = 0.1), col = "lightgray")

xx_rur <- cbind(seq(0.1, 5.1, by = 1),
                seq(0.1, 5.1, by = 1),
                seq(0.5, 5.5, by = 1),
                seq(0.5, 5.5, by = 1))
                
xx_urb <- cbind(seq(0.5, 5.5, by = 1),
                seq(0.5, 5.5, by = 1),
                seq(0.9, 5.9, by = 1),
                seq(0.9, 5.9, by = 1))
ages <- seq(65,90, by = 5)

for(k in 1:6){

rur <- comp_MI_events %>% filter(sex == "w" & 
                                   Age == ages[k] & 
                                   kreis_t_sett == 4)

urb <- comp_MI_events %>% filter(sex == "w" & 
                                   Age == ages[k] & 
                                   kreis_t_sett == 1)

yy_rur <- rbind(c(0, rep(rur$cu_share[1],2), 0),
                c(rur$cu_share[1], rep(rur$cu_share[2],2), rur$cu_share[1]),
                c(rur$cu_share[2], rep(rur$cu_share[3],2), rur$cu_share[2]),
                c(rur$cu_share[3], rep(1,2), rur$cu_share[3]))

yy_urb <- rbind(c(0, rep(urb$cu_share[1],2), 0),
                c(urb$cu_share[1], rep(urb$cu_share[2],2), urb$cu_share[1]),
                c(urb$cu_share[2], rep(urb$cu_share[3],2), urb$cu_share[2]),
                c(urb$cu_share[3], rep(1,2), urb$cu_share[3]))

for(type in 1:4){
  polygon(x=xx_rur[k,], y=yy_rur[type,], col = col_rur[type], 
          border = "white")
  polygon(x=xx_urb[k,], y=yy_urb[type,], col = col_urb[type], 
          border = "white")
}
}

par(las = 1)
axis(2, at = seq(0, 1, by = 0.1), lwd = 3, cex.axis = 1.2)
axis(1, at = seq(0.5, 5.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84",
                "85-89", "90+"), lwd = 0, cex.axis = 1.2)
mtext("Women", 3, line = 0.5, cex = 1.4)

par(las = 0)
mtext("Proportion on MI events", 2, line = 2.4, cex = 1.2)
text(x=0.4, y=0.1, "Rural", cex = 1.1, srt = 90, pos = 3)
text(x=0.8, y=0.1, "Urban", cex = 1.1, srt = 90, pos = 3)

# Men
plot(x=1, y=1, typ = "n", ylim = c(0, 1), xlim = c(0, 6), 
     xlab = NA, ylab = NA, bty = "n", xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i")

segments(x0=0:6, y0=0, 
         x1=0:6, y1=1, col = "lightgray")
segments(x0=0, y0=seq(0,1,by = 0.1), 
         x1=6, y1=seq(0,1,by = 0.1), col = "lightgray")

xx_rur <- cbind(seq(0.1, 5.1, by = 1),
                seq(0.1, 5.1, by = 1),
                seq(0.5, 5.5, by = 1),
                seq(0.5, 5.5, by = 1))

xx_urb <- cbind(seq(0.5, 5.5, by = 1),
                seq(0.5, 5.5, by = 1),
                seq(0.9, 5.9, by = 1),
                seq(0.9, 5.9, by = 1))
ages <- seq(65,90, by = 5)

for(k in 1:6){
  
  rur <- comp_MI_events %>% filter(sex == "m" & 
                                     Age == ages[k] & 
                                     kreis_t_sett == 4)
  
  urb <- comp_MI_events %>% filter(sex == "m" & 
                                     Age == ages[k] & 
                                     kreis_t_sett == 1)
  
  yy_rur <- rbind(c(0, rep(rur$cu_share[1],2), 0),
                  c(rur$cu_share[1], rep(rur$cu_share[2],2), rur$cu_share[1]),
                  c(rur$cu_share[2], rep(rur$cu_share[3],2), rur$cu_share[2]),
                  c(rur$cu_share[3], rep(1,2), rur$cu_share[3]))
  
  yy_urb <- rbind(c(0, rep(urb$cu_share[1],2), 0),
                  c(urb$cu_share[1], rep(urb$cu_share[2],2), urb$cu_share[1]),
                  c(urb$cu_share[2], rep(urb$cu_share[3],2), urb$cu_share[2]),
                  c(urb$cu_share[3], rep(1,2), urb$cu_share[3]))
  
  for(type in 1:4){
    polygon(x=xx_rur[k,], y=yy_rur[type,], col = col_rur[type], 
            border = "white")
    polygon(x=xx_urb[k,], y=yy_urb[type,], col = col_urb[type], 
            border = "white")
  }
}

par(las = 1)
axis(2, at = seq(0, 1, by = 0.1), lwd = 3, cex.axis = 1.2)
axis(1, at = seq(0.5, 5.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84",
                "85-89", "90+"), lwd = 0, cex.axis = 1.2)
mtext("Men", 3, line = 0.5, cex = 1.4)

par(las = 0)
# mtext("Proportion on MI events", 2, line = 2.4, cex = 1.2)

## Legend
par(xpd = TRUE, mar = c(1,1,1,1))
plot(x=1:7, y=1:7, xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n",
     bty = "n", xlab = NA, ylab = NA, typ = "n", xlim =
       c(0,1), ylim = c(0,1))


legend(x=0.15, y= 2.2, legend =c("emergency admission", "other admission", 
                                 "inter-hospital transfer", "death outside hospital"), 
       col = c('#238b45', '#74c476', '#bae4b3', '#edf8e9'), 
       bty = "n", cex = 1.3, pch = 15, horiz = TRUE, pt.cex = 1.8,
       text.col = "white")


legend(x=0.15, y= 1.4, legend =c("emergency admission", "other admission", 
                                 "inter-hospital transfer", "death outside hospital"), 
       col = c('#2171b5', '#6baed6', '#bdd7e7', '#eff3ff'), 
       bty = "n", cex = 1.3, pch = 15, horiz = TRUE, pt.cex = 1.8,
       text.col = "white")

legend(x=0.15, y= 1.75, legend =c("emergency admission", "other admission", 
                                  "inter-hospital transfer", "death outside hospital"), 
       col = NA, 
       bty = "n", cex = 1.3, pch = 15, horiz = TRUE, pt.cex = 1.8,
       text.col = "black")

dev.off()
