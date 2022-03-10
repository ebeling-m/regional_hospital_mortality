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

mean_mx_wo_e65 <- 
  mx_tot %>% 
  group_by(sex, Age) %>% 
  group_map(~boot_mx_kr_sett(x=.), .keep = TRUE) %>% 
  bind_rows()


# Calculate difference for MI and stroke together
mx_cvd <- 
  death_dat %>% 
  group_by(Age, sex, Region) %>% 
  mutate(Dx_t = sum(D),
         mx = Dx_t/Nx) %>%
  filter(dia %in% c(1,2)) %>% 
  group_by(sex, Age, Region) %>%
  mutate(D_CVD = sum(D)) %>% 
  distinct(Age, sex, Region, Dx_t, Nx, kreis_t_sett, RurUrb, mx, D_CVD) %>% 
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
  mx_cvd %>% 
  group_by(sex, Age, e65_cat) %>% 
  group_map(~boot_mx_kr_sett_CVD(x=.), .keep = TRUE) %>% 
  bind_rows()


# Calculate difference for cancer
mx_NEO <- 
  death_dat %>% 
  group_by(Age, sex, Region) %>% 
  mutate(Dx_t = sum(D),
         mx = Dx_t/Nx) %>%
  filter(dia %in% 3) %>% 
  group_by(sex, Age, Region) %>%
  mutate(D_NEO = sum(D)) %>% 
  distinct(Age, sex, Region, Dx_t, Nx, kreis_t_sett, RurUrb, mx, D_NEO) %>% 
  group_by(sex, Region) %>%
  mutate(e65 = lifetable(mx)) %>% 
  group_by(sex) %>% 
  mutate(e65_cat = cut(e65, 
                       breaks = quantile(e65, probs = c(0,1/3, 2/3, 1)), 
                       labels = 1:3, 
                       include.lowest = TRUE))

boot_mx_kr_sett_NEO <- function(x){
  
  # Select rural urban data
  ru <- x %>% filter(kreis_t_sett == 4)
  ur <- x %>% filter(kreis_t_sett == 1)
  
  # Create set of boootstraped death counts and death rates
  # Calculate mean
  mean_ru <- unlist(Map(1:10000, f = function(x) 
    mean(rpois(n = length(ru$D_NEO), lambda = ru$D_NEO)/ru$Nx)))
  
  mean_ur <- unlist(Map(1:10000, f = function(x) 
    mean(rpois(n = length(ur$D_NEO), lambda = ur$D_NEO)/ur$Nx)))
  
  # Quantile values across relative differences
  distDiff <- quantile((mean_ru/mean_ur)-1, probs = c(0.025, 0.5, 0.975))
  
  # Function
  out <- tibble(Age = x$Age[1], sex = x$sex[1], e65_cat = x$e65_cat[1], 
                mx_low = distDiff["2.5%"],
                mx_med = distDiff["50%"], mx_up = distDiff["97.5%"])
  return(out)
}

mean_mx_NEO <- 
  mx_NEO %>% 
  group_by(sex, Age, e65_cat) %>% 
  group_map(~boot_mx_kr_sett_NEO(x=.), .keep = TRUE) %>% 
  bind_rows()


# Plot relative average differences

# Plot outline
plot_outline <- function(){
plot(x=1, y=1, typ = "n", bty = "n", xaxt = "n", yaxt = "n", xaxs = "i",
     yaxs = "i", xlab = NA, ylab = NA, ylim = c(0,7), xlim = c(-0.3, 0.3))

# Axis and grid lines
segments(x0=seq(-0.3, 0.3, by = 0.1), y0=0, 
         x1=seq(-0.3, 0.3, by = 0.1), y1=6, col = "gray", lwd = 1)
segments(x0=seq(-0.25, 0.25, by = 0.1), y0=0, 
         x1=seq(-0.25, 0.25, by = 0.1), y1=6, col = "gray", lwd = 0.5)
segments(x0=0, y0=0, 
         x1=0, y1=6, col = "gray", lwd = 2)
segments(x0=-0.3, y0=seq(0, 6, by = 1), 
         x1=0.3, y1=seq(0, 6, by = 1), col = "gray", lwd = 1)

axis(1, at = seq(-0.3, 0.3, by = 0.1), 
     labels = paste(seq(-30, 30, by = 10), "%", sep = ""), 
     lwd = 3)
axis(1, at = seq(-0.25, 0.25, by = 0.1), labels = FALSE, 
     lwd = 1)
# Labels
axis(2, at = seq(0.5, 5.5, by = 1), 
     labels = c("65-69", "70-74", "75-79", "80-84", "85-89", "90+"), lwd = 0)

arrows(x0=-0.01, y0=6.5, x1=-0.3, y1=6.5, lwd = 2)
arrows(x0=0.01, y0=6.5, x1=0.3, y1=6.5, lwd = 2)

text(x=-0.15, y=6.5, "Urban mortality excess", pos = 3, xpd = TRUE)
text(x=0.15, y=6.5, "Rural mortality excess", pos = 3, xpd = TRUE)
mtext("Age group", 2, line = 2.8, cex = 1.3)
mtext("Relative mortality difference", 1, line = 2.8, cex = 1.3)
}


# plot total unadjusted
dev.off()

# Women
pdf("figures/mx_diff_woE65_women.pdf", family = "Times", width = 10, pointsize = 14)
plot_outline()

plotDat <- 
  mean_mx_wo_e65 %>% 
  filter(sex == "w")

lines(x=plotDat$mx_med, y= seq(0.5, 5.5, by = 1), typ = "p", pch = 16, cex = 1.5)
lines(x=plotDat$mx_med, y= seq(0.5, 5.5, by = 1), typ = "l", lwd = 1)

# CI
lines(x=plotDat$mx_low, y= seq(0.5, 5.5, by = 1), typ = "p", pch = 16, cex = 0.8,
      col = "lightgray")
lines(x=plotDat$mx_low, y= seq(0.5, 5.5, by = 1), typ = "l", lwd = 1, lty = 2, 
      col = "lightgray")

lines(x=plotDat$mx_up, y= seq(0.5, 5.5, by = 1), typ = "p", pch = 16, cex = 0.8,
      col = "lightgray")
lines(x=plotDat$mx_up, y= seq(0.5, 5.5, by = 1), typ = "l", lwd = 1, lty = 2, 
      col = "lightgray")
mtext("All-cause mortality, NUTS-3 areas, Women, 2012-2018", 3, cex = 1.5, line = 1)
dev.off()

# Men
pdf("figures/mx_diff_woE65_men.pdf", family = "Times", width = 10, pointsize = 14)
plot_outline()

plotDat <- 
  mean_mx_wo_e65 %>% 
  filter(sex == "m")

lines(x=plotDat$mx_med, y= seq(0.5, 5.5, by = 1), typ = "p", pch = 16, cex = 1.5)
lines(x=plotDat$mx_med, y= seq(0.5, 5.5, by = 1), typ = "l", lwd = 1)

# CI
lines(x=plotDat$mx_low, y= seq(0.5, 5.5, by = 1), typ = "p", pch = 16, cex = 0.8,
      col = "lightgray")
lines(x=plotDat$mx_low, y= seq(0.5, 5.5, by = 1), typ = "l", lwd = 1, lty = 2, 
      col = "lightgray")

lines(x=plotDat$mx_up, y= seq(0.5, 5.5, by = 1), typ = "p", pch = 16, cex = 0.8,
      col = "lightgray")
lines(x=plotDat$mx_up, y= seq(0.5, 5.5, by = 1), typ = "l", lwd = 1, lty = 2, 
      col = "lightgray")
mtext("All-cause mortality, NUTS-3 areas, Men, 2012-2018", 3, cex = 1.5, line = 1)
dev.off()

# plot total adjusted
col_LE <- c('#a1dab4','#41b6c4','#225ea8')
pdf("figures/mx_diff_E65_women.pdf", family = "Times", width = 10, pointsize = 14)
plot_outline()

for(i in 1:3){
plotDat <- 
  mean_mx %>% 
  filter(sex == "w" & e65_cat == i)

lines(x=plotDat$mx_med, y= seq(0.5, 5.5, by = 1), typ = "p", pch = 16, 
      cex = 1.5, col = col_LE[i])
lines(x=plotDat$mx_med, y= seq(0.5, 5.5, by = 1), typ = "l", lwd = 1,
      col = col_LE[i])
}
mtext("All-cause mortality, NUTS-3 areas by level of LE, Women, 2012-2018", 3, cex = 1.5, line = 1)
legend("bottomright", legend = c("Low LE", "Medium LE", "High LE"), col = col_LE, 
       pch = 16, cex = 1.1, bty = "n")

dev.off()

pdf("figures/mx_diff_E65_men.pdf", family = "Times", width = 10, pointsize = 14)
plot_outline()

for(i in 1:3){
  plotDat <- 
    mean_mx %>% 
    filter(sex == "m" & e65_cat == i)
  
  lines(x=plotDat$mx_med, y= seq(0.5, 5.5, by = 1), typ = "p", pch = 16, 
        cex = 1.5, col = col_LE[i])
  lines(x=plotDat$mx_med, y= seq(0.5, 5.5, by = 1), typ = "l", lwd = 1,
        col = col_LE[i])
}
mtext("All-cause mortality, NUTS-3 areas by level of LE, Men, 2012-2018", 3, cex = 1.5, line = 1)
legend("bottomright", legend = c("Low LE", "Medium LE", "High LE"), col = col_LE, 
       pch = 16, cex = 1.1, bty = "n")

dev.off()



# plot cvd adjusted
col_LE <- c('#a1dab4','#41b6c4','#225ea8')
pdf("figures/mx_diff_E65_CVD_women.pdf", family = "Times", width = 10, pointsize = 14)
plot_outline()

for(i in 1:3){
  plotDat <- 
    mean_mx_CVD %>% 
    filter(sex == "w" & e65_cat == i)
  
  lines(x=plotDat$mx_med, y= seq(0.5, 5.5, by = 1), typ = "p", pch = 16, 
        cex = 1.5, col = col_LE[i])
  lines(x=plotDat$mx_med, y= seq(0.5, 5.5, by = 1), typ = "l", lwd = 1,
        col = col_LE[i])
}
mtext("MI/stroke mortality, NUTS-3 areas by level of LE, Women, 2012-2018", 3, cex = 1.5, line = 1)
legend("bottomright", legend = c("Low LE", "Medium LE", "High LE"), col = col_LE, 
       pch = 16, cex = 1.1, bty = "n")

dev.off()

pdf("figures/mx_diff_E65_CVD_men.pdf", family = "Times", width = 10, pointsize = 14)
plot_outline()

for(i in 1:3){
  plotDat <- 
    mean_mx_CVD %>% 
    filter(sex == "m" & e65_cat == i)
  
  lines(x=plotDat$mx_med, y= seq(0.5, 5.5, by = 1), typ = "p", pch = 16, 
        cex = 1.5, col = col_LE[i])
  lines(x=plotDat$mx_med, y= seq(0.5, 5.5, by = 1), typ = "l", lwd = 1,
        col = col_LE[i])
}
mtext("MI/stroke mortality, NUTS-3 areas by level of LE, Men, 2012-2018", 3, cex = 1.5, line = 1)
legend("bottomright", legend = c("Low LE", "Medium LE", "High LE"), col = col_LE, 
       pch = 16, cex = 1.1, bty = "n")

dev.off()


# plot cancer adjusted
col_LE <- c('#a1dab4','#41b6c4','#225ea8')
pdf("figures/mx_diff_E65_NEO_women.pdf", family = "Times", width = 10, pointsize = 14)
plot_outline()

for(i in 1:3){
  plotDat <- 
    mean_mx_NEO %>% 
    filter(sex == "w" & e65_cat == i)
  
  lines(x=plotDat$mx_med, y= seq(0.5, 5.5, by = 1), typ = "p", pch = 16, 
        cex = 1.5, col = col_LE[i])
  lines(x=plotDat$mx_med, y= seq(0.5, 5.5, by = 1), typ = "l", lwd = 1,
        col = col_LE[i])
}
mtext("Cancer mortality, NUTS-3 areas by level of LE, Women, 2012-2018", 3, cex = 1.5, line = 1)
legend("bottomright", legend = c("Low LE", "Medium LE", "High LE"), col = col_LE, 
       pch = 16, cex = 1.1, bty = "n")

dev.off()

pdf("figures/mx_diff_E65_NEO_men.pdf", family = "Times", width = 10, pointsize = 14)
plot_outline()

for(i in 1:3){
  plotDat <- 
    mean_mx_NEO %>% 
    filter(sex == "m" & e65_cat == i)
  
  lines(x=plotDat$mx_med, y= seq(0.5, 5.5, by = 1), typ = "p", pch = 16, 
        cex = 1.5, col = col_LE[i])
  lines(x=plotDat$mx_med, y= seq(0.5, 5.5, by = 1), typ = "l", lwd = 1,
        col = col_LE[i])
}
mtext("Cancer mortality, NUTS-3 areas by level of LE, Men, 2012-2018", 3, cex = 1.5, line = 1)
legend("bottomright", legend = c("Low LE", "Medium LE", "High LE"), col = col_LE, 
       pch = 16, cex = 1.1, bty = "n")

dev.off()
