# Calculate median death rates total and by cause of death
source("code/packages_functions.R")
load("data_inter/hospCase_deaths.RData")

# prepare data
Dx_data <- 
  hosp %>% 
  group_by(Age, sex, Region) %>% 
  mutate(mx_all = Dx_tot / Nx,
         mx_CVD = Dx_CVD / Nx,
         mx_MI = (Dx_MI-Dx_MI_hosp_prim+Dx_hosp_MI_sec) / Nx,
         test = (Dx_MI-Dx_MI_hosp_prim+Dx_hosp_MI_sec),
         mx_hosp = Dx_hosp_MI_sec / Nx, 
         PD = bev17/area) %>% 
  group_by(sex, Region) %>% 
  mutate(e65 = lifetable(mx_all)) %>% 
  group_by(sex) %>% 
  mutate(e65_cat = cut(e65, 
                       breaks = quantile(e65, probs = c(0,1/3, 2/3, 1)), 
                       labels = 1:3, 
                       include.lowest = TRUE), 
         PD_cat = cut(PD, 
                      breaks = c(0,300, 1000, 20000), 
                      labels = 1:3, 
                      include.lowest = TRUE))

# Life expectancy distribution
e65 <- 
  Dx_data %>% 
  distinct(Region, sex, kreis_t_sett, e65) %>% 
  group_by(sex, kreis_t_sett) %>% 
  summarise(m_e65 = mean(e65), 
            sd65 = sd(e65)) %>% 
  filter(kreis_t_sett %in% c(1,4))

# Bootstrap to get average death rates
# Function performing bootstrap and difference calculation
# x <- Dx_data %>% filter(Age == 65 & sex == "w")

boot_mx_kr_sett <- function(x){
  
  # Select rural urban data
  ru <- x %>% filter(kreis_t_sett == 4)
  ur <- x %>% filter(kreis_t_sett == 1)
  
  # Create set of boootstraped death counts and death rates
  # Calculate mean
  
  # Total mx
  mean_ru <- unlist(Map(1:10000, f = function(x) 
    quantile(rpois(n = length(ru$Dx_tot), lambda = ru$Dx_tot)/ru$Nx, probs = 0.5)))
  mean_ur <- unlist(Map(1:10000, f = function(x) 
    quantile(rpois(n = length(ur$Dx_tot), lambda = ur$Dx_tot)/ur$Nx, probs = 0.5)))
  
  # CVD
  mean_ru_CVD <- unlist(Map(1:10000, f = function(x) 
    quantile(rpois(n = length(ru$Dx_CVD), lambda = ru$Dx_CVD)/ru$Nx, probs = 0.5)))
  mean_ur_CVD <- unlist(Map(1:10000, f = function(x) 
    quantile(rpois(n = length(ur$Dx_CVD), lambda = ur$Dx_CVD)/ur$Nx, probs = 0.5)))
  
  # MI
  mean_ru_MI <- unlist(Map(1:10000, f = function(x) 
    quantile(rpois(n = length(ru$Dx_MI), lambda = (ru$Dx_MI-ru$Dx_MI_hosp_prim+ru$Dx_hosp_MI_sec))/ru$Nx, probs = 0.5, 
             na.rm = TRUE)))
  mean_ur_MI <- unlist(Map(1:10000, f = function(x) 
    quantile(rpois(n = length(ur$Dx_MI), lambda = (ur$Dx_MI-ur$Dx_MI_hosp_prim+ur$Dx_hosp_MI_sec))/ur$Nx, probs = 0.5)))
  
  # Hosp
  mean_ru_ho <- unlist(Map(1:10000, f = function(x) 
    quantile(rpois(n = length(ru$Dx_hosp_MI_sec), lambda = ru$Dx_hosp_MI_sec)/ru$Nx, probs = 0.5)))
  mean_ur_ho <- unlist(Map(1:10000, f = function(x) 
    quantile(rpois(n = length(ur$Dx_hosp_MI_sec), lambda = ur$Dx_hosp_MI_sec)/ur$Nx, probs = 0.5)))
  
  # Quantile values across relative differences
  distDiff_t <- quantile((mean_ru/mean_ur)-1, probs = c(0.025, 0.5, 0.975))
  distDiff_CVD <- quantile((mean_ru_CVD/mean_ur_CVD)-1, probs = c(0.025, 0.5, 0.975))
  distDiff_MI <- quantile((mean_ru_MI/mean_ur_MI)-1, probs = c(0.025, 0.5, 0.975))
  distDiff_hosp <- quantile((mean_ru_ho/mean_ur_ho)-1, probs = c(0.025, 0.5, 0.975))

  # Function
  out <- tibble(Age = x$Age[1], sex = x$sex[1], 
                di_t_25 = distDiff_t["2.5%"],
                di_t_5 = distDiff_t["50%"], 
                di_t_97 = distDiff_t["97.5%"],
                di_CVD_25 = distDiff_CVD["2.5%"],
                di_CVD_5 = distDiff_CVD["50%"], 
                di_CVD_97 = distDiff_CVD["97.5%"],
                di_MI_25 = distDiff_MI["2.5%"],
                di_MI_5 = distDiff_MI["50%"], 
                di_MI_97 = distDiff_MI["97.5%"],
                di_hosp_25 = distDiff_hosp["2.5%"],
                di_hosp_5 = distDiff_hosp["50%"], 
                di_hosp_97 = distDiff_hosp["97.5%"])
  return(out)
}


median_mx <- 
  Dx_data %>% 
  group_by(sex, Age) %>% 
  group_map(~boot_mx_kr_sett(x=.), .keep = TRUE) %>% 
  bind_rows()

# Plot for median rates
basic_bar_wom <- function(){
  plot(x=1, y=1, typ = "n", lwd = 3, xlim = c(-0.4, 0.4), ylim = c(0,6), 
       bty = "n", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", 
       xlab = NA, ylab = NA)
  segments(x0=seq(-0.4, 0.4, by = 0.1), y0=0,
           x1=seq(-0.4, 0.4, by = 0.1), y1=6, 
           col = "lightgray")
  axis(1, at = seq(-0.4, 0.4, by = 0.1), 
       labels = c("40%", "30%", "20%", "10%", "0%", "10%", "20%", "30%", "40%"), 
       lwd = 3, pos = -0.05, cex.axis = 1.5)
  axis(1, at = seq(-0.35, 0.35, by = 0.5), 
       labels = FALSE, 
       lwd = 1, pos = -0.05)
  par(las = 1)
  axis(2, at = seq(0.5, 5.5, by = 1), 
       labels = c("65-69", "70-74", "75-79",
                  "80-84", "85-89", "90+"), 
       lwd = 0, cex.axis = 1.5, pos = -0.39)
  
  # mtext("Relative mortality difference", 1,  line = 3.5, cex = 1.4)
  mtext("Urban excess", 1, line = -1, cex = 1.3, adj = 0.01)
  mtext("Rural excess", 1, line = -1, cex = 1.3, adj = 0.99)
}

basic_bar <- function(){
  plot(x=1, y=1, typ = "n", lwd = 3, xlim = c(-0.4, 0.4), ylim = c(0,6), 
       bty = "n", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", 
       xlab = NA, ylab = NA)
  segments(x0=seq(-0.4, 0.4, by = 0.1), y0=0,
           x1=seq(-0.4, 0.4, by = 0.1), y1=6, 
           col = "lightgray")
  axis(1, at = seq(-0.4, 0.4, by = 0.1), 
       labels = c("40%", "30%", "20%", "10%", "0%", "10%", "20%", "30%", "40%"), 
       lwd = 3, pos = -0.05, cex.axis = 1.5)
  axis(1, at = seq(-0.35, 0.35, by = 0.1), 
       labels = FALSE, 
       lwd = 1, pos = -0.05)
  par(las = 1)
  axis(2, at = seq(0.5, 5.5, by = 1), 
       labels = c("65-69", "70-74", "75-79",
                  "80-84", "85-89", "90+"), 
       lwd = 0, cex.axis = 1.5, pos = -0.39)
  
  mtext("Relative mortality difference", 1,  line = 3.5, cex = 1.4)
  mtext("Urban excess", 1, line = -1, cex = 1.3, adj = 0.01)
  mtext("Rural excess", 1, line = -1, cex = 1.3, adj = 0.99)
}

# dev.off()
pdf("figures/bar_plot_mortDiff_settlement.pdf", family = "Times", 
    width = 21, height = 14, pointsize = 16)
layout(rbind(c(1:3), c(4:6)))
ages <- seq(65, 90, by = 5)
y_cord <- cbind(0:5, 1:6)
rownames(y_cord) <- ages

# Women Total Death Rates
par(mar = c(4, 4, 5, 2) + 0.1)
basic_bar_wom()
mtext("Women", 3, line = -1, adj = 0.01, cex = 1.3)
mtext("All-cause deaths", 3, line = 1, cex = 1.5)
par(xpd = TRUE)
wom <- median_mx %>% filter(sex == "w")
for(i in ages){
  plotDat <- wom %>% 
    filter(Age == i) %>% 
    rename(med = di_t_5, 
           low = di_t_25, 
           up = di_t_97)
  yy <- c(rep(y_cord[paste(i),1],2),
          rep(y_cord[paste(i),2],2))
  xx <- c(0, rep(plotDat$med,2), 0)
  polygon(x=xx, y=yy, col ='#bdbdbd', 
          border = "white")
  lines(x=c(plotDat$low, plotDat$up), 
        y= rep(y_cord[paste(i),1]+0.5,2), 
        col = '#636363', pch = 16, cex = 1.4, 
        typ = "p")
  lines(x=c(plotDat$low, plotDat$up), 
        y= rep(y_cord[paste(i),1]+0.5,2), 
        col = '#636363', lwd = 2)
}

# Women CVD Death Rates
basic_bar_wom()
mtext("CVD deaths", 3, line = 1, cex = 1.5)
par(xpd = TRUE)
wom <- median_mx %>% filter(sex == "w")
for(i in ages){
  plotDat <- wom %>% 
    filter(Age == i) %>% 
    rename(med = di_CVD_5, 
           low = di_CVD_25, 
           up = di_CVD_97)
  yy <- c(rep(y_cord[paste(i),1],2),
          rep(y_cord[paste(i),2],2))
  xx <- c(0, rep(plotDat$med,2), 0)
  polygon(x=xx, y=yy, col ='#bdbdbd', 
          border = "white")
  lines(x=c(plotDat$low, plotDat$up), 
        y= rep(y_cord[paste(i),1]+0.5,2), 
        col = '#636363', pch = 16, cex = 1.4, 
        typ = "p")
  lines(x=c(plotDat$low, plotDat$up), 
        y= rep(y_cord[paste(i),1]+0.5,2), 
        col = '#636363', lwd = 2)
}

# Women MI Death Rates
basic_bar_wom()
mtext("MI-related deaths", 3, line = 1, cex = 1.5)
par(xpd = TRUE)
wom <- median_mx %>% filter(sex == "w")
for(i in ages){
  plotDat <- wom %>% 
    filter(Age == i) %>% 
    rename(med = di_MI_5, 
           low = di_MI_25, 
           up = di_MI_97)
  yy <- c(rep(y_cord[paste(i),1],2),
          rep(y_cord[paste(i),2],2))
  xx <- c(0, rep(plotDat$med,2), 0)
  polygon(x=xx, y=yy, col ='#bdbdbd', 
          border = "white")
  lines(x=c(plotDat$low, plotDat$up), 
        y= rep(y_cord[paste(i),1]+0.5,2), 
        col = '#636363', pch = 16, cex = 1.4, 
        typ = "p")
  lines(x=c(plotDat$low, plotDat$up), 
        y= rep(y_cord[paste(i),1]+0.5,2), 
        col = '#636363', lwd = 2)
}
# 
# # Women MI Death Rates In-hospital
# basic_bar_wom()
# mtext("MI-related in-hospital deaths", 3, line = 1, cex = 1.5)
# par(xpd = TRUE)
# wom <- median_mx %>% filter(sex == "w")
# for(i in ages){
#   plotDat <- wom %>% 
#     filter(Age == i) %>% 
#     rename(med = di_hosp_5, 
#            low = di_hosp_25, 
#            up = di_hosp_97)
#   yy <- c(rep(y_cord[paste(i),1],2),
#           rep(y_cord[paste(i),2],2))
#   xx <- c(0, rep(plotDat$med,2), 0)
#   polygon(x=xx, y=yy, col ='#bdbdbd', 
#           border = "white")
#   lines(x=c(plotDat$low, plotDat$up), 
#         y= rep(y_cord[paste(i),1]+0.5,2), 
#         col = '#636363', pch = 16, cex = 1.4, 
#         typ = "p")
#   lines(x=c(plotDat$low, plotDat$up), 
#         y= rep(y_cord[paste(i),1]+0.5,2), 
#         col = '#636363', lwd = 2)
# }

# Men
par(mar = c(7, 4, 2, 2) + 0.1)
basic_bar()
mtext("Men", 3, line = -1, adj = 0.01, cex = 1.3)
# mtext("All-cause deaths", 3, line = 1, cex = 1.5)
par(xpd = TRUE)
wom <- median_mx %>% filter(sex == "m")
for(i in ages){
  plotDat <- wom %>% 
    filter(Age == i) %>% 
    rename(med = di_t_5, 
           low = di_t_25, 
           up = di_t_97)
  yy <- c(rep(y_cord[paste(i),1],2),
          rep(y_cord[paste(i),2],2))
  xx <- c(0, rep(plotDat$med,2), 0)
  polygon(x=xx, y=yy, col ='#bdbdbd', 
          border = "white")
  lines(x=c(plotDat$low, plotDat$up), 
        y= rep(y_cord[paste(i),1]+0.5,2), 
        col = '#636363', pch = 16, cex = 1.4, 
        typ = "p")
  lines(x=c(plotDat$low, plotDat$up), 
        y= rep(y_cord[paste(i),1]+0.5,2), 
        col = '#636363', lwd = 2)
}

# Women CVD Death Rates
basic_bar()
# mtext("CVD deaths (I00-I99)", 3, line = 1, cex = 1.5)
par(xpd = TRUE)
wom <- median_mx %>% filter(sex == "m")
for(i in ages){
  plotDat <- wom %>% 
    filter(Age == i) %>% 
    rename(med = di_CVD_5, 
           low = di_CVD_25, 
           up = di_CVD_97)
  yy <- c(rep(y_cord[paste(i),1],2),
          rep(y_cord[paste(i),2],2))
  xx <- c(0, rep(plotDat$med,2), 0)
  polygon(x=xx, y=yy, col ='#bdbdbd', 
          border = "white")
  lines(x=c(plotDat$low, plotDat$up), 
        y= rep(y_cord[paste(i),1]+0.5,2), 
        col = '#636363', pch = 16, cex = 1.4, 
        typ = "p")
  lines(x=c(plotDat$low, plotDat$up), 
        y= rep(y_cord[paste(i),1]+0.5,2), 
        col = '#636363', lwd = 2)
}

# Women MI Death Rates
basic_bar()
# mtext("MI deaths (I21-I22)", 3, line = 1, cex = 1.5)
par(xpd = TRUE)
wom <- median_mx %>% filter(sex == "m")
for(i in ages){
  plotDat <- wom %>% 
    filter(Age == i) %>% 
    rename(med = di_MI_5, 
           low = di_MI_25, 
           up = di_MI_97)
  yy <- c(rep(y_cord[paste(i),1],2),
          rep(y_cord[paste(i),2],2))
  xx <- c(0, rep(plotDat$med,2), 0)
  polygon(x=xx, y=yy, col ='#bdbdbd', 
          border = "white")
  lines(x=c(plotDat$low, plotDat$up), 
        y= rep(y_cord[paste(i),1]+0.5,2), 
        col = '#636363', pch = 16, cex = 1.4, 
        typ = "p")
  lines(x=c(plotDat$low, plotDat$up), 
        y= rep(y_cord[paste(i),1]+0.5,2), 
        col = '#636363', lwd = 2)
}

# # Women MI Death Rates In-hospital
# basic_bar()
# # mtext("MI in-hospital deaths", 3, line = 1, cex = 1.5)
# par(xpd = TRUE)
# wom <- median_mx %>% filter(sex == "m")
# for(i in ages){
#   plotDat <- wom %>% 
#     filter(Age == i) %>% 
#     rename(med = di_hosp_5, 
#            low = di_hosp_25, 
#            up = di_hosp_97)
#   yy <- c(rep(y_cord[paste(i),1],2),
#           rep(y_cord[paste(i),2],2))
#   xx <- c(0, rep(plotDat$med,2), 0)
#   polygon(x=xx, y=yy, col ='#bdbdbd', 
#           border = "white")
#   lines(x=c(plotDat$low, plotDat$up), 
#         y= rep(y_cord[paste(i),1]+0.5,2), 
#         col = '#636363', pch = 16, cex = 1.4, 
#         typ = "p")
#   lines(x=c(plotDat$low, plotDat$up), 
#         y= rep(y_cord[paste(i),1]+0.5,2), 
#         col = '#636363', lwd = 2)
# }
dev.off()


