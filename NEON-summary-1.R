

path.neon <- "D:/CSU/Fall 2022/ECOL 610 - Ecosystem Ecology/neon_shiny/data"
list.files(path.neon)


x <- read.csv("D:/CSU/Fall 2022/ECOL 610 - Ecosystem Ecology/neon_shiny/data/neon_daily.csv")
template <- read.csv("D:/CSU/Fall 2022/ECOL 610 - Ecosystem Ecology/NEON_summary_template.csv")
names(template)
################################################################################


library(ggplot2)
library(dplyr)
names(x)


x <- x %>% 
  filter(site == "Harvard Forest") %>% 
  mutate(TIME = 1:nrow(.))

x <- x %>% 
  mutate(season = ifelse(x$Month %in% c(11:12,1:2), "Winter", 1))
x <- x %>% 
  mutate(season = ifelse(x$Month %in% c(3,4,5), "Spring", x$season)) 
x <- x %>% 
  mutate(season = ifelse(x$Month %in% c(6:8), "Summer", x$season))
x <- x %>% 
  mutate(season = ifelse(x$Month %in% c(9:10), "Fall", x$season))

winter <- x %>% 
  filter(season == "Winter")
spring <- x %>% 
  filter(season == "Spring")
summer <- x %>% 
  filter(season == "Summer")
fall <- x %>% 
  filter(season == "Fall")

# GPP
ggplot(x) +
  geom_point(aes(x=TIME, y = GPP, col = season)) +
  scale_color_brewer(palette = "Spectral") +
  ylim(-0.5, 1.6) +
  labs(x="Time", y = "Gross Primary Production") +
  theme_bw()

# NEE
ggplot(x) +
  geom_point(aes(x=TIME, y = NEE, col = season)) +
  scale_color_brewer(palette = "Spectral") +
  ylim(-1, 1.1) +
  labs(x="Time", y = "") +
  theme_bw()

# RE
ggplot(x) +
  geom_point(aes(x=TIME, y = RE, col = season)) +
  scale_color_brewer(palette = "Spectral") +
  ylim(0,1.2) +
  theme_bw()

# TA
ggplot(x) +
  geom_point(aes(x=TIME, y = TA, col = season)) +
  scale_color_brewer(palette = "Spectral") +
  ylim(-36,40) +
  theme_bw()

# TS
ggplot(x) +
  geom_point(aes(x=TIME, y = TS, col = season)) +
  scale_color_brewer(palette = "Spectral") +
  ylim(-11,51) +
  theme_bw()

# PPFD_IN
ggplot(x) +
  geom_point(aes(x=TIME, y = PPFD_IN, col = season)) +
  ylim(.5,1898) +
  theme_bw()

# VPD
ggplot(x) +
  geom_point(aes(x=TIME, y = VPD, col = season)) +
  scale_color_brewer(palette = "Spectral") +
  ylim(-1, 66) +
  theme_bw()


sum_stats <- function(df){
  df <- na.omit(df)
  M <- mean(df)
  SD <- sd(df)
  p5 <- quantile(df, .05)
  p95 <- quantile(df, .95)
  countr <- length(df)
  variable <- as.list.data.frame(c(M, SD, p5, p95, countr))
  return(variable)
}

site <- "Harvard Forest"

GPP_W <- sum_stats(winter$GPP)
GPP_F <- sum_stats(fall$GPP)
GPP_S <- sum_stats(summer$GPP)
GPP_Sp <- sum_stats(spring$GPP)

NEE_W <- sum_stats(winter$NEE)
NEE_F <- sum_stats(fall$NEE)
NEE_S <- sum_stats(summer$NEE)
NEE_Sp <- sum_stats(spring$NEE)

RE_W <- sum_stats(winter$RE)
RE_F <- sum_stats(fall$RE)
RE_S <- sum_stats(summer$RE)
RE_Sp <- sum_stats(spring$RE)

TA_W <- sum_stats(winter$TA)
TA_F <- sum_stats(fall$TA)
TA_S <- sum_stats(summer$TA)
TA_Sp <- sum_stats(spring$TA)

VPD_W <- sum_stats(winter$VPD)
VPD_F <- sum_stats(fall$VPD)
VPD_S <- sum_stats(summer$VPD)
VPD_Sp <- sum_stats(spring$VPD)

PPFD_IN_W <- sum_stats(winter$PPFD_IN)
PPFD_IN_F <- sum_stats(fall$PPFD_IN)
PPFD_IN_S <- sum_stats(summer$PPFD_IN)
PPFD_IN_Sp <- sum_stats(spring$PPFD_IN)

SWC_W <- sum_stats(winter$SWC)
SWC_F <- sum_stats(fall$SWC)
SWC_S <- sum_stats(summer$SWC)
SWC_Sp <- sum_stats(spring$SWC)

TS_W <- sum_stats(winter$TS)
TS_F <- sum_stats(fall$TS)
TS_S <- sum_stats(summer$TS)
TS_Sp <- sum_stats(spring$TS)


template <- as.list.data.frame(c("Site",
                                 "GPP_mean_winter", "GPP_sd_winter", "GPP_5per_winter",
                                 "GPP_95per_winter", "GPP_N_winter", "GPP_mean_fall", 
                                 "GPP_sd_fall", "GPP_5per_fall",
                                 "GPP_95per_fall", "GPP_N_fall", 
                                 "GPP_mean_summer", "GPP_sd_summer", "GPP_5per_summer",
                                 "GPP_95per_summer", "GPP_N_summer", 
                                 "GPP_mean_spring", "GPP_sd_spring", "GPP_5per_spring",
                                 "GPP_95per_spring", "GPP_N_spring",
                                 
                                 "NEE_mean_winter", "NEE_sd_winter", "NEE_5per_winter",
                                 "NEE_95per_winter", "NEE_N_winter", "NEE_mean_fall", 
                                 "NEE_sd_fall", "NEE_5per_fall",
                                 "NEE_95per_fall", "NEE_N_fall", 
                                 "NEE_mean_summer", "NEE_sd_summer", "NEE_5per_summer",
                                 "NEE_95per_summer", "NEE_N_summer", 
                                 "NEE_mean_spring", "NEE_sd_spring", "NEE_5per_spring",
                                 "NEE_95per_spring", "NEE_N_spring",
                                 
                                 "RE_mean_winter", "RE_sd_winter", "RE_5per_winter",
                                 "RE_95per_winter", "RE_N_winter", "RE_mean_fall", 
                                 "RE_sd_fall", "RE_5per_fall",
                                 "RE_95per_fall", "RE_N_fall", 
                                 "RE_mean_summer", "RE_sd_summer", "RE_5per_summer",
                                 "RE_95per_summer", "RE_N_summer", 
                                 "RE_mean_spring", "RE_sd_spring", "RE_5per_spring",
                                 "RE_95per_spring", "RE_N_spring",
                                 
                                 "TA_mean_winter", "TA_sd_winter", "TA_5per_winter",
                                 "TA_95per_winter", "TA_N_winter", "TA_mean_fall", 
                                 "TA_sd_fall", "TA_5per_fall",
                                 "TA_95per_fall", "TA_N_fall", 
                                 "TA_mean_summer", "TA_sd_summer", "TA_5per_summer",
                                 "TA_95per_summer", "TA_N_summer", 
                                 "TA_mean_spring", "TA_sd_spring", "TA_5per_spring",
                                 "TA_95per_spring", "TA_N_spring",
                                 
                                 "VPD_mean_winter", "VPD_sd_winter", "VPD_5per_winter",
                                 "VPD_95per_winter", "VPD_N_winter", "VPD_mean_fall", 
                                 "VPD_sd_fall", "VPD_5per_fall",
                                 "VPD_95per_fall", "VPD_N_fall", 
                                 "VPD_mean_summer", "VPD_sd_summer", "VPD_5per_summer",
                                 "VPD_95per_summer", "VPD_N_summer", 
                                 "VPD_mean_spring", "VPD_sd_spring", "VPD_5per_spring",
                                 "VPD_95per_spring", "VPD_N_spring",
                                 
                                 "PPFD_IN_mean_winter", "PPFD_IN_sd_winter", "PPFD_IN_5per_winter",
                                 "PPFD_IN_95per_winter", "PPFD_IN_N_winter", "PPFD_IN_mean_fall", 
                                 "PPFD_IN_sd_fall", "PPFD_IN_5per_fall",
                                 "PPFD_IN_95per_fall", "PPFD_IN_N_fall", 
                                 "PPFD_IN_mean_summer", "PPFD_IN_sd_summer", "PPFD_IN_5per_summer",
                                 "PPFD_IN_95per_summer", "PPFD_IN_N_summer", 
                                 "PPFD_IN_mean_spring", "PPFD_IN_sd_spring", "PPFD_IN_5per_spring",
                                 "PPFD_IN_95per_spring", "PPFD_IN_N_spring",
                                 
                                 "SWC_mean_winter", "SWC_sd_winter", "SWC_5per_winter",
                                 "SWC_95per_winter", "SWC_N_winter", "SWC_mean_fall", 
                                 "SWC_sd_fall", "SWC_5per_fall",
                                 "SWC_95per_fall", "SWC_N_fall", 
                                 "SWC_mean_summer", "SWC_sd_summer", "SWC_5per_summer",
                                 "SWC_95per_summer", "SWC_N_summer", 
                                 "SWC_mean_spring", "SWC_sd_spring", "SWC_5per_spring",
                                 "SWC_95per_spring", "SWC_N_spring",
                                 
                                 "TS_mean_winter", "TS_sd_winter", "TS_5per_winter",
                                 "TS_95per_winter", "TS_N_winter", "TS_mean_fall", 
                                 "TS_sd_fall", "TS_5per_fall",
                                 "TS_95per_fall", "TS_N_fall", 
                                 "TS_mean_summer", "TS_sd_summer", "TS_5per_summer",
                                 "TS_95per_summer", "TS_N_summer", 
                                 "TS_mean_spring", "TS_sd_spring", "TS_5per_spring",
                                 "TS_95per_spring", "TS_N_spring"
                                 
                                 ))

values <- as.list.data.frame(c(site,
                             GPP_W, GPP_F, GPP_S, GPP_Sp,
                             NEE_W, NEE_F, NEE_S, NEE_Sp,
                             RE_W, RE_F, RE_S, RE_Sp,
                             TA_W, TA_F, TA_S, TA_Sp,
                             VPD_W, VPD_F, VPD_S, VPD_Sp,
                             PPFD_IN_W, PPFD_IN_F, PPFD_IN_S, PPFD_IN_Sp,
                             SWC_W, SWC_F, SWC_S, SWC_Sp,
                             TS_W, TS_F, TS_S, TS_Sp))


neon_data <- rbind.data.frame(template, values)
pander::pander(neon_data)
write.csv(neon_data, file = "D:/CSU/Fall 2022/ECOL 610 - Ecosystem Ecology/Assignments/test.csv")
