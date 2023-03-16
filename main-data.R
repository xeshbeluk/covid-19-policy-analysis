library(astsa)
library(dplyr)
library(ggplot2)
library(data.table)
library(Synth)
library(tidyverse)
options(scipen=10)


main_data <- read.csv('owid-covid-data.csv')
main_data$date <- as.Date(main_data$date, format = '%Y-%m-%d')
economist_rus_excess_deaths <- read.csv('russia_excess_deaths.csv')

rus <- filter(main_data, iso_code == 'RUS' & date >= "2020-02-01" & date <= "2022-08-01")
rus$group <- ifelse(rus$date <= '2020-12-31', rus$group <- '2020', ifelse(rus$date <= '2021-12-31' & rus$date > '2020-12-31', rus$group <- '2021', rus$group <- '2022'))
rus_reg <- lm(rus$new_deaths ~ rus$new_cases)
cooksd_rus <- cooks.distance(rus_reg)
inf_rus <- as.numeric(names(cooksd_rus)[(cooksd_rus > 4*mean(cooksd_rus, na.rm=T))])
rus_no_outliers <- as.data.frame(rus[-c(inf_rus),])

usa <- filter(main_data, iso_code == 'USA'& date >= "2020-02-01" & date <= "2022-08-01")
usa$group <- ifelse(usa$date <= '2020-12-31', usa$group <- '2020', ifelse(usa$date <= '2021-12-31' & usa$date > '2020-12-31', usa$group <- '2021', usa$group <- '2022'))
usa_reg <- lm(usa$new_deaths ~ usa$new_cases)
cooksd_usa <- cooks.distance(usa_reg)
inf_usa <- as.numeric(names(cooksd_usa)[(cooksd_usa > 4*mean(cooksd_usa, na.rm=T))])
usa_no_outliers <- as.data.frame(usa[-c(inf_usa),])

bra <- filter(main_data, iso_code == 'BRA'& date >= "2020-02-01" & date <= "2022-08-01")
bra$group <- ifelse(bra$date <= '2020-12-31', bra$group <- '2020', ifelse(bra$date <= '2021-12-31' & bra$date > '2020-12-31', bra$group <- '2021', bra$group <- '2022'))
bra_reg <- lm(bra$new_deaths ~ bra$new_cases)
cooksd_bra <- cooks.distance(bra_reg)
inf_bra <- as.numeric(names(cooksd_bra)[(cooksd_bra > 4*mean(cooksd_bra, na.rm=T))])
bra_no_outliers <- as.data.frame(bra[-c(inf_bra),])

ukr <- filter(main_data, iso_code == 'UKR'& date >= "2020-02-01" & date <= "2022-08-01")
ukr$group <- ifelse(ukr$date <= '2020-12-31', ukr$group <- '2020', ifelse(ukr$date <= '2021-12-31' & ukr$date > '2020-12-31', ukr$group <- '2021', ukr$group <- '2022'))
ukr_reg <- lm(ukr$new_deaths ~ ukr$new_cases)
cooksd_ukr <- cooks.distance(ukr_reg)
inf_ukr <- as.numeric(names(cooksd_ukr)[(cooksd_ukr > 4*mean(cooksd_ukr, na.rm=T))])
ukr_no_outliers <- as.data.frame(ukr[-c(inf_ukr),])

ita <- filter(main_data, iso_code == 'ITA'& date >= "2020-02-01" & date <= "2022-08-01")
ita$group <- ifelse(ita$date <= '2020-12-31', ita$group <- '2020', ifelse(ita$date <= '2021-12-31' & ita$date > '2020-12-31', ita$group <- '2021', ita$group <- '2022'))
ita_reg <- lm(ita$new_deaths ~ ita$new_cases)
cooksd_ita<- cooks.distance(ita_reg)
inf_ita <- as.numeric(names(cooksd_ita)[(cooksd_ita > 4*mean(cooksd_ita, na.rm=T))])
ita_no_outliers <- as.data.frame(ita[-c(inf_ita),])

gbr <- filter(main_data, iso_code == 'GBR'& date >= "2020-02-01" & date <= "2022-08-01")
gbr$group <- ifelse(gbr$date <= '2020-12-31', gbr$group <- '2020', ifelse(gbr$date <= '2021-12-31' & gbr$date > '2020-12-31', gbr$group <- '2021', gbr$group <- '2022'))
gbr_reg <- lm(gbr$new_deaths ~ gbr$new_cases)
cooksd_gbr<- cooks.distance(gbr_reg)
inf_gbr <- as.numeric(names(cooksd_gbr)[(cooksd_gbr > 4*mean(cooksd_gbr, na.rm=T))])
gbr_no_outliers <- as.data.frame(gbr[-c(inf_gbr),])

ger <- filter(main_data, iso_code == 'DEU'& date >= "2020-02-01" & date <= "2022-08-01")
ger$group <- ifelse(ger$date <= '2020-12-31', ger$group <- '2020', ifelse(ger$date <= '2021-12-31' & ger$date > '2020-12-31', ger$group <- '2021', ger$group <- '2022'))
ger_reg <- lm(ger$new_deaths ~ ger$new_cases)
cooksd_ger<- cooks.distance(ger_reg)
inf_ger <- as.numeric(names(cooksd_ger)[(cooksd_ger > 4*mean(cooksd_ger, na.rm=T))])
ger_no_outliers <- as.data.frame(ger[-c(inf_ger),])

fra <- filter(main_data, iso_code == 'FRA'& date >= "2020-02-01" & date <= "2022-08-01")
fra$group <- ifelse(fra$date <= '2020-12-31', fra$group <- '2020', ifelse(fra$date <= '2021-12-31' & fra$date > '2020-12-31', fra$group <- '2021', fra$group <- '2022'))
fra_reg <- lm(fra$new_deaths ~ fra$new_cases)
cooksd_fra<- cooks.distance(fra_reg)
inf_fra <- as.numeric(names(cooksd_fra)[(cooksd_fra > 4*mean(cooksd_fra, na.rm=T))])
fra_no_outliers <- as.data.frame(fra[-c(inf_fra),])

bgr <- filter(main_data, iso_code == 'BGR'& date >= "2020-02-01" & date <= "2022-08-01")
bgr$group <- ifelse(bgr$date <= '2020-12-31', bgr$group <- '2020', ifelse(bgr$date <= '2021-12-31' & bgr$date > '2020-12-31', bgr$group <- '2021', bgr$group <- '2022'))
bgr_reg <- lm(bgr$new_deaths ~ bgr$new_cases)
cooksd_bgr<- cooks.distance(bgr_reg)
inf_bgr <- as.numeric(names(cooksd_bgr)[(cooksd_bgr > 4*mean(cooksd_bgr, na.rm=T))])
bgr_no_outliers <- as.data.frame(bgr[-c(inf_bgr),])

srb <- filter(main_data, iso_code == 'SRB'& date >= "2020-02-01" & date <= "2022-08-01")
srb$group <- ifelse(srb$date <= '2020-12-31', srb$group <- '2020', ifelse(srb$date <= '2021-12-31' & srb$date > '2020-12-31', srb$group <- '2021', srb$group <- '2022'))
srb_reg <- lm(srb$new_deaths ~ srb$new_cases)
cooksd_srb<- cooks.distance(srb_reg)
inf_srb <- as.numeric(names(cooksd_srb)[(cooksd_srb > 4*mean(cooksd_srb, na.rm=T))])
srb_no_outliers <- as.data.frame(srb[-c(inf_srb),])

rou <- filter(main_data, iso_code == 'ROU'& date >= "2020-02-01" & date <= "2022-08-01")
rou$group <- ifelse(rou$date <= '2020-12-31', rou$group <- '2020', ifelse(rou$date <= '2021-12-31' & rou$date > '2020-12-31', rou$group <- '2021', rou$group <- '2022'))
rou_reg <- lm(rou$new_deaths ~ rou$new_cases)
cooksd_rou<- cooks.distance(rou_reg)
inf_rou <- as.numeric(names(cooksd_rou)[(cooksd_rou > 4*mean(cooksd_rou, na.rm=T))])
rou_no_outliers <- as.data.frame(rou[-c(inf_rou),])

per <- filter(main_data, iso_code == 'PER'& date >= "2020-02-01" & date <= "2022-08-01")
per$group <- ifelse(per$date <= '2020-12-31', per$group <- '2020', ifelse(per$date <= '2021-12-31' & per$date > '2020-12-31', per$group <- '2021', per$group <- '2022'))
per_reg <- lm(per$new_deaths ~ per$new_cases)
cooksd_per<- cooks.distance(per_reg)
inf_per <- as.numeric(names(cooksd_per)[(cooksd_per > 4*mean(cooksd_per, na.rm=T))])
per_no_outliers <- as.data.frame(per[-c(inf_per),])

mex <- filter(main_data, iso_code == 'MEX'& date >= "2020-02-01" & date <= "2022-08-01")
mex$group <- ifelse(mex$date <= '2020-12-31', mex$group <- '2020', ifelse(mex$date <= '2021-12-31' & mex$date > '2020-12-31', mex$group <- '2021', mex$group <- '2022'))
mex_reg <- lm(mex$new_deaths ~ mex$new_cases)
cooksd_mex<- cooks.distance(mex_reg)
inf_mex <- as.numeric(names(cooksd_mex)[(cooksd_mex > 4*mean(cooksd_mex, na.rm=T))])
mex_no_outliers <- as.data.frame(mex[-c(inf_mex),])

pol <- filter(main_data, iso_code == 'POL'& date >= "2020-02-01" & date <= "2022-08-01")
pol$group <- ifelse(pol$date <= '2020-12-31', pol$group <- '2020', ifelse(pol$date <= '2021-12-31' & pol$date > '2020-12-31', pol$group <- '2021', pol$group <- '2022'))
pol_reg <- lm(pol$new_deaths ~ pol$new_cases)
cooksd_pol<- cooks.distance(pol_reg)
inf_pol <- as.numeric(names(cooksd_pol)[(cooksd_pol > 4*mean(cooksd_pol, na.rm=T))])
pol_no_outliers <- as.data.frame(pol[-c(inf_pol),])

spa <- filter(main_data, iso_code == 'ESP'& date >= "2020-02-01" & date <= "2022-08-01")
spa$group <- ifelse(spa$date <= '2020-12-31', spa$group <- '2020', ifelse(spa$date <= '2021-12-31' & spa$date > '2020-12-31', spa$group <- '2021', spa$group <- '2022'))
spa_reg <- lm(spa$new_deaths ~ spa$new_cases)
cooksd_spa<- cooks.distance(spa_reg)
inf_spa <- as.numeric(names(cooksd_spa)[(cooksd_spa > 4*mean(cooksd_spa, na.rm=T))])
spa_no_outliers <- as.data.frame(spa[-c(inf_spa),])

phl <- filter(main_data, iso_code == 'PHL'& date >= "2020-02-01" & date <= "2022-08-01")
phl$group <- ifelse(phl$date <= '2020-12-31', phl$group <- '2020', ifelse(phl$date <= '2021-12-31' & phl$date > '2020-12-31', phl$group <- '2021', phl$group <- '2022'))
phl_reg <- lm(phl$new_deaths ~ phl$new_cases)
cooksd_phl<- cooks.distance(phl_reg)
inf_phl <- as.numeric(names(cooksd_phl)[(cooksd_phl > 4*mean(cooksd_phl, na.rm=T))])
phl_no_outliers <- as.data.frame(phl[-c(inf_phl),])


#quadratic 
v_c <- list(rus, usa, bra, ukr, ita, gbr, ger, fra, bgr, srb, rou, per, mex, pol, spa, phl)



summaries <- list()
n <- 1
for (i in v_c2) {
  summaries[[n]] <- summary(lm(new_deaths ~ new_cases, data = i))
  n <- n + 1
}
summaries

intercepts <- list()
betas <- list()
betasq <- list()
r2 <- list()
for (i in 1:16) {
  intercepts[i] <- summaries[[i]]$coefficient[1]
  betas[i] <- summaries[[i]]$coefficient[2]
  betasq[i] <- summaries[[i]]$coefficient[3]
  r2[i] <- summaries[[i]]$r.squared[1]
}




dd1 <-  as.data.frame(t(matrix(unlist(intercepts), nrow=length(unlist(intercepts[1])))))
dd1$betas <- t(matrix(unlist(betas), nrow=length(unlist(betas[1]))))
dd1$betasq <- t(matrix(unlist(betasq), nrow=length(unlist(betasq[1]))))
dd1$r2 <-t(matrix(unlist(r2), nrow=length(unlist(r2[1]))))

dd1

summary(lm(new_deaths ~ new_cases + I(new_cases^2), data = rus_2022))





rus_2022 <- filter(rus,
                   group == 2022)
usa_2022 <- filter(usa,
                   group == 2022)
bra_2022 <- filter(bra,
                   group == 2022)
ukr_2022 <- filter(ukr,
                   group == 2022)
ita_2022 <- filter(ita,
                   group == 2022)
gbr_2022 <- filter(gbr,
                   group == 2022)
ger_2022 <- filter(ger,
                   group == 2022)
fra_2022 <- filter(fra,
                   group == 2022)
bgr_2022 <- filter(bgr,
                   group == 2022)
srb_2022 <- filter(srb,
                   group == 2022)
rou_2022 <- filter(rou,
                   group == 2022)
per_2022 <- filter(per,
                   group == 2022)
mex_2022 <- filter(mex,
                   group == 2022)
pol_2022 <- filter(pol,
                   group == 2022)
spa_2022 <- filter(spa,
                   group == 2022)
phl_2022 <- filter(phl,
                   group == 2022)
v_c2 <- list(rus_2022, usa_2022, bra_2022, ukr_2022, ita_2022, 
             gbr_2022, ger_2022, fra_2022, bgr_2022, srb_2022, 
             rou_2022, per_2022, mex_2022, pol_2022, spa_2022, phl_2022)


rus_2022$new_cases2 <- rus_2022$new_cases^2
rus_reg_quad <- lm(new_deaths ~ new_cases + new_cases2 , data = rus_2022)
summary(rus_reg_quad)


usa_2022$new_cases2 <- usa_2022$new_cases^2
usa_reg_quad <- lm(new_deaths ~ new_cases + new_cases2 , data = usa_2022)
summary(usa_reg_quad)


bra_2022$new_cases2 <- bra_2022$new_cases^2
bra_reg_quad <- lm(new_deaths ~ new_cases + new_cases2 , data = bra_2022)
summary(bra_reg_quad)


ukr_2022$new_cases2 <- ukr_2022$new_cases^2
ukr_reg_quad <- lm(new_deaths ~ new_cases + new_cases2 , data = ukr_2022)
summary(ukr_reg_quad)


ita_2022$new_cases2 <- ita_2022$new_cases^2
ita_reg_quad <- lm(new_deaths ~ new_cases + new_cases2 , data = ita_2022)
summary(ita_reg_quad)


gbr_2022$new_cases2 <- gbr_2022$new_cases^2
gbr_reg_quad <- lm(new_deaths ~ new_cases + new_cases2 , data = gbr_2022)
summary(gbr_reg_quad)


ger_2022$new_cases2 <- ger_2022$new_cases^2
ger_reg_quad <- lm(new_deaths ~ new_cases + new_cases2, data = ger_2022)
summary(ger_reg_quad)


fra_2022$new_cases2 <- fra_2022$new_cases^2
fra_reg_quad <- lm(new_deaths ~ new_cases + new_cases2, data = fra_2022)
summary(fra_reg_quad)


bgr_2022$new_cases2 <- bgr_2022$new_cases^2
bgr_reg_quad <- lm(new_deaths ~ new_cases + new_cases2, data = bgr_2022)
summary(bgr_reg_quad)


srb_2022$new_cases2 <- srb_2022$new_cases^2
srb_reg_quad <- lm(new_deaths ~ new_cases + new_cases2, data = srb_2022)
summary(srb_reg_quad)


rou_2022$new_cases2 <- rou_2022$new_cases^2
rou_reg_quad <- lm(new_deaths ~ new_cases + new_cases2, data = rou_2022)
summary(rou_reg_quad)


per_2022$new_cases2 <- per_2022$new_cases^2
per_reg_quad <- lm(new_deaths ~ new_cases + new_cases2, data = per_2022)
summary(per_reg_quad)


mex_2022$new_cases2 <- mex_2022$new_cases^2
mex_reg_quad <- lm(new_deaths ~ new_cases + new_cases2, data = mex_2022)
summary(mex_reg_quad)


pol_2022$new_cases2 <- pol_2022$new_cases^2
pol_reg_quad <- lm(new_deaths ~ new_cases + new_cases2, data = pol_2022)
summary(pol_reg_quad)


spa_2022$new_cases2 <- spa_2022$new_cases^2
spa_reg_quad <- lm(new_deaths ~ new_cases + new_cases2, data = spa_2022)
summary(spa_reg_quad)


phl_2022$new_cases2 <- phl_2022$new_cases^2
phl_reg_quad <- lm(new_deaths ~ new_cases + new_cases2, data = phl_2022)
summary(phl_reg_quad)






v_c <- list(rus, usa, bra, ukr, ita, gbr, ger, fra, bgr, srb, rou, per, mex, pol, spa, phl)
v_cn <- list(rus_no_outliers, usa_no_outliers, bra_no_outliers, ukr_no_outliers, ita_no_outliers, gbr_no_outliers, 
             ger_no_outliers, fra_no_outliers, bgr_no_outliers, srb_no_outliers, rou_no_outliers, per_no_outliers, 
             mex_no_outliers, pol_no_outliers, spa_no_outliers, phl_no_outliers)

for (i in v_c) {
  show(cor(i$new_deaths, i$new_cases, use='complete.obs'))
}

for (i in v_cn) {
  show(cor(i$new_deaths, i$new_cases, use='complete.obs'))
}

cor(i$new_deaths, i$new_cases, use='complete.obs')
cor(srb_no_outliers$new_deaths, srb_no_outliers$new_cases, use='complete.obs')



#plots_1
                              
rusplot <- ggplot(data = rus, aes(x = scale(new_cases), y = scale(new_deaths), group = factor(group))) +
            geom_point(aes(color = group), alpha = .2) + 
            geom_smooth(aes(color = group), method = 'lm', formula = y ~ x, se = FALSE, size = 1.5) +
            xlim(-0.5,6) +
             ylim(-0.5,3) +
            labs(x = 'Spreading', y = 'Mortality') +
              theme_bw() +theme(
              axis.text=element_text(size=12)
              ,axis.title=element_text(size=14,face="bold")
              ,plot.background = element_blank()
              ,panel.grid.major = element_blank()
              ,panel.grid.minor = element_blank()
              ,panel.border = element_blank()
              ,panel.background = element_blank()
              ) +
            theme(axis.line = element_line(color = 'black'))
rusplot



# rus_reg <- lm(rus$new_deaths ~ rus$new_cases)
# summary(rus_reg)
# 
# 
# par(mfrow = c(2,2))
# plot(rus_reg)
# cor(rus_no_outliers$new_deaths, rus_no_outliers$new_cases, use = "complete.obs")
# 
# (summary(rus_reg)$sigma)**2
# vcov(rus_reg)[2,2]
# 
# plot(resid(rus_reg))
# plot(resid(ukr_reg))
# plot(resid(per_reg))
# plot(resid(srb_reg))
# plot(resid(ita_reg))
# plot(resid(usa_reg))
# plot(resid(bgr_reg))
# plot(resid(pol_reg))
# plot(resid(phl_reg))
# plot(resid(fra_reg))
# plot(resid(bra_reg))
# 
# usa_reg <- lm(usa$new_deaths ~ usa$new_cases)
# par(mfrow = c(2,2))
# plot(usa_reg)
# 
# ger_reg <- lm(ger$new_deaths ~ ger$new_cases)
# par(mfrow = c(2,2))
# plot(ger_reg)
# 
# mex_reg <- lm(mex$new_deaths ~ mex$new_cases)
# par(mfrow = c(2,2))
# plot(mex_reg)
# 
# per_reg <- lm(per$new_deaths ~ per$new_cases)
# par(mfrow = c(2,2))
# plot(per_reg)
# 
# gbr_reg <- lm(gbr$new_deaths ~ gbr$new_cases)
# par(mfrow = c(2,2))
# plot(gbr_reg)
# 
# 
# rus_res <- resid(rus_reg)
# qqnorm(rus_res)
# qqline(rus_res)
# plot(density(rus_res))
# plot(density(rus_res))





usaplot <- ggplot(data = usa, aes(x = scale(new_cases), y = scale(new_deaths), group = factor(group))) +
  geom_point(aes(color = group), alpha = .4) + 
  geom_smooth(aes(color = group), method = 'lm', formula = y ~ x, se = FALSE, size = 1.5) +
  xlim(-0.5,6) +
  ylim(-0.5,3) +
  labs(x = 'Spreading', y = 'Mortality') +
  theme_bw() +theme(
    axis.text=element_text(size=12)
    ,axis.title=element_text(size=14,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black')) 
usaplot

braplot <- ggplot(data = bra, aes(x = scale(new_cases), y = scale(new_deaths), group = factor(group))) +
  geom_point(aes(color = group), alpha = .4) + 
  geom_smooth(aes(color = group), method = 'lm', formula = y ~ x, se = FALSE, size = 1.5) +
  xlim(-0.5,6) +
  ylim(-0.5,3) +
  labs(x = 'Spreading', y = 'Mortality') +
  theme_bw() +theme(
    axis.text=element_text(size=12)
    ,axis.title=element_text(size=14,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'))
braplot

ukrplot <- ggplot(data = ukr, aes(x = scale(new_cases), y = scale(new_deaths), group = factor(group))) +
  geom_point(aes(color = group), alpha = .4) + 
  geom_smooth(aes(color = group), method = 'lm', formula = y ~ x, se = FALSE, size = 1.5) +
  xlim(-0.5,6) +
  ylim(-0.5,3) +
  labs(x = 'Spreading', y = 'Mortality') +
  theme_bw() +theme(
    axis.text=element_text(size=12)
    ,axis.title=element_text(size=14,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black')) 
ukrplot
#plots_2
itaplot <- ggplot(data = ita, aes(x = scale(new_cases), y = scale(new_deaths), group = factor(group))) +
  geom_point(aes(color = group), alpha = .4) + 
  geom_smooth(aes(color = group), method = 'lm', formula = y ~ x, se = FALSE, size = 1.5) +
  xlim(-0.5,6) +
  ylim(-0.5,3) +
  labs(x = 'Spreading', y = 'Mortality') +
  theme_bw() +theme(
    axis.text=element_text(size=12)
    ,axis.title=element_text(size=14,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'))
itaplot

gbrplot <- ggplot(data = gbr, aes(x = scale(new_cases), y = scale(new_deaths), group = factor(group))) +
  geom_point(aes(color = group), alpha = .4) + 
  geom_smooth(aes(color = group), method = 'lm', formula = y ~ x, se = FALSE, size = 1.5) +
  xlim(-0.5,6) +
  ylim(-0.5,3) +
  labs(x = 'Spreading', y = 'Mortality') +
  theme_bw() +theme(
    axis.text=element_text(size=12)
    ,axis.title=element_text(size=14,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'))
gbrplot

gerplot <- ggplot(data = ger, aes(x = scale(new_cases), y = scale(new_deaths), group = factor(group))) +
  geom_point(aes(color = group), alpha = .4) + 
  geom_smooth(aes(color = group), method = 'lm', formula = y ~ x, se = FALSE, size = 1.5) +
  xlim(-0.5,6) +
  ylim(-0.5,3) +
  labs(x = 'Spreading', y = 'Mortality') +
  theme_bw() +theme(
    axis.text=element_text(size=12)
    ,axis.title=element_text(size=14,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'))
gerplot

fraplot <- ggplot(data = fra, aes(x = scale(new_cases), y = scale(new_deaths), group = factor(group))) +
  geom_point(aes(color = group), alpha = .4) + 
  geom_smooth(aes(color = group), method = 'lm', formula = y ~ x, se = FALSE, size = 1.5) +
  xlim(-0.5,6) +
  ylim(-0.5,3) +
  labs(x = 'Spreading', y = 'Mortality') +
  theme_bw() +theme(
    axis.text=element_text(size=12)
    ,axis.title=element_text(size=14,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'))
fraplot

#plots_3
bgrplot <- ggplot(data = bgr, aes(x = scale(new_cases), y = scale(new_deaths), group = factor(group))) +
  geom_point(aes(color = group), alpha = .4) + 
  geom_smooth(aes(color = group), method = 'lm', formula = y ~ x, se = FALSE, size = 1.5) +
  xlim(-0.5,6) +
  ylim(-0.5,3) +
  labs(x = 'Spreading', y = 'Mortality') +
  theme_bw() +theme(
    axis.text=element_text(size=12)
    ,axis.title=element_text(size=14,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black')) 
bgrplot

srbplot <- ggplot(data = srb, aes(x = scale(new_cases), y = scale(new_deaths), group = factor(group))) +
  geom_point(aes(color = group), alpha = .4) + 
  geom_smooth(aes(color = group), method = 'lm', formula = y ~ x, se = FALSE, size = 1.5) +
  xlim(-0.5,6) +
  ylim(-0.5,3) +
  labs(x = 'Spreading', y = 'Mortality') +
  theme_bw() +theme(
    axis.text=element_text(size=12)
    ,axis.title=element_text(size=14,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black')) 
srbplot

rouplot <- ggplot(data = rou, aes(x = scale(new_cases), y = scale(new_deaths), group = factor(group))) +
  geom_point(aes(color = group), alpha = .4) + 
  geom_smooth(aes(color = group), method = 'lm', formula = y ~ x, se = FALSE, size = 1.5) +
  xlim(-0.5,6) +
  ylim(-0.5,3) +
  labs(x = 'Spreading', y = 'Mortality') +
  theme_bw() +theme(
    axis.text=element_text(size=12)
    ,axis.title=element_text(size=14,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black')) 
rouplot

perplot <- ggplot(data = per, aes(x = scale(new_cases), y = scale(new_deaths), group = factor(group))) +
  geom_point(aes(color = group), alpha = .4) + 
  geom_smooth(aes(color = group), method = 'lm', formula = y ~ x, se = FALSE, size = 1.5) +
  xlim(-0.5,6) +
  ylim(-0.5,3) +
  labs(x = 'Spreading', y = 'Mortality') +
  theme_bw() +theme(
    axis.text=element_text(size=12)
    ,axis.title=element_text(size=14,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black')) 
perplot

#plots_4
mexplot <- ggplot(data = mex, aes(x = scale(new_cases), y = scale(new_deaths), group = factor(group))) +
  geom_point(aes(color = group), alpha = .4) + 
  geom_smooth(aes(color = group), method = 'lm', formula = y ~ x, se = FALSE, size = 1.5) +
  xlim(-0.5,6) +
  ylim(-0.5,3) +
  labs(x = 'Spreading', y = 'Mortality') +
  theme_bw() +theme(
    axis.text=element_text(size=12)
    ,axis.title=element_text(size=14,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black')) 
mexplot

polplot <- ggplot(data = pol, aes(x = scale(new_cases), y = scale(new_deaths), group = factor(group))) +
  geom_point(aes(color = group), alpha = .4) + 
  geom_smooth(aes(color = group), method = 'lm', formula = y ~ x, se = FALSE, size = 1.5) +
  xlim(-0.5,6) +
  ylim(-0.5,3) +
  labs(x = 'Spreading', y = 'Mortality') +
  theme_bw() +theme(
    axis.text=element_text(size=12)
    ,axis.title=element_text(size=14,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black')) 
polplot

spaplot <- ggplot(data = spa, aes(x = scale(new_cases), y = scale(new_deaths), group = factor(group))) +
  geom_point(aes(color = group), alpha = .4) + 
  geom_smooth(aes(color = group), method = 'lm', formula = y ~ x, se = FALSE, size = 1.5) +
  xlim(-0.5,6) +
  ylim(-0.5,3) +
  labs(x = 'Spreading', y = 'Mortality') +
  theme_bw() +theme(
    axis.text=element_text(size=12)
    ,axis.title=element_text(size=14,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black')) 
spaplot

phlplot <- ggplot(data = phl, aes(x = scale(new_cases), y = scale(new_deaths), group = factor(group))) +
  geom_point(aes(color = group), alpha = .4) + 
  geom_smooth(aes(color = group), method = 'lm', formula = y ~ x, se = FALSE, size = 1.5) +
  xlim(-0.5,6) +
  ylim(-0.5,3) +
  labs(x = 'Spreading', y = 'Mortality') +
  theme_bw() +theme(
    axis.text=element_text(size=12)
    ,axis.title=element_text(size=14,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black')) 
phlplot

library(ggpubr)
#1
ggarrange(rusplot, usaplot,
          braplot, ukrplot,
          labels = c('RUS', 'USA', 'BRA', 'UKR'),
          hjust = c(-8,-8,-8,-8),
          ncol = 2, nrow = 2)
#2
ggarrange(itaplot, gbrplot,
          gerplot, fraplot,
          labels = c('ITA', 'GBR', 'GER', 'FRA'),
          hjust = c(-8,-6,-6,-6.5),
          ncol = 2, nrow = 2)
#3
ggarrange(bgrplot, srbplot,
          rouplot, perplot,
          labels = c('BGR', 'SRB', 'ROU', 'PER'),
          hjust = c(-8,-6,-8,-6.5),
          ncol = 2, nrow = 2)
#4
ggarrange(mexplot, polplot,
          spaplot, phlplot,
          labels = c('MEX', 'POL', 'ESP', 'PHL'),
          hjust = c(-5.5,-6,-6,-6.5),
          ncol = 2, nrow = 2)

#mortality plots

rus_mort <- ggplot(data = rus, aes(x = date, y = scale(new_cases), na.rm = TRUE)) +  
  geom_line(color = "red") + 
  geom_line(y = scale(rus$new_deaths), color = "blue") +
  geom_point(y = scale(rus$excess_mortality), color = "green", size = 3) +
  labs(x = 'Date', y = 'Mortality vs Spreading') + 
  scale_x_date(date_breaks = '9 months', date_labels = '%Y-%m') +
  ylim(-2,6.5) +
  theme_bw() +theme(
    axis.text=element_text(size=10)
    ,axis.title=element_text(size=8,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'))
rus_mort

usa_mort <- ggplot(data = usa, aes(x = date, y = scale(new_cases), na.rm = TRUE)) +  
  geom_line(color = "red") + 
  geom_line(y = scale(usa$new_deaths), color = "blue") +
  geom_point(y = scale(usa$excess_mortality), color = "green", size = 3) +
  labs(x = 'Date', y = 'Mortality vs Spreading') + 
  scale_x_date(date_breaks = '9 months', date_labels = '%Y-%m') +
  ylim(-2,6.5) +
  theme_bw() +theme(
    axis.text=element_text(size=10)
    ,axis.title=element_text(size=8,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'))
usa_mort
  
bra_mort <- ggplot(data = bra, aes(x = date, y = scale(new_cases), na.rm = TRUE)) +  
  geom_line(color = "red") + 
  geom_line(y = scale(bra$new_deaths), color = "blue") +
  geom_point(y = scale(bra$excess_mortality), color = "green", size = 3) +
  labs(x = 'Date', y = 'Mortality vs Spreading') + 
  scale_x_date(date_breaks = '9 months', date_labels = '%Y-%m') +
  ylim(-2,6.5) +
  theme_bw() +theme(
    axis.text=element_text(size=10)
    ,axis.title=element_text(size=8,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'))
bra_mort 
  
ukr_mort <- ggplot(data = ukr, aes(x = date, y = scale(new_cases), na.rm = TRUE)) +  
  geom_line(color = "red") + 
  geom_line(y = scale(ukr$new_deaths), color = "blue") +
  geom_point(y = scale(ukr$excess_mortality), color = "green", size = 3) +
  labs(x = 'Date', y = 'Mortality vs Spreading') + 
  scale_x_date(date_breaks = '9 months', date_labels = '%Y-%m') +
  ylim(-2,6.5) +
  theme_bw() +theme(
    axis.text=element_text(size=10)
    ,axis.title=element_text(size=8,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'))
ukr_mort 

ita_mort <- ggplot(data = ita, aes(x = date, y = scale(new_cases), na.rm = TRUE)) +  
  geom_line(color = "red") + 
  geom_line(y = scale(ita$new_deaths), color = "blue") +
  geom_point(y = scale(ita$excess_mortality), color = "green", size = 3) +
  labs(x = 'Date', y = 'Mortality vs Spreading') + 
  scale_x_date(date_breaks = '9 months', date_labels = '%Y-%m') +
  ylim(-2,6.5) +
  theme_bw() +theme(
    axis.text=element_text(size=10)
    ,axis.title=element_text(size=8,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'))


gbr_mort <- ggplot(data = gbr, aes(x = date, y = scale(new_cases), na.rm = TRUE)) +  
  geom_line(color = "red") + 
  geom_line(y = scale(gbr$new_deaths), color = "blue") +
  geom_point(y = scale(gbr$excess_mortality), color = "green", size = 3) +
  labs(x = 'Date', y = 'Mortality vs Spreading') + 
  scale_x_date(date_breaks = '9 months', date_labels = '%Y-%m') +
  ylim(-2,6.5) +
  theme_bw() +theme(
    axis.text=element_text(size=10)
    ,axis.title=element_text(size=8,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'))


ger_mort <- ggplot(data = ger, aes(x = date, y = scale(new_cases), na.rm = TRUE)) +  
  geom_line(color = "red") + 
  geom_line(y = scale(ger$new_deaths), color = "blue") +
  geom_point(y = scale(ger$excess_mortality), color = "green", size = 3) +
  labs(x = 'Date', y = 'Mortality vs Spreading') + 
  scale_x_date(date_breaks = '9 months', date_labels = '%Y-%m') +
  ylim(-2,6.5) +
  theme_bw() +theme(
    axis.text=element_text(size=10)
    ,axis.title=element_text(size=8,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'))


fra_mort <- ggplot(data = fra, aes(x = date, y = scale(new_cases), na.rm = TRUE)) +  
  geom_line(color = "red") + 
  geom_line(y = scale(fra$new_deaths), color = "blue") +
  geom_point(y = scale(fra$excess_mortality), color = "green", size = 3) +
  labs(x = 'Date', y = 'Mortality vs Spreading') + 
  scale_x_date(date_breaks = '9 months', date_labels = '%Y-%m') +
  ylim(-2,6.5) +
  theme_bw() +theme(
    axis.text=element_text(size=10)
    ,axis.title=element_text(size=8,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'))

bgr_mort <- ggplot(data = bgr, aes(x = date, y = scale(new_cases), na.rm = TRUE)) +  
  geom_line(color = "red") + 
  geom_line(y = scale(bgr$new_deaths), color = "blue") +
  geom_point(y = scale(bgr$excess_mortality), color = "green", size = 3) +
  labs(x = 'Date', y = 'Mortality vs Spreading') + 
  scale_x_date(date_breaks = '9 months', date_labels = '%Y-%m') +
  ylim(-2,6.5) +
  theme_bw() +theme(
    axis.text=element_text(size=10)
    ,axis.title=element_text(size=8,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'))

srb_mort <- ggplot(data = srb, aes(x = date, y = scale(new_cases), na.rm = TRUE)) +  
  geom_line(color = "red") + 
  geom_line(y = scale(srb$new_deaths), color = "blue") +
  geom_point(y = scale(srb$excess_mortality), color = "green", size = 3) +
  labs(x = 'Date', y = 'Mortality vs Spreading') + 
  scale_x_date(date_breaks = '9 months', date_labels = '%Y-%m') +
  ylim(-2,6.5) +
  theme_bw() +theme(
    axis.text=element_text(size=10)
    ,axis.title=element_text(size=8,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'))

rou_mort <- ggplot(data = rou, aes(x = date, y = scale(new_cases), na.rm = TRUE)) +  
  geom_line(color = "red") + 
  geom_line(y = scale(rou$new_deaths), color = "blue") +
  geom_point(y = scale(rou$excess_mortality), color = "green", size = 3) +
  labs(x = 'Date', y = 'Mortality vs Spreading') + 
  scale_x_date(date_breaks = '9 months', date_labels = '%Y-%m') +
  ylim(-2,6.5) +
  theme_bw() +theme(
    axis.text=element_text(size=10)
    ,axis.title=element_text(size=8,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'))

per_mort <- ggplot(data = per, aes(x = date, y = scale(new_cases), na.rm = TRUE)) +  
  geom_line(color = "red") + 
  geom_line(y = scale(per$new_deaths), color = "blue") +
  geom_point(y = scale(per$excess_mortality), color = "green", size = 3) +
  labs(x = 'Date', y = 'Mortality vs Spreading') + 
  scale_x_date(date_breaks = '9 months', date_labels = '%Y-%m') +
  ylim(-2,6.5) +
  theme_bw() +theme(
    axis.text=element_text(size=10)
    ,axis.title=element_text(size=8,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'))

mex_mort <- ggplot(data = mex, aes(x = date, y = scale(new_cases), na.rm = TRUE)) +  
  geom_line(color = "red") + 
  geom_line(y = scale(mex$new_deaths), color = "blue") +
  geom_point(y = scale(mex$excess_mortality), color = "green", size = 3) +
  labs(x = 'Date', y = 'Mortality vs Spreading') + 
  scale_x_date(date_breaks = '9 months', date_labels = '%Y-%m') +
  ylim(-2,6.5) +
  theme_bw() +theme(
    axis.text=element_text(size=10)
    ,axis.title=element_text(size=8,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'))

pol_mort <- ggplot(data = pol, aes(x = date, y = scale(new_cases), na.rm = TRUE)) +  
  geom_line(color = "red") + 
  geom_line(y = scale(pol$new_deaths), color = "blue") +
  geom_point(y = scale(pol$excess_mortality), color = "green", size = 3) +
  labs(x = 'Date', y = 'Mortality vs Spreading') + 
  scale_x_date(date_breaks = '9 months', date_labels = '%Y-%m') +
  ylim(-2,6.5) +
  theme_bw() +theme(
    axis.text=element_text(size=10)
    ,axis.title=element_text(size=8,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'))

spa_mort <- ggplot(data = spa, aes(x = date, y = scale(new_cases), na.rm = TRUE)) +  
  geom_line(color = "red") + 
  geom_line(y = scale(spa$new_deaths), color = "blue") +
  geom_point(y = scale(spa$excess_mortality), color = "green", size = 3) +
  labs(x = 'Date', y = 'Mortality vs Spreading') + 
  scale_x_date(date_breaks = '9 months', date_labels = '%Y-%m') +
  ylim(-2,6.5) +
  theme_bw() +theme(
    axis.text=element_text(size=10)
    ,axis.title=element_text(size=8,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'))

phl_mort <- ggplot(data = phl, aes(x = date, y = scale(new_cases), na.rm = TRUE)) +  
  geom_line(color = "red") + 
  geom_line(y = scale(phl$new_deaths), color = "blue") +
  geom_point(y = scale(phl$excess_mortality), color = "green", size = 3) +
  labs(x = 'Date', y = 'Mortality vs Spreading') + 
  scale_x_date(date_breaks = '9 months', date_labels = '%Y-%m') +
  ylim(-2,6.5) +
  theme_bw() +theme(
    axis.text=element_text(size=10)
    ,axis.title=element_text(size=8,face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'))

#1
ggarrange(rus_mort, usa_mort,
          bra_mort, ukr_mort,
          labels = c('RUS', 'USA', 'BRA', 'UKR'),
          hjust = c(-8,-8,-8,-8),
          ncol = 2, nrow = 2)

#2
ggarrange(ita_mort, gbr_mort,
          ger_mort, fra_mort,
          labels = c('ITA', 'GBR', 'GER', 'FRA'),
          hjust = c(-8,-6,-6,-6.5),
          ncol = 2, nrow = 2)
#3
ggarrange(bgr_mort, srb_mort,
          rou_mort, per_mort,
          labels = c('BGR', 'SRB', 'ROU', 'PER'),
          hjust = c(-8,-6,-8,-6.5),
          ncol = 2, nrow = 2)
#4
ggarrange(mex_mort, pol_mort,
          spa_mort, phl_mort,
          labels = c('MEX', 'POL', 'ESP', 'PHL'),
          hjust = c(-5.5,-6,-6,-6.5),
          ncol = 2, nrow = 2)

