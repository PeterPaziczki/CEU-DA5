rm(list = ls())
library(WDI)
library(data.table)
library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(plm)
library(stargazer)
library(fBasics)
library(pander)
library(knitr)
library(dplyr)
library(sandwich) 
library(lmtest)

# DOWNLOADING DATA THROUGH World Bank API-------------------------------------------------------

# SEARCHING FOR DATA: Wage and salaried workers, female (% of female employed)
wag_inds <- WDIsearch('wage')
wagCode <- wag_inds[match
                    ("Wage and salaried workers, female (% of females employed)", 
                      wag_inds[,2],1)]
# DATA DOWNLOAD: Wage and salaried workers, female (% of females employed)
dat_wag = WDI(
  indicator = wagCode, 
  start = 1991, end = 2017)
# FILTERING OUT REGIONS
dt_wag <- data.table(dat_wag)
exclusionList <- dt_wag[,.(itemCnt = .N),by = .(code = dt_wag$iso2c)][1:47, 1]
wagData <- subset(dt_wag, !(dt_wag$iso2c %in%  exclusionList$code))
setnames(wagData, 
         c(paste(wagCode)), 
         c("salaried"))

# SEARCHING FOR DATA: Fertility rate, total (births per woman)
fert_inds <- WDIsearch('fertility')
fertCode <- fert_inds[match
                    ("Fertility rate, total (births per woman)", 
                      fert_inds[,2],1)]
# DATA DOWNLOAD: Fertility rate, total (births per woman)
dat_fert = WDI(
  indicator = fertCode, 
  start = 1960, end = 2015)
# FILTERING OUT REGIONS
dt_fert <- data.table(dat_fert)
exclusionList <- dt_fert[,.(itemCnt = .N),by = .(code = dt_fert$iso2c)][1:47, 1]
fertData <- subset(dt_fert, !(dt_fert$iso2c %in%  exclusionList$code))
setnames(fertData, 
         c(paste(fertCode)), 
         c("fert"))

subset(fertData, fertData$year == 2015)
summary(fertData_2015$fert)
hist_2015 <- hist(fertData_2015$fert,
     main = "Histogram of fertility rate in 2015",
     xlab = "Fertility rate")

subset(fertData, fertData$year == 1960)
summary(fertData_1960$fert)
hist_1960 <- hist(fertData_1960$fert,
     main = "Histogram of fertility rate in 1960",
     xlab = "Fertility rate")


# SEARCHING FOR DATA: Adjusted savings education expenditure (% of GNI)
edu_inds <- WDIsearch('adjusted')
eduCode <- edu_inds[match
                       ("Adjusted savings: education expenditure (% of GNI)", 
                         edu_inds[,2],1)]
# DATA DOWNLOAD: Adjusted savings education expenditure (% of GNI)
dat_edu = WDI(
  indicator = eduCode, 
  start = 1970, end = 2015)
# FILTERING OUT REGIONS
dt_edu <- data.table(dat_edu)
exclusionList <- dt_edu[,.(itemCnt = .N),by = .(code = dt_edu$iso2c)][1:47, 1]
eduData <- subset(dt_edu, !(dt_edu$iso2c %in%  exclusionList$code))
setnames(eduData, 
         c(paste(eduCode)), 
         c("edu"))

# SEARCHING FOR DATA: Health expenditure, total (% of GDP)
health_inds <- WDIsearch('health')
healthCode <- health_inds[match
                          ("Health expenditure, total (% of GDP)", 
                            health_inds[,2],1)]
# DATA DOWNLOAD: Health expenditure, total (% of GDP)
dat_health = WDI(
  indicator = healthCode, 
  start = 1995, end = 2014)
# FILTERING OUT REGIONS
dt_health <- data.table(dat_health)
exclusionList <- dt_health[,.(itemCnt = .N),by = .(code = dt_health$iso2c)][1:47, 1]
healthData <- subset(dt_health, !(dt_health$iso2c %in%  exclusionList$code))
setnames(healthData, 
         c(paste(healthCode)), 
         c("health"))

# SEARCHING FOR DATA: GDP per capita 
gdp_inds <- WDIsearch('gdp')
grep("2005", gdp_inds, value = TRUE )
gdpppCode <- gdp_inds[match
                      ("GDP per capita, PPP (constant 2005 international $)",
                        gdp_inds[,2]),1]
# DATA DOWNLOAD: GDP per capita
dat = WDI(
  indicator=gdpppCode, 
  start = 1990, end = 2016)
# FILTERING OUT REGIONS
dt <- data.table(dat)
exclusionList <-dt[,.(itemCnt=.N),by=.(code = dt$iso2c)][1:47,1]
gdpData <- subset(dt, !(dt$iso2c %in%  exclusionList$code))
setnames(gdpData, 
         c(paste(gdpppCode)), 
         c("gdppc"))

# SEARCHING FOR DATA: Population, total
pop_inds <- WDIsearch('population')
popCode <- pop_inds[match
                    ("Population, total", 
                      pop_inds[,2],1)]
# DATA DOWNLOAD: Population
dat_pop = WDI(
  indicator = popCode, 
  start = 1960, end = 2016)
# FILTERING OUT REGIONS
dt_pop <- data.table(dat_pop)
exclusionList <- dt_pop[,.(itemCnt = .N),by = .(code = dt_pop$iso2c)][1:47, 1]
popData <- subset(dt_pop, !(dt_pop$iso2c %in%  exclusionList$code))
setnames(popData, 
         c(paste(popCode)), 
         c("pop"))

# SEARCHING FOR DATA: Human Development Index
hdi_inds <- WDIsearch('hdi')
hdiCode <- hdi_inds[match
                    ("Human development index (HDI)", 
                      hdi_inds[,2],1)]
# DATA DOWNLOAD: Population
dat_hdi = WDI(
  indicator = hdiCode, 
  start = 1960, end = 2017)

# there is not enough data, need to find another source

# HDI - Human Development index from https://ourworldindata.org
dat_hdi <- fread('Human development index (HDI).csv')
dat_hdi <- gather(dat_hdi, key = "year", value = "count", '1990':'2015')
dat_hdi <- dat_hdi[order(dat_hdi$Country), ]
dt_hdi <- data.table(dat_hdi)
# Renaming a few columns
strTmp = c('HDI Rank (2015)','year','count')
# Transforming the character variables into numeric variables
dt_hdi <- dt_hdi[, (strTmp) := lapply(.SD, as.numeric), .SDcols = strTmp]
setnames(dt_hdi, 
         c("HDI Rank (2015)", "Country"), 
         c("hdi_rank" ,"country"))
# Removing the leading white space character from the beginning of country names
strTmp = c('country')
dt_hdi <- dt_hdi[, (strTmp) := lapply(.SD, trimws), .SDcols = strTmp]
str(dt_hdi)
hdiData <- dt_hdi
setnames(hdiData, 
         c("count"), 
         c("hdi"))

# MERGING DATA INTO PANEL
panelData_1 <- merge(wagData, gdpData,
                     by.x = c("iso2c", "year", "country"), 
                     by.y = c("iso2c", "year", "country") ) 
panelData_2 <- merge(healthData, fertData,
                     by.x = c("iso2c", "year", "country"), 
                     by.y = c("iso2c", "year", "country") ) 
panelData_3 <- merge(panelData_1, panelData_2,  
                     by.x = c("iso2c", "year", "country"), 
                     by.y = c("iso2c", "year", "country") )
panelData_4 <- merge(eduData, panelData_3,  
                     by.x = c("iso2c", "year", "country"), 
                     by.y = c("iso2c", "year", "country") )
panelData_5 <- merge(popData, panelData_4,  
                     by.x = c("iso2c", "year", "country"), 
                     by.y = c("iso2c", "year", "country") )
panelData <- merge(hdiData, panelData_5,
                     by.x = c("country", "year"), 
                     by.y = c("country", "year") )

panelData_fert <- merge(popData, fertData,  
                        by.x = c("iso2c", "year", "country"), 
                        by.y = c("iso2c", "year", "country") )

# CLEANING AND TRANSOFMRING THE DATA------------------------------------------------------------

panelData$iso2c <- NULL

up <- data.table(panelData)
#names(up) <- c('year', 'country', 'health', 'fert', 'pop', 'salaried', 'gdppc')
up$pop <- up$pop/10^6 # Count the population in million
up <- subset(up, up$year < 2017 & up$year > 1960)

# Fertility rate
up_fert <- data.table(panelData_fert)
up_fert$iso2c <- NULL
names(up_fert) <- c('year', 'country', 'pop', 'fert')

# DATA EXPLORATION------------------------------------------------------------------------------

# non-missing observations for each country
bp <- subset(up,
             !is.na(up$salaried) & 
               !is.na(up$fert) & 
               !is.na(up$pop) & 
               !is.na(up$gdppc) & 
               !is.na(up$health) &
               !is.na(up$edu) &
               !is.na(up$hdi)
)

bp %>%
  count(country) %>%
  arrange(n) %>%
  print(n = 260)

# non-missing observations for fertility rate
bp_fert <- subset(up_fert,
             !is.na(up_fert$fert) & 
             !is.na(up_fert$pop)
)

bp_fert %>%
  count(country) %>%
  arrange(n) %>%
  print(n = 260)

# checking world trends
world_trend <- up %>%
  group_by(year) %>%
  summarise(
    salaried = weighted.mean(salaried, na.rm = TRUE),
    fert = weighted.mean(fert, w = pop, na.rm = TRUE),
    health = weighted.mean(health, w = pop, na.rm = TRUE),
    edu = weighted.mean(edu, w = pop, na.rm = TRUE),
    gdppc = weighted.mean(gdppc, w = pop, na.rm = TRUE),
    hdi = weighted.mean(hdi, w = pop, na.rm = TRUE))

world_trend$rellnsalaried = log(world_trend$salaried) - first(log(world_trend$salaried))
world_trend$rellnhealth = log(world_trend$health) - first(log(world_trend$health))
world_trend$rellnedu = log(world_trend$edu) - first(log(world_trend$edu))
world_trend$rellngdp = log(world_trend$gdppc) - first(log(world_trend$gdppc))
world_trend$rellnfert = log(world_trend$fert) - first(log(world_trend$fert))
world_trend$rellnhdi = log(world_trend$hdi) - first(log(world_trend$hdi))

# checking world trends for fertility rate
world_trend_fert <- bp_fert %>%
  group_by(year) %>%
  summarise(
    
    fert = weighted.mean(fert, w = pop, na.rm = TRUE))

world_trend_fert$rellnfert = log(world_trend_fert$fert) - first(log(world_trend_fert$fert))

# subplot for global fertility in years 1960-2011

world_trend_fert %>%
  ggplot(aes(year, fert)) + 
  geom_line(color = "deepskyblue3", size = 1) +
  #geom_point(color = "deepskyblue3") +
  #geom_smooth(method = "lm", se = FALSE) + 
  labs(
    x ="years",
    y = "Fertility rate, total (births per woman)",
    title ="Total fertility rate (births per woman) in years 1960-2011")

up_int %>%
  group_by(country) %>%
  ggplot(aes(year, fert)) + 
  geom_line(aes(color = country, linetype = country), size = 1) +
  ylab('Total fertility rate (births per woman) in years 1960-2011')

# subplot (1)
p1 <- world_trend_fert %>%
  ggplot(aes(year, fert)) + 
  geom_line(size = 1, color = 'darkgreen') + 
  ylab('Fertility rate, total (births per woman)')

# subplot (2)
p2 <- world_trend %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = rellnfert), size = 1, linetype = 'dotted', color = 'darkgreen') +
  geom_line(aes(y = rellnsalaried), size = 1, linetype = 'dashed', color = 'firebrick') +
  geom_text(x = 2004, y = -0.25, label = 'Maternal mortality ratio (per 100,000 live births)', color = 'darkgreen') +
  geom_text(x = 2009, y = -0.05, label = 'salaried female (% of total emolyed)', color = 'firebrick') +
  ylab('log change from 1995')

# arrange these two subplots

grid.arrange(p1, p2)

# subplot (3)
p3 <- world_trend %>%
  ggplot(aes(year, salaried)) + 
  geom_line(size = 1, color = 'firebrick') + 
  ylab('salaried female (% of total emolyed)') # y-axis label

# subplot (4)
p4 <- world_trend %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = rellnfert), size = 1, linetype = 'dotted', color = 'darkgreen') +
  geom_line(aes(y = rellnhealth), size = 1, linetype = 'dashed', color = 'deepskyblue3') +
  geom_text(x = 2004, y = -0.25, label = 'Maternal mortality ratio (per 100,000 live births)', color = 'darkgreen') +
  geom_text(x = 2006, y = 0.01, label = 'Healthcare expenditure (% of GDP)', color = 'deepskyblue3') +
  ylab('log change from 1995')

# arrange these two subplots
grid.arrange(p3, p4)

# country-specific trends: selected countries, difference between 1995 and 2014
interesting_countries <- c(
  "Rwanda", "Kenya", "Philippines", "Hungary", 
  "China", "Luxembourg", "Yemen, Rep.", "United Kingdom", "United States"
)

up_int <- subset(up, up$country %in% interesting_countries, na.rm = TRUE)
p5 <- up_int %>%
  group_by(country) %>%
  ggplot(aes(year, salaried)) + 
  geom_line(aes(color = country, linetype = country), size = 1) +
  ylab('Ratio of wage and salaried of female employment')

p6 <- up_int %>%
  group_by(country) %>%
  ggplot(aes(year, fert)) + 
  geom_line(aes(color = country, linetype = country), size = 1) +
  ylab('Maternal mortality ratio (per 100,000 live births)')

p7 <- up_int %>%
  group_by(country) %>%
  ggplot(aes(year, health)) + 
  geom_line(aes(color = country, linetype = country), size = 1) +
  ylab('Health expenditure (% of GDP)')

# arrange these three subplots
grid.arrange(p5, p6, p7)

# Change in fertility rate in countries of interest
up_int %>%
  group_by(country) %>%
  ggplot(aes(year, fert)) + 
  geom_line(aes(color = country, linetype = country), size = 1) +
  ylab('Total fertility rate (births per woman) in years 1960-2011')


# examining the balanced panel with no missing values on key variables

ggplot(bp) + aes(x = fert) +
  geom_histogram (binwidth = 20, 
                  fill ='darkgreen') +
  labs(
    x ="Maternal mortality ratio (per 100,000 live births)",
    title ="Maternal mortality ratio (per 100,000 live births) in years 1995-2014") +
  theme_bw()

ggplot(bp) + aes(x = salaried) +
  geom_histogram (binwidth = 1, 
                  fill ='firebrick') +
  labs(
    x ="Ratio of wage and salaried of female employment')",
    title ="Ratio of wage and salaried of female employment') in years 1995-2014") +
  theme_bw()


ggplot(bp) + aes(x = health) +
  geom_histogram (binwidth = 0.5, 
                  fill ='deepskyblue3') +
  labs(
    x ="Health expenditure (% of GDP)",
    title ="Health expenditure (% of GDP) in years 1995-2014") +
  theme_bw()

ggplot(bp) + aes(x = gdppc) +
  geom_histogram(binwidth = 1000, 
                 fill ='grey') +
  labs(
    x = "GDP per capita distribution across all years, constant 2005 prices",
    title = "Histogram of GDP per capita distribution in years 1995-2014") +
  theme_bw()

stargazer(
  bp,
  header = FALSE, 
  out = "summarystats.html",
  type = 'latex',
  omit = 'year',
  title = "Descriptive Statistics for the variables in the balanced panel")

up$lngdppc <- log(up$gdppc)
up$lnfert <- log(up$fert)
up$lnpop <- log(up$pop)
up$lnhealth <- log(up$health)
up$lnedu <- log(up$edu)

# ANALYSIS--------------------------------------------------------------------------------------

# xcountry OLS: pooled OLS regardless country difference
dt <- up[, .(
  m_salaried = mean(salaried, na.rm = TRUE),
  m_lnfert= mean(lnfert, na.rm = TRUE),
  m_lngdppc = mean(lngdppc, na.rm = TRUE),
  m_lnpop = mean(lnpop, na.rm = TRUE),
  m_lnhealth = mean(lnhealth, na.rm = TRUE),
  m_lnedu = mean(lnedu, na.rm = TRUE)),
  by = .(country)]

p8 <- ggplot(dt, aes(x = m_salaried, y = m_lnfert)) +
  geom_point(size = 1.5, aes(col = (m_lnhealth))) +
  geom_smooth() +
  labs(
    x = "Average salaried female workers (% of total) per country",
    y = "Average Log Fertility rate per country",
    title ="Average Log Maternal Mortality on salaried female workers (% of total)") +
  scale_color_distiller("Log Health expenditure (% of GDP)", palette = "Spectral") +
  theme_bw()

p9 <- ggplot(dt, aes(x = m_salaried, y = m_lnfert)) +
  geom_point(size = 1.5, aes(col = (m_lngdppc))) +
  geom_smooth() +
  labs(
    x = "Average salaried female workers (% of total) per country",
    y = "Average Log Maternal Mortality per country",
    title ="Average Log Maternal Mortality on salaried female workers (% of total)") +
  scale_color_distiller("Log GDP per capita (const. 2005 $)", palette = "Spectral") +
  theme_bw()

grid.arrange(p8, p9)


# OLS regressions
ols1995 <- lm(data = up[year == 1995], lnfert ~ salaried)
ols2007 <- lm(data = up[year == 2007], lnfert ~ salaried)
ols2014 <- lm(data = up[year == 2014], lnfert ~ salaried)
ols2007_c1 <- lm(data = up[year == 2007], lnfert ~ salaried + lnhealth + lnpop + lnedu)
ols2007_c2 <- lm(data = up[year == 2007], lnfert ~ salaried + lngdppc + lnhealth + lnpop + lnedu)
ols2007_c3 <- lm(data = up[year == 2007], lnfert ~ salaried + lngdppc + lnhealth + lnpop + lnedu + lnhdi)
ols2007_c4 <- lm(data = up[year == 2007], lnfert ~ salaried + lnhdi)


ols_models <- list(ols1995, ols2007, ols2014, ols2007_c1, ols2007_c2, ols2007_c3, ols2007_c4)

cov_ols_1         <- vcovHC(ols1995, type = "HC1")
rob_ols_1         <- sqrt(diag(cov_ols_1))
cov_ols_2         <- vcovHC(ols2007, type = "HC1")
rob_ols_2         <- sqrt(diag(cov_ols_2))
cov_ols_3         <- vcovHC(ols2014, type = "HC1")
rob_ols_3         <- sqrt(diag(cov_ols_3))
cov_ols_4         <- vcovHC(ols2007_c1, type = "HC1")
rob_ols_4         <- sqrt(diag(cov_ols_4))
cov_ols_5         <- vcovHC(ols2007_c2, type = "HC1")
rob_ols_5         <- sqrt(diag(cov_ols_5))
cov_ols_6         <- vcovHC(ols2007_c3, type = "HC1")
rob_ols_6         <- sqrt(diag(cov_ols_6))
cov_ols_7         <- vcovHC(ols2007_c4, type = "HC1")
rob_ols_7         <- sqrt(diag(cov_ols_7))

stargazer(
  title = "OLS models", 
  list(ols1995, ols2007, ols2014, ols2007_c1, ols2007_c2, ols2007_c3, ols2007_c4), digits = 2, 
  column.labels = c('ols1995', 'ols2007', 'ols2014', 'ols2007c1', 'ols2007c2', 'ols2007c3', 'ols2007c4'),
  model.names = FALSE,
  omit.stat = c("adj.rsq", "f"),
  dep.var.caption = 'Dependent variable: Maternal Mortality',
  out = "OLS.html",
  notes.align = "l",
  se = list(rob_ols_1, rob_ols_2, rob_ols_3, rob_ols_4, rob_ols_5, rob_ols_6, rob_ols_7),
  header = FALSE, 
  type ='latex'
)


# FE with explicit time dummies

fe1 <- plm( 
  lnfert ~ salaried + year, data = up, 
  model = 'within')

fe2 <- plm( 
  lnfert ~ salaried + lngdppc + year, data = up, 
  model = 'within')

fe3 <- plm( 
  lnfert ~ salaried + lnhealth + lnedu + year, data = up, 
  model = 'within')

fe4 <- plm( 
  lnfert ~ salaried + lngdppc + lnhealth + lnedu + lnpop + year, data = up, 
  model = 'within')

fe_models <- list(fe1, fe2, fe3, fe4)

cov_fe_1         <- vcovSCC(fe1, type = "HC1")
rob_fe_1         <- sqrt(diag(cov_fe_1))
cov_fe_2         <- vcovSCC(fe2, type = "HC1")
rob_fe_2         <- sqrt(diag(cov_fe_2))
cov_fe_3         <- vcovSCC(fe3, type = "HC1")
rob_fe_3         <- sqrt(diag(cov_fe_3))
cov_fe_4         <- vcovSCC(fe4, type = "HC1")
rob_fe_4         <- sqrt(diag(cov_fe_4))

stargazer(
  title = "Comparing FE models with expicit time dummies", 
  list(fe_models), digits = 2, 
  column.labels = c('FE1', 'FE2', 'FE3', 'FE4'),
  model.names = FALSE,
  omit.stat = c("adj.rsq", "f", "ser"),
  dep.var.caption = 'Dependent variable: Log Maternal Mortality',
  out = "FE.html",
  notes.align = "l",
  se = list(rob_fe_1, rob_fe_2, rob_fe_3, rob_fe_4),
  dep.var.labels.include = FALSE,
  header = FALSE, 
  type ='latex'
)


# FD with explicit time dummies and lags

diff1 <- plm(
  diff(lnfert) ~ diff(salaried) + year, 
  data = up, model = 'pooling'
)

diff2 <- plm(
  diff(lnfert) ~ diff(salaried) + stats::lag(diff(salaried), 1:2) + year,
  data = up, model = 'pooling'
)

diff3 <- plm(
  diff(lnfert) ~ diff(salaried) + stats::lag(diff(salaried), 1:4) + year, 
  data = up, model = 'pooling'
)

diff4 <- plm(
  diff(lnfert) ~ diff(salaried) + stats::lag(diff(salaried), 1:6) + year,
  data = up, model = 'pooling'
)

cov_fd_1         <- vcovSCC(diff1, type = "HC1")
rob_fd_1    <- sqrt(diag(cov_fd_1))
cov_fd_2         <- vcovSCC(diff2, type = "HC1")
rob_fd_2    <- sqrt(diag(cov_fd_2))
cov_fd_3         <- vcovSCC(diff3, type = "HC1")
rob_fd_3    <- sqrt(diag(cov_fd_3))
cov_fd_4         <- vcovSCC(diff4, type = "HC1")
rob_fd_4    <- sqrt(diag(cov_fd_4))

fd_models <- list(diff1, diff2, diff3, diff4)

stargazer(
  title = "Comparing FD models with expicit time dummies", 
  list(fd_models), digits = 2, 
  column.labels = c('FD1', 'FD2', 'FD3', 'FD4'),
  model.names = FALSE,
  omit.stat = c("adj.rsq", "f", "ser"),
  dep.var.caption = 'Dependent variable: Log Maternal Mortality',
  out = "FD.html",
  notes.align = "l",
  se = list(rob_fd_1, rob_fd_2, rob_fd_3, rob_fd_4),
  add.lines = list(
    c("Cumulative Coeff", '-0.04', '-0.045', '-0.04', '1.26')),
  dep.var.labels.include = FALSE,
  header = FALSE, 
  type ='latex'
)

# FD with explicit time dummies, lags and controls

diff_c1 <- plm(
  diff(lnfert) ~ diff(salaried) + year + diff(lnhealth) + diff(lnedu) + diff(lngdppc) + diff(pop), 
  data = up, model = 'pooling'
)

diff_c2 <- plm(
  diff(lnfert) ~ diff(salaried) + stats::lag(diff(salaried), 1:2) + 
    stats::lag(diff(lnhealth), 1:2) + 
    stats::lag(diff(lnedu), 1:2) + 
    stats::lag(diff(lngdppc), 1:2) + 
    stats::lag(diff(lnpop), 1:2) + 
    year,
  data = up, model = 'pooling'
)

diff_c3 <- plm(
  diff(lnfert) ~ diff(salaried) + stats::lag(diff(salaried), 1:4) +
    stats::lag(diff(lnhealth), 1:4) + 
    stats::lag(diff(lnedu), 1:4) + 
    stats::lag(diff(lngdppc), 1:4) + 
    stats::lag(diff(lnpop), 1:4) + 
    year, 
  data = up, model = 'pooling'
)


cov_fdc_1         <- vcovSCC(diff_c1, type = "HC1")
rob_fdc_1    <- sqrt(diag(cov_fdc_1))
cov_fdc_2         <- vcovSCC(diff_c2, type = "HC1")
rob_fdc_2    <- sqrt(diag(cov_fdc_2))
cov_fdc_3         <- vcovSCC(diff_c3, type = "HC1")
rob_fdc_3    <- sqrt(diag(cov_fdc_3))

fd_models_controls <- list(diff_c1, diff_c2, diff_c3)

stargazer(
  title = "Comparing FD models with expicit time dummies and controls", 
  list(fd_models_controls), digits = 2, 
  column.labels = c('FD5', 'FD6', 'FD7'),
  model.names = FALSE,
  omit.stat = c("adj.rsq", "f", "ser"),
  dep.var.caption = 'Dependent variable: Log Maternal Mortality',
  out = "FD_controls.html",
  notes.align = "l",
  se = list(rob_fdc_1, rob_fdc_2, rob_fdc_3),
  add.lines = list(
    c("Cumulative Coeff", '-0.01', '-0.104', '0.02')),
  dep.var.labels.include = FALSE,
  header = FALSE, 
  type ='latex'
)

# Multiple models

multiple_models = list(ols2007_c1, fe4, diff2, diff_c2)

stargazer(
  title = "Comparing multiple models", 
  list(multiple_models), digits = 2, 
  column.labels = c('OLS2007c1', 'FE4c', 'FD2', 'FD2c'),
  model.names = FALSE,
  omit.stat = c("adj.rsq", "f", "ser"),
  dep.var.caption = 'Dependent variable: Log Fertility rate',
  out = "Multiple.html",
  notes.align = "l",
  se = list(rob_ols_4, rob_fe_4, rob_fd_2, rob_fdc_2),
  dep.var.labels.include = FALSE,
  header = FALSE, 
  type ='latex',
  omit = c('year', 'lngdppc', 'lnhealth', 'lnpop', 'lnedu',
           'stats::lag(diff(lnhealth), 1:2)', 
           'stats::lag(diff(lnedu), 1:2)', 
           'stats::lag(diff(lngdppc), 1:2)',
           'stats::lag(diff(lnpop), 1:2)')
)

# Grouping countries by income

# Group 1
up %>% 
  select(salaried, gdppc, pop, hdi_rank) %>%
  as.data.frame() %>%
  stargazer(
    type = 'text', flip = TRUE, digits = 1,
    summary.stat = c('mean', 'min', 'median', 'p25', 'p75', 'max', 'n')
  )

up$group <- ifelse(up$hdi_rank <= 90.5,1,2)
up_1 <- subset(up, up$group == 1)
up_2 <- subset(up, up$group == 2)

g_fd2c <- plm(
  diff(lnfert) ~ diff(salaried) + stats::lag(diff(salaried), 1:2) + 
    stats::lag(diff(lnhealth), 1:2) + 
    stats::lag(diff(lnedu), 1:2) + 
    stats::lag(diff(lngdppc), 1:2) + 
    stats::lag(diff(lnpop), 1:2) + 
    year,
  data = up_1, model = 'pooling'
)

g_fe4c <- plm( 
  lnfert ~ salaried + lngdppc + lnhealth + lnedu + lnpop + year, 
  data = up_1, 
  model = 'within')

# Group 2

gg_fd2c <- plm(
  diff(lnfert) ~ diff(salaried) + stats::lag(diff(salaried), 1:2) + 
    stats::lag(diff(lnhealth), 1:2) + 
    stats::lag(diff(lnedu), 1:2) + 
    stats::lag(diff(lngdppc), 1:2) + 
    stats::lag(diff(lnpop), 1:2) + 
    year,
  data = up_2, model = 'pooling'
)

gg_fe4c <- plm( 
  lnfert ~ salaried + lngdppc + lnhealth + lnedu + lnpop + year, 
  data = up_2, 
  model = 'within')

# Standard errors were adjusted to make sure robust standard errors are used
g_cov_fd        <- vcovSCC(g_fd2c, type = "HC1")
g_rob_fd        <- sqrt(diag(g_cov_fd))
g_cov_fe        <- vcovSCC(g_fe4c, type = "HC1")
g_rob_fe        <- sqrt(diag(g_cov_fe))
gg_cov_fd        <- vcovSCC(gg_fd2c, type = "HC1")
gg_rob_fd        <- sqrt(diag(gg_cov_fd))
gg_cov_fe        <- vcovSCC(gg_fe4c, type = "HC1")
gg_rob_fe        <- sqrt(diag(gg_cov_fe))

stargazer(
  title = "Comparing multiple models for high- and low-income countries", 
  list(g_fd2c, g_fe4c, gg_fd2c, gg_fe4c), digits = 2, 
  column.labels = c('FD2c, G:1', 'FE4c, G:1', 'FD2c, G:2', 'FE4c, G:2'),
  model.names = FALSE,
  omit.stat = c("adj.rsq", "f", "ser"),
  dep.var.caption = 'Dependent variable: Log Maternal mortality',
  out = "Grouping.html",
  notes.align = "l",
  se = list(g_rob_fd, g_rob_fe, gg_rob_fd, gg_rob_fe),
  dep.var.labels.include = FALSE,
  header = FALSE, 
  type ='latex',
  omit = c('year', 'lngdppc', 'lnhealth', 'lnpop', 'lnedu',
           'stats::lag(diff(lnhealth), 1:2)', 
           'stats::lag(diff(lnedu), 1:2)', 
           'stats::lag(diff(lngdppc), 1:2)',
           'stats::lag(diff(lnpop), 1:2)')
)

