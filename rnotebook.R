install.packages("dplyr")
install.packages("psych")
install.packages("Metrics")
install.packages("tidyverse")
install.packages("leaps")
install.packages("car")
install.packages("ggplot2")
install.packages("lubridate")

setwd("~/0.Rproject")
rm(list=ls())

library(ggplot2)
library(lubridate)
library(dplyr)
library(psych)
library(MASS)
library(Metrics)
library(readxl)
library(leaps)
library(car)
library(ggplot2)
library(lubridate)

#### Final Assignment 1
### Step 1: Import and prepare the data for analysis
## 1.1 Bring the data into R 

d_2016 <- read.csv("2016_brooklyn.csv")
d_2016 <- d_2016[-1:-4,]
names(d_2016) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')

d_2017 <- read.csv("2017_brooklyn.csv")
d_2017 <- d_2017[-1:-4,]
names(d_2017) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')

d_2018 <- read.csv("2018_brooklyn.csv")
d_2018 <- d_2018[-1:-7,]
names(d_2018) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')

d_2019 <- read.csv("2019_brooklyn.csv")
d_2019 <- d_2019[-1:-4,]
names(d_2019) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')

d_2020 <- read.csv("2020_brooklyn.csv")
d_2020 <- d_2020[-1:-7,]
names(d_2020) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')

## 1.2 Join the data and make it usable for analysis 
cname <- c("lot", "resunits", "comunits", "totunits", "landsqft", "grosssqft", "yrbuilt", "price")

d_2016[] <- lapply(d_2016, function(x) gsub(",","", x))
d_2016[] <- lapply(d_2016, function(x) gsub(" ","", x))
d_2016[] <- lapply(d_2016, function(x) gsub("-","", x))
d_2016[ , cname] <- lapply (d_2016[ , cname], as.numeric)
d_2016$date <- as.Date(d_2016$date, format="%m/%d/%Y")
d_2016$taxclasscurr <- as.factor(substr(d_2016$taxclasscurr,1,1))
d_2016$taxclasssale <- as.factor(substr(d_2016$taxclasssale,1,1))
d_2016$block <- as.factor(d_2016$block)
d_2016$zip <- as.factor(d_2016$zip)
d_2016$bldclasscurr <- as.factor(d_2016$bldclasscurr)
d_2016$bldclasssale <- as.factor(d_2016$bldclasssale)
d_2016$lot <- as.factor(d_2016$lot)
d_2016$neighborhood <- as.factor(d_2016$neighborhood)
d_2016$bldclasscat <- as.factor(d_2016$neighborhood)

d_2017[] <- lapply(d_2017, function(x) gsub(",","", x))
d_2017[] <- lapply(d_2017, function(x) gsub(" ","", x))
d_2017[] <- lapply(d_2017, function(x) gsub("-","", x))
d_2017[ , cname] <- lapply (d_2017[ , cname], as.numeric)
d_2017$date <- as.Date(d_2017$date, format="%m/%d/%y")
d_2017$taxclasscurr <- as.factor(substr(d_2017$taxclasscurr,1,1))
d_2017$taxclasssale <- as.factor(substr(d_2017$taxclasssale,1,1))
d_2017$block <- as.factor(d_2017$block)
d_2017$zip <- as.factor(d_2017$zip)
d_2017$bldclasscurr <- as.factor(d_2017$bldclasscurr)
d_2017$bldclasssale <- as.factor(d_2017$bldclasssale)
d_2017$lot <- as.factor(d_2017$lot)
d_2017$neighborhood <- as.factor(d_2017$neighborhood)
d_2017$bldclasscat <- as.factor(d_2017$neighborhood)

d_2018$price <- gsub("[$]", "", d_2018$price)
d_2018[] <- lapply(d_2018, function(x) gsub(",","", x))
d_2018[] <- lapply(d_2018, function(x) gsub(" ","", x))
d_2018[] <- lapply(d_2018, function(x) gsub("-","", x))
d_2018[ , cname] <- lapply (d_2018[ , cname], as.numeric)
d_2018$date <- as.Date(d_2018$date, format="%m/%d/%y")
d_2018$taxclasscurr <- as.factor(substr(d_2018$taxclasscurr,1,1))
d_2018$taxclasssale <- as.factor(substr(d_2018$taxclasssale,1,1))
d_2018$block <- as.factor(d_2018$block)
d_2018$zip <- as.factor(d_2018$zip)
d_2018$bldclasscurr <- as.factor(d_2018$bldclasscurr)
d_2018$bldclasssale <- as.factor(d_2018$bldclasssale)
d_2018$lot <- as.factor(d_2018$lot)
d_2018$neighborhood <- as.factor(d_2018$neighborhood)
d_2018$bldclasscat <- as.factor(d_2018$neighborhood)

d_2019[] <- lapply(d_2019, function(x) gsub(",","", x))
d_2019[] <- lapply(d_2019, function(x) gsub(" ","", x))
d_2019[] <- lapply(d_2019, function(x) gsub("-","", x))
d_2019[ , cname] <- lapply (d_2019[ , cname], as.numeric)
d_2019$date <- as.Date(d_2019$date, format="%m/%d/%y")
d_2019$taxclasscurr <- as.factor(substr(d_2019$taxclasscurr,1,1))
d_2019$taxclasssale <- as.factor(substr(d_2019$taxclasssale,1,1))
d_2019$block <- as.factor(d_2019$block)
d_2019$zip <- as.factor(d_2019$zip)
d_2019$bldclasscurr <- as.factor(d_2019$bldclasscurr)
d_2019$bldclasssale <- as.factor(d_2019$bldclasssale)
d_2019$lot <- as.factor(d_2019$lot)
d_2019$neighborhood <- as.factor(d_2019$neighborhood)
d_2019$bldclasscat <- as.factor(d_2019$neighborhood)

d_2020[] <- lapply(d_2020, function(x) gsub(",","", x))
d_2020[] <- lapply(d_2020, function(x) gsub(" ","", x))
d_2020[] <- lapply(d_2020, function(x) gsub("-","", x))
d_2020[ , cname] <- lapply (d_2020[ , cname], as.numeric)
d_2020$date <- as.Date(d_2020$date, format="%m/%d/%y")
d_2020$taxclasscurr <- as.factor(substr(d_2020$taxclasscurr,1,1))
d_2020$taxclasssale <- as.factor(substr(d_2020$taxclasssale,1,1))
d_2020$block <- as.factor(d_2020$block)
d_2020$zip <- as.factor(d_2020$zip)
d_2020$bldclasscurr <- as.factor(d_2020$bldclasscurr)
d_2020$bldclasssale <- as.factor(d_2020$bldclasssale)
d_2020$lot <- as.factor(d_2020$lot)
d_2020$neighborhood <- as.factor(d_2020$neighborhood)
d_2020$bldclasscat <- as.factor(d_2020$neighborhood)

# merge data
d_sum_raw <- rbind(d_2016, d_2017, d_2018, d_2019, d_2020)

## 1.3 Filter the data and make transformations specific to this analysis 
# For the purposes of this analysis, we will only consider purchases of single-family residences and single-unit apartments or condos. Restrict the data to purchases where the building class at the time of sale starts with ‘A’ or ‘R’ 
# and where the number of total units and the number of residential units are both 1.  
# Additionally restrict the data to observations where gross square footage is more than 0 and sale price is non-missing.  
# The resulting dataset should have roughly 19,000 rows. -> 19,637 rows after data cleansing
# Removed blank from data sets to get the result!!!
d_sum <- d_sum_raw %>% filter(substr(d_sum_raw$bldclasssale, 1, 1) == 'A' | substr(d_sum_raw$bldclasssale, 1, 1) == 'R')
d_sum <- d_sum %>% filter(d_sum$totunits == 1 & d_sum$resunits ==1)
d_sum <- d_sum %>% filter(d_sum$grosssqft > 0)
d_sum <- d_sum %>% filter(!is.na(d_sum$price))

# Double-check 
table(substr(d_sum$bldclasssale,1,1))
table(d_sum$totunits)
table(d_sum$resunits)
table(d_sum$grosssqft > 0)
table(is.na(d_sum$price))

### Step 2: EDA and feature engineering 
## 2.1 Exploratory data analysis 
## 2.2 Pre-modeling and feature engineering 

# Duplicate data
d_sum1 <- d_sum

# ADD new variables
bldage <- (as.numeric(substr(d_sum1$date,1,4))) - d_sum1$yrbuilt
d_sum1[ , "bldage"] <- bldage

bi_bldclasssale <- substr(d_sum1$bldclasssale, 1, 1)
d_sum1[ , "bi_bldclasssale"] <- bi_bldclasssale
d_sum1$bi_bldclasssale <- as.factor(d_sum1$bi_bldclasssale)

salesage <- 2020 - (as.numeric(substr(d_sum1$date,1,4)))
d_sum1[ , "salesage"] <- salesage

bk_zip <- read_excel("zip_code.xlsx")
d_sum1 <- merge(d_sum1, bk_zip, by="zip")
d_sum1$zip_class <- as.factor(d_sum1$zip_class)

# extract year, month info
d_sum1$year <- as.factor(year(d_sum1$date))
d_sum1$month <- as.factor(month(d_sum1$date))

# create quarter matrix
q_m <- as.matrix(c(1:12))
q_c <- as.matrix(c("1Q", "1Q", "1Q", "2Q", "2Q", "2Q", "3Q", "3Q", "3Q", "4Q", "4Q", "4Q"))
quarter <- cbind(q_m, q_c)
colnames(quarter) <- c('month', 'quarter')
quarter <- as.data.frame(quarter)
quarter$month <- as.factor(quarter$month)

# add quarter info column 
d_sum1 <- merge(d_sum1, quarter, by="month")
d_sum1$YearQuarter <- paste(d_sum1$year, d_sum1$quarter, sep="")
d_sum1$quarter <- as.factor(d_sum1$quarter )
d_sum1$YearQuarter <- as.factor(d_sum1$YearQuarter )

# remove outliers
d_sum2 <- d_sum1

d_sum2 <- d_sum2 %>% filter(d_sum2$yrbuilt != 0)
bk_yrbuilt <- read_excel("yrbuilt.xlsx")
d_sum2 <- merge(d_sum2, bk_yrbuilt, by="yrbuilt")
d_sum2$yrbuilt_class <- as.factor(d_sum2$yrbuilt_class)

d_sum2 <- d_sum2 %>% filter(d_sum2$price != 0)
d_sum2 <- d_sum2 %>% filter(d_sum2$price < 1.5e+07 )
d_sum2 <- d_sum2 %>% filter(d_sum2$grosssqft < 20000)
d_sum2 <- d_sum2 %>% filter(d_sum2$bldage < 120) 

# plotting
plot(d_sum2$price, d_sum2$grosssqft)
plot(d_sum2$grosssqft, d_sum2$price)
hist(d_sum2$grosssqft)
plot(d_sum2$bldage, d_sum2$price)
plot(d_sum2$salesage, d_sum2$price)
plot(d_sum2$price, d_sum2$salesage)

# plotting: landsqft
hist(d_sum2$landsqft) # high right skewed
lm_landsqft <- lm(price~landsqft, d_sum2)
summary(lm_landsqft)$r.squared
summary(lm_landsqft)$residuals
predict(lm_landsqft)
plot(predict(lm_landsqft), summary(lm_landsqft)$residuals)

hist((d_sum1$landsqft)^(1/5)) # high right skewed
lm_landsqft1 <- lm(price~I((landsqft)^(1/5)), d_sum1)
summary(lm_landsqft1)$r.squared
summary(lm_landsqft1)$residuals
predict(lm_landsqft1)
plot(predict(lm_landsqft1), summary(lm_landsqft1)$residuals)

# plotting: grosssqft
hist(d_sum2$grosssqft) # high right skewed
lm_grosssqft <- lm(price~grosssqft, d_sum2)
summary(lm_grosssqft)$r.squared
summary(lm_grosssqft)$residuals
predict(lm_grosssqft)
plot(predict(lm_grosssqft), summary(lm_grosssqft)$residuals)

hist(log(d_sum2$grosssqft))
lm_grosssqft2 <- lm(price~log(grosssqft), d_sum2)
summary(lm_grosssqft2)$r.squared
summary(lm_grosssqft2)$residuals
predict(lm_grosssqft2)
plot(predict(lm_grosssqft2), summary(lm_grosssqft2)$residuals)

# plotting:yrbuilt
hist(d_sum1$yrbuilt) # medium left skewed
lm_yrbuilt <- lm(price~yrbuilt, d_sum2)
summary(lm_yrbuilt)$r.squared
summary(lm_yrbuilt)$residuals
predict(lm_yrbuilt)
plot(predict(lm_yrbuilt), summary(lm_yrbuilt)$residuals)

# plotting: bldage
hist(d_sum2$bldage) # medium right skewed
lm_bldage <- lm(price~bldage, d_sum2)
summary(lm_bldage)$r.squared
summary(lm_bldage)$residuals
predict(lm_bldage)
plot(predict(lm_bldage), summary(lm_bldage)$residuals)

hist(log(d_sum1$bldage))
lm_bldage1 <- lm(price~I(log(bldage+1)), d_sum1)
summary(lm_bldage1)$r.squared # increase in R.sqaured! Yay!
summary(lm_bldage1)$residuals
predict(lm_bldage1)
plot(predict(lm_bldage1), summary(lm_bldage1)$residuals)

# plotting: salesage
hist(d_sum2$salesage) # medium right skewed
lm_salesage <- lm(price~salesage, d_sum2)
summary(lm_salesage)$r.squared
summary(lm_salesage)$residuals
predict(lm_salesage)
plot(predict(lm_salesage), summary(lm_salesage)$residuals)

# simple linear regression (numeric)

lm_landsqft <- lm(price ~ landsqft , d_sum2)
summary(lm_landsqft)$r.squared # [1] 0.01987117
summary(lm(price~I((landsqft)^(1/5)), d_sum1))$r.squared # [1] 0.001592111

lm_grosssqft <- lm(price ~ grosssqft , d_sum2) 
summary(lm_grosssqft)$r.squared # [1] 0.09484793
summary(lm(price~log(grosssqft), d_sum1))$r.squared # [1] 0.07881149

lm_bldage <- lm(price ~ bldage , d_sum2)
summary(lm_bldage)$r.squared # [1] 0.001049262
summary(lm(price~I(log(bldage+1)), d_sum2))$r.squared  # [1] 0.01161107

lm_salesage <- lm(price ~ salesage , d_sum2)
summary(lm_salesage)$r.squared # [1] 0.005164644

lm_bldclasssale <- lm(price ~ bldclasssale, d_sum2)
lm_bi_bldclasssale <- lm(price ~ bi_bldclasssale, d_sum2)
summary(lm_bldclasssale)$r.squared # [1] 0.07958434
summary(lm_bi_bldclasssale)$r.squared # [1] 0.01473554

lm_taxclasssale <- lm(price ~ taxclasssale, d_sum2)
summary(lm_taxclasssale)$r.squared # [1] 0.01618013

lm_zip <- lm(price ~ zip, d_sum2)
summary(lm_zip)$r.squared # [1] 0.192754

lm_bldclasscurr <- lm(price ~ bldclasscurr, d_sum2)
summary(lm_bldclasscurr)$r.squared # [1] 0.08015735

lm_taxclasscurr <- lm(price ~ taxclasscurr, d_sum2)
summary(lm_taxclasscurr)$r.squared # [1] 0.01627658

# correlation 
cor(d_sum2[c("grosssqft", "landsqft", "salesage", "bldage")])
pairs(d_sum2[c("grosssqft", "landsqft", "salesage", "bldage")])
pairs.panels(d_sum2[c("grosssqft", "landsqft", "salesage", "bldage")])

# select from original variables 
str(d_sum2)
d_sum3 <- d_sum2[ , c("price","landsqft", "grosssqft", "yrbuilt", "zip", "taxclasssale", "year", "month", "quarter", "YearQuarter")]
# d_sum3<- d_sum2[ , c("bldage", "bi_bldclasssale", "salesage", "zip_class",  "yrbuilt_class")]

lmfull <- lm(price~landsqft + grosssqft + yrbuilt + zip + taxclasssale + year + month + quarter + YearQuarter, data = d_sum3)
summary(lmfull)$r.squared
summary(lmfull)$df
summary(lmfull)

# pre-modeling

lm_all <- lm(price ~ ., data=d_sum3)
start <- lm(price~1, data=d_sum3)
step(start, direction = "forward", scope=list(lower=start, upper = lm_all))
# price ~ zip + grosssqft + YearQuarter + landsqft + taxclasssale, data = d_sum3
step(lm_all, direction = "backward")
# price ~ landsqft + grosssqft + zip + taxclasssale + YearQuarter, data = d_sum3
step(start , direction = "both", scope=list(upper=lm_all))
# price ~ zip + grosssqft + YearQuarter + landsqft + taxclasssale, data = d_sum3 


lmall <- lm(price ~ zip + grosssqft + YearQuarter + landsqft + taxclasssale, data = d_sum3)
summary(lmall)$r.squared
summary(lmall)$df
summary(lmall)
predict_lmall <- predict(lmall, d_sum3)
rmse(d_sum3$price, predict_lmall)
vif(lmall)

d_sum4 <- d_sum2[ , c("price","landsqft", "grosssqft", "yrbuilt", "bldage", "salesage", "yrbuilt_class", "zip", "zip_class", "taxclasssale", "bldclasssale", "bi_bldclasssale", "year", "month", "quarter", "YearQuarter")]

# Linear Regression: Trial and Errors
lm1 <- lm(price ~ zip_class + landsqft + grosssqft + YearQuarter +  taxclasssale + bi_bldclasssale + bldage + salesage, data = d_sum4)
summary(lm1)$r.squared
summary(lm1)$df
summary(lm1)
predict_lm1 <- predict(lm1, d_sum4)
rmse(d_sum4$price, predict_lm1)

lm2 <- lm(price ~ zip_class:grosssqft
                  + sqrt(landsqft)*bi_bldclasssale
                  + yrbuilt_class + quarter
                  + salesage
                  , data = d_sum4)
summary(lm2)$r.squared
summary(lm2)$df
summary(lm2)

lm2r <- lm(price ~ zip_class:grosssqft
          + landsqft*bi_bldclasssale
          + yrbuilt_class + quarter
          + salesage
          , data = d_sum4)
predict_lm2r <- predict(lm2r, d_sum4)
rmse(d_sum4$price, predict_lm2r)
vif(lm2r)

lm3 <- lm(price ~ zip_class:grosssqft
          + sqrt(landsqft)*bi_bldclasssale
          + sqrt(landsqft)*grosssqft
          + yrbuilt_class + quarter
          + salesage
          , data = d_sum4)
summary(lm3)$r.squared
summary(lm3)$df
summary(lm3)

lm3r <- lm(price ~ zip_class:grosssqft
          + landsqft*bi_bldclasssale
          + landsqft*grosssqft
          + yrbuilt_class + quarter
          + salesage
          , data = d_sum4)
predict_lm3r <- predict(lm3r, d_sum4)
rmse(d_sum4$price, predict_lm3r)
vif(lm3r)

### 2.3 Reach a stopping point 
lm4 <- lm(price ~ grosssqft*zip_class
                  + grosssqft:salesage
                  + sqrt(landsqft)*bi_bldclasssale
          , data = d_sum4)

summary(lm4)$r.squared
summary(lm4)$df
summary(lm4)

lm4r <- lm(price ~ grosssqft*zip_class
          + grosssqft:salesage
          + landsqft*bi_bldclasssale
          , data = d_sum4)
predict_lm4r <- predict(lm4r, d_sum4)
rmse(d_sum4$price, predict_lm4r)
vif(lm4r)

### iid assumption
plot(lm4)

# normality assumption 
# The null hypothesis (Ho) is that the two dataset values are from the same continuous distribution.
ks.test(lm4$residuals, pnorm, sd=summary(lm4)$sigma)
ks.test(lm4$residuals, pnorm, sd=summary(lm4)$sigma)$p.value 
# p.value = 0, reject the null hypothesis

require(lmtest)
# Heteroskedasticity
# The null hypothesis for this test is that the error variances are all equal. 
bptest(lm4)
bptest(lm4)$p.value
# p.value = 0, reject the null hypothesis

# Serial correlation 
# the null hypothesis that linear regression residuals of time series data are uncorrelated, 
dwtest(lm4, alternative ="two.sided")
dwtest(lm4, alternative ="two.sided")$p.value
# p.value = 0, reject the null hypothesis

saveRDS(list(model=lm4, data=d_sum4), file = "miasong.RDS")

#### Final Assignment 2
## 1. create period data sets
# filter quarter 20
d_20 <- d_sum4 %>% filter(d_sum4$year == "2020")

# filter quarter 203Q/4Q
d_20q3q4 <- d_sum4 %>% filter(d_sum4$YearQuarter == "20203Q" | d_sum4$YearQuarter == "20204Q")

# filter quarter 203Q
d_20q3 <- d_sum4 %>% filter(d_sum4$YearQuarter == "20203Q")
min(d_20q3$grosssqft)
max(d_20q3$grosssqft)

# filter quarter 204Q
d_20q4 <- d_sum4 %>% filter(d_sum4$YearQuarter == "20204Q")

## 2. Methodology
# The RMSE is the square root of the variance of the residuals. It indicates the absolute fit of the model to the data–how close the observed data points are to the model's predicted values. Whereas R-squared is a relative measure of fit, RMSE is an absolute measure of fit.
# 1. residuals comparison: their signs and dispersion
# 2. new linear model(1): variable comparison 
# 3. new linear model(2): quarters as factors

# 3. linear model
lm_d20 <- lm(price ~ grosssqft*zip_class
             + grosssqft:bldage
             + landsqft*bi_bldclasssale
             , data = d_20)
summary(lm_d20)$r.squared
summary(lm_d20)$df
summary(lm_d20)
predict_lm_d20 <- predict(lm_d20, d_20)
rmse(d_20$price, predict_lm_d20)

lm_d20q3q4 <- lm(price ~ grosssqft*zip_class
                 + grosssqft:bldage
                 + landsqft*bi_bldclasssale
                 , data = d_20q3q4)
summary(lm_d20q3q4)$r.squared

lm_d20q3 <- lm(price ~ grosssqft*zip_class
               + grosssqft:bldage
               + landsqft*bi_bldclasssale
               , data = d_20q3)
summary(lm_d20q3)$r.squared

lm_d20q4 <- lm(price ~ grosssqft*zip_class
               + grosssqft:bldage
               + landsqft*bi_bldclasssale
               , data = d_20q4)
summary(lm_d20q4)$r.squared 

# 4. data set used
# d_20q3
# d_20q4

# 5. Fitted Values
predict_lm_d20_3 <- predict(lm_d20, d_20q3)
predict_lm_d20_4 <- predict(lm_d20, d_20q4)

# 6. actual values 
# d_20q3$price
# d_20q4$price

# 7.residuals 
boxplot(d_20q3$price - predict_lm_d20_3)$stats
boxplot(d_20q4$price - predict_lm_d20_4)$stats

# residuals
residuals <- as.matrix(d_20q3$price - predict_lm_d20_3)
residuals <- as.data.frame(residuals)
price_q3 <- cbind(d_20q3$price, predict_lm_d20_3, residuals)
colnames(price_q3) = c("price_actual", "price_predicted", "residuals")
price_q3_p <- price_q3 %>% filter(price_q3$residuals > 0)
price_q3_n <- price_q3 %>% filter(price_q3$residuals < 0)

residuals2 <- as.matrix(d_20q4$price - predict_lm_d20_4)
residuals2 <- as.data.frame(residuals2)
price_q4 <- cbind(d_20q4$price, predict_lm_d20_4, residuals2)
colnames(price_q4) = c("price_actual", "price_predicted", "residuals")
price_q4_p <- price_q4 %>% filter(price_q4$residuals > 0)
price_q4_n <- price_q4 %>% filter(price_q4$residuals < 0)

t.test(price_q3_p$price_actual, price_q3_p$price_predicted, var.equal=TRUE)
t.test(price_q3_n$price_actual, price_q3_n$price_predicted, var.equal=TRUE)

t.test(price_q4_p$price_actual, price_q4_p$price_predicted, var.equal=TRUE)
t.test(price_q4_n$price_actual, price_q4_n$price_predicted, var.equal=TRUE)

t.test(residuals, residuals2, var.equal=TRUE)

# 8. Draw Boxplot
# Light gray background
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
     col = "#ebebeb")

# Add white grid
grid(nx = NULL, ny = NULL, col = "white", lty = 1,
     lwd = par("lwd"), equilogs = TRUE)

# Boxplot
par(new = TRUE)
boxplot(d_20q3$price - predict_lm_d20_3, 
        d_20q4$price - predict_lm_d20_4, # Data
        names = c("Q3", "Q4"),
        horizontal = FALSE, # Horizontal or vertical plot
        lwd = 1, # Lines width
        col = "darkgrey", # Color
        xlab = "Quarter",  # X-axis label
        ylab = "Residual",  # Y-axis label
        main = "Residual Dispersion of Q3 and Q4", # Title
        border = "black",  # Boxplot border color
        outpch = 21,       # Outliers symbol
        outbg = "brown2",   # Outliers color
        whiskcol = "blue", # Whisker color
        whisklty = 3,      # Whisker line type
        lty = 1) # Line type (box and median)

abline(h=0, col="blue", lwd = 1.5)

# Add a legend
legend("topright", legend = "data(25%~75%)", # Position and title
       fill = "darkgrey",  # Color
       inset = c(0.02, 0.02), # Modify margins
       bg = "white") # Legend background color

# 9. Mean difference
# q3.
mean(as.matrix(price_q3_p$price_actual))
mean(as.matrix(price_q3_p$price_predicted))
nrow(price_q3_p)

mean(as.matrix(price_q3_n$price_actual))
mean(as.matrix(price_q3_n$price_predicted))
nrow(price_q3_n)

# q4.
mean(as.matrix(price_q4_p$price_actual))
mean(as.matrix(price_q4_p$price_predicted))
nrow(price_q4_p)

mean(as.matrix(price_q4_n$price_actual)) 
mean(as.matrix(price_q4_n$price_predicted))
nrow(price_q4_n)

162 / (159+162)

# 10. Coeefficient
lmb_sel_q3 <- lm(price ~ landsqft + grosssqft + yrbuilt + bldage
                 + salesage + yrbuilt_class + zip + zip_class +
                   + taxclasssale + bldclasssale + bi_bldclasssale 
                 ,data = d_20q3)
summary(lmb_sel_q3)
summary(lmb_sel_q3)$r.squared

lmb_sel_q4 <- lm(price ~ landsqft + grosssqft + yrbuilt + bldage
                 + salesage + yrbuilt_class + zip + zip_class +
                   + taxclasssale + bldclasssale + bi_bldclasssale 
                 ,data = d_20q4)
summary(lmb_sel_q4)
summary(lmb_sel_q4)$r.squared

# 11. quarters as factors
lm_d20q3q4_new1 <- lm(price ~ grosssqft*zip_class
                      + grosssqft:salesage
                      + sqrt(landsqft)*bi_bldclasssale
                      + quarter
                      , data = d_20q3q4)
summary(lm_d20q3q4_new1)

lm_d20q3q4_new2  <- lm(price ~ 
                         landsqft + grosssqft + yrbuilt 
                       + bldage + salesage + yrbuilt_class 
                       + zip + zip_class + taxclasssale 
                       + bldclasssale + bi_bldclasssale + quarter
                       ,data = d_20q3q4)
summary(lm_d20q3q4_new2)


