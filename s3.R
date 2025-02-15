
# load dataset ------------------------------------------------------------

## sumber: https://doi.org/10.1371/journal.pone.0152688
datTotal <- read.csv("dat/dengue.csv", na.string="#N/A")

names(datTotal)
str(datTotal)

# library
library(dplyr)
library(zoo)
library(ggplot2)
library(ggpubr)


# membuat dataset baru ----------------------------------------------------

dat <- select(datTotal, year,month,
              denguel0,templ0,rainl0,
              denguel1,templ1,rainl1,
              denguel2,templ2,rainl2,
              denguel3,templ3,rainl3)


# membuat variabel baru ---------------------------------------------------

dat$term <- rep(rep(c(1,2), each=6),13)
dat$YM <- paste(dat$month, dat$year, sep="-")
dat$YM <- as.yearmon(dat$YM, format="%m-%Y")
dat$date <- as.Date(dat$YM)
dat$MoY <- as.factor(dat$month)


# rename variables --------------------------------------------------------

names(dat)
names(dat) <- c("year", "month", 
                "cases", "temp", "rain",
                "cases_l1", "temp_l1", "rain_l1",
                "cases_l2", "temp_l2", "rain_l2",
                "cases_l3", "temp_l3", "rain_l3",
                "term", "YM", "date", "MoY")
str(dat)
dat$n <- c(1:nrow(dat))

# visualisasi data --------------------------------------------------------

q1 <- ggplot(dat, aes(x = MoY, y = cases)) +
  geom_boxplot() + theme_bw()
q1

q2 <- ggplot(dat, aes(x = MoY, y = temp)) +
  geom_boxplot() + theme_bw()
q2

q3 <- ggplot(dat, aes(x = MoY, y = rain)) +
  geom_boxplot() + theme_bw()
q3

ggarrange(q1, q2, q3,
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3)


# dataset [train & test] --------------------------------------------------

train <- subset(dat, YM<"Jan 2011")
test <- subset(dat, YM>="Jan 2011")


# modeling - linear regression --------------------------------------------

model.temp <- glm(cases~temp, data=train, family="gaussian", na.action=na.exclude)
summary(model.temp)

##

model.rain <- glm(cases~rain, data=train, family="gaussian", na.action=na.exclude)
summary(model.rain)

### calculate McFadden's R-squared for model
with(summary(model.rain), 1 - deviance/null.deviance)

##

model.rain_l1 <- glm(cases~rain_l1, data=train, family="gaussian", na.action=na.exclude)
summary(model.rain_l1)

### calculate McFadden's R-squared for model
with(summary(model.rain_l1), 1 - deviance/null.deviance)

##

model.rain_l2 <- glm(cases~rain_l2, data=train, family="gaussian", na.action=na.exclude)
summary(model.rain_l2)

### calculate McFadden's R-squared for model
with(summary(model.rain_l2), 1 - deviance/null.deviance)

##

model.rain_l3 <- glm(cases~rain_l3, data=train, family="gaussian", na.action=na.exclude)
summary(model.rain_l3)

### calculate McFadden's R-squared for model
with(summary(model.rain_l3), 1 - deviance/null.deviance)

### visualize
par(mfrow=c(2,2))

# model 1
plot(train$n, train$cases, type="l", ylab="Number Cases", axes=F, xlab="Year")
points(predict(model.rain, type="response"),type="l", col="red")
axis(1, at=c(6,18,30,42,54,66,78,90,102,114),labels=c(2001:2010))
axis(2, at=c(0,50,100,150,200,250))
title(main="Model A")

# model 2
plot(train$n, train$cases, type="l", ylab="Number Cases", axes=F, xlab="Year")
points(predict(model.rain_l1, type="response"),type="l", col="red")
axis(1, at=c(6,18,30,42,54,66,78,90,102,114),labels=c(2001:2010))
axis(2, at=c(0,50,100,150,200,250))
title(main="Model B")

# model 3
plot(train$n, train$cases, type="l", ylab="Number Cases", axes=F, xlab="Year")
points(predict(model.rain_l2, type="response"),type="l", col="red")
axis(1, at=c(6,18,30,42,54,66,78,90,102,114),labels=c(2001:2010))
axis(2, at=c(0,50,100,150,200,250))
title(main="Model C")

# model 4
plot(train$n, train$cases, type="l", ylab="Number Cases", axes=F, xlab="Year")
points(predict(model.rain_l3, type="response"),type="l", col="red")
axis(1, at=c(6,18,30,42,54,66,78,90,102,114),labels=c(2001:2010))
axis(2, at=c(0,50,100,150,200,250))
title(main="Model D")


# modeling - linear regression (validation) -------------------------------

dat$predict <- predict(model.rain_l2, type="response", newdata=dat)

train <- subset(dat, YM<"Jan 2011")
test <- subset(dat, YM>="Jan 2011")

par(mfrow=c(1,1))
plot(dat$n, dat$cases, type="l", ylab="Number Cases", axes=F, xlab="Year")
points(train$n, train$predict, type="l", col="red")
points(test$n, test$predict, type="l", col="blue")
axis(1, at=c(6,18,30,42,54,66,78,90,102,114,126,138,150),labels=c(2001:2013))
axis(2, at=c(0,50,100,150,200,250))
# title(main="")
