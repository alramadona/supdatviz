

# load dataset ------------------------------------------------------------

datTotal <- read.csv("dat/dengue.csv", na.string="#N/A")

names(datTotal)
str(datTotal)

# install packages
# install.packages(dplyr)
# install.packages(zoo)
# install.packages(ggplot2)

# library
library(dplyr)
library(zoo)
library(ggplot2)


# membuat dataset baru ----------------------------------------------------

dat <- select(datTotal, year,month,denguel0,templ0,rainl0)


# membuat variabel baru ---------------------------------------------------

dat$term <- rep(rep(c(1,2), each=6),13)
dat$YM <- paste(dat$month, dat$year, sep="-")
dat$YM <- as.yearmon(dat$YM, format="%m-%Y")
dat$date <- as.Date(dat$YM)
dat$MoY <- as.factor(dat$month)


# rename variables --------------------------------------------------------

names(dat)
names(dat) <- c("year", "month", "cases", "temp", "rain", "term", "YM", "date", "MoY")
str(dat)


# visualisasi data --------------------------------------------------------

ggplot(dat, aes(x = date, y = cases)) +
  geom_line()

ggplot(dat, aes(x = date, y = cases)) +
  geom_line() + theme_bw()

ggplot(dat, aes(x = date, y = temp)) +
  geom_line() + theme_bw()

ggplot(dat, aes(x = date, y = rain)) +
  geom_line() + theme_bw()

# install.packages("ggpubr")
library(ggpubr)

p1 <- ggplot(dat, aes(x = date, y = cases)) +
  geom_line() + theme_bw()

p2 <- ggplot(dat, aes(x = date, y = temp)) +
  geom_line() + theme_bw()

p3 <- ggplot(dat, aes(x = date, y = rain)) +
  geom_line() + theme_bw()

ggarrange(p1, p2, p3,
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3)

# help
?ggarrange


# tugas 1 - silakan dilengkapi! -------------------------------------------

q1 <- ggplot(dat, aes(x = MoY, y = cases)) +
  geom_boxplot() + theme_bw()
q1

q2 <- ggplot(dat, aes(x = ______, y = ______)) + # variabel suhu
  geom_boxplot() + theme_bw()
q2

q3 <- ggplot(dat, aes(x = ______, y = ______)) + # variabel curah hujan
  ______() + ______()
q3

ggarrange(______, ______, ______,
          labels = c("______", "______", "______"),
          ncol = ______, nrow = ______)


# additional --------------------------------------------------------------

ggplot(dat, aes(x = MoY, y = cases)) +
  geom_boxplot(color = "gray60", outlier.alpha = 0) +
  geom_point(size = 3, alpha = 0.25) +
  theme_minimal()

set.seed(2019)

ggplot(dat, aes(x = MoY, y = cases, col = MoY)) +
  theme_minimal() + 
  geom_jitter(size = 2, width = 0.2) + 
  theme(legend.position="none")

cases_avg <-
  dat %>%
  summarize(avg = mean(cases, na.rm = TRUE)) %>%
  pull(avg)
cases_avg

ggplot(dat, aes(x = MoY, y = cases, col = MoY)) +
  geom_hline(aes(yintercept = cases_avg), color = "gray70", size = 0.6) +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 5) +
  theme_bw() +
  theme(legend.position="none")

# library
# install.packages("hrbrthemes")
library(hrbrthemes)

# dataset with different distributions

ggplot(dat, aes(x=cases)) +
  geom_histogram(color="#e9ecef", alpha=0.6, position='identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

ggplot(dat, aes(x=temp)) +
  geom_histogram(color="#e9ecef", alpha=0.6, position='identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

ggplot(dat, aes(x=rain)) +
  geom_histogram(color="#e9ecef", alpha=0.6, position='identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

p <- ggplot(dat, aes(x=cases, fill=as.factor(term))) +
  geom_histogram(color="#e9ecef", alpha=0.6, position='identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")
p 

q <- ggplot(dat, aes(x=temp, fill=as.factor(term))) +
  geom_histogram(color="#e9ecef", alpha=0.6, position='identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")
q

r <- ggplot(dat, aes(x=rain, fill=as.factor(term))) +
  geom_histogram(color="#e9ecef", alpha=0.6, position='identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")
r 

ggarrange(p, q, r,
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)


# tugas 2 - silakan dilengkapi! (suhu atau curah hujan) -------------------

_____avg <-
  dat %>%
  summarize(avg = mean(____, na.rm = TRUE)) %>%
  pull(avg)
_____avg

ggplot(dat, aes(x = ____, y = ____, col = MoY)) +
  geom_hline(aes(yintercept = _____avg), color = "gray70", size = 0.6) +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 5) +
  theme_minimal() +
  theme(legend.position="none")
