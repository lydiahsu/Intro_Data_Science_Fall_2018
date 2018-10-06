
###############
# Skyscrapers #
###############

#Author:  
#Date: 
#Summary: 
## 1. learn about histograms, densities
## 2. implement newton-raphson
## 2. practice ggplot

library("ggplot2")
library("ggmap")

#Primary Land Use Tax Lot Output
#http://www1.nyc.gov/site/planning/data-maps/open-data/dwn-pluto-mappluto.page
pluto <- read.csv("https://github.com/lydiahsu/Intro-Data-Science-Spring-2017/blob/gh-pages/example/MN.csv?raw=true")
pluto <- pluto[pluto$YearBuilt >= 1900 & pluto$YearBuilt <= 2017,]
pluto$NumFloors <- round(pluto$NumFloors)

#How are the floors distributed
ggplot(data = pluto) +
  theme_bw() +
  aes(x = NumFloors) +
  geom_histogram()

#ggplot generates a quantity called count, and uses it as the y variable. We
## can access this variable by using ..count..
ggplot(data = pluto) +
  theme_bw() +
  aes(x = NumFloors, y = ..count..) +
  geom_histogram()

#bins = 30 means increments of 104/30 = 3.46 floors, starting with zero 
ggplot(data = pluto) +
  theme_bw() +
  aes(x = NumFloors, y = ..count..) +
  geom_histogram(binwidth = 104/29, center = 0)

#we can normalize so the histogram sums to one gives probability
ggplot(data = pluto) +
  theme_bw() +
  aes(x = NumFloors, y = ..count.. / sum(..count..)) +
  geom_histogram()
#we interpret the bars as the probability a building, randomly chosen from the
##city, would have a total number of floors in that interval

#the bars measure the probability in units of 104/29 = 3.6 floors. If we 
##normalize the histogram, we generally report a slight different number: we 
##divide by the binwidth so that the probability is in units of 1 floor.
ggplot(data = pluto) +
  theme_bw() +
  aes(x = NumFloors, y = ..count.. / sum(..count..) / (104/29) ) +
  geom_histogram()

#this can be done simply by using ..density..
ggplot(pluto) +
  theme_bw() +
  aes(NumFloors, y = ..density..) +
  geom_histogram()
#we call this the empirical density

#we often draw empirical densities with curves, not bars. This can be done in ggplot with freqpoly
ggplot(pluto) +
  theme_bw() +
  aes(NumFloors, y = ..density..) +
  geom_histogram(alpha = .5) +
  geom_freqpoly(alpha = .5)

#either way, the problem with histograms/empirical densities is that they are sensitive to bin center
ggplot(data = pluto) +
  theme_bw() +
  aes(x = NumFloors, y = ..density..) +
  geom_histogram(binwidth = 104/29, center = 0, fill = "black", alpha = .5) +
  geom_histogram(binwidth = 104/29, center = .5, fill = "red", alpha = .5) +
  geom_histogram(binwidth = 104/29, center = -.5, fill = "orange", alpha = .5) +
  geom_histogram(binwidth = 104/29, center = 1, fill = "blue", alpha = .5)

ggplot(data = pluto) +
  theme_bw() +
  aes(x = NumFloors, y = ..density..) +
  geom_freqpoly(binwidth = 104/29, center = 0, color = "black") +
  geom_freqpoly(binwidth = 104/29, center = .5, color = "red") +
  geom_freqpoly(binwidth = 104/29, center = -.5, color = "orange") +
  geom_freqpoly(binwidth = 104/29, center = 1, color = "blue")

#one solution is to smooth the density using nearby points. This is what 
##geom_density does. We call this the nonparametric density.
ggplot(pluto) +
  theme_bw() +
  aes(NumFloors) +
  geom_density(alpha = .5, bw = 2) +
  geom_freqpoly(aes(y = ..density..), binwidth = 104/29, center = 0, color = "black", alpha = .5) +
  geom_freqpoly(aes(y = ..density..), binwidth = 104/29, center = .5, color = "red", alpha = .5) +
  geom_freqpoly(aes(y = ..density..), binwidth = 104/29, center = -.5, color = "orange", alpha = .5) +
  geom_freqpoly(aes(y = ..density..), binwidth = 104/29, center = 1, color = "blue", alpha = .5)

#now the problem becomes how many nearby points to use. This is called the bandwidth.
#Theory gives some advice on how to pick the bandwidth.
ggplot(pluto) +
  theme_bw() +
  aes(NumFloors) +
  geom_histogram(aes(y = ..density..), alpha = .5, binwidth = 1) +
  geom_density(bw = 1, color = "black") +
  geom_density(bw = 2, color = "red") +
  geom_density(bw = 5, color = "blue") +
  geom_density(bw = 10, color = "orange")

#another approach is to use a function to approximate the distribution with a known
##function. We call this the parametric densities.

#To fit these densities, we will use Newton-Raphson and compare the results with
##built-in R functions

#Try 1: Exponential
summary(glm(NumFloors + 1 ~ 1, family = Gamma, data = pluto),
        dispersion = 1)

f <- function(lambda, x) n * log(lambda) - lambda * x

f_prime <- function(lambda, x) {
  38033 / lambda - x
}

f_prime2 <- function(lambda, x) {
  - 38033 / (lambda^2)
}

param <- .0001
while(abs(f_prime(param, sum(pluto$NumFloors + 1))) > 1e-5) {
 param <- param - 
   f_prime(param, sum(pluto$NumFloors + 1)) /
    f_prime2(param, sum(pluto$NumFloors + 1))
}

#n.b.
##rate = mean

ggplot(pluto) +
  theme_bw() +
  aes(NumFloors + 1) +
  geom_density(bw = 2) +
  stat_function(fun = function(x) dexp(x, rate = 0.1355543), 
                xlim = c(0,100),
                color = "dark green")

#Try 2: Gamma
summary(glm(NumFloors + 1 ~ 1, family = Gamma, data = pluto))

f <- function(alpha, lambda, x, lx) { 
  38033 * alpha * log(lambda) - 38033 * log(gamma(alpha)) + 
    (alpha - 1) * lx - lambda * x
}

}

f_prime(param[1], param[2], 
        x = sum(pluto$NumFloors + 1),
        lx = sum(log(pluto$NumFloors + 1)))


f_prime2 <- function(alpha, lambda) {
  matrix(38033 *
    c(- trigamma(alpha),
      1 / lambda,
      1 / lambda,
      - alpha / lambda^2), 
    ncol = 2)
}

param <- c(1.5, .1916)
while(abs(max(f_prime(param[1], param[2], 
              x = sum(pluto$NumFloors + 1),
              lx = sum(log(pluto$NumFloors + 1))))) > 1e-5) {
  param <- param - 
    solve(f_prime2(param[1], param[2])) %*%
    f_prime(param[1], param[2], 
            x = sum(pluto$NumFloors + 1),
            lx = sum(log(pluto$NumFloors + 1)))
}


#n.b.
##shape = 1/disp 
##rate = mean/disp

ggplot(pluto) +
  theme_bw() +
  aes(NumFloors + 1) +
  geom_density(bw = 2) +
  stat_function(fun = function(x) dgamma(x, 
                                         shape = 1/0.7073093,
                                         rate = 0.1355543/0.7073093), 
                xlim = c(0,100),
                color = "red") +
  stat_function(fun = function(x) dgamma(x, 
                                         shape = param[1],
                                         rate = param[2]), 
                xlim = c(0,100),
                color = "blue")

#Try 3: Weibull
survival::survreg(survival::Surv(NumFloors + 1) ~ 1, dist="weibull", data = pluto)
#R shape = 1/(survival scale)
#R scale = exp(survival intercept)

ggplot(pluto) +
  theme_bw() +
  aes(NumFloors + 1) +
  geom_density(bw = 2) +
  stat_function(fun = function(x) dweibull(x, 
                                           shape = 1/.6795393,
                                           scale = exp(2.112662)), 
                xlim = c(0,100),
                color = "blue")

#Try 4: Log-Normal
summary(glm(log(NumFloors + 1) ~ 1, family = gaussian, data = pluto))

ggplot(pluto) +
  theme_bw() +
  aes(NumFloors + 1) +
  geom_density(bw = 2) +
  stat_function(fun = function(x) dlnorm(x, 
                                        meanlog = 1.8258,
                                        sdlog = sqrt(0.2731726)), 
                xlim = c(0,100),
                color = "orange")

#Often multivariate densities are of interest
#e.g. where are skyscrapers located?
ggplot(pluto) +
  theme_void() +
  aes(XCoord, YCoord) +
  stat_summary_2d(aes(z = NumFloors), fun = mean,
                  binwidth = c(200, 200)) +
  scale_fill_continuous(low = "white", high = "red") +
  theme(legend.position = "left") +
  labs(fill = "Number of Floors") +
  coord_equal()

ggplot(pluto) +
  theme_void() +
  aes(XCoord, YCoord) +
  stat_summary_2d(aes(z = NumFloors), fun = max,
                  binwidth = c(200, 200)) +
  scale_fill_continuous(low = "white", high = "red") +
  theme(legend.position = "left") +
  labs(fill = "Maximum Number of Floors") +
  coord_equal()

ggplot(pluto) +
  theme_void() +
  aes(XCoord, YCoord) +
  stat_summary_hex(aes(z = NumFloors), fun = max,
                   binwidth = c(200, 200)) +
  scale_fill_continuous(low = "white", high = "red") +
  theme(legend.position = "left") +
  labs(fill = "Maximum Number of Floors") +
  coord_equal()

ggplot(pluto) +
  theme_void() +
  aes(XCoord, YCoord) +
  stat_bin2d(aes(fill = ..density.., weight = NumFloors ),
             binwidth = c(200, 200)) +
  scale_fill_continuous(low = "white", high = "red") +
  theme(legend.position = "left") +
  labs(fill = "Number of Floors") +
  coord_equal()

#Often multivariate densities are of interest
#e.g. where are the most expensive buildings?
ggplot(pluto) +
  theme_void() +
  aes(XCoord, YCoord) +
  stat_summary_2d(aes(z = log(AssessTot, base = 10)), fun = mean,
                  binwidth = c(200, 200)) +
  scale_fill_continuous(low = "white", high = "dark green") +
  theme(legend.position = "left") +
  labs(fill = "Total Assessed Value") +
  coord_equal()

ggplot(pluto) +
  theme_void() +
  aes(XCoord, YCoord) +
  stat_summary_2d(aes(z = log(AssessLand, base = 10)), fun = mean,
                  binwidth = c(200, 200)) +
  scale_fill_continuous(low = "white", high = "dark green") +
  theme(legend.position = "left") +
  labs(fill = "Assessed Land Value") +
  coord_equal()

ggplot(pluto) +
  theme_void() +
  aes(XCoord, YCoord) +
  stat_summary_2d(aes(z = log(ExemptLand, base = 10)), fun = mean,
                  binwidth = c(200, 200)) +
  scale_fill_continuous(low = "white", high = "dark green") +
  theme(legend.position = "left") +
  labs(fill = "Exempt Land Value") +
  coord_equal()
