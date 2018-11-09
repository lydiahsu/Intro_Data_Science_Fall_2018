#########################
# SINUSOIDAL REGRESSION #
#########################

#Name: 
#Date: 
#Summary: 

install.packages("splines")
library("splines")
install.packages("tidyverse")
library("tidyverse")
install.packages("ggmap")
library("ggmap")
install.packages("rgdal")
library("rgdal")
install.packages("geosphere")
library("geosphere")
#library(ggplot2)

#while installing packages guess where crime is the highest?
## http://gis.ucla.edu/apps/click2shp/


#New York City Incident Level Data
url <- "https://github.com/jauerbach/LMIS/blob/master/NYPD_Complaint_Data_Historic.csv?raw=true"
crime <- read.csv(url)


crime$time_stamp <- as.POSIXct(x = paste(crime$CMPLNT_FR_DT, 
                                         crime$CMPLNT_FR_TM),
                               tz= "EST", 
                               format =  "%m/%d/%y %H:%M:%S")
crime$Hour <- as.numeric(format(crime$time_stamp, "%H"))
crime$Minute <- as.numeric(format(crime$time_stamp, "%M"))

## rounding errors, unit of analysis should be hour
hist(x = crime$Hour)
hist(x = crime$Hour, xlab = "Hour of Day", ylab = "Number of Crimes")
hist(x = crime$Hour, xlab = "Hour of Day", ylab = "Number of Crimes", breaks = 10)

qplot(x = crime$Hour, xlab = "Hour of Day", ylab = "Number of Crimes", bins = 10)
qplot(x = crime$Hour, bins = 10) + labs(x = "Hour of Day")
qplot(x = crime$Hour, bins = 10) + labs(x = "Hour of Day") + theme_bw()
qplot(x = crime$Hour, bins = 10) + labs(x = "Hour of Day") + theme_classic()
qplot(x = crime$Minute, bins = 10) + labs(x = "Minute of Day") + theme_bw()

theme_set(theme_bw())

ggplot(data = crime) +
  aes(Minute) +
  geom_histogram(bins = 10) +
  labs(x = "Minute of Day")

crime$weekdays <- factor(weekdays(crime$time_stamp),
                         levels = c("Monday", "Tuesday", "Wednesday", 
                                    "Thursday", "Friday", "Saturday", "Sunday"))
crime$week_num <- as.numeric(crime$weekdays)
# the number of the day in the week

#TIME
library(dplyr)
ssp <- spectrum(dplyr::count(crime,
                      as.numeric(as.factor(format(time_stamp, "%Y-%m-%d %H"))))$n,
                plot = FALSE)
period <- 1/ssp$freq[ssp$spec == max(ssp$spec)]

crime$time <- crime$Hour + period * (crime$week_num - 1)
daily_crime <- dplyr::count(crime, time)

plot(x = daily_crime$time, y = daily_crime$n)
qplot(x = daily_crime$time, y = daily_crime$n)
qplot(x = time, y = n, data = daily_crime)

ggplot(data = daily_crime) +
  aes(time, n) +
  geom_point() 

ggplot(data = daily_crime) +
  aes(time, n) +
  geom_point(alpha = .1) +
  geom_smooth(formula = y ~ sin(2 * pi * x / period) + 
                cos(2 * pi * x / period), 
              color = "blue",
              method = "lm", 
              linetype = 2, 
              alpha = .5) +
  scale_x_continuous(breaks = seq(0, period * 7, period)) +
  labs(x = "hours", y = "number of reports") 

model1 <- lm(n ~ sin(2 * pi * time / period) + cos(2 * pi * time / period),
             dplyr::count(crime, time))

coef_data <- data.frame(
  coef = rownames(coef(summary(model1))),
  up_small =   coef(summary(model1))[, 1] + sqrt(2) * coef(summary(model1))[, 2],
  up_large =  coef(summary(model1))[, 1] + 2 * coef(summary(model1))[, 2],
  lo_small =   coef(summary(model1))[, 1] - sqrt(2) * coef(summary(model1))[, 2],
  lo_large =  coef(summary(model1))[, 1] - 2 * coef(summary(model1))[, 2]
)

ggplot(data = coef_data) +
  aes(x = coef) +
  geom_linerange(aes(ymin = lo_small, ymax = up_small), size = 2) +
  geom_linerange(aes(ymin = lo_large, ymax = up_large), size = 1) +
  coord_flip() +
  labs(x = "")

# ggplot(data = coef_data) +
#   aes(x = coef) +
#   geom_linerange(aes(ymin = lo_small, ymax = up_small), size = 2) +
#   geom_linerange(aes(ymin = lo_large, ymax = up_large), size = 1) +
#   coord_flip() +
#   scale_x_discrete("", 
#                    labels = c("Intercept",
#                               expression( paste("sin", bgroup("(", 2 ~ pi ~ frac(time, period),")"))), 
#                               expression( paste("cos", bgroup("(", 2 ~ pi ~ frac(time, period),")")))))

phase <- unname(atan(model1$coefficients[3]/model1$coefficients[2]))
shift <- unname(model1$coefficients[1])
ampl <- unname(sqrt(model1$coefficients[2] ^ 2 + model1$coefficients[3] ^ 2))

phase * period / (2 * pi) #offset of sine curve (in days)
shift                     #average y value (vertical shift above 0)
ampl                      #increase above average y value (factor time 1)
period                    #fixed to be hours

expnd  <- function(n) paste("y ~ ",
                            paste0("sin(",2^(1:n)," * pi * x / period)", 
                                   collapse = " + "),
                            "+",
                            paste0("cos(",2^(1:n)," * pi * x / period)", 
                                   collapse = " + "))

expnd(1)
expnd(2)
expnd(3)

ggplot(dplyr::count(crime, time)) +
  aes(time, n) +
  geom_point(alpha = .1) +
  geom_smooth(formula = expnd(1), color = "blue",
              method = "lm", linetype = 2, alpha = .5, se = FALSE) +
  geom_smooth(formula = expnd(2), color = "orange",
              method = "lm", linetype = 2, alpha = .5, se = FALSE) +
  geom_smooth(formula = expnd(3), color = "red",
              method = "lm", linetype = 2, alpha = .5, se = FALSE) +
  scale_x_continuous(breaks = seq(0, period * 7, period)) +
  labs(x = "hours", y = "number of reports")

ggplot(dplyr::count(crime, time)) +
  aes(time, n) +
  geom_point(alpha = .1) +
  geom_smooth(color = "black", linetype = 2, alpha = .5, se = FALSE, span = .2) +
  geom_smooth(formula = y ~ ns(x, 20), method = "lm",
              color = "grey", alpha = .5, se = FALSE, span = .2) +
  geom_smooth(formula = expnd(4), color = "red",
              method = "lm", linetype = 2, alpha = .5, se = FALSE) +
  scale_x_continuous(breaks = seq(0, period * 7, period)) +
  labs(x = "hours", y = "number of reports")

# investigate the breakdown of the types of crimes

crime$quarter <- factor(format(as.Date(as.character(cut(crime$time_stamp, 
                                                        "quarter")),
                                       "%Y-%m-%d"), "%m"),
                        labels = c("1", "2", "3", "4"))
top_crime <- crime[crime$OFNS_DESC %in% names(tail(sort(table(crime$OFNS_DESC)), 
                                                   n = 4)), ]

ggplot(dplyr::count(top_crime, time, OFNS_DESC)) +
  aes(time, n) +
  geom_point(alpha = .1) +
  theme_bw() +
  geom_smooth(formula = expnd(4), color = "red",
              method = "lm", linetype = 2, alpha = .5) +
  scale_x_continuous(breaks = seq(0, period * 7, period)) +
  labs(x = "hours", y = "number of reports") +
  facet_wrap(~ OFNS_DESC, scales = "free")

# then stratify over period of the year, by quarters

ggplot(dplyr::count(na.omit(top_crime), 
             time, 
             quarter,
             OFNS_DESC)) +
  aes(time, n) +
  geom_point(alpha = .1) +
  theme_bw() +
  geom_smooth(formula = expnd(4), color = "red",
              method = "lm", linetype = 2, alpha = .5) +
  scale_x_continuous(breaks = seq(0, period * 7, period)) +
  labs(x = "hours", y = "number of reports") +
  facet_grid(quarter ~ OFNS_DESC, scales = "free", drop = TRUE)

ggplot(dplyr::count(top_crime, time, OFNS_DESC)) +
  aes(time, n) +
  geom_point(alpha = .1) +
  theme_bw() +
  geom_smooth(formula = expnd(4), aes(color = OFNS_DESC),
              method = "lm", alpha = .5, se = FALSE) +
  scale_x_continuous(breaks = seq(0, period * 7, period)) +
  labs(x = "hours", y = "number of reports") +
  theme(legend.position = "bottom")

#space
plot(x = crime$Longitude, y = crime$Latitude)

qplot(x = crime$Longitude, y = crime$Latitude)

ggplot(data = crime) + 
  aes(x = Longitude, y = Latitude) +
  geom_point()

ggplot(crime) + 
  geom_point(aes(Longitude, Latitude))

ggplot(crime) + 
  geom_density2d(aes(Longitude, Latitude))

ggplot(crime) + 
  stat_density_2d(aes(Longitude, Latitude, fill = ..density..), 
                  geom = "raster", contour = FALSE)

ggplot(crime) + 
  stat_density_2d(aes(Longitude, Latitude, fill = ..level..), 
                  geom = "polygon")

ggplot(crime) + 
  stat_density_2d(aes(Longitude, Latitude, fill = ..level..), 
                  geom = "polygon") +
  scale_fill_continuous(low = "red", high = "yellow")

ggplot(crime) + 
  stat_density_2d(aes(Longitude, Latitude, fill = ..level..), 
                  geom = "polygon") +
  scale_fill_continuous(low = "red", high = "yellow") +
  theme_nothing(legend = TRUE) +
  theme(legend.position = "bottom")

library(ggmap)


if(!requireNamespace("devtools")) install.packages("devtools")
# need to install digest scales and rjson here too
devtools::install_github("dkahle/ggmap", ref = "tidyup")

library(ggmap)

#register_google(key = "")
# you now need a google cloud account (with a credit card) to use the api. Sorry!


ggplot() + 
  stat_density_2d(aes(Longitude, Latitude, fill = ..level..), 
                  geom = "polygon",
                  data = crime) +
  scale_fill_continuous(low = "red", high = "yellow") +
  theme_nothing(legend = TRUE) +
  theme(legend.position = "bottom")

center <- colMeans(crime[,c("Longitude", "Latitude")], na.rm = TRUE)

ggmap(get_map(location = center)) +
  stat_density_2d(aes(Longitude, Latitude, fill = ..level..), 
                  geom = "polygon",
                  data = crime) +
  scale_fill_continuous(low = "red", high = "yellow") +
  theme_nothing(legend = TRUE) +
  theme(legend.position = "bottom")


ggmap(get_map(location = center, maptype = "toner-background")) +
  stat_density_2d(aes(Longitude, Latitude, fill = ..level..), 
                  geom = "polygon",
                  alpha = 0.25,
                  data = crime) +
  scale_fill_continuous(low = "red", high = "yellow") +
  theme_nothing(legend = TRUE) +
  theme(legend.position = "bottom")

ggmap(get_map(location = center, maptype = "toner-background",
              zoom = 11)) +
  stat_density_2d(aes(Longitude, Latitude, fill = ..level..), 
                  geom = "polygon",
                  data = crime[!is.na(crime$quarter), ]) +
  scale_fill_continuous(low = "red", high = "yellow") +
  theme_nothing(legend = TRUE) +
  theme(legend.position = "bottom") +
  facet_wrap(~ quarter)

ggmap(get_map(location = center)) +
  stat_density_2d(aes(Longitude, Latitude),
                  data = top_crime[!is.na(top_crime$quarter),], 
                  color = "red") +
  theme_nothing(legend = TRUE) +
  theme(legend.position = "bottom") +
  facet_grid(OFNS_DESC ~ quarter)

#polygons
library(rgdal)
# this will be wherever you have stuff saved
shape <- readOGR("C:/Users/owenw/Google Drive/Data Science Course_SHP/Crime/NYC Crime/EastHarlem/EastHarlem_poly.shp")
plot(x = shape)

shape_df <- fortify(shape)
ggplot(data = shape) +
  geom_polygon(aes(x = long, y = lat, group = group))

#lon/lat area (in km^2)
slot(shape@polygons[[1]], "area")

library("geosphere")

area <- areaPolygon(shape)
center <- centroid(shape)

map_shape <- ggmap(get_map(location = center, zoom = 15))

map_shape +
  geom_polygon(aes(x = long, y = lat, group = group), 
               alpha = .5, fill = "red", data = shape)

crime_pts <- cbind(crime$Longitude, crime$Latitude)
crime_pts <- na.omit(crime_pts)
crime_pts <- SpatialPoints(coords = crime_pts,
                           proj4string = shape@proj4string)

crime_in_shape <- crime_pts %over% shape
table(crime_in_shape)/area
3280 * table(crime_pts %over% shape)/area
# this is crimes per square foot I think?
