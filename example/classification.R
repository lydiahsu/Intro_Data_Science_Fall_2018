##################
# Classification #
##################

#Name: 
#Date: 
#Summary: 1. Plot maps with ggmap 2. Classify with glm, nnet and neuralnet

###read packages
install.packages("rgeos")
install.packages("rgdal")
install.packages("maptools") 
install.packages("ggplot2")
install.packages("plyr")
install.packages("reshape2")
install.packages("ggmap")
install.packages("neuralnet")
library("nnet")
library("neuralnet")
library("rgeos")
library("rgdal")
library("maptools")
library("ggplot2")
library("plyr")
library("reshape2")
library("ggmap")

#part1: map shapefiles with ggmap

#street shapefile from NYS GIS Program Office 
## @ http://gis.ny.gov/gisdata/inventories/details.cfm?DSID=932
## meta http://gis.ny.gov/gisdata/supportfiles/Streets-Data-Dictionary.pdf
# setwd("/Users/jauerbach/Dropbox/SHP/")
# dir.create(file.path(getwd(), "shapefiles"))
# temp <- tempfile()
# download.file("http://gis.ny.gov/gisdata/fileserver/?DSID=932&file=streets_shp.zip", 
#               temp)
# unzip(temp,exdir="shapefiles")
# streets <- readOGR("shapefiles/Streets_shp/","StreetSegment")
# class(streets)
# streets@proj4string
# streets@bbox
# 
# #Project from NAD83 to WGS84
# WGS84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# streets <- spTransform(streets, WGS84)
# streets@proj4string
# streets@bbox
# 
# #subset nyc
# streets_centers <- getSpatialLinesMidPoints(streets)
# in_nyc <- streets_centers@coords[,1] > -79.76212 &
#           streets_centers@coords[,1] < -71.85732 &
#           streets_centers@coords[,2] > 40.49653 &
#           streets_centers@coords[,2] < 45.01037
# nyc_streets <- streets[in_nyc,]
# nyc_streets_df <- fortify(nyc_streets)
# 
#

#The following .RData file is the result after running the previous lines of code.
load(url("https://github.com/jauerbach/LMIS/blob/master/classification.RData?raw=true"))

#basic maps with ggmap
ggmap(get_map("Columbia University New York", zoom = 14, maptype = "satellite"))
ggmap(get_map("Columbia University New York", zoom = 14, maptype = "satellite")) + 
  theme_nothing()
ggmap(get_map("Columbia University New York", zoom = 18, maptype = "hybrid")) +
  theme_nothing()
ggmap(get_map("Columbia University New York", zoom = 18, maptype="watercolor", source="stamen")) +
  theme_nothing()
ggmap(get_map("Columbia University New York", zoom = 18, maptype="terrain-lines")) +
  theme_nothing()

#five locations in New York City
nyc <- get_map("Empire State Building", zoom = 10, maptype ="terrain-lines")
man <- get_map("Empire State Building", zoom = 14, maptype ="terrain-lines")
bkn <- get_map("Atlantic Terminal Brooklyn", zoom = 14, maptype ="terrain-lines")
sid <- get_map("Staten Island", zoom = 14, maptype ="terrain-lines")
qns <- get_map("Jamaica Queens", zoom = 14, maptype ="terrain-lines")

ggmap(nyc)
ggmap(nyc) + theme_nothing()
ggmap(nyc) + theme_nothing() +
  geom_path(aes(long,lat, group = group), color = "blue",
            data = nyc_streets_df)

ggmap(man) + theme_nothing() +
  geom_path(aes(long,lat, group = group), color = "blue",
            data = nyc_streets_df)

ggmap(man) + theme_nothing() +
  geom_path(aes(long,lat, group = group, color = id),
            data = nyc_streets_df)

ggmap(man) + theme_nothing() +
  geom_path(aes(long,lat, group = group, color = factor(cos(as.numeric(id)))),
            data = nyc_streets_df)

#plot Some of the Street Data
nyc_streets@data$id <- rownames(nyc_streets@data)
nyc_streets_df <- join(nyc_streets_df, nyc_streets@data, by = "id")

ggmap(nyc) + theme_nothing() +
  geom_path(aes(long,lat, group = group, color = factor(LeftCounty)),
            data = nyc_streets_df)

ggmap(man) + theme_nothing(legend=TRUE) +
  geom_path(aes(long,lat, group = group, color = factor(SPEED)),
            data = nyc_streets_df)

ggmap(man) + theme_nothing(legend=TRUE) +
  geom_path(aes(long,lat, group = group, color = factor(ACC)),
            data = nyc_streets_df)

ggmap(man) + theme_nothing(legend=TRUE) +
  geom_path(aes(long,lat, group = group, color = FromToCost),
            data = nyc_streets_df) +
  theme(legend.position = "bottom")

ggmap(man) + theme_nothing(legend=TRUE) +
  geom_path(aes(long,lat, group = group, color = log(FromToCost, base = 10)),
            data = nyc_streets_df) +
  theme(legend.position = "bottom")

ggmap(man) + theme_nothing(legend=TRUE) +
  geom_path(aes(long,lat, group = group, 
                color = factor(is.na(OneWay), labels = c("Not One Way", "One Way"))),
            data = nyc_streets_df) +
  labs(color = "") +
  theme(legend.position = "bottom")

# part2: pedestrian deaths from  New York City Department of Transportation (DOT) 
## @ http://www.nyc.gov/html/dot/html/about/vz_datafeeds.shtml
## meta http://www.nyc.gov/html/dot/downloads/pdf/vision-zero-view-metadata.pdf
deaths_nyc <- read.csv("http://www.nyc.gov/html/dot/downloads/misc/fatality_yearly.csv")
#Only deaths between 2009 and 2016
deaths_nyc <- deaths_nyc[deaths_nyc$PedFatalit>0 & deaths_nyc$YR < 2017,]
deaths_nyc <- deaths_nyc[!is.na(deaths_nyc$nodeX),]

#must project to WGS84 to use ggmap
NAD83 <- "+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 +lat_0=40.16666666666666 
+lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80
+towgs84=0,0,0"
WGS84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
pts_nyc <- SpatialPoints(cbind(deaths_nyc$nodeX,deaths_nyc$nodeY),
                         CRS(NAD83))
pts_nyc <- spTransform(pts_nyc, WGS84)
deaths_nyc$LONGITUD <- pts_nyc@coords[,1]
deaths_nyc$LATITUDE <- pts_nyc@coords[,2]

pts_nyc <-
  pts_nyc[deaths_nyc$LONGITUD > nyc_streets@bbox[1,1] & 
          deaths_nyc$LONGITUD < nyc_streets@bbox[1,2] &
          deaths_nyc$LATITUDE > nyc_streets@bbox[2,1] & 
          deaths_nyc$LATITUDE < nyc_streets@bbox[2,2],]

deaths_nyc <- deaths_nyc[deaths_nyc$LONGITUD > nyc_streets@bbox[1,1] & 
                         deaths_nyc$LONGITUD < nyc_streets@bbox[1,2] &
                         deaths_nyc$LATITUDE > nyc_streets@bbox[2,1] & 
                         deaths_nyc$LATITUDE < nyc_streets@bbox[2,2],]

#takes about 5 min.
ggmap(man) + theme_nothing() +
  geom_path(aes(long,lat, group = group), color = "blue",
            data = nyc_streets_df) +
  geom_point(aes(LONGITUD,LATITUDE), data = deaths_nyc)

#takes about 5 min.
ggmap(man) + theme_nothing(legend = TRUE) +
  geom_path(aes(long,lat, group = group), color = "blue",
            data = nyc_streets_df) +
  geom_point(aes(LONGITUD,LATITUDE, color = factor(YR)), data = deaths_nyc) +
  theme(legend.position = "bottom") + labs(color = "Year")

#takes about 5 min.
dist_mat <- gDistance(pts_nyc,  nyc_streets, byid=TRUE)
deaths_nyc$RoadID <- apply(dist_mat, 2, which.min)
deaths_nyc$MinDiff <- apply(dist_mat, 2, min)

#find the number of fatalities per road
road_deaths <-
  join(data.frame(RoadID = unique(nyc_streets@data$RoadID)),
       aggregate(PedFatalit ~ RoadID,deaths_nyc, sum))
road_deaths$PedFatalit[is.na(road_deaths$PedFatalit)] <- 0

table(road_deaths$PedFatalit)

road_deaths$PedFatalit <- ifelse(road_deaths$PedFatalit == 0, 0 ,1)

table(road_deaths$PedFatalit)
table(road_deaths$PedFatalit)/nrow(road_deaths)

nyc_streets_df <- join(nyc_streets_df, road_deaths, by = "RoadID")

ggmap(man) + theme_nothing(legend = TRUE) +
  geom_path(aes(long,lat, group = group, 
                color = factor(PedFatalit,labels = c("No Death", "Deaths"))),
            data = nyc_streets_df) +
  geom_point(aes(LONGITUD,LATITUDE), data = deaths_nyc) +
  theme(legend.position = "bottom") + labs(color = "")

#Maybe we want all roads close to fatality? Say within .0005 degrees
road_id_list <- apply(dist_mat,2,function(clmn) rownames(dist_mat)[which(clmn < .0005)])
road_deaths <- join(road_deaths, unique(nyc_streets@data[,c("id","RoadID")]), by = "RoadID")
road_deaths$PedFatalit_Adj <- as.numeric(road_deaths$id %in% unlist(road_id_list))

table(road_deaths$PedFatalit_Adj)

nyc_streets_df <- join(nyc_streets_df, road_deaths[,c("RoadID","PedFatalit_Adj")], by = "RoadID")  

ggmap(man) + theme_nothing(legend = TRUE) +
  geom_path(aes(long,lat, group = group, 
                color = factor(PedFatalit_Adj,labels = c("No Death", "Deaths"))),
            data = nyc_streets_df) +
  geom_point(aes(LONGITUD,LATITUDE), data = deaths_nyc) +
  theme(legend.position = "bottom") + labs(color = "")

#part1: predict the locations of fatalities
cov <- c(
"Shape_Leng", #Length of ROad
"ACC",        #Arterial Classification Code (see meta)
"FCC",        #Feature Classification Code (see meta)
"LeftCounty", #Borough
"Navigation", #General Travel Direction
"SHIELD",     #Road Type (Highway, Interstate, etc.)
"JURISDICTI", #Street Owner
"OneWay",     #One Way Direction
"SPEED",      #Speed Limit
"FromToCost", #time cost (travel time) in meters per minute
"ToFromCost", #time cost (travel time) in meters per minute
"FromZlev",   #node elevation
"ToZlev",     #node elevation
"PreType",    #street type
"PostType")   #street type

#logistic regression
model_data <- unique(nyc_streets_df[,c("PedFatalit_Adj","RoadID",cov)])
model_data <- model_data[model_data$LeftCounty %in% 
                           c("Bronx","Kings","New York", "Queens", "Richmond"),]
model_data$LeftCounty <- factor(model_data$LeftCounty)
model_data$Navigation <- addNA(model_data$Navigation)
model_data$SHIELD <- addNA(model_data$SHIELD)
model_data$OneWay <- addNA(model_data$OneWay)
model_data$Cost <- apply(cbind(model_data$FromToCost,model_data$ToFromCost),1,max)
model_data <- model_data[,!colnames(model_data) %in% c("ToFromCost","FromToCost")]
model_data$PreType <- addNA(model_data$PreType)
model_data$PostType <- addNA(model_data$PostType)

#marginal with ggplot
ggplot(model_data) +
  theme_minimal() +
  aes(SPEED, PedFatalit_Adj) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

ggplot(model_data) +
  theme_minimal() +
  aes(SPEED, PedFatalit_Adj) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

marginal_fit <- glm(PedFatalit_Adj ~ SPEED, data = model_data ,family = "binomial")
summary(marginal_fit)
predict.glm(object = marginal_fit, 
            newdata = data.frame(SPEED = c(25,30)), 
            type = "response")
#estimated percent reduction in fatalities
100 * (0.0457 - 0.0494)/.0494

#multivariate logistic regression
multi_fit <- glm(PedFatalit_Adj ~ Shape_Leng + ACC + FCC + SHIELD + SPEED + OneWay,
                 data = model_data,
                 family = "binomial")
summary(multi_fit)
multi_fit$coefficients["SPEED"]

#nb speed impact cut in half
model_data25 <- model_data30 <- model_data[model_data$SPEED == 25,]
model_data25$SPEED <- 25
model_data25$fit <- predict(multi_fit, type = "response", newdata = model_data25)

model_data30$SPEED <- 30
model_data30$fit <- predict(multi_fit, type = "response", newdata = model_data30)

#estimated percent reduction in fatalities
100 * (mean(model_data30$fit) - mean(model_data25$fit)) / mean(model_data25$fit)
 
#how well can we predict deaths?
train_data <- model_data[model_data$LeftCounty != "Kings",]
test_data <- model_data[model_data$LeftCounty == "Kings",]
test_data <- test_data[test_data$FCC != "A48",]

#we usually scale and center data for prediction
train_data[,c(FALSE,FALSE,sapply(train_data[,-c(1,2)],is.numeric))] <- 
  scale(train_data[,c(FALSE,FALSE,sapply(train_data[,-c(1,2)],is.numeric))])
test_data[,c(FALSE,FALSE,sapply(test_data[,-c(1,2)],is.numeric))] <- 
  scale(test_data[,c(FALSE,FALSE,sapply(test_data[,-c(1,2)],is.numeric))])

multi_fit <- glm(PedFatalit_Adj ~ Shape_Leng + ACC + FCC + SHIELD + SPEED + OneWay,
                 data = train_data,
                 family = "binomial")

test_data$lreg <- predict(multi_fit, 
                          newdata = test_data, 
                          type = "response")
sqrt(mean((test_data$lreg - test_data$PedFatalit_Adj)^2))
mean(abs(test_data$lreg - test_data$PedFatalit_Adj))

#can we throw every predictor in?
all_fit <- glm(PedFatalit_Adj ~ .,
                 data = train_data,
                 family = "binomial")

#neural net
nnet_fit <- nnet(factor(PedFatalit_Adj) ~ Shape_Leng + ACC + FCC + SHIELD + SPEED + OneWay,
                 data = train_data, size = 10, decay=5e-4, maxit=200)

test_data$nnet <- predict(nnet_fit, newdata = test_data, type = "raw")
sqrt(mean((test_data$nnet - test_data$PedFatalit_Adj)^2))
mean(abs(test_data$nnet - test_data$PedFatalit_Adj))

nnet_fit <- nnet(factor(PedFatalit_Adj) ~ .,
                 data = train_data, size = 2, decay=5e-4, maxit=200)

test_data$nnet <- predict(nnet_fit, newdata = test_data, type = "raw")
sqrt(mean((test_data$nnet - test_data$PedFatalit_Adj)^2))
mean(abs(test_data$nnet - test_data$PedFatalit_Adj))

#neuralnet
train_matrix <- data.frame(PedFatalit_Adj = train_data$PedFatalit_Adj,
                      model.matrix(PedFatalit_Adj~ Shape_Leng + ACC + FCC + 
                                     SHIELD + SPEED + OneWay,train_data))
test_matrix <- data.frame(PedFatalit_Adj = test_data$PedFatalit_Adj,
                     model.matrix(PedFatalit_Adj~Shape_Leng + ACC + FCC + 
                                    SHIELD + SPEED + OneWay,test_data))
form <- formula(paste0("PedFatalit_Adj ~ ",
               paste0(colnames(train_matrix)[-1],collapse = "+")))

nnet_fit2 <- neuralnet(form,data = train_matrix, 
                       linear.output = TRUE,
                       hidden = 1, stepmax = 100000)

plot(nnet_fit2)
test_data$nnet2 <- compute(nnet_fit2,test_matrix[,-1])$net.result

sqrt(mean((test_data$nnet2 - test_data$PedFatalit_Adj)^2))
mean(abs(test_data$nnet2 - test_data$PedFatalit_Adj))


