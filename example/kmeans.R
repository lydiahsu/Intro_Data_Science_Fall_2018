##########
# KMEANS #
##########

#Name: 
#Date: 
#Summary: 


###read packages
install.packages("rgeos")
install.packages("rgdal")
install.packages("maptools") 
install.packages("ggplot2")
install.packages("plyr")
install.packages("reshape2")
install.packages("ggmap")
library("rgeos")
library("rgdal")
library("maptools")
library("ggplot2")
library("plyr")
library("reshape2")
library("ggmap")

#part1: map shapefiles
dir.create(file.path(getwd(), "shapefiles"))
temp <- tempfile()
download.file("http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyct2010_16c.zip", 
              temp)
unzip(temp,exdir="shapefiles")
tracts <- readOGR("shapefiles/nyct2010_16c","nyct2010")
plot(tracts)

tracts@data$id <- rownames(tracts@data)
tracts.points <- fortify(tracts, region="id")
tracts.df <- join(tracts.points, tracts@data, by="id")

ggplot(tracts.df) +
  aes(long,lat,group=group) + 
  geom_polygon() +
  coord_equal()

ggplot(tracts.df) +
  aes(long,lat,group=group,fill=BoroCode) + 
  geom_polygon() +
  coord_equal() +
  scale_fill_brewer("NYC Borough",palette="Set1") 

ggplot(tracts.df) +
  aes(long,lat,group=group,fill=factor(BoroCode)) + 
  geom_polygon() +
  coord_equal() +
  scale_fill_brewer("NYC Borough",palette="Set3") 

ggplot(tracts.df) +
  theme_nothing(legend=TRUE) +
  aes(long,lat,group=group,fill=factor(BoroCode)) + 
  geom_polygon() +
  coord_equal() +
  scale_fill_brewer("NYC Borough",palette="Set3") 

ggplot(tracts.df) +
  theme_nothing() +
  aes(long,lat,group=group,fill=factor(NTAName)) + 
  geom_polygon() +
  coord_equal()

###part2: acs from Census Planning Database @ http://www.census.gov/research/data/planning_database/
acs <- read.csv("https://raw.githubusercontent.com/lydiahsu/Intro-Data-Science-Fall-2016/gh-pages/example/acs.csv")
income <- acs[,c(1,104,105,119,120)]
nonincome <- acs[,-c(104,105,119,120)]

head(income[,2])
head(gsub("\\$|\\,","",as.character(income[,2])))

income[,-1] <- apply(income[,-1],2,function(x) as.numeric(gsub("\\$|\\,","",as.character(x))))

class(tracts.df$BoroCT2010)
class(income$BoroCT2010)
income$BoroCT2010 <- factor(income$BoroCT2010)

tracts.df <- join(tracts.df,income, by="BoroCT2010")

ggplot() +
  theme_nothing(legend=TRUE) +
  geom_polygon(aes(long,lat,group=group,fill=Med_HHD_Inc_ACS_08_12),data=tracts.df) +
  coord_equal() +
  scale_fill_continuous("Income") 

ggplot() +
  theme_nothing(legend=TRUE) +
  geom_polygon(aes(long,lat,group=group,fill=log(Med_HHD_Inc_ACS_08_12,base=10)),
               data=tracts.df) +
  coord_equal() +
  scale_fill_continuous("Income") 

ggplot() +
  theme_nothing(legend=TRUE) +
  geom_polygon(aes(long,lat,group=group,fill=log(Med_House_value_ACS_08_12,base=10)),
               data=tracts.df) +
  coord_equal() +
  scale_fill_continuous(expression(log[10]("Income"))) +
  theme(legend.position="bottom")

ggplot() +
  theme_nothing(legend=TRUE) +
  geom_polygon(aes(long,lat,group=group,fill=log(Med_HHD_Inc_ACS_08_12,base=10)),
               data=tracts.df) +
  coord_equal() +
  scale_fill_continuous(expression(log[10]("income")),
                        low="red",high="blue",
                        na.value = "white") +
  theme(legend.position="bottom")

long.tracts.df <- tracts.df
long.tracts.df[,19:22] <- apply(long.tracts.df[,19:22],2,function(x) x/max(x,na.rm=TRUE))
long.tracts.df <- melt(long.tracts.df,id=colnames(long.tracts.df)[1:18])
dim(tracts.df)
dim(long.tracts.df)  
633232/158308  

ggplot() +
  theme_nothing(legend=TRUE) +
  geom_polygon(aes(long,lat,group=group,fill=log(value,base=10)),
               data=long.tracts.df) +
  coord_equal() +
  scale_fill_continuous(expression(log[10]("Normalized Income")),
                        low="red",high="blue",na.value="white") +
  theme(legend.position="bottom") +
  facet_wrap(~variable) 

ggplot() +
  theme_nothing(legend=TRUE) +
  geom_polygon(aes(long,lat,group=group,fill=log(value,base=10)),
               data=long.tracts.df) +
  coord_equal() +
  scale_fill_continuous(expression(log[10]("Normalized Income")),
                        low="red",high="blue",na.value="white") +
  theme(legend.position="bottom",
        strip.background = element_rect(fill="white"))+
  facet_wrap(~variable)

###part3: cluster
colnames(nonincome)
colnames(nonincome[,c(13,14,15,17)])

cluster_data <- nonincome[,c(13,14,15,17)]

ggplot(cluster_data) + 
  aes(NH_White_alone_ACS_08_12,NH_Blk_alone_ACS_08_12)+
  geom_point()

table(is.na(cluster_data))
cluster_data[is.na(cluster_data)] <- 0
cluster_data$kmeans <- kmeans(cluster_data,3)$cl
cluster_data$hclust <-factor(cutree(hclust(dist(cluster_data),"ave"),6))

ggplot(cluster_data) + 
  aes(NH_White_alone_ACS_08_12,NH_Blk_alone_ACS_08_12,
      color=kmeans)+
  geom_point()

ggplot(cluster_data) + 
  aes(NH_White_alone_ACS_08_12,NH_Blk_alone_ACS_08_12,
      color=hclust)+
  geom_point()

ggplot(cluster_data) + 
  aes(NH_White_alone_ACS_08_12,NH_Asian_alone_ACS_08_12,
      color=kmeans)+
  geom_point()

ggplot(cluster_data) + 
  aes(NH_White_alone_ACS_08_12,NH_Asian_alone_ACS_08_12,
      color=hclust)+
  geom_point()

ggplot(cluster_data) + 
  aes(Hispanic_ACS_08_12,NH_Blk_alone_ACS_08_12,
      color=kmeans)+
  geom_point()

nonincome$kmeans <- cluster_data$kmeans
nonincome$hclust <- cluster_data$hclust
tracts.df <- join(tracts.df,nonincome, by="BoroCT2010")

ggplot() +
  theme_nothing(legend=TRUE) +
  geom_polygon(aes(long,lat,group=group,fill=factor(kmeans)),
               data=tracts.df) +
  scale_fill_brewer("Race/Ethnic",palette="Set1") +
  coord_equal() +
  theme(legend.position="bottom")

ggplot() +
  theme_nothing(legend=TRUE) +
  geom_polygon(aes(long,lat,group=group,fill=factor(hclust)),
               data=tracts.df) +
  #scale_fill_brewer("Race/Ethnic",palette="Set1") +
  coord_equal() +
  theme(legend.position="bottom")

