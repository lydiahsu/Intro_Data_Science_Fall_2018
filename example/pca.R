#######
# PCA #
#######

#Name: 
#Date: 
#Summary: 

#install/read packages
install.packages("pixmap")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("ggfortify")
library("pixmap")
library("ggplot2")
library("reshape2")
library("ggfortify")

#read pics
dir.create(file.path(getwd(), "pics"))
temp <- tempfile()
download.file("https://github.com/lydiahsu/Intro-Data-Science-Fall-2016/blob/gh-pages/example/jaffe_pgm.zip?raw=true", 
              temp)
unzip(temp,exdir="pics")
setwd("pics/jaffe_pgm/")
files <- dir()[grep("pgm",dir())] 
faces <-  vector("list", length(files))
for(i in seq_along(files)) faces[[i]] <- read.pnm(files[i])
seq_along(files) == 1:length(files)
length(faces)

#plot some pics
plot(faces[[1]])
plot(faces[[50]])
plot(faces[[100]])

ggplot(melt(getChannels(faces[[1]])))+
  theme_minimal() +
  theme(legend.position="none") +
  aes(Var2,-Var1,color=value) +
  geom_point() +
  scale_color_continuous(low="black",high="white") +
  labs(x="",y="")

better_plot <- function(i) 
  ggplot(melt(getChannels(faces[[i]])))+
    theme_minimal() +
    theme(legend.position="none") +
    aes(Var2,-Var1,color=value) +
    geom_point() +
    scale_color_continuous(low="black",high="white") +
    labs(x="",y="")

better_plot(100)
  
#read experiment data
files
file_names <- substr(gsub("\\.","",files),1,5)

dep <- read.csv("https://raw.githubusercontent.com/lydiahsu/Intro-Data-Science-Fall-2016/gh-pages/example/jaffe_dep.txt", sep="")
dep$subj_names <- substr(gsub("-","",dep$PIC),1,2) # person's name
dep$subj_emot  <- substr(gsub("-","",dep$PIC),3,4) # what they were told to do
dep$subj_names[which(! dep$subj_names %in% file_names)]
dep <- dep[gsub("-","",dep$PIC) %in% file_names,]
dep <- dep[order(gsub("-","",dep$PIC)),]

ggplot(dep[dep$subj_names == "KM",]) +
  theme_light() +
  aes(subj_emot,HAP) +
  geom_boxplot()
  
ggplot(melt(dep[dep$subj_names == "KM",])) +
  theme_light() +
  aes(subj_emot,value) +
  geom_boxplot() +
  facet_wrap(~variable)

which(dep$subj_names == "KM" & dep$subj_emot == "FE")
better_plot(51)
better_plot(52)
better_plot(53)

#mean face
pic_mat <- sapply(seq_along(faces),function(i) as.vector(getChannels(faces[[i]])))
class(pic_mat)
dim(pic_mat) #each picture is a row
plot(pixmapGrey(matrix(pic_mat[,1],ncol=256)))
ggplot(melt(matrix(pic_mat[,1],ncol=256))) + geom_point(aes(Var2,-Var1,color=value))
  
mean_face <- rowMeans(pic_mat)
plot(pixmapGrey(matrix(mean_face,ncol=256)))

trim_mean_face = apply(pic_mat,1,mean,trim=.25)
plot(pixmapGrey(matrix(trim_mean_face,ncol=256)))

median_face = apply(pic_mat,1,median)
plot(pixmapGrey(matrix(median_face,ncol=256)))

#pca
pic_mat_centered <- pic_mat
for (i in 1:ncol(pic_mat_centered)){
  pic_mat_centered[,i] = pic_mat[,i] - mean_face
}

plot(pixmapGrey(matrix(pic_mat[,1],ncol=256)))
plot(pixmapGrey(matrix(pic_mat_centered[,1],ncol=256)))

pic_pca <- prcomp(pic_mat_centered)
summary(pic_pca)
plot(pic_pca)
#every picture can be written as combinations of the first two pcs
autoplot(pic_pca)
autoplot(pic_pca,loadings=TRUE, loadings.label = TRUE)

#plot pcs
plot(pixmapGrey(matrix(pic_pca$x[,4],ncol=256)))
plot(pixmapGrey(matrix(pic_pca$x[,12],ncol=256)))

pics <- 1:16
#pics <- 20:36
temp1 <- cbind(matrix(pic_pca$x[,pics[1]],ncol=256),
               matrix(pic_pca$x[,pics[2]],ncol=256),
               matrix(pic_pca$x[,pics[3]],ncol=256),
               matrix(pic_pca$x[,pics[4]],ncol=256))
temp2 <- cbind(matrix(pic_pca$x[,pics[5]],ncol=256),
               matrix(pic_pca$x[,pics[6]],ncol=256),
               matrix(pic_pca$x[,pics[7]],ncol=256),
               matrix(pic_pca$x[,pics[8]],ncol=256))
temp3 <- cbind(matrix(pic_pca$x[,pics[9]],ncol=256),
               matrix(pic_pca$x[,pics[10]],ncol=256),
               matrix(pic_pca$x[,pics[11]],ncol=256),
               matrix(pic_pca$x[,pics[12]],ncol=256))
temp4 <- cbind(matrix(pic_pca$x[,pics[13]],ncol=256),
               matrix(pic_pca$x[,pics[14]],ncol=256),
               matrix(pic_pca$x[,pics[15]],ncol=256),
               matrix(pic_pca$x[,pics[16]],ncol=256))
temp <- rbind(temp1,temp2,temp3,temp4)
plot(pixmapGrey(temp))

#reconstruct faces
cumulative_sum <- matrix(numeric(256^2),nrow=256)
for(i in 1:10)
  cumulative_sum <- cumulative_sum +
    pic_pca$rotation[1,i] * matrix(pic_pca$x[,i],ncol=256)
plot(pixmapGrey(cumulative_sum))

partial_face <- function(n,face_number){
  cumulative_sum <- matrix(numeric(256^2),nrow=256)
  for(i in 1:n)
    cumulative_sum <- cumulative_sum +
      pic_pca$rotation[face_number,i] * matrix(pic_pca$x[,i],ncol=256)
  cumulative_sum}

plot(pixmapGrey(partial_face(50,1)))

temp1 <- cbind(partial_face(1,1),
               partial_face(2,1),
               partial_face(3,1))
temp2 <- cbind(partial_face(5,1),
               partial_face(10,1),
               partial_face(25,1))
temp3 <- cbind(partial_face(50,1),
               partial_face(100,1),
               partial_face(213,1))
temp <- rbind(temp1,temp2,temp3)
plot(pixmapGrey(temp))

temp1 <- cbind(partial_face(1,100),
               partial_face(2,100),
               partial_face(3,100))
temp2 <- cbind(partial_face(5,100),
               partial_face(10,100),
               partial_face(25,100))
temp3 <- cbind(partial_face(50,100),
               partial_face(100,100),
               partial_face(213,100))
temp <- rbind(temp1,temp2,temp3)
plot(pixmapGrey(temp))

#important loadings
summary(pic_pca) #fine to look at first 50
summary(lm(dep$HAP ~ pic_pca$rotation[,1:50]))
index <- order(abs(summary(lm(dep$HAP ~ pic_pca$rotation[,1:50]))$coef[,1]))
#index <- order(summary(lm(dep$HAP ~ pic_pca$rotation[,1:50]))$coef[,4])

temp1 <- cbind(matrix(pic_pca$x[,index[1]],ncol=256),
               matrix(pic_pca$x[,index[2]],ncol=256),
               matrix(pic_pca$x[,index[3]],ncol=256))
temp2 <- cbind(matrix(pic_pca$x[,index[5]],ncol=256),
               matrix(pic_pca$x[,index[6]],ncol=256),
               matrix(pic_pca$x[,index[7]],ncol=256))
temp3 <- cbind(matrix(pic_pca$x[,index[9]],ncol=256),
               matrix(pic_pca$x[,index[10]],ncol=256),
               matrix(pic_pca$x[,index[12]],ncol=256))
temp <- rbind(temp1,temp2,temp3)
plot(pixmapGrey(rbind(temp1,temp2,temp3)))
