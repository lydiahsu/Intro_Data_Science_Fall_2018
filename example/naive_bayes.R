###############
# NAIVE BAYES #
###############

#Name: 
#Date: 
#Summary: This assignment is to 1. practice using R functions 2. demonstrate of Naive Bayes

#Lines with # are comments
#Today we will use the following two packages
install.packages("RCurl")
library("RCurl")
install.packages("plyr")
library("plyr")

#Our data is the King James Bible from Project Gutenberg
url <- "http://www.gutenberg.org/cache/epub/10/pg10.txt"
text <- getURL(url)
text #flood the console with text from King James Bible
substr(text,1,10000) ###\r\n is line break; 1:1 is chapter:verse

#We start by cleaning the data
#First an example using the strsplit and gsub functions
temp <- c("asdfas\r\n\r\n1:1nasds")
temp
?strsplit
strsplit(temp,"\r\n\r\n")
strsplit(temp,"[0-9]:[0-9]")
strsplit(gsub("[\r\n]"," ",temp),"[0-9]:[0-9]")

docs <- strsplit(gsub("[\r\n]"," ",text),"[0-9]:[0-9]")
docs <- unlist(docs)
docs <- docs[2:31103]
docs[31102] <- substr(docs[31102],1,58)
docs <- tolower(trimws(gsub("[0-9]", "",docs)))

head(docs,n=10)

#Now we separate the bible into Old and New Testament
#We know the New Testament starts with the line "now in the first year of cyrus king of persia"
for(i in seq_along(docs)){
  if(substr(docs[i],1,45) == "now in the first year of cyrus king of persia"){j <- i}
}
j/length(docs)
docs[j] #j is 12018
#create outcome variable
class <- c(rep(0,j-1),rep(1,length(docs)-(j-1)))
table(class)/length(class)

####How hard is the classificaiton problem, anyway?
k <- sample(1:31102,1)
docs[k]
k < j

set.seed(1)
k <- sample(
            c(sample(1:(j-1),10),
              sample(j:31102,10)),
            5)
docs[k]
k < j

#Now it's time to classify with naive bayes
num_train <- 10000
old <- docs[class==0][1:num_train]
new <- docs[class==1][1:num_train]

#create a vocab list
old_vocab <- gsub("[[:punct:]]", "", unlist(strsplit(old," ")))
new_vocab <- gsub("[[:punct:]]", "", unlist(strsplit(new," ")))

old_vocab <- count(old_vocab)
new_vocab <- count(new_vocab)

head(new_vocab,n=50)
head(old_vocab,n=50)

vocab <- merge(old_vocab,new_vocab,by="x",all=TRUE)
vocab[is.na(vocab)] <- 1

#remove common words
vocab[vocab$freq.x > 500 | vocab$freq.y > 500,]
vocab <- vocab[! (vocab$freq.x > 500 | vocab$freq.y > 500),]

#Now that we have the vocab list, we can classify verses
verse <- gsub("[[:punct:]]", "", unlist(strsplit(docs[1]," ")))
verse_counts <- merge(verse,vocab)
verse_counts$bayes_old <- verse_counts$freq.x/(verse_counts$freq.x+verse_counts$freq.y)
verse_counts$bayes_new <- verse_counts$freq.y/(verse_counts$freq.x+verse_counts$freq.y)
if((j/length(docs))*prod(verse_counts$bayes_old) > (1-j/length(docs))*prod(verse_counts$bayes_new)){0}else{1}

verse <- gsub("[[:punct:]]", "", unlist(strsplit(docs[30000]," ")))
verse
verse_counts <- merge(verse,vocab)
verse_counts$bayes_old <- verse_counts$freq.x/(verse_counts$freq.x+verse_counts$freq.y)
verse_counts$bayes_new <- verse_counts$freq.y/(verse_counts$freq.x+verse_counts$freq.y)
if((j/length(docs))*prod(verse_counts$bayes_old) > (1-j/length(docs))*prod(verse_counts$bayes_new)){0}else{1}

#Now we evaluate naive bayes with the test set
old_test <- docs[class==0][(num_train+1):j]
new_test <- docs[class==1][(num_train+1):(length(docs)-j)]

naive_bayes <- function(doc){
  verse <- gsub("[[:punct:]]", "", unlist(strsplit(doc," ")))
  verse_counts <- merge(verse,vocab)
  verse_counts$bayes_old <- verse_counts$freq.x/(verse_counts$freq.x+verse_counts$freq.y)
  verse_counts$bayes_new <- verse_counts$freq.y/(verse_counts$freq.x+verse_counts$freq.y)
  if((j/length(docs))*prod(verse_counts$bayes_old) > (1-j/length(docs))*prod(verse_counts$bayes_new)){0}else{1}
}

old_class <- unlist(lapply(old_test,naive_bayes))
new_class <- unlist(lapply(new_test,naive_bayes))

table(old_class)/length(old_class)
table(new_class)/length(new_class)

