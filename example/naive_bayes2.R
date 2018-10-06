###############
# NAIVE BAYES #
###############

#Author:  Jonathan Auerbach
#Contact: jla2167@columbia.edu
#Summary: 
## 1. build a naive bayes classifier to predict whether bible verse is new/old
## 2. practice using R documentation
## 3. learn basics of tidyverse


#install/load packages
install.packages("tidyverse")
install.packages("stringr")

library("tidyverse")
library("stringr")


#read/clean data
text <- readLines(con = "http://www.gutenberg.org/cache/epub/10/pg10.txt")
?readLines

text <- text[38:99849]
text <- text[text != ""]
?`[`

verse <- str_extract(string = text, pattern = "[0-9]+:[0-9]+")

text[which(verse == "1:1")]
text[which(verse == "1:1") - 1]

text <- text[- (which(verse == "1:1") - 1)]

text <- text %>% 
  str_c(collapse = " ") %>%
  str_split(pattern = "[0-9]+:[0-9]+") %>% 
  unlist() %>% 
  str_trim() %>% 
  str_replace_all(pattern = "[[:punct:]]", replacement = "") %>% 
  str_to_lower()

old <- text[2:12017]
new <- text[12018:length(text)]


#how hard is the classificaiton problem anyway?
num_vrs <- 10
k <- sample(x = seq_along(along.with = c(old, new)), 
            size = num_vrs)
c(old, new)[k]
ifelse(test = k <= length(old), yes = "old", no = "new")


#can we train a classifier to do better?
#create train and test sets
set.seed(seed = 1)
num_train <- 1000

smp_old <- sample(x = seq_along(along.with = old), size = num_train)
old_train <- old[smp_old]

smp_new <- sample(x = seq_along(along.with = new), size = num_train)
new_train <- new[smp_new]

old_test <- old[-smp_old]
new_test <- new[-smp_new]


#create vocab list
old_vocab <- old_train %>% str_split(" ") %>% unlist() %>% table() %>% as.tibble()
new_vocab <- new_train %>% str_split(" ") %>% unlist() %>% table() %>% as.tibble()

vocab <- full_join(x = old_vocab, y = new_vocab, by = ".") %>%
            rename(word = ".", old_count = "n.x", new_count = "n.y") %>%
            #"Laplacian smoothing": replace NA with 1
            mutate(old_count = ifelse(is.na(old_count), 1, old_count),
                   new_count = ifelse(is.na(new_count), 1, new_count),
                   old_prob = old_count / sum(old_count),
                   new_prob = new_count / sum(new_count))

naive_bayes <- function(vrs, vcb = vocab, prior = .5) {
  vrs_vocab <- vrs %>% 
                str_split(" ") %>% 
                table() %>% 
                as.tibble() %>% 
                left_join(vcb, by = c("." = "word"))
  #log to prevent underflow
  rule <- sum(log(vrs_vocab$old_prob), na.rm = TRUE) + 
                sum(log(vrs_vocab$n)) + 
                log(prior) > 
          sum(log(vrs_vocab$new_prob), na.rm = TRUE) + 
                sum(log(vrs_vocab$n)) +
                log(1 - prior)
  return(ifelse(rule, "old", "new"))
}

#great in sample :)
class_old_train <- sapply(old_train, naive_bayes)
table(class_old_train)/length(class_old_train)

class_new_train <- sapply(new_train, naive_bayes)
table(class_new_train)/length(class_new_train)

#not so great out of sample :(
class_old_test <- sapply(old_test, 
                         naive_bayes, 
                         prior = 
                           length(old_test)/(length(old_test) + length(new_test)))
table(class_old_test)/length(class_old_test)

class_new_test <- sapply(new_test, 
                         naive_bayes,
                         prior = 
                           length(old_test)/(length(old_test) + length(new_test)))
table(unname(class_new_test))/length(class_new_test)


#remove common words (stop words) strengthens the influence of priors
common_word_threshold <- 500
vocab_uncommon <- vocab %>% filter(old_count + new_count < common_word_threshold)

class_old_train2 <- sapply(old_train, naive_bayes, vcb = vocab_uncommon)
table(class_old_train)/length(class_old_train)
table(class_old_train2)/length(class_old_train2)

class_new_train2 <- sapply(new_train, naive_bayes, vcb = vocab_uncommon)
table(class_new_train)/length(class_new_train)
table(class_new_train2)/length(class_new_train2)

class_old_test2 <- sapply(old_test, 
                         naive_bayes, 
                         vcb = vocab_uncommon,
                         prior = 
                           length(old_test)/(length(old_test) + length(new_test)))
table(class_old_test)/length(class_old_test)
table(class_old_test2)/length(class_old_test2)


class_new_test2 <- sapply(new_test, naive_bayes, 
                         vcb = vocab_uncommon,
                         prior = 
                           length(old_test)/(length(old_test) + length(new_test)))
table(class_new_test)/length(class_new_test)
table(class_new_test2)/length(class_new_test2)
