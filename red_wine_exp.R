text <- "I have a cat"
new_text <- gsub(" ", "_", text)
new_text

path <- "Studies/Sunway/2408-MDS5033 Statistical Methods for Data Science/wine+quality/winequality-red.csv"
red_wine <- read.csv(path, header=TRUE, sep=";")
features <- colnames(red_wine)
features

col_len <- ncol(red_wine)
col_len #12

library("stringr") 

for (col in 1:col_len) {
  #print(colnames(red_wine)[col])
  #colnames(red_wine)[col] <- sub(".", "_", colnames(red_wine)[col])
  #print(colnames(red_wine)[col])
  
  print(str_replace_all("colnames.123",".", "_")) 
}

colnames(red_wine)[2] <- gsub(".", "_", colnames(red_wine)[2])
colnames(red_wine)[2]

newcol1 <- colnames(red_wine)[1] <- "fixed_acidity"
newcol1
colnames(red_wine)[1]
