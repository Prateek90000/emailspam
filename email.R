rm(list=ls())

emails = read.csv("emails.csv", stringsAsFactors = FALSE)
email$numcharacters = nchar(emails$text)
#install.packages("tm")
#install.packages("SnowballC")
library(tm)
library(SnowballC)
corpus <- Corpus(VectorSource(emails$text))
corpus <- tm_map(corpus, tolower)
#corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
dtm <- DocumentTermMatrix(corpus)
dtm
sparseDTM = removeSparseTerms(dtm,0.95)
emailsSparse <- as.data.frame(as.matrix(sparseDTM))
colnames(emailsSparse) <- make.names(colnames(emailsSparse))
emailsSparse$spam = emails$spam
emailsSparse$spam = as.factor(emailsSparse$spam)

library(caTools)
set.seed(3)
split <- sample.split(emailsSparse$spam, SplitRatio = 0.85)
train <- subset(emailsSparse, split == T )
test <- subset(emailsSparse, split == F)

spamModel <- glm(spam ~., data = train, family = binomial)
summary(spamModel)
preds = predict(spamModel,newdata = test)

truthTable =table(test$spam,preds>0.5)

ACCURACY =(truthTable[1,1]+truthTable[2,2])/(truthTable[1,1]+truthTable[1,2]+truthTable[2,1]+truthTable[2,2])
fpr = (truthTable[1,2])/(truthTable[1,2]+truthTable[1,1])
fnr = (truthTable[2,1])/(truthTable[2,1]+truthTable[2,2])
tpr = (truthTable[2,2])/(truthTable[2,1]+truthTable[2,2])
tnr = (truthTable[1,1])/(truthTable[1,1]+truthTable[1,2])
                                                                                 