sms_data1=read.csv(file.choose(),stringsAsFactors = T)
View(sms_data)
str(sms_data)
table(sms_data$type)

library(tm)
sms_corpus=Corpus(VectorSource(sms_data1$text))

#Cleaning data
corpus_clean<-tm_map(sms_corpus,tolower)
corpus_clean<-tm_map(corpus_clean,removeNumbers)
corpus_clean<-tm_map(corpus_clean,removePunctuation)
corpus_clean<-tm_map(corpus_clean,removeWords,stopwords())
corpus_clean<-tm_map(corpus_clean,stripWhitespace)
class(corpus_clean)
as.character(corpus_clean[1])

#Converting to Document term matrix
sms_dtm<-DocumentTermMatrix(corpus_clean)

#Spliting to test and train
sms_dtm_train<-sms_dtm[1:4169,]
sms_dtm_test<-sms_dtm[4170:5559, ]

sms_rawtrain<-sms_data1[1:4169,]
sms_rawtest<-sms_data1[4170:5559,]

#Most frequent terms
sms_freq<-findFreqTerms(sms_dtm_train,5)

sms_dtm_train<-sms_dtm_train[,sms_freq]
sms_dtm_train<-as.data.frame(sms_dtm_test)
inspect(sms_dtm_train[1:10,])

counts<-function(x)
{
  ifelse(x>0,'Yes','No')
}
sms_dtm_train<-apply(sms_dtm_train, MARGIN = 2,counts)
sms_dtm_test<-apply(sms_dtm_test,MARGIN = 2,counts)

#building Naives bayes
sms_corpus
library(e1071)
model=naiveBayes(sms_dtm_train,sms_rawtrain$type)
predvalues<-predict(model,sms_dtm_test)

mean(predvalues==sms_rawtest$type)
#Accuracy is 97%

library(gmodels)
CrossTable(predvalues, sms_rawtest$type)
