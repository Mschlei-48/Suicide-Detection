

library(tidytext)
library(gmodels)
library(randomForest)
library(dplyr)
library(keras)
library(ggplot2)
library(tools)
library(tm)
library(textclean)
library(e1071)
library(syuzhet)
library(lubridate)
library(rsample)
library(reshape2)
library(caret)
library(RCurl)
library(OpenImageR)
library(magick)
library(pdftools)
library(tidyverse)
library(RTextTools)
library(magick)
library(pdftools)
library(tesseract)
library(keras)
library(dplyr)
library(ggplot2)
library(purrr)
library(tensorflow)
library(tidyverse)
library(gutenbergr)
library(caTools)














model_user_input=function(new_data)
{
  new_corpus=Corpus(VectorSource(new_data$Text))
  new_data=tm_map(new_corpus,tolower)
  new_data=tm_map(new_data,removeNumbers)
  new_data=tm_map(new_data,removeWords,stopwords('english'))
  new_data=tm_map(new_data,removePunctuation)
  new_data <- tm_map(new_data, stripWhitespace)
  
  new_data = tm_map(new_data, stemDocument)
  new_data <- DocumentTermMatrix(new_data)
  
  new_data=as.data.frame(as.matrix(new_data))
  colnames(new_data) <- make.names(colnames(new_data))
  
  
  
  
  
  
  
  
  
  
  data=read.csv('C:/Users/Student/Desktop/Suicide_Detection.csv',nrows = 6000)
  
  lang=tesseract("eng")
  
  sui_list=list.files(path="C:/Users/Student/Desktop/3rd Year/3rd Year 2ND Semester/DataScience2B/Project/New_SuicideQuotes", pattern=".jpg|.png",all.files=T, full.names=T) 
  
  sui_images=c()
  for(img in sui_list)
  {
    im=image_read(img)
    sui_images=append(im,sui_images)
  }
  
  
  images=c()
  list=list.files(path="C:/Users/Student/Desktop/3rd Year/3rd Year 2ND Semester/DataScience2B/Project/New_HappyQuotes", pattern=".jpg|.png",all.files=T, full.names=T)
  
  for(img in list)
  {
    im=image_read(img)
    images=append(im,images)
  }
  
  
  
  sui_imp=c()
  for (i in 1:length(sui_images)) {
    img=image_modulate(sui_images[i], brightness = 200)
    img=image_contrast(img)
    img=image_enhance(img)
    sui_imp=append(img,sui_imp)
  }
  imp=c()
  for (i in 1:length(images)) {
    img=image_modulate(images[i], brightness = 200)
    img=image_contrast(img)
    img=image_enhance(img)
    imp=append(img,imp)
  }
  
  
  
  sui_txts=c()
  for (i in 1:length(sui_imp)) {
    text=tesseract::ocr(sui_imp[i], engine = lang)
    sui_txts=append(text,sui_txts)
  }
  txts=c()
  for (i in 1:length(imp)) {
    text=tesseract::ocr(imp[i], engine = lang)
    txts=append(text,txts)
  }
  
  sui_txts=sui_txts[-c(8,14,16,17,19,20,21,26,33,34,41)]
  
  txts=txts[-c(2,5,6,9,10,11,13,14,15,16,17,20,22,23,32,49)]
  
  
  DF <- as.data.frame(sui_txts)
  DF2 <- as.data.frame(txts)
  
  
  DF["class"] <- "suicide"
  DF2["class"] <- "non-suicide"
  
  colnames(DF)=c("text","class")
  colnames(DF2)=c("text","class")
  
  data=subset(data,select = -c(X))
  
  
  dataframe=rbind(DF,data)
  
  dataframe=rbind(DF2,data)
  
  
  corpus=Corpus(VectorSource(dataframe$text))
  
  
  
  
  
  corpus=tm_map(corpus,tolower)
  corpus=tm_map(corpus,removeNumbers)
  corpus=tm_map(corpus,removeWords,stopwords('english'))
  corpus=tm_map(corpus,removePunctuation)
  removeSym=function(x) gsub("\n", "  " , x)
  corpus=tm_map(corpus,content_transformer(removeSym))
  removeSym=function(x) gsub("@", "  " , x)
  corpus=tm_map(corpus,content_transformer(removeSym))
  removeSym=function(x) gsub("retweeted", "  " , x)
  corpus=tm_map(corpus,content_transformer(removeSym))
  removeSym=function(x) gsub("twitter", "  " , x)
  corpus=tm_map(corpus,content_transformer(removeSym))
  removeSym=function(x) gsub("tweet", "  " , x)
  corpus=tm_map(corpus,content_transformer(removeSym))
  removeSym=function(x) gsub("android", "  " , x)
  corpus=tm_map(corpus,content_transformer(removeSym))
  removeSym=function(x) gsub("jan", "  " , x)
  corpus=tm_map(corpus,content_transformer(removeSym))
  removeSym=function(x) gsub("quotes", "  " , x)
  corpus=tm_map(corpus,content_transformer(removeSym))
  
  
  
  corpus = tm_map(corpus, stemDocument)
  
  
  frequencies = DocumentTermMatrix(corpus)
  sparse = removeSparseTerms(frequencies, 0.995)
  
  
  tSparse = as.data.frame(as.matrix(sparse))
  tSparse$class = dataframe$class
  
  split = sample.split(tSparse$class, SplitRatio = 0.7)
  trainSparse = subset(tSparse, split==TRUE)
  
  colnames(trainSparse) <- make.names(colnames(trainSparse))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  common_names = intersect(colnames(trainSparse),colnames(new_data))
  new_data <- subset(new_data, select=names(new_data) %in% common_names)
  
  
  #Add more columns to the new data
  
  for (i in 1:ncol(trainSparse)) {
    for (n in 1:ncol(new_data)) {
      if(colnames(trainSparse)[i]==colnames(new_data)[n])
      {
        
        break
      }
      else if(n==ncol(new_data))
      {
        
        new_data[colnames(trainSparse)[i]]=0
      }
      
      
    }
    
    
  }
  return(new_data)
}



OCR=function(image)
{
    lang=tesseract("eng")
    
    image=image_modulate(image, brightness = 200)
    image=image_contrast(image)
    image=image_enhance(image)
    image_text=tesseract::ocr(image, engine = lang)
    image_text=as.list(image_text)
    image_text=as.data.frame(image_text)
    colnames(image_text)=c("Text")
    
  return(image_text)
}






