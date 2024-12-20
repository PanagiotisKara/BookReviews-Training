library(readxl)
train_data<-read_excel("train_data.xls")
recommender_system<-read_excel("Recommender_System_κΝΝ.xls")
test_data<-read_excel("test_data.xls")
train_data

class(test_data)
train_data<-as.data.frame(train_data)
test_data<-as.data.frame(test_data)

readers_training<-length(train_data$TRAIN_READER)
readers_training

readers_test<-length(test_data$TEST_READER)
readers_test

columns_training<-ncol(train_data)
columns_training

k_NN<-3

names(train_data)[2]
books<-train_data[,-1]
books<-c(names(books))
books


recommendations <- data.frame(reader=character(0),book=character(0),rank=numeric(0))
str(recommendations)

MAE<-c()
MAE


calculate_similarities <- function() {
  nx <- nrow(train_data)
  ny <- nrow(test_data)
  ncolss <- ncol(train_data)
  
  similarities <- data.frame(character(),numeric(),numeric())
  
  for (i in 1:nx) {
    my_pearson <- list()
    for (j in 1:ny) {
      
      cor_x <- list()
      cor_y <- list()
      for(k in 2:ncolss)
      {
        if((!is.na(train_data[[i,k]])) && (!is.na(test_data[[j,k]])))
        {
          cor_x <- append(cor_x, train_data[[i,k]])
          cor_y <- append(cor_y, test_data[[j,k]])
        }
      }
      cor_x <- as.numeric(cor_x)
      cor_y <- as.numeric(cor_y)
      if(length(cor_x) == 0 || length(cor_y) == 0)
      {
        next
      }
      print(cor_x)
      print(cor_y)
      r <- cor(cor_x, cor_y, method = "pearson")
      my_pearson <- append(my_pearson, r)
    }
    r1 <- c(train_data[[i,1]],my_pearson[[1]],my_pearson[[2]])
    similarities <- rbind(similarities, r1)
  }
  names(similarities) <- c("Readers","NU1","NU2")
  return(similarities)
}


get_k_nearest <- function(i, k_NN){

  newframe1 <- cbind(train_data,similarities[,i+1])
  sortedframe1 <- newframe1[order(newframe1$similarities, na.last = T,decreasing = T),]
  k_NNframe1 <- sortedframe1[1:k_NN,]
  colnames(k_NNframe1)[10]<-"similarities"
  k_NNframe1[,10]<-as.numeric(k_NNframe1[,10])
  k_NNframe1
  return(k_NNframe1)
}



calculate_predictions <- function(i,k_nearest){
  vec1<-c()
  for (n in 1:length(books)) {
    met1<-0
    tmp<-0
    for (l in 1:k_NN) {
      if(!is.na(k_nearest[l,n+1])){
        tmp<-tmp+k_nearest[l,n+1]*k_nearest[l,10]
        met1<-met1+k_nearest[l,10]
      } 
    }
    vec1[n]<-tmp/met1
  }
  return(vec1)
}


spot_the_NAs<-function(i){
  test_data_2<-test_data[,-1]
  vec2<-c()
  for (n in 1:ncol(test_data_2)) {
    if(is.na(test_data_2[i,n])){
      vec2<-append(vec2,n)
    }
  }
  return(vec2)
}



calculate_recommendations<-function(i,need_recommendation,predictions,df){
  if(nrow(df)==0){
    for (n in 1:length(need_recommendation)) {
      df[n,1]<-test_data[i,1]
      df[n,2]<-books[need_recommendation[n]]
      df[n,3]<-predictions[need_recommendation[n]]
    }
  }
  else{
    for (n in 1:length(need_recommendation)) {
      counter<-nrow(df)
      df[counter+1,1]<-test_data[i,1]
      df[counter+1,2]<-books[need_recommendation[n]]
      df[counter+1,3]<-predictions[need_recommendation[n]]
    }
  }
  
  return(df)
}

mean_absolute_error<-function(i,predictions,MAE){
  suma<-0
  x<-0
  for (k in 2:ncol(test_data)) {
    if(!is.na(test_data[i,k])) {
      x<-x+1
      suma<-suma+abs(test_data[i,k]-predictions[k])
    }
  }
  MAE[i]<-suma/x
  return(MAE)
}





similarities <- calculate_similarities()
similarities

  for (i in 1:readers_test) {
    k_nearest <- get_k_nearest(i, k_NN)
    k_nearest
    predictions <- calculate_predictions(i, k_nearest)
    predictions
    need_recommendation <- spot_the_NAs(i)
    need_recommendation
    recommendations <- calculate_recommendations(i,need_recommendation,predictions,recommendations)
    recommendations
    MAE <- mean_absolute_error(i,predictions,MAE)
}


recommendations







