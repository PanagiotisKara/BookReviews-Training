#(B) Dimiourgia sinartisis calculate_similarities()
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

#(D) Na sintaxthei o kodikas tis sinartisis get_k_nearest()
get_k_nearest <- function(i, k_NN){
  
  newframe1 <- cbind(train_data,similarities[,i+1])
  sortedframe1 <- newframe1[order(newframe1$similarities, na.last = T,decreasing = T),]
  k_NNframe1 <- sortedframe1[1:k_NN,]
  colnames(k_NNframe1)[10]<-"similarities"
  k_NNframe1[,10]<-as.numeric(k_NNframe1[,10])
  k_NNframe1
  return(k_NNframe1)
}

#(E) Na sintaxthei o kodikas tis sinartisis calculate_predictions()
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

#(ST) Na sintaxthei o kodikas tis sinartisis spot_the_NAs
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
 
#(Z) Na sintaxthei o kodikas tis sinartisis calculate_recommendations()
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

#(H) Na sintaxthei o kodikas tis sinartisis mean_absolute_error()
#den boresa na vrw to provlhma, opws kai na to ulopoihousa mou epestrefe NA NA
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