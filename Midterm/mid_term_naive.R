



data=iris

# converting data into matrix 

data=as.matrix()

pred_train=rep(1,100)
pred_test=rep(1,100)

# index of y
y_index=ncol(data)

# for loop to iterate over 50

for(i in c(1:50))
{
  
  set.seed(i)
  sample_size <- floor(0.80 * nrow(data)) 
  train_ind <- sample(seq_len(nrow(data)), size = sample_size)
  training_data <- data[train_ind, ]
  testing_data <- data[-train_ind, ]
  training_x=training_data[,-y_index]
  training_y=training_data[,y_index]
  
  #mean and standard deviation for the 4 parameter 
  
  
  mean_tra <- apply(training_x,2, mean)  
  sd_tra   <- apply(training_x,2, sd)    
  
  
  tr_offsets <- t(t(training_x) - mean_tra)         
  tr_scaled_data  <- t(t(tr_offsets) / sd_tra)
  
  positive_idx = which(training_data[,y_index] == 1)
  
  
  p_data= tr_scaled_data[positive_idx,]
 n_data = tr_scaled_data[-positive_idx,]
  
  
  pos_means=apply(p_data,2,mean)
  pos_sd=apply(p_data,2,sd)
  
  neg_means=apply(n_data,2,mean)
  neg_sd=apply(n_data,2,sd)
  
  test_x=testing_data[,1:y_index-1]
  
  predict_func=function(test_x_row){
    
    target=0;
    
    #Using dbeta() function for beta distribution
    p_pos=sum(log(dbeta(test_x_row,pos_means,pos_sd)))+log(length(positive_idx)/length(training_y))
    p_neg=sum(log(dbeta(test_x_row,neg_means,neg_sd)))+log( 1 - (length(positive_idx)/length(training_y)))
    
    if(p_pos>p_neg){
      target=1
    }
    else{
      target=0
    }  
  }
  

  
  pred_test[i]=length(which((y_pred==target)==TRUE))/length(target)
  test_off <- t(t(test_x) - mean_tra)         
  tst_scaled_data  <- t(t(test_off) / sd_tra)
  y_pred=apply(tst_scaled_data,1,predict_func)
  target=testing_data[,y_index]
  y_pred_train=apply(tr_scaled_data,1,predict_func)
  
  pred_train[i]=length(which((y_pred_train==training_y)==TRUE))/length(training_y)
  
}

# now getting the accuracy for the prediction train


mean(pred_train)*100


# now getting the accuracy for the prediction test 



q <- mean(pred_test)*100
error = 100 - q
