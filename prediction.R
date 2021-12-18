prediction = function(data_input){
  h2o.init(nthreads=1, max_mem_size="4g")
  b<- as.h2o(pre_data)

  y <- "overall_score" 
  x <- setdiff(names(b), y)                
  parts <- h2o.splitFrame(b, 0.8, seed=99) 
  train <- parts[[1]]                        
  test <- parts[[2]]

  m <- h2o.deeplearning(x, y, train)
  p <- h2o.predict(m, test)
  
  pre_performance <- h2o.performance(m, test)
  
  results <- cbind(as.data.frame(test$overall_score),as.data.frame(p))
  
  aml <- h2o.automl(x, y, train
                    , max_runtime_secs = 180     
                    , max_models = 3            
                    , seed = 123               
  )
  lb <- aml@leaderboard
  
  
  lb <- h2o.get_leaderboard(object = aml, extra_columns = 'ALL')
  
  model<-aml@leader
  
  mydata1<-sapply(mydata[,c(4:9,12:61)],as.numeric)
  tdata<-as.h2o(mydata1)
  predict<-as.data.frame(predict(model,tdata))
  rm(aml,b,lb,m,model,p,parts,pre_performance,test)
  return <- cbind(mydata[,1],predict[,1])
  colnames(return) <- c("name","predicted_score")
  return <- as.data.frame(return)
  return[,2] <- as.numeric(return[,2])
  return[,2]<- round(return[,2],2)
  return(return)
}

