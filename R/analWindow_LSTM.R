analWindow_LSTM <- function(DataFile_cutdata,window_N,ID,periodAge=NULL,DataFile_personal,label,predictor=c(),N,batch_size=100,Epoch=10,units=16){

  #input cutdata
  DataFile_cutdata <- as.data.table(DataFile_cutdata)
  ccscategory <- c(ccstable$CCS_CATEGORY %>% unique())
  if(is.null(periodAge)){
    dataCol <- c(deparse(substitute(window_N)),deparse(substitute(ID)),ccscategory)
  }else{
    dataCol <- c(deparse(substitute(window_N)),deparse(substitute(ID)),deparse(substitute(periodAge)),ccscategory)
  }
  cutdata <- DataFile_cutdata[, dataCol,with=FALSE]
  #input personal
  DataFile_personal <- as.data.table(DataFile_personal)
  dataCol <- c(deparse(substitute(ID)),deparse(substitute(label)),predictor)
  personal <- DataFile_personal[, dataCol,with=FALSE]
  personal[,c("SEX"):=lapply(.SD,function(x)ifelse(x=="F",0L,1L)),.SDcols=c("SEX")]
  #將個人資料以及ccs的window資料合併成最後要用的資料表
  maintable <- merge(cutdata,personal[,c("ID",predictor),with=FALSE],by="ID",all.x=T)
  if(is.null(periodAge)){
    maintable <- cbind(maintable[,c("window_N","ID","SEX")],maintable[,-c("window_N","ID","SEX")])
  }else{
    maintable <- cbind(maintable[,c("window_N","ID","SEX","periodAge")],maintable[,-c("window_N","ID","SEX","periodAge")])
  }
  CCSWide_window <- list()

  #合併所有window，並且加上(t-N)
  for(i in 1:N){
    maintable_eachwindow <- maintable[window_N==i,2:length(maintable),]
    oldname <- c(names(maintable_eachwindow[,2:length(maintable_eachwindow),]))
    period <- paste0("(t-",i,")")
    newname <- paste(names(maintable_eachwindow[,2:length(maintable_eachwindow),]),period)
    setnames(maintable_eachwindow,oldname,newname)
    CCSWide_window[[i]] <- as.data.table(maintable_eachwindow)
  }

  #加入label並以label排序
  series <- Reduce(merge,CCSWide_window)
  series <- merge(series, unique(personal[,c("ID","label")]),by="ID",all.x=T)
  class(series)
  series <- as.data.frame(series)
  series <- series[,order(colnames(series),decreasing=TRUE),]#series??data.frame?A?~?|?O?諸
  setDT(series)
  series <- cbind(series[,c("label","ID")],series[,-c("label","ID")])
  series <- series[order(label)]

  ##Split dataset into training and testing sets: 分層抽樣
  ##取train
  #label = T
  dices_T  <- sample(1:nrow(series[label==1,,]))
  folds_T <- cut(dices_T, breaks = 5, label = FALSE)#train test 8:2
  dices_F  <- sample(1:nrow(series[label==0,,]))
  folds_F <- cut(dices_F, breaks = 5, label = FALSE)#train test 8:2
  test_indices_T <- which(folds_T == 1 , arr.ind = TRUE)
  test_T <- series[label==1,,][test_indices_T,,]
  test_indices_F <- which(folds_F == 1 , arr.ind = TRUE)
  test_F <- series[label==0,,][test_indices_F,,]
  train_T <- series[label==1,,][-test_indices_T,,]
  train_F <- series[label==0,,][-test_indices_F,,]
  train <- rbind(train_T,train_F)
  test <- rbind(test_T,test_F)


  #讓放進去的個案最後能被整除
  test_sam <- sample(1:nrow(test))[1:(nrow(test)-nrow(test) %% batch_size)]#為了能整除刪除了nrow(test) %% batch_size筆data
  test <- test[test_sam,,]
  y_test = array(unlist(test[, 1]))
  x_test = test[, 3:ncol(test)]
  x_test <- array(unlist(x_test),dim=c(nrow(test),N,(ncol(test)-2)/N))
  ###Modeling
  X_shape2 = N
  X_shape3 = (ncol(train)-2)/N

  # Epoch = 10
  # batch_size = 100   # must be a common factor of both the train and test samples
  # units = 16   # can adjust this, in model tuninig phase
  loss= 'binary crossentropy' #
  optimizer ="adam"
  metrics ="accuracy"

  #model definition
  build_model <- function(){
    #Initialize model
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3),return_sequences =FALSE, stateful= TRUE,
                 kernel_regularizer = regularizer_l2(0.001))%>%
      #layer_dropout(rate=0.2) %>%
      #layer_lstm(units, return_sequences = FALSE,stateful= TRUE)%>% #statereful?e?@??batchsize???ǦC?O?ЬO?_?Ǩ??U?@??batchsize
      # layer_dropout(rate=0.2) %>%
      layer_dense(16,activation = 'relu') %>%
      layer_dense(1,activation = 'sigmoid')

    # Optimizer
    model %>% compile(
      loss = 'binary_crossentropy',
      optimizer = 'adam',
      metrics= tf$keras$metrics$AUC())
    #metrics='acc'
  }


  ##tune the model
  k <- 5
  set.seed(2)
  sam_T <- sample(1:nrow(train[label==1]))
  train_T <- train[label==1,,][sam_T[1:nrow(train[label==1])-nrow(train[label==1])%% k]]#traindata中，為了可以切分train test，刪除nrow(train[label==1])%% N個size
  dices_T  <- sample(1:nrow(train_T[label==1]))
  folds_T <- cut(dices_T, breaks = 5, label = FALSE)#train test 8:2
  sam_F <- sample(1:nrow(train[label==0]))
  train_F <- train[label==0,,][sam_F[1:(nrow(train[label==0])-nrow(train[label==0])%% k)]]
  dices_F  <- sample(1:nrow(train_F[label==0,,]))
  folds_F <- cut(dices_F, breaks = 5, label = FALSE)#train test 8:2
  folds <- c(folds_T,folds_F)
  newtrain <- rbind(train_T,train_F)

  y_newtrain = array(unlist(newtrain[, 1]))
  x_newtrain = newtrain[, 3:ncol(newtrain)]
  x_newtrain <- array(unlist(x_newtrain),dim=c(nrow(newtrain),N,(ncol(newtrain)-2)/N))#unlist?Nx_train?ܦ??V?q?A?A??array?ܦ??}?C?A?]??array???u?????J?V?q

  all_auc_histories <- NULL
  for(i in 1:k){
    #validation data
    val_indices <- which(folds == i , arr.ind = TRUE)
    val_sam <- sample(1:length(val_indices))[1:(length(val_indices)-length(val_indices) %% batch_size)] #vaildation中為了能整除，刪除了length(val_indices) %% batch_size筆data
    val_data <- x_newtrain[val_indices,,][val_sam,,]#tag 06:03
    val_targets <- y_newtrain[val_indices][val_sam]
    #train data
    partial_indices <- which(folds != i , arr.ind = TRUE)
    pattial_train_sam <- sample(1:length(partial_indices))[1:(length(partial_indices)-length(partial_indices) %% batch_size)] #traindata中為了能整除，刪除了length(partial_indices) %% batch_size筆data
    partial_train_data <- x_newtrain[partial_indices,,][pattial_train_sam,,]
    partial_train_targets <- y_newtrain[partial_indices][pattial_train_sam]

    model <- build_model()
    history <- model %>% fit(
      x = partial_train_data,
      y = partial_train_targets,
      batch_size = batch_size,
      epoch=Epoch,
      verbose = 1,
      shuffle = FALSE,
      validation_data = list(val_data,val_targets)
    )
    #存下所有validation data的auc紀錄
    auc_history <- history$metrics[[4]]
    all_auc_histories <- rbind(all_auc_histories, auc_history)
  }
  ##summary auc
  average_auc_history <- data.table(
    epoch = seq(1:ncol(all_auc_histories)),
    validation_auc = apply(all_auc_histories, 2, mean)
  )

  #training the final model用全部的訓練集，訓練最後的final data
  ##經過validation data驗證後，找出auc最大的一次epoch
  optimalEpoch = average_auc_history[validation_auc == max(average_auc_history$validation_auc),epoch]
  #訊息說明最佳epoch
  msg <- paste("optimal epoch is ",optimalEpoch)
  message(msg)
  Sys.sleep(1)
  ##讓train data可以整除batch size
  train_sam <- sample(1:nrow(newtrain))[1:(nrow(newtrain)-nrow(newtrain) %% batch_size)] #vaildation中為了能整除，刪除了length(val_indices) %% batch_size筆data
  train_data <- x_newtrain[train_sam,,]
  train_targets <- y_newtrain[train_sam]
  ##train
  model <- build_model()
  model %>% fit(
    x = train_data,
    y = train_targets,
    batch_size = batch_size,
    epoch= optimalEpoch ,
    verbose = 1,
    shuffle = FALSE
    #validation_data = list(val_data,val_targets)
  )
  auc <- model %>% evaluate(x_test, y_test, batch_size = batch_size)
  return(list(model=model,x_test=x_test,y_test=y_test,evaluation=auc))
}
