library(dplyr)
library(readr)
library(stringr)
library(data.table)
library(ggplot2)
first_category_name = list.files("Desktop/textbook/QCM_Sensor_Alcohol_Dataset") 
dir = paste("./Desktop/textbook/QCM_Sensor_Alcohol_Dataset/",first_category_name,sep="")
##1.Preprocessing
for(i in 1:length(dir)){
  index = str_sub(first_category_name[i],1,-5)
  data_index = data.frame(read_csv(dir[i]))
  print(names(data_index))
  names(data_index) = c("ch1p0.8", "ch2p0.8", "ch1p0.7","ch2p0.7","ch1p0.6","ch2p0.6","ch1p0.5","ch2p0.5","ch1p0.4","ch2p0.4","Oct1","Pro1","But2","pro2","iso1")
  data_index <- mutate(data_index,labels = Oct1*1+Pro1*2+But2*3+pro2*4+iso1*5) %>% select(-(Oct1:iso1))
  assign(index, data_index)
}

##2.EDA
#same channel, diff ratio
diffratio = group_by(QCM10,labels) %>% summarize(m0.8=mean(ch1p0.8), m0.7=mean(ch1p0.7), m0.6=mean(ch1p0.6), m0.5=mean(ch1p0.5), m0.4=mean(ch1p0.4))
diffratio["labels"] = c("Oct1","Pro1","But2","Pro2","Iso1")
ggplot(diffratio)+
  geom_point(aes(x=labels,y=m0.8,fill="m0.8"),size=1.5,shape=21,color="black")+ 
  geom_point(aes(x=labels,y=m0.7,fill="m0.7"),size=1.5,shape=21)+
  geom_point(aes(x=labels,y=m0.6,fill="m0.6"),size=1.5,shape=21)+
  geom_point(aes(x=labels,y=m0.5,fill="m0.5"),size=1.5,shape=21)+
  geom_point(aes(x=labels,y=m0.4,fill="m0.4"),size=1.5,shape=21)+
  labs(x="Type of Alcohol",y="Channel_1",title= "Comparsion of different ratios",fill="")+
  theme(plot.title = element_text(hjust = 0.5)) 
#For the channel 1, two types of alcohol But2 and Iso1 have similar measurement for different alcohol/air ratio ranging from 0.4 to 0.8.

#same material, diff channel & ratio
diffchannel = group_by(QCM10,labels) %>% summarize(m1p0.8=mean(ch1p0.8), m2p0.8=mean(ch2p0.8), m1p0.6=mean(ch1p0.6), m2p0.6=mean(ch2p0.6), m1p0.4=mean(ch1p0.4), m2p0.4=mean(ch2p0.4))
diffchannel["labels"] = c("Oct1","Pro1","But2","Pro2","Iso1")
ggplot(diffchannel)+
  geom_point(aes(x=labels,y=m1p0.8,fill="m1p0.8"),size=1,shape=21,color="black")+ 
  geom_point(aes(x=labels,y=m2p0.8,fill="m2p0.8"),size=1,shape=21)+
  geom_point(aes(x=labels,y=m1p0.6,fill="m1p0.6"),size=1,shape=21)+
  geom_point(aes(x=labels,y=m2p0.6,fill="m2p0.6"),size=1,shape=21)+
  geom_point(aes(x=labels,y=m1p0.4,fill="m1p0.4"),size=1,shape=21)+
  geom_point(aes(x=labels,y=m2p0.4,fill="m2p0.4"),size=1,shape=21)+
  labs(x="Type of Alcohol",y="Measurement",title= "Comparison of two channels",fill="")+
  theme(plot.title = element_text(hjust = 0.5)) 
#The difference between But2 and Iso1 in channel 2 is larger than channel 1 (such as m2p0.4) 

#diff material, same channel & ratio
qcm10ch1 = group_by(QCM10,labels) %>% summarize(m0.8=mean(ch1p0.8), m0.6=mean(ch1p0.6), m0.4=mean(ch1p0.4)) %>% select(-(labels))
qcm3ch1 = group_by(QCM3,labels) %>% summarize(m0.8=mean(ch1p0.8), m0.6=mean(ch1p0.6), m0.4=mean(ch1p0.4))
qcm10_3ch1 = mutate(data.frame(qcm10ch1), qcm3m0.8 = data.frame(qcm3ch1)[,2], qcm3m0.6 = data.frame(qcm3ch1)[,3], qcm3m0.4 = data.frame(qcm3ch1)[,4])
qcm10_3ch1["labels"] = c("Oct1","Pro1","But2","Pro2","Iso1")
ggplot(qcm10_3ch1)+
  geom_point(aes(x=labels,y=m0.8,fill="qcm10_0.8"),size=1,shape=21,color="black")+ 
  geom_point(aes(x=labels,y=m0.6,fill="qcm10_0.6"),size=1,shape=21)+
  geom_point(aes(x=labels,y=m0.4,fill="qcm10_0.4"),size=1,shape=21)+
  geom_point(aes(x=labels,y=qcm3m0.8,fill="qcm3_0.8"),size=1,shape=21)+
  geom_point(aes(x=labels,y=qcm3m0.6,fill="qcm3_0.6"),size=1,shape=21)+
  geom_point(aes(x=labels,y=qcm3m0.4,fill="qcm3_0.4"),size=1,shape=21)+
  labs(x="Type of Alcohol",y="Measurement",title= "Comparison of two Detector",fill="")+
  theme(plot.title = element_text(hjust = 0.5)) 
#For the same alcohol/air ratio and channel, difference between QCM10 and QCM10 is obvious.

#3. Model
#3.1 For every detector, we fit a random forest model
library(randomForest)
acc_vec = rep(0,5)
repeat_times = 10
acc_vec_all = matrix(rep(0,repeat_times*5),ncol=5)
for(j in 1:repeat_times){
  for(i in 1:length(dir)){
    index = str_sub(first_category_name[i],1,-5)
    data_index = get(index)
    data_index$labels = as.factor(data_index$labels)
    train_sub = sample(nrow(data_index),7/10*nrow(data_index))
    train_data = data_index[train_sub,]
    test_data = data_index[-train_sub,]
    model <- randomForest(labels~., data=train_data, importance=TRUE,proximity=TRUE)
    #print(model$importance)
    #varImpPlot(model, main = "variable importance")
    test_hat_label <- predict(model,newdata=test_data)
    table(test_data$labels,test_hat_label,dnn=c("Truth","Predict"))
    acc = sum(test_data$labels==test_hat_label)/nrow(test_data)
    acc_vec[i] = acc
    }
  acc_vec_all[j,1:5] = acc_vec
}
sapply(data.frame(acc_vec_all), mean, na.rm = T)  
sapply(data.frame(acc_vec_all), var, na.rm = T)  

#3.2 We only want a big model to process data from five detectors
QCM_all = rbind(QCM3,QCM6,QCM7,QCM10,QCM12)
QCM_all$labels = as.factor(QCM_all$labels)
repeat_times = 10
acc_vec_all = rep(0,repeat_times)
for(j in 1:repeat_times){
  set.seed(1234)
  train_sub = sample(nrow(QCM_all),7/10*nrow(QCM_all))
  train_data = QCM_all[train_sub,]
  test_data = QCM_all[-train_sub,]
  model <- randomForest(labels~., data=train_data, importance=TRUE,proximity=TRUE)
  #print(model$importance)
  #varImpPlot(model, main = "variable importance")
  test_hat_label <- predict(model,newdata=test_data)
  table(test_data$labels,test_hat_label,dnn=c("Truth","Predict"))
  acc_vec_all[j] = sum(test_data$labels==test_hat_label)/nrow(test_data)
  print(acc_vec_all[j])
  if(acc_vec_all[j]!=1){
    false_point = test_data[test_data$labels!=test_hat_label,]
    min_dist = 10000000
    min_point = rep(0,ncol(train_data))
    for(i in 1:nrow(false_point)){
      for(j in 1:nrow(train_data)){
        now_dist = sum((false_point[i,1:10]-train_data[j,1:10])^2)
        if(now_dist < min_dist){
          min_dist = now_dist
          min_point = train_data[j,]
        }
      }
      print(rbind(false_point[i,],min_point))
    }
    break
  }
}
print(mean(acc_vec_all))
print(var(acc_vec_all))
#[1] 0.9763158
#[1] 0.0009926131

#3.3 Fortunately, we find some information related to the ratio of component in two channals of different QCM detectors.
#One of these channel includes molecularly imprinted polymers (MIP), and the other includes nanoparticles (NP).
#Add new columns MIP_ratio and NP_ratio into five QCM dataframes:
QCM3_more = mutate(QCM3,MIP_ratio = rep(1,25), NP_ratio=rep(1,25))
QCM6_more = mutate(QCM3,MIP_ratio = rep(1,25), NP_ratio=rep(0,25))
QCM7_more = mutate(QCM3,MIP_ratio = rep(1,25), NP_ratio=rep(0.5,25))
QCM10_more = mutate(QCM3,MIP_ratio = rep(1,25), NP_ratio=rep(2,25))
QCM12_more = mutate(QCM3,MIP_ratio = rep(0,25), NP_ratio=rep(1,25))
QCM_all_more = rbind(QCM3_more,QCM6_more,QCM7_more,QCM10_more,QCM12_more)
QCM_all_more$labels = as.factor(QCM_all_more$labels)
repeat_times = 10
acc_vec_all = rep(0,repeat_times)
for(j in 1:repeat_times){
  set.seed(1234)
  train_sub = sample(nrow(QCM_all_more),7/10*nrow(QCM_all_more))
  train_data = QCM_all_more[train_sub,]
  test_data = QCM_all_more[-train_sub,]
  model <- randomForest(labels~., data=train_data, importance=TRUE,proximity=TRUE)
  #print(model$importance)
  #varImpPlot(model, main = "variable importance")
  test_hat_label <- predict(model,newdata=test_data)
  table(test_data$labels,test_hat_label,dnn=c("Truth","Predict"))
  acc_vec_all[j] = sum(test_data$labels==test_hat_label)/nrow(test_data)
  print(acc_vec_all[j])
  if(acc_vec_all[j]!=1){
    print(test_data[test_data$labels!=test_hat_label,])
    break
  }
}
print(mean(acc_vec_all))
print(var(acc_vec_all))
#[1] 1
#[1] 0