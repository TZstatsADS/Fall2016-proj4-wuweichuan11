###clusting

###clear environment
rm(list = ls())

###set directory
setwd('C:/Study/Columbia/W4243_Applied_Data_Science/Project4/data')

###load data
load('bars_confidence.RData')
load('bars_start.RData')
load('beats_confidence.RData')
load('beats_start.RData')
load('sections_confidence.RData')
load('sections_start.RData')
load('tatums_confidence.RData')
load('tatums_start.RData')
load('segments_confidence.RData')
load('segments_loudness_max.RData')
load('segments_loudness_max_time.RData')
load('segments_loudness_start.RData')
load('segments_start.RData')
load('segments_timbre.RData')
load('segments_pitches.RData')
load('lyr.RData')
#take out the numbers
lyr<-lyr[,-c(2,3,6:30)]
#get the word list
col_name<-colnames(lyr)

###load library
library("flexclust")

###set parameters
#set train data and test data
set.seed(1)
test_num<-sample(2350,0.2*2350)

#set cluster number
#choose cluster number according to Github of Spring 2016-Project Group 2,check detail in https://zac2116.github.io/
cluster_num<-5

###preprocess
lyr[["train_col"]] <- TRUE
lyr[["train_col"]][test_num] <- FALSE
lyr_matrix_train<-as.matrix(lyr[lyr[["train_col"]]==TRUE, 2:(ncol(lyr)-1)])
lyr_matrix_test<-as.matrix(lyr[lyr[["train_col"]]==FALSE, 2:(ncol(lyr)-1)])

###main function
#1.using the train data to do the clustering
#2.calculate the word probability given a cluster,that is P(word|cluster) using the train data
#  where P(word|cluster)=(number of songs in the specific clustering and contain the word)/(number of songs in the specific clustering)
#3.output a list contain the cluster model and the word probability matrix
calculate_word_pro_by_cluster<-function(feature,test_num,cluster_num,lyr_matrix_train)
{
  dat <- as.data.frame(feature)
  dat[["train"]] <- TRUE
  dat[["train"]][test_num] <- FALSE
  #to make the result can be repeat,we have to set seed since cluster would vary if not set seed
  set.seed(1)
  cluster_model<-kcca(dat[dat[["train"]]==TRUE, 1:(ncol(dat)-1)], k=cluster_num, kccaFamily("kmeans"))
  pred_train <- predict(cluster_model)
  #pred_test <- predict(cluster_model, newdata=dat[dat[["train"]]==FALSE, 1:(ncol(dat)-1)])
  lyr_matrix_train<-cbind(lyr_matrix_train,pred_train)
  word_probability<-matrix(data=NA,nrow=cluster_num,ncol=ncol(lyr_matrix_train)-1)
  for(cluster in 1:cluster_num)
  {
    lyr_matrix_train_part<-lyr_matrix_train[lyr_matrix_train[,ncol(lyr_matrix_train)]==cluster,1:(ncol(lyr_matrix_train)-1)]
    lyr_matrix_train_part[lyr_matrix_train_part>0]<-1
    #if there is only 1 item in a specific clustering,the nrow would be NULL
    if(is.null(nrow(lyr_matrix_train_part)))
    {
      word_count_in_file<-lyr_matrix_train_part
    }else
    {
      word_count_in_file<-apply(lyr_matrix_train_part,2,sum)
    }
    cluster_size<-cluster_model@clusinfo$size[cluster]
    word_probability[cluster,]<-word_count_in_file/cluster_size
    #if the word never appear in the cluster,assign a small probability 0.0001 to it so that when we use it in multiple
    #the overall probabilty won't be 0 because of one item is 0
    word_probability[word_probability==0]<-0.0001
  }
  result<-list('cluster_model'=cluster_model,'word_probability'=word_probability)
  return(result)
}

###using different features to do clustering
result_list_bars_start<-calculate_word_pro_by_cluster(bars_start,test_num,cluster_num,lyr_matrix_train)
result_list_beats_start<-calculate_word_pro_by_cluster(beats_start,test_num,cluster_num,lyr_matrix_train)
result_list_sections_start<-calculate_word_pro_by_cluster(sections_start,test_num,cluster_num,lyr_matrix_train)
result_list_tatums_start<-calculate_word_pro_by_cluster(tatums_start,test_num,cluster_num,lyr_matrix_train)
result_list_segments_start<-calculate_word_pro_by_cluster(segments_start,test_num,cluster_num,lyr_matrix_train)
result_list_segments_loudness_max<-calculate_word_pro_by_cluster(segments_loudness_max,test_num,cluster_num,lyr_matrix_train)
result_list_segments_loudness_max_time<-calculate_word_pro_by_cluster(segments_loudness_max_time,test_num,cluster_num,lyr_matrix_train)
result_list_segments_loudness_start<-calculate_word_pro_by_cluster(segments_loudness_start,test_num,cluster_num,lyr_matrix_train)
result_list_segments_timbre<-calculate_word_pro_by_cluster(segments_timbre,test_num,cluster_num,lyr_matrix_train)
result_list_segments_pitches<-calculate_word_pro_by_cluster(segments_pitches,test_num,cluster_num,lyr_matrix_train)


###save the word probability matrix
save(result_list_bars_start,file='result_list_bars_start.RData')
save(result_list_beats_start,file='result_list_beats_start.RData')
save(result_list_sections_start,file='result_list_sections_start.RData')
save(result_list_tatums_start,file='result_list_tatums_start.RData')
save(result_list_segments_start,file='result_list_segments_start.RData')
save(result_list_segments_loudness_max,file='result_list_segments_loudness_max.RData')
save(result_list_segments_loudness_start,file='result_list_segments_loudness_start.RData')
save(result_list_segments_loudness_max_time,file='result_list_segments_loudness_max_time.RData')
save(result_list_segments_timbre,file='result_list_segments_timbre.RData')
save(result_list_segments_pitches,file='result_list_segments_pitches.RData')

###load word probability
load('result_list_bars_start.RData')
load('result_list_beats_start.RData')
load('result_list_sections_start.RData')
load('result_list_tatums_start.RData')
load('result_list_segments_start.RData')
load('result_list_segments_loudness_max.RData')
load('result_list_segments_loudness_start.RData')
load('result_list_segments_loudness_max_time.RData')
load('result_list_segments_timbre.RData')
load('result_list_segments_pitches.RData')

###function to calculat the word probability for test data given the feature and cluster model
#output is the log of the probability of word.
calculate_test_data_word_pro<-function(feature,test_num,result_list)
{
  dat <- as.data.frame(feature)
  dat[["train"]] <- TRUE
  dat[["train"]][test_num] <- FALSE
  #to make the result can be repeat,we have to set seed since cluster would vary if not set seed
  set.seed(1)
  pred_test <- predict(result_list$cluster_model, newdata=dat[dat[["train"]]==FALSE, 1:(ncol(dat)-1)])
  word_probability_matrix<-result_list$word_probability
  probability_matrix<-matrix(data=NA,nrow=length(test_num),ncol=ncol(word_probability_matrix))
  for(i in 1:length(pred_test))
  {
    probability_matrix[i,]<-word_probability_matrix[pred_test[i],]
  }
  return(log(probability_matrix))
}


###calculate the log of the probability using different cluster
bars_start_test_data_word_pro<-calculate_test_data_word_pro(bars_start,test_num,result_list_bars_start)
beats_start_test_data_word_pro<-calculate_test_data_word_pro(beats_start,test_num,result_list_beats_start)
sections_start_test_data_word_pro<-calculate_test_data_word_pro(sections_start,test_num,result_list_sections_start)
tatums_start_test_data_word_pro<-calculate_test_data_word_pro(tatums_start,test_num,result_list_tatums_start)
segments_start_test_data_word_pro<-calculate_test_data_word_pro(segments_start,test_num,result_list_segments_start)
segments_loudness_max_test_data_word_pro<-calculate_test_data_word_pro(segments_loudness_max,test_num,result_list_segments_loudness_max)
segments_loudness_start_test_data_word_pro<-calculate_test_data_word_pro(segments_loudness_start,test_num,result_list_segments_loudness_start)
segments_loudness_max_time_test_data_word_pro<-calculate_test_data_word_pro(segments_loudness_max_time,test_num,result_list_segments_loudness_max_time)
segments_timbre_test_data_word_pro<-calculate_test_data_word_pro(segments_timbre,test_num,result_list_segments_timbre)
segments_pitches_test_data_word_pro<-calculate_test_data_word_pro(segments_pitches,test_num,result_list_segments_pitches)

#create a feature list and a array to combine all the probability
feature_name<-c('bars_start','beats_start','sections_start','tatums_start','segments_start',
                'segments_loudness_max','segments_loudness_start','segments_loudness_max_time',
                'segments_timbre','segments_pitches')
word_pro<-array(data=NA,dim=c(10,nrow(bars_start_test_data_word_pro),ncol(bars_start_test_data_word_pro)))
word_pro[1,,]<-bars_start_test_data_word_pro
word_pro[2,,]<-beats_start_test_data_word_pro
word_pro[3,,]<-sections_start_test_data_word_pro
word_pro[4,,]<-tatums_start_test_data_word_pro
word_pro[5,,]<-segments_start_test_data_word_pro
word_pro[6,,]<-segments_loudness_max_test_data_word_pro
word_pro[7,,]<-segments_loudness_start_test_data_word_pro
word_pro[8,,]<-segments_loudness_max_time_test_data_word_pro
word_pro[9,,]<-segments_timbre_test_data_word_pro
word_pro[10,,]<-segments_pitches_test_data_word_pro

###combine the log probability and check which combination has the lowest average rank

### The function take the predicted test work probability and the true lyr_matrix_test to calculate the predictive_rank_sum
calculate_predictive_rank<-function(test_data_word_pro,lyr_matrix_test)
{
  word_rank<-apply(-test_data_word_pro,1,rank)
  rank_mean<-apply(word_rank,2,mean)
  lyr_matrix_test[lyr_matrix_test>0]<-1
  m<-apply(lyr_matrix_test,1,sum)
  sum_rw<-diag(lyr_matrix_test%*%word_rank)
  predictive_rank_sum<-sum_rw/m/rank_mean
  result<-list('predictive_rank_sum'=predictive_rank_sum,'mean'=mean(predictive_rank_sum),'sd'=sd(predictive_rank_sum))
  return(result)
}

###The function take a word_pro array which contain all probability matrix,it also take a variable num_of_matrix
#  which mean how many probability is used,then the function would do loop for all combination and give out the 
#  feature used,the mean and sd of the predictive_rank_sum
generate_probability_matrix<-function(word_pro,num_of_matrix,lyr_matrix_test,feature_name)
{
  predictive_rank_sum_mean<-c()
  predictive_rank_sum_sd<-c()
  feature_used<-c()
  combination_list<-combn(10,num_of_matrix)
  for(j in 1:ncol(combination_list))
  {
    probability_matrix<-matrix(data=0,nrow=dim(word_pro)[2],ncol=dim(word_pro)[3])
    feature_used_temp<-''
    for(i in 1:nrow(combination_list))
    {
      probability_matrix<-probability_matrix+word_pro[combination_list[i,j],,]
      feature_used_temp<-paste(feature_used_temp,feature_name[combination_list[i,j]],sep=' ')
    }
    test_result<-calculate_predictive_rank(probability_matrix,lyr_matrix_test)
    predictive_rank_sum_mean<-c(predictive_rank_sum_mean,test_result$mean)
    predictive_rank_sum_sd<-c(predictive_rank_sum_sd,test_result$sd)
    feature_used<-c(feature_used,feature_used_temp)
  }
  result<-data.frame(feature_used,predictive_rank_sum_mean,predictive_rank_sum_sd)
  #result=list('feature'=feature_used,'mean'=predictive_rank_sum_mean,'sd'=predictive_rank_sum_sd)
  return(result)
}

###calculate the mean and sd of predictive_rank_sum using different number of features
#using single feature,segments_loudness_max_time performs best with mean=0.2559,sd=0.1972
feature_result_1<-generate_probability_matrix(word_pro,1,lyr_matrix_test,feature_name)
#using two features,segments_loudness_max_time and segments_pitches performs best with mean=0.2525,sd=0.1902
feature_result_2<-generate_probability_matrix(word_pro,2,lyr_matrix_test,feature_name)
#using 9 feature, exclude segments_timbre performs best with mean=0.2502,sd=0.1933
feature_result_9<-generate_probability_matrix(word_pro,9,lyr_matrix_test,feature_name)
#using all feature, mean=0.2507,sd=0.1934
feature_result_10<-generate_probability_matrix(word_pro,10,lyr_matrix_test,feature_name)


