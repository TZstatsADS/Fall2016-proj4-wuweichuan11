#This file is used to process the test feature and then apply the cluster model to it
###clear environment
rm(list = ls())

###load library
library(rhdf5)

###set parameters
bars_length<-200
beats_length<-600
sections_length<-15
tatums_length<-1300
segments_length<-1000
segments_long_length<-12000

###load .h5 file
file_path<-'C:/Study/Columbia/W4243_Applied_Data_Science/Project4/data/A/A/A'
setwd(file_path)
file_list<-dir()

#set matrix
bars_start<-matrix(data=NA,nrow=length(file_list),ncol=bars_length)
beats_start<-matrix(data=NA,nrow=length(file_list),ncol=beats_length)
sections_start<-matrix(data=NA,nrow=length(file_list),ncol=sections_length)
tatums_start<-matrix(data=NA,nrow=length(file_list),ncol=tatums_length)
segments_loudness_max<-matrix(data=NA,nrow=length(file_list),ncol=segments_length)
segments_loudness_max_time<-matrix(data=NA,nrow=length(file_list),ncol=segments_length)
segments_loudness_start<-matrix(data=NA,nrow=length(file_list),ncol=segments_length)
segments_start<-matrix(data=NA,nrow=length(file_list),ncol=segments_length)
segments_timbre<-matrix(data=NA,nrow=length(file_list),ncol=segments_long_length)
segments_pitches<-matrix(data=NA,nrow=length(file_list),ncol=segments_long_length)

###The function will process the feature in such a way that if the feature length is larger then feature_length
###then discrad the left part,and if the feature is shorted than the feature_length,than it will append its member
###by propertion.
process_feature<-function(feature_list,feature_length)
{
  if(length(feature_list)==0)
  {
    result<-rep(0,feature_length)
    return(result)
  }
  if(length(feature_list)>=feature_length)
  {
    result<-feature_list[1:feature_length]
  }
  else
  {
    multiple_num<-floor(feature_length/length(feature_list))
    left_num<-feature_length-multiple_num*length(feature_list)
    if(left_num ==0)
    {
      result<-rep(feature_list,each=multiple_num)
    }
    else
    {
      result<-c(rep(feature_list,each=multiple_num),feature_list[(length(feature_list)-left_num+1):length(feature_list)])
    }
  }
  return(result)
}

#read file
count<-1
for(file in file_list)
{
  music_feature<-h5read(file,'/analysis')
  #start time of each bar according to The Echo Nest 
  bars_start[count,]<-process_feature(music_feature$bars_start,bars_length)

  #start time of each beat according to The Echo Nest 
  beats_start[count,]<-process_feature(music_feature$beats_start,beats_length)
  
  #start time of each section according to The Echo Nest 
  sections_start[count,]<-process_feature(music_feature$sections_start,sections_length)
  
  #start time of each tatum according to The Echo Nest 
  tatums_start[count,]<-process_feature(music_feature$tatums_start,tatums_length)
  
  #start time of each segment (~ musical event, or onset) according to The Echo Nest 
  segments_start[count,]<-process_feature(music_feature$segments_start,segments_length)
  
  #loudness at the beginning of each segment
  segments_loudness_start[count,]<-process_feature(music_feature$segments_loudness_start,segments_length)
  
  #max loudness during each segment and its time
  segments_loudness_max[count,]<-process_feature(music_feature$segments_loudness_max,segments_length)
  segments_loudness_max_time[count,]<-process_feature(music_feature$segments_loudness_max_time,segments_length)
  
  #start time of each segment (~ musical event, or onset) according to The Echo Nest
  segments_start[count,]<-process_feature(music_feature$segments_start,segments_length)
  
  #chroma features for each segment (normalized so max is 1.)
  segments_pitches[count,]<-process_feature(music_feature$segments_pitches,segments_long_length)
  
  #MFCC-like features for each segment
  segments_timbre[count,]<-process_feature(music_feature$segments_timbre,segments_long_length)
  
  #print the process
  print(paste(as.character(count/length(file_list)*100),'%',' completed',sep=''))
  count<-count+1
}

###set path
setwd('C:/Study/Columbia/W4243_Applied_Data_Science/Project4/Fall2016-proj4-wuweichuan11/data')

###load data
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
calculate_test_data_word_pro<-function(feature,result_list)
{
  dat <- as.data.frame(feature)
  #to make the result can be repeat,we have to set seed since cluster would vary if not set seed
  set.seed(1)
  pred_test <- predict(result_list$cluster_model, newdata=dat)
  word_probability_matrix<-result_list$word_probability
  probability_matrix<-matrix(data=NA,nrow=length(pred_test),ncol=ncol(word_probability_matrix))
  for(i in 1:length(pred_test))
  {
    probability_matrix[i,]<-word_probability_matrix[pred_test[i],]
  }
  return(log(probability_matrix))
}

###calculate the log of the probability using different cluster
bars_start_test_data_word_pro<-calculate_test_data_word_pro(bars_start,result_list_bars_start)
beats_start_test_data_word_pro<-calculate_test_data_word_pro(beats_start,result_list_beats_start)
sections_start_test_data_word_pro<-calculate_test_data_word_pro(sections_start,result_list_sections_start)
tatums_start_test_data_word_pro<-calculate_test_data_word_pro(tatums_start,result_list_tatums_start)
segments_start_test_data_word_pro<-calculate_test_data_word_pro(segments_start,result_list_segments_start)
segments_loudness_max_test_data_word_pro<-calculate_test_data_word_pro(segments_loudness_max,result_list_segments_loudness_max)
segments_loudness_start_test_data_word_pro<-calculate_test_data_word_pro(segments_loudness_start,result_list_segments_loudness_start)
segments_loudness_max_time_test_data_word_pro<-calculate_test_data_word_pro(segments_loudness_max_time,result_list_segments_loudness_max_time)
segments_timbre_test_data_word_pro<-calculate_test_data_word_pro(segments_timbre,result_list_segments_timbre)
segments_pitches_test_data_word_pro<-calculate_test_data_word_pro(segments_pitches,result_list_segments_pitches)

#use all 10 probability matrix
all_matrix<-bars_start_test_data_word_pro+beats_start_test_data_word_pro+sections_start_test_data_word_pro+
            tatums_start_test_data_word_pro+segments_start_test_data_word_pro+segments_loudness_max_test_data_word_pro+
            segments_loudness_start_test_data_word_pro+segments_loudness_max_time_test_data_word_pro+
            segments_timbre_test_data_word_pro+segments_pitches_test_data_word_pro

#calculate rank
word_rank<-round(apply(-all_matrix,1,rank))
  

