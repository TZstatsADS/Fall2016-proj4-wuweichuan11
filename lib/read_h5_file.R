###This code is used for read h5 files and process the feature so that it can be used for clusting
###clear environment
rm(list = ls())

###load library
library(rhdf5)

###set directory
path_first<-'C:/Study/Columbia/W4243_Applied_Data_Science/Project4/data'
setwd(path_first)
###get the img list
folder_list_first <- dir()

###use 15 matrix to store all different features
count<-1
row_num<-2350
bars_length<-200
bars_confidence<-matrix(data=NA,nrow=row_num,ncol=bars_length)
bars_start<-matrix(data=NA,nrow=row_num,ncol=bars_length)
beats_length<-600
beats_confidence<-matrix(data=NA,nrow=row_num,ncol=beats_length)
beats_start<-matrix(data=NA,nrow=row_num,ncol=beats_length)
sections_length<-15
sections_confidence<-matrix(data=NA,nrow=row_num,ncol=sections_length)
sections_start<-matrix(data=NA,nrow=row_num,ncol=sections_length)
tatums_length<-1300
tatums_confidence<-matrix(data=NA,nrow=row_num,ncol=tatums_length)
tatums_start<-matrix(data=NA,nrow=row_num,ncol=tatums_length)
segments_length<-1000
segments_confidence<-matrix(data=NA,nrow=row_num,ncol=segments_length)
segments_loudness_max<-matrix(data=NA,nrow=row_num,ncol=segments_length)
segments_loudness_max_time<-matrix(data=NA,nrow=row_num,ncol=segments_length)
segments_loudness_start<-matrix(data=NA,nrow=row_num,ncol=segments_length)
segments_start<-matrix(data=NA,nrow=row_num,ncol=segments_length)
segments_long_length<-12000
segments_timbre<-matrix(data=NA,nrow=row_num,ncol=segments_long_length)
segments_pitches<-matrix(data=NA,nrow=row_num,ncol=segments_long_length)

###get the feature average length
#bars_start_length<-matrix(data=NA,ncol=2350)
#beats_start_length<-matrix(data=NA,ncol=2350)
#sections_start_length<-matrix(data=NA,ncol=2350)
#tatums_start_length<-matrix(data=NA,ncol=2350)
#segments_start_length<-matrix(data=NA,ncol=2350)
#segments_timbre_length<-matrix(data=NA,ncol=2350)
#segments_pitches_length<-matrix(data=NA,ncol=2350)

###save the length data
#save(bars_start_length,file='bars_start_length.RData')
#save(beats_start_length,file='beats_start_length.RData')
#save(sections_start_length,file='sections_start_length.RData')
#save(tatums_start_length,file='tatums_start_length.RData')
#save(segments_start_length,file='segments_start_length.RData')
#save(segments_pitches_length,file='segments_pitches_length.RData')

###get the detail of length,select the proper length
#quantile(bars_start_length,probs = seq(0, 1, 0.05))
#quantile(beats_start_length,probs = seq(0, 1, 0.05))
#quantile(sections_start_length,probs = seq(0, 1, 0.05))
#quantile(tatums_start_length,probs = seq(0, 1, 0.05))
#quantile(segments_start_length,probs = seq(0, 1, 0.05))
#quantile(segments_pitches_length,probs = seq(0, 1, 0.05))
#quantile(segments_timbre_length,probs = seq(0, 1, 0.05))

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

###the main for loop 
for(i in folder_list_first)
{
  path_second<-paste(path_first,'/',i,sep='')
  folder_list_second<-list.files(path_second)
  for(j in folder_list_second)
  {
    path_third<-paste(path_second,'/',j,sep='')
    folder_list_third<-list.files(path_third)
    for(k in folder_list_third)
    {
      path_forth<-paste(path_third,'/',k,sep='')
      folder_list_forth<-list.files(path_forth)
      for(m in folder_list_forth )
      {
        music_feature<-h5read(paste(path_forth,'/',m,sep=''),'/analysis')
        
        #start time of each bar according to The Echo Nest and its confidence
        bars_confidence[count,]<-process_feature(music_feature$bars_confidence,bars_length)
        bars_start[count,]<-process_feature(music_feature$bars_start,bars_length)
        #bars_start_length[count]<-length(music_feature$bars_start)
        
        #start time of each beat according to The Echo Nest and its confidence
        beats_confidence[count,]<-process_feature(music_feature$beats_confidence,beats_length)
        beats_start[count,]<-process_feature(music_feature$beats_start,beats_length)
        #beats_start_length[count]<-length(music_feature$beats_start)
        
        #start time of each section according to The Echo Nest and its confidence
        sections_confidence[count,]<-process_feature(music_feature$sections_confidence,sections_length)
        sections_start[count,]<-process_feature(music_feature$sections_start,sections_length)
        #sections_start_length[count]<-length(music_feature$sections_start)
        
        #start time of each tatum according to The Echo Nest and its confidence
        tatums_confidence[count,]<-process_feature(music_feature$tatums_confidence,tatums_length)
        tatums_start[count,]<-process_feature(music_feature$tatums_start,tatums_length)
        #tatums_start_length[count]<-length(music_feature$tatums_start)
        
        #start time of each segment (~ musical event, or onset) according to The Echo Nest and its confidence
        segments_confidence[count,]<-process_feature(music_feature$segments_confidence,segments_length)
        segments_start[count,]<-process_feature(music_feature$segments_start,segments_length)
        
        #loudness at the beginning of each segment
        segments_loudness_start[count,]<-process_feature(music_feature$segments_loudness_start,segments_length)
        
        #max loudness during each segment and its time
        segments_loudness_max[count,]<-process_feature(music_feature$segments_loudness_max,segments_length)
        segments_loudness_max_time[count,]<-process_feature(music_feature$segments_loudness_max_time,segments_length)
        
        #start time of each segment (~ musical event, or onset) according to The Echo Nest
        segments_start[count,]<-process_feature(music_feature$segments_start,segments_length)
        #segments_start_length[count]<-length(music_feature$segments_start)
        
        #chroma features for each segment (normalized so max is 1.)
        segments_pitches[count,]<-process_feature(music_feature$segments_pitches,segments_long_length)
        #segments_pitches_length[count]<-length(music_feature$segments_pitches)
        
        #MFCC-like features for each segment
        segments_timbre[count,]<-process_feature(music_feature$segments_timbre,segments_long_length)
        #segments_timbre_length[count]<-length(music_feature$segments_timbre)
        
        #print the process
        print(paste(as.character(count/row_num*100),'%',' completed',sep=''))
        count<-count+1
      }
    }
  }
}

###save data
save(bars_confidence,file='bars_confidence.RData')
save(bars_start,file='bars_start.RData')
save(beats_confidence,file='beats_confidence.RData')
save(beats_start,file='beats_start.RData')
save(sections_confidence,file='sections_confidence.RData')
save(sections_start,file='sections_start.RData')
save(tatums_confidence,file='tatums_confidence.RData')
save(tatums_start,file='tatums_start.RData')
save(segments_confidence,file='segments_confidence.RData')
save(segments_loudness_max,file='segments_loudness_max.RData')
save(segments_loudness_max_time,file='segments_loudness_max_time.RData')
save(segments_loudness_start,file='segments_loudness_start.RData')
save(segments_start,file='segments_start.RData')
save(segments_timbre,file='segments_timbre.RData')
save(segments_pitches,file='segments_pitches.RData')




