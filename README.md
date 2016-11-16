# Project: Words 4 Music

### [Project Description](doc/project4_desc.md)

![image](http://cdn.newsapi.com.au/image/v1/f7131c018870330120dbe4b73bb7695c?width=650)

Term: Fall 2016

+ [Data link](https://courseworks2.columbia.edu/courses/11849/files/folder/Project_Files?preview=763391)-(**coursework login required**)
+ [Data description](doc/readme.html)
+ Contributor's name: Weichuan Wu
+ Projec title: Predict words for music 
+ Project summary: 1.Read .h5 file through a for loop to get the music feature; 2. check the average length of every feature and select the proper length for each feature, and then fix the feature so that the short one is filled and long one is trimmed to the choosen length; 3.When get the standard feature, using every feature to do a cluster and calculate P(word|cluster) for every feature; 4.Lastly,combine all the P(word|cluster),that is take the log of P(word|cluster) and sum them up,and use the probability to do the rank.
	
Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
