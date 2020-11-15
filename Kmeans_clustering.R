############## ISM 645/IAF 601 Predictive Analytics ##########################
#=============================================================================
#============ Data Exploration and Visualization ===============
#=============================================================================

######### Due: Oct 19, 11:59pm


library(tidyverse)


## (1) Import "MSA_M2019_dl.xlsx" and inspect the data.

library(openxlsx)

df <- read.xlsx('MSA_M2019_dl.xlsx')

# summary data
summary(df)

# structure of data

str(df)

if (any(!is.numeric(df))) {
  print(paste0('There is no any numeric in the raw data'))
}

head(df,2)



## (2) Filter the observations of which o_group is "major".

df <- df %>%
  filter (o_group=='major')


## (3) Select area_title, occ_title, jobs_1000 (employment per 1,000 people).

df <-df %>% 
  select(area_title, occ_title,jobs_1000)


## (4) Convert  the data type of jobs_1000 as numeric
##     Hint: Use as.numeric()

class(df$jobs_1000)

df$jobs_1000 <- sapply(df$jobs_1000,as.numeric)

class(df$jobs_1000)



## (5) Make each occupation as each variable (a city is the unit of observation).
##     Hint: Spread the occupation titles into different columns that contain values of jobs_1000.

df <- df %>% 
      spread(occ_title,value = jobs_1000)



## (6) area_title consists of city name and state name (Greensboro-High Point, NC).
##     Split area_title into city and state, based on ",".
##     Apply str_trim() into city and state variables to remove any white spaces.


df <-df %>%
  separate(col = area_title,into = c('city','state'),sep = ',') 


df$state <-str_trim(df$state,side = c("both", "left", "right")) 
df$city <- str_trim(df$city,side = c("both", "left", "right"))


  

## (7) Cluster the U.S. cities based on the occupational employment composition. You determine the number of clusters.
##     If needed, drop all observations with missing values.

if (any(is.na(df))) {
  print(paste("There are Na's in data, kmeans does not accept it"))
  
} 
if(! any(is.na(df))){
  print(paste('The data is ready to go'))
}

df <- df %>%
  drop_na()

if (any(is.na(df))) {
  print(paste("There are Na's in data, kmeans does not accept it"))
  
} 
if(! any(is.na(df))){
  print(paste('The data is ready to go!'))
}

kmeans_df <-df %>% 
  select(-city,-state)

print(paste0('Appling Kmeans Clustering Model'))

library(purrr)


#  map_dbl to run many models with varying value of k (centers)
wss <-map_dbl(1:10,function(k){
  model <- kmeans(x=kmeans_df,centers = k,nstart = 10)
  model$tot.withinss
})

# Generate a data frame containing both k and tot_withinss
elbo_df <-data.frame(
  k=1:10,
  wss=wss
)

ggplot(elbo_df,aes(x=k,y=wss))+
  geom_line()+
  scale_x_continuous(breaks = seq(0,11,1))

library(factoextra)

fviz_nbclust(kmeans_df,kmeans,method = 'wss')

fviz_nbclust(kmeans_df,kmeans,method = 'silhouette')

print(paste0('Applinig Kmenas with the k=5 after finding with Silhoutte method'))



############ KMeans ################

model <-kmeans(kmeans_df,centers = 5,iter.max = 25,nstart = 10)

df <- df %>%
  mutate(Cluster=model$cluster)



## (8) Create a scatter plot with Production Occupations on the x axis and Management Occupations on the y axis.
##     Color the observations based on the clusters revealed in the previous question.
##     Hint: When you specify a variable name with more than one word, it should be enclosed with ` ` (not ' ') (e.g., `Management Occupations`).


pl <- ggplot(df,aes(x=`Production Occupations`,y=`Management Occupations`,color=factor(Cluster)))+
  geom_point(na.rm=T)+
  labs(color='Cluster')

pl
  
  
  


## (9) Add title to the plot and data label (city names) only to NC cities (the state is either "NC" or "NC-SC").
##     Note that visual details will not be accounted for grading.

library(ggrepel)
pl +
  labs(title = 'Kmeans Clustering')+
  geom_text_repel(aes(label=city),force=10,data = subset(df, state=='NC' | state=='NC-SC'))


  


## (10) Investigate other occupations besides management and production. 
##      Briefly discuss the NC cities' occupational structures below (Must use the comments).
  
NC_SC <- df %>% filter(state=='NC' | state=='NC-SC') %>%
   na.omit()  


gathered_NC_SC <-gather(NC_SC,key='occupation',value=n_jobs,-state,-city,-Cluster)

head(gathered_NC_SC,10)
  
ggplot(gathered_NC_SC,aes(x=factor(Cluster),y=n_jobs))+
  geom_boxplot(aes(fill=factor(Cluster)))+
  scale_fill_brewer(palette = 'Set1')+
  xlab('Clusters')+
  ylab('Number of Jobs')+
  theme(legend.title = element_blank())


print(paste0('There are --> ',df %>% n_distinct(), ' distinct jobs in whole states and cities'))
print(paste0('From all the occupations,there are --> ',gathered_NC_SC %>% n_distinct(), ' distinct jobs just in NC-SC'))

print(paste0('The top 5 cities per cluster '))

gathered_NC_SC %>%
  group_by(Cluster) %>% 
  arrange(desc(n_jobs)) %>%
  head(5) %>% 
  select(occupation,city,n_jobs)
  
  

ggplot(gathered_NC_SC,aes(y=occupation,x=n_jobs,fill=factor(Cluster)))+
  geom_bar(position = 'dodge',stat='identity',width = .9)+
  scale_fill_discrete(name='Cluster')+
  theme_minimal()+
  scale_color_brewer(palette = 'Set3')+
  xlab('Number of Jobs Per Cluster')+
  scale_x_continuous(breaks = seq(0,200,15))


ggplot(gathered_NC_SC,aes(y=city,x=n_jobs,fill=factor(Cluster)))+
  geom_bar(position = 'dodge',stat='identity',width = .9)+
  scale_fill_discrete(name='Cluster')+
  theme_minimal()+
  scale_color_brewer(palette = 'Set3')+
  xlab('Number of Jobs Per Cluster')+
  scale_x_continuous(breaks = seq(0,200,15))


  
  
  
  



#########################################################################################
# As it is shown in box plot in NC-SC the number of jobs are normaly distributed and the average
#Medians are mostly equaly likely.
#Also it has show that there is no any cluster number 1 in NC-SC.
#
# As calculated in above from all the occupations in the US there are 292 number of distinct jobs and there are 
# 22 distinc jobs just in NC-SC
#
#Among the whole cities and states.The top five jobs in NC-SC determined that Greensboro, Feyettevile, Ashville, Raleigh ,and Charlotte have the most
# number of jobs.
#
#Note that among the occupations in NC-SC `Office and administrative support occupation`has the most
# number of jobs and fishing, farming has the least occupation of the jopbs. Also, it has found 22 occupation titile joba with 4 cluster in NC-SC which is an exception of 
# the cluster number 1. Further, it has found that Ashvile and Greensboro has the most occupation number of jobs in it. 
# ###
###### NOTE:
# In adition, the graph suggests a uniform distriburiont among the top cities, in which each city class has about the same number of jobs (almost) in it. 




  

