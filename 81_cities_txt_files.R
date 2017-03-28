
#install.packages("ggplot2", lib="/Users/jimmy/Dropbox/Rpackages/")
library(ggplot2, lib.loc="/Users/jimmy/Dropbox/Rpackages/")

eighty_one_cities = read.delim("/Users/jimmy/Dropbox/data_viz/81_cities/tw81cities.txt")


eighty_one_cities$city_index <- 0


eighty_one_cities$city_NO <- 0
for (index in 1:nrow(eighty_one_cities)){
    eighty_one_cities$city_NO[index]=index
}



eighty_one_cities$tw_per_person <- 0.0


#eighty_one_cities$tw_per_person <- 0
#for (index in 1:nrow(eighty_one_cities)){
#    if (eighty_one_cities$weight[index]> 24){
#       eighty_one_cities$size[index]='super_city'
#  }
#}
eighty_one_cities$tw_per_person_category <- 0.0

for (index in 1:nrow(eighty_one_cities)){
    eighty_one_cities$tw_per_person[index]= eighty_one_cities$tweet_count_total[index]/eighty_one_cities$population[index]}
eighty_one_cities
min(eighty_one_cities$tw_per_person)
max(eighty_one_cities$tw_per_person)
median(eighty_one_cities$tw_per_person)
t=quantile(eighty_one_cities$tw_per_person)
#summary(eighty_one_cities$tw_per_person)

eighty_one_cities$tw_per_person_category <- 0.0

for (index in 1:nrow(eighty_one_cities)){
    if(eighty_one_cities$tw_per_person[index] <= t[[2]]){
        eighty_one_cities$tw_per_person_category[index]='0low'
    }
    else if (eighty_one_cities$tw_per_person[index] <= t[[3]]){
        eighty_one_cities$tw_per_person_category[index]='1medium'
    }
   else if ( eighty_one_cities$tw_per_person[index] <= t[[4]]){
        eighty_one_cities$tw_per_person_category[index]='2high'
    }
   else{
       eighty_one_cities$tw_per_person_category[index]='3ultra'
   }
}





kk=ggplot(eighty_one_cities, aes(x= city_index, y= tw_per_person,color= tw_per_person)) + geom_point(size=eighty_one_cities$population/4000000,alpha=0.7)+ facet_wrap(~ continent)+  scale_colour_gradientn(colours = c("red","yellow","green","lightblue","darkblue"),values=c(1.0,0.8,0.6,0.4,0.2,0),guide_legend(title ="twiter per person"))+labs(title="twiter per person for the countries in different continent ", y="twiter per person", x="")+ theme(legend.position = c(.7, .13),panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank())
ggsave("/Users/jimmy/Dropbox/data_viz/81_cities/tw_perperson1.png", kk)


kk2=ggplot(eighty_one_cities, aes(x= city_NO, y= tw_per_person,color= tw_per_person)) + geom_point(size=eighty_one_cities$population/4000000,alpha=0.7)+ facet_wrap(~ continent)+  scale_colour_gradientn(colours = c("red","yellow","green","lightblue","darkblue"),values=c(1.0,0.8,0.6,0.4,0.2,0),guide_legend(title ="twiter per person"))+labs(title="twiter per person for the countries in different continent ", y="twiter per person", x="city index")+ theme(legend.position = c(.7, .13),panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank())

ggsave("/Users/jimmy/Dropbox/data_viz/81_cities/tw_perperson2.png", kk2)


kk3=ggplot(eighty_one_cities, aes(x= city_NO, y= tw_per_person,color= tw_per_person)) + geom_point(size=eighty_one_cities$population/4000000,alpha=0.7)+ facet_wrap(~ dev)+  scale_colour_gradientn(colours = c("red","yellow","green","lightblue","darkblue"),values=c(1.0,0.8,0.6,0.4,0.2,0),guide_legend(title ="twiter per person"))+labs(title="twiter per person for the countries in different development level ", y="twiter per person", x="city index")+ theme(legend.position = c(.88, .85),panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank())

ggsave("/Users/jimmy/Dropbox/data_viz/81_cities/tw_perperson3.png", kk3)


p=ggplot(eighty_one_cities, aes(factor(tw_per_person_category)))  + facet_wrap(~continent)+ geom_bar() +coord_polar(theta='y')+labs(title="twiter per person categories for the countries in different continent ")+ theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())
ggsave("/Users/jimmy/Dropbox/data_viz/81_cities/p1.png", p)

m <- ggplot(eighty_one_cities, aes(x = tw_per_person)) + geom_histogram(binwidth = 0.005)+labs(title="Histogram of twitter per person", y="Count of tw_per_person", x="tw_per_person")+ theme_bw()

ggsave("/Users/jimmy/Dropbox/data_viz/81_cities/histogram1.png", m)



ggplot(eighty_one_cities, aes(x= city_NO, y= tw_per_person,color= tw_per_person)) + geom_point(size=eighty_one_cities$tw_per_person,alpha=0.7)+ facet_wrap(~ dev)+  scale_colour_gradientn(colours = c("red","yellow","green","lightblue","darkblue"),values=c(1.0,0.8,0.6,0.4,0.2,0),guide_legend(title ="twiter per person"))+labs(title="twiter per person for the countries in different development level ", y="twiter per person", x="city index")+ theme(legend.position = c(.88, .85),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

#new_data_frame= eighty_one_cities[,11:40]

new_df$population <- 0
new_df $region <- factor(1:2430)
new_df$city_index <- abs(rnorm(2430, sd=1))
for(index_new in 1:2430){
    new_df$city_index[index_new]=as.integer((index_new-1)/30+1)
    new_df$population[index_new]=eighty_one_cities[new_df$city_index[index_new],3]
}
new_df$time <- 0

#new_df$time <- 0
new_df$tw_per_person <- 0

for (j in 1:2430){
    new_df$time[j]=(j-1) %% 30 + 1
    
}
colname_of_eithyone_cities=colnames(eighty_one_cities)

for (i in 1:30){
    
    new_df$tw_per_person[(1+81*(i-1)):(81*i)]=eighty_one_cities[,10+i]
}

for (k in 1:2430)
if(is.na(new_df$tw_per_person[k])){
    new_df$tw_per_person[k]=0
}

#for (m in  1:2430){
#new_df$population[]
#}
library(dplyr)

plot(new_df$city_index, new_df$time, pch=16, cex=new_df$tw_per_person/new_df$population)
#ggplot(aes(new_df$city_index, new_df$time)) + geom_point(size=new_df$tw_per_person*100,alpha=0.7)

#ggplot(new_df, aes(x= city_index, y= tw_per_person)) + geom_point(size=3,alpha=0.7)


plot(new_df$city_index[40:], new_df$time[40:], pch=16, cex=new_df$tw_per_person/new_df$population*30)
