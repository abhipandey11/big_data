library(ggplot2)
library(dplyr)
library(nycflights13)

data<-nycflights13::flights

head(data)
str(data)
summary(data)

#scatter plot
ggplot(data,aes(x=dep_delay,y=arr_delay))+
  geom_point(color="blue",alpha=0.5)+
  labs(title="scatter plot",x="Departure Delay(minutes)",y="Arrival Delay(minutes)")+
  theme_minimal()

#bar plot
ggplot(data,aes(x=carrier,fill=carrier))+
  geom_bar()+
  labs(title="bar plot",x="Carrier",y="Count")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45,hjust=1))

#box plot
ggplot(data,aes(x=carrier,y=arr_delay,fill=carrier))+
  geom_boxplot()+
  labs(title="box plot",x="Carrier",y="Arrival Delay(minutes)")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=15,hjust=1))
#line plot

data_summary<-data%>%group_by(month)%>%summarise(avg_dep_delay=mean(dep_delay,na.rm=TRUE))
ggplot(data_summary,aes(x=month,y=avg_dep_delay))+
  geom_line(color="blue",size=1)+
  geom_point(color="red",size=3)
labs(title="line plot",x="Month",y="Average Departure Delay(minutes)")+
  theme_minimal()

#facetted scatter plot
ggplot(data,aes(x=dep_delay,y=arr_delay,color=carrier))+
  geom_point(alpha=0.5)+
  facet_wrap(~carrier)+
  labs(title="facetted scatter plot",x="Departure Delay(minutes)",y="Arrival Delay(minutes)")+
  theme_minimal()
#scatter plot with regression
ggplot(data,aes(x=dep_delay,y=arr_delay))+
  geom_point(color="blue",alpha=0.3)+
  geom_smooth(method="lm",color="red",se=TRUE)+
  labs(title="scatter plot regression",x="Departure Delay(minutes)",y="Arrival Delay(minutes)")+
  theme_minimal()
#histogram
ggplot(data,aes(x=dep_delay))+
  geom_histogram()+
  labs(title="box plot",x="Carrier",y="Arrival Delay(minutes)")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=15,hjust=1))
#rug plot
ggplot(data,aes(x=dep_delay))+
  geom_rug()+
  labs(title = "Rug Plot of Departure Delays",
       x = "Departure Delay (minutes)")
#stem plot
stem(flights$dep_delay, scale = 1)
