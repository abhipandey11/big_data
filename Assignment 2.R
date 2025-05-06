library(dplyr)
library(nycflights13)
library(ggplot2)

data<-nycflights13::flights
str(data)


ggplot(data,aes(x=arr_delay,y=dep_delay))+
  geom_point(color="blue",alpha=0.5)+
  labs(title="scatter plot",x="arrival delay",ylab="departure delay")+
  theme_minimal()

ggplot(data,aes(x=carrier,fill=carrier))+
  geom_bar()+
  theme_minimal()

ggplot(data,aes(x=carrier,y=arr_delay,fill=carrier))+ 
  geom_boxplot()+ 
  labs(title="box plot",x="Carrier",y="Arrival Delay(minutes)")+ 
  theme_minimal()
ggplot(data,aes(x=arr_delay,y=arr_delay))+
  geom_line(color="red",linetype="dashed")+
  theme_minimal()
data_summary<-data%>%group_by(month)%>%summarise(avg_dep_delay=mean(dep_delay,na.rm=TRUE)) 
ggplot(data_summary,aes(x=month,y=avg_dep_delay))+ 
  geom_line(color="blue",size=1)+ 
  geom_point(color="red",size=3) 
labs(title="line plot",x="Month",y="Average Departure Delay(minutes)")+ 
  theme_minimal() 


ggplot(data,aes(x=dep_delay,y=arr_delay,color=carrier))+ 
  geom_point(alpha=0.5)+ 
  facet_wrap(~carrier)+ 
  labs(title="facetted scatter plot",x="Departure Delay(minutes)",y="Arrival Delay(minutes)")+ 
  theme_minimal() 

stem(flights$dep_delay, scale = 1) 
