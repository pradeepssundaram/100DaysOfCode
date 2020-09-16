library(tidyverse)
library(magrittr)

bikesharing <- read_csv("./Data/hour.csv")
# yr_mapping = {0: "2011", 1:"2012"}
# mnth_mapping = {1:"Jan",2:"Feb",3:"Mar",4:"Apr",5:"May",6:"Jun",7:"Jul",8:"Aug",9:"Sep",10:"Oct",11:"Nov",12:"Dec"}
# weekday_mapping={0: 'Sunday', 1: 'Monday', 2: 'Tuesday',3: 'Wednesday', 4: 'Thursday', 5: 'Friday', 6: 'Saturday'}
# weather_mapping={1:"Clear",2:"Misty",3:"Light Snow/Rain",4:"Heavy Snow/Rain"}

bikesharing %<>% mutate(
                        season=case_when(
                          season==1 ~ "winter",
                          season==2 ~ "spring",
                          season==3 ~ "summer",
                          season==4 ~ "fall",
                          TRUE~"NA"  ), 
                        yr = case_when(
                          yr==0 ~"2011",
                          yr==1 ~ "2012"
                        ),
                        mnth = case_when(
                          mnth==1 ~"Jan",
                          mnth==2 ~"Feb",
                          mnth==3 ~"Mar",
                          mnth==4 ~"Apr",
                          mnth==5 ~"May",
                          mnth==6 ~"Jun",
                          mnth==7 ~"Jul",
                          mnth==8 ~"Aug",
                          mnth==9 ~"Sep",
                          mnth==10 ~"Oct",
                          mnth==11 ~"Nov",
                          mnth==12 ~"Dec",
                          ),
                        weekday= case_when(
                          weekday==0 ~ "Sunday",
                          weekday==1 ~ "Monday",
                          weekday==2 ~ "Tuesday",
                          weekday==3 ~ "Wednesday",
                          weekday==4 ~ "Thursday",
                          weekday==5 ~ "Friday",
                          weekday==6 ~ "Saturday"
                        ),
                        weathersit= case_when(
                          weathersit==1 ~"Clear",
                          weathersit==2 ~"Misty",
                          weathersit==3 ~"Light Snow or Rain",
                          weathersit==4 ~"Heavy Snow or Rain"
                        )
                        
                          )

# plot_data<-bikesharing %>% pivot_longer(cols=c(registered,casual),names_to="RideType",values_to="NumberOfRides") %>% 
#   select(weekday,RideType,NumberOfRides)
#by season by hour

plot_data<-bikesharing %>% pivot_longer(cols =c(registered,casual),names_to="RideType",values_to="NumberOfRides") %>% 
  select(hr,season,RideType,NumberOfRides) %>% mutate(season=factor(season,levels=c("winter","spring","summer","fall")),RideType=factor(RideType,levels=c("registered","casual")), hr=factor(hr))

plot_data_average <- 
  bikesharing %>% group_by(hr,season) %>% summarise(registered=mean(registered),casual=mean(casual)) %>% 
  pivot_longer(cols =c(registered,casual),names_to="RideType",values_to="AverageRides") %>% 
  select(hr,season,RideType,AverageRides) %>% 
  mutate(season=factor(season,levels=c("winter","spring","summer","fall"))
         ,RideType=factor(RideType,levels=c("registered","casual"))
         , hr=factor(hr))

ggplot(data = plot_data_average) + 
  geom_col(mapping = aes(x=hr,y=AverageRides),fill="lightblue")+ 
  scale_x_discrete(name="hour", breaks=seq(0,23))+
  facet_grid(rows = vars(season),cols = vars(RideType),switch = "y") +
  theme_classic()





ggplot(data = plot_data) + 
  geom_col(mapping = aes(x=hr,y=NumberOfRides),fill="orange")+ 
  scale_x_discrete(name="hour", breaks=seq(0,23))


ggplot(data = plot_data) + 
  geom_col(mapping = aes(x=hr,y=NumberOfRides),fill="lightblue")+ 
  scale_x_discrete(name="hour", breaks=seq(0,23))+
  facet_grid(rows = vars(season),cols = vars(RideType),switch = "y") +
  theme_classic()

ggplot(data = plot_data) + 
  geom_bar(mapping = aes(x=hr,y=NumberOfRides),fill="lightblue",stat="identity")+ 
  facet_grid(rows = vars(season),cols = vars(RideType)) +
  theme_classic()




plot_data<-bikesharing %>% pivot_longer(cols =c(registered,casual),names_to="RideType",values_to="NumberOfRides") %>% 
  select(weekday,season,RideType,NumberOfRides) %>% mutate(season=factor(season,levels=c("winter","spring","summer","fall"))
                                                   ,RideType=factor(RideType,levels=c("registered","casual"))
                                                  ,weekday=factor(weekday,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
                                                )
  
ggplot(data = plot_data) +
  geom_col(mapping = aes(x=weekday,y=NumberOfRides),fill="lightblue") +
  facet_grid(rows = vars(season),cols = vars(RideType)) +
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})+
  theme_classic()


plot_data <- bikesharing %>% group_by(weekday,season) %>% 
             summarise(registered=mean(registered),casual=mean(casual)) %>% 
            pivot_longer(cols = c(registered,casual),names_to="RideType",values_to="AverageRides") %>% 
  mutate(season=factor(season,levels=c("winter","spring","summer","fall"))
         ,RideType=factor(RideType,levels=c("registered","casual"))
         ,weekday=factor(weekday,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
  )
            
            
ggplot(data = plot_data) +
  geom_col(mapping = aes(x=weekday,y=AverageRides),fill="lightblue") +
  facet_grid(rows = vars(season),cols = vars(RideType)) +
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})+
  theme_classic()



ttestData<-bikesharing %>% 
  select(weekday,registered,casual) %>% 
  mutate(IsWeekDay=case_when(
    weekday %in% c("Saturday","Sunday") ~ "No",
    TRUE~"Yes"
  )) %>% 
  select(IsWeekDay,casual,registered) %>% 
  pivot_longer(cols=c(registered,casual),names_to="RideType",values_to="NumRides")

testResults<-ttestData %>% 
  group_by(RideType,IsWeekDay) %>% 
  nest() %>% 
  spread(key =IsWeekDay,value =data  ) %>% 
  mutate(
    t_test = map2(No, Yes, ~{t.test(.x$NumRides, .y$NumRides) %>% tidy()})
    
  ) %>%
  unnest(c(t_test)) %>% 
  select(RideType,statistic,p.value)

testResults

# df %>% spread(key, value) is equivalent to df %>% pivot_wider(names_from = key, values_from = value)

testResults<-ttestData %>% 
  group_by(RideType,IsWeekDay) %>% 
  nest() %>% 
  pivot_wider(names_from = IsWeekDay,values_from=data) %>% 
  mutate(
    t_test = map2(No, Yes, ~{t.test(.x$NumRides, .y$NumRides) %>% tidy()})
    
  ) %>%
  unnest(c(t_test)) %>% 
  select(RideType,statistic,p.value)

testResults


plot_data<- bikesharing %>% select(weekday,registered)


# %>% filter(!weekday %in%c("Saturday","Sunday") )
# tesst1<- t.test(ttestData %>%  filter(IsWeekDay==TRUE ) %>% select(registered),
#                 ttestData %>%  filter(IsWeekDay==FALSE) %>% select(registered),var.equal = FALSE
#               ) %>% tidy()
# View(tesst1)

# category= registered or casual (2)
# group = IsWeekDay
# value = numrides

ttestData %>% 
  group_by(RideType,IsWeekDay) %>% 
  nest() %>% 
  spread(key =IsWeekDay,value =data  ) %>% 
  mutate(
    t_test = map2(No, Yes, ~{t.test(.x$NumRides, .y$NumRides) %>% tidy()})
   
  ) %>% 
  unnest() 

         
  
  ttestData %>%  filter(is(IsWeekDay) %>% select(registered) ,ttestData %>% filter(!is(IsWeekDay) %>% select(registered) ))

  




