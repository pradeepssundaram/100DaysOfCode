library(tidyverse)
library(magrittr)

bikesharing <- read_csv("./Data/day.csv")
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

bikeshare_plot<-bikesharing %>% pivot_longer(cols = c(registered,casual),names_to="RideType",values_to="NumRides") %>% select(RideType,NumRides)

fig1<-ggplot(data = bikesharing) +
    geom_density(mapping = aes(x=registered))+
  geom_density(mapping = aes(x=casual))
fig1 +   scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})

fig <- ggplot(data = bikeshare_plot) +
  geom_density(mapping = aes(x=NumRides,fill=RideType),alpha=0.25) +
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})+
  theme_classic() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

fig


fig <- ggplot(data = bikeshare_plot) +
  geom_density(mapping = aes(x=NumRides,fill=RideType),alpha=0.25) +
  scale_fill_manual( values = c("red","orange"))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})+
  theme_classic()
fig


# 
# 
# fig <- ggplot(data = bikesharing) + 
#   geom_density(mapping = aes(x=registered,colour="red")) +
#   geom_density(mapping = aes(x=casual,colour="blue")) +
#   theme_classic() +
#   theme(panel.grid.major.x = element_blank()) +
#   theme(panel.grid.minor.x = element_blank()) +
#   theme(panel.grid.major.y = element_blank()) +
#   theme(panel.grid.minor.y = element_blank()) +
#   # scale_x_discrete(limit=c(1960,1970,1980,1990,2000,2010),position = "bottom") +
#   # scale_y_discrete(limit=c(2,4,6,8,10,12)) +
#   #theme(panel.background =   element_rect(fill = "#B3000C"))+
#   #theme(panel.background =   element_rect(fill = "#00B32C"))+
#   #theme(panel.background =   element_rect(fill = "#EF0B3D"))+
#   #theme(plot.background = element_rect(fill = "#D8D8D8"))+
#   labs(title = "Distribution of Registered and Casual Riders ",
#        x="Rides",y="")+
#   theme(plot.title = element_text(size=10, face="bold", vjust=1, lineheight=0.6))
#   #theme(plot.title = element_text(size=10, face="bold", vjust=1, lineheight=0.6,colour = "#00B32C"))
# # +
# #   theme(
# #     axis.title.x = element_text(color="#00B32C", vjust=-0.35),
# #     axis.title.y = element_text(color="#00B32C" , vjust=0.35) ,
# #     axis.text = element_text(color="#00B32C")
# #   )
# fig
# 
# #ggsave("./2019-12-24/Figures/ChristmasTrend.png",plot = myplot,dpi = "retina")



# 
# import matplotlib.pyplot as plt
# import seaborn as sns, numpy as np
# 
# sns.set(rc={"figure.figsize": (8, 4)}); np.random.seed(0)
# x = np.random.randn(100)
# ax = sns.distplot(x)
# plt.show()



plot_data<- bikesharing %>% select(dteday,registered,casual) %>% 
  group_by(dteday) %>% summarise(registered=sum(registered),casual=sum(casual)) %>% 
  pivot_longer(cols=c(registered,casual),names_to="RideType",values_to="NumberOfRides")

#plot_data %>% slice_sample(n = 100)

fig<-ggplot(data = plot_data) + 
  geom_line(mapping = aes(x=dteday,y=NumberOfRides,color=RideType))+
  theme_classic()+
  theme(
    legend.position = c(1, 1),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(0, 0, 0, 0)
  )
fig