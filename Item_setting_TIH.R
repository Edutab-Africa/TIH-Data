setwd("C:/Users/Edutab User/Desktop/Kenya TIH_Child Observations 28.10.2021")


library(ggplot2)
library(forcats)
library(tidyr)
library(mudata2)
library(tidyverse)
library(openxlsx)
library("xlsx")
library(dplyr)
library(readxl)
library(plotly)
library(leaflet)
library(scales)



TIH<-

###########################################

Item_homemade <- read_excel("Item_homemade.xlsx")

#names(Item_homemade)

Item_homemade%>%
  select(Activity...3, Item_homemade)%>%
  rename(Activity="Activity...3", Item_type="Item_homemade")%>%
  ggplot(aes(x=Item_type, fill=Item_type))+
  geom_bar()+
  scale_fill_brewer(palette = "Dark2")+
  labs(x = "Item_type",
       title = "Type of the items used")+
  theme(legend.position = "none",
        axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))
############################################################################################
Item_setting<-read_excel("Setting.xlsx")

names(Item_setting)


  
Item_setting%>%
  select(Activity,Setting) %>%
  ggplot(aes(x=Setting, fill=Setting))+
  geom_bar() +
  labs(x = "Item_setting",
       title = "Setting of Activities")+
  theme(legend.position = "none",
        axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))



Item_setting %>%
  group_by(Setting) %>%
  summarise(n = n())

#########################
Activities_Older<-read_excel("TIH_coding.xlsx",sheet = "Activity_older")
Activities_Younger<-read_excel("TIH_coding.xlsx",sheet = "Activity_Young")


Activities_Older <- Activities_Older %>%
  mutate(age = "Older")

Activities_Younger<-Activities_Younger%>%
  mutate(age="Younger")

rbind(Activities_Older,Activities_Younger) %>%
  filter(Country == "Ethiopia" & Country != "NA" ) %>%
  select(  Play,Chores,"School work",Helping,Eat,age)%>%
  pivot_longer(!c(age),values_to = "Count",names_to = "Activities")%>%
  filter(Count==1)%>%
  ggplot(aes(x=Activities,   fill=Activities))+
  geom_bar()+
  scale_fill_brewer(palette ="Dark2")+
  scale_y_continuous(breaks=seq(0,20,3))+
  facet_wrap(~age)+
  labs(x = "Activities", 
       y = "Frequencies",
       title = "Activities done by Younger and Older Children")+
  theme(legend.position = "none",
        axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

  
 



names(b)


#%>%
  pivot_longer(!c(),values_to = "Count",names_to = "Activities")
  
  




names(Activities_Older)
names(Activities_Older)


Activities_Older%>%
  select(Play,Chores,'School work',Helping,Eat)%>%
  pivot_longer(!c(),values_to = "Count",names_to = "Activities")%>%
  filter(Count==1)%>%
  ggplot(aes(x=Activities,  fill=Activities))+
  geom_bar()


  
  
  #list=c("Play","Chores",'School work',"Helping","Eat")%>%
    as.data.frame("list")
  
  
  #%>%
    ggplot(aes(x=list, fill=list))+
    geom_bar() 
    
  
  


  
  
  



                    