# Author: Chido 
# Pre-requisite: ODBC link to the CensusDB and the InterVA 
library(tidyverse)
library(ggplot2)
library(rCharts)
library(plotly)
library(gganimate)
library(gifski)
library(haven)



lt <- read_dta("C:/Users/Chido/Documents/Summary indicators/Out/LT.dta")

cbrew <- c("#a6cee3", "#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a","#ffff99",
           "#b15928")

staticplot <- ggplot(lt, aes(x=age, y=e, color = factor(SexCode))) +
              geom_point(alpha =0.5)


anim = staticplot + transition_states(year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Life expectancy by Age group per Year : {closest_state}',  
       subtitle  =  "Top 10 causes",
       caption  = "Data Source: Main Surveillance")

# For GIF
animate(anim, 200, fps = 20,  width = 600, height = 600, duration = 80, 
        renderer = gifski_renderer("C:/Users/Chido/Documents/CFB Reports/App edits/www/le_interactive.gif"))

pop <- sqlQuery(con,"select * from cfb..PopulationByAgeGrp")%>%
      filter(VillNo== 80) %>%
      mutate(Place = "Agincourt") %>%
     select(Years, AgeGroup, Ordered, MalePop,FemalePop, Place)

popsa <- read.csv("C:/Users/Chido/Documents/CoD South Africa/WPPPopulation.csv") %>%
       rename(Years =ï..Years ) %>%
         mutate(Place = "South Africa", MalePop= MalePop/1000, FemalePop=FemalePop/1000) %>%
        filter(Years>1991 & Years<2020) %>%
        select (Years, AgeGroup, Ordered, MalePop,FemalePop, Place)


popall <- rbind(pop,popsa)


popall$AgeGroup <- reorder(popall$AgeGroup,popall$Ordered)


pt <- popall  %>%
  rename(Male = MalePop, Female = FemalePop) %>%
  gather(key = Gender, value = Population, -c(Years, Ordered,AgeGroup, Place)) %>%
  mutate(t = 
           replace(Population, Gender== "Male", -Population)   ) %>%
  arrange(Ordered)


my.animation <- ggplot(pt  , aes(x =AgeGroup, y = t, fill = Gender)) +
  geom_bar(data=subset(pt, Gender == "Female"), stat = "identity") +
  geom_bar(data=subset(pt, Gender == "Male"), stat = "identity") +
  facet_wrap(~Place, scales = "free") +
  labs(y = "Age group") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(values=c("dodgerblue4", "darkolivegreen4","darkorchid3", "goldenrod1"))+
  theme_classic()+
  theme(axis.title = element_text(size = 15), legend.text = element_text(size = 15), legend.title =element_text(size = 15)
        ,strip.text.x = element_text(size = 15))+
  coord_flip()+
  labs(title = 'Year of the surveillence: {frame_time}', x = 'Age group', y = '         Population                             Population in 000s') +
  transition_time(Years) +
  ease_aes('linear')

# animate in a two step process:
animate(my.animation, height = 600, width =600)
anim_save("C:/Users/Chido/Documents/CFB Reports/App edits/www/agincourt_sa_1.gif")


my.agincourt <- ggplot(subset(pt,Place =="Agincourt")  , aes(x =AgeGroup, y = t, fill = Gender)) +
  geom_bar(data=subset(pt, Gender == "Female"), stat = "identity") +
  geom_bar(data=subset(pt, Gender == "Male"), stat = "identity") +
  #facet_wrap(~Place, scales = "free") +
  labs(y = "Age group") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(values=c("dodgerblue4", "darkolivegreen4","darkorchid3", "goldenrod1"))+
  theme_classic()+
  coord_flip()+
  labs(title = 'Year of update: {frame_time}', x = 'Age group', y = 'Population') +
  transition_time(Years) +
  ease_aes('linear')

# animate in a two step process:
animate(my.agincourt, height = 800, width =800)
anim_save("C:/Users/Chido/Documents/CFB Reports/agincourt.gif")


ggplot(cod_formatted, aes(rank, group = Cause, 
                          fill = as.factor(Cause), color = as.factor(Cause))) +
  geom_tile(aes(y = Percentage_lbl/2,
                height = Percentage_lbl,
                width = 0.5), alpha = 0.8, color = NA) +
  theme_classic() +
  labs() +
  transition_time(Year) +
  ease_aes('linear') +
  geom_text(aes(y = 0, label = paste(Cause, " ")), vjust = 0.2, hjust = 1) +
  #geom_text(aes(y=Percentage_lbl,label = Percentage_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  xlab("Percentage of deaths")
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  labs( title = 'Year: {frame_time}', x = "Percentage of deaths" , subtitle  =  "Top 10 causes",
       caption  = "InterVA | Data Source: Verbal Autopsy interviews from the Agincourt Surveillence Site")+
  theme(
    #axis.line=element_blank(),
     #   axis.text.x=element_blank(),
        axis.text.y=element_blank(),
     #   axis.ticks=element_blank(),
     #   axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.05, color="grey" ),
      # panel.grid.minor.x = element_line( size=.1, color="grey" ),
      #  plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
      #  plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
      #  plot.caption =element_text(size=15,hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(1,1, 1, 7, "cm")
      )

  cod_formatted %>%
    mutate(Cause = ifelse(Cause %in% c("HIV/AIDS & TB","Acute cardiac disease", "Road accident"), Cause, "Other")) %>%
    factor(.,levels = c("HIV/AIDS & TB","Acute cardiac disease", "Road accident", "Other")) %>%
    group_by(Year, Cause) %>%
    mutate(total = sum(n))
  
  
ggplot(, aes(x=Year, y=n, fill=Cause))+
  geom_col(position="fill")
