library(readxl)
library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyverse)
library(readr)
library(hrbrthemes)

data_v1 <- read_excel("DSC-640/Data Viz/Week 11-12/data-v1.xlsx")
acc_3 <- read_excel("DSC-640/Data Viz/Week 11-12/acc_3.xlsx", sheet = "gen_av")
fatal <- read_excel("DSC-640/Data Viz/Week 11-12/acc_3.xlsx", sheet = "fat")
fatal$total <- fatal$Airliner_Fatalities + fatal$Corp._jet_Fatalities + fatal$Hijacking_Fatalities
acc_4 <- read_excel("DSC-640/Data Viz/Week 11-12/acc_3.xlsx", sheet = "acc4")
location <- read_excel("DSC-640/Data Viz/Week 11-12/acc_3.xlsx", sheet = "location2")
vd_melt3 <- location %>%
  group_by(type) %>%
  arrange(desc(type), continent) %>%
  mutate(prop = value / sum(location$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

vd_melt3$Label <- paste(round(vd_melt3$value*100,2),"%", sep=" ")

loc_melt <- location %>%
  group_by(continent) %>%
  arrange(continent, desc(type)) %>%
  mutate(lab_ypos = cumsum(value) - 0.5 * value) 


ggplot(data_v1, aes(x=PassengerMiles, y=Accidents, size=Accidents, color=PassengerMiles)) + 
  geom_point() +
  theme(legend.title=element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  theme_minimal() + ggtitle("Accidents Count over Passenger Miles", subtitle =  "from 1990 to 2019") +
  xlab("Passenger Miles (in Millions)") + ylab("Accident Count") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(labels = scales::comma) +
  labs(caption = 'Data Source: United States Bureau of Transportation Statistics') +
  theme_ipsum()

ggplot(data_v1, aes(x=Year, y=TotalFlights)) + 
  geom_step(color="#24478f", size=1, alpha=0.9, linetype=1) + 
  #scale_y_continuous(labels = function(Total_Miles) format(Total_Miles, scientific = FALSE)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face='bold', size = 20), plot.subtitle = element_text(hjust = 0.5, face='bold', size = 15)) +
  theme(legend.title=element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  ylab('Flight Count') + 
  scale_y_continuous(labels = scales::comma) +
  ggtitle(label = "Recognizing Air Travel Industry Growth in the United States", subtitle =  "Total Flights from 1990-2019") +
  labs(caption = 'Data Source: World Bank') +
  theme_ipsum()

#ggplot(acc_3, aes(x = Year, y = value)) + 
#  geom_line(aes(color = category, linetype = category)) + 
#  scale_color_manual(values = c("#002699", "#809fff","#99ccff","#0059b3","#4d4dff","#6666ff")) +
#  theme_minimal() +
#  theme(plot.title = element_text(hjust = 0.5, face='bold', size = 20), plot.subtitle = element_text(hjust = 0.5, face='bold', size = 15)) +
#  theme(legend.title=element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
#  ylab('Accident Rate') +
#  ggtitle(label="Aircraft Accident Rates", subtitle = "1990–2014 (per 100,000 flight hours)") +
#  labs(caption = 'Data Source: National Business Aviation Association') +
#  theme_ipsum()
  
ggplot(fatal, aes(x=Year, y=total, fill=total)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low='#3399ff', mid='#b3d9ff', high='#0066cc', space='Lab') +
  theme_minimal() +
  ggtitle(label = "Air Travel Fatalities 1990-2019", subtitle =  "Total Count by Year") +
  labs(caption = 'Data Source: Aviation Safety Network') +
  ylab('Fatalities') +
  theme(plot.title = element_text(hjust = 0.5, face='bold', size = 15), plot.subtitle = element_text(hjust = 0.5, face='bold', size = 12)) +
  theme_ipsum() +
  theme(legend.title=element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))

ggplot(acc_4, aes(x=variable, y=value, fill=type)) + 
  geom_boxplot() +
  facet_wrap(~variable, scale="free") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face='bold', size = 20), plot.subtitle = element_text(hjust = 0.5, face='bold', size = 15)) +
  ylab('Rate') +
  xlab("Flight Type") +
  ggtitle(label="Aircraft Accident Rates by Flight Type", subtitle = "1990–2014 (per 100,000 flight hours)") +
  labs(caption = 'Data Source: National Business Aviation Association') +
  theme_ipsum() +
  theme(legend.title=element_blank(), strip.text = element_blank()) +
  scale_fill_brewer(palette="Blues")
  #theme(legend.title=element_blank(), axis.text.x=element_blank())
  
  

ggplot(vd_melt3, aes(x="", y=prop, fill=continent)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y") + 
  theme_void() +
  theme_ipsum() +
  facet_wrap(~type) +
  geom_text(aes(label = vd_melt3$Label), position = position_stack(vjust = 0.50), size=3.5) +
  scale_fill_manual(values = c("Africa" = "#e6f9ff",
                               "Asia" = "#e6f2ff",
                               "Europe" = "#b3d9ff",
                               "North America" = "#80bfff",
                               "South America" = "#4da6ff")) +
  ggtitle("Air Travel Accident and Fatality Distribution", subtitle = "Top 5 Continents from 1945 to 2021") +
  labs(caption = 'Data Source: Aviation Safety Network') +
  ylab("") + xlab("") + labs(x="", y="") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        legend.title=element_blank()) +
  #scale_y_continuous(breaks=cumsum(vd_melt3$value) - vd_melt3$value / 2, labels= vd_melt3$value)
  #scale_fill_manual(values = c("#b3e0ff", "#0066ff","#ffffff")) +
  #ggtitle("Obama Approval Ratings \n by Issue")
