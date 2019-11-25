#libraries required for the analysis
library(shiny)
library(tidyverse)
library(plotly)
library(mapproj)
library(ggmap)
library(dplyr)

#reading the csv.file
climate<-read_csv(file.choose())
head(climate)

#removing the data of 1751 - 1899
#we are interested in the data from 1900 - 2017
climate_new<-climate[,c(1,151:268)]
head(climate_new)

#we create vector of country names
country<-as.character(as.vector(climate_new$Country))

#we create vector of year 1900 - 2017 
year<-names(climate_new)[-1]

#getting emission of co2 in the year 2017
emission_2017<-as.numeric(as.vector(climate_new$'2017'))

#creating daatframe with country name & emission in 2017
pie_2017<-data.frame(country,emission_2017)

#we cleaned the data and read it
pie_2017<-read_csv(file.choose())
pie_2017<-as.data.frame(pie_2017)
pie_2017

#to find which countries has maximum and minimum emission
pie_2017[order(pie_2017$emission_2017),]

#we select the data of 2017 for the main countries 
pie_2017_main<-pie_2017[pie_2017$country %in% c('China','USA','American Samoa','Germany','UK','India','Russia','Japan'),]
pie_2017_main

#we make the other countries in another dataframe
pie_2017_others<-pie_2017[!(pie_2017$country %in% c('China','USA','American Samoa','Germany','UK','India','Russia','Japan')),]
pie_2017_others

#we sum the emission of other countries
emission_others<-sum(pie_2017_others$emission_2017)
emission_others

#we create a dataframe for the other category with their sum emission
pie_2017_others_new<-data.frame(country='Others',emission_2017=emission_others)

#we row bind main countries with other countries category
my_pie<-rbind(pie_2017_main,pie_2017_others_new)
my_pie

#we plot pie chart for contribution of various countries to 2017 co2 emission       
plot_ly(my_pie, labels = ~country, values = ~emission_2017, type = 'pie') %>%
  layout(title = 'Co2 emission in 2017',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#we extract the data for world from 1900-2017
bar<-climate_new[climate_new$Country=='World',]
bar

#we extract the emission of world as a vector
emission_world<-as.numeric(as.vector(bar[1,2:119]))
emission_world

#we create a data frame with year and emission of world in 2017
bar_df<-data.frame(year,emission_world)
bar_df

#we plot bargraph for emission
plot_ly(x=bar_df$year,y=bar_df$emission_world,type="bar") %>% 
  layout(title="Co2 emission from 1900 - 2017")

#we use pie_2017 data to plot world map 
#we need latitude and longitude data for this purpose
world_map=map_data("world")
world_map
write.csv(world_map,"worlddata.csv")

world_map=merge(world_map,pie_2017,by.x="region",by.y="country")
world_map

world_map=world_map[order(world_map$group,world_map$order),]
world_map
write.csv(world_map,"worlddatanew.csv")

ggplot(world_map,aes(x=long,y=lat,group=group,fill=emission_2017))+geom_polygon(color="beige")+scale_fill_continuous(low="light blue",high="dark blue",guide="colorbar")+theme_bw()+labs(fill="co2 emission (in tonnes)",title="Co2 emission in 2017",x="",y="")+scale_y_continuous(breaks=c())+scale_x_continuous(breaks=c())+theme(panel.border=element_blank())

# To find the growth rate of major co2 emitters
climate_new$gb_2016<-(climate_new$`2017`-climate_new$`2016`)/climate_new$`2016`

g<-data.frame(country=climate_new$Country,growth_rate=climate_new$gb_2016,stringsAsFactors=FALSE)
g_new<-g[g$country %in% c('China','United States','American Samoa','Germany','United Kingdom','India','Russia','Japan'),]
g_bar<-data.frame(country=g_new$country,growth_rate=g_new$growth_rate)
g_baro<-g_bar[order(g_bar$growth_rate),]

X_bar<-as.character(as.vector(g_baro$country))
Y_bar<-as.numeric(as.vector(g_baro$growth_rate))

plot_ly(x=X_bar,y=Y_bar,type="bar") %>% 
  layout(title="Growth rate of major CO2 emitters")

# To plot the growth rate world map
world_map=map_data("world")
world_map
write.csv(g,"growth.csv")

g[[220,1]]<-"UK"
g[[221,1]]<-"USA"

world_map=merge(world_map,g,by.x="region",by.y="country")
world_map

world_map=world_map[order(world_map$group,world_map$order),]
world_map

ggplot(world_map,aes(x=long,y=lat,group=group,fill=growth_rate))+geom_polygon(color="beige")+scale_fill_continuous(low="yellow",high="dark red",guide="colorbar")+theme_bw()+labs(title="Growth Rate of Countries",x="",y="")+scale_y_continuous(breaks=c())+scale_x_continuous(breaks=c())+theme(panel.border=element_blank())

#prediction of emission
install.packages("pracma")
library(pracma)
m<-movavg(emission_world,2,"s")
plot(m)

install.packages("forecast")
library(forecast)
f<-forecast(m,3)
plot(f)

plot(f$residuals,type)
p<-f$mean

summary(f)

acf(f$residuals,lags=3)
pacf(f$residuals,lags=3)

year_predict
emission_predict<-c(emission_world,p)
prdf<-data.frame(year=year_predict,emission=emission_predict)

cp<-c(rep('emission',length(emission_world)),rep('prediction',3))
cp


plot_ly()%>% add_lines(x=year,y=emission_world,color=I("blue"),name="observed")%>%add_lines(x=c(2018,2019,2020),y=p,color=I("red"),name="predicted") %>% layout(title="Trend line using Simple Moving Average")

plot_ly(x=prdf$year,y=prdf$emission,type="bar",color=cp) %>% 
  layout(title="CO2 emission from 1900 - 2020")

plot_ly(x=prdf$year,y=prdf$emission,type="bar",color=cp) %>% 
  layout(title="CO2 emission from 1900 - 2020") %>% add_lines(x=year,y=emission_world,color=I("blue"),name="observed")%>%add_lines(x=c(2018,2019,2020),y=p,color=I("red"),name="predicted")

mean(emission_world)
