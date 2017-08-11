system("ls ../input", intern=TRUE)
## [1] "gtd"          "refugee-data"
library(dplyr)
library(ggplot2)
library(highcharter)
library("viridisLite")
library(countrycode)
library(plotly)
library(geomnet)

#importing the data

system("ls ../input", intern=TRUE)
## [1] "gtd"          "refugee-data"
resettlement <- read.csv("../input/refugee-data/resettlement.csv")
assylum_s <- read.csv("../input/refugee-data/asylum_seekers.csv")
assylum_sm <- read.csv("../input/refugee-data/asylum_seekers_monthly.csv")
demographics <- read.csv("../input/refugee-data/demographics.csv")
persons <- read.csv("../input/refugee-data/persons_of_concern.csv")
time <-read.csv("../input/refugee-data/time_series.csv")

resettlement<- rename(resettlement, residence= Country...territory.of.asylum.residence)

assylum_sm <- rename(assylum_sm, residence=Country...territory.of.asylum.residence)

#Applications received from refugees by country of Origin

assylum_sm$Value <- as.numeric(assylum_sm$Value)

countries <- assylum_sm %>% group_by(Origin) %>% summarise(Total = sum(Value))
names(countries) <- c("country", "Total")
countrycode_data <- countrycode_data

countries$iso3 <- countrycode_data[match(countries$country, countrycode_data$country.name.en), "iso3c"]

countries[30,3]<-"CIV"
countries[36,3]<-"CAF"
countries[40,3]<-"HKG"
countries[41,3]<-"MAC"
countries[49,3]<-"CZE"
countries[50,3]<-"PRK"
countries[51,3]<-"COD"
countries[55,3]<-"DOM"
countries[68,3]<-"GMB"
countries[77,3]<-"GNB"
countries[85,3]<-"IRN"
countries[98,3]<-"LAO"
countries[136,3]<-"PSE"
countries[146,3]<-"KOR"
countries[147,3]<-"MDA"
countries[159,3]<-"SRB"
countries[177,3]<-"SYR"
countries[180,3]<-"MKD"
countries[195,3]<-"TZA"
countries[195,3]<-"VEN"

data(worldgeojson, package = "highcharter")
dshmstops <- data.frame(q = c(0, exp(1:5)/exp(5)),
                        c = substring(viridis(5 + 1, option = "D"), 0, 7)) %>%  list_parse2()

highchart() %>% 
  hc_add_series_map(worldgeojson, countries, value = "Total", joinBy = "iso3") %>% 
  hc_colorAxis(stops = dshmstops) %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_add_theme(hc_theme_db()) %>% 
  hc_mapNavigation(enabled = TRUE) %>%
  hc_title(text = " Assylum seekers applications from countries of Origin") %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, text = "Data Source: UNHCR ", style = list(fontSize = "12px")) %>%
  hc_legend(enabled = FALSE)



resettlement$Value <- as.numeric(resettlement$Value)
countries_r <- resettlement %>% group_by(residence) %>% summarise(Total = sum(Value))
countries_r$iso3 <- countrycode_data[match(countries_r$residence, countrycode_data$country.name.en), "iso3c"]

countries_r[13,3]<-"CAF"
countries_r[15,3]<-"CZE"
countries_r[26,3]<-"IRN"
countries_r[44,3]<-"KOR"

data(worldgeojson, package = "highcharter")
dshmstops <- data.frame(q = c(0, exp(1:5)/exp(5)),
                        c = substring(viridis(5 + 1, option = "D"), 0, 7)) %>%  list_parse2()

highchart() %>% 
  hc_add_series_map(worldgeojson, countries_r, value = "Total", joinBy = "iso3") %>% 
  hc_colorAxis(stops = dshmstops) %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_add_theme(hc_theme_db()) %>% 
  hc_mapNavigation(enabled = TRUE) %>%
  hc_title(text = "Resettlement of assylum seekers  arrival by country of residence") %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, text = "Data Source: UNHCR ", style = list(fontSize = "12px")) %>%
  hc_legend(enabled = FALSE)



#Applications received from refugees by year

countries_y <- assylum_sm%>%group_by(Year)%>%summarize(Total=sum(Value))

highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "Applications received by year ") %>% 
  hc_xAxis(categories = countries_y$Year) %>% 
  hc_add_series(data = countries_y$Total,
                name = "no of applications",colorByPoint = TRUE)%>%
  hc_credits(enabled = TRUE, text = "Data Source: UNHCR ", style = list(fontSize = "12px")) %>%
  hc_legend(enabled = FALSE)


assylum_sm$Value <- as.numeric(assylum_sm$Value)

applications_r <- assylum_sm%>%group_by(residence)%>%summarize(Total=sum(Value))%>%arrange(desc(Total))

highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "Applications received by country of residence ") %>% 
  hc_xAxis(categories = applications_r$residence) %>% 
  hc_add_series(data = applications_r$Total,
                name = "no of applications",colorByPoint = TRUE)%>%
  hc_credits(enabled = TRUE, text = "Data Source: UNHCR ", style = list(fontSize = "12px")) %>%
  hc_legend(enabled = FALSE)


#Resettlement of refugees by country of Origin


resettlement$Value <-as.numeric(resettlement$Value)

resettlement_O <- resettlement %>% group_by(Origin)%>% summarize(Total =sum(Value))%>%arrange(desc(Total))%>%head(50)


hchart(resettlement_O,type="treemap",hcaes(x = Origin, value = Total, color = Total)) %>%
  hc_title(text = "No of resettlements by country of Origin") %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, text = "Data Source: UNHCR ", style = list(fontSize = "12px")) %>%
  hc_legend(enabled = FALSE)



resettlement$Value <-as.numeric(resettlement$Value)

resettlement_O <- resettlement %>% group_by(residence)%>% summarize(Total =sum(Value))%>%arrange(desc(Total))%>%head(40)

hchart(resettlement_O,type="column",hcaes(x = residence, y = Total, color = Total)) %>%
  hc_title(text = "No of resettlements by country of residence") %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, text = "Data Source: UNHCR ", style = list(fontSize = "12px")) %>%
  hc_legend(enabled = FALSE)



#importing dataset on Global Terror Attacks

system("ls ../input", intern=TRUE)
## [1] "gtd"          "refugee-data"
data <- read.csv("../input/gtd/globalterrorismdb_0617dist.csv")

data=rename(data,id=eventid,year=iyear,nation=country_txt,Region=region_txt,attack=attacktype1_txt,
            target=targtype1_txt,weapon=weaptype1_txt,Killed=nkill, wounded=nwound)
Data_c <- data%>%group_by(nation)%>%summarize(Attacks=n())

assylum_O <- assylum_sm%>%group_by(Origin)%>%summarize(Applications =sum(Value)/100)

Data_c <-Data_c%>%left_join(assylum_O,by=c("nation"="Origin"))

#No of terror attacks in a country does impact on the no of applications by refugees

p <- ggplot(Data_c,aes(x=Applications, y=Attacks,color=nation,size =Attacks))+
  geom_point()+
  theme(legend.position="none")+
  theme(axis.line = element_line(color = "orange",size=1.25))+
  theme(panel.background=element_blank())+
  ggtitle("No of terror attacks and applications in 100s by country of Origin")

ggplotly(p)



Data_cs <- Data_c%>%filter(!is.na(Applications))

ggplot(Data_cs,aes(x=Applications, y=Attacks))+
  geom_point()+
  geom_smooth(method = "loess")+
  stat_smooth(method = "loess")+
  theme(axis.line = element_line(color = "blue",size=1.25))+
  theme(panel.background=element_blank())+
  ggtitle("relation between terror attacks and refugees ")


#Refugee applications from Afghanistan by country of residence

assylum_A <- assylum_sm%>%filter(Origin=="Afghanistan")%>%group_by(residence,Origin)%>%summarize(Total=sum(Value))%>%arrange(desc(Total))


hchart(assylum_A,type="column",hcaes(x = residence, y = Total, color = Total)) %>%
  hc_title(text = "No of refugees from Afghanistan by country of residence") %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, text = "Data Source: UNHCR ", style = list(fontSize = "12px")) %>%
  hc_legend(enabled = FALSE)



assylum_I <- assylum_sm%>%filter(Origin=="Iraq")%>%group_by(residence,Origin)%>%summarize(Total=sum(Value))


hchart(assylum_I,type="treemap",hcaes(x = residence, value = Total, color = Total)) %>%
  hc_title(text = "No of refugees from Iraq by country of residence") %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, text = "Data Source: UNHCR ", style = list(fontSize = "12px")) %>%
  hc_legend(enabled = FALSE)