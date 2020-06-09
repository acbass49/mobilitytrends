library(tidyverse)
library(maps)
library(lubridate)
library(extrafont)
library(plm)
library(RColorBrewer)

#This file is for vizualizing mobility, but is also in preparation for another project
#examining mobility effects on the coronavirus spread
#reading data
mobility <- read_csv("applemobilitytrends-2020-05-31.csv")
pop <- read_csv("county_pop.csv")
ncases <- read_csv("county_corona.csv")
state_codes <- read_csv("state_codes.csv")
urban <- read_csv("urban.csv")
county <- map_data("county")

#filtering urban and mobility dataset
urban <- urban %>% 
  select(`County name`, `2013 code`, `State Abr.`)
mobility<- mobility %>% 
  filter(country=="United States", geo_type=='county', transportation_type=='driving')

#joining population data with coronavirus cases data; then joining that with urban data
ncases <- left_join(ncases, pop, by=c("County Name"="County Name", "State"="State"))
ncases <- left_join(ncases, urban, by=c("County Name"="County name", "State"="State Abr."))

#joining state codes data with mobility(Other dataset State is coded "New York" instead of "NY")
mobility <- left_join(mobility, state_codes, by=c("sub-region"="Name"))

#generating long data from mobility and coronavirus cases data
mobility <- mobility %>% 
  gather(day, move, 7:146)
ncases <- ncases %>% 
  gather(day, cases, 5:137)

#creating cases variable per 10,000 population by county
ncases$cp10 <- ifelse(ncases$cases==0, 0, ncases$cases/(ncases$population/10000))

#joining coronavirus case data and mobility data
df <- left_join(ncases, mobility, by=c("County Name"="region", "State"="State", "day"="day"))

#finally joining in county data for maps
#in a separate Excel file i deleted the characters " County" then loaded it back into R
write.csv(df, "combined.csv")
df<- read.csv("combined.csv", header = T)

df$subregion <- tolower(df$County.Name)
df$region <- tolower(df$sub.region)
map <- inner_join(county, df, by=c("region","subregion"))

#recoding urban variable + creating urban dummy variable; grouping by week for smoother output
df1 <- map %>% 
  mutate(urbancode = ifelse(`X2013.code`==1,6, 
                            ifelse(`X2013.code`==2, 5, 
                                   ifelse(`X2013.code`==3, 4, 
                                          ifelse(`X2013.code`==4, 3,
                                                 ifelse(`X2013.code`==5, 2, 1))))),
         urban = ifelse(urbancode>3,1,0),
         date = floor_date(mdy(day), unit = "week", week_start = getOption("lubridate.week.start", 1))) %>% 
  group_by(countyFIPS.x, date) %>% 
  mutate(wmove = mean(move, na.rm = TRUE),
         wcases = sum(cases, na.rm = TRUE)) %>% 
  arrange(subregion, date)



#visualize
#need a figure showing urban rural trends over time
df1 %>% 
  group_by(urban, date) %>% 
  summarise(mmove = mean(wmove)) %>% 
  drop_na() %>% 
  mutate(furban = ifelse(urban==1,"urban", "rural")) %>% 
  ggplot(aes(x=date, y=mmove, group=furban))+
  geom_point(aes(color=furban))+
  geom_line(aes(color=furban))+
  scale_color_manual(values = c("darkred", "darkblue"))+
  theme_classic()+
  xlab("Date")+
  ylab("Mobility \n scaled so 100 is group baseline ")+
  ylim(25,200)+
  scale_fill_discrete(name = "")+
  labs(fill="", 
       caption = "Data source: Apple Maps",
       title = "Mobility Trends by Urban/Rural",
       subtitle = "Jan-May 2020")+
  theme(text = element_text(family = "Andale Mono", size=15) ,
      plot.title = element_text(hjust = .5, size = 25, family = "Andale Mono"),
      plot.subtitle = element_text(hjust = .5, size = 20),
      legend.title = element_blank())

#I want to include suburban too
df1 %>% 
  mutate(furban = ifelse(urbancode==1|urbancode==2,"rural", 
                         ifelse(urbancode==3|urbancode==4, "suburban", "urban"))) %>% 
  group_by(furban, date) %>% 
  summarise(mmove = mean(wmove)) %>% 
  drop_na() %>% 
  ggplot(aes(x=date, y=mmove, group=furban))+
  geom_point(aes(color=furban))+
  geom_line(aes(color=furban))+
  scale_color_manual(values = c("darkred", "goldenrod", "darkblue"))+
  theme_classic()+
  xlab("Date")+
  ylab("Mobility \n scaled so 100 is group baseline ")+
  ylim(25,200)+
  scale_fill_discrete(name = "")+
  labs(fill="", 
       caption = "Data source: Apple Maps",
       title = "Mobility Trends by Urban/Rural",
       subtitle = "Jan-May 2020")+
  geom_hline(yintercept = 100)+
  theme(text = element_text(family = "Andale Mono", size=15) ,
        plot.title = element_text(hjust = .5, size = 25),
        plot.subtitle = element_text(hjust = .5, size = 20),
        legend.title = element_blank(),
        plot.caption.position = "plot")



mobility <- read_csv("applemobilitytrends-2020-05-31.csv")

lmobility <- mobility %>% 
  gather(day, value, 7:146)

lmobility %>% 
  filter(country=="United States", geo_type=='sub-region', transportation_type=='driving') %>%
  mutate(date = floor_date(mdy(day), unit = "week", week_start = getOption("lubridate.week.start", 1))) %>% 
  group_by(region, date)  %>% 
  summarise(mean = mean(value, na.rm = TRUE)) %>% 
  arrange(desc(region, date)) %>% 
  ggplot(aes(x=date, y=mean, group=region))+
  geom_line(aes(color=region))+
  geom_point(aes(color=region), size=0.75)+
  geom_hline(yintercept = 100)+
  theme_classic()+
  xlab("Date")+
  ylab("Mobility \n scaled so 100 is group baseline ")+
  labs(fill="", 
       caption = "Data source: Apple Maps",
       title = "Mobility Trends for 50 States",
       subtitle = "Jan-May 2020")+
  theme(text = element_text(family = "Andale Mono", size=15) ,
        plot.title = element_text(hjust = .5, size = 25),
        plot.subtitle = element_text(hjust = .5, size = 20),
        legend.position = "none",
        plot.caption.position = "plot")


