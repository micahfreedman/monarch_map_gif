#### How to make a .gif showing seasonal occurrence patterns of monarch butterflies

#load libraries

library(ggplot2); library(plyr); library(rnaturalearth); library(gganimate); library(gapminder); library(gifski); library(ggmap)

#download data

inat.monarchs <- read.csv(file = './observations-59866.csv')

head(inat.monarchs) #view data

names(inat.monarchs) #choose variables to retain and also filter out captive/reared individuals

inat.monarchs <- inat.monarchs[inat.monarchs$captive_cultivated=='false',]

columns.keep <- c('id','observed_on','quality_grade','place_guess','latitude','longitude','url')

inat.monarchs <- inat.monarchs[,columns.keep]

inat.monarchs <- inat.monarchs[inat.monarchs$quality_grade == 'research',] #restrict data to only research grade observations (those that have been independently confirmed)

inat.monarchs$julian.date <- format(as.Date(inat.monarchs$observed_on), "%j") #create a new column for Julian date of observation

inat.monarchs$julian.date <- as.numeric(inat.monarchs$julian.date) #convert this to a continuous numeric variable

inat.monarchs$month <- format(as.Date(inat.monarchs$observed_on), "%m")

inat.monarchs$month <- month.name[as.numeric(inat.monarchs$month)] #convert 

inat.monarchs$month <- factor(inat.monarchs$month, levels = c('January','February','March','April','May','June','July','August','September','October','November','December')) #rearrange so months are in chronological order


#specify observation IDs to exclude

urls.exclude <- c('https://www.inaturalist.org/observations/9373451','https://www.inaturalist.org/observations/20483568','https://www.inaturalist.org/observations/9879408','https://www.inaturalist.org/observations/20320860','https://www.inaturalist.org/observations/21799902','https://www.inaturalist.org/observations/19494799','https://www.inaturalist.org/observations/11410855','https://www.inaturalist.org/observations/5740325','https://www.inaturalist.org/observations/7469853','https://www.inaturalist.org/observations/18866685','https://www.inaturalist.org/observations/18357363','https://www.inaturalist.org/observations/19379295')

inat.monarchs <- inat.monarchs[-(inat.monarchs$url %in% urls.exclude),] #exclude the above observations

world <- ne_countries(scale= 'medium',returnclass = 'sf') #retrieve map layer (uses rnaturalearth package)

ggplot(data = world)+
  geom_sf()+
  theme_bw()+
  geom_point(data = inat.monarchs,  aes(x = longitude, y = latitude), size = 0.5, col = 'blue')+
  theme(legend.position = 'none')+
  theme(axis.title = element_blank()) #note: this only shows North American observations, since we restricted the iNaturalist observations to just those locations

nMap <- get_map("Topeka, Kansas",zoom=3,maptype="satellite",source="google")
ggmap(nMap)+
  theme_bw()+
  theme(axis.title = element_blank(), axis.text = element_blank())+
  geom_point(data = inat.monarchs, aes(x = longitude, y = latitude, col = month), size = 1)+
  scale_color_viridis_d() #note: in order for this to work, you first need to get an api key from Google; this now requires paying a nominal monthly fee but gives access to some really handy functions, such as geocode(), which returns decimal coordinates when passed a vector of character strings

##okay, now time to actually make the .gif that we want: 

#first define static plot with all data

(plot.static <- ggplot(data = world)+
  geom_sf()+
  theme_bw()+
  theme(axis.title = element_blank(), axis.text = element_blank())+
  xlim(c(-130,-60))+
  ylim(c(12,60))+
  theme(legend.position = 'none')+
  geom_point(data = inat.monarchs, 
             aes(x = longitude, y = latitude, col = julian.date), size = 1)+
  scale_color_viridis_c())

monarchs.anim <- plot.static + transition_time(julian.date) + 
  shadow_wake(wake_length = 0.5, alpha = FALSE)+
  shadow_mark(alpha = 0.4, size = 0.6)

#important: the transition_time argument can only process numeric variables

#transition_time argument specifies that we want to advance based on Julian day. By default, this seems to default to a max of 100 frames per .gif.
#shadow_wake argument leaves an "imprint" or "shadow" behind after points originally appear
#shadow_mark argument leaves behind a permanent point

monarchs.anim #display the .gif

anim_save("~./monarchs.gif", monarchs.anim, height = 6, width = 6, units = "in", res = 240) #export


### alternative -- plot the data according to the month of their occurrence; this time include a legend for month

(plot.static.month <- ggplot(data = world)+
    geom_sf()+
    theme_bw()+
    theme(axis.title = element_blank(), axis.text = element_blank())+
    xlim(c(-130,-60))+
    ylim(c(12,60))+
    geom_point(data = inat.monarchs, 
               aes(x = longitude, y = latitude, col = month), size = 1)+
    scale_color_viridis_d()) #same code as before, but with col = month and scale_color_viridis changed to discrete rather than continuous

monarchs.anim.month <- plot.static.month + transition_time(as.integer(month)) +
  shadow_wake(wake_length = 0.5, alpha = FALSE)+
  shadow_mark(alpha = 0.4, size = 0.6)+
  labs(title = 'Month: {frame_time}')
  
monarchs.anim.month






