Monarch occurrence .gif tutorial
================
Micah Freedman
7/30/2019

This is a quick tutorial on how to make an animated map of monarch butterfly occurrence data over the course of a year in North America. It uses data from iNaturalist (<https://www.inaturalist.org/home>) and is based on another tutorial from DataNovia (<https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/>).
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

### Begin by downloading observations from iNaturalist. Use this link: <https://www.inaturalist.org/observations/export>. For this demonstration, I specified only observations from North America and entered "Danaus plexippus" in the "Taxon" search field. Besides that, I left all of the default options. This dataset contains about 50,000 observations and took about 10 minutes to compile and download. Once it's ready, you will be directed to a link that has a zipped folder which contains a .csv file that has all of the data. The .csv file should have a name like "observations-59866.csv".

### Establish directory (put the downloaded .csv file here; all outputs will also go here)

``` r
knitr::opts_knit$set(root.dir = '../monarch_map_gif/')
```

### Load necessary libraries; if you don't have these installed yet, you'll have to do that first using install.packages('name\_of\_package')

``` r
library(ggplot2); library(plyr); library(rnaturalearth); library(gganimate); library(gapminder); library(gifski)
```

### Load the data and take a look. This could take a little while, depending on the size of the file.

``` r
inat.monarchs <- read.csv(file = './observations-59866.csv')

head(inat.monarchs)
```

    ##      id observed_on_string observed_on time_observed_at
    ## 1  1813         2009-03-01  2009-03-01                 
    ## 2  3340         2009-09-13  2009-09-13                 
    ## 3  6171         2009-06-08  2009-06-08                 
    ## 4  8128         2010-08-28  2010-08-28                 
    ## 5 13261         2009-06-09  2009-06-09                 
    ## 6 14880  November 18, 2010  2010-11-18                 
    ##                    time_zone out_of_range user_id      user_login
    ## 1 Eastern Time (US & Canada)                  169         flapack
    ## 2 Eastern Time (US & Canada)                  169         flapack
    ## 3 Pacific Time (US & Canada)                  382         tsoleau
    ## 4 Eastern Time (US & Canada)                   19    justinscioli
    ## 5 Pacific Time (US & Canada)                  714 curiousgeorge61
    ## 6 Eastern Time (US & Canada)                 1000            muir
    ##                created_at              updated_at quality_grade  license
    ## 1 2009-03-03 05:32:59 UTC 2019-01-31 01:37:29 UTC      research CC-BY-NC
    ## 2 2009-09-19 01:43:13 UTC 2018-03-26 19:35:46 UTC      research CC-BY-NC
    ## 3 2010-03-23 05:46:37 UTC 2017-12-13 21:04:44 UTC      research         
    ## 4 2010-08-29 19:31:35 UTC 2017-12-13 21:05:11 UTC      research         
    ## 5 2011-03-24 20:49:20 UTC 2017-12-13 21:06:16 UTC      research CC-BY-NC
    ## 6 2011-04-20 00:43:42 UTC 2017-12-13 21:06:38 UTC      research CC-BY-NC
    ##                                             url
    ## 1  http://www.inaturalist.org/observations/1813
    ## 2  http://www.inaturalist.org/observations/3340
    ## 3  http://www.inaturalist.org/observations/6171
    ## 4  http://www.inaturalist.org/observations/8128
    ## 5 http://www.inaturalist.org/observations/13261
    ## 6 http://www.inaturalist.org/observations/14880
    ##                                                           image_url
    ## 1  https://static.inaturalist.org/photos/2182/medium.jpg?1444937202
    ## 2  https://static.inaturalist.org/photos/5295/medium.jpg?1444497296
    ## 3 https://static.inaturalist.org/photos/12387/medium.jpg?1444282344
    ## 4 https://static.inaturalist.org/photos/18079/medium.jpg?1444294036
    ## 5            https://static.inaturalist.org/photos/26215/medium.JPG
    ## 6            https://static.inaturalist.org/photos/28970/medium.JPG
    ##   sound_url                             tag_list
    ## 1        NA                                     
    ## 2        NA                                     
    ## 3        NA                                     
    ## 4        NA Monarch, danaus plexippus, butterfly
    ## 5        NA                      June, butterfly
    ## 6        NA                                     
    ##                                                                                                                                   description
    ## 1                                                                                                                                            
    ## 2                                                                                                                                            
    ## 3                                                                                                                                            
    ## 4 Tons of butterflies were everywhere in Eastern Pennsylvania. This monarch was at the edge of a corn field where some milkweed was growing. 
    ## 5                                                                                                                                            
    ## 6                                                                                                                                            
    ##   id_please num_identification_agreements num_identification_disagreements
    ## 1     false                             4                                0
    ## 2     false                             3                                0
    ## 3     false                             2                                0
    ## 4     false                             3                                0
    ## 5     false                             2                                0
    ## 6     false                             2                                0
    ##   captive_cultivated oauth_application_id             place_guess latitude
    ## 1              false                   NA   Pine Island Ridge, FL 26.07879
    ## 2              false                   NA   Pine Island Ridge, FL 26.08047
    ## 3              false                   NA             Cambria, CA 35.55769
    ## 4              false                   NA           Adamstown, PA 40.24120
    ## 5              false                   NA Pepperwood Preserve, CA 38.56666
    ## 6              false                   NA          Santa Cruz, CA 36.95401
    ##    longitude positional_accuracy geoprivacy taxon_geoprivacy
    ## 1  -80.27837                  NA                            
    ## 2  -80.27792                  NA                            
    ## 3 -121.08159                  NA                            
    ## 4  -76.05633                  NA                            
    ## 5 -122.68481                  NA                            
    ## 6 -122.02724                  NA                            
    ##   coordinates_obscured positioning_method positioning_device species_guess
    ## 1                false                                             Monarch
    ## 2                false                                             Monarch
    ## 3                false                                            Monarque
    ## 4                false                                            Monarque
    ## 5                false                                            Monarque
    ## 6                false                                            Monarque
    ##    scientific_name common_name iconic_taxon_name taxon_id
    ## 1 Danaus plexippus     Monarch           Insecta    48662
    ## 2 Danaus plexippus     Monarch           Insecta    48662
    ## 3 Danaus plexippus     Monarch           Insecta    48662
    ## 4 Danaus plexippus     Monarch           Insecta    48662
    ## 5 Danaus plexippus     Monarch           Insecta    48662
    ## 6 Danaus plexippus     Monarch           Insecta    48662

``` r
names(inat.monarchs)
```

    ##  [1] "id"                               "observed_on_string"              
    ##  [3] "observed_on"                      "time_observed_at"                
    ##  [5] "time_zone"                        "out_of_range"                    
    ##  [7] "user_id"                          "user_login"                      
    ##  [9] "created_at"                       "updated_at"                      
    ## [11] "quality_grade"                    "license"                         
    ## [13] "url"                              "image_url"                       
    ## [15] "sound_url"                        "tag_list"                        
    ## [17] "description"                      "id_please"                       
    ## [19] "num_identification_agreements"    "num_identification_disagreements"
    ## [21] "captive_cultivated"               "oauth_application_id"            
    ## [23] "place_guess"                      "latitude"                        
    ## [25] "longitude"                        "positional_accuracy"             
    ## [27] "geoprivacy"                       "taxon_geoprivacy"                
    ## [29] "coordinates_obscured"             "positioning_method"              
    ## [31] "positioning_device"               "species_guess"                   
    ## [33] "scientific_name"                  "common_name"                     
    ## [35] "iconic_taxon_name"                "taxon_id"

### Filter out captive reared and non-research-grade observations.

``` r
inat.monarchs <- inat.monarchs[inat.monarchs$captive_cultivated=='false',]

inat.monarchs <- inat.monarchs[inat.monarchs$quality_grade == 'research',]
```

### Pick which columns to keep

``` r
columns.keep <- c('id','observed_on','quality_grade','place_guess','latitude','longitude','url')

inat.monarchs <- inat.monarchs[,columns.keep]
```

### Get date information into the proper format

``` r
inat.monarchs$julian.date <- format(as.Date(inat.monarchs$observed_on), "%j") #create a new column for Julian day of observation

inat.monarchs$julian.date <- as.numeric(inat.monarchs$julian.date) #convert this to a continuous numeric variable

inat.monarchs$month <- format(as.Date(inat.monarchs$observed_on), "%m")

inat.monarchs$month <- revalue(inat.monarchs$month, c('01' = 'January', '02' = 'February', '03' = 'March', '04' = 'April', '05' = 'May', '06' = 'June', '07' = 'July', '08' = 'August', '09' = 'September', '10' = 'October', '11' = 'November', '12' = 'December')) #assign names for months (revalue function requires plyr library)

inat.monarchs$month <- factor(inat.monarchs$month, levels = c('January','February','March','April','May','June','July','August','September','October','November','December')) #rearrange so months are in chronological order
```

### Specify observations to exclude. These are some observations that, despite being research grade, are not relevant, such as dead monarchs found in melted snow. I've almost certainly missed a few; this list can be updated.

``` r
urls.exclude <- c('https://www.inaturalist.org/observations/9373451','https://www.inaturalist.org/observations/20483568','https://www.inaturalist.org/observations/9879408','https://www.inaturalist.org/observations/20320860','https://www.inaturalist.org/observations/21799902','https://www.inaturalist.org/observations/19494799','https://www.inaturalist.org/observations/11410855','https://www.inaturalist.org/observations/5740325','https://www.inaturalist.org/observations/7469853','https://www.inaturalist.org/observations/18866685','https://www.inaturalist.org/observations/18357363','https://www.inaturalist.org/observations/19379295')

inat.monarchs <- inat.monarchs[-(inat.monarchs$url %in% urls.exclude),] #exclude the above observations
```

### Plot observations onto a world map. Note: observations here are only from North America, since that is what I specified when I downloaded the data from iNaturalist.

``` r
world <- ne_countries(scale= 'medium',returnclass = 'sf') #retrieve map layer (uses rnaturalearth package)

ggplot(data = world)+
  geom_sf()+
  theme_bw()+
  geom_point(data = inat.monarchs,  aes(x = longitude, y = latitude), size = 0.5, col = 'blue')+
  theme(legend.position = 'none')+
  theme(axis.title = element_blank())
```

![](monarch_gif_tutorial_files/figure-markdown_github/unnamed-chunk-7-1.png)

### Okay, now it's time to make the actual .gif itself.

### First, we need to define a static plot that contains all of the data.

``` r
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
```

    ## Warning: Removed 93 rows containing missing values (geom_point).

![](monarch_gif_tutorial_files/figure-markdown_github/unnamed-chunk-8-1.png)

### Now use this map template to generate an animated .gif

#### Arguments and what they do:

#### transition\_time() specifies what to use for indexing / what order to put plots in

#### shadow\_wake() will leave a "trail" or "imprint" behind after each point appears, with the time that it remains visible defined by the wake\_length argument

#### shadow\_mark() will leave a permanent mark after a point appear.

``` r
monarchs.anim <- plot.static + transition_time(julian.date) +
  shadow_wake(wake_length = 0.5, alpha = FALSE)+
  shadow_mark(alpha = 0.4, size = 0.6)
```

### Display the .gif!!!

``` r
monarchs.anim
```

    ## Warning: Removed 5 rows containing missing values (geom_point).

    ## Warning: Removed 6 rows containing missing values (geom_point).

    ## Warning: Removed 8 rows containing missing values (geom_point).

    ## Warning: Removed 9 rows containing missing values (geom_point).

    ## Warning: Removed 11 rows containing missing values (geom_point).

    ## Warning: Removed 11 rows containing missing values (geom_point).

    ## Warning: Removed 13 rows containing missing values (geom_point).

    ## Warning: Removed 14 rows containing missing values (geom_point).

    ## Warning: Removed 15 rows containing missing values (geom_point).

    ## Warning: Removed 16 rows containing missing values (geom_point).

    ## Warning: Removed 17 rows containing missing values (geom_point).

    ## Warning: Removed 17 rows containing missing values (geom_point).

    ## Warning: Removed 20 rows containing missing values (geom_point).

    ## Warning: Removed 21 rows containing missing values (geom_point).

    ## Warning: Removed 22 rows containing missing values (geom_point).

    ## Warning: Removed 22 rows containing missing values (geom_point).

    ## Warning: Removed 22 rows containing missing values (geom_point).

    ## Warning: Removed 25 rows containing missing values (geom_point).

    ## Warning: Removed 27 rows containing missing values (geom_point).

    ## Warning: Removed 29 rows containing missing values (geom_point).

    ## Warning: Removed 31 rows containing missing values (geom_point).

    ## Warning: Removed 33 rows containing missing values (geom_point).

    ## Warning: Removed 34 rows containing missing values (geom_point).

    ## Warning: Removed 35 rows containing missing values (geom_point).

    ## Warning: Removed 36 rows containing missing values (geom_point).

    ## Warning: Removed 38 rows containing missing values (geom_point).

    ## Warning: Removed 44 rows containing missing values (geom_point).

    ## Warning: Removed 47 rows containing missing values (geom_point).

    ## Warning: Removed 47 rows containing missing values (geom_point).

    ## Warning: Removed 49 rows containing missing values (geom_point).

    ## Warning: Removed 50 rows containing missing values (geom_point).

    ## Warning: Removed 51 rows containing missing values (geom_point).

    ## Warning: Removed 52 rows containing missing values (geom_point).

    ## Warning: Removed 53 rows containing missing values (geom_point).

    ## Warning: Removed 53 rows containing missing values (geom_point).

    ## Warning: Removed 53 rows containing missing values (geom_point).

    ## Warning: Removed 53 rows containing missing values (geom_point).

    ## Warning: Removed 53 rows containing missing values (geom_point).

    ## Warning: Removed 53 rows containing missing values (geom_point).

    ## Warning: Removed 54 rows containing missing values (geom_point).

    ## Warning: Removed 55 rows containing missing values (geom_point).

    ## Warning: Removed 55 rows containing missing values (geom_point).

    ## Warning: Removed 56 rows containing missing values (geom_point).

    ## Warning: Removed 57 rows containing missing values (geom_point).

    ## Warning: Removed 58 rows containing missing values (geom_point).

    ## Warning: Removed 59 rows containing missing values (geom_point).

    ## Warning: Removed 61 rows containing missing values (geom_point).

    ## Warning: Removed 61 rows containing missing values (geom_point).

    ## Warning: Removed 62 rows containing missing values (geom_point).

    ## Warning: Removed 63 rows containing missing values (geom_point).

    ## Warning: Removed 63 rows containing missing values (geom_point).

    ## Warning: Removed 64 rows containing missing values (geom_point).

    ## Warning: Removed 67 rows containing missing values (geom_point).

    ## Warning: Removed 67 rows containing missing values (geom_point).

    ## Warning: Removed 69 rows containing missing values (geom_point).

    ## Warning: Removed 75 rows containing missing values (geom_point).

    ## Warning: Removed 75 rows containing missing values (geom_point).

    ## Warning: Removed 75 rows containing missing values (geom_point).

    ## Warning: Removed 75 rows containing missing values (geom_point).

    ## Warning: Removed 75 rows containing missing values (geom_point).

    ## Warning: Removed 76 rows containing missing values (geom_point).

    ## Warning: Removed 76 rows containing missing values (geom_point).

    ## Warning: Removed 77 rows containing missing values (geom_point).

    ## Warning: Removed 77 rows containing missing values (geom_point).

    ## Warning: Removed 77 rows containing missing values (geom_point).

    ## Warning: Removed 77 rows containing missing values (geom_point).

    ## Warning: Removed 77 rows containing missing values (geom_point).

    ## Warning: Removed 79 rows containing missing values (geom_point).

    ## Warning: Removed 79 rows containing missing values (geom_point).

    ## Warning: Removed 79 rows containing missing values (geom_point).

    ## Warning: Removed 79 rows containing missing values (geom_point).

    ## Warning: Removed 80 rows containing missing values (geom_point).

    ## Warning: Removed 81 rows containing missing values (geom_point).

    ## Warning: Removed 81 rows containing missing values (geom_point).

    ## Warning: Removed 81 rows containing missing values (geom_point).

    ## Warning: Removed 81 rows containing missing values (geom_point).

    ## Warning: Removed 81 rows containing missing values (geom_point).

    ## Warning: Removed 81 rows containing missing values (geom_point).

    ## Warning: Removed 82 rows containing missing values (geom_point).

    ## Warning: Removed 83 rows containing missing values (geom_point).

    ## Warning: Removed 83 rows containing missing values (geom_point).

    ## Warning: Removed 83 rows containing missing values (geom_point).

    ## Warning: Removed 84 rows containing missing values (geom_point).

    ## Warning: Removed 86 rows containing missing values (geom_point).

    ## Warning: Removed 87 rows containing missing values (geom_point).

    ## Warning: Removed 87 rows containing missing values (geom_point).

    ## Warning: Removed 87 rows containing missing values (geom_point).

    ## Warning: Removed 87 rows containing missing values (geom_point).

    ## Warning: Removed 88 rows containing missing values (geom_point).

    ## Warning: Removed 88 rows containing missing values (geom_point).

    ## Warning: Removed 88 rows containing missing values (geom_point).

    ## Warning: Removed 89 rows containing missing values (geom_point).

    ## Warning: Removed 89 rows containing missing values (geom_point).

    ## Warning: Removed 89 rows containing missing values (geom_point).

    ## Warning: Removed 89 rows containing missing values (geom_point).

    ## Warning: Removed 89 rows containing missing values (geom_point).

    ## Warning: Removed 89 rows containing missing values (geom_point).

    ## Warning: Removed 92 rows containing missing values (geom_point).

    ## Warning: Removed 93 rows containing missing values (geom_point).

![](monarch_gif_tutorial_files/figure-markdown_github/unnamed-chunk-10-1.gif)

### Export the file in .gif format

``` r
anim_save("../monarchs.gif", monarchs.anim, height = 8, width = 8, units = "in", res = 300)
```

    ## Warning: Removed 5 rows containing missing values (geom_point).

    ## Warning: Removed 6 rows containing missing values (geom_point).

    ## Warning: Removed 8 rows containing missing values (geom_point).

    ## Warning: Removed 9 rows containing missing values (geom_point).

    ## Warning: Removed 11 rows containing missing values (geom_point).

    ## Warning: Removed 11 rows containing missing values (geom_point).

    ## Warning: Removed 13 rows containing missing values (geom_point).

    ## Warning: Removed 14 rows containing missing values (geom_point).

    ## Warning: Removed 15 rows containing missing values (geom_point).

    ## Warning: Removed 16 rows containing missing values (geom_point).

    ## Warning: Removed 17 rows containing missing values (geom_point).

    ## Warning: Removed 17 rows containing missing values (geom_point).

    ## Warning: Removed 20 rows containing missing values (geom_point).

    ## Warning: Removed 21 rows containing missing values (geom_point).

    ## Warning: Removed 22 rows containing missing values (geom_point).

    ## Warning: Removed 22 rows containing missing values (geom_point).

    ## Warning: Removed 22 rows containing missing values (geom_point).

    ## Warning: Removed 25 rows containing missing values (geom_point).

    ## Warning: Removed 27 rows containing missing values (geom_point).

    ## Warning: Removed 29 rows containing missing values (geom_point).

    ## Warning: Removed 31 rows containing missing values (geom_point).

    ## Warning: Removed 33 rows containing missing values (geom_point).

    ## Warning: Removed 34 rows containing missing values (geom_point).

    ## Warning: Removed 35 rows containing missing values (geom_point).

    ## Warning: Removed 36 rows containing missing values (geom_point).

    ## Warning: Removed 38 rows containing missing values (geom_point).

    ## Warning: Removed 44 rows containing missing values (geom_point).

    ## Warning: Removed 47 rows containing missing values (geom_point).

    ## Warning: Removed 47 rows containing missing values (geom_point).

    ## Warning: Removed 49 rows containing missing values (geom_point).

    ## Warning: Removed 50 rows containing missing values (geom_point).

    ## Warning: Removed 51 rows containing missing values (geom_point).

    ## Warning: Removed 52 rows containing missing values (geom_point).

    ## Warning: Removed 53 rows containing missing values (geom_point).

    ## Warning: Removed 53 rows containing missing values (geom_point).

    ## Warning: Removed 53 rows containing missing values (geom_point).

    ## Warning: Removed 53 rows containing missing values (geom_point).

    ## Warning: Removed 53 rows containing missing values (geom_point).

    ## Warning: Removed 53 rows containing missing values (geom_point).

    ## Warning: Removed 54 rows containing missing values (geom_point).

    ## Warning: Removed 55 rows containing missing values (geom_point).

    ## Warning: Removed 55 rows containing missing values (geom_point).

    ## Warning: Removed 56 rows containing missing values (geom_point).

    ## Warning: Removed 57 rows containing missing values (geom_point).

    ## Warning: Removed 58 rows containing missing values (geom_point).

    ## Warning: Removed 59 rows containing missing values (geom_point).

    ## Warning: Removed 61 rows containing missing values (geom_point).

    ## Warning: Removed 61 rows containing missing values (geom_point).

    ## Warning: Removed 62 rows containing missing values (geom_point).

    ## Warning: Removed 63 rows containing missing values (geom_point).

    ## Warning: Removed 63 rows containing missing values (geom_point).

    ## Warning: Removed 64 rows containing missing values (geom_point).

    ## Warning: Removed 67 rows containing missing values (geom_point).

    ## Warning: Removed 67 rows containing missing values (geom_point).

    ## Warning: Removed 69 rows containing missing values (geom_point).

    ## Warning: Removed 75 rows containing missing values (geom_point).

    ## Warning: Removed 75 rows containing missing values (geom_point).

    ## Warning: Removed 75 rows containing missing values (geom_point).

    ## Warning: Removed 75 rows containing missing values (geom_point).

    ## Warning: Removed 75 rows containing missing values (geom_point).

    ## Warning: Removed 76 rows containing missing values (geom_point).

    ## Warning: Removed 76 rows containing missing values (geom_point).

    ## Warning: Removed 77 rows containing missing values (geom_point).

    ## Warning: Removed 77 rows containing missing values (geom_point).

    ## Warning: Removed 77 rows containing missing values (geom_point).

    ## Warning: Removed 77 rows containing missing values (geom_point).

    ## Warning: Removed 77 rows containing missing values (geom_point).

    ## Warning: Removed 79 rows containing missing values (geom_point).

    ## Warning: Removed 79 rows containing missing values (geom_point).

    ## Warning: Removed 79 rows containing missing values (geom_point).

    ## Warning: Removed 79 rows containing missing values (geom_point).

    ## Warning: Removed 80 rows containing missing values (geom_point).

    ## Warning: Removed 81 rows containing missing values (geom_point).

    ## Warning: Removed 81 rows containing missing values (geom_point).

    ## Warning: Removed 81 rows containing missing values (geom_point).

    ## Warning: Removed 81 rows containing missing values (geom_point).

    ## Warning: Removed 81 rows containing missing values (geom_point).

    ## Warning: Removed 81 rows containing missing values (geom_point).

    ## Warning: Removed 82 rows containing missing values (geom_point).

    ## Warning: Removed 83 rows containing missing values (geom_point).

    ## Warning: Removed 83 rows containing missing values (geom_point).

    ## Warning: Removed 83 rows containing missing values (geom_point).

    ## Warning: Removed 84 rows containing missing values (geom_point).

    ## Warning: Removed 86 rows containing missing values (geom_point).

    ## Warning: Removed 87 rows containing missing values (geom_point).

    ## Warning: Removed 87 rows containing missing values (geom_point).

    ## Warning: Removed 87 rows containing missing values (geom_point).

    ## Warning: Removed 87 rows containing missing values (geom_point).

    ## Warning: Removed 88 rows containing missing values (geom_point).

    ## Warning: Removed 88 rows containing missing values (geom_point).

    ## Warning: Removed 88 rows containing missing values (geom_point).

    ## Warning: Removed 89 rows containing missing values (geom_point).

    ## Warning: Removed 89 rows containing missing values (geom_point).

    ## Warning: Removed 89 rows containing missing values (geom_point).

    ## Warning: Removed 89 rows containing missing values (geom_point).

    ## Warning: Removed 89 rows containing missing values (geom_point).

    ## Warning: Removed 89 rows containing missing values (geom_point).

    ## Warning: Removed 92 rows containing missing values (geom_point).

    ## Warning: Removed 93 rows containing missing values (geom_point).
