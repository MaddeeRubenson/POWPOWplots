### Mt Hood Snow Plots
library(devtools)
#install_github("Gibbsdavidl/CatterPlots")
# library(CatterPlots)
# # x <- -10:10
# # y <- -x^2 + 10
# mthood_df <- mthood[mthood$`24HourSnowFall_inches` >0 & mthood$Location == "MeadowsBase",]
# mthood_df$DateTimePST <- as.numeric(as.Date(mthood_df$DateTimePST))
# mthood_df$Location <- as.character(mthood_df$Location)
# 
# purr <- catplot(xs=mthood_df$DateTimePST, ys=mthood_df$`24HourSnowFall_inches`, cat=3, catcolor='#000000FF', canvas = c(17553, 17555, 0, 11.5))
# cats(purr, -x, -y, cat=4, catcolor='#FF0000')
# 
# # for more fun ...
# meow <- multicat(xs=x, ys=y, cat=c(1,2,3), catcolor=list('#33FCFF','#FF0000'), canvas=c(-0.1,1.1, -0.1, 1.1))
# morecats(meow, x, 10*sin(x)+40, size=0.05, cat=c(4,5,6), catcolor=list('#0495EE','#EE7504'), type="line")
# 
# # random cats
# meow <- multicat(xs=x, ys=rnorm(21),
#                  cat=c(1,2,3,4,5,6,7,8,9,10),
#                  catcolor=list('#33FCFF'),
#                  canvas=c(-0.1,1.1, -0.1, 1.1),
#                  xlab="some cats", ylab="other cats", main="Random Cats")

# devtools::install_github("GuangchuangYu/emojifont")
# library(emojifont)
# list.emojifonts()
# load.emojifont('OpenSansEmoji.ttf')
# 
# http://guangchuangyu.github.io/2015/12/use-emoji-font-in-r/


SnowDepthPlot <- function(startdate = (Sys.Date() - 2), enddate = Sys.Date()) {
  #this function creates a plot of the snow depth at Meadows, Skibowl, and Timberline
  #inputs are start date and end date (%y-%m-%d), default set to present date and 15 days prior
  
  #https://www.nwac.us/data-portal/location/mt-hood/q?field_name=snow_depth&year=2017&custom_startdate=2017-12-17&custom_enddate=2017-12-19
  require(ggplot2)
  require(data.table)
  require(curl)
  
  url <- paste0('https://www.nwac.us/data-portal/csv/location/mt-hood/sensortype/snow_depth/start-date/', startdate, '/', 'end-date/', enddate, '/')  
  mthood <- fread(url)
  
  #mthood <- fread('https://www.nwac.us/data-portal/csv/location/mt-hood/sensortype/snow_depth/start-date/2017-12-17/end-date/2017-12-20/')
  
  names(mthood) <- c('DateTimePST', 'SkiBowl', 'MtHoodMeadows', 'TimberlineLodge')
  mthood$DateTimePST <- as.POSIXct(mthood$DateTimePST)
  mthood <- na.omit(mthood)
  
  g <- ggplot(data = mthood, aes()) +
    geom_line(aes(x = DateTimePST, y = SkiBowl, color = 'Ski Bowl Summit (5010 feet)')) +
    geom_line(aes(x = DateTimePST, y = MtHoodMeadows, color = 'Meadows Base(5380 feet)')) +
    geom_line(aes(x = DateTimePST, y = TimberlineLodge, color = 'Timberline Lodge (5880 feet)')) +
    ylab('Snow Depth (inches)') +
    xlab('Date/Time (PST)') + 
    theme_bw()+
    theme(legend.position = "top", legend.direction = "horizontal") +
    labs(colour = "") 
  
  return(g)
  
}

SnowDepthPlot()

snowfall <- function(startdate = (Sys.Date() - 2), enddate = Sys.Date()) {
  url <- paste0('https://www.nwac.us/data-portal/csv/location/mt-hood/sensortype/snowfall_24_hour/start-date/', startdate, '/', 'end-date/', enddate, '/')  
  mthood <- fread(url)
  
  names(mthood) <- c('DateTimePST', 'MeadowsBase', 'TimberlineLodge')
  mthood$DateTimePST <- as.POSIXct(mthood$DateTimePST)
  #mthood <- na.omit(mthood)
  
  mthood <- melt(mthood, id.vars = c('DateTimePST'))
  names(mthood) <- c('DateTimePST', 'Location', '24HourSnowFall_inches')
  y.lim <- c(0, max(mthood$`24HourSnowFall_inches`))
  
  g <- ggplot(mthood) +
    geom_point(aes(DateTimePST, `24HourSnowFall_inches`, color = Location)) +
    #geom_text(family="OpenSansEmoji", size=5) +
    ylim(y.lim) +
    ylab('24-hour Snowfall (inches)') +
    xlab('Date/Time (PST)') +
    theme_bw()+
    theme(legend.position = "top", legend.direction = "horizontal") +
    labs(colour = "") 
  g
  
}

snowfall()

Precip <- function(startdate = (Sys.Date() - 3), enddate = Sys.Date()) {
  url <- paste0('https://www.nwac.us/data-portal/csv/location/mt-hood/sensortype/precipitation/start-date/', startdate, '/', 'end-date/', enddate, '/')  
  mthood <- fread(url)
  
  names(mthood) <- c('DateTimePST','SkiBowl', 'MeadowsBase', 'TimberlineLodge')
  mthood$DateTimePST <- as.POSIXct(mthood$DateTimePST)
  #mthood <- na.omit(mthood)
  
  mthood <- melt(mthood, id.vars = c('DateTimePST'))
  names(mthood) <- c('DateTimePST', 'Location', 'Precip')
  y.lim <- c(0, max(mthood$Precip))
  
  g <- ggplot(mthood) +
    geom_line(aes(DateTimePST, Precip, color = Location)) +
    ylim(y.lim)
  g
  
}

Precip()
