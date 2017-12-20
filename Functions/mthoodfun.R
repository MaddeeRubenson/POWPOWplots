### Mt Hood Snow Depth Plot
#this function creates a plot of the snow depth at Meadows, Skibowl, and Timberline
#inputs are start date and end date (%y-%m-%d), default set to present date and 15 days prior

SnowDepthPlot <- function(startdate = (Sys.Date() - 15), enddate = Sys.Date()) {

#https://www.nwac.us/data-portal/location/mt-hood/q?field_name=snow_depth&year=2017&custom_startdate=2017-12-17&custom_enddate=2017-12-19
library(ggplot2)
library(data.table)

url <- paste0('https://www.nwac.us/data-portal/csv/location/mt-hood/sensortype/snow_depth/start-date/', startdate, '/', 'end-date/', enddate, '/')  
mthood <- fread(url)
  
#mthood <- fread('https://www.nwac.us/data-portal/csv/location/mt-hood/sensortype/snow_depth/start-date/2017-12-17/end-date/2017-12-20/')

names(mthood) <- c('DateTimePST', 'SkiBowl', 'MtHoodMeadows', 'TimberlineLodge')
mthood$DateTimePST <- as.POSIXct(mthood$DateTimePST)
mthood <- na.omit(mthood)

g <- ggplot(data = mthood, aes()) +
  geom_point(aes(x = DateTimePST, y = SkiBowl, color = 'Ski Bowl Summit (5010 feet)')) +
  geom_point(aes(x = DateTimePST, y = MtHoodMeadows, color = 'Meadows Base(5380 feet)')) +
  geom_point(aes(x = DateTimePST, y = TimberlineLodge, color = 'Timberline Lodge (5880 feet)')) +
  ylab('Snow Depth (inches)') +
  xlab('Date/Time (PST)') + 
  theme_bw()+
  theme(legend.position = "top", legend.direction = "horizontal") +
  labs(colour = "") 

return(g)

}

#plot <- SnowDepthPlot(startdate = "2017-12-01", enddate = "2017-12-20")
