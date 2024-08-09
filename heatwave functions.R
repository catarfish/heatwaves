
library(lubridate)
date_slider<-function(date){
  out<-sort(unique(yday(seq(date-days(15), date+days(15), by="1 days"))))
  if(length(out)!=31){
    stop("something went wrong and length isn't 31")
  }
  return(out)
}

heatwave_detector<-function(temp, threshold){
  r<-rle(temp>threshold)
  r$values<-r$lengths >= 3 & r$values
  out<-inverse.rle(r)
  return(out)
}
