###################Case-fatality Rate of COVID-19####################
#                      Mohammad Nayeem Hasan                        #
#####################################################################

library(MASS)
require(foreign)
require(ggplot2)
require(maptools)
library(tidyverse)


####Data Processing#######

COVID <- read.csv("E:\\Study\\ResearchProject\\Jamal Sir\\COVID19-Factors\\data\\New\\owid-covid-data.csv")
COVID
COVID <- subset(COVID, COVID$date >= "2020-01-01") #1/1/2020
COVID <- subset(COVID, COVID$date <= "2020-12-31") #8/31/2020
COVID<-COVID[!(COVID$location=="World" | COVID$location=="International"),]

head(COVID)
COVID$date
countries <- read.csv("E:\\Study\\ResearchProject\\Jamal Sir\\COVID19-Factors\\data\\New\\countries.csv")

Day <- read.csv("E:\\Study\\ResearchProject\\Jamal Sir\\COVID19-Factors\\data\\New\\Day.csv")
GHSI <- read.csv("E:\\Study\\ResearchProject\\Jamal Sir\\COVID19-Factors\\data\\New\\GHSI.csv")
GHSI[GHSI == 0] <- NA
GHSI


WGI <- read.csv("E:\\Study\\ResearchProject\\Jamal Sir\\COVID19-Factors\\data\\New\\WGI.csv")
WGI[WGI == 0] <- NA
WGI

CVD <- read.csv("E:\\Study\\ResearchProject\\Jamal Sir\\COVID19-Factors\\data\\New\\CVD.csv")
CVD[CVD == 0] <- NA
CVD

finaldt1 <- merge(Day, GHSI,  by="location")

finaldt2 <- merge(countries, WGI,  by="location")
finaldt3 <- merge(finaldt1, finaldt2,  by="location")

finaldt4 <- merge(finaldt3, CVD,  by="location")

####Overall#######

COVID_june30 <- subset(COVID, COVID$date == "2020-12-31") #8/31/2020
str(COVID_june30)

finaldt_june30 <- merge(COVID_june30, finaldt4,  by="location")


####Before Pick#######

COVID_April21 <- subset(COVID, COVID$date == "4/26/2020")


final_April21 <- merge(COVID_April21, finaldt4,  by="location")

####MAP total deaths per million
tiff("E:\\Study\\ResearchProject\\Jamal Sir\\COVID19-Factors\\data\\New\\CFRMap.tiff", units="in", width=6, height=4, res=300)

COVID_june30$CFR <- (COVID_june30$total_deaths / COVID_june30$total_cases)*100
COVID_june30$cfrlog <- log(COVID_june30$CFR+1)
COVID_june30$cfrlog[COVID_june30$cfrlog == -Inf] <- NA

library(dplyr)
library(stringr)
library(ggplot2)
library(maps)

options(scipen = 999) ## To disable scientific notation



world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

#log

COVID_june30$cfrlog <- log10(COVID_june30$CFR*100) +1

worldgovt <- dplyr::select(COVID_june30, region = location, cfrlog = cfrlog, "CC" =  iso_code)
head(worldgovt)

diff <- setdiff(world$region, worldgovt$region)

## Clean the dataset accordingly
worldgovt <- worldgovt %>% 
  mutate(region = recode(str_trim(region), "United States" = "USA",
                         "United Kingdom" = "UK",
                         "Korea (Rep.)" = "South Korea",
                         "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
                         "Congo (Rep.)" = "Republic of Congo")) %>%
  ## Editing the "North Korea" entry is a little trickier for some reason
  mutate(region = case_when((CC == "PRK") ~ "North Korea",
                            TRUE ~ as.character(.$region)))

worldSubset <- inner_join(world, worldgovt, by = "region")

## Make the HDI numeric
worldgovt$CFR <- as.numeric(as.character(worldgovt$cfrlog))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldCFR <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = CFR)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("COVID-19 reported Case-fatality rate (%) by country") + labs(fill = "CFR (%)") +
  plain
plot(worldCFR)

dev.off()


#####CFR T-series##########

tiff("E:\\Study\\ResearchProject\\Jamal Sir\\COVID19-Factors\\data\\New\\CFRNN.tiff", units="in", width=6, height=7, res=600)

library(ggplot2)
theme_set(theme_minimal())

#COVID$date_new <- as.Date(COVID$date, "%m/%d/%Y")

#COVID$week <- strftime(COVID$date_new, format = "%V")
#COVID$CFR <- (COVID$total_deaths/COVID$total_cases)*100
#agg = aggregate(COVID$CFR,
#                by = list(COVID$week),
#                FUN = mean, na.omit = T)

world <- read.csv("E:\\Study\\ResearchProject\\Jamal Sir\\COVID19-Factors\\data\\New\\worldcfrWeekly.csv")
#ver<-data.frame(years=c(17,24),labels=c("Pick","Medicine"))
p <- ggplot(data = world, aes(x = ï..Week, y = CFR_mean))+
  geom_line(color = "black", size = 1) +labs(y="Reported CFR (%)", x = "Weeks")
p +geom_line(arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed"), size=1)
p <- p + annotate("segment", x = 17.2, xend = 17.2, y = 7.41, yend = 7.4, colour = "red", size=1, alpha=1, arrow=arrow()) + scale_x_continuous(breaks = seq(from = 0, to = 55, by = 5)) + scale_y_continuous(breaks = seq(from = 0, to = 8, by = 1))
p
  #p <- p +  geom_vline(aes(xintercept=years, color=labels),data=ver, size=1, show.legend=T) + 
  #scale_color_manual("Vertical Lines", values=c("Pick"="sienna","Medicine"="skyblue"))
#p + theme(legend.position = c(0.2, 0.8),legend.background = element_rect(fill="white",
#                                                                         size=0.5, 
#                                                                         linetype="solid", 
#                                                                         colour ="darkblue"))


#######CFR T sires WHO##########
library(ggplot2)
library(scales) 

whocfr <- read.csv("E:\\Study\\ResearchProject\\Jamal Sir\\COVID19-Factors\\data\\New\\WHO\\whocfr.csv")

whocfr$WHO.Region
whocfr$Week
whocfr$CFR_mean


p1 <- ggplot() + geom_line(aes(y = CFR_mean, x = Week, colour = WHO.Region), size=1,
                           data = whocfr, stat="identity")  +labs(y="Reported CFR (%)", x = "Weeks")+
  scale_color_brewer(palette = "Paired") + scale_x_continuous(breaks = seq(from = 0, to = 55, by = 5)) + scale_y_continuous(breaks = seq(from = 0, to = 25, by = 5)) + theme(legend.position="bottom")

p1

#geom_line(arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed"), size=2)
#p1 <- p1 +  geom_vline(xintercept = 17,color = "sienna", size=1)
#p1 <- p1 +  geom_vline(xintercept = 24,color = "skyblue", size=1)
#p1 + annotation_custom(rasterGrob(img, 
#                                  width = unit(1,"npc"),
#                                  height = unit(1,"npc")), 
#                       0,12, 5, 8) 

#annotate("segment", x = 17.2, xend = 17.2, y = 7.41, yend = 7.4, colour = "red", size=1.5, alpha=1, arrow=arrow())+ lims(x= c(0, 35), y = c(0, 8))

require(gridExtra)
gridExtra::grid.arrange(p,p1,heights = 1:2)
dev.off()

#####CFR T-series EU##########
library(ggplot2)
theme_set(theme_minimal())
library(png)
library(grid)
library(ggimage)
#img <- readPNG("2020-07-30_031323.png")
eucfr <- read.csv("E:\\ResearchProject\\Jamal Sir\\COVID19-Factors\\data\\New\\eucfr.csv")

eucfr$Region
eucfr$ï..Week
eucfr$CFR_mean


p1 <- ggplot() + geom_line(aes(y = CFR_mean, x = ï..Week, colour = Region), size=2,
                           data = eucfr, stat="identity")  +labs(y="Reported CFR", x = "Weeks")+
  scale_color_brewer(palette = "Dark2") + ylim(0, 8) + xlim(0, 35)
p1
p1 <- p1 +geom_line(arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed"), size=2)
p1 + annotate("segment", x = 17.2, xend = 17.2, y = 7.41, yend = 7.4, colour = "red", size=2, alpha=1, arrow=arrow())+ lims(x= c(0, 35), y = c(0, 8))


#p1 <- p1 +  geom_vline(xintercept = 17,color = "sienna", size=1)
#p1 <- p1 +  geom_vline(xintercept = 24,color = "skyblue", size=1)
#p1 + annotation_custom(rasterGrob(img, 
#                               width = unit(1,"npc"),
#                               height = unit(1,"npc")), 
#                    0,12, 5, 8) 





#####Top 20 CFR bar plot After##########

tiff("E:\\Study\\ResearchProject\\Jamal Sir\\COVID19-Factors\\data\\New\\barFF.tiff", units="in", width=6, height=4, res=300)

#filter
Countrycfr <- read.csv("E:\\Study\\ResearchProject\\Jamal Sir\\COVID19-Factors\\data\\New\\highcfr.csv")
data <- Countrycfr[with(Countrycfr,order(-CFR)),]

p<-ggplot(data=data, aes(x=reorder(ï..Location,CFR), y=CFR)) +
  geom_bar(stat="identity", fill = '#003366', color = '#add8e6')+coord_flip()+labs(title="The global reported case-fatality rate (%) of COVID-19 (29 April-31 December 2020)", y="Reported Case-fatality rate (%)", x = "Top ten countries")
p <- p + theme(plot.title = element_text(color = "black", size = 10))
p
Countrycfr <- read.csv("E:\\Study\\ResearchProject\\Jamal Sir\\COVID19-Factors\\data\\New\\highcfrBefore.csv")
data <- Countrycfr[with(Countrycfr,order(-CFR)),]

p1<-ggplot(data=data, aes(x=reorder(ï..Location,CFR), y=CFR)) +
  geom_bar(stat="identity", fill = '#003366', color = '#add8e6')+coord_flip()+labs(title="The global reported case-fatality rate (%) of COVID-19 (1 Jan - 28 April 2020)", y="Reported Case-fatality rate (%)", x = "Top ten countries")
p1 <- p1 + theme( plot.title = element_text(color = "black", size = 10))
p1
gridExtra::grid.arrange(p1,p)
dev.off()

####time series

tiff("E:\\Study\\ResearchProject\\Jamal Sir\\COVID19-Factors\\data\\New\\TseriesF.tiff", units="in", width=6, height=5, res=300)
library(Rcpp)
library(prophet)
library(data.table)
library(dplyr)
library(ggplot2)

# Model building
setwd("E:\\Study\\ResearchProject\\Jamal Sir\\COVID19-Factors\\data\\New\\")
world <- read.csv("E:\\Study\\ResearchProject\\Jamal Sir\\COVID19-Factors\\data\\New\\WCFR2.csv")
world$date
world$CFR <- (world$total_deaths/world$total_cases)*100


history <- data.frame(ds = seq(as.Date('2020-01-01'), as.Date('2020-12-31'), by = 'd'),
                      y = world$CFR)
m3 <- prophet(history)
future <- make_future_dataframe(m3, periods = 10)
fcst3 <- predict(m3, future)
par(mfrow=c(3,1))
y <-plot(m3, fcst3, xlab="Months", ylab="Reported CFR (%)") + ggtitle("     Automatic Forecasting Time-series Model") + theme(
  plot.title = element_text(size=10))
plot(y)

last_fcst3 <- fcst3[366,]
rmse <- sqrt(mean((history$y - fcst3$yhat[c(1:366)])^2))
mae <- mean(abs((history$y - fcst3$yhat[c(1:366)])))
final <- cbind(last_fcst3, rmse, mae)
final


#R2
SSE <- sum((history$y - fcst3$yhat[c(1:366)])^2)
SST <- sum((history$y - mean(history$y))^2)
R_square <- 1 - SSE / SST
R_square


#ARIMA
library(forecast)
library(MASS)
library(tseries)
library(forecast)
library(lubridate)
library(ggplot2)

library(zoo)

myts <- ts(world$CFR[1:366])

autoplot(myts)
auto.arima(myts)
Fit<-Arima(myts,order=c(0,2,1))
fcast <- forecast(Fit, h=10)
fcast$x
summary(Fit)

z <- autoplot(fcast, main=NULL)  +
  autolayer(fcast$mean, series="Forecast") +
  autolayer(fitted(Fit), series='Fitted') + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("") + ylab("Reported CFR (%)") + ggtitle(" Auto-Regressive Integrated Moving Average Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom",
                                                                
                                                                axis.text.x=element_blank(),
                                                                axis.ticks.x=element_blank()) +
 theme( legend.title = element_text(color = "Black", size = 8),
legend.text = element_text(color = "Black", size = 8),
plot.title = element_text(size=10))

plot(z)
#library(png)
#library(grid)
#img2 <- readPNG("sss.png")
#z <- z + annotation_custom(rasterGrob(img2, 
#                               width = unit(1.01,"npc"),
#                               height = unit(.03,"npc")), 
#                    -Inf, Inf, -8.1, Inf)



#z




summary(Fit)





#R2
SSE <- sum((resid(Fit[1:365]))^2)
SST <- sum((world$CFR[1:365] - mean(world$CFR[1:365]))^2)
R_square <- 1 - SSE / SST
R_square

gridExtra::grid.arrange(z, y)
dev.off()


####SES########
tiff("E:\\Study\\ResearchProject\\Jamal Sir\\COVID19-Factors\\data\\New\\Tser.tiff", units="in", width=6, height=5, res=300)
library(tidyverse) 
library(fpp2) 

ses.goog <- ses(myts,  
                alpha = .5, 
                h = 10) 
summary(ses.goog)

fcast <- forecast(ses.goog, h=10)
x <- autoplot(ses.goog, main=NULL)+
  autolayer(fcast$mean, series="Forecast") +
  autolayer(fitted(ses.goog), series='Fitted') + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("") + ylab("Reported CFR (%)") + ggtitle("      Simple Exponential Smoothing Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom",
                                                                
                                                                axis.text.x=element_blank(),
                                                                axis.ticks.x=element_blank()) +
  theme( legend.title = element_text(color = "Black", size = 8),
         legend.text = element_text(color = "Black", size = 8),
         plot.title = element_text(size=10))

library(png)
library(grid)
img2 <- readPNG("E:\\Study\\ResearchProject\\Jamal Sir\\COVID19-Factors\\data\\New\\sss.png")
x <- x + annotation_custom(rasterGrob(img2, 
                                      width = unit(1.01,"npc"),
                                      height = unit(.03,"npc")), 
                           -Inf, Inf, -8.1, Inf)



x
gridExtra::grid.arrange(x,z,y)
dev.off()
accuracy(ses.goog)

#R2
SSE <- sum((resid(ses.goog[1:289]))^2)
SST <- sum((world$CFR[1:289] - mean(world$CFR[1:289]))^2)
R_square <- 1 - SSE / SST
R_square



#Menn kendal
library(Kendall)
library(trend)
world <- read.csv("F:\\ResearchProject\\Jamal Sir\\COVID19-Factors\\data\\New\\Before.csv")
myts <- ts(world$CFR_mean)
t.test(world$CFR_mean)$"conf.int"
mean(world$CFR_mean)

MannKendall(myts)
sens.slope(myts, conf.level = 0.95)

world <- read.csv("F:\\ResearchProject\\Jamal Sir\\COVID19-Factors\\data\\New\\After.csv")
mean(world$CFR_mean)
t.test(world$CFR_mean)$"conf.int"
myts <- ts(world$CFR_mean)

library(Kendall)
MannKendall(myts)
sens.slope(myts, conf.level = 0.95)


library(tidyr)
library(ggplot2)
tiff("bar23.tiff", units="in", width=6, height=5, res=300)

bar <- read.csv("E:\\Study\\ResearchProject\\Jamal Sir\\COVID19-Factors\\data\\New\\Bar2.csv")

bar$Months <-factor(bar$Months,
                    levels = c("January" ,  "February"  ,"March"   ,  "April"  ,   "May"   ,    "June"   ,   "July"     ,
                               "August" ,   "September" ,"October"   ,"November" , "December" ))
ylab <- c(5, 10, 15, 20)
p <-ggplot(bar, aes(Months, Cases)) + geom_bar(stat = "identity",  fill="steelblue") + scale_y_continuous(labels = paste0(ylab, "M"),
                                                    breaks = 10^6 * ylab)  + xlab("Months") + ylab("Number of total cases") +theme(plot.title = element_text(color = "black", size = 12), axis.text.x = element_text(angle = 45, hjust = 1))
p <- p + labs(title="            The monthly reported new COVID-19 cases in the world")
p

q <-ggplot(bar, aes(Months, Deaths)) + geom_bar(stat = "identity",  fill="steelblue") + xlab("Months") + ylab("Number of total deaths") +theme(plot.title = element_text(color = "black", size = 12), axis.text.x = element_text(angle = 45, hjust = 1)) + lims(y= c(0, 400000))
q <- q + labs(title="The monthly reported new deaths due to COVID-19 in the world")
q

gridExtra::grid.arrange(p, q)
dev.off()
