library(ggplot2)
library(dplyr)
library(readr)
library(gridExtra)
tiff("IFR.tiff", units="in", width=6, height=4, res=300)
soil_N_summary <- read_csv("E:\\Study\\ResearchProject\\Jamal Sir\\COVID19-Factors\\Last.csv")

soil_N_summary <- mutate(soil_N_summary, Date=as.POSIXct(Date))

soil_N_summary$Age <- as.factor(soil_N_summary$`Age Group`)
p <-  ggplot(soil_N_summary, aes(Date, IFR, fill = `Age Group`, colour = `Age Group`))+
    geom_line() +geom_point() + ylab("Infection rate") + xlab("Months") +
    scale_x_datetime(expand=c(0,0),
                     date_breaks= "31 days", 
                     date_minor_breaks = "30 days", 
                     date_labels = "%b", 
                     limits = as.POSIXct(c("2020-03-01 00:00:00", "2020-12-29 00:00:00"))) + theme()
p
dev.off()

