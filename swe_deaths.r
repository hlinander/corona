library(utils)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(zoo)
data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
swe = filter(data, 
             countriesAndTerritories %in% c("Sweden", 
                                            "Norway", 
                                            "Spain", 
                                            "Italy", 
                                            "Denmark", 
                                            "France", 
                                            "United_States_of_America", 
                                            "United_Kingdom"))
swe$time = as.Date(swe$dateRep, "%d/%m/%Y")
swe = swe %>% 
      group_by(countryterritoryCode) %>% 
      arrange(time) %>% 
      mutate(roll=rollmean(deaths, 7, align="right", na.pad=TRUE))
swerecent = filter(swe, time > as.Date("10/3/2020", "%d/%m/%Y"))
deaths = (ggplot(data=swerecent, 
                aes(x=time, 
                    y=deaths, 
                    group=countriesAndTerritories, 
                    colour=countriesAndTerritories)
               )
         + geom_point() 
         + geom_line(linetype="dashed") 
         + scale_x_date(date_breaks="1 days", date_labels="%b %d") 
         + theme(axis.text.x=element_text(angle=90, hjust=1)) 
         + geom_line(aes(y=roll)))
log_deaths = (deaths 
             + scale_y_continuous(trans="log10") 
             + ylab("deaths per day") + xlab("date"))

withCumDeath = (swe 
               %>% group_by(countryterritoryCode) 
               %>% arrange(time) 
               %>% mutate(cumdeaths=cumsum(deaths)))
fromFirst = filter(withCumDeath, cumdeaths > 0) 
            %>% group_by(countryterritoryCode) 
            %>% arrange(time) 
            %>% mutate(timesincedeath = time - min(time))

deaths_from_first = ggplot(data=fromFirst, 
                           aes(timesincedeath, 
                               deaths, 
                               group=countriesAndTerritories, 
                               colour=countriesAndTerritories)) 
                    + geom_point() 
                    + geom_line(linetype="dashed") 
                    + theme(axis.text.x=element_text(angle=90, hjust=1)) 
                    + ylab("deaths per day") 
                    + xlab("days since first death in country") 
                    + geom_line(aes(y=roll))

log_deaths_from_first = deaths_from_first + scale_y_continuous(trans="log10") 

capPop = c(SE=1, ES=6, NO=0.7, IT=2.8, DK=2)
fromFirstWithCapPop = transform(fromFirst, cappop=capPop[as.character(geoId)], stringsAsFactors=FALSE)
deathsPerCapPop = transform(fromFirstWithCapPop, deathsPerCapPop = deaths / cappop)
deathsPerCapPop = transform(deathsPerCapPop, rollPerCapPop = roll / cappop)

deaths_from_first_per_cap = ggplot(data=deathsPerCapPop, 
                                   aes(timesincedeath, 
                                       deathsPerCapPop, 
                                       group=countriesAndTerritories, 
                                       colour=countriesAndTerritories)) 
                            + geom_point() 
                            + geom_line(linetype="dashed") 
                            + theme(axis.text.x=element_text(angle=90, hjust=1)) 
                            + ylab("deaths per day / (million capital population)") 
                            + xlab("days since first death in country") 
                            + geom_line(aes(y=rollPerCapPop))
log_deaths_from_first_per_cap = deaths_from_first_per_cap + scale_y_continuous(trans="log10")

p = grid.arrange(arrangeGrob(deaths, 
                             deaths_from_first, 
                             #deaths_from_first_per_cap, 
                             top=textGrob("linear", gp = gpar(fontsize = 20, font = 8))), 
                 arrangeGrob(log_deaths, 
                             log_deaths_from_first, 
                             #log_deaths_from_first_per_cap, 
                             top=textGrob("log", gp = gpar(fontsize = 20, font = 8))), ncol=2)
ggsave(filename="cov19.pdf", plot=p)
