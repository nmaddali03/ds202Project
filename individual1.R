library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(lubridate)
install.packages("reshape2")
library(reshape2)


#import the data
data <- readxl::read_xlsx('ClimateChange.xlsx', sheet='Data')
country <- readxl::read_xlsx('ClimateChange.xlsx', sheet='Country')
series <- readxl::read_xlsx('ClimateChange.xlsx', sheet='Series')

#change the ".." cells in the years to n/a or 0
data <- na_if(data, '..')


#How does population growth affect GDP?
#group by country
#select population growth (SP.POP.GROW) and gdp (NY.GDP.MKTP.CD)
#scatter plots
#test box plot

pop <- data %>% filter(`Series code` == 'SP.POP.TOTL') %>% 
  summarize(`Country name`, `Series code`,`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,
            `1997`,`1998`,`1999`,`2000`,`2001`,`2002`,`2003`,`2004`,`2005`,`2006`,`2007`,
            `2008`,`2009`,`2010`,`2011`)

pop<-melt(pop, id.vars=c("Country name", "Series code"))

gdp <- data %>% filter(`Series code` == 'NY.GDP.MKTP.CD') %>% 
  summarize(`Country name`, `Series code`,`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,
            `1997`,`1998`,`1999`,`2000`,`2001`,`2002`,`2003`,`2004`,`2005`,`2006`,`2007`,
            `2008`,`2009`,`2010`,`2011`)

gdp<-melt(gdp, id.vars=c("Country name", "Series code"))

test <- pop %>% inner_join(gdp %>% select("Country name",`Series code`,`variable`,`value`), 
                           by=c("Country name","variable"))
test <- na.omit(test)                           # Apply na.omit function


data1 <- test %>% group_by(`Country name`) %>%
  summarize(`Country name`, "year" = `variable`, "population" = `value.x`, "GDP"=`value.y`)

datSig <- filter(data1, `Country name` %in% c("China", "Germany", "India", "Iran, Islamic Rep.", 
                                            "Japan", "Korea, Dem. Rep.", "Russian Federation", 
                                            "United States"))


#ggplot(datSig, aes(x=population, y=GDP, color = year)) + 
 # geom_point() + facet_wrap(~`Country name`)



ggplot(datSig, aes(x=population, y=GDP)) + geom_point() + facet_wrap(~`Country name`)



#Which country has the most environmental impact from climate change in the time period 
#1990-2011?
#EN.CLC.MDAT.ZS -- check the environmental disaster variable vs year  
#ER.H2O.FWTL.ZS -- Annual freshwater withdrawals (% of internal resources)
#AG.LND.EL5M.ZS -- Land area below 5m (% of land area)
disaster <- data %>% filter(`Series code` == 'EN.CLC.MDAT.ZS') %>% 
  summarize(`Country name`, `Series code`,`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,
            `1997`,`1998`,`1999`,`2000`,`2001`,`2002`,`2003`,`2004`,`2005`,`2006`,`2007`,
            `2008`,`2009`,`2010`,`2011`)
disaster<-melt(disaster, id.vars=c("Country name", "Series code"))

disaster <- filter(disaster, `Country name` %in% c("China", "Germany", "India", 
                                                  "Iran, Islamic Rep.", "Japan", 
                                                  "Korea, Dem. Rep.", "Russian Federation", 
                                                  "United States"))
h2oWithdraw <- data %>% filter(`Series code` == 'ER.H2O.FWTL.ZS') %>% 
  summarize(`Country name`, `Series code`,`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,
            `1997`,`1998`,`1999`,`2000`,`2001`,`2002`,`2003`,`2004`,`2005`,`2006`,`2007`,
            `2008`,`2009`,`2010`,`2011`)
h2oWithdraw<-melt(h2oWithdraw, id.vars=c("Country name", "Series code"))

h2oWithdraw <- filter(h2oWithdraw, `Country name` %in% c("China", "Germany", "India", 
                                                   "Iran, Islamic Rep.", "Japan", 
                                                   "Korea, Dem. Rep.", "Russian Federation", 
                                                   "United States"))
landBelow <- data %>% filter(`Series code` == 'AG.LND.EL5M.ZS') %>% 
  summarize(`Country name`, `Series code`,`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,
            `1997`,`1998`,`1999`,`2000`,`2001`,`2002`,`2003`,`2004`,`2005`,`2006`,`2007`,
            `2008`,`2009`,`2010`,`2011`)
landBelow<-melt(landBelow, id.vars=c("Country name", "Series code"))

landBelow <- filter(landBelow, `Country name` %in% c("China", "Germany", "India", 
                                                         "Iran, Islamic Rep.", "Japan", 
                                                         "Korea, Dem. Rep.", "Russian Federation", 
                                                         "United States"))

impact <- disaster %>% inner_join(h2oWithdraw %>% select("Country name",`Series code`,`variable`,`value`), 
                           by=c("Country name","variable"))
impact <- impact %>% inner_join(landBelow %>% select("Country name",`Series code`,`variable`,`value`), 
                                  by=c("Country name","variable"))
impact <- impact %>% group_by(`Country name`) %>%
  summarize(`Country name`, "year" = `variable`, "Natural Disasters" = `value.x`, 
            "H20 Withdrawal"=`value.y`, "Land Below"=`value`) %>% 
  mutate(`Natural Disasters` = as.numeric(`Natural Disasters`)) %>% 
  mutate(`H20 Withdrawal` = as.numeric(`H20 Withdrawal`)) %>% 
  mutate(`Land Below` = as.numeric(`Land Below`))

test <- impact %>% summarize(`Country name`, `Natural Disasters`, `H20 Withdrawal`, `Land Below`)

impactChina <- filter(impact, `Country name` %in% c("China"))
a <- mean(impactChina$`Natural Disasters`,na.rm=TRUE)
a1 <- mean(impactChina$`H20 Withdrawal`,na.rm=TRUE)
a2 <- mean(impactChina$`Land Below`,na.rm=TRUE)

impactGermany <- filter(impact, `Country name` %in% c("Germany"))
b <- mean(impactGermany$`Natural Disasters`,na.rm=TRUE)
b1 <- mean(impactGermany$`H20 Withdrawal`,na.rm=TRUE)
b2 <- mean(impactGermany$`Land Below`,na.rm=TRUE)

impactIndia <- filter(impact, `Country name` %in% c("India"))
c <- mean(impactIndia$`Natural Disasters`,na.rm=TRUE)
c1 <- mean(impactIndia$`H20 Withdrawal`,na.rm=TRUE)
c2 <- mean(impactIndia$`Land Below`,na.rm=TRUE)

impactUS <- filter(impact, `Country name` %in% c("United States"))
d <- mean(impactUS$`Natural Disasters`,na.rm=TRUE)
d1 <- mean(impactUS$`H20 Withdrawal`,na.rm=TRUE)
d2 <- mean(impactUS$`Land Below`,na.rm=TRUE)

country <- c('China','Germany','India','United States')
avgNaturalDisasters <- c(a,b,c,d)
avgH20Withdrawal <- c(a1,b1,c1,d1)
avgLandBelow <- c(a2,b2,c2,d2)

envirImpact <- data.frame(country, avgNaturalDisasters, avgH20Withdrawal, avgLandBelow)
envirImpact <- envirImpact %>% rowwise() %>% 
  mutate(total = sum(c(avgNaturalDisasters, avgH20Withdrawal, avgLandBelow))) %>% 
  summarize(country, avgNaturalDisasters, avgH20Withdrawal, avgLandBelow, total)

lbls <- c('China','Germany','India','United States')
pct <- round(envirImpact$total/sum(envirImpact$total)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(envirImpact$total,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Countries")



#How does gas emissions affect GDP?
gas <- data %>% filter(`Series code` == 'EN.ATM.CO2E.KT') %>% 
  summarize(Country_Name =`Country name`, Series_Code =`Series code`,`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,
            `1997`,`1998`,`1999`,`2000`,`2001`,`2002`,`2003`,`2004`,`2005`,`2006`,`2007`,
            `2008`,`2009`,`2010`,`2011`)

gas<-melt(gas, id.vars=c("Country_Name", "Series_Code"))

gas <- gas %>%
  group_by(Country_Name) %>%
  summarize(Country_Name,Series_Code, year=`variable`,gas_emission=`value`)


gas$Series_Code <- NULL

gassignificant <- filter(gas, Country_Name %in% c("China", "Germany", "India", 
                                                  "Iran, Islamic Rep.", "Japan", 
                                                  "Korea, Dem. Rep.", "Russian Federation", 
                                                  "United States"))

gdp <- data %>% filter(`Series code` == 'NY.GDP.MKTP.CD') %>% 
  summarize(`Country name`, `Series code`,`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,
            `1997`,`1998`,`1999`,`2000`,`2001`,`2002`,`2003`,`2004`,`2005`,`2006`,`2007`,
            `2008`,`2009`,`2010`,`2011`)

gdp<-melt(gdp, id.vars=c("Country name", "Series code"))

gdp <- gdp %>%
  group_by(`Country name`) %>%
  summarize(Country_Name=`Country name`,`Series code`, year=`variable`,GDP=`value`)

gdp$`Series code` <- NULL
gdp$`Country name` <- NULL

gdpSig <- filter(gdp, Country_Name %in% c("China", "Germany", "India", "Iran, Islamic Rep.", "Japan", "Korea, Dem. Rep.", "Russian Federation", "United States"))

effect <- gdpSig %>%
  inner_join(gassignificant %>%
               select(Country_Name, year, gas_emission),
             by=c("Country_Name", "year"))
effect <- na.omit(effect)                           # Apply na.omit function


#all scatter plots in one figure
#ggplot(effect, aes(gas_emission, GDP, colour = `Country_Name`)) + geom_point()

#multiple graphs in one figure
ggplot(effect, aes(x=gas_emission, y=GDP)) + geom_point() + facet_wrap(~Country_Name, scales="free") + 
  labs(x="Gas Emissions (KtCO2)", y="GDP", title="GDP Vs. Gas Emissions from 1990-2011") + 
  theme(axis.text.x = element_text(angle = 90)) + geom_smooth(method="lm") 

