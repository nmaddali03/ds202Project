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
pop <- data %>% filter(`Series code` == 'SP.POP.GROW') %>% 
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

data1 <- test %>% group_by(`Country name`) %>%
  summarize(`Country name`, "year" = `variable`, "population" = `value.x`, "GDP"=`value.y`)

datSig <- filter(data1, `Country name` %in% c("China", "Germany", "India", "Iran, Islamic Rep.", 
                                            "Japan", "Korea, Dem. Rep.", "Russian Federation", 
                                            "United States"))

ggplot(datSig, aes(x=population, y=GDP, color = year)) + 
  geom_point() + facet_wrap(~`Country name`)



#ggplot(datSig, aes(x=population, y=GDP)) + 
 # geom_point() + facet_wrap(~`Country name`)



#Which country has the most environmental impact from climate change in the time period 1990-2011?



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

ggplot(effect, aes(x=gas_emission, y=GDP)) + geom_point() + facet_wrap(~Country_Name, scales="free") + labs(x="Gas Emissions (KtCO2)", y="GDP", title="GDP Vs. Gas Emissions from 1990-2011") + theme(axis.text.x = element_text(angle = 90))

