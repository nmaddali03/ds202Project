library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(lubridate)
library(reshape2)
install.packages("reshape2")


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

ggplot(data1, aes(x=population, y=GDP, color = year)) + 
  geom_point() + facet_wrap(~`Country name`)

ggplot(data1, aes(x=population, y=GDP)) + 
  geom_point() + facet_wrap(~`Country name`)





##experimenting stuff and reference -- ignore

dat<-ames[ames$Neighborhood%in%c('BrkSide','ClearCr','CollgCr','Somerst'), ]
ggplot(dat, aes(x=YearBuilt, y=SalePrice, colour = LandSlope)) + geom_point() + facet_wrap(~Neighborhood)

ggplot(oregon, aes(x=Name, y=score)) + geom_point() + labs(title="ISU vs. Oregon") +
  theme(axis.text.x=element_text(angle=90))


ggplot(grouped,aes(x=HOUR, y=numAccidents)) +
  geom_bar(stat='identity') +
  facet_grid(sexStr~DAY_WEEK) +
  xlab("Hours of the day") +
  ylab("Total number of accidents")


#Which country has the most environmental impact from climate change in the time period 1990-2011?
#How does GDP affect amount of gas emissions?
