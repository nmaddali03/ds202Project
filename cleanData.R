library(tidyverse)
library(readxl)


#import the data
data <- readxl::read_xlsx('ClimateChange.xlsx', sheet='Data')
country <- readxl::read_xlsx('ClimateChange.xlsx', sheet='Country')
series <- readxl::read_xlsx('ClimateChange.xlsx', sheet='Series')

#need to figure out what the "TEXT" cells mean in the SCALE and Decimals columns
#change the ".." cells in the years to n/a or 0


#Which country had the most cause/impact towards climate change from 1990-2011?

