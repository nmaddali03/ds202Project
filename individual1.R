library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(lubridate)
library(naniar)


#import the data
data <- readxl::read_xlsx('ClimateChange.xlsx', sheet='Data')
country <- readxl::read_xlsx('ClimateChange.xlsx', sheet='Country')
series <- readxl::read_xlsx('ClimateChange.xlsx', sheet='Series')

#change the ".." cells in the years to n/a or 0
data <- na_if(data, '..')
