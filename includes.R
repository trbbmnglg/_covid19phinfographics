library(tidyr)
library(dplyr)
library(readr)
library(shiny)
library(shinyalert)
library(png)
library(chron)
library(extrafont)
library(ggplot2)
library(plotly)
library(hrbrthemes)
library(RColorBrewer)
library(extrafont)


#Set Impact font in LINUX environment
if(Sys.info()[['sysname']] ==
  'Linux'){
  dir.create('~/.fonts')
  file.copy("www/fonts/Oswald-VariableFont_wght.ttf", "~/.fonts")
  system('fc-cache -f ~/.fonts')
}