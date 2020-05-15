library(dplyr)
library(shiny)
library(png)
library(chron)
library(extrafont)
library(ggplot2)
library(plotly)
library(hrbrthemes)


#Initialize dataset directory
tmpshot <- fileSnapshot("_datasets/_2020-05/")

#Get latest file from directory
file <- rownames(tmpshot$info[which.max(tmpshot$info$mtime),])
latestFile <- paste("_datasets/_2020-05/",file,sep="")
coviddatasets <- read.csv(latestFile, header=TRUE,sep=",",quote="\"")

#Extract date from file name
asofDate <- regexpr("([0-9]{8})", file)
asofDate <- regmatches(file, asofDate)
asofDate <- as.Date(asofDate,"%Y%m%d")
asofDate <- format.Date(asofDate, "%B %d, %Y")
asofDate

#Extract only needed column
coviddata <- coviddatasets %>%
  select(Sex,Age,AgeGroup,RegionRes,HealthStatus,DateRepConf) %>%
  arrange(desc(Age))
coviddata

#MinAge
youngest <- min(coviddata$Age, na.rm = TRUE)

#MaxAge
oldest <- max(coviddata$Age, na.rm = TRUE)

#Mid Age
medAge <- median(coviddata$Age, na.rm= TRUE)

#Heatlh Status of the cases
table(coviddata$HealthStatus)

#Latest case for the current date
Sys.setenv(TZ="Asia/Manila")
getDate <- format(Sys.time(), "%H")
getDate <- as.integer(getDate)
getDate
if (getDate >= 18) {
  newtoday <- Sys.Date()
  newtoday
} else {
  newtoday <- Sys.Date() - 1
  newtoday
}

newCase <- table(coviddata$DateRepConf)
newCase <- newCase[names(newCase)==newtoday]
newCase

if(length(newCase) == 0){
  newCase <- "data outdated"
} else{
  newCase <- as.character(newCase)
}
newCase

#Recovered
recovered <- table(coviddata$HealthStatus)
recovered <- recovered["Recovered"]

#Total count of cases
countofCases <- count(coviddata)


#Count of Total Deaths
totalDeaths <- coviddatasets %>% 
  filter(HealthStatus=="Died") %>%
  count(HealthStatus)
totalDeaths


#Gender Percentage
genderPerCentage <- coviddatasets %>% 
  group_by(Sex) %>%
  summarise(
    count = n(),
    perc = round((count / nrow(.)), 2 ) * 100
    )

#Data For Male
malePercentage <- genderPerCentage[genderPerCentage$Sex=="Male","perc"]
malePercentage <- as.character(malePercentage)
malePercentage <- paste(malePercentage,"%",sep="")
malePercentage

#Data For Female
femalePercentage <- genderPerCentage[genderPerCentage$Sex=="Female","perc"]
femalePercentage <- as.character(femalePercentage)
femalePercentage <- paste(femalePercentage,"%",sep="")
femalePercentage

#Region with least case
leastCasereg <- coviddatasets %>% 
  count(RegionRes) %>%
  arrange(n) %>%
  top_n(-1)
leastCasereg

#Region with highest case
highCasereg <- coviddatasets %>% 
  count(RegionRes) %>%
  top_n(1)
highCasereg


#Monthly Trend
covidTrend <- coviddata %>%
  select(DateRepConf) %>%
  count(DateRepConf)
covidTrend$DateRepConf <- as.Date( covidTrend$DateRepConf, '%Y-%m-%d')
covidTrend <- covidTrend %>%
  ggplot(aes(DateRepConf, n )) +
  geom_area(fill="#FDA7DF", alpha=0.5) +
  geom_line(color="#D980FA", size=1, alpha=0.9, linetype=1) +
  theme_ipsum(
    plot_title_family = "Impact", 
    plot_title_size = 30,
    grid_col = "#FDA7DF",
    axis_text_size=15,
    axis_title_size=20,
    axis_title_family="Impact") +
  theme(plot.title = element_text(colour = "#D980FA"),
        axis.text.y = element_text(colour = "#D980FA"),
        axis.text.x = element_text(colour = "#D980FA"),
        axis.title.x = element_text(colour = "#FDA7DF"),
        axis.title.y = element_text(colour = "#FDA7DF")) +
  labs(x="Month", y="# of cases", title="cases trend by month")


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  

  #Output Region with Highest case
  output$highCasereg <- renderText({
    {highCasereg$n}
  })
  
  #Output Total Cases
  output$countofCases <- renderText({
    {format(countofCases$n, nsmall=1, big.mark=",")}
  })
  
  #Output Total Recoveries
  output$recovery <- renderText({
    {paste(format(recovered, nsmall=1, big.mark=",")," recoveries",sep=" ")}
  })
  
  
  #Output Total Deaths
  output$died <- renderText({
    {paste(format(totalDeaths$n, nsmall=1, big.mark=","),"died", sep=" ")}
  })
  
  
  #Output New Case
  output$newCase <- renderText({
    {newCase}
  })
  
  #Output Oldest Case
  output$oldestCase <- renderText({
    {oldest}
  })
  
  #Output Female percentage
  output$fPercent <- renderText({
    {femalePercentage}
  })
  
  #Output Male percentage
  output$mPercent <- renderText({
    {malePercentage}
  })
  
  #Output latest data date
  output$asofDate <- renderText({
    {asofDate}
  })
  
  
  
}

shinyApp(ui = htmlTemplate("www/index.html"), server)