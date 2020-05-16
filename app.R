source("includes.R")

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


#Extract only needed column
coviddata <- coviddatasets %>%
  select(Sex,Age,AgeGroup,RegionRes,HealthStatus,DateRepConf) %>%
  arrange(desc(Age))


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

if(length(newCase) == 0){
  newCase <- "data outdated"
} else{
  newCase <- as.character(newCase)
}


#Recovered
recovered <- table(coviddata$HealthStatus)
recovered <- recovered["Recovered"]

#Total count of cases
countofCases <- count(coviddata)


#Count of Total Deaths
totalDeaths <- coviddatasets %>% 
  filter(HealthStatus=="Died") %>%
  count(HealthStatus)


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

#Data For Female
femalePercentage <- genderPerCentage[genderPerCentage$Sex=="Female","perc"]
femalePercentage <- as.character(femalePercentage)
femalePercentage <- paste(femalePercentage,"%",sep="")

#Region with least case
leastCasereg <- coviddatasets %>% 
  count(RegionRes) %>%
  arrange(n) %>%
  top_n(-1)


#Region with highest case
highCasereg <- coviddatasets %>% 
  count(RegionRes) %>%
  top_n(1)

#Monthly Trend
covidTrend <- coviddata %>%
  select(DateRepConf) %>%
  count(DateRepConf)
covidTrend$DateRepConf <- as.Date( covidTrend$DateRepConf, '%Y-%m-%d')
covidTrend <- covidTrend %>%
  ggplot(aes(DateRepConf, n )) +
  geom_area(fill="#FDA7DF", alpha=0.5) +
  geom_line(color="#D980FA", size=1, alpha=0.9, linetype=1) +
  scale_x_date(expand = c(0, 0)) +
  theme_ipsum(
    plot_title_family = "Impact", 
    plot_title_size = 50,
    grid_col = "#FDA7DF",
    axis_text_size=15,
    axis_title_size=18
    #axis_title_family="Impact"
    ) +
  theme(
        plot.title = element_text(colour = "#D980FA"),
        axis.text.y = element_text(colour = "#D980FA"),
        axis.text.x = element_text(colour = "#D980FA"),
        axis.title.x = element_text(margin = margin(r = 50), colour = "#FDA7DF"),
        axis.title.y = element_text(colour = "#FDA7DF"),
        plot.background = element_rect(fill = "#fde9f6ad", color="#fde9f6ad"),
        plot.margin = margin(0,0,0,0)) +
  labs(x="Month", y="# of cases", title="cases over time") +
  ylim(0, 600)
aspect_ratio <- 2.5
ggsave("www/plots/covidtrend.png", width = 16, height = 12, dpi = "screen", units = "cm", device="png")


#Generate Pie chart for count per Region
nb.cols <- 20
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

regCount <- coviddatasets %>% 
  group_by(RegionRes) %>%
  summarise(CaseCount=n()) %>%
  rename("Region" = "RegionRes") %>%
  arrange(desc(CaseCount))
regCount$Region[regCount$Region==""]<-"Uncategorized"
showRegCount <- ggplot(regCount, aes(x="", y=CaseCount, fill=Region)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual(values = mycolors) +
  theme(plot.margin = margin(0,0,0,0))
ggsave("www/plots/region.png", width = 16, height = 12, dpi = "screen", units = "cm", device="png")

library(shiny)

#Call server to display stuffs
server <- function(input, output, session) {
  
  
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
  
  #Output highest case region
  output$regionHighNum <- renderText({
    {highCasereg$n}
  })
  
  output$regionHighName <- renderText({
    {highCasereg$RegionRes}
  })
  
}

shinyApp(ui = htmlTemplate("www/index.html"), server)