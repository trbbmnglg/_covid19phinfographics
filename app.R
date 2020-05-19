source("includes.R")

#Initialize dataset directory
dataSetDir <- "_datasets/_2020-05/"
tmpshot <- fileSnapshot(dataSetDir)

#Get latest file from directory
file <- rownames(tmpshot$info[which.max(tmpshot$info$mtime),])
latestFile <- paste(dataSetDir,file,sep="")
coviddatasets <- read_csv(latestFile)

#Extract date from file name
asofDate <- regexpr("([0-9]{8})", file)
asofDate <- regmatches(file, asofDate)
orig_asofDate <- as.Date(asofDate,"%Y%m%d")
asofDate <- format.Date(orig_asofDate, "%B %d, %Y")

#Extract only needed column
coviddata <- coviddatasets %>%
  select(Sex,Age,AgeGroup,RegionRes,HealthStatus,DateRepConf, Pregnanttab, RemovalType, DateRepRem) %>%
  arrange(desc(Age))

#Format Date Rep Conf since it has a heterogeneous format (use lubridate) 
coviddata$DateRepConf <- parse_date_time(coviddata$DateRepConf,c("dmY", "ymd"))
coviddata$DateRepConf <- as.Date(coviddata$DateRepConf,format="%m/%d/%Y")

#Format Date Rep Remove since it has a heterogeneous format (use lubridate) 
coviddata$DateRepRem <- parse_date_time(coviddata$DateRepRem,c("dmY", "ymd"))
coviddata$DateRepRem <- as.Date(coviddata$DateRepRem,format="%m/%d/%Y")

#Age select and calculation
age <- coviddatasets %>%
  select(Age) %>%
  filter(!is.na(Age)) %>%
  summarize(
        AverageAge = round(mean(Age),0),
        Oldest = max(Age),
        Youngest = min(Age)
      )
oldest <- age$Oldest
AverageAge <- age$AverageAge

#Latest case for the latest availabe dataset
newtoday <- orig_asofDate
newCase <- table(coviddata$DateRepConf)
newCase <- newCase[names(newCase)==newtoday]

#Count of new deaths and recovery for the latest availabe dataset
newCounts <-coviddata %>%
  select(DateRepRem, RemovalType) %>%
  filter(DateRepRem==newtoday) %>%
  group_by(RemovalType) %>%
  summarise(NewCounts = n())

#New Deaths
newDeaths <- newCounts[newCounts$RemovalType=="Died","NewCounts"]
newDeaths <- newDeaths$NewCounts

#New Recoveries
newRecovery <- newCounts[newCounts$RemovalType=="Recovered","NewCounts"]
newRecovery <- newRecovery$NewCounts

#Recovered
recovered <- table(coviddata$HealthStatus)
recovered <- recovered["Recovered"]

#Total count of cases
countofCases <- count(coviddata)

#Count of Total Deaths
totalDeaths <- coviddatasets %>%
  select(HealthStatus) %>%
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
  select(RegionRes) %>%
  count(RegionRes) %>%
  arrange(n) %>%
  top_n(-1)

#Monthly Trend
covidTrend <- coviddata %>%
  select(DateRepConf) %>%
  count(DateRepConf)
covidTrend <- covidTrend %>%
  ggplot(aes(DateRepConf, n )) +
  geom_line(color="#D980FA", size=1, alpha=0.9, linetype=1) +
  scale_x_date(expand = c(0, 0)) +
  theme_ipsum(
    grid_col = "#FDA7DF",
    axis_text_size=15,
    axis_title_size=18,
    axis_title_family="Oswald"
  ) +
  theme(
    axis.text.y = element_text(colour = "#D980FA"),
    axis.text.x = element_text(colour = "#D980FA"),
    axis.title.x = element_text(margin = margin(r = 50), colour = "#FDA7DF"),
    axis.title.y = element_text(colour = "#FDA7DF"),
    plot.background = element_rect(fill = "#fde9f6ad", color="#fde9f6ad"),
    plot.margin = margin(0,0,0,0),
    plot.title = element_text(family="Oswald", face="plain",colour="#D980FA", size="40",hjust = 0.5)) +
  labs(x="Month", y="# of cases", title="cases over time") +
  ylim(0, 600)
aspect_ratio <- 2.5
ggsave("www/plots/covidtrend.png", width = 17, height = 13, dpi = 72, units = "cm", device="png")

#Generate Pie chart for count per Region
nb.cols <- 20
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

regCount <- coviddatasets %>%
  select(RegionRes) %>%
  group_by(RegionRes) %>%
  summarise(CaseCount=n()) %>%
  top_n(5) %>%
  rename("Region" = "RegionRes") %>%
  arrange(desc(CaseCount))
regCount$Region[regCount$Region==""]<-"Uncategorized"
showRegCount <- ggplot(regCount, aes(x="", y=CaseCount, fill=Region)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual(values = mycolors) +
  theme(plot.margin = margin(0,0,0,0),
        plot.title = element_text(family="Oswald", colour="#66c2a5", size="40",hjust = 0.5)
  ) +
  ggtitle("top 5 region")
ggsave("www/plots/region.png", width = 17, height = 13, dpi = 72, units = "cm", device="png")

#Count of Pregnant Cases
pregnantCount <- coviddatasets %>%
  select(Pregnanttab) %>%
 filter(Pregnanttab=="Yes") %>%
  count(Pregnanttab)



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
  
  #Output latest # of deaths
  output$newDeaths <- renderText({
    {newDeaths}
  })

  #Output latest # of deaths
  output$newRecovery <- renderText({
    {newRecovery}
  })  
  
  #Output oldest case age
  output$oldestCaseAge <- renderText({
    {oldest}
  })  
  
  #Output count of pregnant cases
  output$pregnantCount <- renderText({
    {pregnantCount$n}
  })    
  
  #Output average of cases
  output$AverageAge <- renderText({
    {AverageAge}
  }) 
  
  #Output modal
  shinyalert(
    title = "Thanks for visiting!",
    text = paste("Current data is as of ",asofDate),
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "info",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "I understand.",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
  
}

shinyApp(ui = htmlTemplate("www/index.html"), server)