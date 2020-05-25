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
coviddata <<- coviddatasets %>%
  select(Sex,Age,AgeGroup,RegionRes,HealthStatus,DateRepConf, Pregnanttab, RemovalType, DateRepRem) %>%
  arrange(desc(Age))

#Format Date Rep Conf since it has a heterogeneous format (use lubridate) 
coviddata$DateRepConf <- parse_date_time(coviddata$DateRepConf,c("dmY", "ymd"))
coviddata$DateRepConf <- as.Date(coviddata$DateRepConf,format="%m/%d/%Y")

#Format Date Rep Remove since it has a heterogeneous format (use lubridate) 
coviddata$DateRepRem <- parse_date_time(coviddata$DateRepRem,c("dmY", "ymd"))
coviddata$DateRepRem <- as.Date(coviddata$DateRepRem,format="%m/%d/%Y")

#Age select and calculation
age <- coviddata %>%
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

#Total count of cases
countofCases <- count(coviddata)

source("getdata.R")

#Health status count
recovered <- getData(coviddata, HealthStatus,"Recovered", "Count")
totalDeaths <- getData(coviddata, HealthStatus,"Died","Count")
critical <- getData(coviddata, HealthStatus,"Critical","Count")
asymptomatic <- getData(coviddata, HealthStatus,"Asymptomatic","Count")
mild <- getData(coviddata, HealthStatus,"Mild","Count")
severe <- getData(coviddata, HealthStatus,"Severe","Count")

#Gender percentage
malePercentage <- getData(coviddata, Sex, "Male", "Perc")
femalePercentage <- getData(coviddata, Sex, "Female", "Perc")

#Count of Pregnant Cases
pregnantCount <- getData(coviddata, Pregnanttab, "Yes", "Count")

source("plots.R")

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
    {format(recovered, nsmall=1, big.mark=",")}
  })
  
  #Output Total Deaths
  output$died <- renderText({
    {format(totalDeaths, nsmall=1, big.mark=",")}
  })

  #Output Total asymptomatic
  output$asymptomatic <- renderText({
    {format(asymptomatic, nsmall=1, big.mark=",")}
  })
  
  #Output Total critical
  output$critical <- renderText({
    {format(critical, nsmall=1, big.mark=",")}
  })

  #Output Total mild
  output$mild <- renderText({
    {format(mild, nsmall=1, big.mark=",")}
  })
  
  #Output Total severe
  output$severe <- renderText({
    {format(severe, nsmall=1, big.mark=",")}
  })
  
  #Output New Case
  output$newCase <- renderText({
    {newCase}
  })
  
  #Output Active Cases
  activeCase <- totalDeaths + recovered
  output$ActiveCase <- renderText({
    {format((countofCases$n - activeCase), nsmall=1, big.mark=",")}
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
    {pregnantCount}
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
  
  #Test ggplotly
  output$testPlot <- renderPlotly({
    print(ggplotly(covidTrend))
  })
  
  #Test ggplotly
  output$testPlot2 <- renderPlotly({
    print(ggplotly(diedandrecovered))
  })
  
}

shinyApp(ui = htmlTemplate("www/index.html"), server)