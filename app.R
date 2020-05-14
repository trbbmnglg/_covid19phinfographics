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
  newCase <- " data outdated ☹"
} else{
  newCase <- as.character(paste(newCase, "new cases!", sep=" "))
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

#Data For Male
malePerCentage <- coviddatasets %>% 
  filter(Sex=="Male") %>%
  count(Sex) %>%
  summarize(malePerCentage=round(n/countofCases, 2) * 100)
malePerCentage

#Data For Female
femalePerCentage <- coviddatasets %>% 
  filter(Sex=="Female") %>%
  count(Sex) %>%
  summarize(femalePerCentage=round(n/countofCases, 2) * 100)
femalePerCentage

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
    plot_title_size = 65,
    grid_col = "#FDA7DF",
    axis_text_size=15,
    axis_title_size=20,
    axis_title_family="Impact") +
  theme(plot.title = element_text(colour = "#D980FA"),
        axis.text.y = element_text(colour = "#D980FA"),
        axis.text.x = element_text(colour = "#D980FA"),
        axis.title.x = element_text(colour = "#FDA7DF"),
        axis.title.y = element_text(colour = "#FDA7DF")) +
  labs(x="Month", y="# of cases", title="MONTHLY TREND")



# Web UI
ui <- fluidPage(
  tags$head(
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
    tags$title("COVID 19 PH Infographics")
  ),
  align="center",
  
  includeCSS("www/design.css"),

    # Main panel for displaying outputs
    mainPanel(
      align = "center",
      width=100,
      h1(id="big-heading", "COVID-19 PH INFOGRAPHICS"),
      br(),
      
      #Display total count of cases
      div(
        class="casescount",
        span(id="countofCasesVal", strong(format(countofCases$n, nsmall=1, big.mark=","))),
        span(id="countofCasesText", "total cases")
      ),
      
      #Display total count of recovery
      div(
        class="recovered",
        format(recovered, nsmall=1, big.mark=","),
        " total recovered",
        img(src = "first-aid.png", height = 80, width = 80)
        ),
      
      #Display total count of deaths
      div(
        class="deaths",
        img(src = "cross.png", height = 80, width = 80),
        format(totalDeaths$n, nsmall=1, big.mark=","),
        " died"
        ),
      
      #Display percentage of gender
      div(
        class="gender",
        p(id="header-gender","gender % of total cases"),
        span(id="fpercent",femalePerCentage, "%", img(src = "woman.png", height = 80, width = 80) ),
        span(id="mpercent",malePerCentage, "%", img(src = "man.png", height = 80, width = 80)) ),
      
      #Display count of new case
      div(
        id="new",
        img(src = "bacteria.png", height = 80, width = 80),
        br(),
        newCase
        ),
      
      
      #Display Oldest case
      div(
        id="oldest",
        img(src = "back.png", height = 120, width = 120),
        div(id="oldest_case","OLDEST CASE"),
        div(id="oldest_desc" ,oldest, "yrs. old")
        ),
      
      #Display least case region
      div(
      class="leastcase",
      p(id="header-lc","region w/ least case"),
      div(
      id="container-lc",
      span(id="region-lc", strong(leastCasereg$RegionRes)),
      span(id="count-lc",strong(leastCasereg$n), " cases only"))
      ),
      
      br(),
      
      #Display highest case region
      div(
        class="highcase",
        p(id="header-hc","and highest case is"),
        div(
          id="container-hc",
          span(id="region-hc", strong(highCasereg$RegionRes)),
          span(id="count-hc","with ",strong(format(highCasereg$n, nsmall=1, big.mark=",")), " cases"))
      ),
      
      br(),
      
      #Display monthly trend
      div(
        class="monthly-trend",
        plotOutput(outputId = "monthlyCovidTrend")
      ),
      
      br(),
      br(),
      
      #Footer
      div(
        p(id="footer", file, ". Available in ",a("DOH", href="http://www.doh.gov.ph/2019-nCoV", target="_blank"), " website."),
        p(id="footer", "Icons made by ",
          a("Freepik",href="https://www.flaticon.com/authors/freepik", title="Freepik", target="_blank"),
          " | ", 
          a("Flat Icons",href="https://www.flaticon.com/authors/flat-icons", title="Flat-icons", target="_blank"),
          " | ",
          a("Smashicons", href="https://www.flaticon.com/authors/smashicons", title="Smashicons", target="_blank"),
          " from ",a("www.flaticon.com", href="https://www.flaticon.com")),
        p(id="footer", "Made with", span(id="heart","❤")  , "in R and Shiny |",a(" GitHub", href="https://github.com/rbrtbmnglg/_covid19phinfographics", target="_blank"))
        )
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$monthlyCovidTrend <- renderPlot({
    covidTrend
  })
}
shinyApp(ui = ui, server = server)