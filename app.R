library(dplyr)
library(shiny)
library(png)
library(chron)

coviddatasets <- read.csv("_datasets/_2020-05/DOH COVID Data Drop_ 20200511 - 05 Case Information.csv", header=TRUE,sep=",",quote="\"")

coviddata <- coviddatasets %>%
  select(Sex,Age,AgeGroup,RegionRes,HealthStatus,DateRepConf) %>%
  arrange(desc(Age))
coviddata

#MinAge
MinAge <- min(coviddata$Age, na.rm = TRUE)

#MaxAge
oldest <- max(coviddata$Age, na.rm = TRUE)
youngest <- median(coviddata$Age, na.rm= TRUE)

#Heatlh Status of the cases
table(coviddata$HealthStatus)

#Latest case for the current date
#Change once new dataset is available
newCase <- table(coviddata$DateRepConf)
newCase <- newCase["2020-05-11"]
newCase

#Recovered
recovered <- table(coviddata$HealthStatus)
recovered <- recovered["Recovered"]

countofCases <- count(coviddata)
#Data For Male
maleCount <- table(coviddata$Sex)
maleCount <- maleCount["Male"]
maleCount
malePerCentage <- round(maleCount / countofCases,2) * 100
malePerCentage


#Data For Female
femaleCount <- table(coviddata$Sex)
femaleCount <- femaleCount["Female"]
femaleCount
femalePerCentage <- round(femaleCount / countofCases,2) * 100
femalePerCentage

yesterday <- Sys.Date()-1

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  align="center",
  
  includeCSS("www/design.css"),

    # Main panel for displaying outputs ----
    mainPanel(
      align = "center",
      width=100,
      h1(id="big-heading", "COVID-19 PH INFOGRAPHICS"),
      br(),
      br(),
      p(span(id="countofCasesVal", format(countofCases$n, nsmall=1, big.mark=",")),
      span(id="countofCasesText", "TOTAL CASES")),
      br(),
      p(id="recovered", format(recovered, nsmall=1, big.mark=","), " total recovered",  img(src = "first-aid.png", height = 80, width = 80)),
      
      #Percentage of the freuency of Male and Female
      div(
        id="gender",
        span(id="fpercent",femalePerCentage, "%", img(src = "woman.png", height = 90, width = 90) ),
        span(id="mpercent",malePerCentage, "%", img(src = "man.png", height = 90, width = 90)) ),
      
      #Count of new case
      p(id="new", strong(img(src = "bacteria.png", height = 80, width = 80), newCase, " new")),
      
      
      #Oldest case
      div(
        id="oldest",
        img(src = "back.png", height = 120, width = 120),
        div(id="oldest_case","OLDEST CASE"),
        div(id="oldest_desc" ,oldest, "yrs. old")),
      
      
      br(),
      
      #Footer
      div(
        p(id="footer", "Dataset used: DOH COVID Data Drop_ 20200511 - 05 Case Information.csv. Available in DOH website."),
        p(id="footer", "Icons made by ", a("Freepik",href="https://www.flaticon.com/authors/freepik"), " and ", a("Flat Icons",href="https://www.flaticon.com/authors/flat-icons"), " from ",
        a("www.flaticon.com", href="https://www.flaticon.com")),
        p(id="footer", "Made with", span(id="heart","❤")  , "in R and Shiny ")
        )
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
}
shinyApp(ui = ui, server = server)