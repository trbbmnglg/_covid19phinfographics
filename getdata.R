#Function to get specific count or percentage of selected fields and conditions
getData <- function(data, column, value, returnType){
  column <- enquo(column)
  healthStatus <- data %>%
    select(!!column) %>%
    group_by(!!column) %>%
    summarise(
      count = n(),
      perc = round((count / nrow(.)), 2 ) * 100) %>%
    filter(!!column==value)
  
  #Return value base on the returnType parameter
  if(returnType == "Count"){
    returnCount <- healthStatus$count
    returnCount
  } else if(returnType == "Perc"){
    returnCount <- healthStatus$perc
    returnCount
  }
  
}