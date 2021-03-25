##### Food Atlas Data ######
library(tidyverse)

# Documentation
browseURL("https://www.ers.usda.gov/data-products/food-environment-atlas/documentation/")

# Import Variable list
VariableList <- read_csv("VariableList.csv")
View(VariableList)

# Import Data
state_county_data <- read_csv("StateAndCountyData.csv")
head(state_county_data)
tail(state_county_data)


################################# California ##########################################
# Filter Out States
CA <- state_county_data %>% 
  filter(State == "CA")
CA

# View access to store count 2015
CA %>% 
  filter(Variable_Code == "LACCESS_POP10" | Variable_Code == "LACCESS_POP15") %>% 
  ggplot(aes(reorder(x = County, -Value), y = Value, fill = Variable_Code)) + 
  geom_bar(stat='identity') +
  coord_flip()



  
