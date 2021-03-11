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

# Create data set
access_2010 <- CA %>% 
  filter(Variable_Code == "LACCESS_POP10")
access_2010
write.csv(x=access_2010, file="access_2010")


# View access to grocery stores as a percentage change across counties between 2010 and 2015
CA %>% 
  filter(Variable_Code == "PCH_LACCESS_POP_10_15") %>% 
  arrange(Value) %>% 
  ggplot(aes(reorder(x = County, -Value), y = Value)) + 
  geom_col() +
  coord_flip()


# Geographic map
CA_access_2010 <- CA %>% 
  filter(Variable_Code == "LACCESS_POP10")
CA_access_2010
length(CA_access_2010$Value)



CA_access_2010$color <- gray(n = length(CA_access_2010$Value), CA_access_2010$Value / max(CA_access_2010$Value))

map(database = "county", regions = "California", fill = TRUE, col = CA_access_2010$color, )

  
