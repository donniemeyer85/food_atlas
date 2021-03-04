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

# Look at Unique Values
col_names <- colnames(state_county_data)
col_names <- col_names[2:(length(col_names)-1)]

for (name in col_names) {
  print(unique(state_county_data %>% pull(name)))
  print(length(unique(state_county_data %>% pull(name))))
}

# Filter Data for rows constaing LACCESS_POP10
state_county_data %>% 
  filter(Variable_Code == "PCT_HISP10")

state_county_data %>% 
  filter(Variable_Code == "PCT_HISP10") %>% 
  group_by(State) %>% 
  summarise(avg_value = mean(Value)) %>% 
  ggplot(aes(x = reorder(State, -avg_value), y = avg_value)) +
  geom_col(as=2) +
  xlab("State") +
  ylab("Mexican Population") +
  ggtitle("Mexican Population by State (%)") +
  theme(axis.text.x=element_text(angle=90,hjust=1))

