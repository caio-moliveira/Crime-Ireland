# Crime-Ireland
Exploratory Data Analysis Project Focusing on Crime Statistics in Ireland

---
- title: "Crimes in Ireland"
- author: "Caio Machado"
- date: "2024-01-08"
output:
  html_document: default
  word_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
#### Importing libraries

```{r Echo = FALSE, message=FALSE, warning = FALSE}
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(scales)
library(knitr)
library(rmarkdown)
library(gridExtra)
```
## Introdution



### Importing dataset 

```{r import}
ireland_crime <- read.csv("crimes_irelandV2.csv")
```
### Cleaning data

#### Removing rows that contain zero crimes. 

In order to clean values that won't be any use for the analysis, I decided remove the rows which the values are zeros.

```{r}
ireland_crime = ireland_crime[!(ireland_crime$VALUE == 0), ]
```

#### Removing columns 'STATISTIC.Label' and 'UNIT'

These columns were removed as won't be used for our analysis.

```{r}
ireland_crime <- ireland_crime[ , !(names(ireland_crime) %in% c('STATISTIC.Label'))]
ireland_crime <- ireland_crime[ , !(names(ireland_crime) %in% c('UNIT'))]
```

#### Splitting Quarter column into two different columns (Year and Quartile)

It looked more convenient for the analysis split the values inside the Quarter column, creating then two different columns.

```{r}
ireland_crime$Year <- as.integer(substr(ireland_crime$Quarter, 1, 4))
ireland_crime$Quartile <- as.integer(substr(ireland_crime$Quarter, 6, 7))
```

#### Removing column "Quarter"

After extracting the values of the Quarter column and creating two new ones, the column is removed.

```{r}
ireland_crime <- ireland_crime[ , !(names(ireland_crime) %in% c('Quarter'))]
```

#### Removing the words "Garda" and "Division" from the County names.

In order to reduce all the clutter to have the dataset more visual clear, I remove the words from each Garda Station, turning the column Garda.Division into County.

```{r}
ireland_crime$Garda.Division <- gsub("Garda Division", "", ireland_crime$Garda.Division)
```

#### Creating a new column called "Region" and assigning the corresponding region to each county.

A new column "Region" is creating taking the values of the regions in Ireland according to each county, making easier the analysis of regions.

```{r}
ireland_crime$Garda.Division <- trimws(ireland_crime$Garda.Division)

county_to_region <- c(
  "Cavan/Monaghan" = "Northern Region",
  "Donegal" = "Northern Region",
  "Sligo/Leitrim" = "Northern Region",
  "Louth" = "Northern Region",
  "Clare" = "Western Region",
  "Mayo" = "Western Region",
  "Galway" = "Western Region",
  "Roscommon/Longford" = "Western Region",
  "Cork City" = "Southern Region",
  "Cork North" = "Southern Region",
  "Cork West" = "Southern Region",
  "Kerry" = "Southern Region",
  "Limerick" = "Southern Region",
  "Laois/Offaly" = "Eastern Region",
  "Meath" = "Eastern Region",
  "Wicklow" = "Eastern Region",
  "Westmeath" = "Eastern Region",
  "Kildare" = "Eastern Region",
  "Tipperary" = "South Eastern Region",
  "Wexford" = "South Eastern Region",
  "Kilkenny/Carlow" = "South Eastern Region",
  "Waterford" = "South Eastern Region",
  "D.M.R. South Central" = "Dublin Region",
  "D.M.R. North Central" = "Dublin Region",
  "D.M.R. Northern" = "Dublin Region",
  "D.M.R. Southern" = "Dublin Region",
  "D.M.R. Eastern" = "Dublin Region",
  "D.M.R. Western" = "Dublin Region"
)


ireland_crime$Region <- county_to_region[ireland_crime$Garda.Division]
```

#### Renaming column names

Renaming each column in order to make more easier the analysis when calling each of these columns names inside the code.

```{r}
# Change column names
colnames(ireland_crime)[colnames(ireland_crime) == "Garda.Division"] <- "County"
colnames(ireland_crime)[colnames(ireland_crime) == "Type.of.Offence"] <- "Offence"
colnames(ireland_crime)[colnames(ireland_crime) == "VALUE"] <- "Value"
colnames(ireland_crime)[colnames(ireland_crime) == "Quartile"] <- "Quarter"

```

#### Last check

Last check if there is any missing value in our dataset.

```{r}
na_df <- sum(is.na(ireland_crime))
```


## Analysis

### Comparison between 2022 and 2023 on the third Quarter.

Recent months have seen a growing focus on crime in Ireland. This surge in attention is largely due to a significant shift in the patterns of criminal activities, highlighting the necessity of an in-depth analytical study.

The decision to focus on Ireland in this analysis was influenced by the wide range of criminal offenses observed, reflecting the diverse and nature of crime within the country. This project aims to present a detailed analysis of the patterns and shifts in Irish crime data, particularly emphasizing the years 2022 and 2023.

The analysis is designed to delve into the complexities of the data, highlighting key areas where crime rates have increased or decreased. Through this exploration, we seek to uncover underlying trends and provide insights into the changing landscape of crime in Ireland.


```{r top10Crimes_22_23, echo=FALSE}

# Filter records for Quarter 3 of 2022 and 2023
filtered_data_2022_2023 <- ireland_crime %>%
  filter((Year == 2022 | Year == 2023) & Quarter == 3)

# Group by "Offence" and calculate the count of offences for each year
offences_count_2022_2023 <- filtered_data_2022_2023 %>%
  group_by(Offence, Year) %>%
  summarize(Offences_Count = sum(Value), .groups = "drop")

# Spread the data for each year into separate columns and calculate total count increase
offences_spread <- offences_count_2022_2023 %>%
  pivot_wider(names_from = Year, values_from = Offences_Count) %>%
  mutate(Total_Count_Increase = `2023` - `2022`)

# Calculate the percentage increase
offences_spread <- offences_spread %>%
  mutate(Percentage_Increase = (Total_Count_Increase / `2022`) * 100)

# Filter for the top 10 crimes with the highest total count increase
top_10_increases_2022_2023 <- offences_spread %>%
  arrange(desc(Total_Count_Increase)) %>%
  head(10)
```


### Exploratory Data Analysis

Critical areas of concern are highlighted by the data from 2022 to 2023, especially in crimes that directly harm people, such extortion, car theft, and stealing from people. 


- Theft and Related Offences
    - Increase: **5.51%** 
    - Total increase: **1003 incidents**
    
- Theft/Taking of Vehicle and Related Offences
    - Increase: **27.31%**

- Theft from Person
    - Increase: **54.13%** 
    - From **654 to 1008 incidents**

- Theft from Shop
    - Increase: **4.18%**

- Possession of Drugs for Sale or Supply
    - Increase: **16.34%**

- Robbery, Extortion, and Hijacking Offences
    - Increase: **22.39%** 
    - From **545 to 667 incidents**

- Blackmail or Extortion
    - Increase: **53.38%** 
    - From **148 to 227 incidents**

```{r, fig.width=10, fig.height=6}
# Your first plot (Percentage Increase)
plot1 <- ggplot(top_10_increases_2022_2023, aes(x = reorder(Offence, Percentage_Increase), y = Percentage_Increase)) +
  geom_bar(stat = "identity", aes(fill = Percentage_Increase > 50), width = 0.9) +
  scale_fill_manual(values = c("TRUE" = "lightblue", "FALSE" = "grey")) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage_Increase)), 
            position = position_dodge(width = 0.9), 
            hjust = 1.2) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Percentage Increase in Crimes from 2022 to 2023", x = "", y = "") +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none")

# Your second plot (Total Count Increase)
plot2 <- ggplot(top_10_increases_2022_2023, aes(x = reorder(Offence, Total_Count_Increase), y = Total_Count_Increase)) +
  geom_bar(stat = "identity", aes(fill = Total_Count_Increase > 500), width = 0.9) +
  scale_fill_manual(values = c("TRUE" = "lightblue", "FALSE" = "grey")) +
  geom_text(aes(label = Total_Count_Increase), 
            position = position_dodge(width = 0.3), 
            hjust = 1.3) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Total Count Increase in Crimes from 2022 to 2023", x = "", y = "") +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none")

# Arrange the plots side by side
grid.arrange(plot1, plot2, nrow = 2)
```

![Rplot](https://github.com/caio-moliveira/Crime-Ireland/assets/150807759/0e6a6667-5d9f-494b-ab86-00f1b1782f22)



### Overall Analysis
The data presents a varied landscape of crime in Ireland, with significant increases in certain types of offences, such as theft from individuals, vehicle-related theft, and blackmail or extortion. The modest increases in other areas, like shoplifting and burglary, indicate that these remain persistent issues, but they have not escalated dramatically in the analyzed period. The substantial rises in certain categories could indicate shifting patterns in criminal activities or increased reporting. 



## Crimes per regions

``` {r, echo=FALSE}
# Function to calculate regional increases and percentage for a given crime
calculate_regional_increases_percentage <- function(crime_name) {
  # Filter data for 2022 and 2023 for the specific crime
  data_2022 <- ireland_crime %>%
    filter(Year == 2022, Quarter == 3, Offence == crime_name)
  data_2023 <- ireland_crime %>%
    filter(Year == 2023, Quarter == 3, Offence == crime_name)

  # Summarize by region
  region_sum_2022 <- data_2022 %>%
    group_by(Region) %>%
    summarize(Total_2022 = sum(Value), .groups = 'drop')
  region_sum_2023 <- data_2023 %>%
    group_by(Region) %>%
    summarize(Total_2023 = sum(Value), .groups = 'drop')

  # Merge and calculate the increase and percentage
  merged_data <- merge(region_sum_2022, region_sum_2023, by = "Region")
  merged_data$Increase <- merged_data$Total_2023 - merged_data$Total_2022
  merged_data$Percentage_Increase <- (merged_data$Increase / merged_data$Total_2022) * 100

  # Add the crime name
  merged_data$Offence <- crime_name

  return(merged_data)
}

# Apply this function to each of the top 10 crimes
top_10_crimes <- top_10_increases_2022_2023$Offence
regional_increases_percentage_list <- lapply(top_10_crimes, calculate_regional_increases_percentage)

# Combine the results into a single data frame
regional_increases_percentage <- do.call(rbind, regional_increases_percentage_list)

# Rearrange columns for clarity
regional_increases_percentage <- regional_increases_percentage %>%
  select(Offence, Region, Total_2022, Total_2023, Increase, Percentage_Increase)

```

### Exploratory Data Analysis

This regional analysis clearly indicates that certain types of crime have surged in specific regions. For instance, "Blackmail or Extortion" has seen a dramatic increase across several regions, with the Western Region experiencing the most significant rise. Similarly, "Theft/Taking of Vehicle and Related Offences" and "Robbery, Extortion, and Hijacking Offences" are notably higher in the Northern and South Eastern regions, respectively.


#### Dublin Region
- Theft from Person
    - Increase: **65.7%**

#### Eastern Region
- Blackmail or Extortion
    - Increase: **58.3%**

#### Northern Region
- Theft/Taking of Vehicle and Related Offences
    - Increase: **84.9%**

#### South Eastern Region
- Robbery, Extortion, and Hijacking Offences
    - Increase: **76.7%**
- Blackmail or Extortion
    - Increase: **61.5%**

#### Southern Region
- Blackmail or Extortion
    - Increase: **60.7%**

#### Western Region
- Blackmail or Extortion
    - Increase: **200%**
- Robbery, Extortion, and Hijacking Offences
    - Increase: **91.3%**
- Theft/Taking of Vehicle and Related Offences
    - Increase: **83.8%**



``` {r, fig.width=10, fig.height=6}
# Make sure the data is in the correct format
regional_increases_percentage$Region <- as.factor(regional_increases_percentage$Region)

# Get a list of all regions
regions <- unique(regional_increases_percentage$Region)

for (region in regions) {

  region_data <- subset(regional_increases_percentage, Region == region)

  # Check if there is data for the region
  if(nrow(region_data) > 0) {
    

    # Adjust the order of Offences based on Percentage_Increase for the current region
    region_data$Offence <- reorder(region_data$Offence, region_data$Percentage_Increase)
    
    # Create the plot
    plot <- ggplot(region_data, aes(x = Offence, y = Percentage_Increase)) +
      geom_bar(stat = "identity", aes(fill = Percentage_Increase > 50), width = 0.5) +
      scale_fill_manual(values = c("TRUE" = "lightblue", "FALSE" = "grey")) +
      geom_text(aes(label = sprintf("%.1f%%", Percentage_Increase),
                    hjust = ifelse(Percentage_Increase > 0, 1.1, -0.1)),
                position = position_dodge(width = 0.5),
                color = "black",
                size = 4) +
      coord_flip() +
      theme_minimal() +
      labs(title = paste("Increasing of Crimes in", region, "from 2022 to 2023"),
           x = "",
           y = "Percentage Increase") +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      )

    # Print the plot
    print(plot)
  } else {
    print(paste("No data for region:", region))
  }
}
```
![Western-region](https://github.com/caio-moliveira/Crime-Ireland/assets/150807759/a09f81e1-0838-433d-ae98-7427d3de1a1f)
![Dublin-region](https://github.com/caio-moliveira/Crime-Ireland/assets/150807759/0cd2a956-2cb5-47ad-8df8-162a2e09dc4e)
![Eastern-region](https://github.com/caio-moliveira/Crime-Ireland/assets/150807759/6f7017f8-1a17-41f0-9ba1-1ca9cbc54475)
![Northern-region](https://github.com/caio-moliveira/Crime-Ireland/assets/150807759/88bfc9b4-2963-4bd7-b89a-0890daeed523)
![South-Eastern-region](https://github.com/caio-moliveira/Crime-Ireland/assets/150807759/94d0280c-b8bd-44ea-bfd9-6cb17a199cc2)
![Southern-region](https://github.com/caio-moliveira/Crime-Ireland/assets/150807759/30b41ea7-ccf3-484f-97d6-e494297acd10)


