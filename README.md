# Purpose
r code for Empirical Chapter 2 of the doctoral dissertation "Beyond the Ballot: Reviewing Canadian Political Parties' Leadership Selection Rules" completed at Université Laval and Université libre de Bruxelles (Cevipol). This chapter quantitatively tests for the likelihood of Anomalous outcomes and Act Contingent events to precede a selectorate reform.

## Notes on data
The leadership race inventory was conducted in the fall of 2023. The below code applies a series of transformations to the the various variables. The final dataset output of those transformations is the data used in Empirical Chapter 2 of the dissertation. For the original dataset or the transformed dataset, please contact the author.


# Research Questions
**Q1. Anomalous Outcome 1** Is a party who lost seats in the legislature following the election that preceded the leadership race more likely to adopt a new selectorate in the subsequent leadership race?

**Q2. Anomalous Outcome 2** Is a party who lost votes following the election that preceded the leadership race more likely to adopt a new selectorate in the subsequent leadership race?

**Q3. Anomalous Outcome 3** Is a party whose parliamentary status receded following the election preceding the leadership race more likely to adopt a new selectorate in the subsequent leadership race?

**Q4. Act Contingent 1** Is a party using a selectorate that differs from that of other parties in its jurisdiction more likely to adopt a new selectorate?

**Q5. Act Contingent 2** Is a party using a selectorate that differs from that other parties in its political family more likely to adopt a new selectorate?
Outcome Contingent

**Q6. Outcome Contingent 1** Is a party using a selectorate that differs from that of other parties in its jurisdiction more likely to adopt a new selectorate when that party gets more seats at the election following the leadership race?

**Q7. Outcome Contingent 2** Is a party using a selectorate that differs from that other parties in its political family more likely to adopt a new selectorate when that party gets more votes at the election following the leadership race?

**Q8. Outcome Contingent 3** Is a party using a selectorate that differs from that other parties in its political family more likely to adopt a new selectorate when that party improved its status in parliament at the election following the leadership race?

**Q9. Consultative Process 1** Which party actors are involved in the consultation process?

**Q10. Consultative Process 2** What occurs during the consultation process?

**Q11. Consultative Process 3** Which of the factors depicted Figure 1 necessary for a reform to advance to the consultation process?

**Q12. Reform Enactment** How do parties enact the reform of the leadership selectorate?


# Code
```R
#set path
setwd("~/_rCodesData/chapter2AllLdrRaces")
```

## load packages
```
#For most of the environment
library(tidyverse)
require(GGally)
require(reshape2)
library(readxl)
```

## Importing the Excel file

This file represents the leadership race inventory conducted in the Fall of 2023. 

```
data <- read_excel("_bdChefferie_2023-12-11.xlsx")
```

## Convert & clean some variables for models later

```
# Convert number variables to numeric values 
data <- data %>%
  mutate(across(c(signatureQty:comlianceDeposit, percentWonLost, seatsWonLost), as.numeric))


# Create a unique tag for each party
data <- data %>%
  group_by(jurisdiction, party) %>%
  mutate(nparty = cur_group_id())


# sort by party id
data <- data %>%
  group_by(nparty)


# calculate the government status change between two elections

data$parlStatusChange <- NA
data$parlStatusChange <-  data$parlStatusPreviousElect - data$parlStatusMomentLdrRace

data$seatsWonLost <- as.numeric(data$seatsWonLost)


#Create a panel data
library(plm)


# Declare panel data structure using plm
pdata <- pdata.frame(data, index = c("nparty", "id"))


# reorder the family categorical for more intuitive numbering
# Reorder the levels of the factor variable
pdata$family <- factor(pdata$family, levels = c("conservative", "liberal", "new democratic", "green", "sovereigntist"))

pdata$jurisdiction <- factor(pdata$jurisdiction, levels = c("canada", "bc", "ab", "sk", "mb", "on", "qc", "nb", "ns", "pei","nl"))


# Transform categories into numeric
pdata <- pdata %>%
  mutate(nSelectorate = as.numeric(as.factor(selectorate))) %>%
  mutate(nFamily = as.numeric(as.factor(family))) %>%
  mutate(nJurisdiction = as.numeric(as.factor(jurisdiction)))


#arrange by party name, and then by year (for next step)
pdata <- pdata %>% arrange(nparty, yearLdrVote)



#  a new variable that identifies changes nSelectorate. Variable name: `ref_nselectorate`
pdata <- pdata %>%
  arrange(yearLdrVote) %>%
  group_by(jurisdiction, party) %>%
  mutate(ref_nselectorate = ifelse(c(0, diff(nSelectorate)) != 0, 1, 0)) %>%
  ungroup()

#a new variable that identifies the type of change. Where a positive value woud mean decentralization and a negative value would be centralization. The number represents how many levels of change on the selectorate scale. Variable name: `ref_nselectorateDirection`

pdata <- pdata %>%
  arrange(yearLdrVote) %>%
  group_by(jurisdiction, party) %>%
  mutate(ref_nselectorate = ifelse(c(0, diff(nSelectorate)) != 0, 1, 0),
         ref_nselectorateDirection = ifelse(ref_nselectorate == 1, c(0, diff(nSelectorate)), NA)) %>%
  ungroup()

# pdata a new variable that identifies parties in same jurisdiction that may have influenced reform (H4)
# Function to check if any other party in the same jurisdiction used the same selectorate value in the preceding years
check_influence_jurisdiction <- function(yearLdrVote, jurisdiction, selectorate) {
  any(data$selectorate[pdata$jurisdiction == jurisdiction & pdata$yearLdrVote < yearLdrVote] == selectorate)
}

# Create a binary variable indicating if parties with ref_nselectorate = 1 may have been influenced by other parties in the jurisdiction
pdata <- pdata %>%
  mutate(influencedByOthersJurisdiction = ifelse(ref_nselectorate == 1, mapply(check_influence_jurisdiction, yearLdrVote, jurisdiction, selectorate), NA))

#replace the TRUE/FALSE values with 1/0
pdata$influencedByOthersJurisdiction[pdata$influencedByOthersJurisdiction == TRUE] <- 1
pdata$influencedByOthersJurisdiction[pdata$influencedByOthersJurisdiction == FALSE] <- O


# pdata a new variable that identifies parties in same family that may have influenced reform (H5)
# Function to check if any other party in the same family used the same selectorate value in the preceding years
check_influence_family <- function(yearLdrVote, family, selectorate) {
  any(data$selectorate[data$family == family & data$yearLdrVote < yearLdrVote] == selectorate)
}

# Create a binary variable indicating if parties with ref_nselectorate = 1 may have been influenced by other parties in the same family
pdata <- pdata %>%
  mutate(influencedByOthersFamily = ifelse(ref_nselectorate == 1, mapply(check_influence_family, yearLdrVote, family, selectorate), NA))

#replace the TRUE/FALSE values with 1/0
pdata$influencedByOthersFamily[pdata$influencedByOthersFamily == TRUE] <- 1
pdata$influencedByOthersFamily[pdata$influencedByOthersFamily == FALSE] <- O

#create a variable that represents a % of seatsWonLost compared to the # of seats in the legislature. It makes parties comparables

pdata$percentSeatWonLost <- NA
pdata$percentSeatWonLost <- (pdata$seatsWonLost / pdata$seatsInLegislature) * 100

#create binary variables Anomalous outcomes

#lost seats yes/no
pdata$lostSeatsDummy <- NA
pdata$lostSeatsDummy[pdata$seatsWonLost < 0 ] <- 1
pdata$lostSeatsDummy[pdata$seatsWonLost >= 0 ]<- 0

#lost votes yes/no
pdata$lostVotesDummy <- NA
pdata$lostVotesDummy[pdata$percentWonLost < 0 ] <- 1
pdata$lostVotesDummy[pdata$percentWonLost >= 0 ]<- 0

#lost parliamentary status yes/no
pdata$lostParlStatDummy <- NA
pdata$lostParlStatDummy[pdata$parlStatusChange < 0 ] <- 1
pdata$lostParlStatDummy[pdata$parlStatusChange >= 0 ]<- 0


#rename cols of reform to have clearer tables in the console
pdata$ref_nselectorateText <- NA
pdata$ref_nselectorateText[pdata$ref_nselectorate == 1] <- "Reform"
pdata$ref_nselectorateText[pdata$ref_nselectorate == 0] <- "No Reform"

```

## Export the new dataset as a `.csv` file. 


```
library(rio)

export(pdata, "canadianLdrRacesBrennanDissertation_march2024.csv")
```


## Export the codebook of variables into a Word document


```
library(codebookr)
library(dplyr, warn.conflicts = FALSE)
library(haven)

# pass the df through the codebook function
ldrRaceCanadaCodeBook <- codebook(pdata)

#export in Word. It exports in the file where you keep the dataset
print(codebook(pdata), "ldrRaceCanadaCodeBook.docx")

```

## Visualizing the variables

The `ggplot2` outputs are stored in the repository as PDFs (see above files)

```
# Define custom colors for each party family 
family_colors <- c("conservative" = "blue", "liberal" = "red", "new democratic" = "darkorange", "green" = "darkgreen", "sovereigntist" = "darkblue")  # Add more colors as needed
```


### Plot the observations of my data

File name `allLdrRacesDataCh2.pdf`

```
pdata %>%
  #filter(!is.na(ref_nselectorateText)) %>%
  mutate(jurisdiction = factor(jurisdiction, levels = rev(levels(jurisdiction)))) %>% # reordering the jurisdiction order because of the coord_flip
  ggplot(aes(x = jurisdiction, fill = family)) +
  geom_bar(position = position_dodge(preserve = "single"), color = "white") +  # Modify position to ensure consistent bar widths
  labs(#title = "Leadership Races per Jurisdiction by Party Family",
    x = " ",
    y = " ") +
  scale_fill_manual(name = "Party Family", values = family_colors) +  # Use custom colors
  scale_y_continuous(breaks = seq(0, 15, by = 1)) +  # Set breaks every 5 units
  theme_minimal() +
  theme(axis.text.x = element_text(size = 16, color = "black"),  # Rotate x-axis labels for better readability
        axis.text.y = element_text(size = 25, color = "black"),
        legend.position = "top",
        legend.text = element_text(size = 20),  # Adjust size of legend text
        legend.title = element_text(size = 25),  # Adjust size of legend title
        strip.text = element_text(size = 30),   # Adjust size of facet labels
        strip.background = element_blank()) +   # Remove strip background
  facet_wrap(~ref_nselectorateText) +
  coord_flip()

```

### Boxplot Seats (Absolute QTY)

File name `boxplotSeatQty.pdf`

```
   pdata %>%
        filter(!is.na(ref_nselectorateText)) %>%
      ggplot(aes(x = ref_nselectorate, y = seatsWonLost, fill = as.factor(ref_nselectorateText))) +
        geom_boxplot() +
        geom_jitter (alpha = .3, size = 1) +
        labs(title = "",
             x = "",
             y = "") +
        theme_minimal() +
        scale_x_discrete(labels = NULL) +
        scale_fill_manual(name = "Reformed Selectorate?",
                          values = c("white", "grey")) +
        scale_y_continuous(breaks = seq(-100, 100, by = 5)) +  # Set breaks every 5 units
        theme(axis.text.y = element_text(size = 14),  # Set size of y-axis text
              axis.text.x = element_blank(),          # Remove x-axis text
              axis.title.x = element_blank(),         # Remove x-axis title
              axis.ticks.y = element_line(size = 1.5),  # Increase size of y-axis ticks
              legend.position = "top",                # Move legend to top
              legend.text = element_text(size = 20))  # Set size of legend text
```     

### Boxplot Percent of seats in the house

File name `boxplotSeatPercent.pdf`


```
        pdata %>%
          filter(!is.na(ref_nselectorateText)) %>%
        ggplot(aes(x = ref_nselectorate, y = percentWonLost, fill = as.factor(ref_nselectorateText))) +
          geom_boxplot() +
          geom_jitter (alpha = .3, size = 1) +
          labs(title = "",
               x = " ",
               y = " ") +
          theme_minimal() +
          scale_x_discrete(labels = NULL) +
          scale_fill_manual(name = "Reformed Selectorate?",
                            values = c("white", "grey")) +
          scale_y_continuous(breaks = seq(-100, 100, by = 5)) +  # Set breaks every 5 units
          theme(axis.text.y = element_text(size = 14),  # Set size of y-axis text
                axis.text.x = element_blank(),          # Remove x-axis text
                axis.title.x = element_blank(),         # Remove x-axis title
                axis.ticks.y = element_line(size = 1.5),  # Increase size of y-axis ticks
                legend.position = "top",                # Move legend to top
                legend.text = element_text(size = 20))  # Set size of legend text
```

### Boxplot Change in Vote share (percent of vote)

File name `votesshareWonLost.pdf`


```
      pdata %>%
        filter(!is.na(ref_nselectorateText)) %>%
        ggplot(aes(x = ref_nselectorate, y = percentWonLost, fill = as.factor(ref_nselectorateText))) +
        geom_boxplot() +
        geom_jitter (alpha = .3, size = 1) +
        labs(title = "",
             x = " ",
             y = " ") +
        theme_minimal() +
        scale_x_discrete(labels = NULL) +
        scale_fill_manual(name = "Reformed Selectorate?",
                          values = c("white", "grey")) +
        scale_y_continuous(breaks = seq(-100, 100, by = 5)) +  # Set breaks every 5 units
        theme(axis.text.y = element_text(size = 14),  # Set size of y-axis text
              axis.text.x = element_blank(),          # Remove x-axis text
              axis.title.x = element_blank(),         # Remove x-axis title
              axis.ticks.y = element_line(size = 1.5),  # Increase size of y-axis ticks
              legend.position = "top",                # Move legend to top
              legend.text = element_text(size = 20))  # Set size of legend text
```

### Boxplot Change in Parliamentary Status

File name `parlStatChange.pdf`


```
        pdata %>%
          filter(!is.na(ref_nselectorateText)) %>%
          ggplot(aes(x = as.factor(parlStatusChange), fill = as.factor(ref_nselectorateText))) +
          geom_bar(position = "dodge", color = "black") +
          labs(title = "",
               x = " ",
               y = " ") +
          scale_fill_manual(name = "Reformed Selectorate?",
                            values = c("white", "grey")) +
          scale_y_continuous(breaks = seq(-100, 100, by = 5)) +
          theme_minimal() +
          theme(axis.text.y = element_text(size = 14, ),  
                axis.text.x = element_text(size = 14,
                                           face = "bold"),  
                axis.ticks.y = element_line(size = 1.5),  
                legend.position = "top",                
                legend.text = element_text(size = 20))
```

### Barplot Parliamentary status when the race was launched

File name `parlStatMomentRace.pdf`

```
pdata %>%
  filter(!is.na(ref_nselectorateText)) %>%
  ggplot(aes(x = factor(parlStatusMomentLdrRace), fill = as.factor(ref_nselectorateText))) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "",
       y = " ") +
  scale_x_discrete(labels = c("Government", "Opposition", "Third Party", "Other")) +
  scale_fill_manual(name = "Reformed Selectorate?",
                    values = c("white", "grey")) +
  scale_y_continuous(breaks = seq(-100, 100, by = 5)) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 14, ),  
        axis.text.x = element_text(size = 14,
                                   face = "bold"),  
        axis.ticks.y = element_line(size = 1.5),  
        legend.position = "top",                
        legend.text = element_text(size = 20))

```



## Assumption tests
```
library(nortest)
library(outliers)
```

### Normality Testing
```
shapiro_iv1 <- shapiro.test(pdata$seatsWonLost)
shapiro_iv2 <-shapiro.test(pdata$percentWonLost)
```
### Homogeneity of Variance

```
t_test_resultH1 <- t.test(seatsWonLost ~ ref_nselectorate, data = pdata, var.equal = FALSE)

#Welch Two Sample t-test

#data:  seatsWonLost by ref_nselectorate
#t = 0.92257, df = 13.026, p-value = 0.373
#alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
#95 percent confidence interval:
#-6.62387 16.50134
#sample estimates:
#  mean in group 0 mean in group 1 
#-7.152174      -12.090909 


t_test_resultH2 <- t.test( percentWonLost~ ref_nselectorate, data = pdata, var.equal = FALSE)

#Welch Two Sample t-test

#data:  percentWonLost by ref_nselectorate
#t = 0.95297, df = 13.684, p-value = 0.3571
#alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
#95 percent confidence interval:
#  -2.948979  7.646606
#sample estimates:
#  mean in group 0 mean in group 1 
#-3.823187       -6.172000 
```




### Outlier Detection
```
outliers_iv1 <-   boxplot.stats(pdata$seatsWonLost)$out
outliers_iv2 <- boxplot.stats(pdata$percentWonLost)$out
```

### Correlation Analysis

```
pdataSpearman <- pdata%>%
  filter(!is.na(ref_nselectorate) & !is.na(seatsWonLost) & !is.na(percentWonLost) & !is.na(percentSeatWonLost))

correlation_iv1 <- cor(pdataSpearman$seatsWonLost, pdataSpearman$ref_nselectorate, method = "spearman")
correlation_iv2 <- cor(pdataSpearman$percentWonLost, pdataSpearman$ref_nselectorate, method = "spearman")
```


### Export the results as a Word table

```
# Create a data frame for the diagnostic results

results_df <- data.frame(
  Test = c("Shapiro-Wilk Test (IV1)", "Shapiro-Wilk Test (IV2)", "Welch's t-test Seats", "Welch's t-test Votes",
           "Outliers Seats", "Outliers Votes", "Correlation Seats", "Correlation Votes"),
  Result = c(ifelse(shapiro_iv1$p.value < 0.05, "Not Normal", "Normal"),
             ifelse(shapiro_iv2$p.value < 0.05, "Not Normal", "Normal"),
             ifelse(t_test_resultH1$p.value < 0.05, "Significant Difference", "No Significant Difference"),
             ifelse(t_test_resultH2$p.value < 0.05, "Significant Difference", "No Significant Difference"),
             paste(outliers_iv1, collapse = ", "),
             paste(outliers_iv2, collapse = ", "),
             round(correlation_iv1, 2),
             round(correlation_iv2, 2))
)

# Create a flextable
# Install and load necessary packages
library(flextable)

flex_table <- flextable(results_df)

# Export the table to Word
library(officer)


# Create a Word document
doc <- read_docx()

# Add the table to the document
doc <- body_add_flextable(doc, flextable(results_df))


# Save the document
print(doc, target = "h1H2_diagnosticsTable.docx")
```



## Question 1 A party who obtained less seats in the legislature in the election preceding the leadership race is more likely to use a new selectorate
```
# Load package
library(polycor)

# Calculate biserial correlation
correlationH1_seats <- hetcor(as.numeric(pdata$ref_nselectorate), pdata$seatsWonLost)
correlationH1_seatsPercent <- hetcor(as.numeric(pdata$ref_nselectorate), pdata$percentSeatWonLost)


# Load library
library(psych)

# Calculate point-biserial correlation coefficient
pdataNa1 <-pdata %>%
  filter(!is.na(ref_nselectorateText) & !is.na(lostSeatsDummy))

point_biserialH1 <- cor(pdataNa1$ref_nselectorate, pdataNa1$lostSeatsDummy)

# Calculate sample size
n <- nrow(pdataNa1)

# Calculate standard error
point_biserialH1_se <- sqrt((1 - point_biserialH1^2) / (n - 2))
```


## Question 2 At an election preceding a leadership race, a party who obtained less votes than the previous election is more likely to have changed the selectorate
```
# Calculate biserial correlation
correlationH2Votes <- hetcor(as.numeric(pdata$ref_nselectorate), pdata$percentWonLost)

# Calculate point-biserial correlation coefficient
pdataNa2 <-pdata %>%
  filter(!is.na(ref_nselectorateText) & !is.na(lostVotesDummy))

point_biserialH2 <- cor(pdataNa2$ref_nselectorate, pdataNa2$lostVotesDummy)

# Calculate sample size
n <- nrow(pdataNa2)

# Calculate standard error
point_biserialH2_se <- sqrt((1 - point_biserialH2^2) / (n - 2))
```

## Question 3 At an election preceding a leadership race, a party who obtained a lower status within the legislature is more likely to have changed its leadership selectorate

```
# Calculate biserial correlation
correlationH3ParlStat <- hetcor(as.numeric(pdata$ref_nselectorate), pdata$parlStatusChange)


# Calculate point-biserial correlation coefficient
pdataNa3 <-pdata %>%
  filter(!is.na(ref_nselectorateText) & !is.na(lostParlStatDummy))

point_biserialH3 <- cor(pdataNa3$ref_nselectorate, pdataNa3$lostParlStatDummy)

# Calculate sample size
n <- nrow(pdataNa3)

# Calculate standard error
point_biserialH3_se <- sqrt((1 - point_biserialH3^2) / (n - 2))
```

# Questions 4 - 12
**Questions 4 and 5** have perfect separation. No test are conducted. They results are just discussed in the chapter.
**Questions 6 to 11** cannot be tested quantitatively with the data at our disposal. They are considered using interviews and party reform documents in Empirical Chapter 3. Whereas **Question 12** is also addressed using qualitative methods in the next chapter.

