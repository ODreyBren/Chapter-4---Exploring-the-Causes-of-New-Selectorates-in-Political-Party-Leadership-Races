# Purpose
r code for Chapter 4 of the doctoral dissertation "Beyond the Ballot: Reviewing Canadian Political Parties' Leadership Selection Rules" completed at Université Laval and Université libre de Bruxelles (Cevipol). This chapter quantitatively tests for the likelihood of Anomalous outcomes and Act Contingent events to precede a selectorate reform.

## Notes on data
The leadership race inventory was conducted in the fall of 2023. The below code applies a series of transformations to the the various variables. The final dataset output of those transformations is the data used in Empirical Chapter 2 of the dissertation. For the original dataset or the transformed dataset, please contact the author.


# Research protocol
See Table 17 in Chapter 4.

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


# Declare panel data structure using plm
pdata <- pdata.frame(data, index = c("nparty", "id"))


# reorder the family categorical for more intuitive numbering
# Reorder the levels of the factor variable
pdata$family <- factor(pdata$family, levels = c("conservative", "liberal", "new democratic", "green", "sovereigntist"))

# Rename the levels to have capitalized names (Nicer Figures)
levels(pdata$family) <- c("Conservative", "Liberal", "New Democratic", "Green", "Secessionist")

pdata$jurisdiction <- factor(pdata$jurisdiction, levels = c("canada", "bc", "ab", "sk", "mb", "on", "qc", "nb", "ns", "pei","nl"))

# Rename the levels to have capitalized names (Nicer Figures)
levels(pdata$jurisdiction) <- c("Canada", "BC", "AB", "SK", "MB", "ON", "QC", "NB", "NS", "PEI", "NL")





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



##  A party who obtained less seats in the legislature in the election preceding the leadership race is more likely to use a new selectorate
```
# Load package
library(polycor)

# Calculate biserial correlations
#SeatsQTY
correlationH1_seats <- hetcor(as.numeric(pdata$ref_nselectorate), pdata$seatsWonLost)

#get the standard errors (for ggplot)
correlationH1_seats_se <- as.numeric(correlationH1_seats$std.errors[2])

#seats percent
correlationH1_seatsPercent <- hetcor(as.numeric(pdata$ref_nselectorate), pdata$percentSeatWonLost)

#get the standard errors (for ggplot)
correlationH1_seatsPercent_se <- as.numeric(correlationH1_seatsPercent$std.errors[2])


# Load package
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


## At an election preceding a leadership race, a party who obtained less votes than the previous election is more likely to have changed the selectorate
```
# Calculate biserial correlation
correlationH2Votes <- hetcor(as.numeric(pdata$ref_nselectorate), pdata$percentWonLost)

#get the standard errors (for ggplot)
correlationH2Votes_se <- as.numeric(correlationH2Votes$std.errors[2])


# Calculate point-biserial correlation coefficient
pdataNa2 <-pdata %>%
  filter(!is.na(ref_nselectorateText) & !is.na(lostVotesDummy))

point_biserialH2 <- cor(pdataNa2$ref_nselectorate, pdataNa2$lostVotesDummy)

# Calculate sample size
n <- nrow(pdataNa2)

# Calculate standard error
point_biserialH2_se <- sqrt((1 - point_biserialH2^2) / (n - 2))
```

## At an election preceding a leadership race, a party who obtained a lower status within the legislature is more likely to have changed its leadership selectorate

```
# Calculate biserial correlation
correlationH3ParlStat <- hetcor(as.numeric(pdata$ref_nselectorate), pdata$parlStatusChange)

#get the standard errors (for ggplot)
correlationH3ParlStat_se <- as.numeric(correlationH3ParlStat$std.errors[2])

# Calculate point-biserial correlation coefficient
pdataNa3 <-pdata %>%
  filter(!is.na(ref_nselectorateText) & !is.na(lostParlStatDummy))

point_biserialH3 <- cor(pdataNa3$ref_nselectorate, pdataNa3$lostParlStatDummy)

# Calculate sample size
n <- nrow(pdataNa3)

# Calculate standard error
point_biserialH3_se <- sqrt((1 - point_biserialH3^2) / (n - 2))
```

## Create a coefficients plot

Filename `coefficientPlot.pdf`
```
# Define coefficients and their standard errors
coefficients <- c(correlationH1_seats$correlations[1, 2], correlationH1_seatsPercent$correlations[1, 2], point_biserialH1, correlationH2Votes$correlations[1, 2], point_biserialH2, correlationH3ParlStat$correlations[1, 2], point_biserialH3)
se <- c(correlationH1_seats_se, correlationH1_seatsPercent_se, point_biserialH1_se, correlationH2Votes_se, point_biserialH2_se, correlationH3ParlStat_se, point_biserialH3_se)

# Define questions
questions <- c("Seats qty", "Seats Percent", "Lost Seats", "Vote share change", "Lost votes", "Change Parl. Stat.", "Lost Parl. Stat.")



# Create a data frame for plotting
plot_data <- data.frame(
  Question = rep(questions),
  Coefficient = coefficients,
  SE = rep(se)
)


## Create ggplot

          # Create the coefficient plot with error bars
          ggplot(plot_data, aes(x = Question, y = Coefficient)) +
            geom_point() +
            geom_pointrange(aes(ymin = Coefficient - SE, ymax = Coefficient + SE)) +
            labs(title = " ",
                 x = " ",
                 y = " ") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),  # Add bold parameter
                  axis.text.y = element_text(size = 14, face = "bold"),                         # Add bold parameter
                  axis.title = element_text(size = 14, face = "bold"))                          # Add bold parameter

```

# Other dimensions in the protocol
For the full overview of the protocol see Table 17 in Chapter 4. Furthermore, for the analysis of other elements in Shugart's framework (Chapter 5) consult this GitHub repository: https://github.com/ODreyBren/Chapter-5---The-Process-of-Leadership-Selectorate-Change-in-Canadian-Federal-and-Provincial-Parties

# Code for Appendix analyses (GLM and Firth GLM)
```
# Load necessary libraries

library(stats)
library(officer)
library(flextable)
library(stargazer)
library(glm)
library(brglm2)  

# Define the models basic logistic
base_glm_influencedByOthersFamily <- glm(ref_nselectorate ~ influencedByOthersFamily, data = pdata, family = binomial("logit"))
base_glm_influencedByOthersJurisdiction <- glm(ref_nselectorate ~ influencedByOthersJurisdiction, data = pdata, family = binomial("logit"))
base_glm_lostParlStatDummy <- glm(ref_nselectorate ~ lostParlStatDummy, data = pdata, family = binomial("logit"))
base_glm_parlStatusChange <- glm(ref_nselectorate ~ parlStatusChange, data = pdata, family = binomial("logit"))
base_glm_percentWonLost <- glm(ref_nselectorate ~ percentWonLost, data = pdata, family = binomial("logit"))
base_glm_votesharePreviousElect <- glm(ref_nselectorate ~ votesharePreviousElect, data = pdata, family = binomial("logit"))
base_glm_seatsWonLost <- glm(ref_nselectorate ~ seatsWonLost, data = pdata, family = binomial("logit"))
base_glm_lostSeatsDummy <- glm(ref_nselectorate ~ lostSeatsDummy, data = pdata, family = binomial("logit"))


# Define the models
firth_glm_influencedByOthersFamily <- brglm(ref_nselectorate ~ influencedByOthersFamily, data = pdata, family = binomial("logit"))
firth_glm_influencedByOthersJurisdiction <- brglm(ref_nselectorate ~ influencedByOthersJurisdiction, data = pdata, family = binomial("logit"))
firth_glm_lostParlStatDummy <- brglm(ref_nselectorate ~ lostParlStatDummy, data = pdata, family = binomial("logit"))
firth_glm_parlStatusChange <- brglm(ref_nselectorate ~ parlStatusChange, data = pdata, family = binomial("logit"))
firth_glm_percentWonLost <- brglm(ref_nselectorate ~ percentWonLost, data = pdata, family = binomial("logit"))
firth_glm_votesharePreviousElect <- brglm(ref_nselectorate ~ votesharePreviousElect, data = pdata, family = binomial("logit"))
firth_glm_seatsWonLost <- brglm(ref_nselectorate ~ seatsWonLost, data = pdata, family = binomial("logit"))
firth_glm_lostSeatsDummy <- brglm(ref_nselectorate ~ lostSeatsDummy, data = pdata, family = binomial("logit"))



# Combine the models into lists
base_glm_models <- list(
  base_glm_influencedByOthersFamily,
  base_glm_influencedByOthersJurisdiction,
  base_glm_lostParlStatDummy,
  base_glm_parlStatusChange,
  base_glm_percentWonLost,
  base_glm_votesharePreviousElect,
  base_glm_seatsWonLost,
  base_glm_lostSeatsDummy
)

firth_glm_models <- list(
  firth_glm_influencedByOthersFamily,
  firth_glm_influencedByOthersJurisdiction,
  firth_glm_lostParlStatDummy,
  firth_glm_parlStatusChange,
  firth_glm_percentWonLost,
  firth_glm_votesharePreviousElect,
  firth_glm_seatsWonLost,
  firth_glm_lostSeatsDummy
)



# Base GLM
stargazer(models_base, type = "text", title = "Base GLM Results", out = "base_glm.txt")

# Firth GLM
stargazer(models_firth, type = "text", title = "Firth GLM Results", out = "firth_glm.txt")
```


# Create a combined coefficient plot with side-by-side points
Filename `coefficientPlot_APPENDIX.pdf`
Note: The "influenced by" variables removed for parsimonious plot.
```
library(ggplot2)
library(broom)
library(dplyr)

# Extract coefficients for Base GLM models
base_glm_models <- list(
  "Influenced By Others Family" = base_glm_influencedByOthersFamily,
  "Influenced By Others Jurisdiction" = base_glm_influencedByOthersJurisdiction,
  "Lost Parl Status Dummy" = base_glm_lostParlStatDummy,
  "Parl Status Change" = base_glm_parlStatusChange,
  "Percent Won/Lost" = base_glm_percentWonLost,
  "Vote Share Previous Election" = base_glm_votesharePreviousElect,
  "Seats Won/Lost" = base_glm_seatsWonLost,
  "Lost Seats Dummy" = base_glm_lostSeatsDummy
)

base_glm_coefs <- lapply(base_glm_models, tidy) %>%
  bind_rows(.id = "Model")

# Extract coefficients for Firth GLM models
firth_glm_models <- list(
  "Influenced By Others Family" = firth_glm_influencedByOthersFamily,
  "Influenced By Others Jurisdiction" = firth_glm_influencedByOthersJurisdiction,
  "Lost Parl Status Dummy" = firth_glm_lostParlStatDummy,
  "Parl Status Change" = firth_glm_parlStatusChange,
  "Percent Won/Lost" = firth_glm_percentWonLost,
  "Vote Share Previous Election" = firth_glm_votesharePreviousElect,
  "Seats Won/Lost" = firth_glm_seatsWonLost,
  "Lost Seats Dummy" = firth_glm_lostSeatsDummy
)

firth_glm_coefs <- lapply(firth_glm_models, tidy) %>%
  bind_rows(.id = "Model")

# Combine Base and Firth GLM coefficients and filter out the two variables
combined_coefs <- bind_rows(
  mutate(base_glm_coefs, Method = "Base GLM"),
  mutate(firth_glm_coefs, Method = "Firth GLM")
) %>%
  filter(term != "(Intercept)") %>%  # Exclude intercept
  filter(!Model %in% c("Influenced By Others Family", "Influenced By Others Jurisdiction"))  # Exclude specified models

# Create a combined coefficient plot 1
ggplot(combined_coefs, aes(x = estimate, y = term, color = Method, shape = Method)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.2) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12)
  ) +
  scale_color_manual(values = c("Base GLM" = "darkgrey", "Firth GLM" = "#000000")) +  # Beige and Black
  scale_shape_manual(values = c("Base GLM" = 16, "Firth GLM" = 17)) +
  labs(
    title = "Coefficient Estimates from Base GLM and Firth GLM Models",
    x = "Coefficient Estimate",
    y = "Predictor"
  ) +
  coord_flip()


# Create a combined coefficient plot 2
# Rename terms manually
combined_coefs$term <- recode(
  combined_coefs$term,
  "influencedByOthersFamily" = "Influenced By Others (Family)",
  "influencedByOthersJurisdiction" = "Influenced By Others (Jurisdiction)",
  "lostParlStatDummy" = "Lost Parl Status Dummy",
  "parlStatusChange" = "Parliament Status Change",
  "percentWonLost" = "Percent Won/Lost",
  "votesharePreviousElect" = "Vote Share Previous Election",
  "seatsWonLost" = "Seats Won/Lost",
  "lostSeatsDummy" = "Lost Seats Dummy"
)


# Reorder terms manually
combined_coefs$term <- factor(combined_coefs$term, levels = c(
  "Parliament Status Change",
  "Lost Parl Status Dummy",
  "Lost Seats Dummy",
  "Vote Share Previous Election",
  "Percent Won/Lost",
  "Seats Won/Lost"
))

     
ggplot(combined_coefs, aes(x = estimate, y = term, color = Method, shape = Method)) +
        geom_point(size = 3, position = position_dodge(width = 0.5)) +  # Adjust position
        geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.2, position = position_dodge(width = 0.5)) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 8),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12)
        ) +
        scale_color_manual(values = c("Base GLM" = "darkgrey", "Firth GLM" = "#000000")) +  # Beige and Black
        scale_shape_manual(values = c("Base GLM" = 16, "Firth GLM" = 17)) +
        labs(
          title = "",
          x = "Coefficient Estimate",
          y = "Predictor"
        ) +
        coord_flip()
```


