# beyondBallotCanadianPartyLeaders
r code for the doctoral dissertation "Beyond the Ballot: Reviewing Canadian Political Parties' Leadership Selection Rules" completed at Université Laval and Université libre de Bruxelles (Cevipol)

## Code
```R
##### set path
setwd("~/_rCodesData/chapter2AllLdrRaces")


##### load packages

#For most of the environment
library(tidyverse)
require(GGally)
require(reshape2)


# import data
library(readxl)

# Importing the Excel file
data <- read_excel("_bdChefferie_2023-12-11.xlsx")


## Convert & clean some variables for models later

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



# pdata a new variable that identifies changes nSelectorate
pdata <- pdata %>%
  arrange(yearLdrVote) %>%
  group_by(jurisdiction, party) %>%
  mutate(ref_nselectorate = ifelse(c(0, diff(nSelectorate)) != 0, 1, 0)) %>%
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


# for exporting tables in Word format for whe the table is a little too complex to copy/paste from console
#library(flextable) 


## Export the codebook in word
#library(codebookr)
#library(dplyr, warn.conflicts = FALSE)
#library(haven)

# pass the df through the codebook function
#ldrRaceCanadaCodeBook <- codebook(pdata)

#export in Word
#print(codebook(pdata), "ldrRaceCanadaCodeBook.docx")



# Define custom colors for each party family
family_colors <- c("conservative" = "blue", "liberal" = "red", "new democratic" = "darkorange", "green" = "darkgreen", "sovereigntist" = "darkblue")  # Add more colors as needed

pdf("/Users/audreybrennan/Library/CloudStorage/Dropbox/PhD-Laval-ULB/_rCodesData/chapter2AllLdrRaces/figuresChapter2/allLdrRacesDataCh2.pdf", 16, 12)

# Plot the observations of my data
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

dev.off()


 ######           ANOMALOUS OUTCOMES

# plotting distributions before regressions
pdf("/Users/audreybrennan/Library/CloudStorage/Dropbox/PhD-Laval-ULB/_rCodesData/chapter2AllLdrRaces/figuresChapter2/boxplotSeatQty.pdf", 10, 8)
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
 dev.off()     

 
   pdf("/Users/audreybrennan/Library/CloudStorage/Dropbox/PhD-Laval-ULB/_rCodesData/chapter2AllLdrRaces/figuresChapter2/boxplotSeatPercent.pdf", 10, 8)
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
dev.off()

pdf("/Users/audreybrennan/Library/CloudStorage/Dropbox/PhD-Laval-ULB/_rCodesData/chapter2AllLdrRaces/figuresChapter2/votesshareWonLost.pdf", 10, 8)

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
dev.off()

pdf("/Users/audreybrennan/Library/CloudStorage/Dropbox/PhD-Laval-ULB/_rCodesData/chapter2AllLdrRaces/figuresChapter2/parlStatChange.pdf", 10, 8)

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
dev.off()


pdf("/Users/audreybrennan/Library/CloudStorage/Dropbox/PhD-Laval-ULB/_rCodesData/chapter2AllLdrRaces/figuresChapter2/parlStatMomentRace.pdf", 10, 8)

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

dev.off()






### Assumption tests
library(nortest)
library(outliers)


# 3. Normality Testing
shapiro_iv1 <- shapiro.test(pdata$seatsWonLost)
shapiro_iv2 <-shapiro.test(pdata$percentWonLost)

# 4. Homogeneity of Variance
# Assuming 'data' is your dataset with continuous IV 'IV1' and binary DV 'DV'
t_test_resultH1 <- t.test(seatsWonLost ~ ref_nselectorate, data = pdata, var.equal = FALSE)
#Welch Two Sample t-test

#data:  seatsWonLost by ref_nselectorate
#t = 0.92257, df = 13.026, p-value = 0.373
#alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
#95 percent confidence interval:
#  -6.62387 16.50134
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




# 5. Outlier Detection
outliers_iv1 <-   boxplot.stats(pdata$seatsWonLost)$out
outliers_iv2 <- boxplot.stats(pdata$percentWonLost)$out

# 6. Correlation Analysis
pdataSpearman <- pdata%>%
  filter(!is.na(ref_nselectorate) & !is.na(seatsWonLost) & !is.na(percentWonLost) & !is.na(percentSeatWonLost))

correlation_iv1 <- cor(pdataSpearman$seatsWonLost, pdataSpearman$ref_nselectorate, method = "spearman")
correlation_iv2 <- cor(pdataSpearman$percentWonLost, pdataSpearman$ref_nselectorate, method = "spearman")




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




###### Question 1 A party who obtained less seats in the legislature in the election preceding the leadership race is more likely to use a new selectorate

# Load library
library(polycor)

# Calculate biserial correlation
correlationH1_seats <- hetcor(as.numeric(pdata$ref_nselectorate), pdata$seatsWonLost)
correlationH1_seatsPercent <- hetcor(as.numeric(pdata$ref_nselectorate), pdata$percentSeatWonLost)


# Extract the biserial correlation coefficient
biserial_corH1_seatsPercent <- correlationH1_seatsPercent$correlations[1, 2]

# Print the correlation coefficient
print(biserial_corH1_seatsPercent)


# Load library
library(psych)

# Calculate point-biserial correlation coefficient
pdataNa <-pdata %>%
  filter(!is.na(ref_nselectorateText) & !is.na(lostSeatsDummy))

point_biserialH1 <- cor(pdataNa$ref_nselectorate, pdataNa$lostSeatsDummy)

print(point_biserial)



###### Question 2 At an election preceding a leadership race, a party who obtained less votes than the previous election is more likely to have changed the selectorate

# Calculate biserial correlation
correlationH2 <- hetcor(as.numeric(pdata$ref_nselectorate), pdata$percentWonLost)

# Extract the biserial correlation coefficient
biserial_corH2 <- correlationH2$correlations[1, 2]

# Print the correlation coefficient
print(biserial_corH2)


# Calculate point-biserial correlation coefficient
pdataNa <-pdata %>%
  filter(!is.na(ref_nselectorateText) & !is.na(lostVotesDummy))

point_biserialH2 <- cor(pdataNa$ref_nselectorate, pdataNa$lostVotesDummy)

print(point_biserialH2)



###### Question 3 At an election preceding a leadership race, a party who obtained a lower status within the legislature is more likely to have changed its leadership selectorate

# Calculate biserial correlation
correlationH3 <- hetcor(as.numeric(pdata$ref_nselectorate), pdata$parlStatusChange)

# Extract the biserial correlation coefficient
biserial_corH3 <- correlationH3$correlations[1, 2]

# Print the correlation coefficient
print(biserial_corH3)


# Calculate point-biserial correlation coefficient
pdataNa <-pdata %>%
  filter(!is.na(ref_nselectorateText) & !is.na(lostParlStatDummy))

point_biserialH3 <- cor(pdataNa$ref_nselectorate, pdataNa$lostParlStatDummy)

print(point_biserialH3)


####### issues with data Importing Cospoal to Run the hypotheses and compare the reseults for Canada provincial to COSPAL for the same period

cospalData <- read_csv("COSPAL_final_april_2019.csv")

#create the sister predictors
cospalNA <- cospalData %>%
  filter(`changes to selectorate` <= 1) %>% # get only 0s and 1s
  filter( `leadership race?` == 1) %>% #get only the leadership races
  filter(Year >= 2000) #get the post 2000 period to compare to the canadian provinces


#arrange by party name, and then by year (for next step)
cospalNA <- cospalNA %>% arrange( `party code`, Year)



# pdata a new variable that identifies changes nSelectorate
cospalNA <- cospalNA %>%
  arrange(Year) %>%
  group_by(`party code`) %>%
  mutate(ref_nselectorate = ifelse(c(0, diff(selectorate)) != 0, 1, 0)) %>%
  ungroup()


#rename cols of reform to have clearer tables in the console
cospalNA$ref_nselectorateText <- NA
cospalNA$ref_nselectorateText[cospalNA$ref_nselectorate == 1] <- "Reform"
cospalNA$ref_nselectorateText[cospalNA$ref_nselectorate == 0] <- "No Reform"


#rename cols of families for graph
cospalNA$partyfamilies2Text<- NA
cospalNA$partyfamilies2Text[cospalNA$partyfamilies2 == 1] <- "Liberals"
cospalNA$partyfamilies2Text[cospalNA$partyfamilies2 == 2]	<- "Conservatives" 
cospalNA$partyfamilies2Text[cospalNA$partyfamilies2 == 4] <-  "Social democrats"
cospalNA$partyfamilies2Text[cospalNA$partyfamilies2 == 5] <- "Greens"
cospalNA$partyfamilies2Text[cospalNA$partyfamilies2 == 6] <- "Regionalists/secessionists/Ethnic minorities"
cospalNA$partyfamilies2Text[cospalNA$partyfamilies2 == 7] <- "Radical right"
cospalNA$partyfamilies2Text[cospalNA$partyfamilies2 == 8] <- "Radical left"

# Define colors for each group
colors <- c("Conservatives" = "blue",
            "Greens" = "darkgreen",
            "Liberals" = "red",
            "Radical left" = "pink",
            "Radical right" = "lightblue",
            "Regionalists/secessionists/Ethnic minorities" = "darkblue",
            "Social democrats" = "darkorange")

# Plot the observations of my data
cospalNA %>%
  arrange(Country) %>%
  mutate(Country = factor(Country, levels = rev(unique(Country)))) %>%
  ggplot(aes(x = Country, fill = as.factor(partyfamilies2Text))) +
  geom_bar(position = position_dodge(preserve = "single"), color = "white") +  # Modify position to ensure consistent bar widths
  labs(#title = "Leadership Races per Jurisdiction by Party Family",
    x = " ",
    y = " ") +
  scale_fill_manual(name = "Party Family", values = colors) +  # Use custom colors
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +  # Set breaks every 5 units
  theme_minimal() +
  theme(axis.text.x = element_text(size = 16, color = "black"),  # Rotate x-axis labels for better readability
        axis.text.y = element_text(size = 25, color = "black"),
        legend.position = "top",
        legend.text = element_text(size = 20),  # Adjust size of legend text
        legend.title = element_text(size = 25),  # Adjust size of legend title
        strip.text = element_text(size = 30),   # Adjust size of facet labels
        strip.background = element_blank()) +   # Remove strip background
  facet_wrap(~ref_nselectorateText)  +
  coord_flip()

# remove more vars, get only same as prov data

cospalNA <- cospalNA %>%
  select(IDNR, 
         Country : Year, 
         selectorate, `changes to selectorate`, 
         `party in government?` : `party's rank in last elections (seats)`,
         influencedByOthersJurisdiction : partyfamilies2Text) 



cospalNA %>%
  filter(!is.na(ref_nselectorateText)) %>%
  ggplot(aes(x = ref_nselectorate, y =`seat share change`, fill = as.factor(ref_nselectorateText))) +
  geom_boxplot() +
  geom_jitter (alpha = .3, size = 1) +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal() +
  scale_x_discrete(labels = NULL) +
  scale_fill_manual(name = "Reformed Selectorate?",
                    values = c("white", "grey")) +
  scale_y_continuous(breaks = seq(-500, 500, by = 5)) +  # Set breaks every 5 units
  theme(axis.text.y = element_text(size = 14),  # Set size of y-axis text
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.title.x = element_blank(),         # Remove x-axis title
        axis.ticks.y = element_line(size = 1.5),  # Increase size of y-axis ticks
        legend.position = "top",                # Move legend to top
        legend.text = element_text(size = 20))  # Set size of legend text



cospalNA %>%
  filter(!is.na(ref_nselectorateText)) %>%
  ggplot(aes(x = ref_nselectorate, y =`seat share change`, fill = as.factor(ref_nselectorateText))) +
  geom_boxplot() +
  geom_jitter (alpha = .3, size = 1) +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal() +
  scale_x_discrete(labels = NULL) +
  scale_fill_manual(name = "Reformed Selectorate?",
                    values = c("white", "grey")) +
  scale_y_continuous(breaks = seq(-500, 500, by = 5)) +  # Set breaks every 5 units
  theme(axis.text.y = element_text(size = 14),  # Set size of y-axis text
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.title.x = element_blank(),         # Remove x-axis title
        axis.ticks.y = element_line(size = 1.5),  # Increase size of y-axis ticks
        legend.position = "top",                # Move legend to top
        legend.text = element_text(size = 20))  # Set size of legend text


cospalNA %>%
  filter(!is.na(ref_nselectorateText)) %>%
  filter(`vote share change` <100) %>%
  ggplot(aes(x = ref_nselectorate, y =`vote share change`, fill = as.factor(ref_nselectorateText))) +
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

