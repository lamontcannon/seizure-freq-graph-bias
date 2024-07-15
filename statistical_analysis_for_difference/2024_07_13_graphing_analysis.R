# Load necessary libraries
library(dplyr)


# Load the data
## Due to inconsistencies within individual FDA-approved labels additional nuanced data handling decisions were required. 
#FDA-approved labels often combine multiple trials into a single graph while other labels may contain graphs for some trials but not others. 
#These practices did not affect the basic descriptive statistics assessing whether a publication graphically captured the number of patients with an increase in seizure frequency (SF) in each trial. 
#However, they impacted our ability to determine if a systematic difference existed in the likelihood of seeing a graph of increased SF. For analysis purposes, we treated FDA-approved labels, not the individual trials within them, as independent observations. 
#This necessitated reconciling how to count labels with inconsistencies. We applied an "all or nothing" criterion for the prescribing labels. 
#For instance, if a label had graphs showing the percentage of patients with increased SF in two trials but lacked a graph for a third trial, it would not be counted as graphically depicting the percentage of patients with an increase.
#This approach more heavily penalized the FDA-approved labels, as each peer-reviewed trial publication was counted as an individual observation. 
#Therefore, if two out of three peer-reviewed trial publications showed a graph, but the third did not, the peer-reviewed publication category still received credit for the two with graphs. 
#These methodological choices set more rigorous standards for detecting a bias rather than artifacts of data aggregation practices. 
#The strict all or nothing criterion resulted in the FDA-approved labels of clobazam, lacosamide and zonisamide being counted as not showing graphs because clobazam only showed increased SF for 1 of 2 trials, lacosamide 3 of 4 and zonisamide 2 of 3.  
#Therefore our result likely underestimates the absolute difference between the two publication venues.
#data_harsh treats clobazam, locasimde and zonisamide labels as not having graphs
#data_generous is treating clobazam, lacosamide and zonisamide labels as having graphs
data_harsh <- read.csv("2024_06_21_harsh_criteria.csv")


# Prepare the data for analysis
data_harsh <- data_harsh %>%
  mutate(publication_venue = as.factor(ifelse(publication_venue == 1, "FDA-approved Label","Peer-reviewed" )))

# build contingency table
data_harsh_contingency_table <- data_harsh %>%
  group_by(publication_venue)%>%
  reframe(present = sum(graph_increase_frequency == 1),
          total = sum(graph_increase_frequency == 0 | graph_increase_frequency == 1))%>%
  ungroup()

#binomial test for confidence intervals specifically around fda labels proportion of graphs
harsh_fda_binom_test <- data_harsh_contingency_table %>%
  filter(publication_venue == "FDA-approved Label") 
  
harsh_fda_binom_test <- binom.test(harsh_fda_binom_test$present, harsh_fda_binom_test$total)

harsh_fda_binom_test

#binomial test for confidence intervals specifically around peer-reviewed journal proportion of graphs
harsh_peer_review_binom_test <- data_harsh_contingency_table %>%
  filter(publication_venue == "Peer-reviewed") 

harsh_peer_review_binom_test <- binom.test(harsh_peer_review_binom_test$present, harsh_peer_review_binom_test$total)

harsh_peer_review_binom_test

#prop test for confidence interval of the difference
harsh_diff_prop_test <-prop.test(data_harsh_contingency_table$present, data_harsh_contingency_table$total)

harsh_diff_prop_test


#Repeat analysis above but treating clobazam, locosamide and zonisamide as having graphs
data_generous <- read.csv("2024_06_21_generous_criteria.csv")
data_generous <- data_generous %>%
  mutate(publication_venue = as.factor(ifelse(publication_venue == 1, "FDA-approved Label","Peer-reviewed" )))


# build contingency table
data_generous_contingency_table <- data_generous  %>%
  group_by(publication_venue)%>%
  reframe(present = sum(graph_increase_frequency == 1),
          total = sum(graph_increase_frequency == 0 | graph_increase_frequency == 1))%>%
  ungroup()

#binomial test for confidence intervals specifically around fda labels proportion of graphs
generous_fda_binom_test <- data_generous_contingency_table %>%
  filter(publication_venue == "FDA-approved Label") 

generous_fda_binom_test <- binom.test(generous_fda_binom_test$present, generous_fda_binom_test$total)

generous_fda_binom_test

#binomial test for confidence intervals specifically around peer-reviewed journal proportion of graphs
generous_peer_review_binom_test <-  data_generous_contingency_table %>%
  filter(publication_venue == "Peer-reviewed") 

generous_peer_review_binom_test <- binom.test(generous_peer_review_binom_test$present, generous_peer_review_binom_test$total)

generous_peer_review_binom_test

#prop test for confidence interval of the difference
generous_diff_prop_test <-prop.test(data_generous_contingency_table$present, data_generous_contingency_table$total)

generous_diff_prop_test

