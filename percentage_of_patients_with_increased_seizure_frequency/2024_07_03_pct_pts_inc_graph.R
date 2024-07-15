##Percentage of patients with increased seizure frequency. Data available by publication venue 
#Packages
library(tidyverse)
library(ggsci)
library(svglite)
library(patchwork)

#load in data
pct_increase_data <- read_csv("2024_07_03_percent_of_pts_with_increase.csv")


#First: Obtain an estimate of all the patients with an increase
#Accomplish this by summing up all the estimated patients in the FDA with increase in seizure frequency
#The FDA-approved labels will be used as the reference because the data is available for 77% of the 43 trials
#For missing values among the FDA labels I will use the median among those without missing values

#Calculate the median from FDA labels
#Remove: rufinamide, pregabalin, everolimus, lacosamide study 5, clobazam study 2, and zonisamide study 1 as the data is not present in FDA
median_w_increase_fda_known <- pct_increase_data %>%
  filter(publication_venue == 1) %>%
  filter(drug != "everolimus" & drug != "rufinamide" &
           drug != "pregabalin") %>%
  filter(!(drug == "clobazam" & study_number == 2) & !(drug == "zonisamide" & study_number == 1) &
           !(drug == "lacosamide" & study_number == 5)) %>%
  group_by(asm_placebo_percent_with_increase) %>%
  reframe(median_increase = median(percent_with_worsening),
          min_increase = min(percent_with_worsening),
          max_increase = max(percent_with_worsening)) %>%
  ungroup()

#create medians that can be used as estimates for trials with missing data
median_with_increase_placebo <- median_w_increase_fda_known %>%
  filter(asm_placebo_percent_with_increase == "placebo") %>%
  pull(as.numeric(median_increase)) 

median_with_increase_asm <- median_w_increase_fda_known %>%
  filter(asm_placebo_percent_with_increase == "asm") %>%
  pull(as.numeric(median_increase)) 


#Place estimates back into the data
pct_increase_data <- pct_increase_data %>%
  mutate(percent_with_worsening = case_when(
    publication_venue == 1 & drug == "everolimus" & asm_placebo_percent_with_increase == "placebo" ~ median_with_increase_placebo,
    publication_venue == 1 & drug == "everolimus" & asm_placebo_percent_with_increase == "asm" ~ median_with_increase_asm,
    publication_venue == 1 & drug == "lacosamide" & asm_placebo_percent_with_increase == "placebo" & study_number == 5 ~ median_with_increase_placebo,
    publication_venue == 1 & drug == "lacosamide" & asm_placebo_percent_with_increase == "asm" & study_number == 5 ~ median_with_increase_asm,
    publication_venue == 1 & drug == "clobazam" & asm_placebo_percent_with_increase == "asm" & study_number == 2 ~ median_with_increase_asm,
    publication_venue == 1 & drug == "zonisamide" & asm_placebo_percent_with_increase == "placebo" & study_number == 1 ~ median_with_increase_placebo,
    publication_venue == 1 & drug == "zonisamide" & asm_placebo_percent_with_increase == "asm" & study_number == 1 ~ median_with_increase_asm,
    publication_venue == 1 & drug == "pregabalin" & asm_placebo_percent_with_increase == "placebo" ~ median_with_increase_placebo,
    publication_venue == 1 & drug == "pregabalin" & asm_placebo_percent_with_increase == "asm" ~ median_with_increase_asm,
    publication_venue == 1 & drug == "rufinamide" & asm_placebo_percent_with_increase == "placebo" ~ median_with_increase_placebo,
    publication_venue == 1 & drug == "rufinamide" & asm_placebo_percent_with_increase == "asm" ~ median_with_increase_asm,
    TRUE ~ percent_with_worsening))%>%
  mutate(n_with_worsening = round(N*percent_with_worsening,0)) 

##total with increase 
sum_total <- pct_increase_data %>%
  group_by(publication_venue, asm_placebo_percent_with_increase) %>%
  reframe(num_patient_with_increase = sum(n_with_worsening),
          num_patient_total = sum(N))%>%
  ungroup()

#not accounted for in peer-review
peer_review_missing_data <- sum_total %>%
  group_by(asm_placebo_percent_with_increase) %>%
  reframe(missing_pts_w_increase_peer_review = abs(diff(num_patient_with_increase)),
          missing_pts_total_peer_review = abs(diff(num_patient_total)))%>% #total patients are not "missing". The data set only includes studies that had data for increased SF
  ungroup()


#create rows to represent all the patients without data within peer-reviewed journals
rows_to_add <- tibble(
  publication = c("remaining asm patients","remaining placebo patients"),
  publication_venue = c(2,2),
  study_number = c(1,1),
  drug = c("asm", "asm"),
  dose = c( "dose 1", "placebo"),
  dose_non_descript = c("dose 1", "placebo"),
  N = peer_review_missing_data$missing_pts_total_peer_review,
  percent_with_worsening = c(NA, NA),
  graph_worse = c(0,0),
  text_worse = c(0,0),
  table_worse = c(0,0),
  n_with_worsening = peer_review_missing_data$missing_pts_w_increase_peer_review,
  scaled_percentage_with_worsening = c(NA, NA),
  asm_placebo_percent_with_increase = c("asm", "placebo"),
  graph_worse_binary = c(0,0)
) %>%
  mutate(percent_with_worsening = round(as.numeric(n_with_worsening)/as.numeric(N), 2),
         scaled_percentage_with_worsening = as.numeric(percent_with_worsening*100))
#the reason the percent increase is elevated for the remaining patients is because even when they account for worsening they usually only account for > 25 or > 50% increase

#add the missing from peer review so the total numbers will be equivalent between venue
pct_increase_data <- pct_increase_data %>%
  rbind(rows_to_add)

#everolimus, lacosamide study 5 and clobazam study 2 are trials drug with information in the peer review that is not accounted for in the fda literature. so it will be the verified missing patients in the fda
#since the peer review has a graph that spans 0 i have to make an assumption based on the median number of increase before of how many people are below 0. then I subtract out the patients that are 25% and over which are shown in the peer review
everolimus_lacosamide_clobazam_peer_only <- pct_increase_data %>%
  filter((drug == "lacosamide" & study_number == 5)| 
           (drug == "everolimus" & study_number == 1) |
           (drug == "clobazam" & study_number == 2)) %>%
  group_by(publication_venue, asm_placebo_percent_with_increase) %>%
  reframe(number_patients_with_increase = sum(n_with_worsening)) %>% #Numbers in FDA rows is the estimated number. Numbers in peer-review give how many are accounted for in peer-review
  ungroup() %>%
  mutate(publication_venue = ifelse(
    publication_venue == 1, "fda", "peer_review"
  )) %>%
  pivot_wider(names_from = publication_venue,
              values_from = number_patients_with_increase) %>%
  mutate(missing_assumed = fda - peer_review, #not accounted for in peer-review
         missing_verified = peer_review) %>% #accounted for in peer-review
  pivot_longer(
    cols = starts_with("missing_"),
    names_to = "missing",
    values_to = "number_of_patients"
  ) %>%
  unite("treatment_and_data_capture", c(asm_placebo_percent_with_increase, missing), sep = "_")%>%
  select(treatment_and_data_capture, number_of_patients)%>%
  mutate(publication_venue = "fda") 

#The studies without information in either are all rufinamide and pregabalin, and zonisamide 1
no_data_in_either_venue <- pct_increase_data %>%
  filter(publication_venue == 1) %>%
  filter(drug == "rufinamide" | drug == "pregabalin" | 
           (drug == "zonisamide" & study_number == 1)) %>%
  group_by(asm_placebo_percent_with_increase) %>%
  reframe(number_of_patients = sum(n_with_worsening))%>%
  ungroup() %>%
  mutate(asm_placebo_percent_with_increase = ifelse(
    asm_placebo_percent_with_increase == "placebo","placebo_missing_assumed", "asm_missing_assumed"),
    treatment_and_data_capture = asm_placebo_percent_with_increase, 
    publication_venue = "fda")%>%
  select(-asm_placebo_percent_with_increase)%>%
  select(treatment_and_data_capture, publication_venue, number_of_patients)

###patients assumed and verified missing by FDA
FDA_missing_data <- no_data_in_either_venue %>%
  rbind(everolimus_lacosamide_clobazam_peer_only)%>%
  group_by(treatment_and_data_capture, publication_venue)%>%
  reframe(number_of_patients = sum(number_of_patients)) %>%
  ungroup()

###
FDA_present <- pct_increase_data %>%
  filter(publication_venue == 1) %>%
  filter(drug != "rufinamide" & drug != "pregabalin" & drug != "everolimus" &
           !(drug == "lacosamide" & study_number == 5) &
           !(drug == "clobazam" & study_number == 2) &
           !(drug == "zonisamide" & study_number == 1)) %>%
  group_by(asm_placebo_percent_with_increase) %>%
  reframe(number_of_patients = sum(n_with_worsening))%>%
  ungroup() %>%
  mutate(treatment_and_data_capture = asm_placebo_percent_with_increase,
         treatment_and_data_capture = 
           ifelse(treatment_and_data_capture == "placebo","placebo_data_present", "asm_data_present"),
         publication_venue = "fda")%>%
  select(-asm_placebo_percent_with_increase)

#combining missing and present data
FDA_missing_and_present_data <- FDA_missing_data %>%
  rbind(FDA_present) %>%
  mutate(
    asm_placebo = case_when(
      treatment_and_data_capture == "placebo_data_present" | 
        treatment_and_data_capture == "placebo_missing_assumed" |
        treatment_and_data_capture == "placebo_missing_verified" ~ "placebo",
      TRUE ~ "asm"
    ),
    missing_present = case_when(
      treatment_and_data_capture == "asm_missing_assumed" ~ "missing_assumed",
      treatment_and_data_capture == "asm_missing_verified" ~ "missing_verified",
      treatment_and_data_capture == "asm_data_present" ~ "data_present",
      treatment_and_data_capture == "placebo_missing_assumed" ~ "missing_assumed",
      treatment_and_data_capture == "placebo_missing_verified" ~ "missing_verified",
      TRUE ~ "data_present"
    ),
    missing_present = factor(missing_present, levels = c("missing_verified", "missing_assumed", "data_present"))
  )

missing_assumed <- FDA_missing_and_present_data %>%
  filter(missing_present == "missing_assumed") %>%
  reframe(number_of_patients = sum(number_of_patients)) %>%
  ungroup() %>%
  pull(as.numeric(number_of_patients))
  
missing_verified_fda <- FDA_missing_and_present_data %>%
  filter(missing_present == "missing_verified") %>%
  reframe(number_of_patients = sum(number_of_patients)) %>%
  ungroup() %>%
  pull(as.numeric(number_of_patients))

present_fda <- FDA_missing_and_present_data %>%
  filter(missing_present == "data_present") %>%
  reframe(number_of_patients = sum(number_of_patients)) %>%
  ungroup() %>%
  pull(as.numeric(number_of_patients))

#now separate out peer-reviewed
peer_review_graph_text_table <- pct_increase_data %>%
  filter(publication_venue == 2) %>%
  mutate(missing_present = ifelse(publication == "remaining asm patients" | publication == "remaining placebo patients", "missing_total","data_present"),
         graph_table_text = ifelse(graph_worse_binary == 1 | table_worse == 1 | table_worse == 2 | text_worse == 1| text_worse == 2, 1,0)) %>%
  group_by(missing_present, graph_worse_binary) %>%
  reframe(number_of_patients = sum(n_with_worsening)) %>%
  ungroup() 

peer_review_graph_text_table_total <- peer_review_graph_text_table %>%
  filter(missing_present == "data_present") %>%
  reframe(number_of_patients = sum(number_of_patients))%>%
  ungroup() %>%
  pull(as.numeric(number_of_patients))

peer_review_text_and_table_only <- as.numeric(peer_review_graph_text_table[1,3])
peer_review_graph_only <- as.numeric(peer_review_graph_text_table[2,3])
peer_reviewed_missing_assumed <- missing_assumed 
peer_review_missing_verified <- as.numeric(peer_review_graph_text_table[3,3]) - peer_reviewed_missing_assumed
  
peer_reviewed_tibble <- 
  tibble(publication_venue = rep("Peer-Reviewed",4),
         missing_present = c("data available as graph", 
                             "data available only as text or table",
                             "data only available in FDA-approved label", 
                             "data not available in either publication venue"),
         number_of_patients = c(peer_review_graph_only, 
                                peer_review_text_and_table_only,
                                peer_review_missing_verified, 
                                peer_reviewed_missing_assumed)
  )

fda_tibble <- 
  tibble(publication_venue = rep("FDA-approved prescribing label",3),
         missing_present = c("data available as graph",
                             "data only available in Peer-Reviewed Journal", 
                             "data not available in either publication venue"),
         number_of_patients = c(present_fda, 
                                missing_verified_fda, 
                                missing_assumed)
  )



fda_peer_tibble <- fda_tibble %>%
  rbind(peer_reviewed_tibble) %>%
  mutate(missing_present = factor(missing_present, levels = c("data only available in FDA-approved label",
                                                              "data only available in Peer-Reviewed Journal",
                                                              "data not available in either publication venue",
                                                              "data available only as text or table",
                                                              "data available as graph")),
         publication_venue = factor(publication_venue, levels = c("FDA-approved prescribing label",
                                                                  "Peer-Reviewed")))

jama_color_reordered <- c("#79AF97FF", "#B24745FF", "#DF8F44FF","#00A1D5FF", "#374E55FF","#6A6599FF")

ggplot(fda_peer_tibble, aes(x = publication_venue, y = number_of_patients, 
                                                                          fill = missing_present))+
  geom_col()+
  scale_fill_manual(values = jama_color_reordered) +
  labs(title = str_wrap("Number of patients with increased seizure frequency accounted for by publication venue",
                        width = 75),
       x = "Publication Venue",
       y = "Number of Patients",
       fill = "Data Location")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12))+
  theme_classic()+
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.caption = element_text(size = 10, hjust = 0),
    axis.text.x = element_text(size = 9)  # Rotate x-axis text for better readability
  )


#manipulate the table to show percentages 
percentage_graph <- fda_peer_tibble %>%
  filter(missing_present == "data available as graph" | missing_present == "data available only as text or table")

# Calculate total patients with increased seizure frequency
total_patients <- sum(fda_tibble %>%
                            pull(as.numeric(number_of_patients)))

percentage_graph <- percentage_graph %>%
  mutate(percentage_of_total_patients = round(number_of_patients/total_patients, 2)*100)


jama_color_reordered_2 <- c("#79AF97FF", "#374E55FF", "#DF8F44FF", "#6A6599FF","#00A1D5FF", "#B24745FF")

# Create the stacked bar graph
percentage_graph_plot <- ggplot(percentage_graph, aes(x = publication_venue, y = percentage_of_total_patients, fill = missing_present)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = jama_color_reordered_2) +
  labs(title = str_wrap("Percentage of data available for total patients with increased SF by publication venue",
                        width = 75),
       x = "Publication Venue",
       y = "Percentage of total Patients with increased SF",
       fill = "Data Display Type")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12))+
  theme_classic()+
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.caption = element_text(size = 10, hjust = 0),
    axis.text.x = element_text(size = 9) 
  )+
  scale_y_continuous(limits = c(0, 100), expand = c(0,0))


ggsave("2024_07_05_percentage_graph.png", plot = percentage_graph_plot, width = 8, height = 6, dpi = 1800)


# save as an SVG file
ggsave(filename = "2024_07_05_percentage_graph.svg", plot = percentage_graph_plot, width = 8, height = 6, unit = "in")