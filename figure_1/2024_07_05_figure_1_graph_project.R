###packages##
library(tidyverse)
library(patchwork)
library(ggsci)
library(svglite)

figure_1_data <- read_csv("2024_07_05_bar_plot_example.csv")


figure_1_data_medians <- figure_1_data %>%
  group_by(group)%>%
  reframe(distribution_1_median = median(percent_reduction_distribution_1),
          distribution_2_median = median(percent_reduction_distribution_2),
          distribution_3_median = median(percent_reduction_distribution_3)
          )%>%
  ungroup()

jama_color_reordered_2 <- c("#79AF97FF", "#374E55FF", "#DF8F44FF", "#6A6599FF","#00A1D5FF", "#B24745FF")

theme_custom <- theme_classic() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.caption = element_text(size = 10, hjust = 0),
    axis.text.x = element_text(size = 9)  # Rotate x-axis text for better readability
  )

bar_plot_1 <- ggplot(figure_1_data_medians, aes(x = group, y = distribution_1_median, fill = group))+
  geom_col()+
  labs(
      title = "A: bar chart",
       y = "Median Percent Reduction from Baseline",
       x = "Group")+
  scale_fill_jama()+
  scale_color_jama()+
  theme_custom+
  scale_y_continuous(limits = c(0, 60), expand = c(0, 0)) +
  theme(legend.position = "none")


d1 <- ggplot(figure_1_data, aes(x= group, y = percent_reduction_distribution_1, color = group))+
  #geom_boxplot()+
  geom_jitter(width = .2, alpha = 0.5)+
  labs(
    title = "B: unimodal",
    x= "Group",
    y = "Percent Reduction from Baseline"
  )+
  scale_fill_jama()+
  scale_color_jama()+
  theme_custom+
  ylim(-220,105)+
  theme(legend.position = "none")

d2 <- ggplot(figure_1_data, aes(x= group, y = percent_reduction_distribution_2, color = group))+
  #geom_boxplot()+
  geom_jitter(width = .2, alpha = 0.5)+
  labs(
    title = "C: bimodal",
    x= "Group",
    y = "Percent Reduction from Baseline"
  )+
  scale_fill_jama()+
  scale_color_jama()+
  theme_custom+
  ylim(-220,105)+
  theme(legend.position = "none")

d3 <- ggplot(figure_1_data, aes(x= group, y = percent_reduction_distribution_3, color = group))+
  #geom_boxplot()+
  geom_jitter(width = .2, alpha = 0.5)+
  labs(
    title = "D: clustered\naround the median",
    x= "Group",
    y = "Percent Reduction from Baseline"
  )+
  scale_fill_jama()+
  scale_color_jama()+
  theme_custom+
  ylim(-220,105)+
  theme(legend.position = "none")

# Combine the plots using patchwork
combined_plot <- bar_plot_1 | d1 | d2 | d3

# Display the combined plot
combined_plot

ggsave("2024_07_05_different_distributions.png", plot = combined_plot, width = 9, height = 6, dpi = 1800)


# save as an SVG file
ggsave(filename = "2024_07_05_different_distributions.svg", plot = combined_plot, width = 9, height = 6, unit = "in")
