library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(visdat) 
library(plotly)


#---------------------------------------------------------------------------
# Read in the data
data <- read_csv("D:/Study Material/assignment data/Data Visualization/SDG_dataset/sdg_index_2000-2022.csv")
#---------------------------------------------------------------------------
#Visualize missing values in the data
vis_miss(data)
#---------------------------------------------------------------------------
#Write the data to a CSV file named data.csv
write_csv(data, "data.csv")
#---------------------------------------------------------------------------



#---------------------------------------------------------------------------
# Creating Visualization 1
#---------------------------------------------------------------------------

# Creating a ggplot2 plot (scatter plot)
visualisation_1 <- ggplot(data, aes(x = goal_4_score, y = sdg_index_score, color = sdg_index_score)) +
  geom_point(size = 1, alpha = 0.9) +
  geom_smooth(color = "#c5192d", size = 1.2, linetype = "solid") +
  labs(
    title = "SDG index score vs. Quality Education Score (Goal 4)",
    x = "Quality Education Score (Goal 4)",
    y = "SDG index score"
  ) + 
  scale_color_gradient(
    low = "#24e3ff", high = "#0a58b3",
    name = "SDG Index Score",
    guide = guide_colorbar(
      title.position = "top",
      title.vjust = 0.5,
      label.theme = element_text(size = 8)
    )
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 10, hjust = 0.4, face = "bold")
  )

# Save the visualization 1 plot using ggsave
ggsave("visualisation_1.pdf", plot = visualisation_1, width = 10, height = 6, dpi = 500)


#---------------------------------------------------------------------------
# Creating Visualization 2
#---------------------------------------------------------------------------

# Calculate the average scores for each goal by year
avg_goal_score <- data %>%
  group_by(year) %>%
  summarize(across(starts_with("goal"),mean, na.rm = TRUE))

# Reshape the data to long format
avg_goal_scores_long <- pivot_longer(avg_goal_score, cols = starts_with("goal"), names_to = "goal", values_to = "avg_score")
# Create custom color palette
goal_colors <- c("#e5233d", "#dda73a", "#4ca146", "#c5192d", "#ef402c", "#27bfe6", "#fbc412", "#a31c44", "#FD6925", "#DD1367", "#FD9D24", "#BF8B2E", "#407f46", "#1f97d4", "#59ba48", "#126a9f", "#13496b")

# Define no. of goals
num_goals <- 17

# Use sprintf to create the padded goal names
goal_names <- sprintf("Goal %02d", 1:num_goals)

# Create a named vector with original and new goal names
goal_mapping <- setNames(goal_names, paste0("goal_", 1:num_goals, "_score"))


# Map the goal names using the defined mapping
avg_goal_scores_long <- avg_goal_scores_long %>%
  mutate(goal_name = goal_mapping[goal])

# Create an interactive plot using plotly
plot <- plot_ly(data = avg_goal_scores_long, x = ~year, y = ~avg_score, color = ~goal_name, colors = goal_colors,  mode = 'lines+markers+lines',
                text = ~paste("<b>Title:</b> ", goal_name, "<br><b>Year:</b> ", year, "<br><b>Avg Score:</b> ",  format(avg_score, digits = 3) ),
                hoverinfo = "text") %>%
  layout(title = "Average Scores for each SDG (2000-2022)",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Average SDG Score"),
         legend = list(
           title = list(text = "<b>SDGs</b>"),
           position = "top.right",
           text = list(size = 12,face = "bold"),
           font = list(size = 14, face = "bold"),
           items = list(symbol = "circle", size = 20)
         ),
         margin = list(l = 50, r = 50, b = 50, t = 50),
         font = list(family = "Arial, sans-serif", size = 12, color = "black",  face = "bold"),
         hoverlabel = list(bgcolor = "#fff", bordercolor = "#c2c3cb", borderradius="10", font = list(family = "Arial, sans-serif", size = 12, color = "black")))

# Save the interactive plot as an HTML file
htmlwidgets::saveWidget(as_widget(plot), "visualization_2.html")

#---------------------------------------------------------------------------










