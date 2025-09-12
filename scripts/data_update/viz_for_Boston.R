### visualization for mockup

library(tidyverse)
library(sf)
library(scales)

boston_sds_22_data <- st_read("data/processed/Boston/sds_in_echo_data.geojson")

hingham <- st_drop_geometry(boston_sds_22_data) |> 
  filter(district == "Hingham School District") |> 
  select(pct_native_23:pct_two_plus_23) |> 
  rename(`Percent Native American` = pct_native_23,
         `Percent Asian American / Pacific Islander` = pct_aapi_23,
         `Percent Hispanic or Latino` = pct_latinx_23,
         `Percent Black or African American` = pct_black_23,
         `Percent White` = pct_white_23,
         `Percent Hawaiian or Other Pacific Islander` = pct_hawpi_23,
         `Percent Multiracial` = pct_two_plus_23)


# Reshape the data from wide to long format
hingham_long <- hingham %>%
  pivot_longer(cols = everything(), 
               names_to = "Race", 
               values_to = "Percentage") |> 
  filter(Percentage > 0)

bar_colors <- c("#8dd3c7", "#ffffb3", "#bebada", "#fdb462", "#fb8072", "#80b1d3")

# Create the horizontal stacked bar chart
race_bar <- ggplot(hingham_long, aes(
  x = "", 
  y = Percentage, 
  fill = Race)) + 
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +  # Flip coordinates for horizontal orientation
  labs(title = "Enrollment by Race",
       y = NULL, 
       x = "Race/Ethnicity") +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 14), # Increase legend font size
        axis.title = element_text(size = 18),   # Increase axis title font size
        axis.text = element_text(size = 12),    # Increase axis text font size
        plot.title = element_text(size = 24),
        panel.background = element_rect(fill = "transparent"), # Make panel background transparent
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank()) + # Make plot background transparent
  theme(legend.position = "bottom") +  # Place legend at the bottom
  scale_fill_manual(values = bar_colors) +  # Apply your custom colors
  scale_y_continuous(labels = percent_format())


race_bar

ggsave("img/plots/Boston_Hingham_enrollment_by_race.png", #specify the file path/name/type
       plot = race_bar)



#### AP participation rate

ap_bar_colors <- c("#8dd3c7", "#ffffb3", "#fdb462", "#fb8072", "#80b1d3")

hingham_ap <- st_drop_geometry(boston_sds_22_data) |> 
  filter(district == "Hingham School District") |> 
  select(pct_latinx_ap_enroll_d_race:pct_white_ap_enroll_d_race) |> 
  rename(`Native` = pct_native_ap_enroll_d_race,
         `Asian American` = pct_asian_ap_enroll_d_race,
         `Hispanic` = pct_latinx_ap_enroll_d_race,
         `Black` = pct_black_ap_enroll_d_race,
         `White ` = pct_white_ap_enroll_d_race)


# Reshape the data from wide to long format
hingham_ap_long <- hingham_ap %>%
  pivot_longer(cols = everything(), 
               names_to = "Race", 
               values_to = "Percentage") |> 
  filter(Race != "pct_hawpi_ap_enroll_d_race") |> 
  arrange(-Percentage)

# Create the horizontal bar chart
ap_bar = ggplot(hingham_ap_long, aes(x = Race, y = Percentage, fill = Race)) + 
  geom_col() +  # Use geom_col() for bar charts with y values
  labs(title = "AP Participation by Race",
       y = "Percentage", 
       x = "Race/Ethnicity") +
  theme_minimal() +  # Use a clean theme
  theme(legend.position = "none",  # Remove the legend
        axis.title.y = element_blank(), # Remove y-axis title
        axis.title.x = element_blank(),   # Increase x-axis title font size
        axis.text.y = element_text(size = 14),    # Increase x-axis text font size
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1), # Rotate y-axis labels by 45 degrees
        plot.title = element_text(size = 24),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA)) + 
  scale_fill_manual(values = ap_bar_colors) +  # Apply your custom colors
  scale_y_continuous(labels = percent_format()) # Format y-axis as percentages

ap_bar

ggsave("img/plots/Boston_Hingham_ap_by_race.png", #specify the file path/name/type
       plot = ap_bar)



bar_colors <- c("#8dd3c7", "#ffffb3", "#bebada", "#fdb462", "#fb8072", "#80b1d3")

hingham_educato <- st_drop_geometry(boston_sds_22_data) |> 
  filter(district == "Hingham School District") |> 
  select(pct_black_educ:pct_white_educ) |> 
  rename(`Native` = pct_native_educ,
         `Asian American` = pct_asian_educ,
         `Hispanic` = pct_latinx_educ,
         `Black` = pct_black_educ,
         `White ` = pct_white_educ,
         `Percent Multiracial` = pct_two_plus_educ) |> 
  select(-pct_hawpi_educ)


# Reshape the data from wide to long format
hingham_ed_long <- hingham_educato %>%
  pivot_longer(cols = everything(), 
               names_to = "Race", 
               values_to = "Percentage") |> 
  filter(Race != "pct_hawpi_ap_enroll_d_race") |> 
  arrange(-Percentage)

# Create the horizontal bar chart
ed_bar = ggplot(hingham_ed_long, aes(x = Race, y = Percentage, fill = Race)) + 
  geom_col() +  # Use geom_col() for bar charts with y values
  labs(title = "Teachers by Race",
       y = "Percentage", 
       x = "Race/Ethnicity") +
  theme_minimal() +  # Use a clean theme
  theme(legend.position = "none",  # Remove the legend
        axis.title.y = element_blank(), # Remove y-axis title
        axis.title.x = element_blank(),   # Increase x-axis title font size
        axis.text.y = element_text(size = 14),    # Increase x-axis text font size
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1), # Rotate y-axis labels by 45 degrees
        plot.title = element_text(size = 24),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA)) + 
  scale_fill_manual(values = bar_colors) +  # Apply your custom colors
  scale_y_continuous(labels = percent_format()) # Format y-axis as percentages

ed_bar

ggsave("img/plots/Boston_Hingham_Educators_by_race.png", #specify the file path/name/type
       plot = ed_bar)
