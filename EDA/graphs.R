library(tidyverse)
library(readxl)
library(janitor)
library(gt)
library(patchwork)

read <- read_excel("Data/Reading_Dataset_UVA_Deidentified_02-02-24.xlsx") |> clean_names()
math <- read_excel("Data/Math_Dataset_UVA_Deidentified_03-18-24.xlsx") |> clean_names() |> 
  mutate(
    vertical_scaled_score = as.numeric(vertical_scaled_score)
  )

combined <- bind_rows(read, math) |> 
  mutate(
    race = case_when(
      race == 'White, not of Hispanic origin' ~ 'White', 
      race == "Black, not of Hispanic origin" ~ "Black", 
      TRUE ~ race
    )
  ) |> 
  mutate(
    growth = ifelse(is.na(growth), value_added_score, growth),
    type_of_teacher_certification = case_when(
      is.na(type_of_teacher_certification) ~ 'None', 
      (type_of_teacher_certification == 'NA') ~ 'None',
      (type_of_teacher_certification == 'FULL') ~ 'Full',
      (type_of_teacher_certification == 'MICRO') ~ 'Micro',
      TRUE ~ "None"
    )
  ) 


summ <- combined |> 
  group_by(
    race
  ) |> 
  summarize(
    mean = mean(teacher_years_experience, na.rm = TRUE), 
    median = median(teacher_years_experience, na.rm = TRUE)
  )

combined |> 
ggplot(aes(x = teacher_years_experience, y = race, fill = race)) + 
  geom_boxplot() +
  labs(
    y = 'Student Race',
    x = 'Teacher Years Experience',
    title = "Teacher Years Experience by Student Race"
  ) + theme_classic() +
  geom_text(
    data = summ, 
    aes(x = median, y = race, label = paste("Mean: ", round(mean, 1), "\nMedian: ", median), hjust = -0.1),
    size = 3,
    fontface = "bold"
  ) + 
  scale_fill_brewer(palette="Set2") +
  theme(legend.position = "none")


combined |> 
  group_by(
   `Teacher Certification` = type_of_teacher_certification
  ) |> 
  summarize(
    Growth = mean(growth, na.rm = TRUE), 
    `Year of Teacher Experience` = mean(teacher_years_experience, na.rm = TRUE), 
    Students = n()
  ) |> 
  gt()

combined |> 
  summarize(
    Count = n(),
    .by = c(type_of_teacher_certification, race)
  ) |> ggplot(aes(x = race, y = type_of_teacher_certification, fill = Count, label = Count)) +
  geom_tile() +
  # scale_fill_gradient(low = "pink", high = "red") +
  scale_fill_gradient2(low = "white", mid = "pink", high = "Red") +
  geom_text() +
  theme_classic() +
  labs(
    x = "Student Race", 
    y = "Teacher Certification", 
    title = "Count of Student Race with Teacher Certification"
  )

p1 <- combined |> 
  ggplot(
    aes(
      x = gender, 
      y = expected_achievement, 
      fill = race
    )
  ) +
  geom_boxplot() +
  theme_classic() +
  labs(
    y = "Expected Achievement", 
    x = "Gender", 
    fill = "Race"
  ) +
  theme(axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(200, 600))


p2 <- combined |> 
  ggplot(
    aes(
      x = gender, 
      y = actual_achievement, 
      fill = race
    )
  ) +
  geom_boxplot() +
  theme_classic() +
  labs(
    y = "Actual Achievement", 
    x = "Gender", 
    fill = "Race"
  ) +
  theme(legend.position = "none", axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(200, 600))

p1 + p2 + plot_layout(
  guides = "collect"
  ) & 
  theme(
    legend.position = "bottom",
    legend.direction = 'horizontal'
  ) 
  
  
