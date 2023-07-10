library(tidyverse)
library(ggplot2)


# Individual tasks formatted as a data.frame
tasks <- data.frame(
  Task = c("Research", "Clinical Trials", "Regulatory Approval", "Manufacturing", "Marketing"),
  StartDate = as.Date(c("2023-05-01", "2023-07-01", "2024-01-01", "2024-03-01", "2024-06-01")),
  EndDate = as.Date(c("2023-06-30", "2023-12-31", "2024-06-30", "2024-12-31", "2025-06-30"))
)

tasks

ggplot(tasks, aes(x = StartDate, xend = EndDate, y = fct_rev(fct_inorder(Task)), yend = Task)) + 
  geom_segment()

colors <- c("#deecfb", "#bedaf7", "#7ab3ef", "#368ce7", "#1666ba")

ggplot(tasks, aes(x = StartDate, xend = EndDate, y = fct_rev(fct_inorder(Task)), yend = Task)) + 
  geom_segment(linewidth = 35, color = colors)

ggplot(tasks, aes(x = StartDate, xend = EndDate, y = fct_rev(fct_inorder(Task)), yend = Task)) + 
  geom_segment(linewidth = 35, color = colors) + 
  labs(
    title = "Pharma Company Gantt Chart",
    x = "Duration",
    y = "Task"
  ) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  theme(
    plot.title = element_text(size = 20),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12, angle = 45)
  )