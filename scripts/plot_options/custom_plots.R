
my_theme <- 
  theme(
    plot.title = element_text(hjust = .5, size = 24, face = "bold"),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text  = element_text(size = 18),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    strip.background = element_rect(fill = "oldlace")
  )

yearly_bands <- 
  list(
    annotate("rect", fill = "skyblue1", alpha = 0.2, 
             xmin = as.Date("2019-02-01"), xmax = as.Date("2019-12-31") + 11/12,
             ymin = -Inf, ymax = Inf),
    annotate("rect", fill = "wheat1", alpha = 0.2, 
             xmin = as.Date("2020-01-01"), xmax = as.Date("2020-12-31") + 11/12,
             ymin = -Inf, ymax = Inf),
    annotate("rect", fill = "skyblue1", alpha = 0.2, 
             xmin = as.Date("2021-01-01"), xmax = as.Date("2021-12-31") + 11/12,
             ymin = -Inf, ymax = Inf), 
    annotate("rect", fill = "wheat1", alpha = 0.2, 
             xmin = as.Date("2022-01-01"), xmax = as.Date("2022-12-31") + 11/12,
             ymin = -Inf, ymax = Inf),
    annotate("rect", fill = "skyblue1", alpha = 0.2, 
             xmin = as.Date("2023-01-01"), xmax = as.Date("2023-12-31") + 11/12,
             ymin = -Inf, ymax = Inf),
    annotate("rect", fill = "wheat1", alpha = 0.2, 
             xmin = as.Date("2024-01-01"), xmax = as.Date("2024-07-31") + 11/12,
             ymin = -Inf, ymax = Inf))
