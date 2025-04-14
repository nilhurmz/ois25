
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)


data <- data.frame(
  Group = rep(c("primary education", "high school", "bachelorette"), each = 3),
  Category = rep(c("Gen-Alpha", "Gen-Z", "Gen-Y"), times = 3),
  Value = c(10, 20, 15, 25, 30, 28, 12, 18, 22)
)


ggplot(data, aes(x = Category, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Category-wise Values by Group",
    x = "Category",
    y = "Value"
  ) +
  theme_minimal()
ggplot(data, aes(x = Category, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("primary education" = "#FF6B6B", "high school" = "#4ECDC4", "bachelorette" = "#556270")) +
  labs(
    title = "education levels on different generations",
    x = "Category",
    y = "Value"
  ) +
  theme_minimal()





