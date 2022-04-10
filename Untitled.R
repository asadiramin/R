### 1. Which department has the highest employee turnover? Which one has the lowest?

install.packages("tidyverse")
library(tidyverse)

df_left <- df %>%
  mutate(left_num = case_when(left == "no" ~ 0, left == "yes" ~ 1)) %>%
  filter(left_num == 1) %>%
  select(department, left_num) %>%
  group_by(department) %>%
  summarise(total_left = sum(left_num)) %>%
  arrange(desc(total_left))

ggplot(data = df_left, mapping = aes(x = reorder(toupper(department), total_left), total_left)) + 
  geom_bar(stat = "identity", fill = "black") +
  coord_flip() +
  geom_text(aes(label = total_left, hjust=2, family = "Econ Sans Cnd"), color = "white", size = 4) +
  labs(title = "Highest and Lowest Employee Turnover Across Departments", x="", y="") +
  theme(plot.title = element_text(family = "Econ Sans Cnd",  face = "bold", size = 12))
