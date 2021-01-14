#### Radial Bubble Plot

![plot](https://github.com/sarahcgall/uknoncompliance/blob/master/mediaconsumptionplot.jpeg)


***Code:***

*Included packages:*

```{r}
# Libraries
library(tidyverse)
library(ggtext)
library(ggpmisc)
library(ggrepel)
library(RColorBrewer)
library(extrafont)
library(scales)
```

*Cleaning data:*

```{r}
# Import and Clean Data (NB data has been taken from: https://www.ofcom.org.uk/research-and-data/tv-radio-and-on-demand/news-media/coronavirus-news-consumption-attitudes-behaviour/interactive-data and manually entered using excel))
data <- read.csv("~/uknoncompliance/Media Consumption.csv", header = TRUE)
data <- data %>%
  mutate(individual = ï..individual, `16-34` = X16.34, `35-54` = X35.54, `55+` = X55.) %>%
  select(individual, group, `16-34`, `35-54`, `55+`, id)

# Pivot data longer
data <- data %>%
  pivot_longer(cols = c(`16-34`, `35-54`, `55+`), names_to = "Age Group", values_to = "value")
data <- data %>% arrange(id)

# Get the name and the y position of each label
label_data <- data %>% 
  group_by(id, individual) %>% 
  summarize(tot=sum(value))
angle <- 90 - 360 * (label_data$id-0.5)/nrow(label_data)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle + 180, angle)
label_data <- label_data %>%
  mutate(hjust = ifelse(id %in% c(8,16), 0.5, hjust))

# Add position of y
data <- data %>%
  mutate(position = ifelse(`Age Group` == "55+", 4,
                             ifelse(`Age Group` == "35-54", 3, 2)),
         label1 = ifelse(value > 4, paste0(value, "%"), ""))
```

*Plot:*

```{r}
# Make the plot
ggplot(data) +
  
  # Add grid lines
  geom_hline(yintercept = seq(0, 3, by = 1), colour = "grey90", size = 0.2) +
  geom_segment(x = 0, y = 1, xend = 0, yend = 4, colour = "grey90", size = 0.2) +
  geom_segment(x = 1, y = 1, xend = 1, yend = 4, colour = "grey90", size = 0.2) +
  geom_segment(x = 2, y = 1, xend = 2, yend = 4, colour = "grey90", size = 0.2) +
  geom_segment(x = 3, y = 1, xend = 3, yend = 4, colour = "grey90", size = 0.2) +
  geom_segment(x = 4, y = 1, xend = 4, yend = 4, colour = "grey90", size = 0.2) +
  geom_segment(x = 5, y = 1, xend = 5, yend = 4, colour = "grey90", size = 0.2) +
  geom_segment(x = 6, y = 1, xend = 6, yend = 4, colour = "grey90", size = 0.2) +
  geom_segment(x = 7, y = 1, xend = 7, yend = 4, colour = "grey90", size = 0.2) +
  geom_segment(x = 8, y = 1, xend = 8, yend = 4, colour = "grey90", size = 0.2) +
  geom_segment(x = 9, y = 1, xend = 9, yend = 4, colour = "grey90", size = 0.2) +
  geom_segment(x = 10, y = 1, xend = 10, yend = 4, colour = "grey90", size = 0.2) +
  geom_segment(x = 11, y = 1, xend = 11, yend = 4, colour = "grey90", size = 0.2) +
  geom_segment(x = 12, y = 1, xend = 12, yend = 4, colour = "grey90", size = 0.2) +
  geom_segment(x = 13, y = 1, xend = 13, yend = 4, colour = "grey90", size = 0.2) +
  geom_segment(x = 14, y = 1, xend = 14, yend = 4, colour = "grey90", size = 0.2) +
  geom_segment(x = 15, y = 1, xend = 15, yend = 4, colour = "grey90", size = 0.2) +
  geom_segment(x = 16, y = 1, xend = 16, yend = 4, colour = "grey90", size = 0.2) +
  
  # Add dividers
  geom_segment(x = 15.5, y = 0, xend = 15.5, yend = 4, colour = "grey65", size = 0.2, linetype = "dashed") +
  geom_segment(x = 0.5, y = 0, xend = 0.5, yend = 4, colour = "grey65", size = 0.2, linetype = "dashed") +
  geom_segment(x = 5.5, y = 0, xend = 5.5, yend = 4, colour = "grey65", size = 0.2, linetype = "dashed") +
  geom_segment(x = 7.5, y = 0, xend = 7.5, yend = 4, colour = "grey65", size = 0.2, linetype = "dashed") +
  geom_segment(x = 9.5, y = 0, xend = 9.5, yend = 4, colour = "grey65", size = 0.2, linetype = "dashed") +
  geom_hline(yintercept = 4, colour = "grey65", size = 0.2) +
  geom_bar(aes(x = 8, y = 4), stat = "identity", alpha = 0.25, width = 1, fill = "grey85") +
  geom_bar(aes(x = 9, y = 4), stat = "identity", alpha = 0.25, width = 1, fill = "grey85") +
  
  # Add the bubble plot
  geom_point(aes(x = id, y = position, colour = `Age Group`, size = value), alpha = 0.9) +
  scale_colour_manual(values = c("#344D90", "#E7552C", "#FFB745")) +
  scale_size_continuous(range = c(0, 25)) +
  scale_y_continuous(breaks = c(0,1,2,3,4), limits = c(0,4.7), expand = c(0,0)) +
  scale_x_continuous(limits = c(0,16), expand = c(0,0)) +
  coord_polar(clip = 'off') +
  
  # Add labels on top of each section
  geom_text(data=label_data, 
            aes(x = id, y = 4.7, label = str_wrap(individual,25), hjust = hjust), 
            colour = "black", fontface = "bold", alpha = 0.6, size = 3, angle = 0, inherit.aes = FALSE) +
  
  # Add % to bubbles
  geom_text(data=data,
            aes(x = id, y = position, label = label1), colour = "white", size = 3, fontface = "bold") +
  #manually adding the labels that had values < 4%
  geom_text(data=data,
            aes(x = 6.7, y = 2.1, label = "3%"), colour = "#344D90", size = 3, fontface = "plain") +
  geom_text(data=data,
            aes(x = 6.74, y = 3.05, label = "2%"), colour = "#E7552C", size = 3, fontface = "plain") +
  geom_text(data=data,
            aes(x = 6.82, y = 4, label = "1%"), colour = "#FFB745", size = 3, fontface = "plain") +
  geom_text(data=data,
            aes(x = 5.7, y = 2.2, label = "2%"), colour = "#344D90", size = 3, fontface = "plain") +
  geom_text(data=data,
            aes(x = 6, y = 3, label = "0%"), colour = "#E7552C", size = 3, fontface = "plain") +
  geom_text(data=data,
            aes(x = 6, y = 4, label = "0%"), colour = "#FFB745", size = 3, fontface = "plain") +
  geom_text(data=data,
            aes(x = 3, y = 4.3, label = "3%"), colour = "#FFB745", size = 3, fontface = "plain") +
  geom_text(data=data,
            aes(x = 4, y = 4.3, label = "3%"), colour = "#FFB745", size = 3, fontface = "plain") +
  
  # Adjust Legend
  guides(size = FALSE,
         colour = guide_legend(override.aes = list(size = 5), title = "")) +
  
  # Add heading, subtitle, and captions
  labs(title = "<span><br><br>Percent of sources used by respondents to obtain information or news about the Coronavirus<br>outbreak in the week prior by age group</span>",
       subtitle = "<span>Additionally, includes percent of respondents who said they were trying to avoid news about COVID-19 and percent who said<br>they were confused about what they should be doing in response to COVID-19 highlighted in grey.<br></span>",
       caption = "<span><b>Source:</b> Ofcom <i>(fieldwork by Yonder: 4-6 Dec 2020)</i> | <b>Created by:</b> @sarahcgall_<br></span>") +
  
  # Adjust theme
  theme_minimal() +
  theme(
    plot.margin = margin(0,0.5,0.5,0.5, "cm"),
    panel.grid = element_blank(),
    plot.title = element_markdown(face = "bold", size = 12, family = "Times"),
    plot.subtitle = element_markdown(face = "plain", size = 10, family = "Times"),
    plot.caption = element_markdown(face = "plain", size = 8, family = "Times", hjust = 0),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "top",
    legend.justification = c(.5,13)
  )
```