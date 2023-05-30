## Racist Europe
library(tidyverse)
library(janitor)
library(countrycode)
library(survey) 
library(srvyr)
library(questionr)
library(cowplot)


setwd("~/Desktop/Sociology/Dissertation/Analysis/ESS analysis")

## Load weight data (from ESS official weights)
weight_data <- read_csv("ESS7.csv/ESS7SDDFe1_2.csv")

## Load ESS7, do the proper recoding, and merge the weight data:
ESS7 <- read_csv("ESS7.csv/ESS7.csv") %>%
  mutate(across(c(smegbli, smegbhw, aljewlv, almuslv, algyplv), ~ ifelse(.x > 6, NA, .x)),
         inferior_intellect = ifelse(smegbli == 1, 1, 0),
         inferior_workers = ifelse(smegbhw == 1, 1, 0),
         anweight = pspwght * pweight * 1e4) %>%
  left_join(weight_data, by = c("cntry" = "cntry", "idno" = "idno"))
  
## Weigh the data according to the ESS weighting documents:
ESS7_survey_tidy <- ESS7 %>%
  as_survey_design(ids = psu,  strata = stratum, weights = anweight)

## Create the dataframe for the plots:

plot_data <- ESS7_survey_tidy %>%
  group_by(cntry) %>%
  summarise(intellect = survey_mean(inferior_intellect, na.rm = T, proportion = T),
            working = survey_mean(inferior_workers, na.rm = T, proportion = T), 
            diff = survey_mean(inferior_workers - inferior_intellect, na.rm = T)) %>%
  mutate(diff_se = sqrt(intellect_se^2 + working_se^2)) %>%
  pivot_longer(-c(cntry, diff, diff_se), names_to = "type", values_to = "values") %>%
  separate(type, into = c("racism_type", "stat")) %>%
  mutate(stat = ifelse(is.na(stat), "p", stat)) %>%
  pivot_wider(names_from = stat, values_from = values) %>%  
  group_by(cntry) %>%
  mutate(avg_racism = mean(p),
         country_name = countrycode(cntry, origin = "iso2c", "country.name")) %>%
  filter(country_name != "Israel") %>%
  ungroup() 


p1 <- plot_data %>%
  mutate(country_name = fct_reorder(country_name, avg_racism)) %>%
  ggplot(aes(x = country_name, y = p, color = racism_type, shape = racism_type)) +
  geom_pointrange(aes(ymax = p + 1.96 * se, ymin = p - 1.96 * se)) +
  scale_color_manual(values = c("#59AFCA", "#DB7E1F"), 
                     name = "Are some races born...", labels = c("Less Intelligent?", "Less Hard Working?")) +
  scale_shape_manual(values = c(15, 16), 
                     name = "Are some races born...", labels = c("Less Intelligent?", "Less Hard Working?")) +
  coord_flip(clip="off") +
  theme(
    panel.background = element_rect(fill = "#F2F2F2",
                                    colour = "#F2F2F2"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                    colour = "white"),
    plot.background = element_rect(fill = "#F2F2F2"),
    axis.ticks = element_blank(),
    legend.background = element_rect(fill = "#F2F2F2",
                              colour = "#F2F2F2"),
    text = element_text(size = 14),
    axis.title.y = element_text(size = 12, colour = "gray25"),
    panel.border = element_blank()
  ) +
  labs(x = "More believers in racial difference", 
       y = "Proportion who answered yes"#, caption = "Data comes from the ESS Round 7 (2014)"
       ) +
  annotation_custom(
    grob = linesGrob(arrow = arrow(type = "open", ends = "last", length = unit(2, "mm")), 
                     gp = gpar(col = "gray25", lwd = 2)),
    xmin = 20, xmax = 1, ymin = -0.19, ymax = - 0.19
  )

p2 <- plot_data %>% 
  mutate(country_name = fct_reorder(country_name, diff)) %>%
  ggplot(aes(x = country_name, y = diff)) +
  geom_pointrange(aes(ymax = diff + 1.96 * diff_se, ymin = diff - 1.96 * diff_se), color = "gray35", shape = 17) +
  geom_text(aes(label = country_name), nudge_y = - 0.01, nudge_x =  0.5, hjust = "right") +
  coord_flip(clip = "off") +
  theme(
    panel.background = element_rect(fill = "#F2F2F2",
                                    colour = "#F2F2F2"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                    colour = "white"),
    plot.background = element_rect(fill = "#F2F2F2"),
    axis.ticks = element_blank(),
    text = element_text(size = 14),
    axis.title.y = element_text(size = 12, colour = "gray25"),
    axis.text.y = element_blank(),
    panel.border = element_blank()
  ) +
  labs(x = "More belief accordance", 
       y = "Difference across the two beliefs"#, caption = "Data comes from the ESS Round 7 (2014)"
       ) +
  annotation_custom(
    grob = linesGrob(arrow = arrow(type = "open", ends = "first", length = unit(2, "mm")), 
                     gp = gpar(col = "gray25", lwd = 2)),
    xmin = 20, xmax = 1, ymin = 0.45, ymax =  0.45
  ) +
  scale_x_discrete(position = 'top') 
  

plot_grid(p1, p2, ncol = 2, rel_widths = c(7.5/12, 4.5/12))
