if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyplots, tidyverse, dplyr)

library(tidyplots)
study
study |>
  tidyplot(x = group, y = score, color = dose) |>
  add_mean_bar(alpha = 0.3) |>
  add_sem_errorbar() |>
  add_data_points() |>
  add_test_asterisks(hide_info = TRUE)|>
  save_plot("score_group.png")


study |>
  tidyplot(x = score, y = treatment, color = treatment) |>
  add_mean_bar(alpha = 0.3) |>
  add_sem_errorbar() |>
  add_data_points()|>
  save_plot("score_treatment.pdf")

spendings|>
  tidyplot(x=category, y=amount, color=category)|>
  add_sum_bar() |>
  adjust_x_axis(rotate_labels = TRUE)|>
  sort_x_axis_levels() |>
  remove_legend() |>
  adjust_colors(new_colors = c("Health" = "#ff3b30"))

all_colors <- c("Health" = "#ff3b30", 
                "Food" = "blue",      # Add other categories as needed
                "Housing" = "green")
spendings|>
  tidyplot(x=category, y=amount, color=category)|>
  add_sum_bar() |>
  adjust_x_axis(rotate_labels = TRUE)|>
  sort_x_axis_levels() |>
  remove_legend() |>
  adjust_colors(new_colors = all_colors)

study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_violin() |>
  add_data_points_beeswarm()

study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_violin() |>
  add_data_points_beeswarm() |>
  reorder_x_axis_levels("C", "D", "A", "B")|>
  rename_x_axis_levels(new_names = c(
    "A" = "Dog",
    "B" = "Cat",
    "C" = "Hamster",
    "D" = "Dinosaur"
  ))

study |>
  tidyplot(x=treatment, y=score, color=treatment)|>
  add_mean_dash()|>
  add_sem_errorbar()|>
  add_data_points()|>
  add_test_pvalue(hide_info=TRUE,  comparisons = list(c(1,3),c(2,4),c(2,3), c(1,4)))
#add_test_asterisks(hide_info = FALSE)

study |>
  tidyplot(x=treatment, y=score, color=treatment)|>
  save_plot("st1.png") |>
  add_mean_dash()|>
  save_plot("st2.png") |>
  add_sem_errorbar()|>
  save_plot("st3.png") |>
  add_data_points()|>
  save_plot("st4.png") |>
  add_test_asterisks(hide_info = FALSE) |>
  save_plot("st5.png") 



library(tidyverse)

gene_expression |> 
  filter(external_gene_name %in% c("Apol6", "Col5a3", "Bsn", "Fam96b", "Mrps14", "Tma7")) |> 
  tidyplot(x = sample_type, y = expression, color = condition) |> 
  add_violin() |> 
  add_data_points_beeswarm(white_border = TRUE) |> 
  adjust_x_axis_title("") |> 
  remove_legend() |> 
  add_test_asterisks(hide_info = TRUE, bracket.nudge.y = 0.3) |> 
  adjust_colors(colors_discrete_ibm) |> 
  adjust_y_axis_title("Gene expression") |> 
  adjust_size(width = 35, height = 25) |> 
  split_plot(by = external_gene_name, ncol = 2)

library(tidyverse)
library(tidyplots)

df <- 
  read_csv("https://tidyplots.org/data/pca-plot.csv")

df |> 
  tidyplot(x = pc1, y = pc2, color = group) |> 
  add_data_points(size = 1.3, white_border = TRUE) |> 
  add_ellipse() |> 
  adjust_x_axis_title(paste0("Component 1 (", round(df$pc1_var*100, digits = 1), "%)")) |> 
  adjust_y_axis_title(paste0("Component 2 (", round(df$pc2_var*100, digits = 1), "%)")) |> 
  adjust_colors(colors_discrete_apple) |> 
  adjust_legend_title("Group")



if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyplots, tidyverse, dplyr)

library(tidyplots)
study
study |>
  tidyplot(x = group, y = score, color = dose) |>
  add_mean_bar(alpha = 0.3) |>
  add_sem_errorbar() |>
  add_data_points() |>
  add_test_asterisks(hide_info = TRUE)|>
  save_plot("score_group.png")


study |>
  tidyplot(x = score, y = treatment, color = treatment) |>
  add_mean_bar(alpha = 0.3) |>
  add_sem_errorbar() |>
  add_data_points()|>
  save_plot("score_treatment.pdf")

spendings|>
  tidyplot(x=category, y=amount, color=category)|>
  add_sum_bar() |>
  adjust_x_axis(rotate_labels = TRUE)|>
  sort_x_axis_levels() |>
  remove_legend() |>
  adjust_colors(new_colors = c("Health"= "#ff3b30"))

study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_violin() |>
  add_data_points_beeswarm()

study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_violin() |>
  add_data_points_beeswarm() |>
  reorder_x_axis_levels("C", "D", "A", "B")|>
  rename_x_axis_levels(new_names = c(
    "A" = "Dog",
    "B" = "Cat",
    "C" = "Hamster",
    "D" = "Dinosaur"
  ))

study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_violin() |>
  add_data_points_beeswarm() |>
  reorder_x_axis_levels("C", "D", "A", "B")|>
  rename_x_axis_levels(new_names = c(
    "A" = "$IFN*gamma$",
    "B" = "$TNF*alpha$",
    "C" = "$H[2]*O$",
    "D" = "$C[8]*H[10]*N[4]*O[2]$"
  )) |>
  adjust_size(width=100, height=100)

study |>
  tidyplot(x=treatment, y=score, color=treatment)|>
  add_mean_dash()|>
  add_sem_errorbar()|>
  add_data_points()|>
  add_test_pvalue(hide_info=TRUE,  comparisons = list(c(1,3),c(2,4),c(2,3), c(1,4)))
#add_test_asterisks(hide_info = FALSE)

animals |>
  tidyplot(x=weight, y=speed)|>
  add_reference_lines(x = 4000,
                      y= c(100,200),
                      linetype ="dotdash") |>
  add_data_points()|>
  adjust_size(width=100, height=100)


library(tidyverse)

gene_expression |> 
  filter(external_gene_name %in% c("Apol6", "Col5a3", "Bsn", "Fam96b", "Mrps14", "Tma7")) |> 
  tidyplot(x = sample_type, y = expression, color = condition) |> 
  add_violin() |> 
  add_data_points_beeswarm(white_border = TRUE) |> 
  adjust_x_axis_title("") |> 
  remove_legend() |> 
  add_test_asterisks(hide_info = TRUE, bracket.nudge.y = 0.3) |> 
  adjust_colors(colors_discrete_ibm) |> 
  adjust_y_axis_title("Gene expression") |> 
  adjust_size(width = 35, height = 25) |> 
  split_plot(by = external_gene_name, ncol = 2)

library(tidyverse)
library(tidyplots)

df <- 
  read_csv("https://tidyplots.org/data/pca-plot.csv")

df |> 
  tidyplot(x = pc1, y = pc2, color = group) |> 
  add_data_points(size = 1.3, white_border = TRUE) |> 
  add_ellipse() |> 
  adjust_x_axis_title(paste0("Component 1 (", round(df$pc1_var*100, digits = 1), "%)")) |> 
  adjust_y_axis_title(paste0("Component 2 (", round(df$pc2_var*100, digits = 1), "%)")) |> 
  adjust_colors(colors_discrete_apple) |> 
  adjust_legend_title("Group")


# Function

#Define Style
my_style <- function(x) x|>
  adjust_colors(colors_discrete_candy) |>
  adjust_font(family = "mono")|>
  adjust_size(height = 30)|>
  adjust_size(width=100, height=10)

#Set global options
tidyplots_options(my_style = my_style)

study |>
  tidyplot(x=group, y=score, color = dose) |>
  add_mean_bar()


+
