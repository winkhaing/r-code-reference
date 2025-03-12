shifts <- structure(list(PatientId = c("7391991", "7391992", "7391993", 
                                       "7391994", "7391995", "7391996", "7391997", "7391999", "7392991", 
                                       "7392992", "7392994", "7392995", "7392996", "7392998", "7392999", 
                                       "7392919", "7392911", "7392912", "7392913", "7392914", "7392915", 
                                       "7392916", "7392917", "7392918", "7392919", "7392929", "7392922", 
                                       "7392923", "7392924", "7392925", "7392926", "7394991", "7394992", 
                                       "7396991", "7396992", "7396993", "7396994", "7396995", "7396996", 
                                       "7396997", "7396998", "7397991", "7398991", "7398992", "7398993", 
                                       "7398994", "7398995", "7398996", "7398997", "7398998", "7399991", 
                                       "7399992", "7399993", "7399994", "7319991", "7319992", "7319993", 
                                       "7313991", "7313992", "7313993", "7313994", "7313995", "7316991", 
                                       "7316992", "7317991", "7318991"), Score = c(1.2, 0.8, 3, 3.2, 
                                                                                   2.2, 0.6, 0.4, 0.2, 5.4, 1.8, 2, 4.2, 2.2, 1.4, 3.4, 3.2, 1.6, 
                                                                                   2.2, 3, 3.2, 0, 0, 3.2, 2.8, 3.4, 3.4, 2, 0.4, 2.2, 0.2, 3, 0.8, 
                                                                                   3.2, 1, 3.6, 0.4, 1.2, 1.2, 0.2, 2.4, 1.6, 1, 1.4, 3.8, 1.4, 
                                                                                   3.2, 0.4, 0.8, 1.2, 0, 0.4, 0.8, 0.2, 0.2, 1.4, 2.2, 2, 2, 1.6, 
                                                                                   0.6, 4.2, 0.4, 0.8, 0.2, 1.2, 0.2), ScoreBaseline = c(3.4, 4, 
                                                                                                                                         3, 2.4, 5, 2, 1.4, 1.8, 5, 2, 1.6, 4, 4, 3, 4, 3.2, 1.4, 3.4, 
                                                                                                                                         4.2, 3.4, 1.2, 0.4, 4, 2.8, 2.2, 6, 3.6, 2.8, 2.2, 1.4, 3.2, 
                                                                                                                                         1.4, 3, 1.2, 2.8, 0.6, 2.6, 3.8, 3.4, 2.6, 1.6, 4, 1.4, 2.8, 
                                                                                                                                         3.2, 2.6, 1.8, 2.6, 1.8, 3, 0, 1.4, 2.2, 1, 3.6, 2.4, 1.8, 3.8, 
                                                                                                                                         1.8, 5.4, 5, 1.6, 1.8, 1.2, 2.6, 2)), row.names = c(NA, -66L), Version = 3L, Date = structure(1698083583.12, tzone = "UTC", class = c("POSIXct", 
                                                                                                                                                                                                                                                                               "POSIXt")), class = "data.frame") 


# ----------------------- Data preparation 
library(tidyverse)
shifts %>% 
  mutate(change = Score - ScoreBaseline) %>% 
  pivot_longer(cols = c(ScoreBaseline, Score), names_to = "Timepoint", values_to = "Value") |> 
  
  mutate(Timepoint = factor(Timepoint, 
                            levels = c("ScoreBaseline", "Score"), 
                            labels = c("Baseline", "Week 8"))) %>% 
  
  mutate(change_type = factor(case_when(change > 0 ~ "increase", 
                                        change < 0 ~ "decrease", 
                                        change == 0 ~ "no change"), 
                              levels = c("increase", "decrease", "no change"))) |> 
  
  mutate(MCID = factor(ifelse(abs(change) >= 0.5, "MCID", "nonMCID"), 
                       levels = c("nonMCID", "MCID"), 
                       ordered = TRUE)) -> scores_bas_w8 

# ----------------------- Bands definitions 

df_bands <- data.frame(xmin = -Inf, 
                       xmax = Inf, 
                       ymin = c(0, 0.749, 1.501), 
                       ymax = c(0.75, 1.5, 6), 
                       range = c("well", "partially", "not well")) 

# ----------------------- Plotting 
install.packages("gghalves")
library(gghalves)
library(ggnewscale)
scores_bas_w8 %>% 
  ggplot(aes( 
    x = Timepoint, 
    y = Value, 
    group = interaction(PatientId, MCID) 
  )) + 
  
  # # Draw the bands 
 geom_rect( 
   data = df_bands, 
   aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, fill = range), 
   alpha = 0.32, 
   inherit.aes = FALSE 
 ) + 
 scale_fill_manual( 
   values = c("not well" = "#c00000", "partially" = "#00B0F0", "well" = "green3"), 
   name = "Asthma controlled: " 
 ) + 
 ggnewscale::new_scale_fill() + 
  
# Draw the changes 
  
  geom_line(aes(col = change_type, linetype = MCID, linewidth = MCID)) + 
  geom_line(aes(col = change_type, linetype = MCID, linewidth = MCID)) + 
  scale_color_manual( 
    values = c("increase" = "red2", "decrease" = "green4", "no change" = "#3F4444"), 
    guide = guide_legend(override.aes = list(linewidth = 1)) 
  ) + 
  scale_linewidth_ordinal( 
    range = c(0.3, 0.6), 
    labels = c("MCID" = "|change ≥ 0.5|", "nonMCID" = "|change < 0.5|") 
  ) + 
  scale_linetype_manual( 
    values = c("MCID" = "solid", "nonMCID" = "longdash"), 
    labels = c("MCID" = "|change ≥ 0.5|", "nonMCID" = "|change < 0.5|") 
  ) + 
  
  # count observations - optional 
  geom_count( 
    aes( 
      size = after_stat(prop), 
      alpha = after_stat(prop), 
      group = factor(1) 
    ), 
    fill = "gray50", 
    color = "gray50", 
    show.legend = FALSE 
    
  ) + # I don't want these fractions to be printed, just shown 
  scale_size_area( 
    max_size = 5.5, 
    name = "Fraction of observations" 
  ) + 
  
  scale_alpha( 
    range = c(0.6, 1), 
    guide = "none" 
  ) + 
  
  # Draw boxplots - left and right 
  geom_half_boxplot( 
    data = . %>% filter(Timepoint == "Baseline"), 
    aes(group = Timepoint, x = Timepoint, y = Value), 
    position = position_nudge(x = -0.13), 
    side = "r", 
    outlier.shape = NA, 
    center = FALSE, 
    errorbar.draw = FALSE, 
    width = .14, 
    size = 0.7, 
    col = "gray30" 
  ) + 
  geom_half_boxplot( 
    data = . %>% filter(Timepoint == "Week 8"), 
    aes(group = Timepoint, x = Timepoint, y = Value), 
    position = position_nudge(x = .13), 
    side = "l", 
    outlier.shape = NA, 
    center = FALSE, 
    errorbar.draw = FALSE, 
    width = .14, 
    size = 0.7, 
    col = "gray30" 
  ) + 
  
  # Draw mean +- SD 
  stat_summary( 
    data = . %>% filter(Timepoint == "Baseline"), 
    aes(group = Timepoint, x = Timepoint, y = Value, fill = "Mean ± SD"), 
    col = "black", 
    position = position_nudge(x = -0.18), 
    fun = mean 
  ) + 
  stat_summary( 
    data = . %>% filter(Timepoint == "Week 8"), 
    aes(group = Timepoint, x = Timepoint, y = Value, fill = "Mean ± SD"), 
    col = "black", 
    position = position_nudge(x = 0.18), 
    fun = mean 
  ) + 
  stat_summary( 
    data = . %>% filter(Timepoint == "Baseline"), 
    aes(group = Timepoint, x = Timepoint, y = Value), 
    col = "black", 
    position = position_nudge(x = -0.18), 
    fun.data = mean_sdl, 
    geom = "errorbar", 
    width = 0.02, 
    fun.args = list(mult = 1) 
  ) + 
  stat_summary( 
    data = . %>% filter(Timepoint == "Week 8"), 
    aes(group = Timepoint, x = Timepoint, y = Value), 
    col = "black", 
    position = position_nudge(x = 0.18), 
    fun.data = mean_sdl, 
    geom = "errorbar", 
    width = 0.02, 
    fun.args = list(mult = 1) 
  ) + 
  
  # Draw densities - left and right 
  
  geom_half_violin( 
    data = . %>% filter(Timepoint == "Baseline"), 
    aes(group = Timepoint, x = Timepoint, y = Value), 
    position = position_nudge(x = -0.15), 
    side = "l", 
    fill = "gray40", 
    alpha = 0.4, 
    col = NA 
  ) + 
  geom_half_violin( 
    data = . %>% filter(Timepoint == "Week 8"), 
    aes(group = Timepoint, x = Timepoint, y = Value), 
    position = position_nudge(x = .15), 
    side = "r", 
    fill = "gray40", 
    alpha = 0.4, 
    col = NA 
  ) + 
  theme_bw() + 
  labs( 
    col = "Kind of change: ", 
    linetype = "Magnitude of change", 
    linewidth = "Magnitude of change", 
    fill = "Measures:" 
  ) + 
  theme( 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank() 
  ) + 
  labs( 
    title = "Change in XYZ Score Between Baseline and Week 8", 
    subtitle = paste0("Limited to ", nrow(scores_bas_w8) / 2, " paired observations") 
  ) + 
  xlab(NULL) + 
  ylab("XYZ") + 
  theme( 
    axis.text = element_text(size = 13), 
    axis.title = element_text(size = 14), 
    legend.text = element_text(size = 11), 
    legend.title = element_text(size = 11), 
    legend.key.size = unit(1, "line"), 
    legend.spacing.y = unit(0.3, "line") 
  ) 
