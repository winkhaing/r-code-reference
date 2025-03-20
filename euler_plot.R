install.packages("eulerr")
library(eulerr)
zz <- plot(
  euler(
  c(
  "LUTS"=98,
  "Symptoms of UTI"=46,
  "Positive urine culture"=39,
  "Symptoms of UTI&LUTS"=33,
  "LUTS&Positive urine culture"=24,
  "Symptoms of UTI&Positive urine culture"=22,
  "Symptoms of UTI&LUTS&Positive urine culture"=18
  ),
  input = "union", shape = "ellipse"
  ),
  key = TRUE,
  counts = TRUE,
  quantities = list(type = c('counts',"percent")),
  fills =list(fill=c(viridis::plasma(n = 3))),
  alpha = 0.3, c("#1957FF", "#FF750C", "#FF220C"),
  alpha = 0.3,
  edges=list(lty = 1),
  factor_names = TRUE,
  labels=list(font=2, cex=1),
  legend = FALSE
  )
zz

zz$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.1$children$tag.quantity.1$label <- 'Deep'
zz$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.2$children$tag.quantity.2$label <- 'Very deep'
zz$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.3$children$tag.quantity.3$label <- 'tired'
zz$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.4$children$tag.quantity.4$label <- 'How much more?'
zz$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.5$children$tag.quantity.5$label <- 'Infinity'
zz$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.6$children$tag.quantity.6$label <- 'Aggrh!'
zz$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.7$children$tag.quantity.7$label <- 'R.I.P.'
zz



fit <- euler(c("A" = 10, "B" = 5, "A&B" = 3))
plot(fit, fill_opacity = 0.7)

# Change to italic roman font, remove borders and switch colors
plot(fit, fill = c("dodgerblue4", "darkgoldenrod1"), lwd = 0,
     fontface = "italic")

# Add counts to the plot
plot(fit, counts = TRUE)

# Add a custom legend and retain counts
plot(fit, counts = TRUE, key = list(space = "bottom", columns = 2))

# Plot without fills and distinguish sets with border types instead
plot(fit, lty = c("solid", "dotted"), fill = "transparent", cex = 2,
     fontface = 2, labels = c("foo", "bar"))

# Plot a grid of euler plots
dat <- data.frame(
  A      = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
  B      = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
  gender = sample(c("Men", "Women"), size = 100, replace = TRUE),
  nation = sample(c("Sweden", "Denmark"), size = 100, replace = TRUE)
)

e_grid <- euler(dat[, 1:2], by = dat[, 3:4])
plot(e_grid, key = TRUE)

# We can modify the grid layout as well
plot(e_grid, layout = c(1, 4))
