install.packages("ggblend")
install.packages("ggdist")
library(ggplot2)
library(ggblend)
theme_set(ggdist::theme_ggdist() + theme(
  plot.title = element_text(size = rel(1), lineheight = 1.1, face = "bold"),
  plot.subtitle = element_text(face = "italic"),
  panel.border = element_rect(color = "gray75", fill = NA)
))

set.seed(1234)
df_a = data.frame(x = rnorm(500, 0), y = rnorm(500, 1), set = "a")
df_b = data.frame(x = rnorm(500, 1), y = rnorm(500, 2), set = "b")

df_ab = rbind(df_a, df_b) |>
  transform(order = "draw a then b")

df_ba = rbind(df_b, df_a) |>
  transform(order = "draw b then a")

df = rbind(df_ab, df_ba)
df |>
  ggplot(aes(x, y, color = set)) +
  geom_point(size = 3, alpha = 0.5) +
  scale_color_brewer(palette = "Set1") +
  facet_grid(~ order) +
  labs(title = "geom_point() without blending", subtitle = "Draw order matters.")

df |>
  ggplot(aes(x, y, color = set)) +
  geom_point(size = 3, alpha = 0.5) |> blend("multiply") +
  scale_color_brewer(palette = "Set1") +
  facet_grid(~ order) +
  labs(
    title = "geom_point(alpha = 0.5) |> blend('multiply')",
    subtitle = "Draw order does not matter, but color is too dark."
  )

  df |>
    ggplot(aes(x, y, color = set)) +
    geom_point(size = 3, alpha = 0.5) |> partition(vars(set)) |> blend("multiply") +
    scale_color_brewer(palette = "Set1") +
    facet_grid(~ order) +
    labs(
      title = "geom_point(alpha = 0.5) |> partition(vars(set)) |> blend('multiply')",
      subtitle = "Light outside the intersection, but still dark inside the intersection."
    )

df |>
  ggplot(aes(x, y, color = set)) +
  geom_point(size = 3, alpha = 0.5) |> partition(vars(set)) |> blend("lighten") +
  geom_point(size = 3, alpha = 0.5) |> partition(vars(set)) |> blend("multiply", alpha = 0.5) +
  scale_color_brewer(palette = "Set1") +
  facet_grid(~ order) +
      labs(
        title = 
          "geom_point(size = 3, alpha = 0.5) |> partition(vars(set)) |> blend('lighten') + \ngeom_point(size = 3, alpha = 0.5) |> partition(vars(set)) |> blend('multiply', alpha = 0.5)",
        subtitle = 'A good compromise, but a long specification.'
      ) +
      theme(plot.subtitle = element_text(lineheight = 1.2))

    df |>
      ggplot(aes(x, y, color = set, partition = set)) +
      geom_point(size = 3, alpha = 0.5) * (blend("lighten") + blend("multiply", alpha = 0.5)) +
      scale_color_brewer(palette = "Set1") +
      facet_grid(~ order) +
      labs(
        title = "geom_point(aes(partition = set)) * (blend('lighten') + blend('multiply', alpha = 0.5))",
        subtitle = "Two order-independent blends on one layer using the distributive law."
      ) +
      theme(plot.subtitle = element_text(lineheight = 1.2))
