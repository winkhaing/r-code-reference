install.packages("gcookbook")
library(gcookbook)
library(ggplot2)
heightweight[, c("ageYear", "heightIn")]

hw <- heightweight
ggplot(heightweight, aes(x = ageYear, y = heightIn)) + geom_point()
ggplot(heightweight, aes(x = ageYear, y = heightIn)) + geom_point(shape = 21)
ggplot(heightweight, aes(x = ageYear, y = heightIn)) + geom_point(size = 1.5)


# Grouping Data Points by a Variable Using Shape or Color

ggplot(heightweight, aes(x = ageYear, y = heightIn, shape = sex, colour = sex)) +
 geom_point()

ggplot(heightweight, aes(x = ageYear, y = heightIn, shape = sex, colour = sex)) +
 geom_point() +
 scale_shape_manual(values = c(1,2)) +
 scale_colour_brewer(palette = "Set1")

# Use slightly larger points and use a shape scale with custom values
ggplot(heightweight, aes(x = ageYear, y = heightIn, shape = sex)) +
 geom_point(size = 3) +
 scale_shape_manual(values = c(1, 4))


# Make a copy of the data
hw <- heightweight
# Categorize into <100 and >=100 groups
hw$weightGroup <- cut(hw$weightLb, breaks = c(-Inf, 100, Inf),
 labels = c("< 100", ">= 100"))

# Use shapes with fill and color, and use colors that are empty (NA) and 
# filled
ggplot(hw, aes(x = ageYear, y = heightIn, shape = sex, fill = weightGroup)) +
 geom_point(size = 2.5) +
 scale_shape_manual(values = c(21, 24)) +
 scale_fill_manual(values = c("red", "black"),
 guide = guide_legend(override.aes = list(shape = 21)))

