# This code creates an example Nested Column Chart of the type developed by Brittany Rosenau
# this code will produce the graphic below if you paste it into R Studio and run it
# to run it in a Power BI R visual, just import the csv into Power Query
# and then paste in the code below, changing df <- dataset
# note that "dataset" is the variable name used by convention in Power BI to call the table 
# resulting from the previous step

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr) # To read the CSV data directly from text

# Recreate the data from the provided CSV text
csv_data <- "Region,Segment,Revenue
West,Consumer,364
West,Corporate,232
West,Home Office,143
East,Consumer,357
East,Corporate,204
East,Home Office,131
Central,Consumer,254
Central,Corporate,158
Central,Home Office,91
South,Consumer,196
South,Corporate,122
South,Home Office,74"

# Read the data into a dataframe
df <- read_csv(csv_data)

# Ensure Region is a factor with the desired order (West, East, Central, South)
df$Region <- factor(df$Region, levels = c("West", "East", "Central", "South"))

# Ensure Segment is a factor with the desired order for legend/coloring
df$Segment <- factor(df$Segment, levels = c("Consumer", "Corporate", "Home Office"))

# Calculate total revenue per region to position total labels and grey background bars
region_totals <- df %>%
  group_by(Region) %>%
  summarise(TotalRevenue = sum(Revenue), .groups = "drop") # Use .groups='drop' to avoid grouped df issues

# Define custom colors matching the image
segment_colors <- c(
  "Consumer" = "#2ECC71", # Green
  "Corporate" = "#F39C12", # Orange (or slightly softer like #F5B041)
  "Home Office" = "#3498DB"
) # Blue

# Create the plot
p <- ggplot() +
  # Layer 1: Grey background bars representing total revenue per region
  # Plotted first to be in the background
  geom_col(
    data = region_totals,
    aes(x = Region, y = TotalRevenue),
    fill = "grey85", # Light grey fill
    alpha = 0.8, # Slightly transparent
    width = 0.8
  ) + # Adjust width if needed
  
  # Layer 2: Dodged bars for segment revenue
  geom_col(
    data = df,
    aes(x = Region, y = Revenue, fill = Segment),
    position = position_dodge(width = 0.8), # Dodge bars side-by-side
    width = 0.7
  ) + # Make dodged bars slightly narrower than grey bar
  
  # Layer 3: Labels for individual segment bars ($ Value K)
  geom_text(
    data = df,
    aes(x = Region, y = Revenue, label = paste0("$", Revenue, "K"), group = Segment),
    position = position_dodge(width = 0.8), # Match dodging of bars
    vjust = -0.5, # Position label above the bar
    size = 2.75, # Adjust text size
    family = "sans"
  ) + # Consistent font
  
  # Layer 4: Labels for total region revenue ($ Value K)
  geom_text(
    data = region_totals,
    aes(x = Region, y = TotalRevenue, label = paste0("$", TotalRevenue, "K")),
    vjust = -2.0, # Position above the grey bar, adjust vertical position
    size = 3.5, # Adjust text size
    family = "sans"
  ) + # Consistent font
  
  # Layer 5: Labels for Region Names (West, East, Central, South)
  geom_text(
    data = region_totals,
    aes(x = Region, y = TotalRevenue, label = as.character(Region)), # Ensure label is character
    vjust = -3.5, # Position above the total revenue label
    size = 4, # Adjust text size
    fontface = "bold", # Make region name bold
    family = "sans"
  ) + # Consistent font
  
  # Apply custom color scale
  scale_fill_manual(values = segment_colors) +
  
  # Set plot title
  ggtitle("Total Sales by Region and Segment ($K)") +
  
  # Customize the theme to match the image (minimalist, no axes/grid)
  theme_void(base_size = 12, base_family = "sans") + # Start with a blank theme, set base font
  theme(
    # Plot Title Style
    plot.title = element_text(
      hjust = 0, # Left-align title
      face = "bold", # Bold title text
      size = 16, # Title font size
      margin = margin(b = 15)
    ), # Margin below title
    
    # Legend Style
    legend.position = "top", # Legend at the top
    legend.justification = "left", # Left-align legend items
    legend.title = element_blank(), # No legend title
    legend.text = element_text(size = 11), # Legend text size
    legend.key.size = unit(0.5, "cm"), # Size of color keys in legend
    legend.margin = margin(b = -10), # Reduce space below legend
    
    # Remove all axis elements (lines, ticks, text) as they are not in the image
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    
    # Add margin around the plot
    plot.margin = margin(20, 20, 20, 20) # Top, Right, Bottom, Left margins
  ) +
  
  # Adjust y-axis limits to provide space for labels above the bars
  # Calculate max height needed (max total revenue + buffer for labels)
  # Limit calculation ensures enough space for the highest label (Region Name)
  scale_y_continuous(
    limits = c(0, max(region_totals$TotalRevenue) * 1.35), # ~35% extra space at top
    expand = expansion(mult = c(0, 0))
  ) # Start y-axis at 0

# Print the plot
print(p)
