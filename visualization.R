library(readr)
library(readxl)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggrepel)

# Read in data
df <- read_excel('MCA coord.xlsx', skip = 2)
names(df) <- c('item', 'x', 'y')

# Interpolate categories
df$category_dummy <- is.na(df$x)

# Go through each line. If not a category dummy,
# Get the category
df$category <- NA
for (i in 1:nrow(df)){
  print(i)
  the_line <- i
  while(!df$category_dummy[the_line]){
    the_line <- the_line - 1
  }
  df$category[i] <- df$item[the_line]
}

# Remove the category_dummy rows
df <- df[!df$category_dummy,]

# Remove the category_dummy column
df$category_dummy <- NULL

# Remove trailing underscores
for (i in 1:nrow(df)){
  item <- df$item[i]
  last_is_underscore <- 
    substr(item, nchar(item), nchar(item)) == '_'
  if(last_is_underscore){
    df$item[i] <-
      substr(item, 1, nchar(item) - 1)
  }
}

# Clean up names of items
df$item <-
  gsub("*_", "", df$item, perl = TRUE)
df$item <- 
  gsub('_', '\n', df$item)
df$item <-
  gsub('/', '/\n', df$item)

# Define length of item over 1 (for labeling)
# df$size <- 1/ nchar(df$item) 

# Define colors
cols <- colorRampPalette(brewer.pal(8, 'Dark2'))(length(unique(df$category)))

# Now plot
ggplot(data = df,
       aes(x = x, 
           y = y,
           color = category)) +
  geom_hline(yintercept = 0, alpha = 0.3) +
  geom_vline(xintercept = 0, alpha = 0.3) +
  geom_point(size = 3, alpha = 0.6) +
  scale_color_manual(values = cols,
                     name = 'Category') +
  xlab('Dimension 1') +
  ylab('Dimension 2') +
  geom_label_repel(aes(x, y, 
                      label = item),
                   box.padding = unit(0.1, 'lines'),
                   label.padding = unit(0.1, "lines"), 
                   point.padding = unit(1e-06, "lines"),
                   label.r = unit(0.1, "lines"),
                   label.size = 0.1,
                   alpha = 0.8,
                   show.legend = FALSE,
                   size = 3) 
ggsave('visualization.pdf',
       height = 8.5,
       width = 11)