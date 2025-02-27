#loading the libraries
library("tidyverse")

#loading the data sets
allen.data = read_csv("data/essentia.data.allentown.csv")
all.data = read_csv("data/essentia.data.csv")

allen.bounds <- function(feature = "overall_loudness", #Function that finds whether a song feature is similar to the features of Allentown
                         data = all.data) {
  
  summarize.data <- data |>
    mutate(feature = as.numeric(get({feature}))) |> #Gets the feature from the data set
    group_by(artist) |> #Apply for each artists
    summarize( #Finds the minimum, maximum, Lower Fence, and Upper fence for a box plot distribution of a feature
      minimum = min({feature}, na.rm = TRUE), 
      maximum = max({feature}, na.rm = TRUE),
      LF = quantile({feature}, 0.25, na.rm = TRUE) - 1.5 * IQR({feature}, na.rm = TRUE),
      UF = quantile({feature}, 0.75, na.rm = TRUE) + 1.5 * IQR({feature}, na.rm = TRUE),
    ) |>
    mutate( #Determines whether the feature for Allentown is within range, outlying, or out of range for the box plot of the given feature
      allen.feature = get(feature, allen.data),
      out.of.range = ifelse(minimum > allen.feature | maximum < allen.feature, 
                            TRUE,
                            FALSE),
      unusual = ifelse(LF > allen.feature | UF < allen.feature,
                       TRUE,
                       FALSE),
      description = ifelse(unusual == TRUE,
                           ifelse(out.of.range == TRUE,
                                  "Out of Range",
                                  "Outlying"),
                           "Within Range")
    ) |>
    select(-allen.feature) #Remove comparison variable from final data set
  
  return(summarize.data)
}

#These three lines compile a list a numeric vectors in the initial data set, used for analysis
columns.class = sapply(all.data, class) 
columns.not = which(columns.class != "numeric")
columns = str_split(colnames(all.data), " ", simplify = TRUE)[-columns.not,]

#Object setup for getting the box plot descriptions(i.e. Within Range, Outlying, Out of Range)
descriptions = c()
all.descriptions = tibble(artists = c("All Get Out", 
                                      "Manchster Orchestra", 
                                      "The Front Bottoms"))

for(column in columns) { #For each song feature 
  for(index in 1:3) { # For each Artist
    column.analyze = allen.bounds(column)[[8]][index] #Gets box plot descriptions for a spesfic feature for all artists
    descriptions = c(descriptions, column.analyze)
  }
  all.descriptions = cbind(all.descriptions, descriptions) #Adds description for the specific feature (column) to our final data set
  descriptions = c()
}

#These 3 lines of code count the number of features Within Range, Outlying, and Out of Range
in.range.count = rowSums(all.descriptions == "Within Range")
outlying.count = rowSums(all.descriptions == "Outlying")
out.range.count = rowSums(all.descriptions == "Out of Range")

#Storing data in resulting data set
summarized.data <- tibble(
  artists = c("All Get Out", 
              "Manchster Orchestra", 
              "The Front Bottoms"),
  in.range.count = in.range.count,
  outlying.count = outlying.count,
  out.range.count = out.range.count
  ) 

#Column plot for feature in range
ggplot(aes(x = artists,
           y = in.range.count),
       data = summarized.data) +
  geom_col(width = .5) +
  geom_hline(yintercept = 0, color = "red") +
  theme_bw() +
  labs(x = "Artists",
       y = "Count of Traits Within Range",
       title = "Distribution of Artists by Similar Song Traits (to Allentown)")

#Column plot for features that are outlying
ggplot(aes(x = artists,
           y = outlying.count),
       data = summarized.data) +
  geom_col(width = .5) +
  geom_hline(yintercept = 0, color = "red") +
  theme_bw() +
  labs(x = "Artists",
       y = "Count of Traits That Are Marginal",
       title = "Distribution of Artists by Marginal Song Traits (to Allentown)")

#Column plot for feature out of range
ggplot(aes(x = artists,
           y = out.range.count),
       data = summarized.data) +
  geom_col(width = .5) +
  geom_hline(yintercept = 0, color = "red") +
  theme_bw() +
  labs(x = "Artists",
       y = "Count of Differing Traits",
       title = "Distribution of Artists by Differing Song Traits (to Allentown)")





  


