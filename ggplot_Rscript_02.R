# ggplot2 R Script ==============================
# EXERCISE: MAKE A barplot 
# of the mean body_mass_g
# BY sex and year
# with error bars ======
library(palmerpenguins)
library(ggplot2)
library(dplyr)
data("penguins")

# see example at 
# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

# components needed
# means of body mass by sex
# and value to use for +/- for 
# min, max of each error bar
# could be sd, se, or ci

# a helpful function, summarySE ======================
# summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <-
  function(data = NULL,
           measurevar,
           groupvars = NULL,
           na.rm = FALSE,
           conf.interval = .95,
           .drop = TRUE) {
    library(plyr)
    
    # New version of length which can handle NA's: 
    # if na.rm==T, don't count them
    length2 <- function (x, na.rm = FALSE) {
      if (na.rm)
        sum(!is.na(x))
      else
        length(x)
    }
    
    # This does the summary. For each group's data frame, 
    # return a vector with
    # N, mean, and sd
    datac <- ddply(
      data,
      groupvars,
      .drop = .drop,
      .fun = function(xx, col) {
        c(
          N    = length2(xx[[col]], na.rm = na.rm),
          mean = mean   (xx[[col]], na.rm = na.rm),
          sd   = sd     (xx[[col]], na.rm = na.rm)
        )
      },
      measurevar
    )
    
    # Rename the "mean" column
    datac <- rename(datac, c("mean" = measurevar))
    
    datac$se <-
      datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
    
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval:
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval / 2 + .5, datac$N - 1)
    datac$ci <- datac$se * ciMult
    
    return(datac)
  }

# look at the global environment
# see functions

# get the summary stats from penguins dataset
penguins_bm <- 
  summarySE(penguins, 
            measurevar="body_mass_g", 
            groupvars=c("sex","year"))

# Use 95% confidence intervals 
ggplot(penguins_bm, 
       aes(x=year, y=body_mass_g, fill=sex)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=body_mass_g-ci, 
                    ymax=body_mass_g+ci),
                width=.2,                # Width of the error bars
                position=position_dodge(.9))

# filter out NAs and redo plot
penguins_bmc <- penguins_bm %>%
  filter(complete.cases(.))

# Use 95% confidence intervals 
ggplot(penguins_bmc, 
       aes(x=year, y=body_mass_g, fill=sex)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=body_mass_g-ci, 
                    ymax=body_mass_g+ci),
                width=.2,                # Width of the error bars
                position=position_dodge(.9))


# EXERCISE - YOUR TURN ==================================
# Make clustered bar plot
# for flipper_length_mm
# by year and species
# use sd (standard deviation) for error bars





# Stacked Proportional Bar Chart
# see examples at https://r-graphics.org/recipe-bar-graph-proportional-stacked-bar
# Make proportional bar chart
# of sex by island
# We need "counts" of each first
penguins_tab1 <-
  table(penguins$sex,
        penguins$island)

# make it a data.frame
penguins_tab1.df <- data.frame(penguins_tab1)

# change names
names(penguins_tab1.df) <-
  c("sex", "island", "count")

ggplot(penguins_tab1.df, 
       aes(x = sex, y = count, fill = island)) +
  geom_col(position = "fill")

# make a little better
ggplot(penguins_tab1.df, 
       aes(x = sex, y = count, fill = island)) +
  geom_col(colour = "black", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    x = "Biological Sex",
    y = "Relative Percent",
    title = "Percentage of Island by Biological Site",
    fill = "Island Surveyed"
  )
  

# EXERCISE - YOUR TURN ==================================
# Make proportional bar chart
# of species by island



