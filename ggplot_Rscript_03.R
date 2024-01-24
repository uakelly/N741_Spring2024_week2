# Arranging Plots with ggarrange ================
# By Madelyn Houser, 01/23/2024
# Example 1 =====================================
library(palmerpenguins)
library(ggpubr)

# save each plot as an object
p1 <- ggplot(penguins,
             aes(x = bill_length_mm,
                 y = bill_depth_mm,
                 color = species)) +
  geom_point()

p2 <- ggplot(penguins,
             aes(x = flipper_length_mm,
                 y = bill_length_mm,
                 color = species)) +
  geom_point()

p3 <- ggplot(penguins,
             aes(x = body_mass_g,
                 y = flipper_length_mm,
                 color = species)) +
  geom_point()

p4 <- ggplot(penguins,
             aes(x = body_mass_g,
                 y = bill_length_mm,
                 color = species)) +
  geom_point()

# arrange plots as you like
ggarrange(
  p1,
  p2,
  p3,
  p4,
  nrow = 2,
  ncol = 2,
  common.legend = TRUE,
  legend = "right"
)

# Example 2 =====================================
library(palmerpenguins)
library(ggpubr)

# save each plot as an object
p1 <-
  ggplot(penguins,
         aes(x = bill_length_mm,
             y = bill_depth_mm,
             color = species)) +
  geom_point()

p2 <-
  ggplot(penguins, aes(x = species,
                       y = bill_length_mm)) +
  geom_boxplot() +
  xlab("Species") +
  ylab("Bill Depth (mm)")

p3 <-
  ggplot(penguins,
         aes(x = flipper_length_mm)) +
  geom_histogram(
    aes(y = ..density..),
    binwidth = 2,
    colour = "black",
    fill = "white"
  ) +
  geom_density(alpha = .2, fill = "#FF6666") +
  facet_wrap(vars(species))

# arrange plots as you like
ggarrange(
  ggarrange(p1, p2, widths = c(2, 1)),
  p3, nrow = 2, ncol = 1)
