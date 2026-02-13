library(tidyverse)
library(here)
library(brms)
library(tidybayes)

# Plot TRAP realisation by school
qb2 |> 
  # Count how many individuals went to each school
  group_by(school) |> 
  mutate(
    student_n = length(unique(part_id))
  ) |> 
  # remove schools with fewer than 3 students.
  filter(student_n >= 3) |> 
  ungroup() |> 
  # Get the mean values for each speaker.
  group_by(part_id) |> 
  summarise(
    F1_lob2 = mean(F1_lob2, na.rm=T),
    school = first(school)
  ) |> 
  # Plot F1 by school.
  ggplot(
    aes(
      y = F1_lob2,
      x = reorder(school, F1_lob2)
    )
  ) +
  geom_boxplot() +
  theme(
    axis.text.x = element_text(size=8, angle = 45, hjust = 1)
  ) +
  labs(
    y = "TRAP F1 (Lobanov)",
    x = "High school"
  )

# Exercise: Do some more plots and decide if there are any major confounders
# here in terms of, say, gender or age of participants.

# Restrict data to two schools.
trap_sub <- qb2 |> 
  filter(
    school %in% c(
      "Avonside Girls' High School", 
      "St Margaret's College"
    )
  ) |> 
  select(
    age, school, F1_lob2, part_id
  )

# Fit a non-Bayesian model
trap_fit <- lm(
  F1_lob2 ~ school,  
  data = trap_sub
)

# Exercise: spend a minute or two looking at this model, with whatever tools
# you would usually use.

# Fit a Bayesian model
trap_fit_b <- brm(
  F1_lob2 ~ school,  
  data = trap_sub
)

summary(trap_fit_b)

# Plot the intercept coefficient, i.e., the expected formant value when
# the school is Avonside.
trap_fit_b |> 
  spread_draws(b_Intercept) |> 
  rename(
    Avonside = b_Intercept
  ) |> 
  ggplot(
    aes(
      x = Avonside
    )
  ) +
  geom_density(alpha=0.4, colour="darkgreen", fill="darkgreen") +
  labs(
    x = "Avonside coefficient (intercept)"
  )

# Let's look at the same with some interval ranges using `geom_halfeyeh()`
trap_fit_b |> 
  spread_draws(b_Intercept) |> 
  rename(
    Avonside = b_Intercept
  ) |> 
  ggplot(
    aes(
      x = Avonside
    )
  ) +
  geom_halfeyeh(
    slab_alpha = 0.6,
    fill = "darkgreen",
    .width = c(0.5, 0.7, 0.9),
    point_interval = "mean_qi",
    interval_size_range = c(2, 4)
  ) +
  labs(
    x = "Avonside coefficient (intercept)"
  )

## Exercise: try a different set of intervals. To do this, modify the `.width`
## argument to `geom_halfeyeh()`

# Let's make some predictions for these schools
predictions <- tibble(
    # We're creating a data frame with the values we want predictions for.
    school = unique(trap_sub$school)
  ) |> 
  add_epred_draws(trap_fit_b) 

# We'll plot the predicted values for each school.
predictions |> 
  ggplot(
    aes(
      colour = school,
      fill = school,
      x = .epred
    )
  ) +
  geom_density(alpha = 0.4)


# And now the _difference_ between predictions for each school
predictions |> 
  pivot_wider(
    names_from = school,
    values_from = .epred,
    id_cols = .draw
  ) |> 
  mutate(
    Difference = `St Margaret's College` -  `Avonside Girls' High School`
  ) |> 
  ggplot(
    aes(
      x = Difference
    )
  ) +
  geom_density(alpha = 0.4, fill = "grey") +
  geom_vline(xintercept = 0, linetype="dashed")

## Exercise: have a look at the summaries for our models. Is this distribution
## essentially the same as the distribution for the St Margaret's coefficient?
## Modify the following code to find out visually:

## HINT: find out what the name of the variable is using 
# `get_variables(trap_fit_b)`. Replace `b_Intercept` in the following code 
# with the variable name you think is right.
trap_fit_b |> 
  spread_draws(b_Intercept) |> 
  rename(
    Avonside = b_Intercept
  ) |> 
  ggplot(
    aes(
      x = Avonside
    )
  ) +
  geom_halfeyeh(
    slab_alpha = 0.6,
    fill = "darkgreen",
    .width = c(0.5, 0.7, 0.9),
    point_interval = "mean_qi",
    interval_size_range = c(2, 4)
  ) +
  labs(
    x = "Avonside coefficient (intercept)"
  )


## Exercise: Rerun analysis above using two different schools.
## Exercise: See what happens if you add age to the model (there are a few
## options for this).
