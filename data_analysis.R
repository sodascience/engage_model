library(tidyverse)
library(marginaleffects)

# loading the data
df <- read_csv("synthetic_data.csv", col_select = 2:last_col())


# a little bit of exploratory data analysis on GAD
df_gad <- 
  df |>
  select(starts_with("GAD"), IDnumber, Content) |>
  pivot_longer(starts_with("GAD"),
               names_prefix = "GAD_7_TOTAL_",
               names_to = "timepoint") |>
  summarize(gad_7 = first(value), content = first(Content), .by = c(IDnumber, timepoint))

pos <- position_dodge(width = 0.2)

df_gad |>
  ggplot(aes(x = timepoint, y = gad_7, group = IDnumber)) +
  geom_line(position = pos, alpha = 0.2) +
  geom_point(position = pos) +
  theme_minimal()

  
df_gad |> summarize(
  mean = mean(gad_7, na.rm = TRUE),
  sd = sd(gad_7, na.rm = TRUE),
  .by = "timepoint"
)

df_wide <- df |> mutate(
    IDnumber = as_factor(IDnumber),
    timepoint = `T` - 1,
    .keep = "unused"
  ) |>
  pivot_wider(
    names_from = timepoint,
    values_from = c(ends_with("Engagement")),
    names_sep = "_T"
  )


# QUESTION 1: does the intervention have an effect?
# TODO: add demographics
frm_intervention <- GAD_7_TOTAL_T1 ~ 1 + GAD_7_TOTAL_T0 + Content + Design + Feedback
fit_intervention <- lm(frm_intervention, data = df_wide)

# overall effect of intervention components
anova(fit_intervention)

# let's look at the coefficients
summary(fit_intervention)

# Let's use marginaleffects package to inspect
plot_predictions(fit_intervention, by = "Content")
plot_predictions(fit_intervention, by = c("GAD_7_TOTAL_T0", "Content")) +
  theme_minimal() +
  labs(title = "Pre-post marginal predictions by type", 
       x = "Pre-measurement", 
       y = "Post-measurement")
ggsave("plot.png", width = 6, height = 4, bg = "white")


# Full model with interactions
frm_intervention_all <- GAD_7_TOTAL_T1 ~ 1 + GAD_7_TOTAL_T0 + Content * Design * Feedback
fit_intervention_all <- lm(frm_intervention_all, data = df_wide)

# overall effect of intervention components
anova(fit_intervention_all)
# let's look at the coefficients
summary(fit_intervention_all)


plot_predictions(fit_intervention, by = c("Content", "Feedback", "Design"))



# EXTRA QUESTION: does the intervention have an effect?
frm_interaction <- GAD_7_TOTAL_T1 ~ 1 + GAD_7_TOTAL_T0 * Content
fit_interaction <- lm(frm_interaction, data = df_wide)
plot_predictions(fit_interaction, by = c("GAD_7_TOTAL_T0", "Content"))
summary(fit_interaction)



# QUESTION 2: is there an effect of engagement?
df_wide |> 
  summarise(
    behavior  = mean(behaviorEngagement_T1 , na.rm = TRUE),
    affective = mean(affectiveEngagement_T1, na.rm = TRUE),
    cognitive = mean(cognitiveEngagement_T1, na.rm = TRUE),
    behavior_sd  = sd(behaviorEngagement_T1 , na.rm = TRUE),
    affective_sd = sd(affectiveEngagement_T1, na.rm = TRUE),
    cognitive_sd = sd(cognitiveEngagement_T1, na.rm = TRUE),
    .by = Content
  )

frm_engagement <- GAD_7_TOTAL_T1 ~ 1 + GAD_7_TOTAL_T0 + Content + Design + Feedback + behaviorEngagement_T1 + cognitiveEngagement_T1 + affectiveEngagement_T1
fit_engagement <- lm(frm_engagement, data = df_wide)

frm_engagement_only <- GAD_7_TOTAL_T1 ~ 1 + GAD_7_TOTAL_T0 + behaviorEngagement_T1 + cognitiveEngagement_T1 + affectiveEngagement_T1
fit_engagement_only <- lm(frm_engagement_only, df_wide)

# Refit intervention model without engagement missing values
fit_interv_sub <- lm(frm_intervention, data = df_wide, subset = !is.na(df_wide$behaviorEngagement_T1))

# compute if there is an effect of these engagement variables
anova(fit_interv_sub, fit_engagement)

# let's take a look at engagement
summary(fit_engagement)

plot_predictions(fit_engagement, by = c("affectiveEngagement_T1"))
plot_predictions(fit_engagement, by = c("GAD_7_TOTAL_T0", "affectiveEngagement_T1"))




