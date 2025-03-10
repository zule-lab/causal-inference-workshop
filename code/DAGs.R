# code for the DAGs used in the slides

library(ggdag)


# forks -------------------------------------------------------------------


fork_dag <- dagify(
  temp ~ canopy + SVF,
  canopy ~ SVF,
  labels = c(
    "temp" = "Temperature",
    "canopy" = "Canopy Cover",
    "SVF" = "Sky View Factor"
  ),
  coords = list(x = c(canopy = -1, SVF = 0, temp = 1),
                y = c(canopy = 1, SVF = 0, temp = 1)),
  exposure = "canopy",
  outcome = "temp"
)


ggdag(fork_dag, text = FALSE, use_labels = "label") + theme_dag()


ggdag_adjustment_set(fork_dag, effect = "direct", text = FALSE, use_labels = "label", shadow = TRUE) +
  theme_dag()

temp_model <- lmer(temperature ~ canopy + SVF + (1|date),
                   data = temp_df)



# pipes -------------------------------------------------------------------
pipe_dag <- dagify(
  temp ~ canopy + SVF,
  canopy ~ SVF,
  labels = c(
    "temp" = "Temperature",
    "canopy" = "Canopy Cover",
    "SVF" = "Sky View Factor"
  ),
  coords = list(x = c(canopy = -1, SVF = 0, temp = 1),
                y = c(canopy = 1, SVF = 0, temp = 1)),
  exposure = "SVF",
  outcome = "temp"
)

ggdag(pipe_dag, text = FALSE, use_labels = "label") + theme_dag()


ggdag_adjustment_set(pipe_dag, effect = "total", text = FALSE, use_labels = "label", shadow = TRUE) +
  theme_dag()



temp_model <- lmer(temperature ~ SVF + (1|date),
                   data = temp_df)



# colliders ---------------------------------------------------------------


collider_dag <- dagify(
  func ~ public + private,
  labels = c(
    "func" = "Urban Forest Functional Diversity",
    "public" = "Public Tree Species Richness",
    "private" = "Private Tree Species Richness"
  ),
  coords = list(x = c(public = -1, func = 0, private = 1),
                y = c(public = 0, func = 0, private = 0))
)

ggdag(collider_dag, text = FALSE, use_labels = "label") + theme_dag()



# descendants -------------------------------------------------------------

descendant_dag <- dagify(
  bird ~ native,
  native ~ land,
  invasive ~ native,
  labels = c(
    "bird" = "Bird Behaviour",
    "native" = "Proportion Native Trees",
    "invasive" = "Proportion Invasive Trees",
    "land" = "Land Use Type"
  ),
  coords = list(x = c(land = -1, native = 0, invasive = 0, bird = 1),
                y = c(land = 0.5, native = 0.5, invasive = 0, bird = 0.75))
)

ggdag(descendant_dag, text = FALSE, use_labels = "label") + theme_dag()
