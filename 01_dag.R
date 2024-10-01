
library(ggdag)
library(dagitty)
library(ggplot2)

bigger_dag <- dagify(y ~ x + a + b,
                     x ~ a + b,
                     exposure = x,
                     outcome = y)
#  automatically searches the paths between the variables labelled exposure and
#  outcome
dag_paths(bigger_dag)


dag <- dagitty("dag{y <- z -> x}")
tidy_dagitty(dag)

dagified <- dagify(race_ethnicity ~ urban,
                   urban ~ population_size,
                   major_roads ~ urban,
                   industrial_activity ~ urban,
                   monitors ~ population_size,
                   monitors ~ environmental_racism,
                   pollution_sources ~ environmental_racism,
                   environmental_racism ~ race_ethnicity,
                   major_roads ~ pollution_sources,
                   industrial_activity ~ pollution_sources,
                   monitors ~ race_ethnicity,
                   exposure = "race_ethnicity",
                   outcome = "monitors")

tidy_dagitty(dagified)

ggdag(dagified, layout = "circle")
ggdag_parents(dagified, "race_ethnicity")

dagified %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_point() +
  geom_dag_edges_arc() +
  geom_dag_text() +
  theme_dag()

ggdag_adjustment_set(dagified)





