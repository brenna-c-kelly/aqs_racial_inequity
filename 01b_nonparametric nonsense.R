
library(INLA)
library(SemiPar)

data(lidar)

xx <- seq(390, 720, by = 5)
# Add data for prediction
new.data <- cbind(range = xx, logratio = NA)
new.data <- rbind(lidar, new.data)
m.poly <- inla(logratio ~ 1 + range +  I(range^2),
                 data = new.data, control.predictor = list(compute = TRUE))

library("splines")

acs <- read.csv("/Users/brenna/Downloads/7020_data_count_poll.csv")

knots <- seq(400, 700, by = 10)
m.bs3 <- inla(n_upper_pm ~ 1 + ns(black_perc, df = 2),
              data = acs, control.predictor = list(compute = TRUE))
summary(m.bs3)

library(quantregGrowth)
quantiles_lin <- gcrq(n_upper_pm ~ black_perc, 
                      data = acs, tau = c(0.1, 0.25, 0.5, 0.75, 0.9))
plot(acs$black_perc, acs$n_upper_pm)
plot(quantiles_lin, add = TRUE)
plot(quantiles_lin, term = "black_perc",
     axis.tau = TRUE, conf.level = .95, col = 2)


ggplot(acs, aes(y = n_upper_pm, x = asian_perc)) +
  geom_point(alpha = 0.1, color = "black") + 
  theme_minimal() +
  stat_smooth(method = "lm", 
              formula = y ~ poly(x, df = 3), 
              lty = 1, col = "darkgoldenrod1") + # odd behavior at the tails
  stat_smooth(method = "lm", 
              formula = y ~ bs(x, df = 3), 
              lty = 1, col = "darkorchid3") + # extrapolates, but otherwise similar to ns()
  stat_smooth(method = "lm", 
              formula = y ~ ns(x, df = 3), 
              lty = 1, col = "firebrick1") + # well-behaved
  labs(title = paste0("Splines for Gestational Age", " df = ", 3),
       xlab = "Birth weight (g)",
       ylab = "Gestational age (wk)",
       caption = "bs() cannot take df < 3 and begins at 3")



