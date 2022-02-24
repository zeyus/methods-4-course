library("rethinking")

data(foxes)

head(foxes)
plot(foxes$area, foxes$weight)
hist(foxes$area, breaks = 50)
hist(log(foxes$area), breaks = 50)
length(unique(foxes$group))
hist(foxes$weight, breaks = 50)
summary(foxes)
# not useful
hist(log(foxes$weight), breaks = 50)
# predict the weight of a fox by its area
# create priors

sd(foxes$weight)

xbar <- mean(foxes$area)

n_sample <- 1e4

sample_sigma <- runif(n_sample, 0, 2)
sample_a <- runif(n_sample, 0, 0)
sample_b <- rnorm(n_sample, 0.2, 2)
sample_mu <- sample_a + sample_b * (rnorm(n_sample, 0, 0.5))
prior_y <- rnorm(n_sample, sample_mu, sample_sigma)
head(prior_y)
dens(prior_y)


foxes$std_weight <- scale(foxes$weight)
plot(foxes$std_weight)
fox_model_w_area <- quap(
  alist(
    std_weight ~ dnorm(mu, sigma),
    mu <- a + b * area,
    a ~ dnorm(0, 0.2),
    b ~ dnorm(0.2, 2),
    sigma ~ dunif(0, 2)
  ), data = foxes
)

precis(fox_model_w_area)

summary(fox_model_w_area)

prior_weight <- normal(0, 1)
prior_area <- normal(0, 1)

