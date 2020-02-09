library(tidyverse)

# we set the seed to make the results of `runif()` reproducible.
set.seed(4)
pos <- 
  replicate(100, runif(16, -1, 1)) %>%        # here's the simulation
  as_tibble() %>%                             # for data manipulation, we'll make this a tibble
  rbind(0, .) %>%                             # here we add a row of zeros above the simulation results
  mutate(step = 0:16) %>%                     # this adds a step index
  gather(key, value, -step) %>%               # here we convert the data to the long format
  mutate(person = rep(1:100, each = 17)) %>%  # this adds a person id index
  # the next two lines allow us to make cumulative sums within each person
  group_by(person) %>%
  mutate(position = cumsum(value)) %>%
  ungroup() 



ggplot(data = pos, 
       aes(x = step, y = position, group = person)) +
  geom_vline(xintercept = c(4, 8, 16), linetype = 2) +
  geom_line(aes(color = person < 2, alpha  = person < 2)) +
  scale_color_manual(values = c("skyblue4", "black")) +
  scale_alpha_manual(values = c(1/5, 1)) +
  scale_x_continuous("step number", breaks = c(0, 4, 8, 12, 16)) +
  theme(legend.position = "none")


pos %>%
  filter(step == 16) %>%
  ggplot(aes(x = position)) +
  stat_function(fun = dnorm, 
                args = list(mean = 0, sd = 2.180408),
                linetype = 2) +  # 2.180408 came from the previous code block
  geom_density(color = "transparent", fill = "dodgerblue3", alpha = 1/2) +
  coord_cartesian(xlim = -6:6) +
  labs(title = "16 steps",
       y     = "density")


tibble(
  x = seq(-4, 4, .1),
  exp_x = exp(x),
  x2 = x^2,
  exp_x2 = exp(-x^2)
) %>%
  ggplot(aes(y = exp_x2, x = x)) +
  geom_line()


library(rethinking)
data(Howell1)
d <- Howell1
rm(Howell1)
detach(package:rethinking, unload = T)
library(brms)


d2 <- 
  d %>%
  filter(age >= 18)


ggplot(data = tibble(x = seq(from = 100, to = 250, by = .1)), 
       aes(x = x, y = dnorm(x, mean = 178, sd = 20))) +
  geom_line() +
  ylab("density")

tibble(x = seq(from = -10, to = 60, by = .1)) %>%
  ggplot(aes(x = x, y = dunif(x, min = 0, max = 50))) +
  geom_line()


n <- 1e4
set.seed(4)
tibble(sample_mu    = rnorm(n, mean = 178,       sd = 20),
       sample_sigma = runif(n, min = 0,         max = 50)) %>% 
  mutate(x          = rnorm(n, mean = sample_mu, sd = sample_sigma)) %>% 
  ggplot(aes(x = x)) +
  geom_density(fill = "black", size = 0) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(subtitle = expression(paste("Prior predictive distribution for ", italic(h[i]))),
       x        = NULL) +
  theme(panel.grid = element_blank())



n <- 200

d_grid <-
  tibble(mu    = seq(from = 140, to = 160, length.out = n),
         sigma = seq(from = 4,   to = 9,   length.out = n)) %>% 
  # we'll accomplish with `tidyr::expand()` what McElreath did with base R `expand.grid()`
  expand(mu, sigma)

grid_function <- function(mu, sigma){
  dnorm(d2$height, mean = mu, sd = sigma, log = T) %>% 
    sum()
}


d_grid <-
  d_grid %>% 
  mutate(log_likelihood = map2(mu, sigma, grid_function)) %>% 
  unnest() %>% 
  mutate(prior_mu       = dnorm(mu,    mean = 178, sd  = 20, log = T),
         prior_sigma    = dunif(sigma, min  = 0,   max = 50, log = T)) %>% 
  mutate(product        = log_likelihood + prior_mu + prior_sigma) %>% 
  mutate(probability    = exp(product - max(product)))


d_grid %>% 
  ggplot(aes(x = mu, y = sigma, z = probability)) + 
  geom_contour() +
  labs(x = expression(mu),
       y = expression(sigma)) +
  coord_cartesian(xlim = range(d_grid$mu),
                  ylim = range(d_grid$sigma)) +
  theme(panel.grid = element_blank())


d_grid %>% 
  ggplot(aes(x = mu, y = sigma)) + 
  geom_raster(aes(fill = probability),
              interpolate = T) +
  scale_fill_viridis_c(option = "A") +
  labs(x = expression(mu),
       y = expression(sigma)) +
  theme(panel.grid = element_blank())


set.seed(4)
d_grid_samples <- 
  d_grid %>% 
  sample_n(size = 1e4, replace = T, weight = probability)

d_grid_samples %>% 
  ggplot(aes(x = mu, y = sigma)) + 
  geom_point(size = .9, alpha = 1/15) +
  scale_fill_viridis_c() +
  labs(x = expression(mu[samples]),
       y = expression(sigma[samples])) +
  theme(panel.grid = element_blank())

d_grid_samples %>% 
  select(mu, sigma) %>% 
  gather() %>% 
  ggplot(aes(x = value)) + 
  geom_density(fill = "grey33", size = 0) +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab(NULL) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~key, scales = "free")

library(tidybayes)

d_grid_samples %>% 
  select(mu, sigma) %>% 
  gather() %>% 
  group_by(key) %>% 
  mode_hdi(value)



b4.1 <- 
  brm(data = d2, family = gaussian,
      height ~ 1,
      prior = c(prior(normal(178, 20), class = Intercept),
                prior(uniform(0, 50), class = sigma)),
      iter = 31000, warmup = 30000, chains = 4, cores = 4,
      seed = 4)




b4.1_half_cauchy <- 
  brm(data = d2, family = gaussian,
      height ~ 1,
      prior = c(prior(normal(178, 20), class = Intercept),
                prior(cauchy(0, 1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 4)

post <- posterior_samples(b4.1_half_cauchy)


posterior_summary(b4.1_half_cauchy)


ggplot(data = d2, 
       aes(x = weight, y = height)) +
  geom_point(shape = 1, size = 2) +
  theme_bw() +
  theme(panel.grid = element_blank())






b4.3 <- 
  brm(data = d2, family = gaussian,
      height ~ 1 + weight,
      prior = c(prior(normal(156, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(uniform(0, 50), class = sigma)),
      iter = 41000, warmup = 40000, chains = 4, cores = 4,
      seed = 4)


posterior_summary(b4.3)

d2 <- 
  d2 %>%
  mutate(weight_c = weight - mean(weight))



b4.4 <- 
  brm(data = d2, family = gaussian,
      height ~ 1 + weight_c,
      prior = c(prior(normal(178, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(uniform(0, 50), class = sigma)),
      iter = 46000, warmup = 45000, chains = 4, cores = 4,
      seed = 4)

posterior_summary(b4.4)


d2 %>%
  ggplot(aes(x = weight, y = height)) +
  geom_abline(intercept = fixef(b4.3)[1], 
              slope     = fixef(b4.3)[2]) +
  geom_point(shape = 1, size = 2, color = "royalblue") +
  theme_bw() +
  theme(panel.grid = element_blank())




post <- posterior_samples(b4.3)


mu_at_50 <- 
  post %>% 
  transmute(mu_at_50 = b_Intercept + b_weight * 50)


mu_at_50 %>%
  ggplot(aes(x = mu_at_50)) +
  geom_density(size = 0, fill = "royalblue") +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(x = expression(mu["height | weight = 50"])) +
  theme_classic()



mu_at_50 %>%
  ggplot(aes(x = mu_at_50)) +
  geom_density(size = 0, fill = "royalblue") +
  stat_pointintervalh(aes(y = 0), 
                      point_interval = mode_hdi, .width = .95) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(x = expression(mu["height | weight = 50"])) +
  theme_classic()

mu <- fitted(b4.3, summary = F)

weight_seq <- tibble(weight = seq(from = 25, to = 70, by = 1))

mu <-
  fitted(b4.3,
         summary = F,
         newdata = weight_seq) %>%
  as_tibble() %>%
  # here we name the columns after the `weight` values from which they were computed
  set_names(25:70) %>% 
  mutate(iter = 1:n())

mu <- 
  mu %>%
  gather(weight, height, -iter) %>% 
  # We might reformat `weight` to numerals
  mutate(weight = as.numeric(weight))


d2 %>%
  ggplot(aes(x = weight, y = height)) +
  geom_point(data = mu %>% filter(iter < 101),
             alpha = .1)

d2 %>%
  ggplot(aes(x = weight, y = height)) +
  geom_point(data = mu %>% filter(iter < 101), 
             color = "navyblue", alpha = .05) +
  theme(text = element_text(family = "Times"),
        panel.grid = element_blank())


mu_summary <-
  fitted(b4.3, 
         newdata = weight_seq) %>%
  as_tibble() %>%
  # let's tack on the `weight` values from `weight_seq`
  bind_cols(weight_seq)

d2 %>%
  ggplot(aes(x = weight, y = height)) +
  geom_smooth(data = mu_summary,
              aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
              stat = "identity",
              fill = "grey70", color = "black", alpha = 1, size = 1/2) +
  geom_point(color = "navyblue", shape = 1, size = 1.5, alpha = 2/3) +
  coord_cartesian(xlim = range(d2$weight)) +
  theme(text = element_text(family = "Times"),
        panel.grid = element_blank())




pred_height <-
  predict(b4.3,
          newdata = weight_seq) %>%
  as_tibble() %>%
  bind_cols(weight_seq)



d2 %>%
  ggplot(aes(x = weight)) +
  geom_ribbon(data = pred_height, 
              aes(ymin = Q2.5, ymax = Q97.5),
              fill = "grey83") +
  geom_smooth(data = mu_summary,
              aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
              stat = "identity",
              fill = "grey70", color = "black", alpha = 1, size = 1/2) +
  geom_point(aes(y = height),
             color = "navyblue", shape = 1, size = 1.5, alpha = 2/3) +
  coord_cartesian(xlim = range(d2$weight),
                  ylim = range(d2$height)) +
  theme(text = element_text(family = "Times"),
        panel.grid = element_blank())


