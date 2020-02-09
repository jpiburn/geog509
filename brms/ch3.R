library(tidyverse)
library(tidybayes)

n <- 1001
n_success <- 6
n_trials  <- 9

(
  d <-
    tibble(p_grid     = seq(from = 0, to = 1, length.out = n),
           # note we're still using a flat uniform prior
           prior      = 1) %>% 
    mutate(likelihood = dbinom(n_success, size = n_trials, prob = p_grid)) %>% 
    mutate(posterior  = (likelihood * prior) / sum(likelihood * prior))
)


# how many samples would you like?
n_samples <- 1e4

# make it reproducible
set.seed(3)

samples <-
  d %>% 
  sample_n(size = n_samples, weight = posterior, replace = T)

glimpse(samples)


samples %>% 
  mutate(sample_number = 1:n()) %>% 
  ggplot(aes(x = sample_number, y = p_grid)) +
  geom_line(size = 1/10) +
  labs(x = "sample number",
       y = "proportion of water (p)")

samples %>% 
  ggplot(aes(x = p_grid)) +
  geom_density(fill = "black") +
  coord_cartesian(xlim = 0:1) +
  xlab("proportion of water (p)")

d %>% 
  filter(p_grid < .5) %>% 
  summarise(sum = sum(posterior))

samples %>% 
  filter(p_grid < .5) %>% 
  summarise(sum = n() / n_samples)

samples %>% 
  filter(p_grid > .5 & p_grid < .75) %>% 
  summarise(sum = n() / n_samples)

samples %>% 
  filter(p_grid > .5, p_grid < .75) %>% 
  summarise(sum = n() / n_samples)


d %>% 
  ggplot(aes(x = p_grid)) +
  geom_line(aes(y = posterior)) +
  geom_ribbon(data = d %>% filter(p_grid < .5),
              aes(ymin = 0, ymax = posterior)) +
  labs(x = "proportion of water (p)",
       y = "density")



# upper right panel
d %>% 
  ggplot(aes(x = p_grid)) +
  geom_line(aes(y = posterior)) +
  # note this next line is the only difference in code from the last plot
  geom_ribbon(data = d %>% filter(p_grid < .75 & p_grid > .5),
              aes(ymin = 0, ymax = posterior)) +
  labs(x = "proportion of water (p)",
       y = "density")



(q_80 <- quantile(samples$p_grid, prob = .8))

(q_10_and_90 <- quantile(samples$p_grid, prob = c(.1, .9)))



d %>% 
  ggplot(aes(x = p_grid)) +
  geom_line(aes(y = posterior)) +
  geom_ribbon(data = d %>% filter(p_grid < q_80),
              aes(ymin = 0, ymax = posterior)) +
  annotate(geom = "text",
           x = .25, y = .0025,
           label = "lower 80%") +
  labs(x = "proportion of water (p)",
       y = "density")


d %>% 
  ggplot(aes(x = p_grid)) +
  geom_line(aes(y = posterior)) +
  geom_ribbon(data = d %>% filter(p_grid > q_10_and_90[1] & p_grid < q_10_and_90[2]),
              aes(ymin = 0, ymax = posterior)) +
  annotate(geom = "text",
           x = .25, y = .0025,
           label = "middle 80%") +
  labs(x = "proportion of water (p)",
       y = "density")



# here we update the `dbinom()` parameters
n_success <- 3
n_trials  <- 3

# update `d`
d <-
  d %>% 
  mutate(likelihood = dbinom(n_success, size = n_trials, prob = p_grid)) %>% 
  mutate(posterior  = (likelihood * prior) / sum(posterior))

# make the next part reproducible
set.seed(3)

# here's our new samples tibble
(
  samples <-
    d %>% 
    sample_n(size = n_samples, weight = posterior, replace = T)
)


# from tidybayes
median_qi(samples$p_grid, .width = .5)
median_qi(samples$p_grid, .width = c(.5, .8, .99))

mode_hdi(samples$p_grid, .width = .5)



make_loss <- function(our_d){
  d %>% 
    mutate(loss = posterior * abs(our_d - p_grid)) %>% 
    summarise(weighted_average_loss = sum(loss))
}

(
  l <-
    d %>% 
    select(p_grid) %>% 
    rename(decision = p_grid) %>% 
    mutate(weighted_average_loss = purrr::map(decision, make_loss)) %>% 
    unnest() 
)



min_loss <-
  l %>% 
  filter(weighted_average_loss == min(weighted_average_loss)) %>% 
  as.numeric()

# the plot
l %>%   
  ggplot(aes(x = decision)) +
  geom_ribbon(aes(ymin = 0, ymax = weighted_average_loss),
              fill = "grey75") +
  geom_vline(xintercept = min_loss[1], color = "white", linetype = 3) +
  geom_hline(yintercept = min_loss[2], color = "white", linetype = 3) +
  ylab("expected proportional loss") +
  theme(panel.grid = element_blank())


n_draws <- 1e5

simulate_binom <- function(probability){
  set.seed(3)
  rbinom(n_draws, size = 9, prob = probability) 
}

d_small <-
  tibble(probability = seq(from = .1, to = .9, by = .1)) %>% 
  mutate(draws       = purrr::map(probability, simulate_binom)) %>% 
  unnest(draws) %>% 
  mutate(label       = str_c("p = ", probability))



d_small %>%
  ggplot(aes(x = draws)) +
  geom_histogram(binwidth = 1, center = 0,
                 color = "grey92", size = 1/10) +
  scale_x_continuous(NULL, breaks = seq(from = 0, to = 9, by = 3)) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(subtitle = "Sampling distributions") +
  coord_cartesian(xlim = 0:9) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ label, ncol = 9) 


n_samples <- 1e4

# make it reproducible
set.seed(3)

samples <-
  d %>% 
  sample_n(size = n_samples, weight = posterior, replace = T) %>% 
  mutate(w = purrr::map_dbl(p_grid, rbinom, n = 1, size = 9))

samples %>% 
  ggplot(aes(x = w)) +
  geom_histogram(binwidth = 1, center = 0,
                 color = "grey92", size = 1/10) +
  scale_x_continuous("number of water samples",
                     breaks = seq(from = 0, to = 9, by = 3)) +
  scale_y_continuous(NULL, breaks = NULL) +
  ggtitle("Posterior predictive distribution") +
  coord_cartesian(xlim = 0:9,
                  ylim = 0:3000) +
  theme(panel.grid = element_blank())
