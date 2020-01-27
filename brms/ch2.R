library(tidyverse)

d <-
  tibble(p_1 = 0,
         p_2 = rep(1:0, times = c(1, 3)),
         p_3 = rep(1:0, times = c(2, 2)),
         p_4 = rep(1:0, times = c(3, 1)),
         p_5 = 1)

head(d)

d %>% 
  gather() %>% 
  mutate(x = rep(1:4, times = 5),
         possibility = rep(1:5, each = 4)) %>% 
  ggplot(aes(x = x, y = possibility, 
             fill = value %>% as.character())) +
  geom_point(shape = 21, size = 5) +
  scale_fill_manual(values = c("white", "navy")) +
  scale_x_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = c(.75, 4.25),
                  ylim = c(.75, 5.25)) +
  theme(legend.position = "none")


tibble(draw            = 1:3,
       marbles         = 4) %>% 
  mutate(possibilities = marbles ^ draw)

d <-
  tibble(position = c((1:4^1) / 4^0, 
                      (1:4^2) / 4^1, 
                      (1:4^3) / 4^2),
         draw     = rep(1:3, times = c(4^1, 4^2, 4^3)),
         fill     = rep(c("b", "w"), times = c(1, 3)) %>% 
           rep(., times = c(4^0 + 4^1 + 4^2)))


d %>% 
  ggplot(aes(x = position, y = draw)) +
  geom_point(aes(fill = fill),
             shape = 21, size = 3) +
  scale_y_continuous(breaks = 1:3) +
  scale_fill_manual(values  = c("navy", "white")) +
  theme(panel.grid.minor = element_blank(),
        legend.position  = "none")


lines_1 <-
  tibble(x    = rep((1:4), each = 4),
         xend = ((1:4^2) / 4),
         y    = 1,
         yend = 2)

lines_2 <-
  tibble(x    = rep(((1:4^2) / 4), each = 4),
         xend = (1:4^3) / (4^2),
         y    = 2,
         yend = 3)

d %>% 
  ggplot(aes(x = position, y = draw)) +
  geom_segment(data  = lines_1,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               size  = 1/3) +
  geom_segment(data  = lines_2,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               size  = 1/3) +
  geom_point(aes(fill = fill),
             shape = 21, size = 3) +
  scale_y_continuous(breaks = 1:3) +
  scale_fill_manual(values  = c("navy", "white")) +
  theme(panel.grid.minor = element_blank(),
        legend.position  = "none")

d <-
  d %>% 
  mutate(denominator = ifelse(draw == 1, .5,
                              ifelse(draw == 2, .5 / 4,
                                     .5 / 4^2))) %>% 
  mutate(position    = position - denominator)


lines_1 <-
  lines_1 %>% 
  mutate(x    = x - .5,
         xend = xend - .5 / 4^1)


lines_2 <-
  lines_2 %>% 
  mutate(x    = x - .5 / 4^1,
         xend = xend - .5 / 4^2)

d %>% 
  ggplot(aes(x = position, y = draw)) +
  geom_segment(data  = lines_1,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               size  = 1/3) +
  geom_segment(data  = lines_2,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               size  = 1/3) +
  geom_point(aes(fill = fill),
             shape = 21, size = 3) +
  scale_y_continuous(breaks = 1:3) +
  scale_fill_manual(values  = c("navy", "white")) +
  theme(panel.grid.minor = element_blank(),
        legend.position  = "none")

d %>% 
  ggplot(aes(x = position, y = draw)) +
  geom_segment(data  = lines_1,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               size  = 1/3) +
  geom_segment(data  = lines_2,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               size  = 1/3) +
  geom_point(aes(fill = fill),
             shape = 21, size = 4) +
  scale_fill_manual(values  = c("navy", "white")) +
  scale_x_continuous(NULL, limits = c(0, 4), breaks = NULL) +
  scale_y_continuous(NULL, limits = c(0.75, 3), breaks = NULL) +
  theme(panel.grid      = element_blank(),
        legend.position = "none") +
  coord_polar()



sequence_length <- 50
d <- 
  tibble(
    toss = c("w", "l", "w", "w", "w", "l", "w", "l", "w")
  ) %>% 
  mutate(
    n_trials  = 1:9,
    n_success = cumsum(toss == "w")
  ) %>%
  expand(
    nesting(n_trials, toss, n_success), 
    p_water = seq(from = 0, to = 1, length.out = sequence_length)
  ) %>% 
  group_by(p_water) %>% 
  # you can learn more about lagging here: https://www.rdocumentation.org/packages/stats/versions/3.5.1/topics/lag or here: https://dplyr.tidyverse.org/reference/lead-lag.html
  mutate(
    lagged_n_trials  = lag(n_trials,  k = 1),
    lagged_n_success = lag(n_success, k = 1)
  ) %>% 
  ungroup() %>% 
  mutate(
    prior = ifelse(
      n_trials == 1, .5,
      dbinom(
        x    = lagged_n_success, 
        size = lagged_n_trials, 
        prob = p_water
      )
    ),
    likelihood = dbinom(
      x    = n_success, 
      size = n_trials, 
      prob = p_water
    ),
    strip = str_c("n = ", n_trials)
  ) %>% 
  # the next three lines allow us to normalize the prior and the likelihood, 
  # putting them both in a probability metric 
  group_by(n_trials) %>% 
  mutate(
    prior      = prior      / sum(prior),
    likelihood = likelihood / sum(likelihood)
  ) %>%   
  # plot!
  ggplot(aes(x = p_water)) +
  geom_line(aes(y = prior), linetype = 2) +
  geom_line(aes(y = likelihood)) +
  scale_x_continuous("proportion water", breaks = c(0, .5, 1)) +
  scale_y_continuous("plausibility", breaks = NULL) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~strip, scales = "free_y")



d <-
    tibble(
      p_grid = seq(from = 0, to = 1, length.out = 50),  # define grid
      prior  = 1
    ) %>%                                   # define prior
    mutate(
      likelihood = dbinom(6, size = 9, prob = p_grid),
      unstd_posterior = likelihood * prior,             
      posterior = unstd_posterior / sum(unstd_posterior)
    )


d %>% 
  ggplot(aes(x = p_grid, y = posterior)) +
  geom_point() +
  geom_line() +
  labs(subtitle = "20 points",
       x = "probability of water",
       y = "posterior probability") +
  theme(panel.grid = element_blank())


library(rethinking)

globe_qa <-
  rethinking::map(
    alist(
      w ~ dbinom(9, p),  # binomial likelihood
      p ~ dunif(0, 1)    # uniform prior
    ), 
    data = list(w = 6))

precis(globe_qa)
