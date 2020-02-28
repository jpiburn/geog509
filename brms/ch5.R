library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
rm(WaffleDivorce)
detach(package:rethinking, unload = T)
library(brms)
library(tidyverse)

library(ggrepel)

d %>%
  ggplot(aes(x = WaffleHouses/Population, y = Divorce)) +
  stat_smooth(method = "lm", fullrange = T, size = 1/2,
              color = "firebrick4", fill = "firebrick", alpha = 1/5) +
  geom_point(size = 1.5, color = "firebrick4", alpha = 1/2) +
  geom_text_repel(data = d %>% filter(Loc %in% c("ME", "OK", "AR", "AL", "GA", "SC", "NJ")),  
                  aes(label = Loc), 
                  size = 3, seed = 1042) +  # this makes it reproducible
  scale_x_continuous("Waffle Houses per million", limits = c(0, 55)) +
  coord_cartesian(xlim = 0:50, ylim = 5:15) +
  ylab("Divorce rate") +
  theme_bw() +
  theme(panel.grid = element_blank())  


d <-
  d %>%
  mutate(MedianAgeMarriage_s = (MedianAgeMarriage - mean(MedianAgeMarriage)) /
           sd(MedianAgeMarriage))


b5.1 <- 
  brm(data = d, family = gaussian,
      Divorce ~ 1 + MedianAgeMarriage_s,
      prior = c(prior(normal(10, 10), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(uniform(0, 10), class = sigma)),
      iter = 2000, warmup = 500, chains = 4, cores = 4,
      seed = 5)

nd <- tibble(MedianAgeMarriage_s = seq(from = -3, to = 3.5, length.out = 30))

f <- 
  fitted(b5.1, newdata = nd) %>%
  as_tibble() %>%
  # tack the `nd` data onto the `fitted()` results
  bind_cols(nd)


ggplot(data = f, 
       aes(x = MedianAgeMarriage_s, y = Estimate)) +
  geom_smooth(aes(ymin = Q2.5, ymax = Q97.5),
              stat = "identity",
              fill = "firebrick", color = "firebrick4", alpha = 1/5, size = 1/4) +
  geom_point(data = d, 
             aes(y = Divorce), 
             size = 2, color = "firebrick4") +
  ylab("Divorce") +
  coord_cartesian(xlim = range(d$MedianAgeMarriage_s), 
                  ylim = range(d$Divorce)) +
  theme_bw() +
  theme(panel.grid = element_blank())  



d <-
  d %>%
  mutate(Marriage_s = (Marriage - mean(Marriage)) / sd(Marriage))

b5.3 <- 
  brm(data = d, family = gaussian,
      Divorce ~  Marriage_s + MedianAgeMarriage_s,
      prior = c(prior(normal(10, 10), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(uniform(0, 10), class = sigma)),
      iter = 2000, warmup = 500, chains = 4, cores = 4,
      seed = 5)


mcmc_plot(b5.3)

library(bayesplot)

post <- posterior_samples(b5.3)

color_scheme_set("red")
mcmc_intervals(post[, 1:4], 
               prob = .5,
               point_est = "median") +
  labs(title = "My fancy bayesplot-based coefficient plot") +
  theme(axis.text.y  = element_text(hjust = 0),
        axis.line.x  = element_line(size = 1/4),
        axis.line.y  = element_blank(),
        axis.ticks.y = element_blank())


library(tidybayes)

post %>% 
  select(-lp__) %>% 
  gather() %>% 
  
  ggplot(aes(x = value, y = reorder(key, value))) +  # note how we used `reorder()` to arrange the coefficients
  geom_vline(xintercept = 0, color = "firebrick4", alpha = 1/10) +
  stat_pointintervalh(point_interval = mode_hdi, .width = .95, 
                      size = 3/4, color = "firebrick4") +
  labs(title = "My tidybayes-based coefficient plot",
       x = NULL, y = NULL) +
  theme_bw() +
  theme(panel.grid   = element_blank(),
        panel.grid.major.y = element_line(color = alpha("firebrick4", 1/4), linetype = 3),
        axis.text.y  = element_text(hjust = 0),
        axis.ticks.y = element_blank())




b5.4 <- 
  brm(data = d, family = gaussian,
      Marriage_s ~ 1 + MedianAgeMarriage_s,
      prior = c(prior(normal(0, 10), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(uniform(0, 10), class = sigma)),
      iter = 2000, warmup = 500, chains = 4, cores = 4,
      seed = 5)


f <- 
  fitted(b5.4) %>%
  as_tibble() %>%
  bind_cols(d)

head(f)




f %>% 
  ggplot(aes(x = MedianAgeMarriage_s, y = Marriage_s)) +
  geom_point(size = 2, shape = 1, color = "firebrick4") +
  geom_segment(aes(xend = MedianAgeMarriage_s, yend = Estimate), 
               size = 1/4) +
  geom_line(aes(y = Estimate), 
            color = "firebrick4") +
  coord_cartesian(ylim = range(d$Marriage_s)) +
  theme_bw() +
  theme(panel.grid = element_blank())  


r <- 
  residuals(b5.4) %>%
  # to use this in ggplot2, we need to make it a tibble or data frame
  as_tibble() %>% 
  bind_cols(d)

# for the annotation at the top
text <-
  tibble(Estimate = c(- 0.5, 0.5),
         Divorce = 14.1,
         label = c("slower", "faster"))

# plot
r %>% 
  ggplot(aes(x = Estimate, y = Divorce)) +
  stat_smooth(method = "lm", fullrange = T,
              color = "firebrick4", fill = "firebrick4", 
              alpha = 1/5, size = 1/2) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  geom_point(size = 2, color = "firebrick4", alpha = 2/3) +
  geom_text(data = text,
            aes(label = label)) +
  scale_x_continuous("Marriage rate residuals", limits = c(-2, 2)) +
  coord_cartesian(xlim = range(r$Estimate),
                  ylim = c(6, 14.1)) +
  theme_bw() +
  theme(panel.grid = element_blank())





# we need new `nd` data
nd <- 
  tibble(Marriage_s          = seq(from = -3, to = 3, length.out = 30),
         MedianAgeMarriage_s = mean(d$MedianAgeMarriage_s))

fitted(b5.3, newdata = nd) %>% 
  as_tibble() %>% 
  # since `fitted()` and `predict()` name their intervals the same way, 
  # we'll need to `rename()` them to keep them straight
  rename(f_ll = Q2.5,
         f_ul = Q97.5) %>% 
  # note how we're just nesting the `predict()` code right inside `bind_cols()`
  bind_cols(
    predict(b5.3, newdata = nd) %>% 
      as_tibble() %>% 
      # since we only need the intervals, we'll use `transmute()` rather than `mutate()`
      transmute(p_ll = Q2.5,
                p_ul = Q97.5),
    # now tack on the `nd` data
    nd) %>% 
  # we're finally ready to plot
  ggplot(aes(x = Marriage_s, y = Estimate)) +
  geom_ribbon(aes(ymin = p_ll, ymax = p_ul),
              fill = "firebrick", alpha = 1/5) +
  geom_smooth(aes(ymin = f_ll, ymax = f_ul),
              stat = "identity",
              fill = "firebrick", color = "firebrick4", alpha = 1/5, size = 1/4) +
  coord_cartesian(xlim = range(d$Marriage_s),
                  ylim = c(6, 14)) +
  labs(subtitle = "Counterfactual plot for which\nMedianAgeMarriage_s = 0",
       y = "Divorce") +
  theme_bw() +
  theme(panel.grid = element_blank())     


nd <- 
  tibble(MedianAgeMarriage_s = seq(from = -3, to = 3.5, length.out = 30),
         Marriage_s          = mean(d$Marriage_s))

# `fitted()` + `predict()`
fitted(b5.3, newdata = nd) %>% 
  as_tibble() %>% 
  rename(f_ll = Q2.5,
         f_ul = Q97.5) %>% 
  bind_cols(
    predict(b5.3, newdata = nd) %>% 
      as_tibble() %>% 
      transmute(p_ll = Q2.5,
                p_ul = Q97.5),
    nd
  ) %>% 
  
  # plot
  ggplot(aes(x = MedianAgeMarriage_s, y = Estimate)) +
  geom_ribbon(aes(ymin = p_ll, ymax = p_ul),
              fill = "firebrick", alpha = 1/5) +
  geom_smooth(aes(ymin = f_ll, ymax = f_ul),
              stat = "identity",
              fill = "firebrick", color = "firebrick4", alpha = 1/5, size = 1/4) +
  coord_cartesian(xlim = range(d$MedianAgeMarriage_s),
                  ylim = c(6, 14)) +
  labs(subtitle = "Counterfactual plot for which\nMarriage_s = 0",
       y = "Divorce") +
  theme_bw() +
  theme(panel.grid = element_blank())   




fitted(b5.3) %>%
  as_tibble() %>%
  bind_cols(d) %>%
  
  ggplot(aes(x = Divorce, y = Estimate)) +
  geom_abline(linetype = 2, color = "grey50", size = .5) +
  geom_point(size = 1.5, color = "firebrick4", alpha = 3/4) +
  geom_linerange(aes(ymin = Q2.5, ymax = Q97.5),
                 size = 1/4, color = "firebrick4") +
  geom_linerange(aes(ymin = Estimate - Est.Error, 
                     ymax = Estimate + Est.Error),
                 size = 1/2, color = "firebrick4") +
  # Note our use of the dot placeholder, here: https://magrittr.tidyverse.org/reference/pipe.html
  geom_text(data = . %>% filter(Loc %in% c("ID", "UT")),
            aes(label = Loc), 
            hjust = 0, nudge_x = - 0.65) +
  labs(x = "Observed divorce", 
       y = "Predicted divorce") +
  theme_bw() +
  theme(panel.grid = element_blank())




residuals(b5.3) %>% 
  as_tibble() %>% 
  rename(f_ll = Q2.5,
         f_ul = Q97.5) %>% 
  bind_cols(
    predict(b5.3) %>% 
      as_tibble() %>% 
      transmute(p_ll = Q2.5,
                p_ul = Q97.5),
    d
  ) %>% 
  # here we put our `predict()` intervals into a deviance metric
  mutate(p_ll = Divorce - p_ll,
         p_ul = Divorce - p_ul) %>% 
  
  # now plot!
  ggplot(aes(x = reorder(Loc, Estimate), y = Estimate)) +
  geom_hline(yintercept = 0, size = 1/2, 
             color = "firebrick4", alpha = 1/10) +
  geom_pointrange(aes(ymin = f_ll, ymax = f_ul),
                  size = 2/5, shape = 20, color = "firebrick4") + 
  geom_segment(aes(y    = Estimate - Est.Error, 
                   yend = Estimate + Est.Error,
                   x    = Loc, 
                   xend = Loc),
               size = 1, color = "firebrick4") +
  geom_segment(aes(y    = p_ll, 
                   yend = p_ul,
                   x    = Loc, 
                   xend = Loc),
               size = 3, color = "firebrick4", alpha = 1/10) +
  labs(x = NULL, y = NULL) +
  coord_flip(ylim = c(-6, 5)) +
  theme_bw() +
  theme(panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0))



residuals(b5.3) %>% 
  as_tibble() %>% 
  bind_cols(d) %>% 
  mutate(wpc = WaffleHouses / Population) %>% 
  
  ggplot(aes(x = wpc, y = Estimate)) +
  geom_point(size = 1.5, color = "firebrick4", alpha = 1/2) +
  stat_smooth(method = "lm", fullrange = T,
              color  = "firebrick4", size = 1/2, 
              fill   = "firebrick", alpha = 1/5) + 
  geom_text_repel(data = . %>% filter(Loc %in% c("ME", "AR", "MS", "AL", "GA", "SC", "ID")),
                  aes(label = Loc),
                  seed = 5.6) +
  scale_x_continuous("Waffles per capita", limits = c(0, 45)) +
  coord_cartesian(xlim = range(0, 40)) +
  ylab("Divorce error") +
  theme_bw() +
  theme(panel.grid = element_blank())


library(rethinking)
data(milk)
d <- milk


rm(milk)
detach(package:rethinking, unload = T)
library(brms)


d %>% 
  select(kcal.per.g, mass, neocortex.perc) %>% 
  pairs(col = "firebrick4")


b5.5 <- 
  brm(data = d, family = gaussian,
      kcal.per.g ~ 1 + neocortex.perc,
      prior = c(prior(normal(0, 100), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(cauchy(0, 1), class = sigma)),
      iter = 2000, warmup = 500, chains = 4, cores = 4,
      seed = 5)


dcc <- 
  d %>%
  drop_na(ends_with("_s"))  



print(b5.5, digits = 3)


library(rethinking)
data(Howell1)
d <- Howell1


rm(Howell1)
detach(package:rethinking, unload = T)
library(tidybayes)
library(brms)

b5.15 <- 
  brm(data = d, family = gaussian,
      height ~ 1 + male,
      prior = c(prior(normal(178, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 2), class = sigma)),
      iter = 3000, warmup = 500, chains = 4, cores = 4,
      seed = 5)




print(b5.15)


post <- posterior_samples(b5.15)

post %>%
  transmute(male_height = b_Intercept + b_male) %>% 
  mean_qi(.width = .89)

nd <- tibble(male = 1)

fitted(b5.15,
       newdata = nd)


fitted(b5.15,
       newdata = nd,
       summary = F) %>% 
  as_tibble() %>%
ggplot(aes(x = V1, y = 0)) +
  geom_halfeyeh(fill = "firebrick4", 
                point_interval = median_qi, .width = .95) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(subtitle = "Model-implied male heights",
       x = expression(alpha + beta["male"])) +
  theme_bw() +
  theme(panel.grid = element_blank())

library(rethinking)
data(milk)
d <- milk

rm(milk)
detach(package:rethinking, unload = T)
library(brms)

d <- 
  d %>%
  mutate(clade_nwm = ifelse(clade == "New World Monkey", 1, 0),
         clade_owm = ifelse(clade == "Old World Monkey", 1, 0),
         clade_s   = ifelse(clade == "Strepsirrhine", 1, 0),
         clade_ape = ifelse(clade == "Ape", 1, 0))



b5.16_alt <- 
  brm(data = d, family = gaussian,
      kcal.per.g ~ 0 + clade,
      prior = c(prior(normal(.6, 10), class = b),
                prior(uniform(0, 10), class = sigma)),
      iter = 2000, warmup = 500, chains = 4, cores = 4,
      seed = 5)

print(b5.16_alt)
