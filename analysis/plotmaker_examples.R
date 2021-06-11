library(gbm)
library(tidyverse)
library(gridExtra)

path <- "R/"

for(file in list.files(path)) source(file.path(path, file))

source("analysis/load_data_gstage.R")


xval <- xval[idval == 7,]
gapval <- gapval[idval == 7]
yval <- yval[idval == 7]
idval <- idval[idval == 7]

fitgbm <- gest_fit(xtrain, ytrain, gaptrain, idtrain, method = "gbm")

print(summary(fitgbm, plotit = FALSE)[1:10, 1])

predy_gbm <- predict.gest_fit(fitgbm, xval, gapval, idval)

predy_gbm2 <- alignment_response(predy_gbm, gapval, idval)



# plot_dataframe <- tibble(true_value = yval,
#                          `No alignment` = predy_gbm,
#                          `Aligned` = predy_gbm2)
# plot_dataframe <- plot_dataframe %>%
#   pivot_longer(cols = c(`No alignment`,
#                         `Aligned`)) %>%
#   mutate(name = factor(name, levels = c("No alignment",
#                                         "Aligned")))
# 
# p <- ggplot(plot_dataframe) +
#   geom_point(aes(x = true_value, y = value)) +
#   geom_abline() +
#   labs(x = "True Response",
#        y = "Predicted Value",
#        colour = "Subject ID") +
#   facet_wrap( ~ name, ncol = 3) +
#   theme_bw()

plot_dataframe <- tibble(true_value = yval,
                         `No alignment` = predy_gbm,
                         `Aligned` = predy_gbm2)
plot_dataframe <- plot_dataframe %>%
  pivot_longer(cols = c(`No alignment`,
                        `Aligned`)) %>%
  mutate(name = factor(name, levels = c("Aligned",
                                        "No alignment")))


p <- ggplot(plot_dataframe, aes(x = true_value, y = value, shape = name)) +
  coord_equal() +
  geom_point(size = 1.2) +
  geom_line(aes(linetype = name), size = .8) +
  geom_abline() +
  labs(x = "True Response",
       y = "Predicted Value",
       colour = "Subject ID", 
       linetype = NULL) +
  guides(shape = FALSE) +
  scale_shape_manual(values = c(16, 17)) +
  theme_bw() + 
  theme(legend.position = "bottom")
p

ggsave("example_point.png",
       plot = p,
       width = 3.5,
       height = 3.5)


fitgbm_int_example <- gest_fit_gbm_intervals(xtrain, ytrain, gaptrain, idtrain, limits = c(.3, .7))

print(summary(fitgbm, plotit = FALSE)[1:10, 1])

predy_gbm_int <- predict.gest_fit_gbm_intervals(fitgbm_int_example, xval, gapval, idval)

predy_gbm_int2 <- alignment_response_intervals(predy_gbm_int, gapval, idval)


plotdf1 <- data.frame(predy_gbm_int) %>% cbind(yval, idval)
plotdf2 <- data.frame(predy_gbm_int2) %>% cbind(yval, idval) 

plotdf <- bind_rows(plotdf1, plotdf2, .id = "name") %>%
  mutate(name = as.factor(name),
         yval.hat = 0.5*(yval.hat.left + yval.hat.right))

levels(plotdf$name) <- c("Aligned", "No Alignment")



p2 <- ggplot(plotdf, aes(x = yval)) + 
  geom_point(aes(y = yval.hat, shape = name)) +
  geom_line(aes(y = yval.hat, linetype = name), size = .6) +
  geom_linerange(aes(ymin = yval.hat.left, ymax = yval.hat.right), size = .8) + 
  geom_abline(linetype = 2) + theme_bw() + 
  labs(x = "True Response",
       y = "Predicted Value") +
  facet_wrap(~ name) +
  theme_bw() + 
  scale_shape_manual(values = c(16, 17)) +
  guides(shape = FALSE, linetype = FALSE) +
  theme(legend.position = "bottom")

p2

ggsave("example_intervals.png",
       plot = p2,
       width = 7,
       height = 3.5)

