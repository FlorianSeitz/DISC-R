soft <- function(x, tau) {
  exp(x/tau) / (exp(x/tau) + exp((1-x)/tau))
}

dt_tau <- data.table(x = seq(0, 1, .01))
dt_tau[, small := soft(x, .25)]
dt_tau[, large := soft(x, 1)]
dt_tau <- as.data.table(pivot_longer(dt_tau, cols = c("small", "large")))
dt_tau[, name := factor(name, labels = c("small (0.25)", "large (1)"), levels = c("small", "large"))]

ggplot(dt_tau, aes(x, value, group = name, lty = name)) +
  geom_line(size = .7) +
  scale_linetype_manual(values = 1:2) +
  labs(x = expression(paste("Evidence ", italic(E), "(", italic(A), "|", italic(i), ")")), 
       y = expression(paste("Response probability ", italic(P), "(", italic(A), "|", italic(i), ")")), 
       lty = expression(paste("Parameter ", tau))) + 
  bg_theme + 
  theme(legend.position = c(0.25, 0.75))
ggsave("../../../output/images/softmax.png", width = 4, height = 4)
       