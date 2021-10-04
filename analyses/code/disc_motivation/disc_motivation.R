fs <- 2:6
vs <- 2:8
designs <- expand.grid(f = fs, v = vs)

if(length(fs) == 1) {
  noise <- .01
  w_pars <- t((xsimplex(fs, 3) + noise) / (3 * (1 + noise)))
  ws <- 1:nrow(w_pars)
} else {
  ws <- c(1, 3)
}
cs <- 1:5
ps <- c(1, 2)
rs <- c(1, 2)
parameters <- expand.grid(w = ws, c = cs, p = ps, r = rs)

similarity <- function(x, w, c, p, r) {
  c(exp(-c * (abs(x)^r %*% w)^(p/r)))
}

res <- rbindlist(apply(designs, 1, function(design) {
  f <- design["f"]; v <- design["v"]
  x <- expand.grid(replicate(f, list(1:v)))
  colnames(x) <- paste0("f", 1:f)
  x <- as.matrix(do.call("rbind", c(list(x - x), lapply(1:(v^2-1), function(i) apply(x, 2, diff, lag = i)))))
  
  rbindlist(apply(parameters, 1, function(parameter) {
    w <- parameter["w"]; c <- parameter["c"]; p <- parameter["p"]; r <- parameter["r"]
    if(length(fs) == 1) {
      w <- w_pars[w, ]
    } else {
      ifelse(w == 1, w <- rep(1 / f, f), 
             ifelse(w == 2, w <- c(.75, rep(.25 / (f - 1), f - 1)), w <- c(.99, rep(.01 / (f - 1), f - 1))))
    }
    sims <- data.table(disc = similarity(x != 0, w = w, c = c, p = p, r = r),
                       mink = similarity(x, w = w, c = c, p = p, r = r))
    sims[, .(f = f, v = v, w = round(w[1], 2), c = c, p = p, r = r, 
             rmse = sqrt(mean((disc - mink)^2)),
             rmse_corrected = sqrt(mean((disc[rowMeans(sims) != 1] - mink[rowMeans(sims) != 1])^2)),
             cor = cor(disc, mink), 
             cor_corrected = cor(disc[rowMeans(sims) != 1], mink[rowMeans(sims) != 1]),
             equal = mean(disc == mink),
             equal_corrected = mean(disc[rowMeans(sims) != 1] == mink[rowMeans(sims) != 1]))]
  }))
}))
dt_plot <- res[, .(cor = mean(cor), 
        cor_corrected = mean(cor_corrected), 
        # rmse = mean(rmse),
        # rmse_corrected = mean(rmse_corrected),
        equal = mean(equal),
        equal_corrected = mean(equal_corrected)), by = .(f, v)]

dt_plot <- as.data.table(pivot_longer(dt_plot, !c(f, v), names_to = "gofs"))
dt_plot[, ":=" (corrected = grepl("corrected", gofs, fixed = TRUE),
                equal = grepl("equal", gofs, fixed = TRUE))]

f_max <- 4
labs <- paste(2:f_max, "Features")
names(labs) <- as.character(2:f_max)
ggplot(dt_plot[f <= f_max], aes(x = as.factor(v), y = value, group = gofs, color = equal)) + 
  geom_point(data = dt_plot[f <= f_max & v != 2], size = 2) +
  geom_line(aes(lty = corrected)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_color_manual(name = "Measure", 
                     values = c("TRUE" = "darkgrey", "FALSE" = "black"),
                     labels = c("TRUE" = "equal similarity", "FALSE" = "correlation")) +
  scale_linetype_manual(name = "Auto-Similarity",
                        values = c("TRUE" = 2, "FALSE" = 1),
                        labels = c("TRUE" = "not included", "FALSE" = "included")) +
  labs(x = "Number of Feature Values", 
       y = "Correspondence",
       lty = "Auto-Similarity") + 
  theme(panel.grid = element_blank()) +
  facet_wrap(~f, ncol = 1, labeller = labeller(f = labs))
ggsave("../../../output/images/fig_0.png", width = 4)





f <- 2
v <- 2

x <- expand.grid(replicate(f, list(1:v)))
colnames(x) <- paste0("f", 1:f)
x <- as.matrix(do.call("rbind", c(list(x - x), lapply(1:(v^2-1), function(i) apply(x, 2, diff, lag = i)))))

similarity <- function(x, w, c, p, r) {
  c(exp(-c * (abs(x)^r %*% w)^(p/r)))
}

sims <- data.table(disc = similarity(x != 0, w = rep(1 / f, f), c = 1, p = 1, r = 1),
                   mink = similarity(x, w = rep(1 / f, f), c = 1, p = 1, r = 1))
sims[, cor(disc, mink)]
sims[rowMeans(sims) != 1, cor(disc, mink)]
sims[, cor(disc[rowMeans(sims) != 1], mink[rowMeans(sims) != 1])]
sims[, mean(disc == mink)]
