subj_ids <- c("phjb", "wiyq", "dxcy", "dfzp")
dt_weird <- dt[subj_id %in% subj_ids, ]
dt_weird <- dt_weird[too_slow == FALSE, ]
dt_weird_long <- melt(dt_weird, id.vars = c("subj_id", "stim"), 
                      measure.vars = paste0("pred_", c("disc", "mink", "disc_unidim", "mink_unidim", "random")))
dt_weird_long <- dt_weird_long[!duplicated(dt_weird_long), ]

labs <- dt_weird[, unique(best_model), by = subj_id]
ids <- paste0("Best model: ", as.character(labs$V1), " (", labs$subj_id, ")")
names(ids) <- labs$subj_id

ggplot(dt_weird, aes(x = stim, y = response)) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  stat_summary(geom = "errorbar") +
  stat_summary(geom = "point") +
  stat_summary(geom = "line", data = dt_weird_long, aes(y = value, color = variable, group = variable)) +
  stat_summary(geom = "point", data = dt_weird_long, aes(y = value, color = variable)) +
  ylim(0, 1) +
  xlab("Stimulus ID") +
  ylab("Proportion category A responses") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  facet_wrap(~subj_id, scales = "free_x", labeller = labeller(subj_id = ids)) +
  labs(title = "Participant Responses and Model Predictions", color = "Model") +
  scale_color_discrete(labels = models)
ggsave("../../output/images/fig_weird_participants.png")
