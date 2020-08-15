dt[!is.na(contemplation_time_pressure), .(mean_ct = mean(contemplation_time_pressure),
                                          median_ct = median(contemplation_time_pressure),
                                          sd_ct = sd(contemplation_time_pressure),
                                          mean_rt = mean(reaction_time_pressure),
                                          median_rt = median(reaction_time_pressure),
                                          sd_rt = sd(reaction_time_pressure))]
dt[!is.na(response), mean(response), by = subj_id][, .(mean = mean(V1),
                                                       median = median(V1),
                                                       sem = sd(V1))]

dt[!is.na(response), .(min = min(response),
                       max = max(response)), by = subj_id][, .(mean_min = mean(min),
                                                               median_min = median(min),
                                                               sd_min = sd(min),
                                                               mean_max = mean(max),
                                                               median_max = median(max),
                                                               sd_max = sd(max))]
dt[, mean(is.na(response)), by = list(subj_id, block)][, .(mean = mean(V1),
                                                           median = median(V1),
                                                           sd = sd(V1)), by = block]
dt[, .(mean = mean(is.na(response)),
       median = median(is.na(response)),
       sd = sd(is.na(response))), by = list(block)]
