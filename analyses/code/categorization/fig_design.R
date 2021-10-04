library(data.table)
dt <- as.data.table(expand.grid(f1 = 0:1, f2 = 0:1, f3 = 1:2, phase = "learn"))
dt[, cat := c("A", "A", "A", "B", "B", "B", "A", "B")]
dt[, c := match(cat, LETTERS[1:2]) - 1]

dt <- rbind(dt, data.table(f1 = c(0, 1, 2, 2, 3, 3),
           f2 = c(0, 0, 2, 3, 2, 3),
           f3 = c(3, 0, 1, 1, 1, 1), 
           phase = "transfer", 
           cat = "?",
           c = 0.5))

### plot_ly
library(plotly)
a <- list(autotick = FALSE, tick0 = 0, dtick = 1)
m <- list(l = 10, b = 0, t = 0, pad = 4)
x <- list(title = "Feature 1", showticklabels = TRUE, autotick = FALSE, tick0 = 0, dtick = 1)
y <- list(title = "Feature 2", showticklabels = TRUE, autotick = FALSE, tick0 = 0, dtick = 1)
z <- list(title = "Feature 3", showticklabels = TRUE, autotick = FALSE, tick0 = 0, dtick = 1)

dt[, cat := factor(cat, levels = c("A", "B", "?"), labels = c("A", "B", "?"))]
dt[, id := paste0(f1, f2, f3)]
cols <- dt[, ifelse(cat == "B", "white", "black")]
plot_ly(dt, x = ~f1, y = ~f2, z = ~f3) %>%
  add_markers(color = ~cat, colors = c("white", "black", "grey"), marker = list(size = 18, opacity = 0.8, line = list(color = rep("black", 14), width = 2))) %>%
  add_text(text = ~id, textposition = "middle center", textfont = list(color = toRGB(cols)), showlegend = FALSE) %>%
  layout(font = list(size = 14), scene = list(xaxis = x, yaxis = y, zaxis = z), legend = list(x = 100, y = 0.5, title = list(text = "Category")), margin = m)






cols <- c(rep(c("white", "black"), each = 3), "white", "black", rep("grey", 6))

library(rgl)

# open 3d window
open3d()

# resize window
par3d(windowRect = c(100, 100, 612, 612))

# plot points
dt[, plot3d(f1, f2, f3, col = cols, border = "black", type = "s", 
            size = 4, xlab = "Feature 1", ylab ="Feature 2", zlab = "Feature 3",
            box = FALSE, axes = TRUE
            # colkey = list(at = c(0, 1), labels = c("Category A", "Category B"))
)]
dt[, texts3d(f1 - 0.29, f2, f3 + 0.16, text = paste0(f1, f2, f3), col = "black", cex = .7)]
# dt[, texts3d(f1, f2 + 0.4, f3 + 0.2, text = paste0(f1, f2, f3, "(", cat, ")"), col = "black", cex = .7)]

# add legend
legend3d("right", title = "Category", legend = c('A', 'B', '?'), fill = c("white", "black", "grey"), cex = 1, inset = c(0.02))

snapshot3d(filename = '../../../output/images/3dplot2.png', fmt = 'png')



library(plot3D)
dt[, points3D(f1, f2, f3, colvar = as.integer(c), bty = "u", 
              col.axis = "black", col.grid = "grey", 
              ticktype = "detailed", nticks = 4,
              bg = cols, col = "black", pch = 21, cex = 3, 
              phi = 15, theta = 15,
              xlab = "Feature 1", ylab ="Feature 2", zlab = "Feature 3"
              # colkey = list(at = c(0, 1), labels = c("Category A", "Category B"))
)]
legend("topright", c("A", "B", "?"), title = "Category", 
       fill = c("white", "black", "grey"), col = "black", cex = 1)




#### gg3d plot
# Packages
library(ggplot2)
library(gg3D)  # devtools::install_github("AckerDWM/gg3D")

# Make plot
make_plot <- function(theta=0, phi=0){
  ggplot(dt, aes(x=f1, y=f2, z=f3, colour=cat)) +
    axes_3D(theta=theta, phi=phi) +
    stat_3D(theta=theta, phi=phi, geom="point", size = 3) +
    labs_3D(theta=theta, phi=phi,
            labs=c("Feature 1", "Feature 2", "Feature 3"),
            angle=c(0,0,0),
            hjust=c(0,2,2),
            vjust=c(2,2,-2)) +
    theme_void()
}

make_plot(theta=15,phi=15)
