# This file plots the data outputted in "output.txt" in interactive, static and animated methods

library(car)
library(rgl)
library(png)
library(scatterplot3d)
library(plot3D)
library(plotly)
library(lazyeval)
library(dplyr)



# Load output from C++ code to R readable file as a table
# Check that data is correct to relevant planetary information
orbit <- read.table(file = "src//output.txt", header = FALSE)
# orbit <- data.frame(xax = orbit[c(1)], yax = orbit[c(2)])

time <- orbit[c(1)]

# #read first column in table, store as x-position
x <- orbit[c(2)]

# #read second column in table, store as y-position
y <- orbit[c(3)]

# #read third column in table, store as z-position
z <- orbit[c(4)]

# #store each column in a df
orb = data.frame(xax = x, yax = y, zax = z)

xpos <- as.vector(x)
ypos <- as.vector(y)
zpos <- as.vector(z)

#2D orbit: uncomment plot to view 2d orbit

# plot(ypos[,], zpos[,], main="2D orbit", col='royalblue1')


# Prepare for 3D orbit

#interactive: uncomment plot to view 2d orbit

# plot = plot3d(x = orb[1:10,c(1)], y = orb[1:10,c(2)], z = orb[1:10,c(3)], type = "l", fit = "smooth", axis.col = c("black", "black", "black"), surface=FALSE, xlab = "x (km)", ylab = "y (km)", zlab = "z (km)", point.col = "red")
# plot


#static but marker graph in svg and png format: uncomment plot and orca commands to view 2d orbit (note: needs SVG viewer extension for .svg file output)

# plot = plot_ly(orb, x = orb[,c(1)], y = orb[,c(2)], z = orb[,c(3)], mode = 'lines', color = 'red') %>% 
#     layout(
#         scene = list(
#             xaxis = list(title = "x"),
#             yaxis = list(title = "y"),
#             zaxis = list(title = "z")
#     )
#   )

# orca(plot, "src\\plotOrbits\\staticorbit.svg")
# orca(plot, "src\\plotOrbits\\staticorbit")



#animated: uncomment plot3d and play3d to view 2d orbit


#Surface of Planet

Radius = 6787    #specifiy planet's radius
# read a image containing surface of planet
img <- readPNG("src//plotOrbits//mars_surface.png")


if (exists("rasterImage")) { # can plot only in R 2.11.0 and higher
    plot(1:2, type='n')
    rasterImage(img,  1, 2, 2, 1, interpolate=FALSE)
}

img <- as.raster(img)

Radius = 6787
phi <- seq(0, pi, length = 200)
theta <- seq(0, 2*pi, length = 200)
x = Radius*outer(sin(phi),cos(theta))
y = Radius*outer(sin(phi),sin(theta))
z = Radius * (matrix(1, 200, 200)*cos(phi))
# print(sqrt(x**2 + y**2 + z**2))


# Relevant Colors
clr <- '#b20000'

#Background of Visual
axx <- list(
  backgroundcolor="rgb(200, 200, 230",
  gridcolor="rgb(255,255,255)",
  showbackground=TRUE,
  title = 'x position',
  range = c(-9000, 9000),
  zerolinecolor="rgb(255,255,255"
)

axy <- list(
  backgroundcolor="rgb(230, 200,230)",
  gridcolor="rgb(255,255,255)",
  showbackground=TRUE,
  title = 'y position',
  range = c(-9000, 9000),
  zerolinecolor="rgb(255,255,255"
)

axz <- list(
  backgroundcolor="rgb(230, 230,200)",
  gridcolor="rgb(255,255,255)",
  showbackground=TRUE,
  title = 'z position',
  range = c(-9000, 9000),
  zerolinecolor="rgb(255,255,255"
)


accumulate_by <- function(dat, var) {
  var <- f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  bind_rows(dats)
}


orbit <- data.frame(time = orbit[,c(1)], xpos = orbit[,c(2)], ypos = orbit[,c(3)], zpos = orbit[,c(4)])
df <- orbit %>% accumulate_by(~time)


base <- df %>%
  plot_ly(
    x = ~xpos, 
    y = ~ypos,
    z = ~zpos,
    frame = ~frame,
    color = ~frame,
    type = 'scatter3d',
    mode = 'lines',
    name = 'Orbit',
    line = list(simplyfy = F, width = 3),
    showscale = FALSE
  ) %>% layout(
    scene = list(xaxis = axx, yaxis = axy, zaxis = axz)
  ) %>% animation_opts(
    frame = 1 %/% 21900,
    easing = 'linear',
    transition = 1 %/% 20000
  )



surf <- plot_ly(x = ~x, y = ~y, z = ~z, colorscale = ~list(c(0, img[333,549]), c(1, img[222,898])), showscale = FALSE, showlegend = FALSE) %>% add_surface() %>%
  layout(
    scene = list(xaxis = axx, yaxis = axy, zaxis = axz)
  )


print(subplot(surf, base))



