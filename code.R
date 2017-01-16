library(pixmap) # library for representing image in form of matrix with real-valued entries

system("convert strawberries.tif strawberries.ppm")

# reading image as matrix
image <- read.pnm("strawberries.ppm") 


# extracting individual color matrix
red.matrix <- matrix(image@red, nrow = image@size[1], ncol = image@size[2])
green.matrix <- matrix(image@green, nrow = image@size[1], ncol = image@size[2])
blue.matrix <- matrix(image@blue, nrow = image@size[1], ncol = image@size[2])

# for rotating the input matrix clockwise by 90
rotate <- function(x) t(apply(x, 2, rev)) 

# visualizing the image and the three color matrices
# png(filename="matrices.png")
par(mfrow = c(2,2))

plot(image)

image(rotate(green.matrix), col = heat.colors(255))
image(rotate(red.matrix), col = heat.colors(255))
image(rotate(blue.matrix), col = heat.colors(255))
# dev.off()
# __ as from the visualization the red matrix seems best to work with, we will be working with that only. __

# applying SVD to the red matrix
red.matrix.svd <- svd(red.matrix)
red.d <- red.matrix.svd$d
red.u <- red.matrix.svd$u
red.v <- red.matrix.svd$v

# visualizing original and compressed images
par(mfrow = c(3,2))

for (i in c(666, 10, 30, 50, 80, 100))
   {
         red.comprsd <- red.u[,1:i] %*% diag(red.d[1:i]) %*% t(red.v[,1:i])
         image(rotate(red.comprsd), col = heat.colors(255))
     }



