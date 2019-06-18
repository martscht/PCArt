pca_plot <- function(image, n_comps) {
  
  sol <- apply(image, 3, stats::prcomp, center = FALSE)
  sol_img <- sapply(sol, function(j)  j$x[, 1:n_comps] %*% t(j$rotation[, 1:n_comps]), simplify = 'array')
  sol_img[sol_img > 1] <- 1; sol_img[sol_img < 0] <- 0
  sol_dim <- dim(sol_img)
  graphics::plot(1, 1, xlim = c(1, sol_dim[2]), ylim = c(1, sol_dim[1]), 
    type = 'n', yaxt = 'n', xaxt = 'n', xlab = '', ylab = '', asp = 1, bty = 'n')
  graphics::rasterImage(sol_img, 1, 1, sol_dim[2], sol_dim[1])
  
}