image_loop <- function(image, max.tries = 10) {
  
  if (class(image)!='PCArtImage') stop('The image data are not classified as a PCArtImage.')
  
  n_comps <- c(1, 1)
  for (i in 3:(max.tries+1)) { n_comps[i] <- n_comps[i-1] + n_comps[i-2]}
  
  n_comps <- n_comps[-1]
  
  for (i in n_comps) {
    message(paste('\nPCA with', i, ifelse(i == 1, 'component.', 'components.')))
    pca_plot(image$image, i)

    continue <- readline(prompt='Any idea what it could be?\n Press Y to reveal the correct answer, N to continue with more components, or Q to give up.')
    if (continue %in% c('y', 'Y', 'q', 'Q')) {
      points <- (max.tries+1) - which(n_comps == i)
      break
    }
  }

  if (continue %in% c('y', 'Y')) {
    
    message(paste0('\nThe correct answer is: ', image$title, ' by ', image$artist, '.'))
    
    message(paste('\nWas your answer correct?\nY: Yes\nN: No\n'))
    correct <- readline()
    
    if (correct %in% c('Y', 'y')) return(points)
  }

  return(0)

}
