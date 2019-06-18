PCArtQuiz <- function(database = NULL, n.images = 4, max.tries = 10) {
  
  if (is.null(database)) data('database1', envir = environment())
  else load(database, envir = environment())
  
  img_ind <- unlist(eapply(environment(), function(x) class(x) == 'PCArtImage'))
  
  if (n.images > sum(img_ind)) {
    warning(paste0('n.images (', n.images,') is larger than number of images in the database you provided (', sum(img_ind), '. All images in the database will be used once.'))
    n.images <- sum(img_ind)
  }
  
  img_order <- sample(names(img_ind)[img_ind], n.images)
  score <- 0
  
  for (i in img_order) {
    message('\nOn to the next image.')
    score <- score + image_loop(get(i), max.tries)
  }
  
  message(paste0('Your total score was ', score, ' out of ', n.images, '.'))
  
}