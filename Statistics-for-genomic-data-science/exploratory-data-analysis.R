tropical = c ('darkorange','dodgerblue','hotpink','limegreen','yellow')
palette(tropical)
library(knitr)

knit_hooks$set(setPch = function(before,options,envir){
  if(before) par(pch = 19)
})
opts_chunk$set(setPch = TRUE)

knitr::opts_chunk$set(fig.width = 5,fig.height = 5, size = "footnotesize", warning = FALSE, message = FALSE)
