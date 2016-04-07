# not very generic! Uses functions in derivSimulCI.R courtesy of Gavin Simpson.
plotCIdate <- function(g, df, add=FALSE, linecols=c("black","red","forestgreen"), ...){  
  
  dates <- seq(min(df$Date), max(df$Date), by="1 day")
  
  fd <- derivSimulCI(g, samples = 10000, n=length(dates))
  
  dydt <- fd[[1]]$deriv[,1]
  CI <- apply(fd[[1]]$simulations, 1, quantile,probs = c(0.025, 0.975))
  
  if(!add){
    plot(dates, dydt, type='l', col=linecols[1], ... ,
         panel.first=addpoly(x=dates, y1=CI[2,], y2=CI[1,]))
  } else {
    addpoly(x=dates, y1=CI[2,], y2=CI[1,])
    lines(dates, dydt, col=linecols[1])
  }
  abline(h=0)
  
  x <- signifD(dydt,dydt,CI[2,],CI[1,],0)
  lines(dates, x[["incr"]], col=linecols[3], lwd=2)
  lines(dates, x[["decr"]], col=linecols[2], lwd=2)
}



addpoly <- function(x,y1,y2,col=alpha("lightgrey",0.8),...){
  ii <- order(x)
  y1 <- y1[ii]
  y2 <- y2[ii]
  x <- x[ii]
  polygon(c(x,rev(x)), c(y1, rev(y2)), col=col, border=NA,...)
}



alpha <- function (colour, alpha = NA) {
  col <- col2rgb(colour, TRUE)/255
  if (length(colour) != length(alpha)) {
    if (length(colour) > 1 && length(alpha) > 1) {
      stop("Only one of colour and alpha can be vectorised")
    }
    if (length(colour) > 1) {
      alpha <- rep(alpha, length.out = length(colour))
    }
    else if (length(alpha) > 1) {
      col <- col[, rep(1, length(alpha)), drop = FALSE]
    }
  }
  alpha[is.na(alpha)] <- col[4, ][is.na(alpha)]
  new_col <- rgb(col[1, ], col[2, ], col[3, ], alpha)
  new_col[is.na(colour)] <- NA
  new_col
}

