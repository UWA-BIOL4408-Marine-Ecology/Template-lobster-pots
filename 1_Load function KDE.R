
# Load this all of this function to test for differences in Density estimates
# Langlois, T. J., B. R. Fitzpatrick, D. V. Fairclough, C. B. Wakefield, S. Alex Hesp, D. L. Mc Lean, E. S. Harvey, and J. J. Meeuwig. 2012. Similarities between line fishing and baited stereo-video estimations of length-frequency: novel application of kernel density estimates. PloS one 7:e45973. 

library(sm)


library(KernSmooth)


 WF=function(Data,nperm,Species){
   hv=dpik(Data$distance[Data$Model=='Pots'],kernel='normal')
   hl=dpik(Data$distance[Data$Model=='NULL'],kernel='normal')
   h.m=(hl+hv)/2 #arithmetic mean of the KDEs
   h.m=sqrt(hl*hv) #geometrtic mean of KDEs
   h.m=prod(c(hl,hv))^(1/length(c(hl,hv))) #geometric mean for multiple products
      sm.density.compare2(Data$distance,Data$Model,h=h.m,model='equal',xlab='',main=paste(Species),font.main=3,nboot=nperm,col.band="grey",col="black",ylab='')

 }


  sm.density.compare2=function (x, group, h, model = "none", ...)
 {
     if (!is.vector(x))
         stop("sm.density.compare can handle only 1-d data")
     opt <- sm.options(list(...))
     sm:::replace.na(opt, ngrid, 500)
     sm:::replace.na(opt, display, "line")
     sm:::replace.na(opt, xlab, deparse(substitute(x)))
     sm:::replace.na(opt, ylab, "Density")
     sm:::replace.na(opt, xlim, c(min(x) - diff(range(x))/4, max(x) +
         diff(range(x))/4))
     sm:::replace.na(opt, eval.points, seq(opt$xlim[1], opt$xlim[2],
         length = opt$ngrid))
     if (is.na(opt$band)) {
         if (model == "none")
             opt$band <- FALSE
         else opt$band <- TRUE
     }
     if ((model == "none") && opt$band)
         opt$band <- FALSE
     band <- opt$band
     ngrid <- opt$ngrid
     xlim <- opt$xlim
     nboot <- opt$nboot
     y <- x
     if (is.na(opt$test)) {
         if (model == "none")
             opt$test <- FALSE
         else opt$test <- TRUE
     }
     if ((model == "none") && opt$test)
         opt$test <- FALSE
     test <- opt$test
     if (opt$display %in% "none")
         band <- FALSE
     fact <- factor(group)
     fact.levels <- levels(fact)
     nlev <- length(fact.levels)
     ni <- table(fact)
     if (band & (nlev > 2)) {
         cat("Reference band available to compare two groups only.",
             "\n")
         band <- FALSE
     }
     if (length(opt$lty) < nlev)
         opt$lty <- 2:(nlev+1)
     if (length(opt$col) < nlev)
         opt$col <- seq(1:1,length.out=nlev)
     if (missing(h))
         h <- h.select(x, y = NA, group = group, ...)
     opt$band <- band
     opt$test <- test
     estimate <- matrix(0, ncol = opt$ngrid, nrow = nlev)
     se <- matrix(0, ncol = opt$ngrid, nrow = nlev)
     for (i in 1:nlev) {
         sm <- sm.density(y[fact == fact.levels[i]], h = h, display = "none",
             eval.points = opt$eval.points)
         estimate[i, ] <- sm$estimate
         se[i, ] <- sm$se
     }
     eval.points <- sm$eval.points
     if (!(opt$display %in% "none" | band)) {
         plot(xlim, c(0, 1.1 * max(as.vector(estimate))), xlab = opt$xlab,
             ylab = opt$ylab, type = "n")
         for (i in 1:nlev) lines(eval.points, estimate[i, ], lty = opt$lty[i],
             col = opt$col[i])
     }
     est <- NULL
     p <- NULL
     if (model == "equal" & test) {
         if (nlev == 2) {
             ts <- sum((estimate[1, ] - estimate[2, ])^2)
         }
         else {
             sm.mean <- sm.density(y, h = h, xlim = opt$xlim,
                 ngrid = opt$ngrid, display = "none")$estimate
             ts <- 0
             for (i in 1:nlev) ts <- ts + ni[i] * sum((estimate[i,
                 ] - sm.mean)^2)
         }
         p <- 0
         est.star <- matrix(0, ncol = opt$ngrid, nrow = nlev)
         for (iboot in 1:nboot) {
             ind <- (1:length(y))
             for (i in 1:nlev) {
                 indi <- sample((1:length(ind)), ni[i])
                 est.star[i, ] <- sm.density(y[ind[indi]], h = h,
                   ngrid = opt$ngrid, xlim = opt$xlim, display = "none")$estimate
                 ind <- ind[-indi]
             }
             if (nlev == 2) {
                 ts.star <- sum((est.star[1, ] - est.star[2, ])^2)
             }
             else {
                 sm.mean <- sm.density(y, h = h, xlim = opt$xlim,
                   ngrid = opt$ngrid, display = "none")$estimate
                 ts.star <- 0
                 for (i in 1:nlev) {
                   ts.star <- ts.star + ni[i] * sum((est.star[i,
                     ] - sm.mean)^2)
                 }
             }
             if (ts.star > ts)
                 p <- p + 1
             if (opt$verbose > 1) {
                 cat(iboot)
                 cat(" ")
             }
         }
         p <- p/nboot
         cat("\nTest of equal densities:  p-value = ", round(p,
             3), "\n")
         est <- list(p = p, h = h)
     }
     if (model == "equal" & band) {
         av <- (sqrt(estimate[1, ]) + sqrt(estimate[2, ]))/2
         se <- sqrt(se[1, ]^2 + se[2, ]^2)
         upper <- (av + se)^2
         lower <- pmax(av - se, 0)^2
         plot(xlim, c(0, 1.1 * max(as.vector(estimate), upper)),
             xlab = opt$xlab, ylab = opt$ylab, type = "n")
         polygon(c(eval.points, rev(eval.points)), c(upper, rev(lower)),
             col = "grey", border = 0)
         lines(eval.points, estimate[1, ], lty = 'longdash',lwd = 1.25, col = "black")
         lines(eval.points, estimate[2, ], lty = 'solid',lwd = 1.25, col = "black")
         est <- list(p = p, upper = upper, lower = lower, h = h)
     }
     invisible(est)
 }

