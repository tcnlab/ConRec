###################################################### 
##########         Pre processing           ########## 
######################################################

count <- function(x) { 
  length(na.omit(x)) 
}

iqr <- function(x) { 
  quantile(x)[[4]] - quantile(x)[[2]] 
}




proctTable <- function(tab){
  tab = tab$tTable
  rows = names(tab[,1])
  out = ""
  for(rowi in rows){
    val = round( tab[rowi,], 3)
    rowi = gsub('\\(','',rowi)
    rowi = gsub('\\)','',rowi)
    rowi = gsub("llag",'Slope',rowi)
    
    if(val[["p-value"]] < 0.001){
      pval = ", p <0.001 $" 
    } else if (val[["p-value"]] < 0.01){
      pval = ", p <0.01 $"
    } else{
      pval = paste(", p =" , val[["p-value"]] , "$")
    }
      
    
    out = paste(out, "\n", rowi, "\n",                                  # Intercept / slope
             "$",val[["Value"]], "\\pm", val[["Std.Error"]] ,"$" , "\n")     # slope std error
    out = paste( out, "$ t(", val[["DF"]], ") = ", round(val[["t-value"]],2),  pval ,"\n\n", val[["p-value"]]  , sep = ""  )
  }
  return(out)
}

###################################################### 
##############      Plots             ################ 
######################################################


plotModel<- function(fname = "Model.png", titlex = "", lagsim , xl1 = 0.46, xl2 = 1.8, col = 0)  {
  
  pdf(file = fname, width = 8, height = 6)
  
  lw1=2
  nl = len(lagsx)
  alpha = sort(seq(0,1,1/nl), decreasing = TRUE)
  
  par(new=F)
  # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
  par(mar=c(3,6.8,3.5,2)+0.1)  
  for(i in 1:nl){
    datsel <- NULL
    for(j in 1:len(subs)){
      subi = subs[j]
      #datTemp <-  MM1sel$RT[MM1sel$lag==lagsx[i] & MM1sel$RT>0 & MM1sel$sub==subi ]
      #datTemp = remOutlierLag(datTemp,diag = TRUE, sub = subi, lag = lag)
      #datsel = c(datsel,datTemp)
    }
    
    #n = len(datsel)
    #plot( sort(datsel), (1:n)/n, type = 'l', ylim = c(0, 1), xlim = c(xl1,xl2), col=rgb((i-1)/nl,(i-1)/nl,(i-1)/nl,0.5), lwd = 2, xlab="",ylab="")
    #par(new=T)
    #plot(seq(xl1,xl2,0.001),lagdistr[[i]],col=rgb(1,0,0,alpha[i]) , lwd = 2, type = "l", xlab="",ylab="")
    #par(new=T)
    n = len(lagsim[[i]]) + 2
    plot( sort( c(0,0,lagsim[[i]])), (1:n)/n, type = 'l', ylim = c(0, 1), xlim = c(xl1,xl2), col=rgb(col,0,0,alpha[i]), lwd = 3, xlab="",ylab="", xaxt='n',yaxt='n')
    par(new=T)
  }
  title( main = titlex, cex.main = 2.5 )
  title( xlab = "Response Time (s)",cex.lab = 2.5, line = 3.5)
  title( ylab = "Cumulative probability"  ,cex.lab = 2.5, line = 4.5)
  axis(1, labels=T,col.axis="black", las=1, cex.axis = 2) # Below
  axis(2, labels=T, col.axis="black", las=2, cex.axis = 2) # Left
  
#   text(1.4, y = 0.3, labels = "Scale:",cex = 2)
#   segments(1.4, 0.2, x1 = 1.5, y1 = .2, lty = 1, lwd = 3)
#   text(1.65, y = 0.2, labels = "100 ms",cex = 2)
  
#   text(1.0, y = 0.3, labels = "Scale:",cex = 2)
#   segments(1.0, 0.2, x1 = 1.1, y1 = .2, lty = 1, lwd = 3)
#   text(1.15, y = 0.2, labels = "100 ms",cex = 2)
  
  dev.off()
}

plotModel2<- function(fname = "Model.png", titlex = "", lagsim,lagDat = 0 , xl1 = 0.46, xl2 = 1.8, col = 0)  {
  
  pdf(file = fname, width = 8, height = 6)
  
  lw1=2
  nl = len(lagsx)
  alpha = sort(seq(0,1,1/nl), decreasing = TRUE)
  
  par(new=F)
  # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
  par(mar=c(5,6.8,3.5,2)+0.1)  
  for(i in 1:nl){
    datsel <- NULL
    for(j in 1:len(subs)){
      subi = subs[j]
      #datTemp <-  MM1sel$RT[MM1sel$lag==lagsx[i] & MM1sel$RT>0 & MM1sel$sub==subi ]
      #datTemp = remOutlierLag(datTemp,diag = TRUE, sub = subi, lag = lag)
      #datsel = c(datsel,datTemp)
    }
    
    #n = len(datsel)
    #plot( sort(datsel), (1:n)/n, type = 'l', ylim = c(0, 1), xlim = c(xl1,xl2), col=rgb((i-1)/nl,(i-1)/nl,(i-1)/nl,0.5), lwd = 2, xlab="",ylab="")
    #par(new=T)
    #plot(seq(xl1,xl2,0.001),lagdistr[[i]],col=rgb(1,0,0,alpha[i]) , lwd = 2, type = "l", xlab="",ylab="")
    #par(new=T)
    n = len(lagsim[[i]]) + 2
    plot( sort( c(0,0,lagsim[[i]])), (1:n)/n, type = 'l', ylim = c(0, 1), xlim = c(xl1,xl2), col=rgb(1,0,0,alpha[i]), lwd = 3, xlab="",ylab="", xaxt='n',yaxt='n')
    par(new=T)
    n = len(lagDat[[i]]) + 2
    plot( sort( c(0,0,lagDat[[i]])), (1:n)/n, type = 'l', ylim = c(0, 1), xlim = c(xl1,xl2), col=rgb(col,0,0,alpha[i]), lwd = 3, xlab="",ylab="", xaxt='n',yaxt='n')
    par(new=T)
  }
  title( main = titlex,cex.main = 2.5 )
  title( xlab = "Response Time (s)",cex.lab = 2.5, line = 3.5)
  title( ylab = "Cumulative probability"  ,cex.lab = 2.5, line = 4.5)
  axis(1, labels=T,col.axis="black", las=1, cex.axis = 2) # Below
  axis(2, labels=T, col.axis="black", las=2, cex.axis = 2) # Left
  
  #   text(1.4, y = 0.3, labels = "Scale:",cex = 2)
  #   segments(1.4, 0.2, x1 = 1.5, y1 = .2, lty = 1, lwd = 3)
  #   text(1.65, y = 0.2, labels = "100 ms",cex = 2)
  
  #   text(1.0, y = 0.3, labels = "Scale:",cex = 2)
  #   segments(1.0, 0.2, x1 = 1.1, y1 = .2, lty = 1, lwd = 3)
  #   text(1.15, y = 0.2, labels = "100 ms",cex = 2)
  
  dev.off()
}




plotModelD<- function(fname = "Model.png", titlex = "", lagsim , xl1 = 0.46, xl2 = 1.8, col = 0, yl2 = 2)  {
  
  pdf(file = fname, width = 8, height = 6)
  
  lw1=2
  nl = len(lagsx)
  alpha = sort(seq(0,1,1/nl), decreasing = TRUE)
  
  par(new=F)
  # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
  par(mar=c(5,6.8,3.5,2)+0.1)  
  for(i in 1:nl){
    n = len(lagsim[[i]])
    den = density(lagsim[[i]])
    #den = density(lagsim[[i]], adjust = 2.0)
    #plot( den$x, den$y/max(den$y), type = 'l', ylim = c(0,yl2), xlim = c(xl1,xl2), col=rgb(col,0,0,alpha[i]), lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n')
    
    plot( den$x, den$y, type = 'l', ylim = c(0,yl2), xlim = c(xl1,xl2), col=rgb(col,0,0,alpha[i]),
          lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n')
    
    #tick = quantile(lagsim[[i]], seq(0,1,0.1))
    #den = hist(lagsim[[i]],breaks = tick,plot = FALSE )
    #plot( den$mids, den$density, type = 'l', ylim = c(0, 2.4), xlim = c(xl1,xl2), col=rgb(col,0,0,alpha[i]), lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n')
    
    par(new=T)
  }
  title( main = titlex ,cex.main = 2.5)
  title( xlab = "Response Time (s)",cex.lab = 2.5, line = 3.5)
  title( ylab = "Density"  ,cex.lab = 2.5, line = 4.5)
  axis(1, labels=T,col.axis="black", las=1, cex.axis = 2) # Below
  axis(2, labels=T, col.axis="black", las=2, cex.axis = 2) # Left
  dev.off()
}


plotModelDa<- function(fname = "Model.png", titlex = "", lagsim , xl1 = 0.46, xl2 = 1.8, col = 0)  {
  
  pdf(file = fname, width = 8, height = 6)
  
  lw1=2
  nl = len(lagsx)
  alpha = sort(seq(0,1,1/nl), decreasing = TRUE)
  alpha[8] = alpha[7] 
  par(new=F)
  # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
  par(mar=c(5,6.8,3.5,2)+0.1)  
  for(i in c(1,4,8)){
    n = len(lagsim[[i]])
    den = density(lagsim[[i]], adjust = 2.0) 
    #den1 = density(lagsim[[i]], adjust = 2.0) 
    #den2 = density(lagsim[[i-1]], adjust = 2.0)
    #den3 = density(lagsim[[i-2]], adjust = 2.0)
#     if (i != 8){
#       de = den1$y + den2$y + den3$y
#     }else{
#       de = den1$y + den2$y
#     }
    de = den$y
    plot( den$x, de/max(de), type = 'l', ylim = c(0,1), xlim = c(xl1,xl2), 
            col=rgb(col,0,0,alpha[i]), lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n')  
    #plot( den$x, den$y/max(den$y), type = 'l', ylim = c(0,1), xlim = c(xl1,xl2), 
    #      col=rgb(col,0,0,alpha[i]), lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n')
    
    #tick = quantile(lagsim[[i]], seq(0,1,0.1))
    #den = hist(lagsim[[i]],breaks = tick,plot = FALSE )
    #plot( den$mids, den$density, type = 'l', ylim = c(0, 2.4), xlim = c(xl1,xl2), col=rgb(col,0,0,alpha[i]), lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n')
    
    par(new=T)
  }
  title( main = titlex,cex.main = 2.5 )
  title( xlab = "Response Time (s)",cex.lab = 2.5, line = 3.5)
  title( ylab = "Normalized density"  ,cex.lab = 2.5, line = 4.5)
  axis(1, labels=T,col.axis="black", las=1, cex.axis = 2) # Below
  axis(2, labels=T, col.axis="black", las=2, cex.axis = 2) # Left
  dev.off()
}



plotModelDaC<- function(fname = "Model.png", titlex = "", lagsim , xl1 = 0.35, xl2 = 2.2, yl2 = 1.8, col = 0)  {
  
  pdf(file = fname, width = 8, height = 6)
  
  lw1=2
  nl = len(lagsx)
  alpha = sort(seq(0,1,1/nl), decreasing = TRUE)
  #alpha[8] = alpha[7] 
  #cols = rainbow(nl)
  par(new=F)
  # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
  seqL = seq(1,nl) #c(1,4,7)#
  
  par(mar=c(5,6.8,3.5,2)+0.1)  
  for(i in seqL){
    n = len(lagsim[[i]])
    den = density(lagsim[[i]], adjust = 2.0) 
    de = den$y
    dex = den$x
    
    
#     den1 = density(lagsim[[i]], adjust = 2.0) 
#     den2 = density(lagsim[[i-1]], adjust = 2.0)    
#     dex = den1$x
#     de = den1$y + den2$y
#   if (i != 8){
#           den3 = density(lagsim[[i-2]], adjust = 2.0)
#           de = den1$y + den2$y + den3$y
#          }else{
#            de = den1$y + den2$y
#          }
#     
    
    #de/max(de)
    #plot( dex, de, type = 'l', ylim = c(0,yl2), xlim = c(xl1,xl2), 
    #      col=cols[i], lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n')  
    plot( den$x, den$y , type = 'l', ylim = c(0,yl2), xlim = c(xl1,xl2),  #/max(den$y)
          col=rgb(col,0,0,alpha[i]), lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n')
    
    #tick = quantile(lagsim[[i]], seq(0,1,0.1))
    #den = hist(lagsim[[i]],breaks = tick,plot = FALSE )
    #plot( den$mids, den$density, type = 'l', ylim = c(0, 2.4), xlim = c(xl1,xl2), col=rgb(col,0,0,alpha[i]), lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n')
    
    par(new=T)
  }
  title( main = titlex,cex.main = 2.5 )
  title( xlab = "Response Time (s)",cex.lab = 2.5, line = 3.5)
  title( ylab = "Density"  ,cex.lab = 2.5, line = 4.5)
  axis(1, labels=T,col.axis="black", las=1, cex.axis = 2) # Below
  axis(2, labels=T, col.axis="black", las=2, cex.axis = 2) # Left
  #legend("topright", as.character(paste(lagsx[seqL]  ) ) ,lty = 1,lwd=3,cex = 1.75 ,col =cols[seqL]   )
  #legend(col =,  )
 
  dev.off()
}





plotPar <- function(fname = "par.pdf", titlex = "Parameter vs Lag", dfs , yl1=0.3, yl2=0.8, xl1 = 0.0, xl2 = 7.00) {
  
  pdf(file = fname, width = 8, height = 8)
  par(new=F)
  # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
  par(mar=c(5,8,3.5,2)+0.1)  
#   
#   lmList <- vector("list",len(subs))
#   parL <- vector("list",len(subs))


  par(new=F)
  plot(dfs$llag, dfs$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
       lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', pch = 16)
#xaxt='n',yaxt='n'
  abline(lm(dfs$parV~dfs$llag), lwd = 2)
  add.error.bars(dfs$llag, dfs$parV,dfs$ci,0.1,col=1, lwd = 2);
#   # Plot Su Tau vs lag:
#   for(j in 1:len(subs)){    
#     par = estimSnmX1[[j]]$par
#     plot(llagsx, par[3:(nl+2)], col = rainbow(14)[[j]], pch=j, ylim = c(yl1,yl2), xlab = "", ylab = "")
#     par(new=T)
#     abline(lm(par[3:(nl+2)]~llagsx))
#     lmList[[j]] = lm(par[3:(nl+2)]~llagsx)$coefficients[[2]]
#     parL[[j]] = par
#     par(new=T)
#   }
  title( main = titlex, cex.main = 2.5)
  title( xlab = "Lag",cex.lab = 2.5, line = 3.5 )
  title( ylab = "Response time (s)"  ,cex.lab = 2.5, line = 5.5)
  axis(1, col.axis="black", las=1, cex.axis = 2, at = dfs$llag, labels = round(dfs$lag) ) # Below
  axis(2, labels=T, col.axis="black", las=2, cex.axis = 2) # Left

  dev.off()
}




plotParClip <- function(fname = "par.pdf", titlex = "Parameter vs Lag", dfs , yl1=0.3, yl2=0.8, exclude = c(1), xl2) {
  
  pdf(file = fname, width = 8, height = 8)
  par(new=F)
  # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
  par(mar=c(5,8,3.5,2)+0.1)  
  #   
  #   lmList <- vector("list",len(subs))
  #   parL <- vector("list",len(subs))
  xl1 = 0.0
  #xl2 = 7.00
  
  par(new=F)
  plot(dfs$llag, dfs$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
       lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', pch = 16)
  #xaxt='n',yaxt='n'
  #abline(lm(dfs$parV~dfs$llag), lwd = 2)
  
  dfs2 = dfs[dfs$lag != exclude,]
  ablineclip(lm(dfs2$parV~dfs2$llag) , x1 = min(dfs2$llag), x2 = max(dfs2$llag), lwd = 2)
  
  add.error.bars(dfs$llag, dfs$parV,dfs$ci,0.1,col=1, lwd = 2);
  #   # Plot Su Tau vs lag:
  #   for(j in 1:len(subs)){    
  #     par = estimSnmX1[[j]]$par
  #     plot(llagsx, par[3:(nl+2)], col = rainbow(14)[[j]], pch=j, ylim = c(yl1,yl2), xlab = "", ylab = "")
  #     par(new=T)
  #     abline(lm(par[3:(nl+2)]~llagsx))
  #     lmList[[j]] = lm(par[3:(nl+2)]~llagsx)$coefficients[[2]]
  #     parL[[j]] = par
  #     par(new=T)
  #   }
  title( main = titlex,cex.main = 2.5 )
  title( xlab = "Lag",cex.lab = 2.5, line = 3.5 )
  title( ylab = "Response time (s)"  ,cex.lab = 2.5, line = 5.5)
  axis(1, col.axis="black", las=1, cex.axis = 2, at = dfs$llag, labels = round(dfs$lag) ) # Below
  axis(2, labels=T, col.axis="black", las=2, cex.axis = 2) # Left
  
  
  dev.off()
}



add.error.bars <- function(X,Y,SE,w,col=1,lwd = 1){
  X0 = X; Y0 = (Y-SE); X1 =X; Y1 = (Y+SE);
  arrows(X0, Y0, X1, Y1, code=3,angle=90,length=w,col=col,lwd=lwd);
}


# For bar plot
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}




plotAcc <- function(fname = "Acc.png", titlex = "Accuracy vs Lag", dfs , 
                    yl1=0.0, yl2=1.0, axLine = 4.0,  xl2 = 7.10, lrmodel) {
  pdf(file = fname, width = 8, height = 6)

  par(new=F)
  # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
  par(mar=c(5,8,3.5,2)+0.1)  
  xl1 = 0.0
  
  par(new=F)
  plot(dfs$llag, dfs$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
       lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', pch = 16)
  #xaxt='n',yaxt='n'
  par(new=T)
  tt <- seq(1,7,0.05)
  yy <- 1/(1+exp(-(lrmodel@beta[1]+lrmodel@beta[2]*(tt))))
  plot(tt,yy, xlim = c(xl1,xl2), ylim = c(yl1,yl2), type='l', lwd=2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
  par(new = T)
  
  
  #abline(lm(dfs$parV~dfs$llag), lwd = 2)
  add.error.bars(dfs$llag, dfs$parV,dfs$ci,0.1,col=1, lwd = 2);
  title( main = titlex, cex.main = 2.5 )
  title( xlab = "Lag",cex.lab = 2.5, line = 3.5 )
  title( ylab = "Hit rate "  ,cex.lab = 2.5, line = axLine)
  axis(1, col.axis="black", las=1, cex.axis = 2, at = dfs$llag, labels = round(dfs$lag) ) # Below
  axis(2, labels=T, col.axis="black", las=2, cex.axis = 2) # Left
  
  par(new=F)
  
  dev.off()
}


# plotAccBar <- function(fname = "AccBar.png", titlex = "Accuracy vs Lag", dfs , yl1=0.0, yl2=102.0) {
#   
#   #png(filename = fname, width = 800, height = 600)
#   svg(filename = fname, width = 8, height = 6)
#   par(new=F)
#   # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
#   par(mar=c(5,8,2,2)+0.1)  
#   xl1 = 0.0
#   xl2 = 5.00
#   
#   par(new=F)
#   barplot(dfs$parV,ylim=c(yl1,yl2),  col=rgb(0.5,0.5,0.5,0.7), 
#           #plot(dfs$llag, dfs$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
#           lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n')
#   
#   add.error.bars(dfs$llag, dfs$parV,dfs$ci,0.1,col=1, lwd = 2);
#   title( main = titlex )
#   title( xlab = "Lag",cex.lab = 2.5, line = 3.5 )
#   title( ylab = "Percent correct"  ,cex.lab = 2.5, line = 4.0)
#   axis(1, col.axis="black", las=1, cex.axis = 2, at = dfs$llag, labels = round(dfs$lag2) ) # Below
#   axis(2, labels=T, col.axis="black", las=2, cex.axis = 2) # Left
#   
#   dev.off()
# }






plotRT <- function(fname = "RT.pdf", titlex = "RT vs Lag", dfs , 
                   yl1=0.7, yl2=1.5, xl1 = 0.0, xl2 = 7.00, xlb = "Lag",
                   axLine = 4.0, ylb = "Response Time (s) ") {
  

  
  pdf(file = fname, width = 8, height = 6)
  par(new=F)
  # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
  par(mar=c(5,8,3.5,2)+0.1)  

  
  par(new=F)
  plot(dfs$llag, dfs$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
       lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n')
  #xaxt='n',yaxt='n'
  abline(lm(dfs$parV~dfs$llag), lwd = 2)
  add.error.bars(dfs$llag, dfs$parV,dfs$ci,0.1,col=1, lwd = 2);
  title( main = titlex,cex.main = 2.5 )
  title( xlab = xlb,cex.lab = 2.5, line = 3.5 )
  title( ylab = ylb  ,cex.lab = 2.5, line = axLine)
  axis(1, col.axis="black", las=1, cex.axis = 2, at = as.numeric(as.character(dfs$llag)), 
       labels = round(dfs$lag) ) # Below
  axis(2, labels=T, col.axis="black", las=2, cex.axis = 2) # Left
  
  dev.off()
}





plotMulRT <- function(fname = "RT.pdf", titlex = "RT vs Lag", dfs0 , dfs1, dfs2,
                   yl1=0.7, yl2=1.5, xl1 = 0.0, xl2 = 7.00, xlb = "Lag",exclude = c(0),
                   axLine = 4.0, ylb = "Response Time (s) ", legendPos = "topleft", loess = FALSE, splitLag = 0) {
  
  pdf(file = fname, width = 8, height = 6)
  par(new=F)
  # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
  par(mar=c(5,8,3.5,2)+0.1)  
  
  
  par(new=F)
  plot(dfs0$llag, dfs0$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
       lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n')
  add.error.bars(dfs0$llag, dfs0$parV,dfs0$ci,0.1,col=1, lwd = 2);
  
  dfsC = dfs0[dfs0$llag != exclude,]
  if(loess){ 
    test <- loess(dfsC$parV~dfsC$llag, degree=2, span=2)
    lines(test$x,test$fitted,lwd=2)}
  else
    if(splitLag==0)
      ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), lwd = 2)
    else{ 
      ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), lwd = 2, lty = 3)
      dfsC = dfsC[dfsC$llag < splitLag,]
      ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), lwd = 2)
      }
  
   
  par(new=T)
  plot(dfs1$llag, dfs1$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
       lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "red")
  add.error.bars(dfs1$llag, dfs1$parV,dfs1$ci,0.1,col="red", lwd = 2 );
  
  dfsC = dfs1[dfs1$llag != exclude,]
  
  if(loess){ 
    test <- loess(dfsC$parV~dfsC$llag, degree=2,  span=2)
    lines(test$x,test$fitted,lwd=2, col = "red")
    }
  else
    if(splitLag==0)
      ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "red" ,lwd = 2)
    else{ 
      ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "red" ,lwd = 2, lty = 3)
      dfsC = dfsC[dfsC$llag < splitLag,]
      ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "red" ,lwd = 2) }
  
      
  par(new=T)
  plot(dfs2$llag, dfs2$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
       lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "blue")
  add.error.bars(dfs2$llag, dfs2$parV,dfs2$ci,0.1,col="blue", lwd = 2);
  
  dfsC = dfs2[dfs2$llag != exclude,]
  if(loess){ 
    test <- loess(dfsC$parV~dfsC$llag, degree=2, span=2)
    lines(test$x,test$fitted,lwd=2, col = "blue")
    }
  else
    if(splitLag==0)
      ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "blue" ,lwd = 2)
    else{ 
      ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "blue" ,lwd = 2, lty = 3)
      dfsC = dfsC[dfsC$llag < splitLag,]
      ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "blue" ,lwd = 2) }
  
  title( main = titlex ,cex.main = 2.5)
  title( xlab = xlb,cex.lab = 2.5, line = 3.5 )
  title( ylab = ylb  ,cex.lab = 2.5, line = axLine)
  axis(1, col.axis="black", las=1, cex.axis = 2, at = as.numeric(as.character(dfs2$llag)), 
       labels = round(dfs2$lag) ) # Below
  axis(2, labels=T, col.axis="black", las=2, cex.axis = 2) # Left
  
  legend(legendPos, c("Exp 1","Exp 2","Exp 3") ,lty = 1,lwd=1.5,
         cex = 1.5 ,col = c("black", "red", "blue")   )
  #legend("topright", c("1st repetition", "2nd repetition") ,lty = 1,lwd=2.5,cex = 1.2 ,col = c("black","red") )
  
  
  dev.off()
}


plotMulRT4 <- function(fname = "RT.pdf", titlex = "RT vs Lag", dfs0 , dfs1, dfs2,dfs3,
                      yl1=0.7, yl2=1.5, xl1 = 0.0, xl2 = 7.00, xlb = "Lag",exclude = c(0),
                      axLine = 4.0, ylb = "Response Time (s) ", legendPos = "topleft", loess = FALSE, splitLag = 0) {
  
  mainDir = setwd('/Users/ianmbright/Desktop/Desktop/Main_Desktop/Continuous_Recognition_Analysis')
  subDir = paste("/plots_",Sys.Date(),condFolder,sep = "")
  setwd(paste(mainDir,subDir,sep=""))
  
  pdf(file = fname, width = 8, height = 6)
  par(new=F)
  # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
  par(mar=c(5,8,3.5,2)+0.1)  
  
  
  par(new=F)
  plot(dfs0$llag, dfs0$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
       lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n')
  add.error.bars(dfs0$llag, dfs0$parV,dfs0$ci,0.1,col=1, lwd = 2);
  
  dfsC = dfs0[dfs0$llag != exclude,]
  if(loess){ 
    test <- loess(dfsC$parV~dfsC$llag, degree=2, span=2)
    lines(test$x,test$fitted,lwd=2)}
  else
    if(splitLag==0)
      ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), lwd = 2)
  else{ 
    ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), lwd = 2, lty = 3)
    dfsC = dfsC[dfsC$llag < splitLag,]
    ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), lwd = 2)
  }
  
  
  par(new=T)
  plot(dfs1$llag, dfs1$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
       lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "black", pch=2)
  add.error.bars(dfs1$llag, dfs1$parV,dfs1$ci,0.1,col="black", lwd = 2 );
  
  dfsC = dfs1[dfs1$llag != exclude,]
  
  if(loess){ 
    test <- loess(dfsC$parV~dfsC$llag, degree=2,  span=2)
    lines(test$x,test$fitted,lwd=2, col = "black", lty=2)
  }
  else
    if(splitLag==0)
      ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "black", lty=2 ,lwd = 2)
  else{ 
    ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "black" ,lwd = 2, lty =4)
    dfsC = dfsC[dfsC$llag < splitLag,]
    ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "black", lty=2,lwd = 2) }
  
  
  par(new=T)
  
  plot(dfs2$llag, dfs2$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
       lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "blue")
  add.error.bars(dfs2$llag, dfs2$parV,dfs2$ci,0.1,col="blue", lwd = 2);
  
  dfsC = dfs2[dfs2$llag != exclude,]
  if(loess){ 
    test <- loess(dfsC$parV~dfsC$llag, degree=2, span=2)
    lines(test$x,test$fitted,lwd=2, col = "blue")
  }
  else
    if(splitLag==0)
      ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "blue" ,lwd = 2)
  else{ 
    ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "blue" ,lwd = 2, lty = 3)
    dfsC = dfsC[dfsC$llag < splitLag,]
    ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "blue" ,lwd = 2) }
  
  par(new=T)
  
  plot(dfs3$llag, dfs3$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
       lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "blue", lty=2, pch=2)
  add.error.bars(dfs3$llag, dfs3$parV,dfs3$ci,0.1,col="blue", lwd = 2);
  
  dfsC = dfs3[dfs3$llag != exclude,]
  if(loess){ 
    test <- loess(dfsC$parV~dfsC$llag, degree=2, span=2)
    lines(test$x,test$fitted,lwd=2, col = "blue")
  }
  else
    if(splitLag==0)
      ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "blue", lty=2,lwd = 2)
  else{ 
    ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "blue" ,lwd = 2, lty = 3)
    dfsC = dfsC[dfsC$llag < splitLag,]
    ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "blue" ,lty = 2, lwd = 2) }
  
  
  
  title( main = titlex ,cex.main = 2.5)
  title( xlab = xlb,cex.lab = 2.5, line = 3.5 )
  title( ylab = ylb  ,cex.lab = 2.5, line = axLine)
  axis(1, col.axis="black", las=1, cex.axis = 2, at = as.numeric(as.character(dfs2$llag)), 
       labels = round(dfs2$lag) ) # Below
  axis(2, labels=T, col.axis="black", las=2, cex.axis = 2) # Left
  
  legend(legendPos, c("Exp 1","Exp 2","Exp 3", "Exp 4") ,lty = c(1,2,1,2),lwd=1.5,
         cex = 1.5 ,col = c("black", "black", "blue", "blue"), pch=c(1,2,1,2))
  #legend("topright", c("1st repetition", "2nd repetition") ,lty = 1,lwd=2.5,cex = 1.2 ,col = c("black","red") )
  
  setwd('/Users/ianmbright/Desktop/Desktop/Main_Desktop/Continuous_Recognition_Analysis')
  
  
  dev.off()
}


plotMulRT6 <- function(fname = "RT.pdf", titlex = "RT vs Lag", dfs0 , dfs1, dfs2, dfs3, dfs4, dfs5,
                       yl1=0.7, yl2=1.5, xl1 = 0.0, xl2 = 7.00, xlb = "Lag",exclude = c(0),
                       axLine = 4.0, ylb = "Response Time (s) ", legendPos = "topleft", HR = FALSE) {
  
  pdf(file = fname, width = 8, height = 6)
  par(new=F)
  # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
  par(mar=c(5,8,3.5,2)+0.1)  
  
  
  par(new=F)

  if(HR)
  {
    plot(dfs0$llag, dfs0$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
         lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n')
    add.error.bars(dfs0$llag, dfs0$parV,dfs0$ci,0.1,col=1, lwd = 2);
    par(new=T)

    tt <- seq(1,7,0.05)
    yy <- 1/(1+exp(-(LTS0LogReg@beta[1]+LTS0LogReg@beta[2]*(tt))))
    plot(tt,yy, xlim = c(xl1,xl2), ylim = c(yl1,yl2), type='l', lwd=2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
    par(new=T)
    
    plot(dfs1$llag, dfs1$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
         lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "black", pch=2)
    add.error.bars(dfs1$llag, dfs1$parV,dfs1$ci,0.1,col="black", lwd = 2 );
    par(new=T)
    
    tt <- seq(1,7,0.05)
    yy <- 1/(1+exp(-(LTS1LogReg@beta[1]+LTS1LogReg@beta[2]*(tt))))
    plot(tt,yy, xlim = c(xl1,xl2), ylim = c(yl1,yl2), type='l', lwd=2,lty=2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
    
    par(new=T)
    
    plot(dfs2$llag, dfs2$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
         lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "blue")
    add.error.bars(dfs2$llag, dfs2$parV,dfs2$ci,0.1,col="blue", lwd = 2);
    
    par(new=T)
    tt <- seq(1,7,0.05)
    yy <- 1/(1+exp(-(LTS2LogReg@beta[1]+LTS2LogReg@beta[2]*(tt))))
    plot(tt,yy, xlim = c(xl1,xl2), ylim = c(yl1,yl2), type='l', lwd=2, col = "blue",xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
    
    par(new=T)
    
    plot(dfs3$llag, dfs3$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
         lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "blue", lty=2, pch=2)
    add.error.bars(dfs3$llag, dfs3$parV,dfs3$ci,0.1,col="blue", lwd = 2);
    
    par(new=T)
    
    tt <- seq(1,7,0.05)
    yy <- 1/(1+exp(-(LTS3LogReg@beta[1]+LTS3LogReg@beta[2]*(tt))))
    plot(tt,yy, xlim = c(xl1,xl2), ylim = c(yl1,yl2), type='l', lwd=2, xlab = '', col="blue",lty=2,ylab = '', xaxt = 'n', yaxt = 'n')
    
    par(new=T)
    
    plot(dfs4$llag, dfs4$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
         lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "red")
    add.error.bars(dfs4$llag, dfs4$parV,dfs4$ci,0.1,col="red", lwd = 2);
    
    par(new=T)
    
    tt <- seq(1,7,0.05)
    yy <- 1/(1+exp(-(LTS4LogReg@beta[1]+LTS4LogReg@beta[2]*(tt))))
    plot(tt,yy, xlim = c(xl1,xl2), ylim = c(yl1,yl2), type='l', lwd=2, xlab = '', col="red",ylab = '', xaxt = 'n', yaxt = 'n')
    
    par(new=T)
    
    plot(dfs5$llag, dfs5$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
         lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "red", lty=2, pch=2)
    add.error.bars(dfs5$llag, dfs5$parV,dfs5$ci,0.1,col="red", lwd = 2);
    
    par(new=T)
    
    tt <- seq(1,7,0.05)
    yy <- 1/(1+exp(-(LTS5LogReg@beta[1]+LTS5LogReg@beta[2]*(tt))))
    plot(tt,yy, xlim = c(xl1,xl2), ylim = c(yl1,yl2), type='l', lwd=2, xlab = '', col="red",lty=2,ylab = '', xaxt = 'n', yaxt = 'n')
    
    
  }
  else{
    plot(dfs0$llag, dfs0$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
         lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n')
    add.error.bars(dfs0$llag, dfs0$parV,dfs0$ci,0.1,col=1, lwd = 2);
    
    dfsC = dfs0[dfs0$llag != exclude,]
    ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), lwd = 2)
    
    par(new=T)
    plot(dfs1$llag, dfs1$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
         lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "black", pch=2)
    add.error.bars(dfs1$llag, dfs1$parV,dfs1$ci,0.1,col="black", lwd = 2 );
    
    dfsC = dfs1[dfs1$llag != exclude,]
    ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "black", lty=2 ,lwd = 2)
    par(new=T)
    
    plot(dfs2$llag, dfs2$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
         lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "blue")
    add.error.bars(dfs2$llag, dfs2$parV,dfs2$ci,0.1,col="blue", lwd = 2);
    
    dfsC = dfs2[dfs2$llag != exclude,]
    ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "blue" ,lwd = 2)
    
    par(new=T)
    
    plot(dfs3$llag, dfs3$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
         lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "blue", lty=2, pch=2)
    add.error.bars(dfs3$llag, dfs3$parV,dfs3$ci,0.1,col="blue", lwd = 2);
    
    dfsC = dfs3[dfs3$llag != exclude,]
    
    ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "blue", lty=2,lwd = 2)
    
    par(new=T)
    
    plot(dfs4$llag, dfs4$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
         lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "red")
    add.error.bars(dfs4$llag, dfs4$parV,dfs4$ci,0.1,col="red", lwd = 2);
    
    dfsC = dfs4[dfs4$llag != exclude,]
    
    ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "red" ,lwd = 2)
    
    par(new=T)
    
    plot(dfs5$llag, dfs5$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
         lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "red", lty=2, pch=2)
    add.error.bars(dfs5$llag, dfs5$parV,dfs5$ci,0.1,col="red", lwd = 2);
    
    dfsC = dfs5[dfs5$llag != exclude,]
    ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "red", lty=2,lwd = 2)
    
  }
  
  title( main = titlex ,cex.main = 2.5)
  title( xlab = xlb,cex.lab = 2.5, line = 3.5 )
  title( ylab = ylb  ,cex.lab = 2.5, line = axLine)
  axis(1, col.axis="black", las=1, cex.axis = 2, at = as.numeric(as.character(dfs2$llag)), 
       labels = round(dfs2$lag) ) # Below
  axis(2, labels=T, col.axis="black", las=2, cex.axis = 2) # Left
  
  legend(legendPos, c("Exp 1","Exp 2","Exp 3", "Exp 4", "Exp 5", "Exp 6") ,lty = c(1,2,1,2,1,2),lwd=1.5,
         cex = 1.5 ,col = c("black", "black", "blue", "blue","red","red"), pch=c(1,2,1,2,1,2), ncol=3)
  #legend("topright", c("1st repetition", "2nd repetition") ,lty = 1,lwd=2.5,cex = 1.2 ,col = c("black","red") )
  par(new=F)
 
  dev.off()
}

plotMulLMF6 <- function(fname = "LMF.pdf", titlex = "Lag Modulation Factor by Quantile", dfs0 , dfs1, dfs2, dfs3, dfs4, dfs5,
                       yl1=0, yl2=40, xl1 = 0.0, xl2 = 0.40, xlb = "Quantile",exclude = c(0),
                       axLine = 4.0, ylb = "Lag Modulation Factor (ms) ", legendPos = "bottomleft") {
  
  pdf(file = fname, width = 8, height = 6)
  par(new=F)
  # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
  par(mar=c(5,8,3.5,2)+0.1)  
  
  
  par(new=F)
  

    plot(dfs0$lag, dfs0$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
         lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n')
    add.error.bars(dfs0$lag, dfs0$parV,dfs0$ci,0.1,col=1, lwd = 2);
    abline(lm(dfs0$parV~dfs0$lag), lwd = 2)
    par(new=T)
    
    plot(dfs1$lag, dfs1$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
         lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "black", pch=2)
    add.error.bars(dfs1$lag, dfs1$parV,dfs1$ci,0.1,col="black", lwd = 2 );
    abline(lm(dfs1$parV~dfs1$lag), col = "black", lty=2 ,lwd = 2)
    par(new=T)
    
    plot(dfs2$lag, dfs2$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
         lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "blue")
    add.error.bars(dfs2$lag, dfs2$parV,dfs2$ci,0.1,col="blue", lwd = 2);
    abline(lm(dfs2$parV~dfs2$lag), col = "blue" ,lwd = 2)
    
    par(new=T)
    
    plot(dfs3$lag, dfs3$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
         lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "blue", lty=2, pch=2)
    add.error.bars(dfs3$lag, dfs3$parV,dfs3$ci,0.1,col="blue", lwd = 2);
    abline(lm(dfs3$parV~dfs3$lag) , col = "blue", lty=2,lwd = 2)
    
    par(new=T)
    
    plot(dfs4$lag, dfs4$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
         lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "red")
    add.error.bars(dfs4$lag, dfs4$parV,dfs4$ci,0.1,col="red", lwd = 2);
    abline(lm(dfs4$parV~dfs4$lag), col = "red" ,lwd = 2)
    
    par(new=T)
    
    plot(dfs5$lag, dfs5$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
         lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "red", lty=2, pch=2)
    add.error.bars(dfs5$lag, dfs5$parV,dfs5$ci,0.1,col="red", lwd = 2);
    abline(lm(dfs5$parV~dfs5$lag), col = "red", lty=2,lwd = 2)

  
  title( main = titlex ,cex.main = 2.4)
  title( xlab = xlb,cex.lab = 2.5, line = 3.5 )
  title( ylab = ylb  ,cex.lab = 2.5, line = axLine)
  axis(1, col.axis="black", las=1, cex.axis = 2, at = c(0,.1,.2,.3,.4), 
       labels = c(0,.1,.2,.3,.4)) # Below
  axis(2, labels=T, col.axis="black", las=2, cex.axis = 2) # Left
  
  legend(legendPos, c("Exp 1","Exp 2","Exp 3", "Exp 4", "Exp 5", "Exp 6") ,lty = c(1,2,1,2,1,2),lwd=1.5,
         cex = 1.5 ,col = c("black", "black", "blue", "blue","red","red"), pch=c(1,2,1,2,1,2), ncol=3)
  #legend("topright", c("1st repetition", "2nd repetition") ,lty = 1,lwd=2.5,cex = 1.2 ,col = c("black","red") )
  par(new=F)
  
  dev.off()
}



plotSingleLMF <- function(fname = "LMFMul.pdf", titlex = "Lag Modulation Factor by Quantile", dfs5,
                        yl1=0, yl2=40, xl1 = 0.0, xl2 = 0.40, xlb = "Quantile",exclude = c(0),
                        axLine = 4.0, ylb = "Lag Modulation Factor (ms) ", legendPos = "bottomleft") {
  
  pdf(file = fname, width = 8, height = 6)
  par(new=F)
  # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
  par(mar=c(5,8,3.5,2)+0.1)  
  
  
  par(new=F)
  
  
  
  plot(dfs5$lag, dfs5$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
       lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "red", lty=2, pch=2)
  add.error.bars(dfs5$lag, dfs5$parV,dfs5$ci,0.1,col="red", lwd = 2);
  abline(lm(dfs5$parV~dfs5$lag), col = "red", lty=2,lwd = 2)
  
  
  title( main = titlex ,cex.main = 2.4)
  title( xlab = xlb,cex.lab = 2.5, line = 3.5 )
  title( ylab = ylb  ,cex.lab = 2.5, line = axLine)
  axis(1, col.axis="black", las=1, cex.axis = 2, at = c(0,.1,.2,.3,.4), 
       labels = c(0,.1,.2,.3,.4)) # Below
  axis(2, labels=T, col.axis="black", las=2, cex.axis = 2) # Left
  
  #legend(legendPos, c("Exp 6") ,lty = c(2),lwd=1.5,cex = 1.5 ,col = c("red"), pch=c(2))
  #legend("topright", c("1st repetition", "2nd repetition") ,lty = 1,lwd=2.5,cex = 1.2 ,col = c("black","red") )
  par(new=F)
  
  dev.off()
}



plotRepExpWise  <- function(fname = "medRepExpRT.pdf", titlex = "RT vs Lag", dfs1 , rep1,
                            yl1=0.7, yl2=1.5, xl1 = 1.0, xl2 = 7.00, xlb = "Lag",exclude = c(0), width = 8,
                            axLine = 4.0, ylb = "Response Time (s) ", legendPos = "topleft", loess = FALSE, splitLag = 0, coli = "black") {
  
  pdf(file = fname, width = width, height = 6)
  #par(new=T)
  # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
  par(mar=c(5,8,3.5,2)+0.1)  
  
  repLags = unique(rep1$llag)
  dfs1 = dfs1[(dfs1$llag %in% repLags),]
  
  par(new=F)
  plot(dfs1$llag, dfs1$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
       lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = coli)
  add.error.bars(dfs1$llag, dfs1$parV,dfs1$ci,0.1,col=coli, lwd = 2 );
  
  dfsC = dfs1[dfs1$llag != exclude,]
  
  if(loess){ 
    test <- loess(dfsC$parV~dfsC$llag, degree=2,  span=2)
    lines(test$x,test$fitted,lwd=2, col = "red")
  }
  else
    if(splitLag==0)
      ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = coli ,lwd = 2)
  else{ 
    ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = coli,lwd = 2, lty = 3)
    dfsC = dfsC[dfsC$llag < splitLag,]
    ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = coli,lwd = 2) }
  
  par(new=T)
  plot(rep1$llag, rep1$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), pch = 4,
       lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = coli)
  add.error.bars(rep1$llag, rep1$parV,rep1$ci,0.1,col=coli, lwd = 2 );
  ablineclip(lm(rep1$parV~rep1$llag) , x1 = min(rep1$llag), x2 = max(rep1$llag), col = coli ,lwd = 2, lty = 3)
  
  title( main = titlex ,cex.main = 2.5)
  title( xlab = xlb,cex.lab = 2.5, line = 3.5 )
  title( ylab = ylb  ,cex.lab = 2.5, line = axLine)
  axis(1, col.axis="black", las=1, cex.axis = 2, at = as.numeric(as.character(dfs1$llag)), 
       labels = round(dfs1$lag) ) # Below
  axis(2, labels=T, col.axis="black", las=2, cex.axis = 2) # Left
  
  legend(legendPos, c("Rep once","Rep twice") ,lty = c(1,3) ,lwd=1.5,
         cex = 1.5 ,col = c("black")   )
  #legend("topright", c("1st repetition", "2nd repetition") ,lty = 1,lwd=2.5,cex = 1.2 ,col = c("black","red") )
  
  
  dev.off()
}


plotMulRTwRep <- function(fname = "RT.pdf", titlex = "RT vs Lag", dfs0 , dfs1, dfs2, rep0, rep1, rep2,
                      yl1=0.7, yl2=1.5, xl1 = 0.0, xl2 = 7.00, xlb = "Lag",exclude = c(0),
                      axLine = 4.0, ylb = "Response Time (s) ", legendPos = "topleft", loess = FALSE, splitLag = 0) {
  
  pdf(file = fname, width = 8, height = 6)
  par(new=F)
  # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
  par(mar=c(5,8,3.5,2)+0.1)  
  
  
  par(new=F)
  plot(dfs0$llag, dfs0$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
       lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n')
  add.error.bars(dfs0$llag, dfs0$parV,dfs0$ci,0.1,col=1, lwd = 2);
  
  dfsC = dfs0[dfs0$llag != exclude,]
  if(loess){ 
    test <- loess(dfsC$parV~dfsC$llag, degree=2, span=2)
    lines(test$x,test$fitted,lwd=2)}
  else
    if(splitLag==0)
      ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), lwd = 2)
  else{ 
    ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), lwd = 2, lty = 3)
    dfsC = dfsC[dfsC$llag < splitLag,]
    ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), lwd = 2)
  }
  
  
  par(new=T)
  plot(dfs1$llag, dfs1$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
       lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "red")
  add.error.bars(dfs1$llag, dfs1$parV,dfs1$ci,0.1,col="red", lwd = 2 );
  
  dfsC = dfs1[dfs1$llag != exclude,]
  
  if(loess){ 
    test <- loess(dfsC$parV~dfsC$llag, degree=2,  span=2)
    lines(test$x,test$fitted,lwd=2, col = "red")
  }
  else
    if(splitLag==0)
      ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "red" ,lwd = 2)
  else{ 
    ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "red" ,lwd = 2, lty = 3)
    dfsC = dfsC[dfsC$llag < splitLag,]
    ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "red" ,lwd = 2) }
  
  
  par(new=T)
  plot(dfs2$llag, dfs2$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
       lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "blue")
  add.error.bars(dfs2$llag, dfs2$parV,dfs2$ci,0.1,col="blue", lwd = 2);
  
  dfsC = dfs2[dfs2$llag != exclude,]
  if(loess){ 
    test <- loess(dfsC$parV~dfsC$llag, degree=2, span=2)
    lines(test$x,test$fitted,lwd=2, col = "blue")
  }
  else
    if(splitLag==0)
      ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "blue" ,lwd = 2)
  else{ 
    ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "blue" ,lwd = 2, lty = 3)
    dfsC = dfsC[dfsC$llag < splitLag,]
    ablineclip(lm(dfsC$parV~dfsC$llag) , x1 = min(dfsC$llag), x2 = max(dfsC$llag), col = "blue" ,lwd = 2) }
  
  
  par(new=T)
  plot(rep0$llag, rep0$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), pch = 4,
       lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "black")
  add.error.bars(rep0$llag, rep0$parV,rep0$ci,0.1,col="black", lwd = 2 );
  ablineclip(lm(rep0$parV~rep0$llag) , x1 = min(rep0$llag), x2 = max(rep0$llag), col = "black" ,lwd = 2, lty = 3)
  
  par(new=T)
  plot(rep1$llag, rep1$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), pch = 4,
       lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "red")
  add.error.bars(rep1$llag, rep1$parV,rep1$ci,0.1,col="red", lwd = 2 );
  ablineclip(lm(rep1$parV~rep1$llag) , x1 = min(rep1$llag), x2 = max(rep1$llag), col = "red" ,lwd = 2, lty = 3)
  
  par(new=T)
  plot(rep2$llag, rep2$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), pch = 4,
       lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col = "blue")
  add.error.bars(rep2$llag, rep2$parV,rep2$ci,0.1,col="blue", lwd = 2 );
  ablineclip(lm(rep2$parV~rep2$llag) , x1 = min(rep2$llag), x2 = max(rep2$llag), col = "blue" ,lwd = 2, lty = 3)
  
  
  
  
  
  title( main = titlex ,cex.main = 2.5)
  title( xlab = xlb,cex.lab = 2.5, line = 3.5 )
  title( ylab = ylb  ,cex.lab = 2.5, line = axLine)
  axis(1, col.axis="black", las=1, cex.axis = 2, at = as.numeric(as.character(dfs2$llag)), 
       labels = round(dfs2$lag) ) # Below
  axis(2, labels=T, col.axis="black", las=2, cex.axis = 2) # Left
  
  legend(legendPos, c("Exp 1","Exp 2","Exp 3") ,lty = 1,lwd=1.5,
         cex = 1.5 ,col = c("black", "red", "blue")   )
  #legend("topright", c("1st repetition", "2nd repetition") ,lty = 1,lwd=2.5,cex = 1.2 ,col = c("black","red") )
  

  dev.off()
}









plotRTclip <- function(fname = "RT.pdf", titlex = "RT vs Lag", dfs , 
                   yl1=0.7, yl2=1.5, xl1 = 0.0, xl2 = 7.00, xlb = "Lag",
                   axLine = 4.0, ylb = "Response Time (s) ", exclude = c(0)) {
  
  pdf(file = fname, width = 8, height = 6)
  par(new=F)
  # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
  par(mar=c(5,8,3.5,2)+0.1)  
  
  
  par(new=F)
  plot(dfs$llag, dfs$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
       lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n')
  #xaxt='n',yaxt='n'
  add.error.bars(dfs$llag, dfs$parV,dfs$ci,0.1,col=1, lwd = 2);
  
  #abline(lm(dfs$parV~dfs$llag), lwd = 2)
  dfs2 = dfs[dfs$lag != exclude,]
  ablineclip(lm(dfs2$parV~dfs2$llag) , x1 = min(dfs2$llag), x2 = max(dfs2$llag))
  
  title( main = titlex,cex.main = 2.5 )
  title( xlab = xlb,cex.lab = 2.5, line = 3.5 )
  title( ylab = ylb  ,cex.lab = 2.5, line = axLine)
  axis(1, col.axis="black", las=1, cex.axis = 2, at = as.numeric(as.character(dfs$llag)), 
       labels = round(dfs$lag) ) # Below
  axis(2, labels=T, col.axis="black", las=2, cex.axis = 2) # Left
  
  dev.off()
}




plotRep <- function(fname = "Rep.pdf", titlex = "RT vs Lag", dfs1 , 
                   yl1=0.7, yl2=1.5, xl1 = 0.0, xl2 = 7.00, xlb = "Lag",
                   axLine = 4.0, ylb = "Response Time (s) ") {
  
  
  pdf(file = fname, width = 8, height = 6)
  par(new=F)
  # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
  par(mar=c(5,8,3.5,2)+0.1)  
  
  
  par(new=F)
  for(i in 1:2){
    dfs = dfs1[[i]]
    plot(dfs$llag, dfs$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
         lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', pch = i, col = c("red", "black", "blue")[i] )
    abline(lm(dfs$parV~dfs$llag), lwd = 2,col = c("red", "black", "blue")[i])
    
    print(summary(lm(dfs$parV~dfs$llag)))
    add.error.bars(dfs$llag, dfs$parV,dfs$ci,0.1, lwd = 2,col = c("red", "black", "blue")[i] )
   
    par(new=T)
    
  }

  title( main = titlex, cex.main = 2.5 )
  title( xlab = xlb,cex.lab = 2.5, line = 3.5 )
  title( ylab = ylb  ,cex.lab = 2.5, line = axLine)
  lab = 2^seq(0,7)
  axis(1, col.axis="black", las=1, cex.axis = 2, at = log2(lab), 
       labels = lab ) # Below
  axis(2, labels=T, col.axis="black", las=2, cex.axis = 2) # Left
  
  dev.off()
}



plotRepM<- function(fname = "Model.png", titlex = "", lagsim , xl1 = 0.46, xl2 = 1.8, yl2 = 2, col = 0)  {
  
  pdf(file = fname, width = 8, height = 6)
  
  lw1=2
  nl = 2
  alpha = c(0.2,0.8)
  
  par(new=F)
  # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
  par(mar=c(5,6.8,3.5,2)+0.1)  
  for(i in 1:2){
    datsel <- lagsim$RT[lagsim$rseq==i]
    n = len(datsel)
    
    #ECDF
    #plot( sort( datsel), (1:n)/n, type = 'l', ylim = c(0, 1), xlim = c(xl1,xl2), col=rgb(col,0,0,alpha[i]), lwd = 3, xlab="",ylab="", xaxt='n',yaxt='n')
    
    den = density(datsel, adjust = 2.0) 
    plot( den$x, den$y , type = 'l', ylim = c(0,yl2), xlim = c(xl1,xl2),  #/max(den$y)
          col=rgb(col,0,0,alpha[i]), lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n')
    
    
    
    #n = len(datsel)
    #plot( sort(datsel), (1:n)/n, type = 'l', ylim = c(0, 1), xlim = c(xl1,xl2), col=rgb((i-1)/nl,(i-1)/nl,(i-1)/nl,0.5), lwd = 2, xlab="",ylab="")
    #par(new=T)
    #plot(seq(xl1,xl2,0.001),lagdistr[[i]],col=rgb(1,0,0,alpha[i]) , lwd = 2, type = "l", xlab="",ylab="")
    #par(new=T)
    #n = len(lagsim[[i]]) + 2
    #plot( sort( c(0,0,lagsim[[i]])), (1:n)/n, type = 'l', ylim = c(0, 1), xlim = c(xl1,xl2), col=rgb(col,0,0,alpha[i]), lwd = 3, xlab="",ylab="", xaxt='n',yaxt='n')
    par(new=T)
  }
  title( main = titlex, cex.main = 2.5 )
  title( xlab = "Response Time (s)",cex.lab = 2.5, line = 3.5)
  title( ylab = "Density"  ,cex.lab = 2.5, line = 4.5)
  axis(1, labels=T,col.axis="black", las=1, cex.axis = 2) # Below
  axis(2, labels=T, col.axis="black", las=2, cex.axis = 2) # Left
  
  #   text(1.4, y = 0.3, labels = "Scale:",cex = 2)
  #   segments(1.4, 0.2, x1 = 1.5, y1 = .2, lty = 1, lwd = 3)
  #   text(1.65, y = 0.2, labels = "100 ms",cex = 2)
  
  #   text(1.0, y = 0.3, labels = "Scale:",cex = 2)
  #   segments(1.0, 0.2, x1 = 1.1, y1 = .2, lty = 1, lwd = 3)
  #   text(1.15, y = 0.2, labels = "100 ms",cex = 2)
  
  dev.off()
}





plotBar <- function(fname = "RT.pdf", titlex = "RT vs Lag", dfs , 
                   yl1=0.7, yl2=1.5, xl1 = 0.0, xl2 = 7.00, xlb = "Lag",
                   axLine = 4.0, ylb = "Response Time (s) ") {

  
  pdf(file = fname, width = 8, height = 6)
  par(new=F)
  # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
  par(mar=c(5,8,3.5,2)+0.1)  
  
  
  par(new=F)
  barx <- barplot(dfs$rt, beside=TRUE,col=gray.colors(2), ylim=c(0.0,1), 
                  names.arg=c("1","2"), space = 0.4, xlim=c(0,1), width = 0.3,
                  axis.lty=1, xlab="", ylab="",xaxt='n',yaxt='n')
  #plot(dfs$llag, dfs$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
  #     lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n')
  #xaxt='n',yaxt='n'
  #abline(lm(dfs$parV~dfs$llag), lwd = 2)
  #add.error.bars(dfs$llag, dfs$parV,dfs$ci,0.1,col=1, lwd = 2);
  error.bar(barx,dfs$rt,dfs$ci,lwd = 3, length = 0.13)
  title( main = titlex,cex.main = 2.5 )
  title( xlab = xlb,cex.lab = 2.5, line = 3.5 )
  title( ylab = ylb  ,cex.lab = 2.5, line = axLine)
  axis(1, col.axis="black", las=1, cex.axis = 2, at = c(.3,.7), 
       labels = round(dfs$llag) ) # Below
  axis(2, labels=T, col.axis="black", las=2, cex.axis = 2) # Left
  
  
 
  #error.bar(barx,yy1,ee1)
  #text(5,95,"*",cex=2)
  
  
  
  
  dev.off()
}




plotQ <- function(fname = "RT.pdf", titlex = "RT vs Lag", dfs , dfs2,
                   yl1=0.4, yl2=1.5, xl1 = 0.1, xl2 = 1.00, xlb = "Quantiles",
                   axLine = 4.0, ylb = "Response Time (s) ") {
  
  
  
  pdf(file = fname, width = 8, height = 6)
  par(new=F)
  # (bottom, left, top, right) - c(5, 4, 4, 2) + 0.1.
  par(mar=c(5,8,3.5,2)+0.1)  
  
  
  par(new=F)
  plot(dfs$lag, dfs$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
       lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n')
  abline(lm(dfs$parV~dfs$lag), lwd = 2)
  add.error.bars(dfs$lag, dfs$parV,dfs$ci,0.1,col=1, lwd = 2);
  
  dfs = dfs2
  par(new=T)
  plot(dfs$lag, dfs$parV, type = 'p', ylim = c(yl1, yl2), xlim = c(xl1,xl2), 
       lwd = 2, xlab="",ylab="", xaxt='n',yaxt='n', col="red")
  abline(lm(dfs$parV~dfs$lag), lwd = 2, col="red")
  add.error.bars(dfs$lag, dfs$parV,dfs$ci,0.1,col="red", lwd = 2);
  
   
  title( main = titlex, cex.main = 2.5)
  title( xlab = xlb,cex.lab = 2.5, line = 3.5 )
  title( ylab = ylb  ,cex.lab = 2.5, line = axLine)
  axis(1, col.axis="black", las=1, cex.axis = 2, at = as.numeric(as.character(dfs$lag)), 
       labels = round(dfs$lag,1) ) # Below
  axis(2, labels=T, col.axis="black", las=2, cex.axis = 2) # Left
  legend("bottomright", c("1st repetition", "2nd repetition") ,lty = 1,lwd=2.5,cex = 1.2 ,col = c("black","red") )
  
  dev.off()
}


###################################################### 
##########       AIC BIC Processing         ########## 
######################################################


getAIC <- function(MM1sel, nLL, k = 10, corr = TRUE){ 

MM1selC = MM1sel[MM1sel$RT != 0 & MM1sel$lag != 256,]
respSub <- aggregate(MM1selC$RT, by=list(MM1selC$sub), FUN=count)
#respSubL <- aggregate(MM1selC$RT, by=list(MM1selC$sub,MM1selC$lag), FUN=count)
AIC = 2 * k  + 2 * nLL 
if(corr) return( AIC +  ( 2* k * (k + 1) ) / (respSub$x - k - 1) )
else print("uncorected")

}


getBIC <- function(MM1sel, nLL , k = 10){ 
  
  MM1selC = MM1sel[MM1sel$RT != 0 & MM1sel$lag != 256,]
  respSub <- aggregate(MM1selC$RT, by=list(MM1selC$sub), FUN=count)
  #respSubL <- aggregate(MM1selC$RT, by=list(MM1selC$sub,MM1selC$lag), FUN=count)
  #names(respSubL) = c("sub","lag","count")
  #respL <- aggregate(respSubL$count, by=list(respSubL$lag), FUN=median)
  #boxplot(count~lag, data= respSubL)
  return(BIC = 2 * nLL  + k * log(respSub$x))
  
}


###################################################### 
##########      Summarizing functions       ########## 
######################################################



## Summarizes data, handling within-subjects variables by removing inter-subject variability.
## It will still work if there are no within-S variables.
## Gives count, un-normed mean, normed mean (with same between-group mean),
##   standard deviation, standard error of the mean, and confidence interval.
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}


## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}



## Norms the data within specified groups in a data frame; it normalizes each
## subject (identified by idvar) so that they have the same mean, within each group
## specified by betweenvars.
##   data: a data frame.
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   na.rm: a boolean that indicates whether to ignore NA's
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  require(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}




###################################################### 
## Give this function the aggregated statistic by   ##
## subject and lag and it will get the within       ##
## subject summary and do log2 conversions for lag. ##
######################################################

wrapperWithinSub <- function(rtMn){ 
  
  names(rtMn) = c("sub","lag","rt")
  #rtMn$lag2 = as.numeric(as.character(rtMn$lag))
  rtMn$llag = log2(as.numeric(as.character(rtMn$lag)))
  
  nCs <- summarySEwithin(rtMn, measurevar="rt", withinvars="llag",
                         idvar="sub", na.rm=FALSE, conf.interval=.95)
  
  nCs$llag = as.numeric(as.character(nCs$llag))
  nCs$lag = 2^(nCs$llag)
  nCs$parV = nCs$rt
  return(nCs)
}

wrapperWithinSub2 <- function(rtMn){ 
  
  names(rtMn) = c("sub","llag","rt","rep")
  
  nCs <- summarySEwithin(rtMn, measurevar="rt", withinvars=c("llag","rep"),
                  idvar="sub", na.rm=TRUE, conf.interval=.95)
  
  
  nCs$llag = as.numeric(as.character(nCs$llag))
  nCs$lag = 2^(nCs$llag)
  nCs$parV = nCs$rt
  return(nCs)
}

wrapperWithinSub3 <- function(rtMn){ 
  
  names(rtMn) = c("sub","llag","rep","rt")
  #rtMn$llag <- log2(as.numeric(rtMn$lag))
  
  
  nCs <- summarySEwithin(rtMn, measurevar="rt", withinvars=c("llag","rep"),
                         idvar="sub", na.rm=TRUE, conf.interval=.95)
  
  
  nCs$llag = as.numeric(as.character(nCs$llag))
  nCs$lag = 2^(nCs$llag)
  nCs$parV = nCs$rt
  return(nCs)
}
