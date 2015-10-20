library(openintro)

Htest<-function (dist="n",mean=0,se=1,size=200,p=2,tails=1){
    X <- seq(-4, 4, 0.01)
    if (dist=="n"){
        Y <- dt(X,mean,se)  
    }
    else if (dist =="t"){
        Y <- dt(X,size)  
    }
    
    
    plot(X, Y,
         type = 'l',
         axes = FALSE,
         xlim = c(-3.4, 3.4),
         font.lab = 3,
         font.axis=3)
    at = c(-5, -p, 0, p, 5)
    labels = c(-5, 'T=-0.87', 0, 'T=0.87',5)
    axis(1, at, labels, cex.axis = 0.8,font.axis=3)
    abline(h=0)
    
if (tails==2) {
    these <- which(X < -p)
    polygon(c(X[these[1]], X[these], X[rev(these)[1]]),
            c(0, Y[these], 0),
            col = "#569BBDC0")
    
}    
    
    these <- which(X > p)
    polygon(c(X[these[1]], X[these], X[rev(these)[1]]),
            c(0, Y[these], 0),
            col = "#569BBDC0")
    
    
    lines(c(0, 0), c(0, dt(0,199)),
          col = "#569BBDC0",
          lty = 3)    
}


Htest("n",size=199,p=1.39,tails=1)

# Htest("t",size=10,p=2)

