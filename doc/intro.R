## -----------------------------------------------------------------------------
gammaCI <- function(alpha,lambda,beta){
  a = 0
  if (qgamma(1-alpha,shape = lambda,scale = beta) > (lambda-1)/beta){
    b = (lambda-1)/beta
  }
  else{
    b = qgamma(1-alpha,shape = lambda,scale = beta)
  }
  for (i in 1:40) {
    c = (a+b)/2
    ap = pgamma(a,shape = lambda,scale = beta)
    aint = pgamma(a,shape = lambda,scale = beta)+alpha
    bp = pgamma(b,shape = lambda,scale = beta)
    bint = pgamma(b,shape = lambda,scale = beta)+alpha
    a_interval = qgamma(aint,shape = lambda,scale = beta)-qgamma(ap,shape = lambda,scale = beta)
    b_interval = qgamma(bint,shape = lambda,scale = beta)-qgamma(bp,shape = lambda,scale = beta)
    if (b_interval>a_interval)
      b = c
    else
      a = c
  }
  cat('when the confidence is',alpha,'confidence interval is','[',qgamma(ap,shape = lambda,scale = beta),',',qgamma(aint,shape = lambda,scale = beta),']')
}

## -----------------------------------------------------------------------------
gammasolution <- function(value,lambda,beta,prec){
  leftx <- function(value,lambda,beta,prec){
    a=0
    b=(lambda-1)/beta
    ga=dgamma(a,shape = lambda,scale = beta)-value
    gb=dgamma(b,shape = lambda,scale = beta)-value
    while(b-a>prec){
      c=(a+b)/2
      gc=dgamma(c,shape = lambda,scale = beta)-value
      if (ga*gc < 0){
        b=c
        gb=gc
      }
      else{
        a=c
        ga=gc
      }
    }
    (a+b)/2
  }
  
  rightx <- function(value,lambda,beta,prec){
    a=20
    b=(lambda-1)/beta
    ga=dgamma(a,shape = lambda,scale = beta)-value
    gb=dgamma(b,shape = lambda,scale = beta)-value
    while(b-a>prec){
      c=(a+b)/2
      gc=dgamma(c,shape = lambda,scale = beta)-value
      if (ga*gc < 0){
        b=c
        gb=gc
      }
      else{
        a=c
        ga=gc
      }
    }
    (a+b)/2
  }
  left = leftx(value,lambda,beta,prec)
  right = rightx(value,lambda,beta,prec)
  cat('when the value of the function is',value,'the solution is',left,right)
}

