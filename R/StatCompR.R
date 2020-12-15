#' @title CIdichotomy 
#' @description Find confidence intervals by dichotomy
#' @param lam the confidence coefficient
#' @param alpha the shape parameter of the gamma function
#' @return a label for test set
#' @importFrom  stats qgamma
#' @importFrom  stats pgamma
#' @export

cfint <- function(lam,alpha){
  a = 0
  #设定置信区间左端点的初始值
  if (qgamma(1-lam,alpha) > alpha-1){
    b = alpha-1
  }
  else{
    b = qgamma(1-lam,alpha)
  }
  # 设定置信区间右端点的初始值（取概率密度最大点和能构成置信区间的最小值点的较小的那个）
  for (i in 1:40) {
    c = (a+b)/2
    # 取二分法的新端点值为两者中点
    ap = pgamma(a,alpha)
    aint = pgamma(a,alpha)+lam
    bp = pgamma(b,alpha)
    bint = pgamma(b,alpha)+lam
    a_interval = qgamma(aint,alpha)-qgamma(ap,alpha)
    b_interval = qgamma(bint,alpha)-qgamma(bp,alpha)
    if (b_interval>a_interval)
      b = c
    else
      a = c
    # 若由a生成的置信区间更小，则表明最短置信区间的左端点应该更加靠近a，否则则更加靠近b
    # 将c赋值给远离置信区间左端点真实值的那个
  }
  cat('confidence',lam,'confidence interval',qgamma(ap,alpha),',',qgamma(aint,alpha),']')
}

#' @title SFldichotomy 
#' @description Find the solution of the function by dichotomy 
#' @param lam function value
#' @param alpha the shape parameter of t he gamma function
#' @param prec balabal
#' @importFrom  stats dgamma
#' @export
leftx <- function(lam,alpha,prec){
  a=0
  b=alpha-1
  ga=dgamma(a,alpha)-lam
  gb=dgamma(b,alpha)-lam
  while(b-a>prec){
    c=(a+b)/2
    gc=dgamma(c,alpha)-lam
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

#' @title SFrdichotomy 
#' @description Find the solution of the function by dichotomy 
#' @param lam function value
#' @param alpha the shape parameter of t he gamma function
#' @param prec balala
#' @importFrom  stats dgamma
#' @export
rightx <- function(lam,alpha,prec){
  a=20
  b=alpha-1
  ga=dgamma(a,alpha)-lam
  gb=dgamma(b,alpha)-lam
  while(b-a>prec){
    c=(a+b)/2
    gc=dgamma(c,alpha)-lam
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
