##-------------------- 設定環境 --------------------
file_path <- "C:/Users/User/Documents/DBCD"

##-------------------- package --------------------
library(expm)
library(dplyr)

# Calculate Sample Size
CalculateSampleSize <- function(Pow, beta1, beta2, theta, r0, lam0, alpha_star, tau, Method){
  #################### Fisher Information Matrix ###################### 
  Fisher <- function(b1, b2, r0, lam0, theta, tau) {
    
    EY <- Edta <- U <- fv <- Ek <- DD <- l_bb <- l_aa <- l_rr <- l_ll <- l_br <- l_ba <- l_bta <- l_bl <- 
      l_ata <- l_ar <- l_al <- l_tar <- l_tal <- l_rl <- l_tata <- f0 <- f1 <- f2 <- NA
    U <- function(v,z) { v*exp(b2*z)*lam0 }
    EY <- function(v,z) { 1/U(v,z)*(1-exp(-tau*U(v,z))) }
    fv <- function(v) { v^(1/theta-1)*exp(-v/theta)/theta^(1/theta)/gamma(1/theta) }
    Edta <- function(v,z) { 1-exp(-tau*U(v,z)) }
    Ek <- function(v,z) { r0*v*exp(b1*z)*EY(v,z) }
    A <- function(v,z){Ek(v,z)+Edta(v,z)+1/theta}
    B <- function(v,z) { EY(v,z)*r0*exp(b1*z)+EY(v,z)*lam0*exp(b2*z)+1/theta }
    
    
    l_bb <- function(v,z){ A(v,z)*(z^2*EY(v,z)*r0*exp(b1*z)*B(v,z)-(z*EY(v,z)*r0*exp(b1*z))^2)/(B(v,z))^2 }
    l_aa <- function(v,z){ A(v,z)*(z^2*EY(v,z)*lam0*exp(b2*z)*B(v,z)-(z*EY(v,z)*lam0*exp(b2*z))^2)/(B(v,z))^2 }
    l_rr <- function(v,z){ Ek(v,z)/r0^2-A(v,z)*(EY(v,z)*exp(b1*z)*EY(v,z)*exp(b1*z))/(B(v,z))^2 }
    l_ll <- function(v,z){ Edta(v,z)/lam0^2-A(v,z)*(EY(v,z)*exp(b2*z)*EY(v,z)*exp(b2*z))/(B(v,z))^2 }
    
    l_br <- function(v,z){ A(v,z)*(z*EY(v,z)*exp(b1*z)*B(v,z)-z*EY(v,z)*r0*exp(b1*z*2)*EY(v,z))/(B(v,z))^2 }
    l_ba <- function(v,z){ A(v,z)*(-z*EY(v,z)*r0*exp(b1*z)*EY(v,z)*lam0*z*exp(b2*z))/(B(v,z))^2 }
    l_bta <- function(v,z){ (z*EY(v,z)*r0*exp(b1*z))*((-1/theta^2)*B(v,z)-(-1/theta^2)*(A(v,z)))/(B(v,z))^2  }
    l_bl <- function(v,z){ A(v,z)*(-z*EY(v,z)*r0*exp(b1*z)*EY(v,z)*exp(b2*z))/(B(v,z))^2 }
    
    l_ata <- function(v,z){ (z*EY(v,z)*lam0*exp(b2*z))*((-1/theta^2)*B(v,z)-(-1/theta^2)*(A(v,z)))/(B(v,z))^2  }
    l_ar <- function(v,z){ A(v,z)*(-EY(v,z)*exp(b1*z)*z*EY(v,z)*lam0*exp(b2*z))/(B(v,z))^2 }
    l_al <- function(v,z){ A(v,z)*(z*EY(v,z)*exp(b2*z)*B(v,z)-EY(v,z)*exp(b2*z*2)*z*lam0*EY(v,z))/(B(v,z))^2 }
    
    l_tar <- function(v,z){ -1/theta^2*EY(v,z)*exp(b1*z)*(1/B(v,z)-A(v,z)/(B(v,z))^2) }
    l_tal <- function(v,z){ -1/theta^2*EY(v,z)*exp(b2*z)*(1/B(v,z)-A(v,z)/(B(v,z))^2) }
    
    l_rl <- function(v,z){ A(v,z)*(-EY(v,z)*exp(b2*z)*EY(v,z)*exp(b1*z))/(B(v,z))^2 }
    
    f1 <- function(v,z){  r0*exp(b1*z)*EY(v,z)+lam0*exp(b2*z)*EY(v,z)+1/theta }
    f2 <- function(v,z){  digamma(1/theta)+log(theta)-1-digamma(A(v,z))+ log(f1(v,z))+ A(v,z)/f1(v,z) }
    l_tata <- function(v,z){ 2/theta^3*f2(v,z) - 1/theta^2*
        (-1/theta^2*trigamma(1/theta)+1/theta+1/theta^2*trigamma(A(v,z))+
           -2/theta^2/f1(v,z)+ 1/theta^2*A(v,z)/(f1(v,z))^2) }
    
    #------------ expectation ----------------------
    
    fbb <- faa <- frr <- fll <- fbr <- fba <- fbta <- fbl <- fata <- far <- fal <- ftar <- ftal <- frl <- ftata <- NA
    dbb <- daa <- drr <- dll <- dbr <- dba <- dbta <- dbl <- data <- dar <- dal <- dtar <- dtal <- drl <- dtata <- NA
    fbb <- function(v){ (l_bb(v,0)*p+l_bb(v,1)*(1-p))*fv(v) }
    faa <- function(v){ (l_aa(v,0)*p+l_aa(v,1)*(1-p))*fv(v) }
    frr <- function(v){ (l_rr(v,0)*p+l_rr(v,1)*(1-p))*fv(v) }
    fll <- function(v){ (l_ll(v,0)*p+l_ll(v,1)*(1-p))*fv(v) }
    fbr <- function(v){ (l_br(v,0)*p+l_br(v,1)*(1-p))*fv(v) }
    fba <- function(v){ (l_ba(v,0)*p+l_ba(v,1)*(1-p))*fv(v) }
    fbta <- function(v){ (l_bta(v,0)*p+l_bta(v,1)*(1-p))*fv(v) }
    fbl <- function(v){ (l_bl(v,0)*p+l_bl(v,1)*(1-p))*fv(v) }
    
    fata <- function(v){ (l_ata(v,0)*p+l_ata(v,1)*(1-p))*fv(v) }
    far <- function(v){ (l_ar(v,0)*p+l_ar(v,1)*(1-p))*fv(v) }
    fal <- function(v){ (l_al(v,0)*p+l_al(v,1)*(1-p))*fv(v) }
    ftar <- function(v){ (l_tar(v,0)*p+l_tar(v,1)*(1-p))*fv(v) }
    ftal <- function(v){ (l_tal(v,0)*p+l_tal(v,1)*(1-p))*fv(v) }
    
    frl <- function(v){ (l_rl(v,0)*p+l_rl(v,1)*(1-p))*fv(v) }
    ftata <- function(v){ (l_tata(v,0)*p+l_tata(v,1)*(1-p))*fv(v) }
    
    dbb <- integrate(fbb, lower = 0, upper =Inf, rel.tol = 1e-10)$value
    daa <- integrate(faa, lower = 0, upper =Inf, rel.tol = 1e-10)$value
    drr <- integrate(frr, lower = 0, upper =Inf, rel.tol = 1e-10)$value
    dll <- integrate(fll, lower = 0, upper =Inf, rel.tol = 1e-10)$value
    dbr <- integrate(fbr, lower = 0, upper =Inf, rel.tol = 1e-10)$value
    dba <- integrate(fba, lower = 0, upper =Inf, rel.tol = 1e-10)$value
    dbta <-integrate(fbta, lower = 0, upper =Inf, rel.tol = 1e-10)$value
    dbl <- integrate(fbl, lower = 0, upper =Inf, rel.tol = 1e-10)$value
    data <-integrate(fata, lower = 0, upper =Inf, rel.tol = 1e-10)$value
    dar <- integrate(far, lower = 0, upper =Inf, rel.tol = 1e-10)$value
    dal <- integrate(fal, lower = 0, upper =Inf, rel.tol = 1e-10)$value
    dtar <-integrate(ftar, lower = 0, upper =Inf, rel.tol = 1e-10)$value
    dtal <-integrate(ftal, lower = 0, upper =Inf, rel.tol = 1e-10)$value
    drl <- integrate(frl, lower = 0, upper =Inf, rel.tol = 1e-10)$value
    dtata <- integrate(ftata, lower = 0, upper =Inf, rel.tol = 1e-10)$value
    # ----------- second derivtive -----------
    S <- S1 <- S2 <- S3 <- NA
    S <- matrix(c( dbb,  dba,  dbta,  dbr,  dbl,
                   dba,  daa,  data,  dar,  dal,
                   dbta, data, dtata, dtar, dtal,
                   dbr,  dar,  dtar,  drr,  drl,
                   dbl,  dal,  dtal,  drl,  dll), 5, 5, byrow=T) 
    S
  }
  
  #------------------- Calculate Sample Size -------------------
  #------------------- parameter -------------------
  
  # Pow <- 0.8
  # 
  # beta1 <- log(1/0.55)  # recurrent
  # beta2 <- log(1.25)    # death
  # theta <- 1  # for gamma frailty
  
  # r0 <- 0.2
  # lam0 <- 0.2  
  
  p <- 0.5
  n.point <- 100 # number of time point
  # tau <- 1
  
  
  #------------ Main function ----------------------
  S0 <- Sa <- sigma_0 <- sigma_a <- Kr <- t1 <- t2 <- a1 <- a2 <- n_adj <- n_org <- NA
  
  S0 <- Fisher(b1 = 0, b2 = 0, r0 = r0, lam0 = lam0, theta = theta, tau = tau)
  Sa <- Fisher(b1 = beta1, b2 = beta2, r0 = r0, lam0 = lam0, theta = theta, tau = tau)
  sigma_0 <- solve(S0) # variance sigma_H0
  sigma_a <- solve(Sa) # variance sigma_Ha
  
  a1 <- a2 <- a12 <- delta <- NA
  
  a1 <- (sqrtm(sigma_a) %*% sqrtm(sigma_a))[1,1] %>% Re()
  a2 <- (sqrtm(sigma_a) %*% sqrtm(sigma_a))[2,2] %>% Re()
  a12 <- (sqrtm(sigma_a) %*% sqrtm(sigma_a))[1,2] %>% Re()
  
  delta <- 1/(a1*a2-a12^2)*((beta1)^2*a2+(beta2)^2*a1-2*(beta1)*(beta2)*a12)
  
  if(Method == "I"){
    # Method I
    #------------------ alpha_star=0.05
    # alpha_star
    # naive (df=1 formula) 
    # (qnorm(1-Pow, lower.tail = F)*sqrt((sigma_a)[1])+ qnorm(0.025, lower.tail = F)* sqrt((sigma_a)[1]))^2/b_Ha^2
    
    # alpha_star <- 0.05
    my.ncp <- function(ncp) {qchisq(Pow, df = 2, ncp = ncp , lower.tail = F) - 
        qchisq(alpha_star, df = 2, lower.tail = F)}
    non_cp <- uniroot(my.ncp, c(0.001,100))$root
    n_adj <- non_cp/delta
    # cat("Sample Size is", round(n_adj,1), "by method I")
    return(round(n_adj,1))
  }
  
  if(Method == "II"){
    # Method II
    #------------------ new alpha
    # new (df=1 formula)
    # (qnorm(1-Pow, lower.tail = F)*sqrt((sigma_a)[1])+ qnorm(0.025, lower.tail = F)* sqrt((sigma_0)[1]))^2/b_Ha^2
    
    alpha_star <-  pchisq(sigma_0[1,1]/sigma_a[1,1]*qchisq(.05, df = 2, lower.tail = F), 
                          df = 2, ncp = 0, lower.tail = F)
    my.ncp <- function(ncp) {qchisq(Pow, df = 2, ncp = ncp , lower.tail = F) - 
        qchisq(alpha_star, df = 2, lower.tail = F)}
    non_cp <- uniroot(my.ncp, c(0.001,100))$root
    n_adj <- non_cp/delta
    # cat("Sample Size is", round(n_adj,1), "by method II")
    round(n_adj,1)
  }
}

E_VarF <- function(beta1, beta2, the, lamD, lamR, p) {
  tau <- 100
  EY <- Edta <- U <- fv <- Ek <- DD <- l_bb <- l_aa <- l_rr <- l_ll <- l_br <- l_ba <- l_bta <- l_bl <- 
    l_ata <- l_ar <- l_al <- l_tar <- l_tal <- l_rl <- l_tata <- f0 <- f1 <- f2 <- NA
  
  U <- function(v,z) { v*exp(beta2*z)*lamD }
  EY <- function(v,z) { 1/U(v,z)*(1-exp(-tau*U(v,z))) }
  fv <- function(v) { v^(1/the-1)*exp(-v/the)/the^(1/the)/gamma(1/the) }
  Edta <- function(v,z) { 1-exp(-tau*U(v,z)) }
  Ek <- function(v,z) { lamR*v*exp(beta1*z)*EY(v,z) }
  f0 <- function(v,z){Ek(v,z)+Edta(v,z)+1/the}
  DD <- function(v,z) { EY(v,z)*lamR*exp(beta1*z)+EY(v,z)*lamD*exp(beta2*z)+1/the }
  l_bb <- function(v,z){ f0(v,z)*(z^2*EY(v,z)*lamR*exp(beta1*z)*DD(v,z)-(z*EY(v,z)*lamR*exp(beta1*z))^2)/(DD(v,z))^2 }
  l_aa <- function(v,z){ f0(v,z)*(z^2*EY(v,z)*lamD*exp(beta2*z)*DD(v,z)-(z*EY(v,z)*lamD*exp(beta2*z))^2)/(DD(v,z))^2 }
  l_rr <- function(v,z){ Ek(v,z)/lamR^2-(Ek(v,z)+Edta(v,z)+1/the)*(EY(v,z)*exp(beta1*z)*EY(v,z)*exp(beta1*z))/(DD(v,z))^2 }
  l_ll <- function(v,z){ Edta(v,z)/lamD^2-f0(v,z)*(EY(v,z)*exp(beta2*z)*EY(v,z)*exp(beta2*z))/(DD(v,z))^2 }
  l_br <- function(v,z){ f0(v,z)*(z*EY(v,z)*exp(beta1*z)*DD(v,z)-z*EY(v,z)*lamR*exp(beta1*z*2)*EY(v,z))/(DD(v,z))^2 }
  l_ba <- function(v,z){ f0(v,z)*(-z*EY(v,z)*lamR*exp(beta1*z)*EY(v,z)*lamD*z*exp(beta2*z))/(DD(v,z))^2 }
  l_bta <- function(v,z){ (z*EY(v,z)*lamR*exp(beta1*z))*((-1/the^2)*DD(v,z)-(-1/the^2)*(f0(v,z)))/(DD(v,z))^2  }
  l_bl <- function(v,z){ f0(v,z)*(-z*EY(v,z)*lamR*exp(beta1*z)*EY(v,z)*exp(beta2*z))/(DD(v,z))^2 }
  l_ata <- function(v,z){ (z*EY(v,z)*lamD*exp(beta2*z))*((-1/the^2)*DD(v,z)-(-1/the^2)*(f0(v,z)))/(DD(v,z))^2  }
  l_ar <- function(v,z){ f0(v,z)*(-EY(v,z)*exp(beta1*z)*z*EY(v,z)*lamD*exp(beta2*z))/(DD(v,z))^2 }
  l_al <- function(v,z){ f0(v,z)*(z*EY(v,z)*exp(beta2*z)*DD(v,z)-EY(v,z)*exp(beta2*z*2)*z*lamD*EY(v,z))/(DD(v,z))^2 }
  l_tar <- function(v,z){ -1/the^2*EY(v,z)*exp(beta1*z)*(1/DD(v,z)-(Ek(v,z)+Edta(v,z)+1/the)/(DD(v,z))^2) }
  l_tal <- function(v,z){ -1/the^2*EY(v,z)*exp(beta2*z)*(1/DD(v,z)-(Ek(v,z)+Edta(v,z)+1/the)/(DD(v,z))^2) }
  l_rl <- function(v,z){ f0(v,z)*(-EY(v,z)*exp(beta2*z)*EY(v,z)*exp(beta1*z))/(DD(v,z))^2 }
  f1 <- function(v,z){  lamR*exp(beta1*z)*EY(v,z)+lamD*exp(beta2*z)*EY(v,z)+1/the }
  f2 <- function(v,z){  digamma(1/the)+log(the)-1-digamma(f0(v,z))+ log(f1(v,z))+ f0(v,z)/f1(v,z) }
  l_tata <- function(v,z){ 2/the^3*f2(v,z) - 1/the^2*
      (-1/the^2*trigamma(1/the)+1/the+1/the^2*trigamma(f0(v,z))+
         -2/the^2/f1(v,z)+ 1/the^2*f0(v,z)/(f1(v,z))^2) }
  
  #------------ expectation ----------------------
  
  fbb <- faa <- frr <- fll <- fbr <- fba <- fbta <- fbl <- fata <- far <- fal <- ftar <- ftal <- frl <- ftata <- NA
  dbb <- daa <- drr <- dll <- dbr <- dba <- dbta <- dbl <- data <- dar <- dal <- dtar <- dtal <- drl <- dtata <- NA
  
  
  fbb <- (function(v){ (l_bb(v,0)*p+l_bb(v,1)*(1-p))*fv(v) })
  faa <- (function(v){ (l_aa(v,0)*p+l_aa(v,1)*(1-p))*fv(v) })
  frr <- (function(v){ (l_rr(v,0)*p+l_rr(v,1)*(1-p))*fv(v) })
  fll <- (function(v){ (l_ll(v,0)*p+l_ll(v,1)*(1-p))*fv(v) })
  fbr <- (function(v){ (l_br(v,0)*p+l_br(v,1)*(1-p))*fv(v) })
  fba <- (function(v){ (l_ba(v,0)*p+l_ba(v,1)*(1-p))*fv(v) })
  fbta <-(function(v){ (l_bta(v,0)*p+l_bta(v,1)*(1-p))*fv(v) })
  fbl <- (function(v){ (l_bl(v,0)*p+l_bl(v,1)*(1-p))*fv(v) })
  fata <-(function(v){ (l_ata(v,0)*p+l_ata(v,1)*(1-p))*fv(v) })
  far <- (function(v){ (l_ar(v,0)*p+l_ar(v,1)*(1-p))*fv(v) })
  fal <- (function(v){ (l_al(v,0)*p+l_al(v,1)*(1-p))*fv(v) })
  ftar <-(function(v){ (l_tar(v,0)*p+l_tar(v,1)*(1-p))*fv(v) })
  ftal <-(function(v){ (l_tal(v,0)*p+l_tal(v,1)*(1-p))*fv(v) })
  frl <- (function(v){ (l_rl(v,0)*p+l_rl(v,1)*(1-p))*fv(v) })
  ftata <- function(v){ (l_tata(v,0)*p+l_tata(v,1)*(1-p))*fv(v) }
  
  dbb <- integrate(fbb, lower = 0, upper =Inf, rel.tol = 1e-10)$value
  daa <- integrate(faa, lower = 0, upper =Inf, rel.tol = 1e-10)$value
  drr <- integrate(frr, lower = 0, upper =Inf, rel.tol = 1e-10)$value
  dll <- integrate(fll, lower = 0, upper =Inf, rel.tol = 1e-10)$value
  dbr <- integrate(fbr, lower = 0, upper =Inf, rel.tol = 1e-10)$value
  dba <- integrate(fba, lower = 0, upper =Inf, rel.tol = 1e-10)$value
  dbta <-integrate(fbta, lower = 0, upper =Inf, rel.tol = 1e-10)$value
  dbl <- integrate(fbl, lower = 0, upper =Inf, rel.tol = 1e-10)$value
  data <-integrate(fata, lower = 0, upper =Inf, rel.tol = 1e-10)$value
  dar <- integrate(far, lower = 0, upper =Inf, rel.tol = 1e-10)$value
  dal <- integrate(fal, lower = 0, upper =Inf, rel.tol = 1e-10)$value
  dtar <-integrate(ftar, lower = 0, upper =Inf, rel.tol = 1e-10)$value
  dtal <-integrate(ftal, lower = 0, upper =Inf, rel.tol = 1e-10)$value
  drl <- integrate(frl, lower = 0, upper =Inf, rel.tol = 1e-10)$value
  dtata <- integrate(ftata, lower = 0, upper =Inf, rel.tol = 1e-10)$value
  
  # ----------- second derivative -----------
  
  S <- S1 <- S2 <- S3 <- NA
  S <- matrix(c( dbb,  dba,  dbta,  dbr,  dbl,
                 dba,  daa,  data,  dar,  dal,
                 dbta, data, dtata, dtar, dtal,
                 dbr,  dar,  dtar,  drr,  drl,
                 dbl,  dal,  dtal,  drl,  dll), 5, 5, byrow=T) 
  sigma_a <- solve(S)
  a1 <- (sqrtm(sigma_a) %*% sqrtm(sigma_a))[1,1] %>% Re()
  a2 <- (sqrtm(sigma_a) %*% sqrtm(sigma_a))[2,2] %>% Re()
  a12 <- (sqrtm(sigma_a) %*% sqrtm(sigma_a))[1,2] %>% Re()
  1/(a1*a2-a12^2)*((beta1)^2*a2+(beta2)^2*a1-2*(beta1)*(beta2)*a12)  # delta
}
