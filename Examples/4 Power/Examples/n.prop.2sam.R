#*********************************************************************************
# FILE:    C:\Projects\Practical Tools\Book\Book Chapters\4 Power\n.prop.2sam.R   
# TOPIC:   Compute sample size for a given power when comparing proportions       
#          from overlapping samples.                                              
# DATE:    6/16/09                                                                
# AUTHOR:  R. Valliant                                                            
# PURPOSE:                                                                        
# REVISED:                                                                        
#*********************************************************************************

n.prop.2sam <- function(px, py, pxy, g, r, alt, sig.level=0.05, pow=0.80){
                    # check for allowable alt values
    alt.ok <- alt %in% c("one.sided", "two.sided")
    if (!alt.ok)
        stop("alt must be either 'one.sided' or 'two.sided'.\n ")
    else {
        if (alt == "one.sided")
            za <- qnorm(1-sig.level)
        if (alt=="two.sided")
            za <- qnorm(1-sig.level/2)
    }
    
    zb <- qnorm(1-pow)
    del <- px - py
    s2x <- px*(1-px)
    s2y <- py*(1-py)
    sxy <- pxy - px*py
                    # check for allowable pxy
    if ( (pxy <= px*py + sqrt(s2x*s2y)) &
         (pxy >= px*py - sqrt(s2x*s2y)) &
         (pxy <= min(px,py))
       )
         { n1 <- (s2x + r*s2y - 2*g*r*sxy) / del^2 * (za-zb)^2 }
    else stop("pxy not in allowable range.")
    
    METHOD <- "Two-sample comparison of proportions\n Sample size calculation for overlapping samples"

    structure(list(n1 = ceiling(n1),
         n2 = ceiling(n1/r),
         px.py.pxy = c(px, py, pxy),
         gamma = g,
         r = r,
         alt = alt,
         sig.level = sig.level,
         power = pow,
         method = METHOD
    ), class="power.htest")    
}

n.prop.2sam(px=0.5, py=0.55, pxy=0.3, g=0.7, r=1, alt="one.sided")

n.prop.2sam(px=0.5, py=0.55, pxy=0.3, g=0.7, r=0.5)
n.prop.2sam(px=0.5, py=0.55, pxy=0.4, g=0.7, r=0.5, alt="one.sided")
n.prop.2sam(px=0.5, py=0.55, pxy=0.4, g=0.7, r=0.5, alt="two.sided")
n.prop.2sam(px=0.5, py=0.60, pxy=0.4, g=0.7, r=0.5, alt="two.sided")

                # test error traps
n.prop.2sam(px=0.5, py=0.55, pxy=0.3, g=0.7, r=1, alt="3.sided")
n.prop.2sam(px=0.5, py=0.55, pxy=0.9, g=0.7, r=0.75, alt="two.sided")
n.prop.2sam(px=0.5, py=0.55, pxy=0.9, g=0.7, r=1, alt="two.sided")
n.prop.2sam(px=0.3, py=0.6, pxy=0.31, g=0.5, r=1, alt="two.sided")
n.prop.2sam(px=0.3, py=0.6, pxy=0.9, g=0.5, r=1, alt="two.sided")

                # combination ok
n.prop.2sam(px=0.5, py=0.55, pxy=0.45, g=0.5, r=1, alt="two.sided")




n.prop.2sam(px=0.5, py=0.55, pxy=0.5, g=1, r=1, alt="one.sided")
n.prop.2sam(px=0.60, py=0.45, pxy=0.45, g=1, r=1, alt="one.sided")
n.prop.2sam(px=0.15, py=0.20, pxy=0.15, g=1, r=1, alt="one.sided")
n.prop.2sam(px=0.20, py=0.10, pxy=0.10, g=1, r=1, alt="one.sided")
n.prop.2sam(px=0.20, py=0.05, pxy=0.05, g=1, r=1, alt="one.sided")


  
