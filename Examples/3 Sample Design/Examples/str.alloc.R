#****************************************************************************
# FILE:    str.alloc.R                                                       
# PROJECT: Practical Tools book                                              
# DATE:    08/12/09                                                          
# AUTHOR:  R. Valliant                                                       
# PURPOSE: Compute sample sizes for stratified sampling                      
# REVISED:                                                                   
#****************************************************************************

# Parameters
#   n.tot   = fixed total sample size
#   Nh      = pop stratum sizes or pop stratum proportions
#   Sh      = stratum unit standard deviations
#   cost    = total cost
#   ch      = vector of cost per unit in stratum h
#   V0      = fixed variance target for estimated mean
#   CV0     = fixed CV target for estimated mean
#   ybarU   = pop mean of y
#   alloc   = type of allocation, must be one of "prop", "neyman", "totcost", "totvar"

str.alloc <- function(n.tot=NULL, Nh=NULL, Sh = NULL, cost=NULL, ch=NULL, 
        V0=NULL, CV0=NULL, ybarU=NULL, alloc){
    if ( !(alloc %in% c("prop", "neyman", "totcost", "totvar")) )
        stop("Illegal allocation specified.\n")
    if (is.null(Nh)) 
        stop("Nh cannot be NULL.\n")
    if ( !(alloc=="prop") ){
        if (is.null(Sh) )
            stop("Sh cannot be NULL unless allocation is proportional.\n")
        if ( !(alloc == "neyman") & !(alloc == "totcost")){
            if (!is.null(CV0) & !is.null(V0)) 
                stop("Only one of CV0 and V0 should be non-null unless allocation is proportional, Neyman, cost constrained.\n")
        }
    }
    if (any(Nh <= 0, Sh <= 0, CV0 <= 0, V0 <=0, cost <= 0)) 
        stop("Nh, Sh, CV0, V0, and cost cannot be <= 0.\n")
         
    N <- sum(Nh)
    Wh <- Nh / N
    if (alloc == "prop") nh <- n.tot * Wh
    
    if ( (alloc == "neyman") & !is.null(n.tot) ){
        nh <- n.tot * Wh * Sh / sum(Wh * Sh)  
    }
    
    if ( (alloc == "totcost") ){
        if (is.null(cost)) 
            stop("If alloc=totcost, cost must be specified.\n")
        d1 <- sum(Wh * Sh / sqrt(ch))
        ph.cost <- Wh * Sh/ sqrt(ch) / d1
        n.cost <- cost * d1 / sum(Wh * Sh * sqrt(ch))
        nh <- n.cost * ph.cost
    }

    if ( (alloc == "totvar") ){
        if (is.null(CV0) & is.null(V0)) 
                stop("CV0 and V0 cannot both be NULL if allocation is totvar.\n")
        if (!is.null(CV0)){
            V0 <- (CV0 * ybarU)^2
        }
        d1 <- sum(Wh * Sh / sqrt(ch))
        d2 <- sum(Wh * Sh * sqrt(ch))
        d3 <- V0 + sum(Wh * Sh^2)/N
        ph.cost <- Wh * Sh/ sqrt(ch) / d1
        n.cost <- d1 * d2 / d3
        nh <- n.cost * ph.cost
    }
    
    list("allocation" = alloc,
          nh = nh, 
          "nh/n" = nh/sum(nh))
}


#______________________________________________________________________________________
Nh <- c(215, 65, 252, 50, 149, 144)
Sh <- c(26787207, 10645109, 6909676, 11085034, 9817762, 44553355)
ch <- c(1400, 200, 300, 600, 450, 1000)

                    # check allocations in Table 3.2
str.alloc(n.tot = 100, Nh = Nh, alloc = "prop")
str.alloc(n.tot = 100, Nh = Nh, Sh = Sh, alloc = "neyman")
str.alloc(Nh = Nh, Sh = Sh, cost = 100000, ch = ch, alloc = "totcost")
str.alloc(Nh = Nh, Sh = Sh, cost = 200000, ch = ch, alloc = "totcost")

str.alloc(Nh = Nh, Sh = Sh, CV0 = 0.05, ch = ch, ybarU = 11664181, alloc = "totvar")
str.alloc(Nh = Nh, Sh = Sh, CV0 = 0.10, ch = ch, ybarU = 11664181, alloc = "totvar")

                    # check illegal inputs
                    # alloc omitted
str.alloc(n.tot = 100, Nh = Nh)
                    # illegal alloc
str.alloc(n.tot = 100, Nh = Nh, alloc = "prap")
                    # illegal Nh
str.alloc(n.tot = 100, Nh = -Nh, alloc = "prop")
str.alloc(n.tot = 100, Nh = Inf, alloc = "prop")
str.alloc(n.tot = 100, alloc = "prop")

                    # illegal Sh
str.alloc(n.tot = 100, Nh = Nh, Sh = NULL, alloc = "neyman")
str.alloc(n.tot = 100, Nh = Nh, Sh = -Sh, alloc = "neyman")
                    # illegal cost
str.alloc(Nh = Nh, Sh = Sh, cost = 0, ch = ch, alloc = "totcost")
str.alloc(Nh = Nh, Sh = Sh, cost = -1000, ch = ch, alloc = "totcost")
str.alloc(Nh = Nh, Sh = Sh, ch = ch, alloc = "totcost")
                    # illegal totvar
str.alloc(Nh = Nh, Sh = Sh, ch = ch, ybarU = 11664181, alloc = "totvar")
str.alloc(Nh = Nh, Sh = Sh, CV0 = 0.05, V0 = 100, ch = ch, ybarU = 11664181, alloc = "totvar")
