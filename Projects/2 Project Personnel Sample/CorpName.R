A1 <- c("v","a","l","l","i","a","n","t", 
        "d", "e","v","e","r",
        "k","r","e","u","t","e","r")

find.name <- function(D1){
    D1 <- NULL
    sam <- sample(1:length(D1),length(D1))
    for (i in 1:length(A1)){
        D1 <- paste(D1, D1[sam[i]], sep="") 
    }
D1
}

find.name(A1)
