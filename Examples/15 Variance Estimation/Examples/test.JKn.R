data(scd)
scddes <- svydesign(data=scd, prob=~1, id=~ambulance, strata=~ESA,
                    nest=TRUE, fpc=rep(5,6))
scd2jkn <- as.svrepdesign(scddes, type="JKn")

names(scd2jkn)

repwts <- scd2jkn$repweights
repwts
repwts <- unlist(repwts$weights)
repwts <- matrix(repwts, ncol=6, byrow=TRUE)
repwts

JKn <- svrepdesign(data=scd, weights = rep(1,6), repweights = ~repwts,
            combined.weights=FALSE,
            type="JKn",
            rscales=rep(1/2,6))

svymean(~arrests, design = JKn)
