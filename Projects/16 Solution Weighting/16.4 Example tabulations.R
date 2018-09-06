#****************************************************************************************
# FILE:     16.4 Example tabulations.R                                                   
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples                   
# DATE:     01/22/2011                                                                   
# AUTHOR:   R. Valliant                                                                  
# REVISED:  
#****************************************************************************************

file_loc <- "C:\\Projects\\Practical Tools Book\\Book Chapters\\18 Solution Weighting\\"
attach(paste(file_loc, "sam.lin.ub.RData", sep=""))

summary(sam.lin.ub)

            # estimated totals for re-enlistment item
svytotal(~ as.factor(ra008), design = sam.lin.ub, na.rm = TRUE)
            # CV's of estimated totals for re-enlistment item
cv(svytotal(~ as.factor(ra008), design = sam.lin.ub, na.rm = TRUE))

            # proportions for re-enlistment item
reenlist <- svymean(~ as.factor(ra008), design = sam.lin.ub, na.rm = TRUE)
cv(svymean(~ as.factor(ra008), design = sam.lin.ub, na.rm = TRUE))

            # format with row labels
out <- print(ftable(reenlist, 
        rownames = list(c("Very unlikely", 
                          "Unlikely",
                          "Neither likely nor unlikely",
                          "Likely",
                          "Very likely")
                    )
), digits = 3)
out

out <- data.frame(out)
out <- cbind(out[1:5,], out[6:10,])
out <- out[, c(1,3,6)]
dimnames(out)[[2]] <- c("Response", "Proportion", "SE")
write.csv(out, file = "c:\\table.csv")


            # crosstab of re-enlistment by service
reenlist <- svymean(~interaction(as.factor(xsrrcr), as.factor(ra008)), 
        design = sam.lin.ub,
        na.rm = TRUE)
print(ftable(reenlist, 
        rownames = list(xsrrcr = c("Very unlikely", 
                          "Unlikely",
                          "Neither",
                          "Likely",
                          "Very likely"),
                        ra008 = c("Army National Guard",
                          "Army Reserve",
                          "Navy Reserve",
                          "Marine Corp Reserve",
                          "Air National Guard",
                          "Air Force Reserve"))
), digits = 3)

#	Combine Very Unlikely and Unlikely to reenlist and tabulate
sam.lin.ub$variables$newra008 <- sam.lin.ub$variables$ra008 
	# combine categories 1 & 2
sam.lin.ub$variables$newra008[sam.lin.ub$variables$newra008 %in% c(1,2)] <- 1
 
#	This will give the proportions who are either Very Unlikely or Unlikely to reenlist ...
svymean(~ as.factor(newra008), design = sam.lin.ub, na.rm = TRUE)
 
#	This will do it by service ...
 svymean(~interaction(as.factor(xsrrcr), as.factor(newra008)), 
      design = sam.lin.ub,
      na.rm = TRUE)

