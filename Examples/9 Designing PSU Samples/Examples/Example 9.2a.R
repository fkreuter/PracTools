#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples\Examples\Example 9.2a.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     02/24/2012
# AUTHOR:   R. Valliant
# PURPOSE:  Compute variance components for an srs/srs/ design
#           Group small BGs together and recompute resulting delta's.
#*********************************************************************************************************

attach("C:\\Projects\\Practical Tools Book\\Data\\MDarea.pop.RData", pos=2)

trtBG <- 10*MDarea.pop$TRACT + MDarea.pop$BLKGROUP

    # recode all BGs in a set of tracts to have the same trtBG value
    # this makes very little difference in delta's'
trtBGa <- trtBG
trtBGa[MDarea.pop$TRACT == 702800] <- 7028001
trtBGa[MDarea.pop$TRACT == 740105] <- 7401051
trtBGa[MDarea.pop$TRACT == 740400] <- 7404001
trtBGa[MDarea.pop$TRACT == 741000] <- 7410001
trtBGa[MDarea.pop$TRACT == 750203] <- 7502031
trtBGa[MDarea.pop$TRACT == 750600] <- 7506001
trtBGa[MDarea.pop$TRACT == 750700] <- 7507001
trtBGa[MDarea.pop$TRACT == 740603] <- 7406031
trtBGa[MDarea.pop$TRACT == 706402] <- 7064021
trtBGa[MDarea.pop$TRACT == 750201] <- 7502011


drop <- c(702202, 702800,740105, 740400, 741000, 750203, 750600, 750700)
pick <- !(MDarea.pop$TRACT %in% drop)
popsub <- MDarea.pop[pick, ]
dim(popsub)

BW2stageSRS(popsub$y1, psuID=popsub$TRACT)
BW2stageSRS(popsub$y1, psuID=trtBG[pick])
BW2stageSRS(popsub$y3, psuID=popsub$TRACT)
BW2stageSRS(popsub$y3, psuID=trtBG[pick])

    # create grouped trtBGs with about equal Ni in each
    # Note that method used below will split BGs sometimes.
    # Making groups with same Ni makes a huge difference in delta's '
pop <- cbind(MDarea.pop, trtBG)
dim(pop)
pop[1:5,]
pop <- pop[order(pop$trtBG), ]
Ni <- table(pop$trtBG)

H <- 400        # no. of SSUs
one <- rep(1, nrow(pop))
cumBG <- cumsum(as.numeric(one))
last <- nrow(pop)
size <- cumBG[last]/H
size
brks <- (0:H)*size
grps <- cut(cumBG, breaks = brks, labels = 1:H)
table(grps)

BW2stageSRS(pop$y1, psuID=grps)
BW2stageSRS(pop$y1, psuID=trtBG)

BW2stageSRS(pop$y2, psuID=grps)
BW2stageSRS(pop$y2, psuID=trtBG)

BW2stageSRS(pop$y3, psuID=grps)
BW2stageSRS(pop$y3, psuID=trtBG)

BW2stageSRS(pop$ins.cov, psuID=grps)
BW2stageSRS(pop$ins.cov, psuID=trtBG)

BW2stageSRS(pop$hosp.stay, psuID=grps)
BW2stageSRS(pop$hosp.stay, psuID=trtBG)

> BW2stageSRS(pop$y1, psuID=grps)
         B2          W2 unit relvar       B2+W2  delta full 
 0.03653178  1.42770886  1.46274119  1.46424065  0.02394614 
> BW2stageSRS(pop$y1, psuID=trtBG)
         B2          W2 unit relvar       B2+W2  delta full 
  0.3488622   1.9498600   1.4627412   2.2987221   0.1507963 
> BW2stageSRS(pop$y2, psuID=grps)
         B2          W2 unit relvar       B2+W2  delta full 
 0.01690448  1.00042397  1.01629837  1.01732845  0.01561720 
> BW2stageSRS(pop$y2, psuID=trtBG)
         B2          W2 unit relvar       B2+W2  delta full 
  0.3484909   1.3338149   1.0162984   1.6823058   0.2061368 
> 
> BW2stageSRS(pop$y3, psuID=grps)
         B2          W2 unit relvar       B2+W2  delta full 
 0.01836718  0.09537982  0.11360699  0.11374700  0.16043801 
> BW2stageSRS(pop$y3, psuID=trtBG)
         B2          W2 unit relvar       B2+W2  delta full 
  0.3492103   0.1220131   0.1136070   0.4712234   0.7403939 
> 
> BW2stageSRS(pop$ins.cov, psuID=grps)
         B2          W2 unit relvar       B2+W2  delta full 
0.003241692 0.258074380 0.261052883 0.261316072 0.011407924 
> BW2stageSRS(pop$ins.cov, psuID=trtBG)
         B2          W2 unit relvar       B2+W2  delta full 
  0.3408334   0.3426111   0.2610529   0.6834445   0.4976921 
> 
> BW2stageSRS(pop$hosp.stay, psuID=grps)
          B2           W2  unit relvar        B2+W2   delta full 
 0.055832436 12.854914197 12.897895629 12.910746633  0.003331177 
> BW2stageSRS(pop$hosp.stay, psuID=trtBG)
         B2          W2 unit relvar       B2+W2  delta full 
  0.4245935  17.2695144  12.8978956  17.6941079   0.0231955 