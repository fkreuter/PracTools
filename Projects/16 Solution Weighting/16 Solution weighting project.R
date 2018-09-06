#****************************************************************************************
# FILE:     Project2(12.12.2010).R            
# TOPIC:    SURVMETH 895 - Project 2                                                     
# DATE:     12/23/2010                                                                   
# AUTHOR:   R. Valliant                                                                  
# REVISED:  12/16/2010
#****************************************************************************************

# Define a working directory
file_loc <- "C:\\Projects\\Practical Tools Book\\Book Chapters\\13 Project Weighting\\Data files\\"
#setwd(paste(file_loc))

#libraries
require(foreign)
require(survey)
require(Hmisc)
require(doBy)
require(rpart)
require(sampling)
#require(quadprog) 


sofr <- sasxport.get(paste(file_loc,"sofr.xpt",sep=""))
dim(sofr)
#[1] 71701    19

table(sofr$respstat,useNA="ifany")

#    1     2     3     4     5    18    19    22    23    25    26    27    29    35 
#25539    20   524   503    97     9     2    35   193     8 39872  1339     6  3554 

# Data files will be created based on the Figure 14.1 in the text
# Step 1- Base weights - Use full sample
# Step 2- Distribute the weights of the unknown eligibles to the respondents, nonrespondents and known ineligibles - Use full sample
# Step 3- Nonresponse adjustment - Use respondents and nonrespondents data
# Step 4- Calibration - Use respondents and known ineligibles data


#---------------------------------------------------------------------------------------------------
        # Step 1: Compute base weights
sofr$d0 <- sofr$nstrat/sofr$nsamp #number of persons in the population (NSTRAT) and the sample (NSAMP) in the design stratum
sofr$f0 <- sofr$nsamp/sofr$nstrat #fpc

sum(sofr$d0)
#[1] 870373


#table(sofr$stratum,useNA="ifany")

#   1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23   24   25   26   27   28   29   30   31   32   33   34   35 
# 456   19  175   21   62  507   54  284   47  223  353   97  154  647   32   60  420  201   94    6   21   61   87    6    9    9   30   36   54   98   82  807   22   50   84 
#  36   37   38   39   40   41   42   43   44   45   46   47   48   49   50   51   52   53   54   55   56   57   58   59   60   61   62   63   64   65   66   67   68   69   70 
#1262  699   94   29   81   32   53  180   84   55   88  242   27   34  233   16   79  479   27   55  410  178   72   28   80   89   25   38   34   60  460  319   68   13   72 
#  71   72   73   74   75   76   77   78   79   80   81   82   83   84   85   86   87   88   89   90   91   92   93   94   95   96   97   98   99  100  101  102  103  104  105 
#  13    9   11   18   11   35   99  137  182   11   34   49   85  732   34   85  866  490  162   21   73  248   68   48  103  367   20   44  352  145   12   34   42   56  147 
# 106  107  108  109  110  111  112  113  114  115  116  117  118  119  120  121  122  123  124  125  126  127  128  129  130  131  132  133  134  135  136  137  138  139  140 
#  25   30  183  103  167  468   48  293  235   64  374  224  354   72   71  105   10   53   67  115   87  182  110  126  387   17  211   30  176  124   60   58  110  137   64 
# 141  142  143  144  145  146  147  148  149  150  151  152  153  154  155  156  157  158  159  160  161  162  163  164  165  166  167  168  169  170  171  172  173  174  175 
#  94   80 1016  250  294  489   87 1287  740  348   49  631  254  188  278  710  348   42   72   72  541  167   51   89   79  564  232   78  315  100   97   11   28   29   36 
# 176  177  178  179  180  181  182  183  184  185  186  187  188  189  190  191  192  193  194  195  196  197  198  199  200  201  202  203  204  205  206  207  208  209  210 
#  11  134   30   53  159   81  126  218   36   66   31   28   38  156  180   84   57   61   90   45   12   22   27  325   31   75   25   23   23   20   40   47   46  564   39 
# 211  212  213  214  215  216  217  218  219  220  221  222  223  224  225  226  227  228  229  230  231  232  233  234  235  236  237  238  239  240  241  242  243  244  245 
# 192   17   68  354   64  226  210   51  211  142  167   48  173  100   97  190  395   47  160   22   26   34  141   43  183  162  224   16   15   26   55  430   86  245   27 
# 246  247  248  249  250  251  252  253  254  255  256  257  258  259  260  261  262  263  264  265  266  267  268  269  270  271  272  273  274  275  276  277  278  279  280 
# 847  539  115  173   15  474   89  365   62  145   61   85   35   24   58   88   49  447  119 1439  125   35   44  933  133   46  271  360  826  143  334  112   57   20   15 
# 281  282  283  284  285  286  287  288  289  290  291  292  293  294  295  296  297  298  299  300  301  302  303  304  305  306  307  308  309  310  311  312  313  314  315 
# 244  106  130  148  185   79   61   15   80   48  153  161  109  331  860  176  570  178  123  293  139   95  119  324   40  164   52  109  144  561  519   93  148  342  146 
# 316  317  318  319  320  321  322  323  324  325  326  327  328  329  330  331  332  333  334  335  336  337  338  339  340  341  342  343  344  345  346  347  348  349  350 
#  20  105   27   61  143  569   25  421  107  212   91 1130  163  476  753  148   50   64   92  302  241  142  976  114  421   62  487  133   81  921   91  364  169  105  111 
# 351  352  353  354  355  356  357  358  359  360  361  362  363  364  365  366  367  368  369  370  371  372  373  374  375  376  377  378  379  380  381  382  383  384  385 
# 260  108  217   81  131  300  269   84   10   11   14   42  113  408   67  329   91   48   59  100   70   59  138  705   80   80  183  304  101   41  134  129  141  170  227 
# 386  387  388  389  390  391  392  393  394  395  396  397  398  399  400  401  402  403  404 
#  15  152   37   41   78   78   86  335  297  135   69   89   53   55  155   19   36   49 1171 


#------------------------------------------------------------------------------------------------
# Map disposition codes into weighting groups
# Define 4 major groups: (1)Respondents, 
#                        (2) Eligible noninterview, 
#                        (3) Ineligible, 
#                        (4) Unknown eligibility

        # respondents: respstat = 1,2
sofr$disp[sofr$respstat==1|sofr$respstat==2] <- 1 
        # eligible noninterview: respstat = 3, 23, 26
sofr$disp[sofr$respstat==3|sofr$respstat==23|sofr$respstat==26] <- 2
        # ineligible: respstat = 4, 18, 19, 22, 35
sofr$disp[sofr$respstat==4|sofr$respstat==18|sofr$respstat==19|
          sofr$respstat==22|sofr$respstat==35] <- 3
        # unknown eligibility: respstat = 5, 25, 27, 29
sofr$disp[sofr$respstat==5|sofr$respstat==25|sofr$respstat==27|sofr$respstat==29] <- 4 

        # Unweighted counts in weighting categories
table.1  <-  table(sofr$disp,useNA="ifany")
table.1
#     1     2     3     4 
# 25559 40589  4103  1450 
print(prop.table(table.1),digits=3)

#      1      2      3      4 
# 0.3565 0.5661 0.0572 0.0202 

        # Weighted counts in weighting categories
d0.dsgn <- svydesign(ids = ~0, # no clusters
                   strata = ~stratum, 
                   fpc = ~f0,
                   data = sofr,
                   weights = ~d0)

table.2 <- cbind(svytable(~disp, design=d0.dsgn),
                 svymean(~ as.factor(disp), design=d0.dsgn))
table.2
#       [,1]       [,2]
#1 320677.30 0.36843663
#2 474675.22 0.54536988
#3  55769.68 0.06407561
#4  19250.80 0.02211787


#------------------------------------------------------------------------------------------------
# Map disposition codes into AAPOR categories
        # respondents: respstat = 1
sofr$aapor[sofr$respstat==1] <- "I" 
        # partial respondents: respstat = 2
sofr$aapor[sofr$respstat==2] <- "P"
        # refusal/break-off: respstat = 3, 23
sofr$aapor[sofr$respstat==3|sofr$respstat==23] <- "R"
        # other eligible noninterview: respstat = 26
sofr$aapor[sofr$respstat==26] <- "O"
        # ineligible: respstat = 4, 18, 19, 22, 35
sofr$aapor[sofr$respstat==4|sofr$respstat==18|sofr$respstat==19|
          sofr$respstat==22|sofr$respstat==35] <- "NE"
        # unknown eligibility: respstat = 5, 25, 27, 29
sofr$aapor[sofr$respstat==5|sofr$respstat==25|sofr$respstat==27|sofr$respstat==29] <- "U" 

cbind(table(sofr$aapor,useNA="ifany"),
        round(100*table(sofr$aapor)/sum(table(sofr$aapor)),2)
)
(aapor.tab <- table(sofr$aapor,useNA="ifany"))
#    I    NE     O     P     R     U 
#25539  4103 39872    20   717  1450 
        # e = (I+P+R+O)/(I+P+R+O+NE)
e <- (aapor.tab[1]+aapor.tab[4]+aapor.tab[5]+aapor.tab[3]) /
        (aapor.tab[1]+aapor.tab[4]+aapor.tab[5]+aapor.tab[3]+aapor.tab[2])
e
#        I 
#0.9415951 
        
RR1 <- aapor.tab[1] / (aapor.tab[1]+aapor.tab[4]+aapor.tab[5]+aapor.tab[3]+aapor.tab[6])
RR1
#       I 
#0.377807 

RR4 <- aapor.tab[1] / (aapor.tab[1]+aapor.tab[4]+aapor.tab[5]+aapor.tab[3]+ e*aapor.tab[6])
RR4
#        I 
#0.3782809 

#---------------------------------------------------------------------------------------------------
        # Step 2: Distribute the unknown ineligibles weights to respondents, nonrespondents and known eligibles

# Compute weighted sums and weighted proportions
table.disp <- summaryBy(d0 ~ disp, data = sofr,
                            FUN = function(x) { c(sum.w = sum(x)) } )

print(table.disp)
#  disp d0.sum.w
#1    1 320677.30
#2    2 474675.22
#3    3  55769.68
#4    4  19250.80

# Weighted proportions of each disposition type
table.1.all <- prop.table(table.disp[,2])

#[1] 0.36843663 0.54536988 0.06407561 0.02211787

print(table.1.kn <- prop.table(table.disp[1:3,2]),digits=3)

#[1] 0.3768 0.5577 0.0655


        # sum of base wts for known eligibles
sum(table.disp[1:3,2])
#[1] 851122.2


# Compute adjustment factor: a1
a1 <- sum(table.disp[,2])/sum(table.disp[1:3,2])
a1
#1.022618

# Compute the adjusted weights
sofr$d1 <- sofr$d0 * a1

table.disp.1 <- summaryBy(d1 ~ disp, data = sofr,
                            FUN = function(x) { c(sum.w = sum(x)) } )
print(table.disp.1)

#  disp d1.sum.w
#1    1 327930.42
#2    2 485411.49
#3    3  57031.09
#4    4  19686.22


# Distributed weighted proportions
(table.2.all <- prop.table(table.disp.1[1:3,2]))

#[1] 0.37676999 0.55770513 0.06552488


        # Step 3a: Nonresponse adjustments

attach("C:\\Projects\\Practical Tools Book\\Book Chapters\\18 Solution Weighting\\sofr.d1.RData")
# exclude ineligibles and UNKNOWNS

pick <- sofr$disp!=3 & sofr$disp!=4
sofr.0 <- sofr[pick,]
dimnames(sofr.0)[[2]]

        # count of eligible respondents and nonrespondents
table(sofr.0$disp)
#    1     2 
#25559 40589 

# Compute resp variable: 1- Respondents, 0- Nonrespondents
sofr.0$resp <- abs(sofr.0$disp - 2)
table(sofr.0$resp)

#nonresp  resp
#    0     1 
#40589 25559


#---------------------------------------------------------------------------------------------------
# Step3a.1. NONRESPONSE ADJUSTMENT - METHOD1 
# Propensity score modeling

# Respondent/Nonrespondent tabs in Table 8- tabs by resp (excluding known and unknown eligibles)
        # by service
(counts.xsrrcr <- with(sofr.0,table(resp,xsrrcr,useNA="ifany")))
#    xsrrcr
#resp     1     2     3     4     5     6
#   0 10060  8398  4686  7869  4855  4721
#   1  5424  5179  3617  3283  4207  3849

prp.xsrrcr <- prop.table(counts.xsrrcr,2)
100*round(prp.xsrrcr,3)
#    xsrrcr
#resp    1    2    3    4    5    6
#   0 65.0 61.9 56.4 70.6 53.6 55.1
#   1 35.0 38.1 43.6 29.4 46.4 44.9

(counts.xreth4r <- with(sofr.0,table(resp,xreth4r,useNA="ifany")))
prp.xreth4r <- prop.table(counts.xreth4r,2)
round(prp.xreth4r,3)
#    xreth4r
#resp     1     2
#   0 20625 19964
#   1 16833  8726
 
#    xreth4r
#resp     1     2
#   0 0.551 0.696
#   1 0.449 0.304

(counts.xsexr <- with(sofr.0,table(resp,xsexr,useNA="ifany")))
prp.xsexr <- prop.table(counts.xsexr,2)
round(prp.xsexr,3)
#    xsexr
#resp     1     2
#   0 34100  6489
#   1 21007  4552
#    xsexr
#resp     1     2
#   0 0.619 0.588
#   1 0.381 0.412

(counts.xcpay1r <- with(sofr.0,table(resp,xcpay1r,useNA="ifany")))
prp.xcpay1r <- prop.table(counts.xcpay1r,2)
round(prp.xcpay1r,3)
#    xcpay1r
#resp     1     2     3     4     5     6     7
#   0  7026 12936 10146  2810   987  3185  3499
#   1  1494  4125  5653  3162  1356  3783  5986
#    xcpay1r
#resp     1     2     3     4     5     6     7
#   0 0.825 0.758 0.642 0.471 0.421 0.457 0.369
#   1 0.175 0.242 0.358 0.529 0.579 0.543 0.631

#   The following vars each have some missing values
(counts.srmarst <- with(sofr.0,table(resp,srmarst,useNA="ifany")))
prop.table(counts.srmarst,2)
(counts.xact2r <- with(sofr.0,table(resp,xact2r,useNA="ifany")))
prop.table(counts.xact2r,2)
(counts.sred <- with(sofr.0,table(resp,sred,useNA="ifany")))
prop.table(counts.sred,2)

# Fit a logistic model
# Main effects only           
glm.logit1  <-  glm(resp ~ as.factor(xsrrcr)
                        + as.factor(xreth4r)
                        + as.factor(xsexr)
                        + as.factor(xcpay1r),
                        family=binomial(link = "logit"),
                        data = sofr.0)
summary(glm.logit1)

#Call:
#glm(formula = resp ~ as.factor(xsrrcr) + as.factor(xreth4r) + 
#    as.factor(xsexr) + as.factor(xcpay1r), family = binomial(link = "logit"), 
#    data = sofr.0)

#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-1.6145  -0.8954  -0.6446   1.0915   2.0874  

#Coefficients:
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)         -1.39320    0.03495 -39.859  < 2e-16 ***
#as.factor(xsrrcr)2   0.12014    0.02620   4.585 4.54e-06 ***
#as.factor(xsrrcr)3   0.36529    0.02996  12.194  < 2e-16 ***
#as.factor(xsrrcr)4  -0.33442    0.02941 -11.372  < 2e-16 ***
#as.factor(xsrrcr)5   0.31690    0.02916  10.867  < 2e-16 ***
#as.factor(xsrrcr)6   0.28908    0.02974   9.721  < 2e-16 ***
#as.factor(xreth4r)2 -0.33083    0.01827 -18.105  < 2e-16 ***
#as.factor(xsexr)2    0.11087    0.02307   4.805 1.55e-06 ***
#as.factor(xcpay1r)2  0.25836    0.03453   7.483 7.26e-14 ***
#as.factor(xcpay1r)3  0.83612    0.03365  24.847  < 2e-16 ***
#as.factor(xcpay1r)4  1.53353    0.03923  39.090  < 2e-16 ***
#as.factor(xcpay1r)5  1.76907    0.05203  34.000  < 2e-16 ***
#as.factor(xcpay1r)6  1.47846    0.03838  38.526  < 2e-16 ***
#as.factor(xcpay1r)7  1.90332    0.03627  52.473  < 2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

#(Dispersion parameter for binomial family taken to be 1)

#    Null deviance: 88256  on 66147  degrees of freedom
#Residual deviance: 79833  on 66134  degrees of freedom
#AIC: 79861

#Number of Fisher Scoring iterations: 4

anova(glm.logit1, test="Chisq")
#Analysis of Deviance Table
#
#Model: binomial, link: logit
#
#Response: resp
#
#Terms added sequentially (first to last)
#
#                   Df Deviance Resid. Df Resid. Dev P(>|Chi|)    
#NULL                               66147      88256              
#as.factor(xsrrcr)   5    951.3     66142      87304 < 2.2e-16 ***
#as.factor(xreth4r)  1   1376.9     66141      85927 < 2.2e-16 ***
#as.factor(xsexr)    1     13.2     66140      85914 0.0002764 ***
#as.factor(xcpay1r)  6   6081.2     66134      79833 < 2.2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

# Main effects + 2-way interactions           
glm.logit2  <-  glm(resp ~ as.factor(xsrrcr)*as.factor(xreth4r)
                        + as.factor(xsrrcr)*as.factor(xsexr)
                        + as.factor(xsrrcr)*as.factor(xcpay1r)
                        + as.factor(xreth4r)*as.factor(xsexr)
                        + as.factor(xreth4r)*as.factor(xcpay1r)
                        + as.factor(xsexr)*as.factor(xcpay1r),
                        family=binomial(link = "logit"),
                        data = sofr.0)
summary(glm.logit2)
anova(glm.logit2, test="Chisq")
#Analysis of Deviance Table
#
#Model: binomial, link: logit
#
#Response: resp
#
#Terms added sequentially (first to last)
#                                      Df Deviance Resid. Df Resid. Dev P(>|Chi|)    
#NULL                                                  66147      88256              
#as.factor(xsrrcr)                      5    951.3     66142      87304 < 2.2e-16 ***
#as.factor(xreth4r)                     1   1376.9     66141      85927 < 2.2e-16 ***
#as.factor(xsexr)                       1     13.2     66140      85914 0.0002764 ***
#as.factor(xcpay1r)                     6   6081.2     66134      79833 < 2.2e-16 ***
#as.factor(xsrrcr):as.factor(xreth4r)   5     71.0     66129      79762 6.379e-14 ***
#as.factor(xsrrcr):as.factor(xsexr)     5     15.9     66124      79746 0.0070584 ** 
#as.factor(xsrrcr):as.factor(xcpay1r)  30    209.9     66094      79536 < 2.2e-16 ***
#as.factor(xreth4r):as.factor(xsexr)    1     31.2     66093      79505 2.291e-08 ***
#as.factor(xreth4r):as.factor(xcpay1r)  6      7.2     66087      79498 0.3004589    
#as.factor(xsexr):as.factor(xcpay1r)    6     36.8     66081      79461 1.937e-06 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 


#---------------------------------------------------------------------------------------------------
        #Create adjustment classes based on propensity scores for interaction model
        
L.hat  <-  glm.logit2$linear.predictors # Extract predicted values from logistic regression

pred.logit  <-  exp(L.hat) / (1 + exp(L.hat) ) # Convert predicted values to propensity scores

                # 5 adjustment classes based on propensity scores
quintiles5  <-  quantile(pred.logit, probs = seq(0,1,0.2))
p.class.5  <-  cut(pred.logit, breaks = quintiles5,include.lowest=T)
table(p.class.5,useNA="always")

#p.class5
#[0.0981,0.213]    (0.213,0.3]    (0.3,0.446]  (0.446,0.569]  (0.569,0.735]           <NA> 
#         13568          13539          13077          13213          12751              0 


        # compare different methods of computing a class propensity adjustment
cbind("Unweighted mean" = by(data = pred.logit, p.class.5, mean), 
                                # weights sofr.0$d1 are base wts adjusted for unknown eligibility
      "Weighted mean" = by(data = data.frame(pred.logit, wt = sofr.0$d1), 
                           p.class.5, 
                           function(x) {weighted.mean(x$pred.logit, x$wt)}),
      "Unweighted response rate" = by(as.numeric(sofr.0$resp), p.class.5, mean),
      "Weighted response rate" = by(data = data.frame(sofr.0,wt = sofr.0$d1), 
                                    p.class.5, 
                                    function(x) {weighted.mean(x$resp, x$wt)}), 
      "Median response propensity" = by(pred.logit, p.class.5, median)
)


#---------------------------------------------------------------------------------------------------
                #10 adjustment classes based on propensity scores
quintiles10  <-  quantile(pred.logit, probs = seq(0,1,0.1))
p.class.10  <-  cut(pred.logit, breaks = quintiles10,include.lowest=T)
table(p.class.10,useNA="always")

#p.class.10
#[0.0981,0.166]  (0.166,0.213]  (0.213,0.264]    (0.264,0.3]     (0.3,0.36]   (0.36,0.446] 
#          8218           5350           7600           5939           6053           7024 
# (0.446,0.519]  (0.519,0.569]  (0.569,0.627]  (0.627,0.735]           <NA> 
#          6182           7031           6136           6615              0 

        # compare different methods of computing a class propensity adjustment
cbind("Unweighted mean" = by(data = pred.logit, p.class.10, mean), 
      "Weighted mean" = by(data = data.frame(pred.logit, wt = sofr.0$d1), 
                           p.class.10, 
                           function(x) {weighted.mean(x$pred.logit, x$wt)}),
      "Unweighted response rate" = by(as.numeric(sofr.0$resp), p.class.10, mean),
      "Weighted response rate" = by(data = data.frame(sofr.0,wt = sofr.0$d1), 
                                    p.class.10, 
                                    function(x) {weighted.mean(x$resp, x$wt)}), 
      "Median response propensity" = by(pred.logit, p.class.10, median)
)


sofr.0$pred.logit <- pred.logit #merge propensity score into the data
#sofr.0$p.class.5 <- p.class.5   #merge 5 class identifiers
sofr.0$p.class.10 <- p.class.10 #merge 10 class identifiers

                              #compute unweighted mean propensity per class
#means.logit.5  <-  summaryBy(pred.logit ~ p.class.5,data=sofr.0,
#                          FUN = function(x) { c(mean.c = mean(x)) } )
means.logit.10  <-  summaryBy(pred.logit ~ p.class.10,data=sofr.0,
                          FUN = function(x) { c(mean.c = mean(x)) } )
 
            # boxplots of propensity scores by 5 and 10 adjustment classes
par(mfrow=c(1,2), mar=c(5, 4, 5, 2))

name5 <- names(table(p.class.5))
                      
boxplot(pred.logit~p.class.5,
        names=name5,
        las=2,
        col = "peach puff",
        #ylim=c(0.45,1),
        #xlab = "Class",
        cex.axis=0.7)
grid(nx = NULL, col = "gray50")
points(x=seq(from=1,to=5,by=1),y=means.logit.5[,2], pch = 19, col="blue")
title(main="Predicted Probabilities by 5 adjustment classes\nDots represent class mean", 
      cex.main=0.8)

name10 <- names(table(p.class.10))

boxplot(pred.logit~p.class.10,
        names=name10,
        las=2,
        col = "peach puff",
        #ylim=c(0.45,1),
        #xlab = "Class"
        cex.axis=0.7)
grid(nx = NULL, col = "gray50")
points(x=seq(from=1,to=10,by=1),y=means.logit.10[,2], pch = 19, col="blue")
title(main="Predicted Probabilities by 10 adjustment classes\nDots represent class mean", 
      cex.main=0.8)


        # comparison of individual propensity score and weighting cell adjustments
        # using median
#ind.wt  <-  1/pred.logit
#cl5.wt  <-  1/by(pred.logit, p.class.5, median)
#cl10.wt  <-  1/by(pred.logit, p.class.10, median)
#summary(ind.wt)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.373   1.839   2.770   3.256   4.154   8.834 

summary(cl5.wt)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.600   1.918   2.770   3.089   3.823   5.330 

summary(cl10.wt)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.450   1.922   2.870   3.210   3.975   6.627 


#---------------------------------------------------------------------------------------------------
# Weighted propensity model

sofr.1.dsgn  <-  
         svydesign(ids = ~0, # no clusters
                   strata = ~stratum, 
                   fpc = ~f0,
                   data = sofr.0,
                   weights = ~d1)

glm.logit.wt  <-  svyglm(resp ~ as.factor(xsrrcr)*as.factor(xreth4r)
                        + as.factor(xsrrcr)*as.factor(xsexr)
                        + as.factor(xsrrcr)*as.factor(xcpay1r)
                        + as.factor(xreth4r)*as.factor(xsexr)
                        + as.factor(xreth4r)*as.factor(xcpay1r)
                        + as.factor(xsexr)*as.factor(xcpay1r),
                        family=binomial(link = "logit"),
                        design = sofr.1.dsgn)
summary(glm.logit.wt)

        # test whether interaction terms are significant
regTermTest(glm.logit.wt, "as.factor(xsrrcr):as.factor(xreth4r)")
#Wald test for as.factor(xsrrcr):as.factor(xreth4r)
# in svyglm(resp ~ as.factor(xsrrcr) * as.factor(xreth4r) + as.factor(xsrrcr) * 
#    as.factor(xsexr) + as.factor(xsrrcr) * as.factor(xcpay1r) + 
#    as.factor(xreth4r) * as.factor(xsexr) + as.factor(xreth4r) * 
#    as.factor(xcpay1r) + as.factor(xsexr) * as.factor(xcpay1r), 
#    family = binomial(link = "logit"), design = sofr.1.dsgn)
#F =  4.304580  on  5  and  65678  df: p= 0.00064586 

regTermTest(glm.logit.wt, "as.factor(xsrrcr):as.factor(xsexr)")
#Wald test for as.factor(xsrrcr):as.factor(xsexr)
# in svyglm(resp ~ as.factor(xsrrcr) * as.factor(xreth4r) + as.factor(xsrrcr) * 
#    as.factor(xsexr) + as.factor(xsrrcr) * as.factor(xcpay1r) + 
#    as.factor(xreth4r) * as.factor(xsexr) + as.factor(xreth4r) * 
#    as.factor(xcpay1r) + as.factor(xsexr) * as.factor(xcpay1r), 
#    family = binomial(link = "logit"), design = sofr.1.dsgn)
#F =  3.453976  on  5  and  65678  df: p= 0.0040186 
 
regTermTest(glm.logit.wt, "as.factor(xsrrcr):as.factor(xcpay1r)")
#Wald test for as.factor(xsrrcr):as.factor(xcpay1r)
# in svyglm(resp ~ as.factor(xsrrcr) * as.factor(xreth4r) + as.factor(xsrrcr) * 
#    as.factor(xsexr) + as.factor(xsrrcr) * as.factor(xcpay1r) + 
#    as.factor(xreth4r) * as.factor(xsexr) + as.factor(xreth4r) * 
#    as.factor(xcpay1r) + as.factor(xsexr) * as.factor(xcpay1r), 
#    family = binomial(link = "logit"), design = sofr.1.dsgn)
#F =  4.171519  on  30  and  65678  df: p= 1.4361e-13 

regTermTest(glm.logit.wt, "as.factor(xreth4r):as.factor(xcpay1r)")
#Wald test for as.factor(xreth4r):as.factor(xcpay1r)
# in svyglm(resp ~ as.factor(xsrrcr) * as.factor(xreth4r) + as.factor(xsrrcr) * 
#    as.factor(xsexr) + as.factor(xsrrcr) * as.factor(xcpay1r) + 
#    as.factor(xreth4r) * as.factor(xsexr) + as.factor(xreth4r) * 
#    as.factor(xcpay1r) + as.factor(xsexr) * as.factor(xcpay1r), 
#    family = binomial(link = "logit"), design = sofr.1.dsgn)
#F =  1.070505  on  6  and  65678  df: p= 0.37752 

regTermTest(glm.logit.wt, "as.factor(xreth4r):as.factor(xsexr)")
#Wald test for as.factor(xreth4r):as.factor(xsexr)
# in svyglm(resp ~ as.factor(xsrrcr) * as.factor(xreth4r) + as.factor(xsrrcr) * 
#    as.factor(xsexr) + as.factor(xsrrcr) * as.factor(xcpay1r) + 
#    as.factor(xreth4r) * as.factor(xsexr) + as.factor(xreth4r) * 
#    as.factor(xcpay1r) + as.factor(xsexr) * as.factor(xcpay1r), 
#    family = binomial(link = "logit"), design = sofr.1.dsgn)
#F =  3.913208  on  1  and  65678  df: p= 0.047912 

regTermTest(glm.logit.wt, "as.factor(xsexr):as.factor(xcpay1r)")
#Wald test for as.factor(xsexr):as.factor(xcpay1r)
# in svyglm(resp ~ as.factor(xsrrcr) * as.factor(xreth4r) + as.factor(xsrrcr) * 
#    as.factor(xsexr) + as.factor(xsrrcr) * as.factor(xcpay1r) + 
#    as.factor(xreth4r) * as.factor(xsexr) + as.factor(xreth4r) * 
#    as.factor(xcpay1r) + as.factor(xsexr) * as.factor(xcpay1r), 
#    family = binomial(link = "logit"), design = sofr.1.dsgn)
#F =  1.341596  on  6  and  65678  df: p= 0.23451 


#---------------------------------------------------------------------------------------------------
# Step 3a.2. NONRESPONSE ADJUSTMENT - METHOD2 
# Forming adjustment classes by classification trees

set.seed(611033736)

        # Classification tree including xcpay1r+xreth4r+xsexr+xsrrcr
        # minbucket is set to 250 after running some iterations (iterations are not shown here)
                  
t1  <-  rpart(resp ~ xcpay1r + xreth4r + xsexr + xsrrcr,
                   method = "class",
                   control = rpart.control(minbucket = 250, cp=0),
                   data = sofr.0)

print(t1, digits=4)

#n= 66148 

#node), split, n, loss, yval, (yprob)
#      * denotes terminal node

#  1) root 66148 25560 0 (0.6136 0.3864)  
#    2) xcpay1r< 3.5 41380 11270 0 (0.7276 0.2724) *
#    3) xcpay1r>=3.5 24768 10480 1 (0.4232 0.5768)  
#      6) xcpay1r< 6.5 15283  6982 1 (0.4568 0.5432)  
#       12) xreth4r>=1.5 4966  2423 0 (0.5121 0.4879)  
#         24) xsrrcr< 4.5 3727  1766 0 (0.5262 0.4738)  
#           48) xsrrcr>=3.5 669   285 0 (0.5740 0.4260) *
#           49) xsrrcr< 3.5 3058  1481 0 (0.5157 0.4843)  
#             98) xsrrcr< 1.5 1125   510 0 (0.5467 0.4533) *
#             99) xsrrcr>=1.5 1933   962 1 (0.4977 0.5023)  
#              198) xsrrcr< 2.5 1637   807 0 (0.5070 0.4930)  
#                396) xsexr>=1.5 473   222 0 (0.5307 0.4693) *
#                397) xsexr< 1.5 1164   579 1 (0.4974 0.5026)  
#                  794) xcpay1r< 4.5 562   279 0 (0.5036 0.4964) *
#                  795) xcpay1r>=4.5 602   296 1 (0.4917 0.5083) *
#              199) xsrrcr>=2.5 296   132 1 (0.4459 0.5541) *
#         25) xsrrcr>=4.5 1239   582 1 (0.4697 0.5303) *
#       13) xreth4r< 1.5 10317  4439 1 (0.4303 0.5697)  
#         26) xsrrcr>=3.5 4376  1980 1 (0.4525 0.5475)  
#           52) xsrrcr< 4.5 1055   476 0 (0.5488 0.4512)  
#            104) xcpay1r< 4.5 494   190 0 (0.6154 0.3846) *
#            105) xcpay1r>=4.5 561   275 1 (0.4902 0.5098) *
#           53) xsrrcr>=4.5 3321  1401 1 (0.4219 0.5781) *
#         27) xsrrcr< 3.5 5941  2459 1 (0.4139 0.5861) *
#      7) xcpay1r>=6.5 9485  3499 1 (0.3689 0.6311) *

par(mfrow = c(1,1))
plot(t1, uniform=TRUE, compress=TRUE, margin = 0.1)
text(t1, use.n=TRUE, all=TRUE,
     digits=15,
     cex=1,
     pretty=1.2,
     fancy=FALSE,
     xpd = TRUE,
     font = 3)

sofr.0 <- cbind(sofr.0,nr.class=t1$where) # Merge class identifier

unwt.rr  <-  by(as.numeric(sofr.0[, "resp"]), t1$where, mean)
with(sofr.0,mean(resp))
#[1] 0.3863911  # Overall unweighted response rate


            # tabulate number of persons (R + NR) in each class
table(t1$where,useNA="ifany")

#    2     7     9    12    14    15    16    17    21    22    23    24    25 
#41380   669  1125   473   562   602   296  1239   494   561  3321  5941  9485 

# Weighted response rate
wt.rr  <-  by(data = data.frame(resp = as.numeric(sofr.0[,"resp"]),
            wt = sofr.0[,"d1"]),
            t1$where,
            function(x) {weighted.mean(x$resp, x$wt)} )

            # unwtd and wtd response rates in cart cells
tree.rr  <-  cbind(nr.class=as.numeric(names(wt.rr)), unwt.rr, wt.rr)

weighted.mean(sofr.0$resp, sofr.0$d1)
#[1] 0.4031889   # Overall weighted response rate


# merge NR class and response rates onto sofr.0 file
sofr.0.NR  <-  merge(sofr.0, data.frame(tree.rr), by="nr.class")
sofr.0.NR  <-  sofr.0.NR[order(sofr.0.NR$rec.id),]

        # Counts of R + NR for the 4 vars used to form the tree
with(sofr.0,table(nr.class,xcpay1r,useNA="ifany"))
#        xcpay1r
#nr.class     1     2     3     4     5     6     7
#      2   8520 17061 15799     0     0     0     0
#      7      0     0     0   502    89    78     0
#      9      0     0     0   468   130   527     0
#      12     0     0     0   199    26   248     0
#      14     0     0     0   562     0     0     0
#      15     0     0     0     0   108   494     0
#      16     0     0     0   118    14   164     0
#      17     0     0     0   733     2   504     0
#      21     0     0     0   494     0     0     0
#      22     0     0     0     0   287   274     0
#      23     0     0     0  1419     5  1897     0
#      24     0     0     0  1477  1682  2782     0
#      25     0     0     0     0     0     0  9485


with(sofr.0,table(nr.class,xreth4r,useNA="ifany"))
#        xreth4r
#nr.class     1     2
#      2  19532 21848
#      7      0   669
#      9      0  1125
#      12     0   473
#      14     0   562
#      15     0   602
#      16     0   296
#      17     0  1239
#      21   494     0
#      22   561     0
#      23  3321     0
#      24  5941     0
#      25  7609  1876


with(sofr.0,table(nr.class,xsexr,useNA="ifany"))
#        xsexr
#nr.class     1     2
#      2  34517  6863
#      7    614    55
#      9    988   137
#      12     0   473
#      14   562     0
#      15   602     0
#      16   219    77
#      17   931   308
#      21   384   110
#      22   517    44
#      23  2598   723
#      24  5079   862
#      25  8096  1389

with(sofr.0,table(nr.class,xsrrcr,useNA="ifany"))
#        xsrrcr
#nr.class     1     2     3     4     5     6
#      2  10061  8369  5740  7144  5089  4977
#      7      0     0     0   669     0     0
#      9   1125     0     0     0     0     0
#      12     0   473     0     0     0     0
#      14     0   562     0     0     0     0
#      15     0   602     0     0     0     0
#      16     0     0   296     0     0     0
#      17     0     0     0     0   645   594
#      21     0     0     0   494     0     0
#      22     0     0     0   561     0     0
#      23     0     0     0     0  1891  1430
#      24  3005  2004   932     0     0     0
#      25  1293  1567  1335  2284  1437  1569

sofr.d2 <- sofr.0


#---------------------------------------------------------------------------------------------------
# Step 4. CALIBRATION

# main effects only models to see which covariates are related to survey response variables
# Use only respondents data to fit the model

datafile <- sofr.0[sofr.0$resp==1,]

            # Variables RA006A, RA006B, RA008, RA115, RA118 are coded as
            # 1 = very dissatisfied
            # 2 = dissatisfied
            # 3 = neither satisfied or dissatisfied
            # 4 = satisfied
            # 5 = very satisfied
            
            # recode as 
            # 0 = 1-3
            # 1 = 4-5

        # total compensation
datafile$ra006aR <- datafile$ra006a
a<- (datafile$ra006a %in% 1:3) 
datafile$ra006aR[a] <- 0
datafile$ra006aR[!a] <- 1
table(datafile$ra006a)
table(datafile$ra006aR)
        # type of work    
datafile$ra006bR <- datafile$ra006b
a<- (datafile$ra006b %in% 1:3) 
datafile$ra006bR[a] <- 0
datafile$ra006bR[!a] <- 1
table(datafile$ra006b)
cumsum(table(datafile$ra006b))
table(datafile$ra006bR)
        # likely to re-enlist
datafile$ra008R <- datafile$ra008
a<- (datafile$ra008 %in% 1:3) 
datafile$ra008R[a] <- 0
datafile$ra008R[!a] <- 1
table(datafile$ra008)
cumsum(table(datafile$ra008))
table(datafile$ra008R)
        # prepared for job
datafile$ra115R <- datafile$ra115
a<- (datafile$ra115 %in% 1:3) 
datafile$ra115R[a] <- 0
datafile$ra115R[!a] <- 1
table(datafile$ra115)
cumsum(table(datafile$ra115))
table(datafile$ra115R)
        # level of stress
        # 1 Much less than usual 
        # 2 Less than usual 
        # 3 About the same as usual
        # 4 More than usual
        # 5 Much more than usual
datafile$ra118R <- datafile$ra118
a<- (datafile$ra118 %in% 1:3) 
datafile$ra118R[a] <- 0
datafile$ra118R[!a] <- 1
table(datafile$ra118)
cumsum(table(datafile$ra118))
table(datafile$ra118R)

            # satisfaction with total compensation
#model.1 <- glm(ra006a~as.factor(xsrrcr) + as.factor(xsexr) + as.factor(xcpay1r) + 
#               as.factor(xreth4r) + as.factor(sred) + as.factor(srmarst) + as.factor(xact2r),
#               data=datafile)
model.1 <- glm(ra006bR~as.factor(xsrrcr) + as.factor(xsexr) + as.factor(xcpay1r) + 
               as.factor(xreth4r) + as.factor(sred) + as.factor(srmarst) + as.factor(xact2r),
               data=datafile)

model.1 <- glm(ra006aR ~ as.factor(xsrrcr):as.factor(xsexr) 
                     + as.factor(xsrrcr):as.factor(xcpay1r)  
                     + as.factor(xsrrcr):as.factor(xreth4r) 
                     + as.factor(xsrrcr):as.factor(sred) 
                     + as.factor(xsrrcr):as.factor(srmarst) 
                     + as.factor(xsrrcr):as.factor(xact2r)
                     
                     + as.factor(xsexr):as.factor(xcpay1r)  
                     + as.factor(xsexr):as.factor(xreth4r)
                     + as.factor(xsexr):as.factor(sred) 
                     + as.factor(xsexr):as.factor(srmarst)
                     + as.factor(xsexr):as.factor(xact2r)
                     
                     + as.factor(xcpay1r):as.factor(xreth4r)
                     + as.factor(xcpay1r):as.factor(sred) 
                     + as.factor(xcpay1r):as.factor(srmarst)
                     + as.factor(xcpay1r):as.factor(xact2r)
                     
                     + as.factor(xreth4r):as.factor(sred)
                     + as.factor(xreth4r):as.factor(srmarst)
                     + as.factor(xreth4r):as.factor(xact2r) 
                     
                     + as.factor(sred):as.factor(srmarst)
                     + as.factor(sred):as.factor(xact2r)
                     
                     + as.factor(srmarst):as.factor(xact2r),
               family=binomial(link = "logit"),
               data=datafile)
anova(model.1, test="Chisq")

model.2 <- glm(ra006bR ~ as.factor(xsrrcr):as.factor(xsexr) 
                     + as.factor(xsrrcr):as.factor(xcpay1r)  
                     + as.factor(xsrrcr):as.factor(xreth4r) 
                     + as.factor(xsrrcr):as.factor(sred) 
                     + as.factor(xsrrcr):as.factor(srmarst) 
                     + as.factor(xsrrcr):as.factor(xact2r)
                     
                     + as.factor(xsexr):as.factor(xcpay1r)  
                     + as.factor(xsexr):as.factor(xreth4r)
                     + as.factor(xsexr):as.factor(sred) 
                     + as.factor(xsexr):as.factor(srmarst)
                     + as.factor(xsexr):as.factor(xact2r)
                     
                     + as.factor(xcpay1r):as.factor(xreth4r)
                     + as.factor(xcpay1r):as.factor(sred) 
                     + as.factor(xcpay1r):as.factor(srmarst)
                     + as.factor(xcpay1r):as.factor(xact2r)
                     
                     + as.factor(xreth4r):as.factor(sred)
                     + as.factor(xreth4r):as.factor(srmarst)
                     + as.factor(xreth4r):as.factor(xact2r) 
                     
                     + as.factor(sred):as.factor(srmarst)
                     + as.factor(sred):as.factor(xact2r)
                     
                     + as.factor(srmarst):as.factor(xact2r),
                     family=binomial(link = "logit"),
               data=datafile)
anova(model.2, test="Chisq"))

model.3 <- glm(ra008R ~ as.factor(xsrrcr):as.factor(xsexr) 
                     + as.factor(xsrrcr):as.factor(xcpay1r)  
                     + as.factor(xsrrcr):as.factor(xreth4r) 
                     + as.factor(xsrrcr):as.factor(sred) 
                     + as.factor(xsrrcr):as.factor(srmarst) 
                     + as.factor(xsrrcr):as.factor(xact2r)
                     
                     + as.factor(xsexr):as.factor(xcpay1r)  
                     + as.factor(xsexr):as.factor(xreth4r)
                     + as.factor(xsexr):as.factor(sred) 
                     + as.factor(xsexr):as.factor(srmarst)
                     + as.factor(xsexr):as.factor(xact2r)
                     
                     + as.factor(xcpay1r):as.factor(xreth4r)
                     + as.factor(xcpay1r):as.factor(sred) 
                     + as.factor(xcpay1r):as.factor(srmarst)
                     + as.factor(xcpay1r):as.factor(xact2r)
                     
                     + as.factor(xreth4r):as.factor(sred)
                     + as.factor(xreth4r):as.factor(srmarst)
                     + as.factor(xreth4r):as.factor(xact2r) 
                     
                     + as.factor(sred):as.factor(srmarst)
                     + as.factor(sred):as.factor(xact2r)
                     
                     + as.factor(srmarst):as.factor(xact2r),
                     family=binomial(link = "logit"),
               data=datafile)
anova(model.3, test="Chisq"))

model.4 <- glm(ra115 ~ as.factor(xsrrcr):as.factor(xsexr) 
                     + as.factor(xsrrcr):as.factor(xcpay1r)  
                     + as.factor(xsrrcr):as.factor(xreth4r) 
                     + as.factor(xsrrcr):as.factor(sred) 
                     + as.factor(xsrrcr):as.factor(srmarst) 
                     + as.factor(xsrrcr):as.factor(xact2r)
                     
                     + as.factor(xsexr):as.factor(xcpay1r)  
                     + as.factor(xsexr):as.factor(xreth4r)
                     + as.factor(xsexr):as.factor(sred) 
                     + as.factor(xsexr):as.factor(srmarst)
                     + as.factor(xsexr):as.factor(xact2r)
                     
                     + as.factor(xcpay1r):as.factor(xreth4r)
                     + as.factor(xcpay1r):as.factor(sred) 
                     + as.factor(xcpay1r):as.factor(srmarst)
                     + as.factor(xcpay1r):as.factor(xact2r)
                     
                     + as.factor(xreth4r):as.factor(sred)
                     + as.factor(xreth4r):as.factor(srmarst)
                     + as.factor(xreth4r):as.factor(xact2r) 
                     
                     + as.factor(sred):as.factor(srmarst)
                     + as.factor(sred):as.factor(xact2r)
                     
                     + as.factor(srmarst):as.factor(xact2r),
                     family=binomial(link = "logit"),
               data=datafile)
anova(model.4, test="Chisq"))

model.5 <- glm(ra118 ~ as.factor(xsrrcr):as.factor(xsexr) 
                     + as.factor(xsrrcr):as.factor(xcpay1r)  
                     + as.factor(xsrrcr):as.factor(xreth4r) 
                     + as.factor(xsrrcr):as.factor(sred) 
                     + as.factor(xsrrcr):as.factor(srmarst) 
                     + as.factor(xsrrcr):as.factor(xact2r)
                     
                     + as.factor(xsexr):as.factor(xcpay1r)  
                     + as.factor(xsexr):as.factor(xreth4r)
                     + as.factor(xsexr):as.factor(sred) 
                     + as.factor(xsexr):as.factor(srmarst)
                     + as.factor(xsexr):as.factor(xact2r)
                     
                     + as.factor(xcpay1r):as.factor(xreth4r)
                     + as.factor(xcpay1r):as.factor(sred) 
                     + as.factor(xcpay1r):as.factor(srmarst)
                     + as.factor(xcpay1r):as.factor(xact2r)
                     
                     + as.factor(xreth4r):as.factor(sred)
                     + as.factor(xreth4r):as.factor(srmarst)
                     + as.factor(xreth4r):as.factor(xact2r) 
                     
                     + as.factor(sred):as.factor(srmarst)
                     + as.factor(sred):as.factor(xact2r)
                     
                     + as.factor(srmarst):as.factor(xact2r),
                     family=binomial(link = "logit"),
               data=datafile)
anova(model.5, test="Chisq"))

t1 <- rpart(ra006aR ~ xsrrcr + xsexr + xcpay1r + xreth4r + sred + srmarst + xact2r,
                   method = "class",
                   control = rpart.control(minbucket = 250, cp=0),
                   data = datafile)

print(t1, digits=4)

#n= 25559 
#
#node), split, n, loss, yval, (yprob)
#      * denotes terminal node
#
#  1) root 25559 8813 1 (0.3448 0.6552)  
#    2) xcpay1r< 3.5 11272 5067 1 (0.4495 0.5505)  
#      4) xcpay1r< 2.5 5619 2767 1 (0.4924 0.5076)  
#        8) xsrrcr< 4.5 3886 1799 0 (0.5371 0.4629)  
#         16) xsrrcr>=3.5 639  235 0 (0.6322 0.3678) *
#         17) xsrrcr< 3.5 3247 1564 0 (0.5183 0.4817)  
#           34) sred>=2.5 2507 1175 0 (0.5313 0.4687)  
#             68) xsrrcr< 2.5 1716  775 0 (0.5484 0.4516)  
#              136) xsrrcr>=1.5 820  346 0 (0.5780 0.4220) *
#              137) xsrrcr< 1.5 896  429 0 (0.5212 0.4788)  
#                274) xreth4r>=1.5 542  243 0 (0.5517 0.4483) *
#                275) xreth4r< 1.5 354  168 1 (0.4746 0.5254) *
#             69) xsrrcr>=2.5 791  391 1 (0.4943 0.5057)  
#              138) xact2r>=2.5 447  203 0 (0.5459 0.4541) *
#              139) xact2r< 2.5 344  147 1 (0.4273 0.5727) *
#           35) sred< 2.5 740  351 1 (0.4743 0.5257) *
#        9) xsrrcr>=4.5 1733  680 1 (0.3924 0.6076) *
#      5) xcpay1r>=2.5 5653 2300 1 (0.4069 0.5931) *
#    3) xcpay1r>=3.5 14287 3746 1 (0.2622 0.7378) *


t1 <- rpart(ra006bR ~ xsrrcr + xsexr + xcpay1r + xreth4r + sred + srmarst + xact2r,
                   method = "class",
                   control = rpart.control(minbucket = 100, cp=0),
                   data = datafile)

print(t1, digits=4)

#n= 25559 
#
#node), split, n, loss, yval, (yprob)
#      * denotes terminal node
#
#  1) root 25559 5567 1 (0.2178 0.7822)  
#    2) xcpay1r< 3.5 11272 3369 1 (0.2989 0.7011)  
#      4) xcpay1r< 2.5 5619 1978 1 (0.3520 0.6480)  
#        8) xsrrcr< 4.5 3886 1543 1 (0.3971 0.6029)  
#         16) sred>=3.5 2216  949 1 (0.4282 0.5718)  
#           32) srmarst>=3.5 1113  514 1 (0.4618 0.5382)  
#             64) xact2r< 2.5 547  270 1 (0.4936 0.5064)  
#              128) xsrrcr< 2.5 332  157 0 (0.5271 0.4729) *
#              129) xsrrcr>=2.5 215   95 1 (0.4419 0.5581) *
#             65) xact2r>=2.5 566  244 1 (0.4311 0.5689) *
#           33) srmarst< 3.5 1103  435 1 (0.3944 0.6056) *
#         17) sred< 3.5 1670  594 1 (0.3557 0.6443) *
#        9) xsrrcr>=4.5 1733  435 1 (0.2510 0.7490) *
#      5) xcpay1r>=2.5 5653 1391 1 (0.2461 0.7539) *
#    3) xcpay1r>=3.5 14287 2198 1 (0.1538 0.8462) *

t1 <- rpart(ra008R ~ xsrrcr + xsexr + xcpay1r + xreth4r + sred + srmarst + xact2r,
                   method = "class",
                   control = rpart.control(minbucket = 250, cp=0),
                   data = datafile)

print(t1, digits=4)

#n= 25559 
#
#node), split, n, loss, yval, (yprob)
#      * denotes terminal node
#
# 1) root 25559 5605 1 (0.2193 0.7807)  
#   2) xcpay1r< 2.5 5619 2257 1 (0.4017 0.5983)  
#     4) xsrrcr< 4.5 3886 1719 1 (0.4424 0.5576)  
#       8) xsrrcr>=3.5 639  241 0 (0.6228 0.3772) *
#       9) xsrrcr< 3.5 3247 1321 1 (0.4068 0.5932)  
#        18) xsrrcr< 2.5 2278 1036 1 (0.4548 0.5452)  
#          36) xact2r< 2.5 1221  600 0 (0.5086 0.4914)  
#            72) srmarst>=4 636  276 0 (0.5660 0.4340) *
#            73) srmarst< 4 585  261 1 (0.4462 0.5538) *
#          37) xact2r>=2.5 1057  415 1 (0.3926 0.6074) *
#        19) xsrrcr>=2.5 969  285 1 (0.2941 0.7059) *
#     5) xsrrcr>=4.5 1733  538 1 (0.3104 0.6896) *
#   3) xcpay1r>=2.5 19940 3348 1 (0.1679 0.8321) *

t1 <- rpart(ra115R ~ xsrrcr + xsexr + xcpay1r + xreth4r + sred + srmarst + xact2r,
                   method = "class",
                   control = rpart.control(minbucket = 100, cp=0),
                   data = datafile)

print(t1, digits=4)

#n= 25559 
#
#node), split, n, loss, yval, (yprob)
#      * denotes terminal node
#
# 1) root 25559 4156 1 (0.1626 0.8374)  
#   2) xcpay1r< 2.5 5619 1622 1 (0.2887 0.7113)  
#     4) xact2r>=2.5 2579  952 1 (0.3691 0.6309)  
#       8) xsexr>=1.5 636  306 1 (0.4811 0.5189)  
#        16) xreth4r>=1.5 384  168 0 (0.5625 0.4375) *
#        17) xreth4r< 1.5 252   90 1 (0.3571 0.6429) *
#       9) xsexr< 1.5 1943  646 1 (0.3325 0.6675) *
#     5) xact2r< 2.5 3040  670 1 (0.2204 0.7796) *
#   3) xcpay1r>=2.5 19940 2534 1 (0.1271 0.8729) *

t1 <- rpart(ra118R ~ xsrrcr + xsexr + xcpay1r + xreth4r + sred + srmarst + xact2r,
                   method = "class",
                   control = rpart.control(minbucket = 400, cp=0),
                   data = datafile)

print(t1, digits=4)

#n= 25559 
#
#node), split, n, loss, yval, (yprob)
#      * denotes terminal node
#
#  1) root 25559 11560 0 (0.5479 0.4521)  
#    2) xreth4r>=1.5 8726  3513 0 (0.5974 0.4026) *
#    3) xreth4r< 1.5 16833  8042 0 (0.5222 0.4778)  
#      6) xact2r>=2.5 7845  3450 0 (0.5602 0.4398)  
#       12) xcpay1r< 5.5 3796  1569 0 (0.5867 0.4133) *
#       13) xcpay1r>=5.5 4049  1881 0 (0.5354 0.4646)  
#         26) xsrrcr< 3.5 2064   924 0 (0.5523 0.4477) *
#         27) xsrrcr>=3.5 1985   957 0 (0.5179 0.4821)  
#           54) xcpay1r< 6.5 641   293 0 (0.5429 0.4571) *
#           55) xcpay1r>=6.5 1344   664 0 (0.5060 0.4940)  
#            110) xsrrcr>=4.5 727   348 0 (0.5213 0.4787) *
#            111) xsrrcr< 4.5 617   301 1 (0.4878 0.5122) *
#      7) xact2r< 2.5 8988  4396 1 (0.4891 0.5109)  
#       14) xcpay1r< 4.5 4713  2234 0 (0.5260 0.4740)  
#         28) xsrrcr>=4.5 2016   877 0 (0.5650 0.4350) *
#         29) xsrrcr< 4.5 2697  1340 1 (0.4968 0.5032)  
#           58) srmarst>=2.5 1033   475 0 (0.5402 0.4598) *
#           59) srmarst< 2.5 1664   782 1 (0.4700 0.5300) *
#       15) xcpay1r>=4.5 4275  1917 1 (0.4484 0.5516)  
#         30) srmarst>=2.5 824   409 0 (0.5036 0.4964) *
#         31) srmarst< 2.5 3451  1502 1 (0.4352 0.5648) *

#------------------------------------------------------------------------------------------------
# Set population controls
# Computed in SAS
# Missing data are imputed with the most frequent value in the population counts

pop.counts <- sasxport.get(paste(file_loc,"rccpds57.xpt",sep=""))

        # tabulate number of missing for each variable
table(pop.counts$service, useNA="always")
#   1    2    3    4    5    6 <NA> 
# 859  854  762  551  704  631    4 
table(pop.counts$gender, useNA="always")
#   1    2 <NA> 
#2733 1628    4 
table(pop.counts$pg.group, useNA="always")
#   1    2    3    4    5    6    7 <NA> 
# 484  891 1056  715  251  495  468    5 
table(pop.counts$raceth, useNA="always")
#   1    2 <NA> 
#2350 1992   23 
table(pop.counts$educcat, useNA="always")
#   1    2    3    4    5    6    7 <NA> 
#  84  472  543  717  614  919  652  364 
table(pop.counts$marit, useNA="always")
#   1    2    3    4    5 <NA> 
#1724  298  969   70 1266   38 
table(pop.counts$activatd, useNA="always")
#   1    2    3 <NA> 
# 409 1897 1903  156 
 

#_____________________________________________________________________________________________
# Impute missing covariate values in pop count file. A random draw is made from allowable
# codes in proportion to the pop code-counts for non-missing records.

set.seed(-1570473091)
new.cnts <- pop.counts

impute <- function(col.no){
    rows <- 1:(nrow(cnts)-1)
    codes <- cnts[rows,1]
    prop <- cnts[rows,2]/sum(cnts[rows,2])
    n.impute <- sum(is.na(pop.counts[,col.no]))
    imp <- rep(0,n.impute)

    for (i in 1:n.impute){
        imp[i] <- codes[UPrandomsystematic(prop)==1]
    }
    NAs <- (1:nrow(pop.counts))[is.na(pop.counts[,col.no])]
    new.cnts[NAs,col.no] <- imp
    new.cnts
}

cnts <- summaryBy(count ~ service, data = pop.counts,
                            FUN = function(x) { c(sum.w = sum(x)) } )
new.cnts <- impute(col.no=1)

cnts <- summaryBy(count ~ gender, data = pop.counts,
                            FUN = function(x) { c(sum.w = sum(x)) } )
new.cnts <- impute(col.no=2)

cnts <- summaryBy(count ~ pg.group, data = pop.counts,
                            FUN = function(x) { c(sum.w = sum(x)) } )
new.cnts <- impute(col.no=3)

cnts <- summaryBy(count ~ raceth, data = pop.counts,
                            FUN = function(x) { c(sum.w = sum(x)) } )
new.cnts <- impute(col.no=4)

cnts <- summaryBy(count ~ educcat, data = pop.counts,
                            FUN = function(x) { c(sum.w = sum(x)) } )
new.cnts <- impute(col.no=5)

cnts <- summaryBy(count ~ marit, data = pop.counts,
                            FUN = function(x) { c(sum.w = sum(x)) } )
new.cnts <- impute(col.no=6)

cnts <- summaryBy(count ~ activatd, data = pop.counts,
                            FUN = function(x) { c(sum.w = sum(x)) } )
new.cnts <- impute(col.no=7)


        # re-tabulate after imputation
table(new.cnts$service, useNA="always")
table(new.cnts$gender, useNA="always")
table(new.cnts$pg.group, useNA="always")
table(new.cnts$raceth, useNA="always")
table(new.cnts$educcat, useNA="always")
table(new.cnts$marit, useNA="always")
table(new.cnts$activatd, useNA="always")

#_____________________________________________________________________________________________

        # Tabulate new pop counts in categories to be used in calibration
sum(summaryBy(count ~ service, data = new.cnts,
                            FUN = function(x) { c(sum.w = sum(x)) } ))
sum(summaryBy(count ~ pg.group, data = new.cnts,
                            FUN = function(x) { c(sum.w = sum(x)) } ))
sum(summaryBy(count ~ activatd, data = new.cnts,
                            FUN = function(x) { c(sum.w = sum(x)) } ))
 
                            
svc.pay <- summaryBy(count ~ service + pg.group, data = new.cnts,
                            FUN = function(x) { c(sum.w = sum(x)) } )
svc <- summaryBy(count.sum.w ~ service, data = svc.pay,
                            FUN = function(x) { c(sum.w = sum(x)) } )
pay <- summaryBy(count.sum.w ~ pg.group, data = svc.pay,
                            FUN = function(x) { c(sum.w = sum(x)) } )
                            
svc.act <- summaryBy(count ~ service + activatd, data = new.cnts,
                            FUN = function(x) { c(sum.w = sum(x)) } )
activated <- summaryBy(count.sum.w ~ activatd, data = svc.act,
                            FUN = function(x) { c(sum.w = sum(x)) } )
                            
educ <-  summaryBy(count ~ educcat, data = new.cnts,
                            FUN = function(x) { c(sum.w = sum(x)) } )
gender <-  summaryBy(count ~ gender, data = new.cnts,
                            FUN = function(x) { c(sum.w = sum(x)) } )
raceth <-  summaryBy(count ~ raceth, data = new.cnts,
                            FUN = function(x) { c(sum.w = sum(x)) } )
marital <-  summaryBy(count ~ marit, data = new.cnts,
                            FUN = function(x) { c(sum.w = sum(x)) } )

        # check totals vs. N
        # After imputation all pop counts sum to 801,809 which is the total count in 
        # the RCCPDS57 pop count file.
(N <- sum(pop.counts$count))
#[1] 801809

sum(svc.pay[,3])
#[1] 801809
sum(svc.act[,3])
#[1] 801809
sum(educ[,2])
#[1] 801809
sum(gender[,2])
#[1] 801809
sum(raceth[,2])
#[1] 801809
sum(marital[,2])
#[1] 801809

                            
#_____________________________________________________________________________________________
# Impute missing values for education and activation in the sample file

        # extract respondents
pick <- sofr.0$disp == 1
sofr.0.R <- sofr.0[pick,]
table(sofr.0.R$disp)
tmp2 <- sofr.0.R

impute.sam <- function(col.no){
#browser()
    cnts <- table(tmp2[,col.no],useNA="always")
#    cnts <- table(sofr.cal[,10],useNA="always")
    
    codes <- as.numeric(names(cnts[-length(cnts)]))
    prop <- cnts[-length(cnts)]/sum(cnts[-length(cnts)])
    n.impute <- cnts[length(cnts)]
    imp <- rep(0,n.impute)

    for (i in 1:n.impute){
        imp[i] <- codes[UPrandomsystematic(prop)==1]
    }
    NAs <- (1:nrow(tmp2))[is.na(tmp2[,col.no])]
    tmp2[NAs,col.no] <- imp
    tmp2
}

        # col 9 = sred
        # col 12 = xact2r

tmp2 <- impute.sam(9)
table(sofr.0.R$sred, useNA="always")
table(tmp2$sred, useNA="always")

tmp2 <- impute.sam(12)
table(sofr.0.R$xact2r, useNA="always")
table(tmp2$xact2r, useNA="always")

sofr.0.R <- tmp2

#_____________________________________________________________________________________________


###############################################################################################################################
# COMPUTE UNBOUNDED GREG WEIGHTS                                                                                               
# Please note that bounded GREG estimation is not converged for various iterations
# Since there are no negative weights, the solution from unbounded GREG estimation will be kept

# Based on the Chapter 13 assignment, we can assume that the pop counts do not include 
# any ineligibles.

# merge NR class and response rates onto sofr.rie file

tmp1  <-  cbind(nr.class=as.numeric(names(wt.rr)), unwt.rr, wt.rr)
sofr.cal  <-  merge(sofr.0.R, data.frame(tmp1), by="nr.class", all=TRUE)
sofr.cal  <-  sofr.cal[order(sofr.cal$rec.id),]

with(sofr.cal,table(disp,useNA="ifany"))
with(sofr.cal,table(d1,useNA="ifany"))
with(sofr.cal,table(unwt.rr,useNA="ifany"))
with(sofr.cal,table(wt.rr,useNA="ifany"))

sofr.cal$a2 <- 1/(sofr.cal$wt.rr) # Use weighted response rates for NR adjustment
sofr.cal$d2 <- sofr.cal$d1 * sofr.cal$a2


with(sofr.cal,table(xsrrcr,useNA="ifany"))
#xsrrcr
#   1    2    3    4    5    6 
#5424 5179 3617 3283 4207 3849 

with(sofr.cal,table(xcpay1r,useNA="ifany"))
#xcpay1r
#   1    2    3    4    5    6    7 
#1494 4125 5653 3162 1356 3783 5986 

with(sofr.cal,table(xsexr,useNA="ifany"))
#xsexr
#    1     2 
#21007  4552 

with(sofr.cal,table(xreth4r,useNA="ifany"))
#xreth4r
#    1     2 
#16833  8726 

with(sofr.cal,table(sred,useNA="ifany"))
#sred
#   1    2    3    4    5    6    7 <NA> 
# 146 2059 2465 4967 2399 7750 4912  861 

with(sofr.cal,table(srmarst,useNA="ifany"))
#srmarst
#    1     2     3     4     5  <NA> 
#16934   397  2538    75  5577    38 

with(sofr.cal,table(xact2r,useNA="ifany"))
#xact2r
#    1     2     3  <NA> 
#  611 12912 11814   222 

sofr.cal.dsgn  <-  svydesign(ids = ~0, # no clusters
                       strata = ~v.strat, 
                       data = data.frame(sofr.cal),
                       weights = ~d2)

### need to delete some categories
### svc 6, pg 7, act 3, educ 7, sex 2, reth 2, marst 5
### need to force pop tots to be same across all control vars

pop.tots <- c(N,
              svc[-1,2],
              pay[-1,2],
              activated[-1,2],
              educ[-1,2],
              gender[-1,2],
              raceth[-1,2],
              marital[-1,2],
              svc.pay[-c(1:8,15,22,29,36),3],
              svc.act[-c(1:4,7,10,13,16),3])
length(pop.tots)              

        # check how design matrix is formed in calibrate
mm <- model.matrix(~ as.factor(xsrrcr) * as.factor(xcpay1r) 
                    + as.factor(xsrrcr) * as.factor(xact2r)
                    + as.factor(sred)
                    + as.factor(xsexr) 
                    + as.factor(xreth4r)
                    + as.factor(srmarst),
                    data = sofr.cal)
dimnames(mm)[[2]]

sam.lin.ub  <-  calibrate(design = sofr.cal.dsgn,
                    formula = ~  as.factor(xsrrcr) * as.factor(xcpay1r) 
                    + as.factor(xsrrcr) * as.factor(xact2r)
                    + as.factor(sred)
                    + as.factor(xsexr) 
                    + as.factor(xreth4r)+
                    + as.factor(srmarst),
                    population = pop.tots,
                    bounds = c(-Inf,Inf),
                    calfun = c("linear")
)
        # main effects only
pop.tots <- c(N,
              svc[-1,2],
              pay[-1,2],
#              activated[-1,2],
#              educ[-1,2],
              gender[-1,2],
              raceth[-1,2],
              marital[-1,2])
length(pop.tots) 

sam.lin.ub  <-  calibrate(design = sofr.cal.dsgn,
                    formula = ~  as.factor(xsrrcr) 
                    + as.factor(xcpay1r) 
#                    + as.factor(xact2r)
#                    + as.factor(sred)
                    + as.factor(xsexr) 
                    + as.factor(xreth4r)+
                    + as.factor(srmarst),
                    population = pop.tots,
                    bounds = c(-Inf,Inf),
                    calfun = c("linear")
)
svytable(~xsrrcr, design=sam.lin.ub)


tmp <- model.matrix(~  as.factor(xsrrcr) * as.factor(xcpay1r) 
                    + as.factor(xsrrcr) * as.factor(xact2r)
                    + as.factor(sred)
                    + as.factor(xsexr) 
                    + as.factor(xreth4r)+
                    + as.factor(srmarst),
                    data=sofr.cal)


#Warning message:
#In regcalibrate.survey.design2(design, formula, population, aggregate.stage = aggregate.stage,  :
#  Sample and population totals have different names.

# Check calibrated survey totals

svytotal(~as.factor(xsrrcr), sam.lin.ub)

#                    total        SE
#as.factor(xsrrcr)1 322344 3.631e-12
#as.factor(xsrrcr)2 190235 1.916e-12
#as.factor(xsrrcr)3  77022 1.437e-12
#as.factor(xsrrcr)4  36094 4.586e-13
#as.factor(xsrrcr)5 105092 1.707e-12
#as.factor(xsrrcr)6  71022 1.321e-12


svytotal(~as.factor(xsexr), sam.lin.ub)

#                   total        SE
#as.factor(xsexr)1 663235 2.236e-12
#as.factor(xsexr)2 138574 1.879e-12

svytotal(~as.factor(xcpay1r), sam.lin.ub)

#                     total        SE
#as.factor(xcpay1r)1 112244 2.596e-12
#as.factor(xcpay1r)2 197930 1.504e-12
#as.factor(xcpay1r)3 265506 1.767e-12
#as.factor(xcpay1r)4 110397 1.089e-12
#as.factor(xcpay1r)5  10948 7.474e-13
#as.factor(xcpay1r)6  41176 7.609e-13
#as.factor(xcpay1r)7  63608 7.659e-13

svytotal(~as.factor(xreth4r), sam.lin.ub)

#                    total        SE
#as.factor(xreth4r)1 541075 1.898e-12
#as.factor(xreth4r)2 260734 1.655e-12

svytotal(~as.factor(sred), sam.lin.ub)

#                  total        SE
#as.factor(sred)1  10819 3.166e-12
#as.factor(sred)2 116933 1.684e-12
#as.factor(sred)3 113512 1.598e-12
#as.factor(sred)4 250408 1.933e-12
#as.factor(sred)5  96073 1.196e-12
#as.factor(sred)6 147450 6.963e-13
#as.factor(sred)7  66614 7.061e-13

svytotal(~as.factor(srmarst), sam.lin.ub)

#                     total        SE
#as.factor(srmarst)1 457244 2.356e-12
#as.factor(srmarst)2  11748 6.957e-13
#as.factor(srmarst)3  75025 5.763e-13
#as.factor(srmarst)4   3324 8.245e-14
#as.factor(srmarst)5 254468 1.230e-12

svytotal(~as.factor(xact2r), sam.lin.ub)

#                    total        SE
#as.factor(xact2r)1  37171 1.904e-12
#as.factor(xact2r)2 250808 8.064e-13
#as.factor(xact2r)3 513830 1.670e-12

summary(weights(sam.lin.ub))

#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#  0.1593   3.5390  10.5700  27.0400  25.9500 616.0000 

quantile(weights(sam.lin.ub), probs = seq(0,1,0.01))

#         0%          1%          2%          3%          4%          5%          6%          7%          8%          9%         10%         11%         12%         13% 
#  0.1593114   0.5997335   0.6941717   0.9483232   1.1178114   1.2423580   1.4415349   1.6337220   1.6653922   1.7716356   1.7986160   1.9026364   1.9585352   2.0497745 
#        14%         15%         16%         17%         18%         19%         20%         21%         22%         23%         24%         25%         26%         27% 
#  2.1023899   2.2197300   2.3300904   2.4266603   2.5517990   2.6972226   2.8076787   2.9045336   3.0617810   3.1867087   3.4073697   3.5386422   3.6689008   3.7811289 
#        28%         29%         30%         31%         32%         33%         34%         35%         36%         37%         38%         39%         40%         41% 
#  3.9288601   4.0622375   4.1967651   4.3904828   4.6514561   4.8692391   5.1150556   5.3921471   5.6826806   5.9141691   6.1028166   6.3856371   6.7085007   7.0058733 
#        42%         43%         44%         45%         46%         47%         48%         49%         50%         51%         52%         53%         54%         55% 
#  7.3410571   7.7071714   8.0447054   8.3849945   8.8050661   9.2348699   9.5845880  10.1408830  10.5680982  10.7940511  11.3321844  11.6561612  11.9267811  12.4558182 
#        56%         57%         58%         59%         60%         61%         62%         63%         64%         65%         66%         67%         68%         69% 
# 12.9830749  13.2796831  13.6996622  14.3378454  14.6287558  15.0237208  15.7437735  16.3194194  17.1345442  17.7264524  18.2931623  19.1682213  19.7456392  20.3712987 
#        70%         71%         72%         73%         74%         75%         76%         77%         78%         79%         80%         81%         82%         83% 
# 21.2882220  21.9214115  22.8847398  23.4173156  24.5693498  25.9472886  27.4654623  28.7041929  30.9343176  33.1434231  34.9720853  37.2134878  39.7746575  42.2011524 
#        84%         85%         86%         87%         88%         89%         90%         91%         92%         93%         94%         95%         96%         97% 
# 44.9164616  47.6310793  50.8005996  53.5207449  56.0594412  59.2332610  62.9759405  68.1543296  73.0825529  78.8300767  83.8336736  90.6223556 106.7902185 144.3425671 
#        98%         99%        100% 
#189.8061805 358.0494775 615.9675207 

summary(sofr.rie.NR$d.2)

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.021   4.277  12.470  29.350  30.840 514.900 

summary(sofr.rie.NR$d1)

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.021   2.247   6.109  12.960  14.630 182.000 


summary(sofr.rie.NR$d1)

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.000   2.201   5.984  12.690  14.330 178.300 


# Define d.3 in the data file

sofr.rie.NR$d.3 <- weights(sam.lin.ub)

# Boxplots for weights (not shown in the report)

plot(sofr.rie.NR$d1,
     weights(sam.lin.ub),
     xlab="Base weights",
     ylab="Adjusted weights",
     ylim=c(0,650),
     xlim=c(0,250),
     main="Adjusted weights vs. Base weights",
     pch=1,
     col="blue",
     lwd=2
    )
title (sub="Black line represents a 45 degree line")
abline(0,1)

points(sofr.rie.NR$d1,
     sofr.rie.NR$d.2,
     pch=4,
     col="red",
     lwd=2
    )

legend("bottomright",
       legend=c("Calibrated","Nonresponse Adjusted"), 
       col = c("blue","red"),
       lwd=2, 
       pch=c(1,4),
       lty=0)

# Calibrated vs. Nonresponse Adjusted Weights
# Calibration adds more variation to the weights

plot(sofr.rie.NR$d.2,
     weights(sam.lin.ub),
     xlab="Nonresponse adjusted weights",
     ylab="Calibrated weights",
     #ylim=c(-2,37),
     main="Adjusted weights vs. Base weights",
     pch=1,
     col="blue",
     lwd=2
    )

title (sub="Black line represents a 45 degree line")
abline(0,1)


op <-  par(mfrow = c(1,4))

boxplot(sofr.rie.NR$d1,
        col = "peach puff",
        xlab = "Base weights",
        ylim=c(0,650))

boxplot(sofr.rie.NR$d1,
        col = "peach puff",
        xlab = "Unknown eligibility adjusted base weights",
        ylim=c(0,650))

boxplot(sofr.rie.NR$d.2,
        col = "peach puff",
        xlab = "Nonresponse adjusted weights",
        ylim=c(0,650))

boxplot(sofr.rie.NR$d.3,
        col = "peach puff",
        xlab = "Calibrated weights",
        ylim=c(0,650))
        
par(op)
        
# DEFF component - Kish
kish.deff  <-  nrow(sofr.rie.NR)*sum(sofr.rie.NR$d.3^2) / (sum(sofr.rie.NR$d.3)^2)
kish.deff


# Weight Trimming

# Quadratic programming
# Couldn't use it due to memory size problems

# Set up constraints for calibration to pop totals

X  <-  model.matrix(~ as.factor(xsrrcr) + as.factor(xsexr) + 
                    as.factor(xcpay1r) + as.factor(xreth4r)+
                    as.factor(sred) + as.factor(srmarst) + 
                    as.factor(xact2r), data = sofr.rie.NR)

           
c0a  <-  pop.tots

(n <- nrow(sofr.rie.NR))
    
# Set up constraints for lower and upper weight bounds
In  <-  diag(nrow = n)
L  <-  1 # Lower bound on wts
U  <-  90.6223556 # Upper bound on wts (95% quantile)
one  <-  rep(1, n) # RAN INTO MEMORY.SIZE PROBLEMS, >3.3 GB
c0b  <-  c( L * one,
         -U * one)
Cmat  <-  rbind(X, In, -In)
wts  <-  solve.QP(Dmat = diag(1/d),
                dvec = 2 * one,
                Amat = t(Cmat),
                bvec = c(c0a, c0b),
                meq = 27 # first 26 constraints are equality constraints
                )

quad.dsgn  <-  svydesign(ids = ~0, # no clusters
                       strata = NULL, # no strata
                       data = data.frame(sofr.rie.NR),
                       weights = ~wts$solution)
                       
range(weights(quad.dsgn))

sum(weights(quad.dsgn))

# Weight Trimming and Redistribution
# Set the weights to 90.6 (95% quantile) when they exceed 95% quantile
# This step increases bias but reduces variation in weights as a result variance estimates of the estimates

trim  <-  function(maxiter=NULL, show.iter=FALSE,
tol=0.001){
wt  <-  sofr.rie.NR$d.3
wt[sofr.rie.NR$d.3>90.6] <- 90.6
wt[sofr.rie.NR$d.3<1] <- 1
meet  <-  FALSE
step  <-  0
while(!meet & (step <= maxiter)) {
wt[wt>90.6] <- 90.6
wt[wt<90.6]  <-  wt[wt<90.6]*(sum(sofr.rie.NR$d.3)-sum(wt[wt==90.6]))/sum(wt[wt<90.6])
if(max(wt) <= 90.6) {meet  <-  TRUE}
step  <-  step+1
}
if ((step >= maxiter) & !meet){
cat("Maximum no. of iterations reached without
convergence.\n")
}
else{
cat("Convergence attained in ", step-1, "steps.\n")
wt
}
}

sofr.rie.NR$d.4  <-  trim(maxiter=100, show.iter=FALSE,tol=0.001)

kish.deff  <-  nrow(sofr.rie.NR)*sum(sofr.rie.NR$d.4^2) / (sum(sofr.rie.NR$d.4)^2)
kish.deff

# Trimmed vs. Calibrated Weights

plot(sofr.rie.NR$d.3,
     sofr.rie.NR$d.4,
     xlab="Calibrated weights",
     ylab="Trimmed weights",
     #ylim=c(-2,37),
     pch=1,
     col="blue",
     lwd=2
    )

title (sub="Black line represents a 45 degree line")
abline(0,1)

# Trimmed weights will not satisfy the calibrated survey totals.

# keep respondents only
sofr.rie.NR  <-  sofr.rie.NR[which(sofr.rie.NR$disp==1),] 

#create final dataset
names(sofr.rie.NR)
# [1] "nr.class" "rec.id"   "respstat" "srmarst"  "ra006a"   "ra006b"   "ra008"    "ra115"    "ra118"    "sred"     "ra112ra"  "xsrrcr"   "xact2r"  
#[14] "xreth4r"  "xsexr"    "xcpay1r"  "nsamp"    "nstrat"   "v.strat"  "stratum"  "d0"      "f0"      "disp"     "d.1"      "unwt.rr"  "wt.rr"   
#[27] "a2"       "d.2"      "d.3"      "d.4"   
sofr.rie.NR$nr.class  <-  NULL
sofr.rie.NR$disp  <-  NULL
sofr.rie.NR$unwt.rr  <-  NULL
sofr.rie.NR$wt.rr  <-  NULL
sofr.rie.NR$a2  <-  NULL
data.for.users  <-  sofr.rie.NR

names(data.for.users)
# [1] "nr.class" "rec.id"   "respstat" "srmarst"  "ra006a"   "ra006b"   "ra008"    "ra115"    "ra118"    "sred"     "ra112ra"  "xsrrcr"   "xact2r"  
#[14] "xreth4r"  "xsexr"    "xcpay1r"  "nsamp"    "nstrat"   "v.strat"  "stratum"  "d0"      "f0"      "d.1"      "d.2"      "d.3"      "d.4"   
nrow(data.for.users)
#[1] 25559

write.foreign(data.for.users, "data.txt", "project2.sas", package = "SAS")
