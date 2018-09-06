#****************************************************************************************
# FILE:     16.1 Solution bwt-unknown adj.R.                                             
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples                   
# DATE:     12/31/2010                                                                   
# AUTHOR:   R. Valliant                                                                  
# REVISED:  
#****************************************************************************************

# Define a working directory
file_loc <- "C:\\Projects\\Practical Tools Book\\Book Chapters\\13 Project Weighting\\Data files\\"
file_loc_out <- "C:\\Projects\\Practical Tools Book\\Book Chapters\\18 Solution Weighting\\"


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
sofr$d1[sofr$disp==4] <- NA
sofr$a1 <- a1
sofr$a1[sofr$disp==4] <- NA
table(sofr$a1, useNA="always")
#1.02261813971506             <NA> 
#           70251             1450 


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

sofr.d1 <- sofr
dim(sofr.d1)
#[1] 71701    24    This file includes all original sample cases

save(sofr.d1, file=paste(file_loc_out,"sofr.d1.RData", sep=""))
