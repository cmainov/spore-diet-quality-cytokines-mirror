###------------------------------------------------------
###   07-FIT PROPORTIONAL ODDS/LOGIT MODELS ON QUARTILES
###------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------------------------------------
# 
# In this script, we will redo the analysis from 05-prop-odds-logit-models.R but use tertile versions of the diet quality
# index variables as a sensitivity analysis
# 
# INPUT DATA FILE: "03-Data-Rodeo/04-analytic-data.rds"
#
# OUTPUT FILES: "04-Figures-Tables/table-s2.txt", 
#
# Resources: https://acsjournals.onlinelibrary.wiley.com/doi/pdfdirect/10.1002/cncr.28778 
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

library( tidyverse )
library( VGAM )  # for proportional odds model-fitting

# read helper functions into global environment (table-generating helper functions reside in this file)
source( "R/utils.R" )

### Read-in Data ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
( d <- readRDS( "03-Data-Rodeo/04-analytic-data.rds" ) ) %>%
  summarize( n = n( ), unique = length( unique( idnum ) ), ncol = ncol(.) ) 
# 146 obs. and 123 cols.
# ---------------------------------------------------------------------------------------------------------------------------------------------------------



### Loop Prep ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

## vectors loop will pull from ##
outcome.variables <- c( "ifn.cat", "il6.cat", "tnf.bin", "il10.cat", "il17.bin", "il8.cat", "tgf.cat",
                        "gro.cat", "vegf.cat", "hgf.cat" )  # outcome variables we will use

covariates <- c( "age","sex", "tum.site", "smoker", "drinker", "calor", "stage.binary" )  # covariates we will adjust for

cov.collapse <- paste0( covariates, collapse = " + ") # collapse covariates into a single string for model formula

i.names <- c( "ahei", "amed", "dash", "low.carb", "a.low.carb", "v.low.carb" )

indices <- paste0( i.names, ".q3" ) # diet quality indices

t.names <- paste0( indices, ".trend" ) # trend variables for trend analysis

# ---------------------------------------------------------------------------------------------------------------------------------------------------------



### Loop ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
index.res.list <- list() # list to store results and then rowbind from
for( i in 1: length( indices ) ){   # outer loop will index on the diet indices
  
  out.var.list <- list() # list to hold results of inner loop (resets after each iteration)
  for( j in 1:length( outcome.variables ) ) {    # inner loop will index on the response variables
    
    # length of vector of levels of response variable to assess which model to use:
    outcome.var.length <- length( levels( factor( dat[[ outcome.variables[j] ]] ) ) )
    
    
    ## if length of outcome factor levels exceed 2 use prop.odds model ##
    if(  outcome.var.length > 2 ){ 
      
      # fit prop. odds model with `vglm` from the `VGAM` package
      mod1 <- vglm( formula( paste0( "factor( ", outcome.variables[j],", ordered = T ) ~ ",
                                     paste0( indices[i], " + " ),
                                     cov.collapse ) ),
                    family = propodds( reverse = FALSE ), data = dat )
      
      sum.mod1 <- data.frame( coef( summary( mod1 ) ) ) # store summary as data.frame so we can access p.values for labeling significant results
      
      # fit prop. odds model for test for linear trend
      mod2 <- vglm( formula( paste0( "factor( ", outcome.variables[j],", ordered = T ) ~ ",
                                     paste0( t.names[i], " + " ),
                                     cov.collapse ) ),
                    family = propodds( reverse = FALSE ), data = dat )
      
      sum.mod2 <- data.frame( coef( summary( mod2 ) ) ) # store summary as data.frame so we can access p.values for labeling significant results
      
      # fit logit model for with continuous variable scaled by standard deviation of the index
      mod3 <- vglm( formula( paste0( "factor( ", outcome.variables[j],", ordered = T ) ~ ",
                                     paste0( "I( ", i.names[i], "/sd( ", i.names[i], ", na.rm = T ) ) + " ),
                                     cov.collapse ) ),
                    family = propodds( reverse = FALSE ), data = dat )
      
      sum.mod3 <- data.frame( coef( summary( mod3 ) ) ) # store summary as data.frame so we can access p.values for labeling significant results
      
      # fit logit model for with quadratic transformation
      mod4 <- vglm( formula( paste0( "factor( ", outcome.variables[j],", ordered = T ) ~ ",
                                     paste0( i.names[i], " + I( ",
                                             i.names[i], "^2 ) + "),
                                     cov.collapse ) ),
                    family = propodds( reverse = FALSE ), data = dat )
      
      sum.mod4 <- data.frame( coef( summary( mod4 ) ) ) # store summary as data.frame so we can access p.values for labeling significant results
      
    }
    
    
    ## if length of outcome factor levels = 2 use logit model ##
    else if(  outcome.var.length == 2 ){ 
      
      # fit logit model using `glm` and specifying link function
      mod1 <- glm( formula( paste0( "factor( ", outcome.variables[j],") ~ ",
                                    paste0( indices[i], " + " ),
                                    cov.collapse ) ),
                   family = binomial( link = logit ), data = dat )
      
      sum.mod1 <- data.frame( coef( summary( mod1 ) ) ) # store summary as data.frame so we can access p.values for labeling significant results
      
      # fit logit model for test for linear trend
      mod2 <- glm( formula( paste0( "factor( ", outcome.variables[j],") ~ ",
                                    paste0( t.names[i], " + " ),
                                    cov.collapse ) ),
                   family = binomial( link = logit ), data = dat )
      
      sum.mod2 <- data.frame( coef( summary( mod2 ) ) ) # store summary as data.frame so we can access p.values for labeling significant results
      
      # fit logit model for with continuous variable scaled by standard deviation of the index
      mod3 <- glm( formula( paste0( "factor( ", outcome.variables[j],") ~ ",
                                    paste0( "I( ", i.names[i], "/sd( ", i.names[i], ", na.rm = T ) ) + " ),
                                    cov.collapse ) ),
                   family = binomial( link = logit ), data = dat )
      
      sum.mod3 <- data.frame( coef( summary( mod3 ) ) ) # store summary as data.frame so we can access p.values for labeling significant results
      
      # fit logit model for with quadratic transformation
      mod4 <- glm( formula( paste0( "factor( ", outcome.variables[j],") ~ ",
                                    paste0( i.names[i], " + I( ",
                                            i.names[i], "^2 ) + "),
                                    cov.collapse ) ),
                   family = binomial( link = logit ), data = dat )
      
      sum.mod4 <- data.frame( coef( summary( mod4 ) ) ) # store summary as data.frame so we can access p.values for labeling significant results
      
    }
    
    
    ## put results into tabular format for presentation and store in the list ##
    
    # extract row indices of rows from the results table we are interested in (i.e., Q2 and Q3 of the indices and trend variable)
    q2.r <- which( str_detect(rownames( sum.mod1 ), paste0( indices[i], "2" ) ) )
    q3.r <- which( str_detect(rownames( sum.mod1 ), paste0( indices[i], "3" ) ) )
    trend.r <- which( str_detect(rownames( sum.mod2 ), t.names[i] ) )
    cont.r <- which( str_detect(rownames( sum.mod3 ), i.names[i] ) )
    quad.r <- which( str_detect(rownames( sum.mod4 ), paste0( i.names[i],"\\^2" ) ) )
    
    # extract p values for each of Q2 and Q3 so we can label significant results in the final table
    q2.p <- sum.mod1[ q2.r, 4 ]  # column 4 is p-value column
    q3.p <- sum.mod1[ q3.r, 4 ]
    
    # extract p values for trend test, continuous variable model, and quadratic model so we can label significant results in the final table
    t.p <- sum.mod2[ trend.r, 4 ]
    c.p <- sum.mod3[ cont.r, 4 ]
    q.p <- sum.mod4[ quad.r, 4 ]
    
    
    # assemble results table
    out.var.list[[j]] <- tibble( outcome.variable = outcome.variables[j],
                                 index = indices[i],
                                 
                                 odds.ratio.q1 = "1.00",   # Q1 is the referent
                                 
                                 # elements for Q2 results
                                 odds.ratio.q2 = round( exp( coef( mod1 )[ q2.r ] ), digits = 2 ),
                                 conf.int.q2 = paste0( '(', round( exp( confint( mod1 ) )[ q2.r, 1 ], digits = 2 ), ', ',
                                                       round( exp( confint( mod1 ) )[ q2.r, 2 ], digits = 2 ), ')' ),
                                 # elements for Q3 results
                                 odds.ratio.q3 = round( exp( coef( mod1 )[ q3.r ] ), digits = 2 ),
                                 
                                 conf.int.q3 = paste0( '(', round( exp( confint( mod1 ) )[ q3.r, 1 ], digits = 2 ), ', ',
                                                       round( exp( confint( mod1 ) )[ q3.r, 2 ], digits = 2 ), ')' ),
                                 
                                 # p values for trend, quadratic model, and results from running model with continuous variable
                                 trend.p = paste0( round( t.p, digits = 2 ) ),
                                 odds.ratio.cont = round( exp( coef( mod3 )[ cont.r ] ), digits = 2 ),
                                 conf.int.cont = paste0( '(', round( exp( confint( mod3 ) )[ cont.r, 1 ], digits = 2 ), ', ',
                                                         round( exp( confint( mod3 ) )[ cont.r, 2 ], digits = 2 ), ')' ),
                                 quad.p = paste0( round( q.p, digits = 2 ) ) ) %>%
      
      # label significant results (p-values and confidence intervals) with * or **
      mutate( conf.int.q2 = ifelse( q2.p < 0.05 & q2.p >= 0.01, paste0( conf.int.q2, "*"),
                                    ifelse( q2.p < 0.01, paste0( conf.int.q3, "**" ), conf.int.q2 ) ),
              conf.int.q3 = ifelse( q3.p < 0.05 & q3.p >= 0.01, paste0( conf.int.q3, "*"),
                                    ifelse( q3.p < 0.01, paste0( conf.int.q3, "**" ), conf.int.q3 ) ),
              conf.int.cont = ifelse( c.p < 0.05 & c.p >= 0.01, paste0( conf.int.cont, "*"),
                                      ifelse( c.p < 0.01, paste0( conf.int.cont, "**" ), conf.int.cont ) ),
              trend.p = ifelse( t.p < 0.05 & t.p >= 0.01, paste0( trend.p, "*"),
                                ifelse( t.p < 0.01, paste0( "< 0.01**" ), trend.p ) ),
              quad.p = ifelse( q.p < 0.05 & q.p >= 0.01, paste0( quad.p, "*"),
                               ifelse( q.p < 0.01, paste0( "< 0.01**" ), quad.p ) ) )
    
    
    
  }
  
  index.res.list[[i]] <- do.call( "rbind", out.var.list )
  
}


## rowbind table and collapse OR and CI columns into single columns ##
t.4 <- do.call( "rbind", index.res.list ) %>%
  unite( q2, contains( "q2" ), sep = " " ) %>%
  unite( q3, contains( "q3" ), sep = " " ) %>%
  unite( cont, contains( "cont" ), sep = " " )

## tidy up significant digits ##

t.4 <- data.frame( t.4 ) # convert to dataframe first

# odds ratios and confidence intervals
for( i in c( 4, 5, 7 ) ) {
  t.4[,i] <- str_replace( t.4[,i], '(\\(\\d)\\,', "\\1\\.00," ) # match open parenthesis followed by a digit and then a comma. Retain everything except the comma and add ".00,"
  t.4[,i] <- str_replace( t.4[,i], "(\\(\\d\\.\\d)\\,","\\10\\,") # match open parenthesis followed by a digit, period, digit, and then a comma. Retain everything except the comma and add ".0,"
  t.4[,i] <- str_replace( t.4[,i], "(\\(\\d\\.\\d)\\,", "\\10\\," ) # match open parenthesis followed by digit, period, digit and comma.Retain everything except the comma and add "0,"
  t.4[,i] <- str_replace( t.4[,i], "(\\d\\.\\d)\\s", "\\10 " ) # match digit, period, digit and space. Retain everything except space and add "0 "
  t.4[,i] <- str_replace( t.4[,i], "^(\\d)\\s\\(", "\\1\\.00 \\(" ) # match beginning of string followed by single digit, followed by a space and parenthesis. Reatin the single digit and and ".00 ()
  t.4[,i] <- str_replace( t.4[,i], "(\\d\\.\\d)\\)", "\\10\\)") # match digit, period, digit, close parenthesis. Retain everything except parenthesis and add "0)" to end
  
}

for( i in c( 6,8 ) ){
  t.4[,i] <- str_replace( t.4[,i], "(\\d\\.\\d)$", "\\10" ) # match digit, period, digit,end and add a 0 before the end
  t.4[,i] <- str_replace( t.4[,i], "^1$", "0.99" ) # round down probabilities = 1
}

# ---------------------------------------------------------------------------------------------------------------------------------------------------------


### Save ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
write.table( t.4, "04-Figures-Tables/table-s2.txt")
# ---------------------------------------------------------------------------------------------------------------------------------------------------------




