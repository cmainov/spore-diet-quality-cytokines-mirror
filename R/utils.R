###---------------------------------------------------
###   RESULT-GENERATING AND OTHER HELPER FUNCTIONS
###---------------------------------------------------

#####################################
########## %notin% operator #########
#####################################
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
`%notin%` <- Negate( `%in%` )
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

#####################################
###### list to rbind function #######
#####################################
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
list_it <- function( list ) {
  do.call( "rbind" , list )
}
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

####################################################################################################
#################################### Quantile Cutting Function #####################################
####################################################################################################

# ---------------------------------------------------------------------------------------------------------------------------------------------------------

quant_cut<-function(var,x,df){
  
  xvec<-vector() # initialize null vector to store
  
  for (i in 1:x){
    xvec[i]<-i/x
  }
  
  qs<-c(min(df[[var]],na.rm=T), quantile(df[[var]],xvec,na.rm=T))
  
  df[['new']]=x+1 # initialize variable
  
  for (i in 1:(x)){
    df[['new']]<-ifelse(df[[var]]<qs[i+1] & df[[var]]>=qs[i],
                        c(1:length(qs))[i],
                        ifelse(df[[var]]==qs[qs==max(qs)],x,df[['new']]))
  }
  
  return(df[['new']])
}
# ---------------------------------------------------------------------------------------------------------------------------------------------------------






####################################################################################################
#################################### Quantiles for Patterns #######################################
####################################################################################################

### Bin Observations into Diet Pattern Quantiles (Binary Tertiles, Quartiles, and Quintiles)  ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


bin_patterns <- function( df, pattern.cols ) {
  
  
  # bin into quintiles
  i.names <- pattern.cols
  
  # binary bin variable
  i.names.q2 <- paste0( i.names, ".q2" )
  # tertile variable names
  i.names.q3 <- paste0( i.names, ".q3" )
  # quartile variable names
  i.names.q4 <- paste0( i.names, ".q4" )
  # quintile variable names
  i.names.q5 <- paste0( i.names, ".q5" )
  
  # for-loop to do the work
  for ( i in 1:length( i.names ) ){
    
    # quantiles will only be computed for those included in the analysis (i.e., cc == 1)
    
    # dichotomize
    df[ , i.names.q2[i] ] <- quant_cut( var = i.names[i], x = 2, df = df ) 
    
    # tertiles
    df[ , i.names.q3[i] ] <- quant_cut( var = i.names[i], x = 3, df = df ) 
    
    # quartiles
    df[ , i.names.q4[i] ] <- quant_cut( var = i.names[i], x = 4, df = df ) 
    
    # quintiles
    df[ , i.names.q5[i] ] <- quant_cut( var = i.names[i], x = 5, df = df ) 
    
    
  }
  
  
  # convert quantile columns to class factor
  
  these <- which( colnames( df ) %in% c( paste0( i.names, ".q2" ),
                                         paste0( i.names, ".q3" ), 
                                          paste0( i.names, ".q4" ),
                                          paste0( i.names, ".q5" ) ) )
  
  for( i in these ){
    
    df[,i] <- as.factor( df[[ i ]] )
  }
  
  return( df )
}

# ---------------------------------------------------------------------------------------------------------------------------------------------------------


####################################################################################################
#################################### Trend Variable Function #######################################
####################################################################################################

# ---------------------------------------------------------------------------------------------------------------------------------------------------------

trend_func<-function(rank.var,cont.var,df,trend.var,x){
  
  df[[trend.var]] = 1
  
  medians<-vector()
  
  for (i in 1:x){
    
    newdf<-df[df[[rank.var]]==i,]
    
    medians[i]<-median(newdf[[cont.var]],na.rm=T)
    
    df[[trend.var]]<-ifelse(df[[rank.var]]==i,medians[i],df[[trend.var]])
    
  }
  
  return( df )
}
# ---------------------------------------------------------------------------------------------------------------------------------------------------------



####################################################################################################
################################## Trend Variables for Patterns ####################################
####################################################################################################

## generate trend variables for patterns in analyses (for Binary Indicator, Tertiles, Quartiles, and Quintiles)  ##
trend_patterns<- function( df, pattern.cols ){
  # bin into quintiles
  i.names <- pattern.cols
  
  # binary bin variable
  i.names.q2 <- paste0( i.names, ".q2" )
  # tertile variable names
  i.names.q3 <- paste0( i.names, ".q3" )
  # quartile variable names
  i.names.q4 <- paste0( i.names, ".q4" )
  # quintile variable names
  i.names.q5 <- paste0( i.names, ".q5" )
  
  
  # binary bin variable
  t.names.q2 <- paste0( i.names, ".q2.trend" )
  # tertile variable names
  t.names.q3 <- paste0( i.names, ".q3.trend" )
  # quartile variable names
  t.names.q4 <- paste0( i.names, ".q4.trend" )
  # quintile variable names
  t.names.q5 <- paste0( i.names, ".q5.trend" )
  
  # for-loop to do the work
  for ( i in 1:length( i.names ) ){
    
    # quantiles will only be computed for those included in the analysis (i.e., cc == 1)
    
    # dichotomize
    df <- trend_func( rank.var = i.names.q2[i],
                      cont.var = i.names[i], 
                      df = df, 
                      trend.var = t.names.q2[i], 
                      x = 2 )
    
    # tertiles
    df <- trend_func( rank.var = i.names.q3[i],
                      cont.var = i.names[i], 
                      df = df, 
                      trend.var = t.names.q3[i], 
                      x = 3 )  
    # quartiles
    df <- trend_func( rank.var = i.names.q4[i],
                      cont.var = i.names[i], 
                      df = df, 
                      trend.var = t.names.q4[i], 
                      x = 4 )  
    # quintiles
    df <- trend_func( rank.var = i.names.q5[i],
                      cont.var = i.names[i], 
                      df = df, 
                      trend.var = t.names.q5[i], 
                      x = 5 )  
  }
  
  
  
  return( df )
  
}
# ---------------------------------------------------------------------------------------------------------------------------------------------------------



####################################################################################################
################################## Table 1 (Continuous Variables) ##################################
####################################################################################################

# ---------------------------------------------------------------------------------------------------------------------------------------------------------

tab1.var.mean<-function(var.name,df,table.var.name,strata.var=NULL,strata.level=NULL){
  
  if(is.null(strata.var)==T){
    df<- data.frame( df )
  }
  else {
    df<-data.frame( df[df[[strata.var]]==strata.level,] )
  }
  
  
  rowvar.name<-c(table.var.name)
  rowvar.mean<-c(paste0(round(mean(df[[var.name]],na.rm=T),digits=1),' (',round(sd(df[[var.name]], na.rm = T),digits=1),')'))
  
  partial.table<-data.frame(cbind(rowvar.name,rowvar.mean))
  colnames(partial.table)<-c('Characteristic','Frequency (%) or Mean (SD)')
  return(partial.table)
  
}
# ---------------------------------------------------------------------------------------------------------------------------------------------------------




####################################################################################################
######################### Residual Method for Total Calorie Adjustment #############################
####################################################################################################

# ---------------------------------------------------------------------------------------------------------------------------------------------------------


# Helper function #
residual_bind <- function( exp, dat.f, cal ) {
  
  dat.f <- data.frame( dat.f )
  mod <- lm( formula( paste0( exp," ~ ", cal ) ), data = dat.f )
  
  step.1.residuals <- mod$residuals
  
  
  ## add arbitrary constant value since residuals have mean 0 ##
  
  # will add the predicted value for the mean value of calories in the dataset
  step.2.residuals <- data.frame( step.1.residuals + ( mod$coefficients[1] + mod$coefficients[2]*mean( dat.f[[ cal ]], na.rm = T ) ) )
  colnames( step.2.residuals ) <- "x.adjusted"
  
  # create rowid column based on rownames of the model matrix (will be used for faithful merging)
  step.2.residuals <- step.2.residuals %>%
    mutate( rowid = rownames( mod$model ) )  # label residuals by their rowid
  
  # join residuals to original dataframe using rowid (this ensure missings are set to missing accordingly)
  dat.f <- dat.f %>%
    mutate( rowid = rownames( dat.f ) ) %>%
    left_join( step.2.residuals, by = "rowid" )
  
  return( dat.f %>% select( rowid, x.adjusted ) )
}

# Main function #
energy_residual <- function( dat, x, calories, overwrite = "no" ){
  # x is the nutrient/food group/ index score needing to be energy adjusted ( can be a single variable or 
  # a vector of character strings with each item representing a different column)
  # calories is a character vector (single column) indicating the column name for total calories
  # df is a dataframe:
  # overwrite is a "yes" or "no", depending on if use wants to overwrite previous versions of the columns and keep the
  # same column names. Default is "no". 
  # RETURN: this function returns a dataframe with the energy adjusted variables appended to the end of the
  # frame and named by pasting their original name with ".adj" at the end of the string or the original column names.
  
  ## checks ##
  
  if( overwrite %notin% c( "yes", "no" ) ) stop( '`overwrite` must be one of "yes" or "no"')
  
  ## regress diet index scores on total energy and extract residuals ##
  
  
  # loop and bind residuals for all "x" variables
  dat.list<- list()
  for( i in 1: length( indices) ) {
    
    h <- residual_bind( exp = x[i], dat.f = dat, cal = calories ) 
    
    if( overwrite == "no" ){
      
      dat.list[[i]] <- h %>%
        rename( !!paste0( x[i], ".adj" ) := x.adjusted ) # rename column according to input variable names
    }
    
    else if( overwrite == "yes" ){
      
      dat.list[[i]] <- h %>%
        rename( !!paste0( indices[i] ) := x.adjusted )
    }
    
  }
  
  ## bind columns and produce final data output ##
  
  if( overwrite == "no" ){
    dat <- dat %>% mutate( rowid = rownames( dat ) )  # create a rowid column for merge
  }
  
  else if( overwrite == "yes" ){
    dat <- dat %>% mutate( rowid = rownames( dat ) ) %>%  # create a rowid column for merge
      select (- indices ) # remove old old columns of the x variables if they are to be overwritten
  }
  
  out.dat <- dat.list %>% reduce( inner_join, by = "rowid" ) %>%  # inner_join all elements of the list
    data.frame() %>%
    left_join( dat, ., by = "rowid" ) %>%
    select( -rowid )
  
  return( out.dat )
  
}


# ---------------------------------------------------------------------------------------------------------------------------------------------------------



####################################################################################################
################################# Table 1 (Categorical Variables) ##################################
####################################################################################################

# ---------------------------------------------------------------------------------------------------------------------------------------------------------

tab1.var.freq<-function(var.name,df,table.var.name,strata.var=NULL,strata.level=NULL){ #var.name is quoted string of how
  #variable is stored in dfset, df=is the dfset stored in R environment
  #and table.var.name is a character string of how that section of table 1 should be titled
  #strata.var is the variable, quoted, to stratify on, and strata.level is a quoted string
  #containing the level of strata.var that is to be examined
  
  
  df <- data.frame( df )
  
  
  
  if (is.null(strata.var)==T){

    df2<-data.frame( df )
    
    df2[[ var.name ]] <- factor( df2[[var.name]] )
  }
  
  if (is.null(strata.var)==F) {
    df2<-data.frame( df[df[[strata.var]] %in% strata.level,] )
    
    df2[[ var.name ]] <- factor(df2[[ var.name ]], 
                                levels =c( levels( factor( df2[[var.name]] ) ), levels( factor( df[[var.name]] ) )[which( levels( factor( df[[var.name]] ) ) %notin% levels( factor( df2[[var.name]] ) ) ) ] ))
    
    }
  
  rowvar.name<-vector()
  levelvec<-levels(df2[[var.name]])[order(levels(df2[[var.name]]))]
  
  for (i in 1:length(levelvec)){
    rowvar.name[i]<-paste0(levelvec[i])
  }
  
  # add to variable name header
  rowvar.name <- c(table.var.name,rowvar.name)
  
  rowvar.freq<-vector()
  for (i in 1:length(levelvec)){
    rowvar.freq[i]<-paste0(table(df2[[var.name]])[levelvec[i]],' (',round(100*table(df2[[var.name]])[levelvec[i]]/sum(table(df2[[var.name]]), na.rm = T),digits=1),')')
  }
  
  rowvar.freq<-c('',rowvar.freq)
  rowvar.freq<-ifelse(rowvar.freq=='NA (NA)',paste0('0 (0.0)'),rowvar.freq)
  
  partial.table<-data.frame(cbind(rowvar.name,rowvar.freq))
  colnames(partial.table)<-c('Characteristic','Frequency (%) or Mean (SD)')
  return(partial.table)
  
  
}
# ---------------------------------------------------------------------------------------------------------------------------------------------------------






####################################################################################################
#################################### Model-Fitting and Results Function #######################################
####################################################################################################


fit_res <- function( dat, x, q.x, y, z, trend, seed.1 = 23, seed.2 = 765 ){
  # dat: a dataframe
  # x: a character or character vector with the names of the explanatory variables (continuous variables)
  # qx: a character or character vector with the names of the explanatory variables (categorical variables)
  # y: a character or character vector with the names of the response variables (categorical--binary or ordinal-- variables)
  # z: a character or character vector with the names of the covariates
  # z: a character or character vector with the names of the trend variable names for the trend tests
  # seed.1: set a seed needed for getting nudge values for the Manhattan plot 
  # seed.2: set a seed needed for getting nudge values for the Manhattan plot 
  
  
  
  cov.collapse <- paste0( z, collapse = " + ") # collapse covariates into a single string for model formula
  ### Loop ###
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  index.res.list <- list() # list to store results and then rowbind from
  raw.p <- list()
  obs.used <- vector()   # store no. of obs used in each model
  for( i in 1: length( q.x ) ){   # outer loop will index on the diet q.x
    
    fac.levels <- levels( factor( dat[[ q.x[i] ]] ) )
    fac.length <- length( fac.levels )
    
    out.var.list <- list() # list to hold results of inner loop (resets after each iteration)
    raw.p.vals <- list() # list to store raw p values of Q4 and trend test in inner loop
    for( j in 1:length( y ) ) {    # inner loop will index on the response variables
      
      # length of vector of levels of response variable to assess which model to use:
      outcome.var.length <- length( levels( factor( dat[[ y[j] ]] ) ) )
      
      
      ## if length of outcome factor levels exceed 2 use prop.odds model ##
      if(  outcome.var.length > 2 ){ 
        
        # fit prop. odds model with `vglm` from the `VGAM` package
        mod1 <- vglm( formula( paste0( "factor( ", y[j],", ordered = T ) ~ ",
                                       paste0( q.x[i], " + " ),
                                       cov.collapse ) ),
                      family = propodds( reverse = FALSE ), data = dat )
        
        sum.mod1 <- data.frame( coef( summary( mod1 ) ) ) # store summary as data.frame so we can access p.values for labeling significant results
        
        # fit prop. odds model for test for linear trend
        mod2 <- vglm( formula( paste0( "factor( ", y[j],", ordered = T ) ~ ",
                                       paste0( trend[i], " + " ),
                                       cov.collapse ) ),
                      family = propodds( reverse = FALSE ), data = dat )
        
        sum.mod2 <- data.frame( coef( summary( mod2 ) ) ) # store summary as data.frame so we can access p.values for labeling significant results
        
        # fit logit model for with continuous variable scaled by standard deviation of the index
        mod3 <- vglm( formula( paste0( "factor( ", y[j],", ordered = T ) ~ ",
                                       paste0( "I( ", x[i], "/sd( ", x[i], ", na.rm = T ) ) + " ),
                                       cov.collapse ) ),
                      family = propodds( reverse = FALSE ), data = dat )
        
        sum.mod3 <- data.frame( coef( summary( mod3 ) ) ) # store summary as data.frame so we can access p.values for labeling significant results
        
        # fit logit model for with quadratic transformation
        mod4 <- vglm( formula( paste0( "factor( ", y[j],", ordered = T ) ~ ",
                                       paste0( x[i], " + I( ",
                                               x[i], "^2 ) + "),
                                       cov.collapse ) ),
                      family = propodds( reverse = FALSE ), data = dat )
        
        sum.mod4 <- data.frame( coef( summary( mod4 ) ) ) # store summary as data.frame so we can access p.values for labeling significant results
        
      }
      
      
      ## if length of outcome factor levels = 2 use logit model ##
      else if(  outcome.var.length == 2 ){ 
        
        # fit logit model using `glm` and specifying link function
        mod1 <- glm( formula( paste0( "factor( ", y[j],") ~ ",
                                      paste0( q.x[i], " + " ),
                                      cov.collapse ) ),
                     family = binomial( link = logit ), data = dat )
        
        sum.mod1 <- data.frame( coef( summary( mod1 ) ) ) # store summary as data.frame so we can access p.values for labeling significant results
        
        obs.used <- nrow( model.matrix( mod1  ) )
        
        # fit logit model for test for linear trend
        mod2 <- glm( formula( paste0( "factor( ", y[j],") ~ ",
                                      paste0( trend[i], " + " ),
                                      cov.collapse ) ),
                     family = binomial( link = logit ), data = dat )
        
        sum.mod2 <- data.frame( coef( summary( mod2 ) ) ) # store summary as data.frame so we can access p.values for labeling significant results
        
        # fit logit model for with continuous variable scaled by standard deviation of the index
        mod3 <- glm( formula( paste0( "factor( ", y[j],") ~ ",
                                      paste0( "I( ", x[i], "/sd( ", x[i], ", na.rm = T ) ) + " ),
                                      cov.collapse ) ),
                     family = binomial( link = logit ), data = dat )
        
        sum.mod3 <- data.frame( coef( summary( mod3 ) ) ) # store summary as data.frame so we can access p.values for labeling significant results
        
        # fit logit model for with quadratic transformation
        mod4 <- glm( formula( paste0( "factor( ", y[j],") ~ ",
                                      paste0( x[i], " + I( ",
                                              x[i], "^2 ) + "),
                                      cov.collapse ) ),
                     family = binomial( link = logit ), data = dat )
        
        sum.mod4 <- data.frame( coef( summary( mod4 ) ) ) # store summary as data.frame so we can access p.values for labeling significant results
        
      }
      
      obs.used[i] <- nrow( model.matrix( mod1  ) )
      
      ## put results into tabular format for presentation and store in the list ##
      
      # extract row q.x of rows from the results table we are interested in (i.e., Q2, Q3, and Q4...etc of the q.x and trend variable)
      
      for (g in 2:fac.length ){  # start at 2 since we don't include first quantile
        
        assign( paste0("q", g, ".r"), which( str_detect(rownames( sum.mod1 ), paste0( q.x[i], fac.levels[g] ) ) ) )
        
      }
      
      trend.r <- which( str_detect(rownames( sum.mod2 ), trend[i] ) )
      cont.r <- which( str_detect(rownames( sum.mod3 ), x[i] ) )
      quad.r <- which( str_detect(rownames( sum.mod4 ), paste0( x[i],"\\^2" ) ) )
      
      
      # assign p values for each of the quantiles as objects
      for ( k in 2:fac.length ){
        
        r.i <- get( paste0("q", k, ".r" ) ) # row index
        
        assign( paste0( "q", k, ".p" ), sum.mod1[ r.i, 4 ] ) # assign p value objects
        
        assign( paste0( "odds.ratio.q", k ), round( exp( coef( mod1 )[ r.i ] ), digits = 2 ) ) # assign odds ratio objects
        
        assign( paste0( "conf.int.q", k ), paste0( '(', round( exp( confint( mod1 ) )[ r.i, 1 ], digits = 2 ), ', ',
                                                   round( exp( confint( mod1 ) )[ r.i, 2 ], digits = 2 ), ')' ) ) # assign 95% confidence interval objects
        
      }
      
      # extract p values for trend test, continuous variable model, and quadratic model so we can label significant results in the final table
      t.p <- sum.mod2[ trend.r, 4 ]
      c.p <- sum.mod3[ cont.r, 4 ]
      q.p <- sum.mod4[ quad.r, 4 ]
      
      
      ## assemble results table ##
      
      # base table
      res <- tibble( outcome.variable = y[j],
                     index = q.x[i],
                     
                     odds.ratio.q1 = "1.00" )   # Q1 is the referent
      
      for( v in 2:fac.length ){
        
        pre.c.names <- c( colnames( res ) ) # collect column names before adding new columns
        
        # bind columns
        res <- bind_cols( res, get( paste0( "odds.ratio.q", v ) ), get( paste0( "conf.int.q", v ) ) )
        
        # reassign column labels
        colnames( res ) <- c( pre.c.names, paste0( "odds.ratio.q", v ), paste0( "conf.int.q", v ) )
        
      }
      
      # add remainder of table columns     
      
      res <- res %>%
        # p values for trend, quadratic model, and results from running model with continuous variable
        mutate( trend.p.raw = t.p,
                qmax.p.raw = get( paste0( "q", fac.length, ".p" ) ),
                trend.p = paste0( round( t.p, digits = 2 ) ),
                odds.ratio.cont = round( exp( coef( mod3 )[ cont.r ] ), digits = 2 ),
                conf.int.cont = paste0( '(', round( exp( confint( mod3 ) )[ cont.r, 1 ], digits = 2 ), ', ',
                                        round( exp( confint( mod3 ) )[ cont.r, 2 ], digits = 2 ), ')' ),
                quad.p = paste0( round( q.p, digits = 2 ) ),
                
                # label significant results (p-values) with * or **
                trend.p = ifelse( t.p < 0.05 & t.p >= 0.01, paste0( trend.p, "*"),
                                  ifelse( t.p < 0.01, paste0( "< 0.01**" ), trend.p ) ),
                quad.p = ifelse( q.p < 0.05 & q.p >= 0.01, paste0( quad.p, "*"),
                                 ifelse( q.p < 0.01, paste0( "< 0.01**" ), quad.p ) ),
                conf.int.cont = ifelse( c.p < 0.05 & c.p >= 0.01, paste0( conf.int.cont, "*"),
                                        ifelse( c.p < 0.01, paste0( conf.int.cont, "**" ), conf.int.cont ) ) )
      
      # label confidence intervals with * or **
      
      for( h in 2:fac.length ){
        
        ci <- paste0( "conf.int.q", h )  # ci index
        p.r <- paste0( "q", h, ".p" )    # p value index
        
        # use string references in mutate to alter the confidence interval and odds ratio columns
        res <- res %>%
          mutate( !!ci :=  ifelse( get( p.r ) < 0.05 & get( p.r ) >= 0.01, paste0( get( ci ), "*"),
                                   ifelse( get( p.r ) < 0.01, paste0( get( ci ) , "**" ), get( ci )  ) ) )
        # `!!` are used to evaluate strings as expression in dplyr
        
      }
      
      
      # keep table of raw p values for Q4 and trend (for Manhattan plot) and then "nice" table for manuscript presentation
      
      raw.p.vals[[j]] <- res %>% select( outcome.variable, index, contains( "raw" ) )
      
      out.var.list[[j]] <- res %>% select( -contains( "raw" ) ) # remove raw p values from "nice" table
      
    }
    
    raw.p[[i]] <- do.call( "rbind", raw.p.vals )
    index.res.list[[i]] <- do.call( "rbind", out.var.list )
    
  }
  
  p.4 <- do.call( "rbind", raw.p ) %>%
    mutate( outcome.variable = str_remove_all( outcome.variable, ".cat" ),
            outcome.variable = str_remove_all( outcome.variable, ".bin" ),
            index = str_remove( index, paste0( ".q", fac.length ) ),
            outcome.variable = toupper( outcome.variable ) )
  
  p.4$index <- recode_factor( p.4$index, ahei = "AHEI", amed = "aMED", dash = "DASH", low.carb = "Low Carb", 
                              a.low.carb = "Animal LC", v.low.carb = "Plant LC" )
  
  p.4 <- p.4 %>% unite( model, index, outcome.variable, sep = ", " )
  
  
  ## rowbind table and collapse respective OR and CI columns into single columns ##
  t.4 <- do.call( "rbind", index.res.list ) 
  
  for( v in 2: fac.length ) {
    
    q.column <- paste0( "q", v )
    
    # `unite` function to collapse columns
    t.4 <- t.4 %>%
      unite( !!q.column, contains( q.column ), sep = " " )   # "q" columns
    
  }
  
  # unite columns for continuous variable OR
  t.4 <- unite( t.4, cont, contains( "cont" ), sep = " " ) 
  
  ## tidy up significant digits ##
  
  t.4 <- data.frame( t.4 ) # convert to dataframe first
  
  # column indices for odds ratio/ci
  col.ind.q <- c( which( str_detect( colnames( t.4 ), "q\\d" ) ), 
                      which( str_detect( colnames( t.4 ), "cont" ) ) )
  
  # odds ratios and confidence intervals
  for( i in col.ind.q ) {
    t.4[,i] <- str_replace( t.4[,i], '(\\(\\d)\\,', "\\1\\.00," ) # match open parenthesis followed by a digit and then a comma. Retain everything except the comma and add ".00,"
    t.4[,i] <- str_replace( t.4[,i], "(\\(\\d\\.\\d)\\,","\\10\\,") # match open parenthesis followed by a digit, period, digit, and then a comma. Retain everything except the comma and add ".0,"
    t.4[,i] <- str_replace( t.4[,i], "(\\(\\d\\.\\d)\\,", "\\10\\," ) # match open parenthesis followed by digit, period, digit and comma.Retain everything except the comma and add "0,"
    t.4[,i] <- str_replace( t.4[,i], "(\\d\\.\\d)\\s", "\\10 " ) # match digit, period, digit and space. Retain everything except space and add "0 "
    t.4[,i] <- str_replace( t.4[,i], "^(\\d)\\s\\(", "\\1\\.00 \\(" ) # match beginning of string followed by single digit, followed by a space and parenthesis. Reatin the single digit and and ".00 ()
    t.4[,i] <- str_replace( t.4[,i], "(\\d\\.\\d)\\)", "\\10\\)") # match digit, period, digit, close parenthesis. Retain everything except parenthesis and add "0)" to end
    t.4[,i] <- str_replace( t.4[,i], "(\\,\\s\\d)\\)", "\\1.00\\)") # match comma, space, digit, close parenthesis. Retain everything except parenthesis and add ".00)" to end
    
  }
  
  # column indices for columns containing p values 
  col.ind.q <- which( str_detect( colnames( t.4 ), "\\.p" ) )
  
  for( i in col.ind.q ){
    t.4[,i] <- str_replace( t.4[,i], "(\\d\\.\\d)$", "\\10" ) # match digit, period, digit,end and add a 0 before the end
    t.4[,i] <- str_replace( t.4[,i], "^1$", "0.99" ) # round down probabilities = 1
  }
  
  
  ### Manhattan-like Plot of P Values ###
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  
  p.plot.d <- p.4 %>%
    mutate( sig.res.q4 = as.factor( ifelse( qmax.p.raw < 0.05, "p < 0.05", 
                                            ifelse( qmax.p.raw >= 0.05, "p > 0.05", NA ) ) ),  # labels for significant genera
            sig.res.trend = as.factor( ifelse( trend.p.raw < 0.05, "p < 0.05", 
                                               ifelse( trend.p.raw >= 0.05, "p > 0.05", NA ) ) ) )
  
  # subset of the data that will be labeled by geom_repel
  set.seed( seed.1 ) # set.seed for the `runif` function
  
  dodges.qmax <- filter( p.plot.d, qmax.p.raw < 0.05 ) %>%
    mutate( dodgey = runif( nrow( . ), -1.5, 0.5 ),
            dodgex = runif( nrow( . ), -1.5, 1.5 ))   # `dodge` will be used to nudge the labels in the y direction
  
  set.seed( seed.2 ) # set.seed for the `runif` function
  
  dodges.trend <- filter( p.plot.d, trend.p.raw < 0.05 ) %>%
    mutate( dodgey = runif( nrow( . ), -0.5, 1.2 ),
            dodgex = runif( nrow( . ), -1.5, 1.5 ))   # `dodge` will be used to nudge the labels in the y direction
  
  # in order to minimize overlap
  
  # set y limit for the y axis so that no points are omitted 
  y.upper <- max( -log10( min( p.plot.d$trend.p.raw ) ), 
                  -log10( min( p.plot.d$qmax.p.raw ) ) )
  
  p.out <- p.plot.d %>%
    ggplot(  ) +
    # P values for Q4 and their labels
    geom_point( p.plot.d, mapping = aes( y = -log10( qmax.p.raw ), x = model, color = sig.res.q4 ) ) +
    ylim( 0,2 ) +
    
    # labels
    geom_label_repel(data = dodges.qmax,
                     mapping = aes( y = -log10( qmax.p.raw ), x = model ),
                     label = dodges.qmax$model,
                     nudge_y = dodges.qmax$dodgey,
                     nudge_x = dodges.qmax$dodgex,
                     label.size = 0.25,
                     box.padding   = 0.35,
                     point.padding = 0.35,
                     segment.color = "grey50",
                     colour = "red3",
                     max.overlaps = 100,
                     show.legend = F,
                     size = 2 ) +
    
    # coloring
    scale_colour_manual( NULL, breaks = c( "p < 0.05", "p > 0.05" ),
                         values = c( "red3", "gray82"),
                         guide = guide_legend( order = 1 ),
                         labels = unname( TeX( c( paste0( "$p_{Q", fac.length,"} < 0.05$" ), paste0( "$p_{Q", fac.length, "} \\geq 0.05$" ) ) ) ) ) +
    new_scale_color() + # below this, a new color scale can be used
    
    # P values for trend test and their labels
    geom_point( p.plot.d, mapping = aes( y = -log10( trend.p.raw ), x = model, color = sig.res.trend ) ) +
    ylim( 0,2 ) +
    
    # labels
    geom_label_repel(data = dodges.trend,
                     mapping = aes( y = -log10( trend.p.raw ), x = model ),
                     label = dodges.trend$model,
                     nudge_y = dodges.trend$dodgey,
                     nudge_x = dodges.trend$dodgex,
                     label.size = 0.25,
                     box.padding   = 0.35,
                     point.padding = 0.35,
                     segment.color = "grey50",
                     colour = 'navyblue',
                     max.overlaps = 100,
                     show.legend = F,
                     size = 2 ) +
    # coloring
    scale_colour_manual( NULL, breaks = c( "p < 0.05", "p > 0.05" ),
                         values = c( "navyblue", "gray82"),
                         guide = guide_legend(order = 2),
                         labels = unname( TeX( c( "$p_{Trend} < 0.05$", "$p_{Trend} \\geq 0.05$") ) ) ) +
    
    ylab( "-Log10 (p-Values)" ) +
    xlab( "Diet Index, Response Variable" ) +
    geom_hline(
      yintercept = -log10( 0.05 ) , # dashed line indicating alpha = 0.05 threshold
      col = "black",
      linetype = "dashed",
      size = 0.4 ) +
    theme_classic() +
    theme( axis.text.x = element_blank(),
           text = element_text( family = "Avenir" ) ) + 
    scale_y_continuous( limits = c( 0, y.upper ) )  # set y limit and ensure we do not get a warning
  # of e.g., Warning messages:
  # 1: Removed 60 rows containing missing values
  # (geom_point). 
  # 2: Removed 12 rows containing missing values
  # (geom_label_repel). 
  
  
  
  ## Significant Digits in `nice.table`
  
  
  return( list( nice.table = t.4, raw.table = p.4, m.plot = p.out ) )
  
}

# example: 
# fj <- fit_res( df = d,
#                x = i.names,
#                z = covariates,
#                y = outcome.variables,
#                q.x =indices,
#                trend = t.names )
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

