###---------------------------------------------------
###   05-FIT PROPORTIONAL ODDS/LOGIT MODELS
###---------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------------------------------------
# 
# In this script, we will fit proportional odds models and logit models (depending on which outcome variable
# we are using--more is explained on this below). We will use quartile-cut versions of the dietary variables as the
# explanatory variables. All results will be tabulated and polished for presentation.
# 
# INPUT DATA FILE: "/02-Data-Wrangled/02-fg-index-scores.rds"
#
# OUTPUT FILES: "03-Data-Rodeo/04-analytic-data.rds", "04-Figures-Tables/table-4.txt", 
#
# Resources: https://acsjournals.onlinelibrary.wiley.com/doi/pdfdirect/10.1002/cncr.28778 
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

library( tidyverse )
library( VGAM )  # for proportional odds model-fitting
library( ggrepel ) # for plotting
library( ggnewscale ) # for multiple color scales in single ggplot
library( latex2exp ) # for incorporating LaTex in plots

# read helper functions into global environment (table-generating helper functions reside in this file)
source( "R/utils.R" )

### Read-in Data ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
( d <- readRDS( "03-Data-Rodeo/03-analytic-data.rds" ) ) %>%
  summarize( n = n( ), unique = length( unique( idnum ) ), ncol = ncol(.) ) 
# 146 obs. and 123 cols.
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


## Prepare Vectors Loop Will Pull From ##
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

outcome.variables <- c( "ifn.cat", "il6.cat", "tnf.bin", "il10.cat", "il17.bin", "il8.cat", "tgf.cat",
                        "gro.cat", "vegf.cat", "hgf.cat", "cyt.sum.cat" )  # outcome variables we will use

covariates <- c( "age","sex", "bmi", "tum.site", 
                 "smoker", "drinker", "calor", 
                 "stage.binary", "hpv.status" )  # covariates we will adjust for

cov.collapse <- paste0( covariates, collapse = " + ") # collapse covariates into a single string for model formula

i.names <- c( "ahei", "amed", "dash", "low.carb", "a.low.carb", "v.low.carb" )

indices.q4 <- paste0( i.names, ".q4" ) # diet quality indices (note quartiles)

indices.q3 <- paste0( i.names, ".q3" ) # diet quality indices (note tertiles for sensitivity analysis)

t.names.q4 <- paste0( indices.q4, ".trend" ) # trend variables for trend analysis

t.names.q3 <- paste0( indices.q3, ".trend" ) # trend variables for trend analysis (tertiles sensitivity analysis)

# ---------------------------------------------------------------------------------------------------------------------------------------------------------




### Use Helper Function to Fit Models, Generate Tables, and Plots ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

# using quartile versions of the cut variables
 analysis.q4 <- fit_res( dat = d,
                x = i.names,
                z = covariates,
                y = outcome.variables,
                q.x =indices.q4,
                trend = t.names.q4 )
ggsave( "04-Figures-Tables/figure-1.tiff" )



# using tertile versions of the cut variables as a robustness analysis
analysis.q3 <- fit_res( dat = d,
                        x = i.names,
                        z = covariates,
                        y = outcome.variables,
                        q.x =indices.q3,
                        trend = t.names.q3 )


# ---------------------------------------------------------------------------------------------------------------------------------------------------------




### Stratify by Cancer Stage ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
# using quartile versions of the cut variables

indices <- c( "ahei", "amed", "dash", "low.carb", "a.low.carb", "v.low.carb" )

d.03 <- d %>% 
  filter( stage.binary == "Stages 0-3" ) %>%
  bin_patterns( df = ., pattern.cols = indices ) # bin observations in the subset

d.4 <- d %>% 
  filter( stage.binary == "Stage 4" ) %>%
  bin_patterns( df = ., pattern.cols = indices ) # bin observations in the subset


analysis.q3.x1 <- fit_res( dat = d.03,
                        x = i.names,
                        z = covariates[ !covariates %in% c( "stage.binary", "tum.site" ) ],
                        y = outcome.variables,
                        q.x =indices.q3, # use tertile variables given limited sample size
                        trend = t.names.q3 )

analysis.q3.x2 <- fit_res( dat = d.4,
                           x = i.names,
                           z = covariates[ !covariates %in% c( "stage.binary", "tum.site" ) ],
                           y = outcome.variables,
                           q.x =indices.q3, # use tertile variables given limited sample size
                           trend = t.names.q3 )
# ---------------------------------------------------------------------------------------------------------------------------------------------------------




### Save Tables and Figures ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

# full table 3
write.table( analysis.q4$nice.table %>%
               filter( ! outcome.variable %in% c("il17.bin", "tnf.bin" ) ),
             "04-Figures-Tables/table-3.txt", sep = "," )

# truncated table 3 with only significant results (proportional odds models)
analysis.q4$nice.table %>%
  filter( ! outcome.variable %in% c("il17.bin", "tnf.bin" ) ) %>% # keep only prop odds models results
  filter( str_detect( q4, "\\*" ) |  # filter significant results
            str_detect( trend.p, "\\*" ) ) %>%
  write.table( "04-Figures-Tables/table-3-trunc.txt", sep = "," )


# full table 4
write.table( analysis.q4$nice.table %>%
               filter( outcome.variable %in% c("il17.bin", "tnf.bin" ) ),
             "04-Figures-Tables/table-4.txt", sep = "," )

# truncated table 4 with only significant results (logit models)
analysis.q4$nice.table %>%
  filter( outcome.variable %in% c("il17.bin", "tnf.bin" ) ) %>% # keep only logit models results
  filter( str_detect( q4, "\\*" ) | 
            str_detect( trend.p, "\\*" ) ) %>%
  write.table( "04-Figures-Tables/table-4-trunc.txt", sep = "," )


# merge and save stratified results as table s3
strat.merge <- rbind( analysis.q3.x1$nice.table %>%
  mutate( group = "Stages 0-III" ) %>%
  relocate( group, .before = "outcome.variable" ),
analysis.q3.x2$nice.table %>%
  mutate( group = "Stage IV" ) %>%
  relocate( group, .before = "outcome.variable" ) ) 

# full table s1
strat.merge %>%
  filter( outcome.variable %notin% c( "il17.bin", "tnf.bin" ) ) %>% # keep only prop odds models results
  write.table( "04-Figures-Tables/table-s1.txt", sep = "," )

# truncated table s1 with only significant results 
strat.merge %>%
  filter( outcome.variable %notin% c( "il17.bin", "tnf.bin" ) ) %>% # keep only prop odds models results
  filter( str_detect( q3, "\\*" ) | 
            str_detect( trend.p, "\\*" ) ) %>%
  write.table( "04-Figures-Tables/table-s1-trunc.txt", sep = "," )


# full table s2
strat.merge %>%
  filter( outcome.variable %in% c( "il17.bin", "tnf.bin" ) ) %>% # keep only logit models results
  write.table( "04-Figures-Tables/table-s2.txt", sep = "," )

# truncated table s2 with only significant results 
strat.merge %>%
  filter( outcome.variable %in% c( "il17.bin", "tnf.bin" ) ) %>% # keep only logit models results
  filter( str_detect( q3, "\\*" ) | 
            str_detect( trend.p, "\\*" ) ) %>%
  write.table( "04-Figures-Tables/table-s2-trunc.txt", sep = "," )


  


# ---------------------------------------------------------------------------------------------------------------------------------------------------------






### Food Groups Analysis ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

# Food groups to analyze (note: eggs and soy was ommitted given )
fds <-  c( "f.total", "f.citmlb", "f.other", "f.juice", "v.total", "v.drkgr", "v.redor.tomato", "v.redor.total",
           "v.starchy.total", "v.starchy.potato", "v.starchy.other", "v.other", "v.legumes", "pf.total", "pf.mps.total",
           "pf.meat", "pf.curemeat", "pf.organ", "pf.poult", "pf.seafd", "pf.nuts", "d.total",
           "d.yogurt", "d.cheese", "oils", "solid.fats", "add.sugars", "a.drinks", "g.refined", "g.whole" ) # fd grp variables

fds.q <- paste0( fds, ".q" ) # bin variables

fds.trend <- paste0( fds, ".trend" ) # trend variables

fds.all <- list() # initialize list to store results
for( i in 1:length( fds ) ){
  
# create quartile version of fd-group variable
d.fd <- d %>% 
  mutate( !!fds.q[i] := as.factor( quant_cut( var = fds[i], x = 4, df = . ) ) )

# create trend variable
d.fd <- trend_func( rank.var = fds.q[i] ,cont.var = fds[i], df = d.fd,
            trend.var = fds.trend[i],
            x = 4 ) # bin fd group into quartiles and create trend variables

# run analysis
fd.g.analysis <- fit_res( dat = d.fd,
         x = fds[i],
         z = covariates,
         y = outcome.variables,
         q.x =fds.q[i],
         trend = fds.trend[i] )

fds.all[[i]] <- fd.g.analysis$nice.table  # store results in list

}


# some food groups were not able to be categorized into quartiles given limited intake
# thus, we will add columns with "NA" for those that were not able to be categorized into Q3 and Q4

for( i in 1:length( fds.all) ){
  
if ( sum( str_detect( colnames( fds.all[[i]] ), "q3") ) == 0 ){
  
  fds.all[[i]] <- fds.all[[i]] %>%
    mutate( q3 = "NA", 
            q4 = "NA" ) %>%
    relocate( q3, .after = q2 ) %>%
    relocate( q4, .after = q3 )
  
}

else if ( sum( str_detect( colnames( fds.all[[i]] ), "q4") ) == 0 ){
  
  fds.all[[i]] <- fds.all[[i]] %>%
    mutate( q4 = "NA" ) %>%
    relocate( q4, .after = q3 )
  
}
  
}

# row bind all results
all.fd.res <- list_it( fds.all )


## Save Tables ##

# full table s3 (prop odds models)
write.table( all.fd.res %>%
               filter( ! outcome.variable %in% c("il17.bin", "tnf.bin" ) ),
             "04-Figures-Tables/table-s3.txt", sep = "," )

# full table s4 (logit models)
write.table( all.fd.res %>%
               filter( outcome.variable %in% c("il17.bin", "tnf.bin" ) ),
             "04-Figures-Tables/table-s4.txt", sep = "," )

# truncated table s4 with only significant results (proportional odds models)
all.fd.res %>%
  filter( ! outcome.variable %in% c("il17.bin", "tnf.bin" ) ) %>% # keep only prop odds models results
  filter( str_detect( q4, "\\*" ) |  # filter significant results
            str_detect( trend.p, "\\*" ) ) %>%
  write.table( "04-Figures-Tables/table-s3-trunc.txt", sep = "," )

# truncated table s5 with only significant results (logit models)
all.fd.res %>%
  filter( outcome.variable %in% c("il17.bin", "tnf.bin" ) ) %>% # keep only prop odds models results
  filter( str_detect( q4, "\\*" ) |  # filter significant results
            str_detect( trend.p, "\\*" ) ) %>%
  write.table( "04-Figures-Tables/table-s4-trunc.txt", sep = "," )




