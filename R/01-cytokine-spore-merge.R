
###---------------------------------------------------
###   01-CYTOKINE DATA IMPORT AND WRANGLING
###---------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------------------------------------
# 
# In this script, we will import and clean the SPORE cytokine data. Subsequently, some exclusions
# ( based on caloric intake and missing cytokine data ) will be made.
# 
# INPUT DATA FILE: "01-Data-Raw/cytokine.sas7bdat"
#
# OUTPUT DATA FILE: "02-Data-Wrangled/01-cytokine-spore-merge.rds"
#
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

library( haven )
library( tidyverse )

### Import cytokine data ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

( d.cyt <- read_sas( '01-Data-Raw/cytokine.sas7bdat' )%>%
  rename( idnum = RESP_LABEL )%>% # rename ID variable
  mutate( idnum = as.numeric( idnum ) ) ) %>% # change type
  summarize( n = n( ), unique = length( unique( idnum ) ), ncol = ncol(.) ) 

# using regex on single cytokine column to remove ', ' and '>' in order to convert to numeric properly
as.numeric( str_remove_all( d.cyt$IL10, '[ \\, \\> ]' ) ) 

# apply function and clean up cytokine character data by removing ", " and ">" to ensure numeric data can be used
d.cyt[ , 9:ncol( d.cyt ) ] <- sapply( d.cyt[ , 9:ncol( d.cyt ) ], 
                                      function( x ) as.numeric( str_remove_all( x, '[ \\, \\> ]' ) ) )
# ---------------------------------------------------------------------------------------------------------------------------------------------------------



### Import Original Spore Data ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

d.spore <- read_sas( "01-Data-Raw/final_merge.sas7bdat" )%>%
  rename( idnum = Idnum ) # ensure column names match up

# merge cytokine data with remainder of SPORE data
( merged <- left_join( d.spore, d.cyt, by = 'idnum' ) ) %>%
  summarize( n = n( ), unique = length( unique( idnum ) ), ncol = ncol(.) ) 

# check number of subjects with cytokine data available
sapply( merged[ , ( ncol( merged )-10 ):ncol( merged ) ], function( x ) sum( is.na( x ) == F ) )
# there are 210 subjects with cytokine data

# column index positions of inflammatory cytokine variables:
cyt.pos <- which( colnames( merged ) %in% names( d.cyt )[ 9:length( colnames( d.cyt ) ) ] )
# ---------------------------------------------------------------------------------------------------------------------------------------------------------




### Exclusions ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

# filter and keep only those with complete cytokine data
( merged.b <- merged%>%
  filter( is.na( IL6 ) == F ) ) %>%
  summarize( n = n( ), unique = length( unique( idnum ) ), ncol = ncol(.) ) 

# calories
( merged.c <- merged.b%>%
  filter( CALOR >= 200 & CALOR <= 5000 ) ) %>%
  summarize( n = n( ), unique = length( unique( idnum ) ), ncol = ncol(.) ) 

# 2 observations with greater than 5000 kcals and none with less than 200 kcals but
# 47 with missing KCAL data.
sapply( merged.c[ , ( ncol( merged.c ) - 10 ):ncol( merged.c ) ], function( x ) sum( is.na( x ) == F ) )

# keep only those with dietary data and covariates
( merged.c %>%
  filter( !is.na( CALOR ) | !is.na( Age_at_Diagnosis ) | !is.na( dissite ) |
            !is.na( STAGE_B ) | !is.na( smoker ) | !is.na( drinker ) ) ) %>%
  summarize( n = n( ), unique = length( unique( idnum ) ), ncol = ncol(.) ) 
  
saveRDS( merged.c, "02-Data-Wrangled/01-cytokine-spore-merge.rds" )
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


