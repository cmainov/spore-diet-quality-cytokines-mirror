
###---------------------------------------------------
###   04-TABLES: EPIDEMIOLOGIC CHARACTERISTICS
###---------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------------------------------------
# 
# In this script, we will generate Table 1 (epidemiological characteristics of the study sample) and Table 2 (epidemiological
# characteristics by diet quality indices) to understand the influence of potential observed confounders prior to any
# formal analysis.
# 
# INPUT DATA FILE: "/02-Data-Wrangled/02-fg-index-scores.rds"
#
# OUTPUT FILES: "04-Figures-Tables/table-1.txt", 
#
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


library( tidyverse )


# read helper functions into global environment (table-generating helper functions reside in this file)
source( "R/utils.R" )

### Read-in Data ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
( d <- readRDS( "03-Data-Rodeo/03-analytic-data.rds" ) )%>%
  summarize( n = n( ), unique = length( unique( idnum ) ), ncol = ncol(.) ) 
# 146 obs. and 78 cols.
# ---------------------------------------------------------------------------------------------------------------------------------------------------------



### Table 1 ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

## Table 1 will consist of one column (i.e., not stratified) summarizing the epidemiological characteristics of the study sample:

# continuous variables that will be used to generate the table
these.1 <- c( "age", "bmi", "ahei", "amed", "dash", "low.carb", "a.low.carb", "v.low.carb" )

# their names in the table
tnames.1 <- c( "Age", "Body Mass Index (kg/m^2)", "AHEI Index", "aMED Index", "DASH Index", 
               "Low Carbohydrate Index", "Animal-Based Low Carbohydrate Index", "Plant-Based Low Carbohydrate Index" )

# categorical variables that will be used to generate the table
these.2 <- c( "sex","race.ethnicity", "hpv.status", "tum.site", "stage.binary", "ace.binary", "smoker", "drinker" )
tnames.2 <- c( "Sex", "Race and Ethnicity", "HPV Status", "Tumor Site", "Tumor Stage", "ACE Comorbidity Score", "Smoking Status", "Drinking Status" )

## for-loop to generate table (continuous variables only)
# outer loop will loop through the three datasets (to generate three columns) and the inner loop
# will loop through the variables, which will result in a separate row for each variable in the table


  d.in <- data.frame()  # initialize data.frame for loop to store rows of the table
  for ( i in 1: length( these.1 ) ) {
    
    d.in <- rbind( d.in, tab1.var.mean( var.name = these.1[i],
                                        df = d,
                                        table.var.name = tnames.1[i],
                                        strata.var = NULL,
                                        strata.level = NULL ) ) 
  
}



# for-loop for categorical variables

  d.in.2 <- data.frame()     
  for ( i in 1: length( these.2 ) ) {
    
    d.in.2 <- rbind( d.in.2, tab1.var.freq( var.name = these.2[i],
                                        df = d,
                                        table.var.name = tnames.2[i],
                                        strata.var = NULL,
                                        strata.level = NULL ) )
  }
  


# merge as rows and reorder rows for final presentation
t.1 <- rbind( d.in, d.in.2 )[ c( 1, 9:11, 12:17, 2, 22:28, 18:21, 29:39, 3:8 ), ]


# polish significant digits
t.1[,2] <- str_replace( t.1[,2], "\\(0\\)", "(0.0)" )          # a zero in parentheses
t.1[,2] <- str_replace( t.1[,2], "(?<=\\(\\d)\\)", ".0)" )     # closing parentheses preceded by an opening parentheses followed by a single digit
t.1[,2] <- str_replace( t.1[,2], "(?<=\\(\\d\\d)\\)", ".0)" )  # closing parentheses preceded by an opening parentheses followed by two digits

# ---------------------------------------------------------------------------------------------------------------------------------------------------------


### Save Table 1 ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
write.table( t.1, "04-Figures-Tables/table-1.txt", sep = "," )
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


### Table 2 ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

# Table 2 will be similar to Table 1. We will stratify by two levels of each diet index (median-split)
# and look at the distribution of the covariates across those two levels to see if the diet indices may be correlated
# with any of the characteristics. Finally, we will add another dimension by making two tables--one for cases and one 
# for controls.


# first make variables splitting observations at the median
names.q2 <- paste0( c( "ahei", "amed", "dash", "low.carb", "a.low.carb", "v.low.carb" ), ".q2" )
i.names <- c( "ahei", "amed", "dash", "low.carb", "a.low.carb", "v.low.carb" )

## Table 2 for Cases ##

# for-loop for continuous variables

t.out.1 <- list()
for( g in 1:length( i.names ) ) {
  
  d.out.3 <- list()                                               # initialize list to store columns of Table 2
  for ( j in 1:2 ) {                                              # index on 1st/2nd quantile
    
    d.in <- data.frame()     
    for ( i in 1: length( these.1 ) ) {
      
      d.in <- rbind( d.in, tab1.var.mean( var.name = these.1[i],
                                                      df = d,
                                                      table.var.name = tnames.1[i],
                                                      strata.var = names.q2[g],
                                                      strata.level = c( 1,2 )[[j]] ) ) 

    }
    
    d.out.3[[j]] <- d.in
    
    
  }
  
  t.out.1[[g]] <- do.call( "cbind", d.out.3 )
  
}

# column bind controls and cases separate
final.bind <- do.call( "cbind", t.out.1 )[ , -c( 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23 )]


# for-loop for categorical variables

t.out.2.cat <- list()
for( g in 1:length( names.q2 ) ) {
  
  d.out.4.cat <- list()
  for ( j in 1:2 ) {                                              # index on case or control status
    
    d.in.cat <- data.frame()
    for ( i in 1: length( these.2 ) ) {
      
      d.in.cat <- rbind( d.in.cat, tab1.var.freq( var.name = these.2[i],
                                                            df = d,
                                                            table.var.name = tnames.2[i],
                                                            strata.var = names.q2[g],
                                                            strata.level = c( 1,2 )[[j]] ) )
      
    }
    
    d.out.4.cat[[j]] <- d.in.cat

  }
  
  t.out.2.cat[[g]] <- do.call( "cbind", d.out.4.cat )
  
}

# column bind controls and cases separate
final.bind.cat <- do.call( "cbind", t.out.2.cat )[ , -c( 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23 )]
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


### Put Together Final Table 2 ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

t.2 <- bind_rows( final.bind, final.bind.cat )[ c( 1, 9:11, 12:17, 2, 22:28, 18:21, 29:39, 3:8 ), ] # reorder rows for final presentation



## polish significant digits ##

for( i in c( 2:12 ) ){  # columns 2:12 only
  t.2[,i] <- str_replace( t.2[,i], "\\(0\\)", "(0.0)" )          # a zero in parentheses

  t.2[,i] <- str_replace( t.2[,i], "(?<=\\(\\d)\\)", ".0)" )          # closing parentheses preceded by an opening parentheses followed by a single digit

  t.2[,i] <- str_replace( t.2[,i], "(?<=\\(\\d\\d)\\)", ".0)" )        # closing parentheses preceded by an opening parentheses followed by two digits

}


## column names ##

# store n values for each median split
ahei.q.n <- table( d$ahei.q2 )
amed.q.n <- table( d$amed.q2 )
dash.q.n <- table( d$dash.q2 )
low.carb.q.n <- table( d$low.carb.q2 )
a.low.carb.q.n <- table( d$a.low.carb.q2 )
v.low.carb.q.n <- table( d$v.low.carb.q2 )

# n values label each column
colnames( t.2 ) <- c( "Characteristic", paste0( "n = ", ahei.q.n ),
   paste0( "n = ", amed.q.n ),
   paste0( "n = ", dash.q.n ),
   paste0( "n = ", low.carb.q.n ),
   paste0( "n = ", a.low.carb.q.n ),
   paste0( "n = ", v.low.carb.q.n ) )

# an additional layer of column names will be necessary: the order of the indices is: AHEI, aMED,
# DASH, Low Carb (LC), Animal-based LC, Plant-based LC
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

### Save ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
write.table( t.2, "04-Figures-Tables/table-2.txt", sep = "," )
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

