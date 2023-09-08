###---------------------------------------------------
###   02-COMPUTE AND APPEND DIET QUALITY SCORES
###---------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------------------------------------
# 
# In this script, we will compute the diet quality index scores (AHEI-2010, aMED, DASH, Low Carbohydrate,
# Animal-Based Low Carbohydrate, and Plant-Based Low Carbohydrate) and then append them to the SPORE/Cytokine
# data wrangled in the previous step. Also, food groups will be computed as we will compare the results of the
# analyses with diet scores to food groups.
# 
# INPUT DATA FILE: "02-Data-Wrangled/01-cytokine-spore-merge.rds"
#
# OUTPUT DATA FILE: 
#
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


library( tidyverse )

# read helper functions into global environment
source( "R/utils.R" )


### Read-in Data ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

( d <- readRDS( "02-Data-Wrangled/01-cytokine-spore-merge.rds" ) ) %>%
  summarize( n = n( ), unique = length( unique( idnum ) ), ncol = ncol(.) ) 
# 160 obs. and 2559 cols. (there appears to be one duplicate row--let's examine if there is a difference
# in the diet scores n those duplicate rows at the end of this script. If there are no differences in the 
# scores, we will delete one of those rows arbitrarily) NOTE: we tried using the `distinct` function but it
# appears that this subject has different responses in one or more of the 2559 cols. across both of the rows they
# occupy

colnames( d ) <- str_replace_all( colnames( d ), "\\_", "." )
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


### Unit Conversions ( Raw Input to servings/day ) ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

## First Conversion
vars.1 <- "AVOCADO RAISGRP PRUN.J BAN A.J GRFRT.J O.J O.J.CALC OTH.F.J ICE.LET ROM.LET CELERY TOM.J TOFU SALSA CARROT.R CARROT.C YOG.FR.NONFAT YOG ICE.CR YOG.PLAIN WALNUTS NUTS OTH.NUTS HOTDOG PROC.MTS CHIX.DOG CHIX.NO CHIX.SK EGG.BEAT EGGS.OMEGA EGGS ENG.MUFF MUFFIN BR.RICE WH.RICE PASTA GRAIN PANCAK PIZZA.02 POT TUNA COT.CH CR.CH OTH.CH"
vars.1 <- unlist( str_split( vars.1, ' ' ) )

d[ , vars.1 ]                          #this shows the above diet variables are present in the combined dataset

#get column indices
these.1 <- which( colnames( d )%in% vars.1 )

# Use for-loop for conversion

for ( i in these.1 ){
  d[i] <- ifelse( d[i] == 1, 0,
                  ifelse( d[i] == 2, 0.02,
                          ifelse( d[i] == 3, 0.08,
                                  ifelse( d[i] == 4, 0.14,
                                          ifelse( d[i] == 5, 0.43,
                                                  ifelse( d[i] == 6, 0.8,
                                                          ifelse( d[i] == 7, 1,
                                                                  ifelse( d[i] == 8, 2,
                                                                          ifelse( d[i] == 9, 0, NA ) ) ) ) ) ) ) ) )
}

# check and ensure fidelity
d[ , these.1 ]



## Second Conversion

vars.2 <- "PRUN A.SCE PEACHES STRAW TOM BROC CABB.COLE CAUL BRUSL CORN PEAS MIX.VEG BEANS ZUKE SWT.POT SPIN.CKD SPIN.RAW KALE YEL.SQS ONIONS ONIONS1 PEPPERS BACON HAMB XTRLEAN.HAMBURG SAND.BF.HAM PORK BEEF02 dk.fish oth.fish fr.fish.kids shrimp.ckd"
vars.2 <- toupper( vars.2 )               # ensure consistent casing

vars.2 <- unlist( str_split( vars.2, ' ' ) )
d[ , vars.2 ]                          #this shows the above diet variables are present in the combined dataset

# get column indices
these.2 <- which( colnames( d )%in% vars.2 )

# Use for-loop for conversion

for ( i in these.2 ){
  d[i] <- ifelse( d[i] == 1, 0,
                  ifelse( d[i] == 2, 0.02,
                          ifelse( d[i] == 3, 0.08,
                                  ifelse( d[i] == 4, 0.14,
                                          ifelse( d[i] == 5, 0.43,
                                                  ifelse( d[i] == 6, 0.8,
                                                          ifelse( d[i] == 7, 1,
                                                                  ifelse( d[i] == 8, 0, NA ) ) ) ) ) ) ) )
}

# check and ensure fidelity
d[ , these.2 ]



## Third Conversion

vars.3 <- "CANT PUNCH APPLE ORANG GRFRT SOYMILK.FORT SKIM.KIDS MILK2 MILK P.BU COKE OTH.CARB LOCALNO LOCALCAF T.BU.CAN.LOL BU MARGARINE COLD.CER CKD.CER CRAX TORTILLAS"
vars.3 <- unlist( str_split( vars.3, ' ' ) )

d[ , vars.3 ]                          #this shows the above diet variables are present in the combined dataset

#get column indices
these.3 <- which( colnames( d )%in% vars.3 )

# Use for-loop for conversion

for ( i in these.3 ){
  d[i] <- ifelse( d[i] == 1, 0,
                  ifelse( d[i] == 2, 0.02,
                          ifelse( d[i] == 3, 0.08,
                                  ifelse( d[i] == 4, 0.14,
                                          ifelse( d[i] == 5, 0.43,
                                                  ifelse( d[i] == 6, 0.8,
                                                          ifelse( d[i] == 7, 1,
                                                                  ifelse( d[i] == 8, 2.5,
                                                                          ifelse( d[i] == 9, 4,
                                                                                  ifelse( d[i] == 10, 0, NA ) ) ) ) ) ) ) ) ) )
}

# check and ensure fidelity
d[ , vars.3 ]



## Fourth Conversion

vars.4 <- "blue apricot tom.s st.beans BOLOGNA CHIX.NO.SAND"
vars.4 <- toupper( vars.4 )
vars.4 <- unlist( str_split( vars.4, ' ' ) )

d[ , vars.4 ]                          #this shows the above diet variables are present in the combined dataset


#get column indices
these.4 <- which( colnames( d )%in% vars.4 )

# Use for-loop for conversion

for ( i in these.4 ){
  d[i] <- ifelse( d[i] == 1, 0,
                  ifelse( d[i] == 2, 0.02,
                          ifelse( d[i] == 3, 0.08,
                                  ifelse( d[i] == 4, 0.14,
                                          ifelse( d[i] == 5, 0.43,
                                                  ifelse( d[i] == 6, 0.8,
                                                          ifelse( d[i] == 7, 0, NA ) ) ) ) ) ) )
}


# check and ensure fidelity
d[ , vars.4 ]



## Fifth Conversion ( Alcoholic Drinks )

vars.5 <- "R.WINE W.WINE BEER BEER.LITE LIQ GARLIC2 OLIVE.OIL WH.BR RYE.BR DK.BR"
vars.5 <- toupper( vars.5 )
vars.5 <- unlist( str_split( vars.5, ' ' ) )

d[ , vars.5 ]                          #this shows the above diet variables are present in the combined dataset


#get column indices
these.5 <- which( colnames( d )%in% vars.5 )

# Use for-loop for conversion

#serving for alcohol is 1 cup per day
for ( i in these.5 ){
  d[i] <- ifelse( d[i] == 1, 0,
                  ifelse( d[i] == 2, 0.02,
                          ifelse( d[i] == 3, 0.08,
                                  ifelse( d[i] == 4, 0.14,
                                          ifelse( d[i] == 5, 0.43,
                                                  ifelse( d[i] == 6, 0.8,
                                                          ifelse( d[i] == 7, 1,
                                                                  ifelse( d[i] == 8, 2.5,
                                                                          ifelse( d[i] == 9, 4.5,
                                                                                  ifelse( d[i] == 10, 6,
                                                                                          ifelse( d[i] == 11, 0, NA ) ) ) ) ) ) ) ) ) ) )
}


# check and ensure fidelity
d[ , vars.5 ]


## Sixth Conversion

vars.6 <- "LIVER CHIX.LIVER"
vars.6 <- toupper( vars.6 )
vars.6 <- unlist( str_split( vars.6, ' ' ) )

d[ , vars.6 ]                          #this shows the above diet variables are present in the combined dataset


# get column indices
these.6 <- which( colnames( d )%in% vars.6 )

# Use for-loop for conversion

for ( i in these.6 ){
  d[i] <- ifelse( d[i] == 1, 0,
                  ifelse( d[i] == 2, 0.02,
                          ifelse( d[i] == 3, 0.03,
                                  ifelse( d[i] == 4, 0.08,
                                          ifelse( d[i] == 5, 0.14,
                                                  ifelse( d[i] == 6, 0.43,
                                                          ifelse( d[i] == 7, 0, NA ) ) ) ) ) ) )
}


# check and ensure fidelity
d[ , vars.6 ]

# ---------------------------------------------------------------------------------------------------------------------------------------------------------


### Yogurt Variables ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

# change NA's in the corresponding columns to "0" so that subsequent conditional statements work
d[  , c( "YOG.NONE", "YOG.LOFAT", "YOG.REG", "YOG.NONFAT" ) ][ is.na( d[  , c( "YOG.NONE", "YOG.LOFAT", "YOG.REG", "YOG.NONFAT" ) ] ) ] <- 0

# create variables
d <- d %>% mutate( 
  
  # Low-fat yogurt
  YOG.LF.FLAV = ifelse( YOG.LOFAT == 1 & ( YOG.REG == 0 & YOG.NONFAT == 0 &
                                             YOG.NONE == 0 ), YOG, 
                        ifelse( YOG.LOFAT == 1 & ( YOG.REG == 1 & YOG.NONFAT == 0 &
                                                     YOG.NONE == 0 ), 0,
                                ifelse( YOG.LOFAT == 1 & ( YOG.REG == 0 & YOG.NONFAT == 1 &
                                                             YOG.NONE == 0 ), YOG, 0 ) ) ),
  
  YOG.LF.PLAIN = ifelse( YOG.LOFAT == 1 & ( YOG.REG == 0 & YOG.NONFAT == 0 &
                                              YOG.NONE == 0 ), YOG.PLAIN, 
                         ifelse( YOG.LOFAT == 1 & ( YOG.REG == 1 & YOG.NONFAT == 0 &
                                                      YOG.NONE == 0 ), 0,
                                 ifelse( YOG.LOFAT == 1 & ( YOG.REG == 0 & YOG.NONFAT == 1 &
                                                              YOG.NONE == 0 ), YOG.PLAIN, 0 ) ) ),
  # Non-fat yogurt
  YOG.NF.FLAV = ifelse( YOG.NONFAT == 1 & ( YOG.REG == 0 & YOG.LOFAT == 0 &
                                              YOG.NONE == 0 ), YOG, 
                        ifelse( YOG.NONFAT == 1 & ( YOG.REG == 1 & YOG.LOFAT == 0 &
                                                      YOG.NONE == 0 ), 0,
                                ifelse( YOG.NONFAT == 1 & ( YOG.REG == 0 & YOG.LOFAT == 1 &
                                                              YOG.NONE == 0 ), 0, YOG ) ) ),
  
  YOG.NF.PLAIN = ifelse( YOG.NONFAT == 1 & ( YOG.REG == 0 & YOG.LOFAT == 0 &
                                               YOG.NONE == 0 ), YOG.PLAIN, 
                         ifelse( YOG.NONFAT == 1 & ( YOG.REG == 1 & YOG.LOFAT == 0 &
                                                       YOG.NONE == 0 ), 0,
                                 ifelse( YOG.NONFAT == 1 & ( YOG.REG == 0 & YOG.LOFAT == 1 &
                                                               YOG.NONE == 0 ), 0, 0 ) ) ),
  # Regular yogurt
  YOG.REG.FLAV = ifelse( YOG.REG == 1 , YOG, 0 ),
  
  YOG.REG.PLAIN = ifelse( YOG.REG == 1 , YOG.PLAIN, 0 ) )
# ---------------------------------------------------------------------------------------------------------------------------------------------------------



### DASH Score Computation ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

## Food Group Categorizations

fg <- d %>%
  
  # total fruit servings
  mutate( T.FRUIT = BLUE + APRICOT + CANT + APPLE + ORANG + GRFRT + PRUN + A.SCE + PEACHES +
            STRAW + AVOCADO + RAISGRP + BAN,
          
          # total vegetable servings
          T.VEG = TOM.S + TOM + BROC + CABB.COLE + CAUL + BRUSL + CORN + MIX.VEG + ZUKE + SWT.POT +
            SPIN.CKD + SPIN.RAW + KALE + YEL.SQS + ONIONS + ONIONS1 + PEPPERS + ICE.LET +
            ROM.LET + CELERY + TOM.J + SALSA + CARROT.R + CARROT.C + GARLIC2,
          
          # total legume servings
          T.LEGUME = ST.BEANS + BEANS + SOYMILK.FORT + PEAS + P.BU + TOFU + WALNUTS + NUTS,
          
          #making a total legume category as well one that collapses nuts and beans separately
          LEGUME.BEAN = ST.BEANS + BEANS + SOYMILK.FORT + PEAS + TOFU,
          LEGUME.NUTS = P.BU + WALNUTS + NUTS + OTH.NUTS,
          
          # total red and processed meats
          T.RED.PROC.MTS = BOLOGNA + BACON + HOTDOG + PROC.MTS + CHIX.DOG + HAMB + XTRLEAN.HAMBURG +
            SAND.BF.HAM + PORK + BEEF02 + LIVER + CHIX.LIVER )


# Computing final dairy, drink, and fish variables

fg <- fg %>%
  
  mutate( T.LF.DAIRY = MILK2 + SKIM.KIDS + YOG.FR.NONFAT + YOG.NF.FLAV + YOG.NF.PLAIN + YOG.LF.FLAV + YOG.LF.PLAIN,
          
          T.DAIRY = MILK2 + SKIM.KIDS + YOG.FR.NONFAT + ICE.CR + YOG + YOG.PLAIN + MILK + CREAM +
            SKIM.KIDS,
          
          SUG.SWT.BEV = PUNCH + OTH.CARB + COKE + OTH.F.J,
          
          T.FRUITJUICE.SWT.BEV = A.J + GRFRT.J + O.J + O.J.CALC + PRUN.J + PUNCH + COKE + OTH.CARB,
          
          TOTAL.FISH = TUNA + DK.FISH + OTH.FISH + FR.FISH.KIDS + SHRIMP.CKD )



# collecting names so that a for loop can be used to bin key food group variables inton quintiles per operationalization

c.vars <- c( "SODIUM", "WHGRN", "T.VEG", "T.LEGUME", "T.FRUIT", "T.LF.DAIRY",
             "SUG.SWT.BEV", "T.RED.PROC.MTS", "T.DAIRY", "LEGUME.BEAN", "LEGUME.NUTS" )


# Get column locations
these.dash <- which( colnames( fg ) %in% c.vars ) 

# Generate new bin variable names
cut.var.names <- paste0( c.vars, ".q" )


# Create quintile variables
for ( i in 1:length( c.vars ) ){
  
  fg[ , cut.var.names[i] ] <- quant_cut( var = c.vars[i], df = fg, x = 5 )
  
}

# "BAD" food groups that get inverse weighting for final DASH score computation
these.bad <- which( colnames( fg ) %in% c( 'T.RED.PROC.MTS.q', 'SODIUM.q', 'SUG.SWT.BEV.q' ) )

# create flag variables for final score computation
flag.names <- names( fg[ , these.bad ] )
flag.vars <- paste0( flag.names, ".flag" )

for ( i in 1:length( flag.names ) ){
  fg[ , flag.vars[i] ] <-
    ifelse( fg[ , flag.names[i] ] == 1, 5,
            ifelse( fg[ , flag.names[i] ] == 2, 4,
                    ifelse( fg[ , flag.names[i] ] == 3, 3,
                            ifelse( fg[ , flag.names[i] ] == 4, 2,
                                    ifelse( fg[ , flag.names[i] ] == 5, 1, NA ) ) ) ) )
  
  
  
}                               

# Computation of final index score
( fg.dash <- fg %>%
    mutate( DASH = as.numeric( WHGRN.q ) + as.numeric( T.FRUIT.q ) + as.numeric( T.VEG.q ) +
              as.numeric( T.LEGUME.q ) + as.numeric( T.LF.DAIRY.q ) + as.numeric( SODIUM.q.flag ) +
              as.numeric( T.RED.PROC.MTS.q.flag ) + as.numeric( SUG.SWT.BEV.q.flag ) ) ) %>%
  summarize( mean = mean( DASH, na.rm = T ), min = min( DASH, na.rm = T ), max = max( DASH, na.rm = T ), 
             median = median( DASH, na.rm = T ), missing = sum( is.na( DASH ) ) )

# visualize distribution
hist( fg.dash$DASH, breaks = 20 )

# ---------------------------------------------------------------------------------------------------------------------------------------------------------



### AHEI-2010 Score Computation ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
( fg.ahei <- fg.dash %>%
    
    # veg
    mutate( VEG.AHEI = ifelse( T.VEG == 0, 0,
                               ifelse( T.VEG > 0 & T.VEG <  5, 2*T.VEG,
                                       ifelse( T.VEG >= 5, 10, NA ) ) ) ) %>%
    # fruit
    mutate( FRUIT.AHEI = ifelse( T.FRUIT == 0, 0,
                                 ifelse( T.FRUIT <  4 & T.FRUIT > 0, 2.5*T.FRUIT,
                                         ifelse( T.FRUIT >= 4, 10, NA ) ) ) ) %>%
    # whole grains
    mutate( WHGRN.AHEI = ifelse( WHGRN == 0, 0,
                                 ifelse( SEX == 1 & WHGRN <  90 & WHGRN > 0, ( 10 / 90 )*WHGRN,
                                         ifelse( SEX == 2 & WHGRN <  75 & WHGRN > 0, ( 10 / 75 )*WHGRN,
                                                 ifelse( SEX == 2 & WHGRN >= 75, 10,
                                                         ifelse( SEX == 1 & WHGRN >= 90, 10, NA ) ) ) ) ) ) %>%
    # sugar sweetened beverages
    mutate( SWT.BEV.AHEI = ifelse( T.FRUITJUICE.SWT.BEV == 0, 10,
                                   ifelse( T.FRUITJUICE.SWT.BEV > 0 & T.FRUITJUICE.SWT.BEV <  1, 10-10*( T.FRUITJUICE.SWT.BEV ),
                                           ifelse( T.FRUITJUICE.SWT.BEV >= 1, 0, NA ) ) ) ) %>%
    # legumes
    mutate( LEGUME.AHEI = ifelse( T.LEGUME == 0, 0,
                                  ifelse( T.LEGUME <  1 & T.LEGUME > 0, 10*T.LEGUME,
                                          ifelse( T.LEGUME >= 1, 10, NA ) ) ) ) %>%
    # red and processed meats
    mutate( RED.PROCMT.AHEI = ifelse( T.RED.PROC.MTS == 0, 10,
                                      ifelse( T.RED.PROC.MTS > 0 & T.RED.PROC.MTS <  1.5, ( 10/1.5 )*T.RED.PROC.MTS,
                                              ifelse( T.RED.PROC.MTS >= 1.5, 0, NA ) ) ) ) %>%
    # trans fats
    mutate( TTRANSF = T161 + T181 + T201 + ( CLA/1000 ) ) %>%
    mutate( TRANS.FAT.EN = ( ( TTRANSF*9 )/CALOR ) ) %>%
    mutate( TRANS.FAT.AHEI = ifelse( TRANS.FAT.EN >= 0.04, 0,
                                     ifelse( TRANS.FAT.EN <  0.04 & TRANS.FAT.EN > 0.005, 10-( TRANS.FAT.EN-0.005 )/( 0.035*10 ),
                                             ifelse( TRANS.FAT.EN <= 0.005, 10, NA ) ) ) ) %>%
    # omega-3 fatty acids
    mutate( OMEGA.AHEI = ifelse( OMEGA == 0, 0,
                                 ifelse( OMEGA >= 0.25, 10,
                                         ifelse( OMEGA > 0 & OMEGA <  0.25, OMEGA*40, NA ) ) ) ) %>%
    
    # PUFAs
    mutate( POLY.ENERGY = POLY*9/CALOR ) %>%
    mutate( PUFA.AHEI = ifelse( POLY.ENERGY <= 0.02, 0,
                                ifelse( POLY.ENERGY > 0.02 & POLY.ENERGY <  0.10, ( ( POLY.ENERGY-0.02 )/0.08*10 ),
                                        ifelse( POLY.ENERGY >= 0.10, 10, NA ) ) ) ) %>%
    
    # alcohol
    mutate( ALC.SERVINGS = R.WINE + W.WINE + BEER + BEER.LITE + LIQ ) %>%
    mutate( ALC.AHEI = ifelse( SEX == 1 & ALC.SERVINGS >= 3.5, 0,
                               ifelse( SEX == 2 & ALC.SERVINGS >= 2.5, 0,
                                       ifelse( SEX == 1 & ALC.SERVINGS <= 2 & ALC.SERVINGS >= 0.5, 10,
                                               ifelse( SEX == 2 & ALC.SERVINGS <= 1.5 & ALC.SERVINGS >= 0.5, 10,
                                                       ifelse( SEX == 1 & ALC.SERVINGS <  0.5 & ALC.SERVINGS >= 0, 5,
                                                               ifelse( SEX == 2 & ALC.SERVINGS <  0.5 & ALC.SERVINGS >= 0, 5,
                                                                       ifelse( SEX == 1 & ALC.SERVINGS > 2 & ALC.SERVINGS <  3.5, 5,
                                                                               ifelse( SEX == 2 & ALC.SERVINGS > 1.5 & ALC.SERVINGS <  2.5, 5, NA ) ) ) ) ) ) ) ) ) %>%
    # sodium
    mutate( SOD.DEC = as.numeric( quant_cut( var = 'SODIUM',
                                             df = fg.dash, x = 11 ) )-1 ) %>%
    mutate( SODIUM.AHEI = 10-SOD.DEC ) %>%
    
    # final score computation
    mutate( AHEI = SODIUM.AHEI + ALC.AHEI + PUFA.AHEI + OMEGA.AHEI + TRANS.FAT.AHEI + RED.PROCMT.AHEI +
              LEGUME.AHEI + SWT.BEV.AHEI + WHGRN.AHEI + FRUIT.AHEI + VEG.AHEI ) ) %>%
  
  summarize( mean = mean( AHEI, na.rm = T ), min = min( AHEI, na.rm = T ), max = max( AHEI, na.rm = T ), median = median( AHEI, na.rm = T ),
             missing = sum( is.na( AHEI ) ) )

# visualize distribution
hist( fg.ahei$AHEI, breaks = 20 )


# ---------------------------------------------------------------------------------------------------------------------------------------------------------





### aMED Score Computation
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


# monounsaturated fat to saturated fat ratio needed for computation
fg.amed <- fg.ahei %>%
  mutate( MFAT.SFAT.RATIO = MONFAT/SATFAT )

# use for-loop to create binary ( median split ) variablesfor the 9 food groups
# collecting names so that a do loop can be used

# column locations for key variables
amed.grps <- which( colnames( fg.amed ) %in% c( "WHGRN", "T.RED.PROC.MTS", "LEGUME.BEAN", "LEGUME.NUTS",
                                                "T.FRUIT", "T.VEG", "TOTAL.FISH", "MFAT.SFAT.RATIO" ) ) 

cont.var.names.amed <- names( fg.amed[ , amed.grps ] )  # food group names
cut.var.names.amed <- paste0( cont.var.names.amed, ".q" )



#create median variables and subtract 1 to scale to 0 or 1

for ( i in 1:length( cont.var.names.amed ) ){
  fg.amed[ , cut.var.names.amed[i] ] <- as.numeric( quant_cut( var = cont.var.names.amed[i], df = fg.amed, x = 2 ) )-1
  
}

# generate final index score
( fg.amed <- fg.amed %>%
    
    # alcohol groups rule
    mutate( ALC.AMED = ifelse( ALCO >= 5 & ALCO <= 15, 1, 0 ) ) %>%
    
    mutate( aMED = ALC.AMED + WHGRN.q + abs( T.RED.PROC.MTS.q-1 ) + LEGUME.BEAN.q + LEGUME.NUTS.q +
              T.FRUIT.q + T.VEG.q + TOTAL.FISH.q + MFAT.SFAT.RATIO.q ) ) %>%
  summarize( mean = mean( aMED ), min = min( aMED ), max = max( aMED ), median = median( aMED ),
             missing = sum( is.na( aMED ) ) )

# visualize distribution
hist( fg.amed$aMED, breaks = 20 )
# ---------------------------------------------------------------------------------------------------------------------------------------------------------



### Low-Carbohydrate Scores Computation ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
fg.lc <- fg.amed %>%
  
  mutate( PERC.CARB = ( CARBO*4 ) / CALOR ) %>%
  
  # percent calories from fat ( all fat ) and protein ( all protein )
  mutate( PERC.FAT = ( TFAT*9 ) / CALOR ) %>%
  mutate( PERC.PROT = ( PROT*4 ) / CALOR ) %>%
  
  # percent calories from fat ( animal fat ) and protein ( animal protein )
  mutate( PERC.AFAT = ( AFAT*9 ) / CALOR ) %>%
  mutate( PERC.APROT = ( APROT*4 ) / CALOR ) %>%
  
  # percent calories from fat ( vegetable fat ) and protein ( vegetable protein )
  mutate( PERC.VFAT = ( VFAT*9 ) / CALOR ) %>%
  mutate( PERC.VPROT = ( VPROT*4 ) / CALOR )



# categorize into deciles and then compute final score
lc.grps <- which( colnames( fg.lc ) %in% c( 'PERC.PROT', 'PERC.FAT', 'PERC.CARB', 
                                            'PERC.APROT', 'PERC.AFAT', 'PERC.VPROT', 'PERC.VFAT' ) )


cont.var.names.lc <- names( fg.lc[ , lc.grps ] )     # food group names
cut.var.names.lc <- paste0( cont.var.names.lc, ".q" )



#create decile variables and subtract 1 to scale to 0 ( minimum ) or 10 ( max )

for ( i in 1:length( cont.var.names.lc ) ){
  fg.lc[ , cut.var.names.lc[i] ] <- as.numeric( quant_cut( var = cont.var.names.lc[i], df = fg.lc, x = 11 ) )-1
  
}

# final score computation
( fg.lc <- fg.lc %>%
    
    # inverse score is assigned for carbohydrate intake
    mutate( PERC.CARB.q = 10-PERC.CARB.q ) %>%
    
    # general low carbohydrate score
    mutate( low.carb = PERC.FAT.q + PERC.PROT.q + PERC.CARB.q ) %>%
    
    # animal-based low carbohydrate score
    mutate( a.low.carb = PERC.AFAT.q + PERC.APROT.q + PERC.CARB.q ) %>%
    
    # plant-based general low carbohydrate score
    mutate( v.low.carb = PERC.VFAT.q + PERC.VPROT.q + PERC.CARB.q ) ) %>%
  
  summarize( mean = mean( low.carb ), min = min( low.carb ), max = max( low.carb ), median = median( low.carb ),
             missing = sum( is.na( low.carb ) ),
             mean.a = mean( a.low.carb, na.rm = T ), min.a = min( a.low.carb, na.rm = T  ), max.a = max( a.low.carb, na.rm = T  ), median.a = median( a.low.carb, na.rm = T  ),
             missing.a = sum( is.na( a.low.carb ) ),
             mean.v = mean( v.low.carb ), min.v = min( v.low.carb ), max.v = max( v.low.carb ), median.v = median( v.low.carb ),
             missing.v = sum( is.na( v.low.carb ) ) ) 

# 45 subjects missing plant based low-carbohydrate index scores (seems that they are missing `PROT.V` in the raw data)

# visualize distributions
hist( fg.lc$low.carb, breaks = 20 )
hist( fg.lc$a.low.carb, breaks = 20 )   # a bit of left skew
hist( fg.lc$v.low.carb, breaks = 20 )

# ---------------------------------------------------------------------------------------------------------------------------------------------------------



### Food Groups for Individual Food Groups Analysis
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

# 114 individual food items we will group into food groups
fds.raw <- "AVOCADO RAISGRP PRUN.J BAN A.J GRFRT.J O.J O.J.CALC OTH.F.J ICE.LET ROM.LET CELERY TOM.J TOFU SALSA CARROT.R CARROT.C YOG YOG.FR.NONFAT ICE.CR YOG.PLAIN WALNUTS NUTS OTH.NUTS HOTDOG PROC.MTS CHIX.DOG CHIX.NO CHIX.SK EGG.BEAT EGGS.OMEGA EGGS ENG.MUFF MUFFIN BR.RICE WH.RICE PASTA GRAIN PANCAK PIZZA.02 POT TUNA COT.CH CR.CH OTH.CH PRUN A.SCE PEACHES STRAW TOM BROC CABB.COLE CAUL BRUSL CORN PEAS MIX.VEG BEANS ZUKE SWT.POT SPIN.CKD SPIN.RAW KALE YEL.SQS ONIONS ONIONS1 PEPPERS BACON HAMB XTRLEAN.HAMBURG SAND.BF.HAM PORK BEEF02 dk.fish oth.fish fr.fish.kids shrimp.ckd CANT PUNCH APPLE ORANG GRFRT SOYMILK.FORT SKIM.KIDS MILK2 MILK P.BU COKE OTH.CARB LOCALNO LOCALCAF T.BU.CAN.LOL BU MARGARINE COLD.CER CKD.CER CRAX TORTILLAS blue apricot tom.s st.beans BOLOGNA CHIX.NO.SAND R.WINE W.WINE BEER BEER.LITE LIQ GARLIC2 OLIVE.OIL WH.BR RYE.BR DK.BR"

# text process them
fds <- toupper( fds.raw )
fds <- unlist( str_split( fds, ' ' ) )


# grouping scheme will follow the conventions used in the FPED:
# URL: https://www.ars.usda.gov/ARSUserFiles/80400530/pdf/fped/FPED_1718.pdf
# page 14


fg.fg <- fg.lc %>%
  mutate( F.TOTAL = BLUE + APRICOT + CANT + APPLE + ORANG + GRFRT + PRUN + A.SCE + PEACHES +
            STRAW + AVOCADO + RAISGRP + BAN,
          F.CITMLB = BLUE + CANT + ORANG + GRFRT + STRAW,
          F.OTHER =  APRICOT + APPLE + PRUN + A.SCE + PEACHES +
            AVOCADO + RAISGRP + BAN,
          F.JUICE = A.J + GRFRT.J + O.J + O.J.CALC + PRUN.J + OTH.F.J + TOM.J,
          V.TOTAL = TOM.S + TOM + BROC + CABB.COLE + CAUL + BRUSL + CORN + MIX.VEG + ZUKE + SWT.POT +
            SPIN.CKD + SPIN.RAW + KALE + YEL.SQS + ONIONS + ONIONS1 + PEPPERS + ICE.LET +
            ROM.LET + CELERY + TOM.J + SALSA + CARROT.R + CARROT.C + GARLIC2,
          V.DRKGR = BROC + SPIN.CKD + SPIN.RAW + KALE,
          V.REDOR.TOMATO = TOM.S + TOM + TOM.J + SALSA,
          V.REDOR.TOTAL = SWT.POT + PEPPERS + CARROT.R + CARROT.C +TOM.S + TOM + SALSA,
          V.STARCHY.TOTAL = CORN + PEAS + POT,
          V.STARCHY.POTATO = POT,
          V.STARCHY.OTHER = CORN + PEAS,
          V.OTHER = CABB.COLE + CAUL + BRUSL + MIX.VEG + ZUKE + ICE.LET + ONIONS + 
            ONIONS1 + ROM.LET + CELERY + GARLIC2 + PEPPERS,
          V.LEGUMES = ST.BEANS + BEANS + PEAS + TOFU,
          PF.TOTAL = TOFU + SOYMILK.FORT + EGG.BEAT + EGGS.OMEGA + EGGS +
            TUNA + DK.FISH + OTH.FISH + FR.FISH.KIDS + SHRIMP.CKD + BOLOGNA + BACON + HOTDOG + PROC.MTS + 
            CHIX.DOG + HAMB + XTRLEAN.HAMBURG + SAND.BF.HAM + PORK + BEEF02 + P.BU + WALNUTS + NUTS +
            OTH.NUTS + LIVER + CHIX.LIVER,
          PF.MPS.TOTAL = TUNA + DK.FISH + OTH.FISH + FR.FISH.KIDS + SHRIMP.CKD + BOLOGNA + BACON + HOTDOG + PROC.MTS + 
            CHIX.DOG + HAMB + XTRLEAN.HAMBURG + SAND.BF.HAM + PORK + BEEF02,
          PF.MEAT = HAMB + XTRLEAN.HAMBURG + PORK + BEEF02,
          PF.CUREMEAT = BOLOGNA + BACON + HOTDOG + PROC.MTS + CHIX.DOG,
          PF.ORGAN = LIVER + CHIX.LIVER,
          PF.POULT = CHIX.NO.SAND + CHIX.SK + CHIX.NO + CHIX.DOG,
          # will need to have a total fish category since we do not have enough information to divide into 
          # high n-3 and low n-3
          PF.SEAFD = TUNA + DK.FISH + OTH.FISH + FR.FISH.KIDS + SHRIMP.CKD,
          PF.EGGS = EGG.BEAT + EGGS.OMEGA + EGGS,
          PF.SOY =  SOYMILK.FORT + TOFU,
          PF.NUTS = P.BU + WALNUTS + NUTS + OTH.NUTS,
          D.TOTAL = MILK2 + SKIM.KIDS + YOG.FR.NONFAT + ICE.CR + YOG + YOG.PLAIN + MILK + CREAM +
            SKIM.KIDS,
          D.YOGURT = YOG + YOG.PLAIN,
          D.CHEESE = COT.CH + CR.CH + OTH.CH,
          OILS = OLIVE.OIL,
          SOLID.FATS = BU + MARGARINE + T.BU.CAN.LOL,
          ADD.SUGARS = ADDSUG,
          A.DRINKS =  ALCO / 14,  # using 14 g of ETOH per standard drink: PMID: 29723083
          G.REFINED = COLD.CER + CKD.CER + WH.BR + ENG.MUFF + MUFFIN + WH.RICE + PASTA + PANCAK + CRAX + PIZZA.02,
          G.WHOLE = WHGRN / 16 )  # using 16 g of whole grains as a serving (Whole Grain Council)
# REF: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7236605/

# ---------------------------------------------------------------------------------------------------------------------------------------------------------



### Select Columns to Keep ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

colnames( fg.fg )

# specify column indices for LQ variables, diet score variables, and food group variables
keep.these <-  c( which( colnames( fg.fg ) %in% 
                           c( "idnum", "Age.at.Diagnosis", "SEX", "RACE", "STAGE.B", "dissite", "smoker", "drinker",
                              "HPV.STATUS", "ACE.OVERALL.SCORE", "BMI", "CALOR", "AHEI", "aMED", "DASH", "low.carb", "a.low.carb", 
                              "v.low.carb", "F.TOTAL", "F.CITMLB", "F.OTHER", "F.JUICE", "V.TOTAL",
                              "V.DRKGR", "V.REDOR.TOMATO", "V.REDOR.TOTAL", "V.STARCHY.TOTAL", "V.STARCHY.POTATO",
                              "V.STARCHY.OTHER", "V.OTHER", "V.LEGUMES", "PF.TOTAL", "PF.MPS.TOTAL",
                              "PF.MEAT", "PF.CUREMEAT", "PF.ORGAN", "PF.POULT", "PF.SEAFD", "PF.EGGS", "PF.SOY", "PF.NUTS",
                              "D.TOTAL", "D.YOGURT", "D.CHEESE", "OILS", "SOLID.FATS", "ADD.SUGARS", "A.DRINKS", "G.REFINED",
                              "G.WHOLE", "IFN", "IL6", "IL17", "IL10", "IL8", "VEGF", "GRO", "TGF", "HGF", "TNF" ) ) )

# select
dat <- fg.fg[ , keep.these ]
names( dat ) # check
# ---------------------------------------------------------------------------------------------------------------------------------------------------------




### Check on Missing Data and Label Complete Cases for Analysis ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

sapply( dat, function( x ) sum( is.na( x ) ) )
# 45 missing plant-based low carb index scores (they are missing the VPROT column)
# 45 missing added sugars group variable
# 1 missing solid fats and total dairy
# and 1 missing all the covariates and DASH/AHEI


# examine who the one individual (or more) missing covariate is:
dat %>% filter( is.na( BMI ) | is.na( dissite ) | is.na( RACE ) ) #id: 2408, 2283, 1908, 1603

# examine individual missing AHEI/DASH/ADD.SUGARS
dat %>% filter( is.na( AHEI ) | is.na( DASH ) ) # id: c( 2283, 2342 )

# PLAN: remove those individuals discovered above and label those 45 subjects missing plant-based low carb index
( dat.b <- dat %>%
    filter( !is.na( AHEI ) | !is.na( DASH ) | !is.na( dissite ) | !is.na( RACE ) ) ) %>% # remove those
    
    summarize( n = n(), unique = length( unique( idnum ) ) ) 

# remove subjects with tumors not in the larynx, oropharynx, hypopharnx, or oral cavity
( dat.c <- dat.b %>%
  filter( dissite %in% c( 1:4 ) ) ) %>%
summarize( n = n( ), unique = length( unique( idnum ) ), ncol = ncol(.) ) 
  
# create a variable to identify observations with complete observed data
( ( dat.d <- dat.c %>%
    mutate( cc = factor( ifelse( is.na( v.low.carb ) | is.na( ADD.SUGARS ) | is.na( DASH ), 0, 1 ) ) ) ) %>%
    count( cc ) )
# 106 with complete diet quality index scores, 41 missing plant-based low carbohydrate

  
# check on duplicate individual
dat.d %>%
  filter( duplicated( idnum ) ) # id: 2735

dat.d %>%
  filter( idnum == 2735 ) %>%
  select( AHEI, DASH, aMED, low.carb, a.low.carb, v.low.carb )

# try and remove them with `distinct` (will only work if they have the same values in all columns across both rows)
(dat.e <- dat.d %>% 
    distinct() ) %>% 
  summarize( n = n( ), unique = length( unique( idnum ) ), ncol = ncol(.) )  # no success

# looks like this individual had blood drawn twice. We will use their first draw and we can do a sensitivity analysis
# later to see if using the data from the other draw affects the results

( dat.e <- dat.e %>% filter( !( idnum == 2735 & IL8 == 35 ) ) )%>%
  summarize( n = n( ), unique = length( unique( idnum ) ), ncol = ncol(.) )  # success
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
  


### Recode Categorical Variables for Table Presentation ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
(dat.f <- dat.e %>% 
  rename_all( tolower ) %>%  # make all column names lower case for consistency
  rename( age = age.at.diagnosis ) %>%
   
# recode categorical variables #
  mutate( race.binary = factor( ifelse( race == 1, 'Non-Hispanic White',
                                            ifelse( race!= 1, 'Other', NA ) ) ),
          stage.binary = factor( ifelse( stage.b %in% c( 0, 1, 2, 3 ), 'Stages 0-3',
                                             ifelse( stage.b %in% c( 4 ), 'Stage 4', NA ) ) ),
          sex = factor( ifelse( sex == 1, "Male", ifelse( sex == 2, "Female", NA ) ) ),
          ace.binary = factor( ifelse( ace.overall.score %in% c( 0, 1 ), 'None/Mild',
                                           ifelse( ace.overall.score %in% c( 2, 3 ), 'Moderate/Severe', NA ) ) ),
          tum.site = factor( ifelse( dissite %in% c( 1, 4 ), "Larynx",         # collapse hypopharynx and larynx into single group (Dr. Greg Wolf suggested this)
                                 ifelse( dissite == 3, "Oropharynx",      
                                         ifelse( dissite ==2, "Oral Cavity", NA ) ) ) ),
          smoker = factor( ifelse( smoker == 0, "Never",       
                             ifelse( smoker == 1, "Current",      
                                     ifelse( smoker == 2, "Former", NA ) ) ) ),
          drinker = factor( ifelse( drinker == 0, "Never",        
                           ifelse( drinker == 1, "Current",    
                                   ifelse( drinker == 2, "Former", NA ) ) ) ),
          hpv.status = factor( ifelse( hpv.status == 0, "negative",
                               ifelse( hpv.status == 1, "positive",
                                       ifelse( hpv.status == 2, "equivocal", NA ) ) ) ) ) %>%
  relocate( race.binary, stage.binary, ace.binary, tum.site, .after = hpv.status ) ) %>%  # move new columns to location with other clinical covariates
  #Save Working Dataset
  saveRDS( "02-Data-Wrangled/02-fg-index-scores.rds" ) 
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
