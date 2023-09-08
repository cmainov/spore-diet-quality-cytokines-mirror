###---------------------------------------------------
###   03-ADJUSTMENT FOR TOTAL ENERGY INTAKE
###---------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------------------------------------
# 
# In this script, we will Adjust the computed diet index scores and food groups variables for total energy using the 
# Willet Nutrient Residual Model/Method
# 
# INPUT DATA FILE: "/02-Data-Wrangled/02-fg-index-scores.rds"
#
# OUTPUT FILES: 
#
# Resources: https://watermark.silverchair.com/1220s.pdf?token=AQECAHi208BE49Ooan9kkhW_Ercy7Dm3ZL_9Cf3qfKAc485ysgAAAsgwggLEBgkqhkiG9w0BBwagggK1MIICsQIBADCCAqoGCSqGSIb3DQEHATAeBglghkgBZQMEAS4wEQQMmPKHNsCpPLTLD87PAgEQgIICe3Nf-K5YGKrXlGQBAaqPTRnfVGSe3vdpePY4qb6Rn7a0L47Hs4WRkoblhuLyHCe9CO8xr5v0FS8dbmjnaLnAo-9T7DNQZx42mKYAEQFQR8b1T6C2JssLuD8bwhjvkU-zrbqZVJzipg-yxc-k5asJau9KZmY1bw4Oj5a2uDvE2O36yQYQIYQmH7nNJSSQQOaIyKjqVBc9115kL44amXdf-FpsPKZ8y8wT0HE-NkDBpE--i22P_XnLB0EHP5GIZQtMlOHM4hgOBDMk6tTinNm24q1qBaD3Q9cbk9-MykKHkDEoj_37RNk6glZ3MgETwMtvtudCkbP96FVoZzvwhwcfdT_F4LBGJn-jFc1arWf5CoE43rE8TGIfOzznbjpcyJUl19g84Vga_lGCGJu1oB5Rq_FAi_KSJS0aJxbXphT9FKZJ9bbqSoT9cH7zYOMQ7DeCCItEPnn_Fl95SwvLiAt6j2-fl1LpY95opuJG5lyqP_brXbso1RFfY5L2v2RF8O3JQrbVjqNcA8if6CGYW4NXg1e4LVyUibGjfDSs34KoXDkPJe5ukn2L9cO6twsCMYFnsixZm0Y3d6TwSvyG_tg3rT2PcESguV6SbPigbhRgpn3sOM32MWLzJuI6ZYGh4_sJCvpJdQvDK4pHDQVUMrLxqQ891arUWnQ6KAt1kgn4mG4ZooCMfBMV5sxFCX1Qsw9IzaXfWFgUDslPZJUZdmxksqdDr3t9gVIakqmPui-IOsFKO14s_Pk6x1D15PbK5uiY4i3jzftIFjC22fsyZ8NPTJdCJqifo_fVWERkG9eeRZ54vdUGTW39m2y1KEoUHjPTkMBz5qbDdBCvkzYZ
# # Willet paper on adjustment for total energy in epidemiologic studies
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


library( tidyverse )

# read helper functions into global environment (table-generating helper functions reside in this file)
source( "R/utils.R" )

### Read-in Data ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
( d <- readRDS( "02-Data-Wrangled/02-fg-index-scores.rds" ) )%>%
  summarize( n = n( ), unique = length( unique( idnum ) ), ncol = ncol(.) ) 
# n = 146, 113 columns
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


### Apply Willet Residual Method ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

# examine distribution of caloric intake and diet indices
hist( d$calor, breaks = 30 ) # some right skew
hist( d$ahei, breaks = 30 )  # looks ok
hist( d$amed, breaks = 30 )  # looks ok
hist( d$dash, breaks = 30 )  # looks ok
hist( d$low.carb, breaks = 30 ) # looks ok
hist( d$a.low.carb, breaks = 30 ) # looks ok
hist( d$v.low.carb, breaks = 30 ) # looks ok


## use `energy_residual`, bin into quantiles, and create trend variables ##

# columns to use/manipulate
indices <- c( "ahei", "amed", "dash", "low.carb", "a.low.carb", "v.low.carb" )

# apply functions for energy adjustment, binning, and creating trend variables
dat.int <- energy_residual( dat = d, x = indices, calories = "calor", overwrite = "yes" ) %>%
  bin_patterns( df = ., pattern.cols = indices ) %>%         # bin into q2, q3, q4, and q5
  trend_patterns( df = ., pattern.cols = indices )           # create trend variables for linear trend analysis

# ---------------------------------------------------------------------------------------------------------------------------------------------------------



### Response/Outcome Variable Categorizations ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

# check no. of zeros
nrow( dat.int[ which( dat.int$tnf == 0 ), "tnf" ] ) / nrow( dat.int ) # 0.6506849
nrow( dat.int[ which( dat.int$ifn == 0 ), "ifn" ] ) / nrow( dat.int ) # 0.3972603
nrow( dat.int[ which( dat.int$il6 == 0 ), "il6" ] ) / nrow( dat.int ) # 0.4041096
nrow( dat.int[ which( dat.int$il8 == 0 ), "il8" ] ) / nrow( dat.int ) # 0.006849315
nrow( dat.int[ which( dat.int$il10 == 0 ), "il10" ] ) / nrow( dat.int ) # 0.3082192
nrow( dat.int[ which( dat.int$il17 == 0 ), "il17" ] ) / nrow( dat.int ) # 0.7876712
nrow( dat.int[ which( dat.int$vegf == 0 ), "vegf" ] ) / nrow( dat.int ) # 0.03424658
nrow( dat.int[ which( dat.int$gro == 0 ), "gro" ] ) / nrow( dat.int ) # 0.02054795
nrow( dat.int[ which( dat.int$tgf == 0 ), "tgf" ] ) / nrow( dat.int ) # 0.00
nrow( dat.int[ which( dat.int$hgf == 0 ), "hgf" ] ) / nrow( dat.int ) # 0.00

# decision: those with greater than 50% zeros will be dichotomized, those with at least 
# 30% zeros will be categorized into 0, 0 < value < median, >= median and those with less than 30% zeros
# will be categorized into tertiles


## Categorize response variables accordingly and create summary composite ##
dat.out <- dat.int %>%
    mutate( 
            # 0, 0 < value < median, >= median
            ifn.cat = ifelse( ifn == 0, 1,
                              ifelse( ifn > 0 & ifn < median( ifn, na.rm = T ), 2,
                                      ifelse( ifn >= median( ifn, na.rm = T ), 3, NA ) ) ),
            il6.cat = ifelse( il6 == 0, 1,
                              ifelse( il6 > 0 & il6 < median( il6, na.rm = T ), 2,
                                      ifelse( il6 >= median( il6, na.rm = T ), 3, NA ) ) ),
            il10.cat = ifelse( il10 == 0, 1,
                               ifelse( il10 > 0 & il10 < median( il10, na.rm = T ), 2,
                                       ifelse( il10 >= median( il10, na.rm = T ), 3, NA ) ) ),
            
            
            # tertiles (using manual coding as opposed to `quant_cut` fct due to discrete values for these cytokines which breaks the function)
            il8.cat = ifelse( il8 < quantile( .$il8, 0.33 ), 1,
                              ifelse( il8 >= quantile( .$il8, 0.33 ) & il8 < quantile( .$il8, 0.66 ), 2,
                                      ifelse( il8 >= quantile( .$il8, 0.66 ), 3, NA ) ) ),
            vegf.cat = ifelse( vegf < quantile( .$vegf, 0.33 ), 1,
                               ifelse( vegf >= quantile( .$vegf, 0.33 ) & vegf < quantile( .$vegf, 0.66 ), 2,
                                       ifelse( vegf >= quantile( .$vegf, 0.66 ), 3, NA ) ) ),
            gro.cat = ifelse( gro < quantile( .$gro, 0.33 ), 1,
                              ifelse( gro >= quantile( .$gro, 0.33 ) & gro < quantile( .$gro, 0.66 ), 2,
                                      ifelse( gro >= quantile( .$gro, 0.66 ), 3, NA ) ) ),
            tgf.cat = ifelse( tgf < quantile( .$tgf, 0.33 ), 1,
                              ifelse( tgf >= quantile( .$tgf, 0.33 ) & tgf < quantile( .$tgf, 0.66 ), 2,
                                      ifelse( tgf >= quantile( .$tgf, 0.66 ), 3, NA ) ) ),
            hgf.cat = ifelse( hgf < quantile( .$hgf, 0.33 ), 1,
                              ifelse( hgf >= quantile( .$hgf, 0.33 ) & hgf < quantile( .$hgf, 0.66 ), 2,
                                      ifelse( hgf >= quantile( .$hgf, 0.66 ), 3, NA ) ) ),
            
            # binary-coded
            il17.bin = ifelse( il17 == 0, 0,
                               ifelse( il17 > 0, 1, NA ) ),
            tnf.bin = ifelse( tnf == 0, 0,
                              ifelse( tnf > 0, 1, NA ) ),
            
            # transform cytokines into Z-scores
            ifn.z = ( ifn - mean( .$ifn, na.rm = T ) ) / sd( .$ifn, na.rm = T ),
            il6.z = ( il6 - mean( .$il6, na.rm = T ) ) / sd( .$il6, na.rm = T ),
            il8.z = ( il8 - mean( .$il8, na.rm = T ) ) / sd( .$il8, na.rm = T ),
            il10.z = ( il10 - mean( .$il10, na.rm = T ) ) / sd( .$il10, na.rm = T ),
            il17.z = ( il17 - mean( .$il17, na.rm = T ) ) / sd( .$il17, na.rm = T ),
            vegf.z = ( vegf - mean( .$vegf, na.rm = T ) ) / sd( .$vegf, na.rm = T ),
            gro.z = ( gro - mean( .$gro, na.rm = T ) ) / sd( .$gro, na.rm = T ),
            tgf.z = ( tgf - mean( .$tgf, na.rm = T ) ) / sd( .$tgf, na.rm = T ),
            hgf.z = ( hgf - mean( .$hgf, na.rm = T ) ) / sd( .$hgf, na.rm = T ),
            tnf.z = ( tnf - mean( .$tnf, na.rm = T ) ) / sd( .$tnf, na.rm = T ),
            
            # create final cytokine composite score as sum of Z scores
            cyt.sum = ifn.z + il6.z + il8.z + il17.z + vegf.z + gro.z +
              hgf.z + tnf.z  ) %>% # do not include IL-10 and TGF-beta bc they are anti-inflammatory
  
  select( -( contains( ".z" ) ) ) # remove Z score transformations from final dataset


## Check distribution for composite score ##
hist( dat.out$cyt.sum, breaks = 30 )


# check no. of zeros
sum( dat.out[ which( dat.out$cyt.sum == 0 ), ] ) # no zeros for this variable


# categorize composite into tertiles for proportional odds model and then save
dat.out %>%
  mutate( cyt.sum.cat = as.factor( quant_cut( var = "cyt.sum", 3, df = . ) ) ) %>%
     

# ---------------------------------------------------------------------------------------------------------------------------------------------------------

### Save ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS( "03-Data-Rodeo/03-analytic-data.rds" )
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

