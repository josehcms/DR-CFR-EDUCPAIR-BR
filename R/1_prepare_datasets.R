################################################################################
### DR Paper:  Educational Pairings and Fertility Decline in Brazil
### R Script 1: read and process raw IPUMS data files, adjust variables and 
###             prepare database for anlysis
### Author: Jose H C Monteiro da Silva
### Last Update: 2022-01-04
################################################################################

### 1. Housekeeping  #----------------------------------------------------------
rm( list = ls( ) )
graphics.off( )

require( data.table ); require( dplyr ); require( ipumsr )

# Dictionary for Brazilian regions recodification 
reg_dict <- 
  c(
    '1' = 'North-Northeast',
    '2' = 'North-Northeast',
    '3' = 'South-Southeast',
    '4' = 'South-Southeast',
    '5' = 'Midwest'
  )
################################################################################

### 2. Prepare functions to process raw IPUMS data #----------------------------

## 2.1 Match partners 
match_func <- 
  function( ipums_raw ){
    
    dat <- 
      ipums_raw %>%
      as.data.table %>%
      copy %>%
      .[,
        list(
          REGNBR  = reg_dict[ paste0( substr( GEO1_BR, 4, 4 ) )  ] ,
          YEAR,
          SERIAL,
          RELATE  = RELATE %>% paste0 %>% as.numeric,
          RELATED = RELATED %>% paste0 %>% as.numeric,
          AGE = AGE %>% paste0 %>% as.numeric %>% floor,
          SEX = SEX %>% paste0 %>% as.numeric,
          MARST = MARST %>% paste0 %>% as.numeric,
          CHBORN = CHBORN %>% paste0 %>% as.numeric %>% floor,
          EDATTAIN,
          PERWT
        ) ]

    
    # first match those couples directly related to household head, 
    # i.e. male or female are household heads
    match_dat1 <- 
      merge(
        # males list:
        dat[ SEX == 1 & RELATE < 3 & RELATED < 2300 & MARST == 2,
             list(
               Region   = REGNBR,
               Year     = YEAR,
               HouseID  = SERIAL,
               AgeMale  = AGE,
               EducMale = EDATTAIN
             ) ],
        # females list:
        dat[ AGE %in% seq( 40, 69 ) & 
               SEX == 2 & RELATE < 3 & RELATED < 2300 & MARST == 2,
             list(
               Region     = REGNBR,
               Year       = YEAR,
               HouseID    = SERIAL,
               AgeFemale  = AGE,
               EducFemale = EDATTAIN,
               Parity     = CHBORN,
               SampWeight = PERWT
             )],
        
        by = c( 'Year', 'Region', 'HouseID' )
      )
    
    # match couples whose members are not household heads, i.e, parents
    ids_match_dat2 <- 
      dat[ 
        RELATED %in% c( 4200, 4210, 4220 ),
        .N,
        .( SERIAL, RELATED, MARST ) 
      ] %>%
      .[ N == 2 & MARST == 2,
         SERIAL
      ]
    
    match_dat2 <- 
      merge(
        # males list:
        dat[ SEX == 1 & MARST == 2 & RELATED %in% c( 4200, 4210, 4220 ),
             list(
               Region   = REGNBR,
               Year     = YEAR,
               HouseID  = SERIAL,
               AgeMale  = AGE,
               EducMale = EDATTAIN,
               RELATED
             ) ],
        # females list:
        dat[ AGE %in% seq( 40, 69 ) & 
               SEX == 2 & 
               MARST == 2 &  
               SERIAL %in% ids_match_dat2 & 
               RELATED %in% c( 4200, 4210, 4220 ),
             list(
               Region     = REGNBR,
               Year       = YEAR,
               HouseID    = SERIAL,
               AgeFemale  = AGE,
               EducFemale = EDATTAIN,
               Parity     = CHBORN,
               SampWeight = PERWT,
               RELATED
             ) ],
        
        by = c( 'Year', 'Region', 'HouseID', 'RELATED' )
      ) %>%
      .[ , - 'RELATED' ]
    
    # bind both matched datasets
    match_dat <- 
      rbind( match_dat1, match_dat2 )  %>%
      # correct implausible parities and set cohorts for analysis
      .[ , 
         `:=`(
           # adjust sample weights
           SampWeight     = as.numeric( SampWeight ) / 100,
           # compute birth cohorts
           FemCohort   = Year - AgeFemale,
           # compute 5-year group cohorts
           FemCohort5  = cut( ( Year - AgeFemale ), 
                              breaks = c( -Inf, 
                                          seq( 1925, 1970, 5 ),
                                          Inf ),
                              labels = c( '< 1925',
                                          paste0( seq( 1925, 1965, 5 ) ),
                                          '> 1969' ),
                              right = FALSE,
                              include.lowest = TRUE ) %>% paste0,
           # compute 15-year group cohorts
           FemCohort15 = cut( ( Year - AgeFemale ), 
                              breaks = c( -Inf, 
                                          seq( 1925, 1970, 15 ),
                                          Inf ),
                              labels = c( '< 1925',
                                          paste0( seq( 1925, 1965, 15 ) ),
                                          '> 1969' ),
                              right = FALSE,
                              include.lowest = TRUE ) %>% paste0,
           # adjust unexpected parities                 
           ParityCor = ifelse( Parity > 30,
                               NA,
                               ifelse( Parity > ( 2 / 3 * AgeFemale - 8 ) & AgeFemale > 57, 
                                       30,
                                       ifelse( Parity > ( 2 / 3 * AgeFemale - 8 ) & AgeFemale < 58,
                                               trunc( 2 / 3 * AgeFemale - 8 ),
                                               Parity
                                       )
                               )
           ),
           # flag adjusted parities               
           ParityFlag = ifelse( Parity > 30,
                                NA,
                                ifelse( Parity > ( 2 / 3 * AgeFemale - 8 ) & AgeFemale > 57, 
                                        1,
                                        ifelse( Parity > ( 2 / 3 * AgeFemale - 8 ) & AgeFemale < 58,
                                                1,
                                                0
                                        )
                                )
           )
         ) ]

    # return dataset
    return( match_dat )
  }

## 2.2 All women - in union or not
allwomen_func <- 
  function( ipums_raw ){
   
     dat <- 
      ipums_raw %>%
      as.data.table %>%
      copy %>% 
      .[,
        list(
          REGNBR  = reg_dict[ paste0( substr( GEO1_BR, 4, 4 ) ) ],
          YEAR,
          SERIAL,
          RELATE  = RELATE %>% paste0 %>% as.numeric,
          RELATED = RELATED %>% paste0 %>% as.numeric,
          AGE = AGE %>% paste0 %>% as.numeric %>% floor,
          SEX = SEX %>% paste0 %>% as.numeric,
          MARST = MARST %>% paste0 %>% as.numeric,
          CHBORN = CHBORN %>% paste0 %>% as.numeric %>% floor,
          EDATTAIN,
          PERWT
        ) ] %>%
      .[ AGE %in% seq( 40, 69 ) & SEX == 2,
         list(
           Region     = REGNBR,
           Year       = YEAR,
           HouseID    = SERIAL,
           AgeFemale  = AGE,
           EducFemale = EDATTAIN,
           Parity     = CHBORN,
           SampWeight = PERWT
         ) ] %>%
      .[ , 
         `:=`(
           # adjust sample weights
           SampWeight = as.numeric( SampWeight ) / 100,
           # compute birth cohorts
           FemCohort = Year - AgeFemale,
           # compute 5-year group cohorts
           FemCohort5 = cut( ( Year - AgeFemale ),
                             breaks = c( -Inf,
                                         seq( 1925, 1970, 5 ),
                                         Inf ),
                             labels = c( '< 1925',
                                         paste0( seq( 1925, 1965, 5 ) ),
                                         '> 1969' ),
                             right = FALSE,
                             include.lowest = TRUE ) %>% paste0,
           # compute 15-year group cohorts
           FemCohort15 = cut( ( Year - AgeFemale ), 
                              breaks = c( -Inf, 
                                          seq( 1925, 1970, 15 ),
                                          Inf ),
                              labels = c( '< 1925',
                                          paste0( seq( 1925, 1965, 15 ) ),
                                          '> 1969' ),
                              right = FALSE,
                              include.lowest = TRUE ) %>% paste0,
           # adjust unexpected parities                 
           ParityCor = ifelse( Parity > 30,
                               NA,
                               ifelse( Parity > ( 2 / 3 * AgeFemale - 8 ) & 
                                         AgeFemale > 57, 
                                       30,
                                       ifelse( Parity > 
                                                 ( 2 / 3 * AgeFemale - 8 ) &
                                                 AgeFemale < 58,
                                               trunc( 2 / 3 * 
                                                        AgeFemale - 8 ),
                                               Parity
                                       )
                               )
           ),
           # flag adjusted parities               
           ParityFlag = ifelse( Parity > 30,
                                NA,
                                ifelse( Parity > 
                                          ( 2 / 3 * AgeFemale - 8 ) & 
                                          AgeFemale > 57, 
                                        1,
                                        ifelse( Parity > 
                                                  ( 2 / 3 * AgeFemale - 8 ) & 
                                                  AgeFemale < 58,
                                                1,
                                                0
                                        )
                                )
           )
         ) ]
    
    
    # return dataset
    return( dat )
  }

################################################################################

### 3. Load IPUMS Data #--------------------------------------------------------

ddi <- 
  read_ipums_ddi( "DATA/IPUMS/xxxxxxxxx.xml" ) # include path to .xml file

ipums_dat <- 
  read_ipums_micro( ddi )

################################################################################

### 4. Process raw data #-------------------------------------------------------

datBRAll <- allwomen_func( ipums_dat )
datBRMatched <- match_func( ipums_dat )

################################################################################

### 5. Save data #--------------------------------------------------------------

save( datBRMatched, datBRAll,
      file = 'DATA/PROCESSED/prepared_data_br.RData' )

################################################################################

### 6. The End
