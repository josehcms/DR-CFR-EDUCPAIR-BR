################################################################################
### DR Paper:  Educational Pairings and Fertility Decline in Brazil
### R Script 3: data quality assessment - Appendix A
### Author: Jose H C Monteiro da Silva
### Last Update: 2022-01-04
################################################################################

### 1. Housekeeping and package loading #---------------------------------------
rm(list = ls())
graphics.off()

require(data.table);require(dplyr)

load( 'DATA/PROCESSED/prepared_data_br.RData' )

datBRMatched <- 
  rbind(
    datBRMatched %>% copy %>%
      .[ , Region := 'Brazil' ],
    datBRMatched
  )
################################################################################

### 2. Missing information #----------------------------------------------------

# 2.1 % of missing parity
parityNA_tab <- 
  datBRMatched[ FemCohort15 %in% c( '1925', '1940', '1955' ),
                .( 
                  ParityNA = 
                    round( 100 * 
                             sum( SampWeight[ is.na( ParityFlag ) ] ) / 
                             sum( SampWeight ),
                           2 ) 
                ), 
                .( Region, Year )
                ] %>%
  setorder( Region, Year ) %>%
  dcast( Year ~ Region )
  
# 2.2 % of parity implausible
parityImp_tab <- 
  datBRMatched[ !is.na( ParityFlag ) & 
                  FemCohort15 %in% c( '1925', '1940', '1955' ),
                .( 
                  ParityImp = 
                    round( 100 * 
                             sum( SampWeight[ ParityFlag == 1 ] ) / 
                             sum( SampWeight ),
                           2 ) 
                ), 
                .( Region, Year )
  ] %>%
  setorder( Region, Year ) %>%
  dcast( Year ~ Region )

# 2.3 missing education for either male or female
EducNA_tab <- 
  datBRMatched[ FemCohort15 %in% c( '1925', '1940', '1955' ),
                .( 
                  EducFemNA = 
                    round( 100 * 
                             sum( SampWeight[ ( ! EducFemale %in% 1:4 ) |
                                                ( ! EducMale %in% 1:4 ) ] ) / 
                             sum( SampWeight ),
                           2 ) 
                ), 
                .( Region, Year )
  ] %>%
  setorder( Region, Year ) %>%
  dcast( Year ~ Region )

################################################################################

### 3. Sample size #------------------------------------------------------------

datBRMatched[ , n := 1 ]

SampSizeTab <- 
  datBRMatched[ FemCohort15 %in% c( 1925, 1940, 1955 ) & 
                  ! is.na( ParityFlag ) &
                  EducFemale %in% 1:4 &
                  EducMale %in% 1:4, ] %>%
  copy %>%
  .[ ,.N,
     .( Region, Year, FemCohort5 ) ] %>%
  dcast( FemCohort5 + Year ~ Region )
                

################################################################################

### 4. Prepare tables #---------------------------------------------------------

tab_a1 <- 
  SampSizeTab[ ,
               list(
                 `Cohort` = paste0( as.numeric( paste0( FemCohort5 ) ), 
                                    '-',
                                    as.numeric( paste0( 
                                      ( FemCohort5  ) ) ) + 4 ),
                 `Census Year` = Year,
                 Brazil,
                 Midwest,
                 `North-Northeast`,
                 `South-Southeast`
               ) ] 

tab_a2 <-
  EducNA_tab[ ,
              list(
                `Census Year` = Year,
                Brazil,
                Midwest,
                `North-Northeast`,
                `South-Southeast`
              ) ] 

tab_a3 <-
  parityNA_tab[ ,
              list(
                `Census Year` = Year,
                Brazil,
                Midwest,
                `North-Northeast`,
                `South-Southeast`
              ) ] 

tab_a4 <-
  parityImp_tab[ ,
                 list(
                   `Census Year` = Year,
                   Brazil,
                   Midwest,
                   `North-Northeast`,
                   `South-Southeast`
                 ) ] 

write.table( tab_a1,
             file = 'OUTPUTS/TABLES/taba1.csv',
             row.names = FALSE,
             sep = ';' )

write.table( tab_a2,
             file = 'OUTPUTS/TABLES/taba2.csv',
             row.names = FALSE,
             sep = ';' )

write.table( tab_a3,
             file = 'OUTPUTS/TABLES/taba3.csv',
             row.names = FALSE,
             sep = ';' )

write.table( tab_a4,
             file = 'OUTPUTS/TABLES/taba4.csv',
             row.names = FALSE,
             sep = ';' )

################################################################################

### 5. The End