################################################################################
### DR Paper:  Educational Pairings and Fertility Decline in Brazil
### R Script 2: data analysis and visualization
### Author: Jose H C Monteiro da Silva
### Last Update: 2022-01-04
################################################################################

### 1. Housekeeping and package loading #---------------------------------------
rm(list = ls())
graphics.off()

require(data.table);require(dplyr);require(ggplot2)

dic_educ <- 
  c( '1' = 'LP',
     '2' = 'P',
     '3' = 'S+' )

load( 'DATA/PROCESSED/prepared_data_br.RData' )

datBRMatched <- 
  rbind(
    datBRMatched %>% copy %>%
      .[ , Region := 'Brazil' ], # Include Brazil as a region for analysis
    datBRMatched
  ) %>%
  .[ EducMale %in% 1:4 & 
       EducFemale %in% 1:4 & 
       FemCohort %in% ( 1925 : 1969 ) &
       ParityFlag == 0, ] %>%
  .[ , 
     `:=` (
       # Group Secondary and Tertiary
       EducMale   = ifelse( EducMale == 4, 3, EducMale ), 
       EducFemale = ifelse( EducFemale == 4, 3, EducFemale )
     ) ]

datBRAll <- 
  rbind(
    datBRAll %>% copy %>%
      .[ , Region := 'Brazil' ], # Include Brazil as a region for analysis
    datBRAll
  ) %>%
  .[ EducFemale %in% 1:4 & 
       FemCohort %in% ( 1925 : 1969 ) &
       ParityFlag == 0, ] %>%
  # Group Secondary and Tertiary
  .[ , EducFemale := ifelse( EducFemale == 4, 3, EducFemale ) ]

################################################################################

### 2. Prepare data for plot #--------------------------------------------------
assort_dat <- 
  datBRMatched[ , 
                list(
                  Region,
                  PairingType = ifelse( EducMale > EducFemale,
                                        'Hypergamy',
                                        ifelse( EducFemale > EducMale,
                                                'Hypogamy',
                                                'Homogamy' ) ),
                  FemCohort5 = as.numeric( paste0( FemCohort5 ) ),
                  SampWeight
                ) ] %>%
  .[ , 
     list(
       NPar = sum( SampWeight )
     ),
     .( Region, FemCohort5, PairingType ) ] %>%
  .[ , 
     list(
       PairingType,
       PrevPar = NPar / sum( NPar )
     ),
     .( Region, FemCohort5 )]
################################################################################

### 3. Plot prevalence of educational pairings #--------------------------------

plot_heterogamy_dat <- 
  assort_dat[ PairingType != 'Homogamy' ] %>%
  ggplot() +
  geom_line( aes( x = FemCohort5, y = PrevPar, 
                  color = Region, linetype = Region ),
             size = 0.90 ) +
  scale_color_manual( values = c( 'South-Southeast'  = 'gray75',
                                  'North-Northeast'  = 'tomato3',
                                  'Midwest'          = 'gray30',
                                  'Brazil'           = 'black' ),
                      name = '' ) +
  scale_linetype_manual( values = c( 'South-Southeast'  = 'dashed',
                                     'North-Northeast'  = 'longdash',
                                     'Midwest'          = 'solid',
                                     'Brazil'           = 'dotted' ),
                         name = '' ) +
  scale_x_continuous( breaks = seq( 1925, 1965, 5 ),
                      name = 'Female birth cohort' ) +
  scale_y_continuous( breaks = seq( 0, 1, 0.025 ),
                      labels = format( 100 * seq( 0, 1, 0.025 ),
                                       nsmall = 1 ),
                      limits = c( 0, 0.25 ),
                      name   = 'Prevalence of pairing type (%)' ) +
  facet_wrap( ~ PairingType, nrow = 1 ) +
  theme_bw() +
  theme(
    axis.text.y = element_text( size = 12, color = 'black' ),
    axis.text.x = element_text( size = 12, angle = 90,
                                hjust = 0, vjust = 0.5,
                                color = 'black' ),
    panel.grid.major = element_line( size = 0.15,
                                     linetype = 'dotted',
                                     color = 'gray75' ),
    panel.grid.minor = element_line( size = 0.15,
                                     linetype = 'dotted',
                                     color = 'gray75' ),
    strip.text = element_text( size = 13, color = 'black',
                               face = 'bold' ),
    strip.background = element_rect( fill = 'gray90',
                                     color = 'white'),
    axis.title = element_text( size = 13, color = 'black' ),
    legend.position = 'bottom',
    legend.direction = 'horizontal',
    legend.text = element_text( size = 12 )
  )

plot_homogamy_dat <- 
  assort_dat[ PairingType == 'Homogamy' ] %>%
  ggplot() +
  geom_line( aes( x = FemCohort5, y = PrevPar, 
                  color = Region, linetype = Region ),
             size = 0.90 ) +
  scale_color_manual( values = c( 'South-Southeast'  = 'gray75',
                                  'North-Northeast'  = 'tomato3',
                                  'Midwest'          = 'gray30',
                                  'Brazil'           = 'black' ),
                      name = '' ) +
  scale_linetype_manual( values = c( 'South-Southeast'  = 'dashed',
                                     'North-Northeast'  = 'longdash',
                                     'Midwest'          = 'solid',
                                     'Brazil'           = 'dotted' ),
                         name = '' ) +
  scale_x_continuous( breaks = seq( 1925, 1965, 5 ),
                      name = 'Female birth cohort' ) +
  scale_y_continuous( breaks = seq( 0, 1, 0.05 ),
                      labels = format( 100 * seq( 0, 1, 0.05 ),
                                       nsmall = 0 ),
                      limits = c( 0.6, 1 ),
                      name   = 'Prevalence of pairing type (%)' ) +
  facet_wrap( ~ PairingType, nrow = 1 ) +
  theme_bw() +
  theme(
    axis.text.y = element_text( size = 12, color = 'black' ),
    axis.text.x = element_text( size = 12, angle = 90,
                                hjust = 0, vjust = 0.5,
                                color = 'black' ),
    panel.grid.major = element_line( size = 0.15,
                                     linetype = 'dotted',
                                     color = 'gray75' ),
    panel.grid.minor = element_line( size = 0.15,
                                     linetype = 'dotted',
                                     color = 'gray75' ),
    strip.text = element_text( size = 13, color = 'black',
                               face = 'bold' ),
    strip.background = element_rect( fill = 'gray90',
                                     color = 'white'),
    axis.title = element_text( size = 13, color = 'black' ),
    legend.position = 'bottom',
    legend.direction = 'horizontal',
    legend.text = element_text( size = 12 )
  )

ggsave( filename = 'OUTPUTS/FIGS/fig1.png',
        dpi = 300,
        plot = plot_heterogamy_dat,
        width  = 8,
        height = 4 )

ggsave( filename = 'OUTPUTS/FIGS/fig2.png',
        dpi = 300,
        plot = plot_homogamy_dat,
        width  = 6,
        height = 4 )
################################################################################

### 4. Plot CFR by educational pairing and for all females #--------------------
cfr5_pair_tab <- 
  datBRMatched[ ParityFlag == 0, 
                list(
                  Region,
                  EducFemale = dic_educ[ as.character( EducFemale ) ],
                  EducMale   = dic_educ[ as.character( EducMale ) ],
                  ParityCor,
                  FemCohort5 = as.numeric( paste0( FemCohort5 ) ),
                  SampWeight
                ) ] %>%
  .[ , 
     list(
       CFR5 = sum( ParityCor * SampWeight ) / sum( SampWeight )
     ),
     .( Region, FemCohort5, EducMale, EducFemale ) ]  %>%
  setorder( Region, FemCohort5, EducMale, EducFemale ) %>%
  .[ , EducMale := ifelse ( EducMale == 'LP',
                            "Less Than Primary",
                            ifelse ( EducMale == 'P',
                                     "Primary",
                                     ifelse ( EducMale == 'S+',
                                              "Secondary/Tertiary",
                                              NA
                                     )
                            )
  ) %>% factor( levels = c( "Less Than Primary",
                            "Primary",
                            "Secondary/Tertiary" ) ) ] %>%
  .[ , EducFemale := ifelse ( EducFemale == 'LP',
                              "Female: Less Than Primary",
                              ifelse ( EducFemale == 'P',
                                       "Female: Primary",
                                       ifelse ( EducFemale == 'S+',
                                                "Female: Secondary/Tertiary",
                                                NA
                                       )
                              )
  ) %>% factor( levels = c( "Female: Less Than Primary",
                            "Female: Primary",
                            "Female: Secondary/Tertiary" ) ) ]

cfr5_married_fem_tab <- 
  datBRMatched[ ParityFlag == 0, 
                list(
                  Region,
                  EducFemale = dic_educ[ as.character( EducFemale ) ],
                  EducMale   = dic_educ[ as.character( EducMale ) ],
                  ParityCor,
                  FemCohort5 = as.numeric( paste0( FemCohort5 ) ),
                  SampWeight
                ) ] %>%
  .[ , 
     list(
       CFR5 = sum( ParityCor * SampWeight ) / sum( SampWeight )
     ),
     .( Region, FemCohort5, EducFemale ) ]  %>%
  setorder( Region, FemCohort5, EducFemale ) %>%
  .[ , EducFemale := ifelse ( EducFemale == 'LP',
                              "Female: Less Than Primary",
                              ifelse ( EducFemale == 'P',
                                       "Female: Primary",
                                       ifelse ( EducFemale == 'S+',
                                                "Female: Secondary/Tertiary",
                                                NA
                                       )
                              )
  ) %>% factor( levels = c( "Female: Less Than Primary",
                            "Female: Primary",
                            "Female: Secondary/Tertiary" ) ) ]


cfr5_educ_fem_tab <- 
  datBRAll[ ParityFlag == 0, 
            list(
              Region,
              EducFemale = dic_educ[ as.character( EducFemale ) ],
              ParityCor,
              FemCohort5 = as.numeric( paste0( FemCohort5 ) ),
              SampWeight
            ) ] %>%
  .[ , 
     list(
       CFR5 = sum( ParityCor * SampWeight ) / sum( SampWeight )
     ),
     .( Region, FemCohort5, EducFemale ) ]  %>%
  setorder( Region, FemCohort5, EducFemale ) %>%
  .[ , EducFemale := ifelse ( EducFemale == 'LP',
                              "Female: Less Than Primary",
                              ifelse ( EducFemale == 'P',
                                       "Female: Primary",
                                       ifelse ( EducFemale == 'S+',
                                                "Female: Secondary/Tertiary",
                                                NA
                                       )
                              )
  ) %>% factor( levels = c( "Female: Less Than Primary",
                            "Female: Primary",
                            "Female: Secondary/Tertiary" ) ) ] 
    

cfr5_all_fem_tab <- 
  datBRAll[ ParityFlag == 0, 
            list(
              Region,
              EducFemale = dic_educ[ as.character( EducFemale ) ],
              ParityCor,
              FemCohort5 = as.numeric( paste0( FemCohort5 ) ),
              SampWeight
            ) ] %>%
  .[ , 
     list(
       CFR5 = sum( ParityCor * SampWeight ) / sum( SampWeight )
     ),
     .( Region, FemCohort5 ) ]  %>%
  setorder( Region, FemCohort5 )

cfr5_all_fem_tab <- 
  rbind(
    cfr5_all_fem_tab %>% copy %>%
      .[ , EducFemale := 'Female: Less Than Primary'],
    cfr5_all_fem_tab %>% copy %>%
      .[ , EducFemale := 'Female: Primary'],
    cfr5_all_fem_tab %>% copy %>%
      .[ , EducFemale := 'Female: Secondary/Tertiary']
  ) %>%
  .[ , EducFemale := factor( EducFemale,
                             levels = c( "Female: Less Than Primary",
                                         "Female: Primary",
                                         "Female: Secondary/Tertiary" ) ) ]

cfr5_all_marfem_tab <- 
  datBRMatched[ ParityFlag == 0, 
                list(
                  Region,
                  EducFemale = dic_educ[ as.character( EducFemale ) ],
                  ParityCor,
                  FemCohort5 = as.numeric( paste0( FemCohort5 ) ),
                  SampWeight
                ) ] %>%
  .[ , 
     list(
       CFR5 = sum( ParityCor * SampWeight ) / sum( SampWeight )
     ),
     .( Region, FemCohort5 ) ]  %>%
  setorder( Region, FemCohort5 )


cfr5_all_marfem_tab <- 
  rbind(
    cfr5_all_marfem_tab %>% copy %>%
      .[ , EducFemale := 'Female: Less Than Primary'],
    cfr5_all_marfem_tab %>% copy %>%
      .[ , EducFemale := 'Female: Primary'],
    cfr5_all_marfem_tab %>% copy %>%
      .[ , EducFemale := 'Female: Secondary/Tertiary']
  ) %>%
  .[ , EducFemale := factor( EducFemale,
                             levels = c( "Female: Less Than Primary",
                                         "Female: Primary",
                                         "Female: Secondary/Tertiary" ) ) ]

shape_legend <- 
  data.table(
    EducFemale = c( "Female: Secondary/Tertiary", 
                    "Female: Secondary/Tertiary", 
                    "Female: Primary", 
                    "Female: Primary" ),
    FemCohort5 = c( 1965, 1965, 1965, 1965 ),
    CFR5 = c( 7, 6.5, 7, 6.5 ),
    lab  = c( 'All women by education category', 
              'Women in union by education category',
              'All women (overall)',
              'Women in union (overall)' )
  )

plot_cfr5_br <- 
  ggplot() +
  # 1: replacement fertility
  geom_hline(
    yintercept = 2.1,
    size = 0.5,
    color = 'skyblue',
    linetype = 'dashed',
    alpha = 0.90 
  ) +
  # 2: Educational Pairing CFR
  geom_line( data = cfr5_pair_tab[ Region == 'Brazil' ], 
             aes( 
               x = FemCohort5,
               y = CFR5,
               color    = EducMale,
               linetype = EducMale,
               group    = EducMale
             ),
             size = 1 ) +
  # 3: All Married Females CFR
  geom_point( data = cfr5_married_fem_tab[ Region == 'Brazil' ], 
              aes( 
                x = FemCohort5,
                y = CFR5
              ),
              size  = 2,
              color = 'gray51',
              shape = 16 ) +
  # 3: Married Females CFR legend
  geom_point( 
    data = data.table( x = 1930, y = 7,
                       EducFemale = "Female: Secondary/Tertiary" ),
    aes( 
      x = x,
      y = y 
    ),
    size  = 2,
    color = 'gray51',
    shape = 4
  ) +
  # 4: All females CFR by educ group
  geom_point( data = cfr5_educ_fem_tab[ Region == 'Brazil' ], 
              aes( 
                x = FemCohort5,
                y = CFR5
              ),
              size  = 2,
              color = 'gray51',
              shape = 4 ) +
  # 4: All females CFR by educ group legend
  geom_point( 
    data = data.table( x = 1925, y = 6.5,
                       EducFemale = "Female: Secondary/Tertiary" ),
    aes( 
      x = x,
      y = y 
    ),
    size  = 2,
    color = 'gray51',
    shape = 16
  ) +
  # 5: All females CFR 
  geom_line( data = cfr5_all_fem_tab[ Region == 'Brazil' ], 
             aes( 
               x = FemCohort5,
               y = CFR5
             ),
             size  = 1.25,
             color = 'steelblue4',
             alpha = 0.25 ) +
  # 5: All females CFR legend
  geom_segment( 
    data = data.table( x1 = 1940, x2 = 1944,
                       y1 = 7, y2 = 7,
                       EducFemale = "Female: Primary" ),
    aes( 
      x    = x1,
      xend = x2,
      y    = y1,
      yend = y2
    ),
    size  = 1.25,
    color = 'steelblue4',
    alpha = 0.25
  ) +
  # 5: All married females CFR 
  geom_line( data = cfr5_all_marfem_tab[ Region == 'Brazil' ], 
             aes( 
               x = FemCohort5,
               y = CFR5
             ),
             size  = 1.25,
             color = 'orange',
             alpha = 0.40,
             linetype = 'dashed') +
  # 6: All married females CFR legend
  geom_segment( 
    data = data.table( x1 = 1935, x2 = 1938,
                       y1 = 6.5, y2 = 6.5,
                       EducFemale = "Female: Primary" ),
    aes( 
      x    = x1,
      xend = x2,
      y    = y1,
      yend = y2
    ),
    size  = 1.25,
    color = 'orange',
    alpha = 0.40
  ) +
  geom_text(
    data = shape_legend,
    aes( x = FemCohort5,
         y = CFR5,
         label = lab 
    ),
    hjust = 1,
    size = 3
  ) +
  facet_wrap( ~ EducFemale, nrow = 1 ) + 
  labs( 
    x = "\nFemale birth cohort", 
    y = "Cohort fertility rates (CFR)\n", 
    color = ""
  ) +
  scale_color_manual( 
    labels = c( "Less than primary", "Primary", 
                "Secondary/Tertiary", "Tertiary" ), 
    values = c( "black", "gray35", "tomato3", "navyblue" ),
    name = "Male educational\nattainment level" 
  ) +
  scale_linetype_manual( 
    labels = c( "Less than primary", "Primary", 
                "Secondary/Tertiary", "Tertiary" ), 
    values = c( "solid", "longdash", 
                "dashed", "dotted" ),
    name = "Male educational\nattainment level" 
  ) +
  theme_bw() +
  theme(
    legend.position  = "bottom",
    axis.text.y      = element_text( size = 12, color = 'black'  ),
    axis.text.x      = element_text( size = 12, angle = 90,
                                     vjust = 0.5, color = 'black' ),
    axis.title       = element_text( size = 13, color = 'black', face = 'bold' ),
    legend.title     = element_text( size = 13, color = 'black', face = 'bold' ),
    legend.text      = element_text( size = 12, color = 'black'  ),
    strip.text       = element_text( size = 12, color = 'black' ),
    strip.background = element_rect( fill = 'gray90', 
                                     color = 'white' ),
    panel.grid.major = element_line( colour = "gray65", 
                                     size = 0.15, linetype = 'dotted' ),
    panel.grid.minor = element_line( colour = "gray65", 
                                     size = 0.15, linetype = 'dotted' ),
    legend.direction = 'horizontal',
    legend.background = element_rect( color = 'white', 
                                      fill = 'transparent', size = 0.21 )
  ) +
  scale_y_continuous( 
    breaks = seq( 1, 15, 1 ),
    limits = c( 1.5, 7 )
  ) +
  scale_x_continuous( 
    breaks = seq( 1925, 1965, 5 ),
    limits = c( 1925, 1965 )
  ) 

plot_cfr5_sse <- 
  ggplot() +
  # 1: replacement fertility
  geom_hline(
    yintercept = 2.1,
    size = 0.5,
    color = 'skyblue',
    linetype = 'dashed',
    alpha = 0.90 
  ) +
  # 2: Educational Pairing CFR
  geom_line( data = cfr5_pair_tab[ Region == 'South-Southeast' ], 
             aes( 
               x = FemCohort5,
               y = CFR5,
               color    = EducMale,
               linetype = EducMale,
               group    = EducMale
             ),
             size = 1 ) +
  # 3: All Married Females CFR
  geom_point( data = cfr5_married_fem_tab[ Region == 'South-Southeast' ], 
              aes( 
                x = FemCohort5,
                y = CFR5
              ),
              size  = 2,
              color = 'gray51',
              shape = 16 ) +
  # 3: Married Females CFR legend
  geom_point( 
    data = data.table( x = 1930, y = 7,
                       EducFemale = "Female: Secondary/Tertiary" ),
    aes( 
      x = x,
      y = y 
    ),
    size  = 2,
    color = 'gray51',
    shape = 4
  ) +
  # 4: All females CFR by educ group
  geom_point( data = cfr5_educ_fem_tab[ Region == 'South-Southeast' ], 
              aes( 
                x = FemCohort5,
                y = CFR5
              ),
              size  = 2,
              color = 'gray51',
              shape = 4 ) +
  # 4: All females CFR by educ group legend
  geom_point( 
    data = data.table( x = 1925, y = 6.5,
                       EducFemale = "Female: Secondary/Tertiary" ),
    aes( 
      x = x,
      y = y 
    ),
    size  = 2,
    color = 'gray51',
    shape = 16
  ) +
  # 5: All females CFR 
  geom_line( data = cfr5_all_fem_tab[ Region == 'South-Southeast' ], 
             aes( 
               x = FemCohort5,
               y = CFR5
             ),
             size  = 1.25,
             color = 'steelblue4',
             alpha = 0.25 ) +
  # 5: All females CFR legend
  geom_segment( 
    data = data.table( x1 = 1940, x2 = 1944,
                       y1 = 7, y2 = 7,
                       EducFemale = "Female: Primary" ),
    aes( 
      x    = x1,
      xend = x2,
      y    = y1,
      yend = y2
    ),
    size  = 1.25,
    color = 'steelblue4',
    alpha = 0.25
  ) +
  # 5: All married females CFR 
  geom_line( data = cfr5_all_marfem_tab[ Region == 'South-Southeast' ], 
             aes( 
               x = FemCohort5,
               y = CFR5
             ),
             size  = 1.25,
             color = 'orange',
             alpha = 0.40,
             linetype = 'dashed') +
  # 6: All married females CFR legend
  geom_segment( 
    data = data.table( x1 = 1935, x2 = 1938,
                       y1 = 6.5, y2 = 6.5,
                       EducFemale = "Female: Primary" ),
    aes( 
      x    = x1,
      xend = x2,
      y    = y1,
      yend = y2
    ),
    size  = 1.25,
    color = 'orange',
    alpha = 0.40
  ) +
  geom_text(
    data = shape_legend,
    aes( x = FemCohort5,
         y = CFR5,
         label = lab 
    ),
    hjust = 1,
    size = 3
  ) +
  facet_wrap( ~ EducFemale, nrow = 1 ) + 
  labs( 
    x = "\nFemale birth cohort", 
    y = "Cohort fertility rates (CFR)\n", 
    color = ""
  ) +
  scale_color_manual( 
    labels = c( "Less than primary", "Primary", 
                "Secondary/Tertiary", "Tertiary" ), 
    values = c( "black", "gray35", "tomato3", "navyblue" ),
    name = "Male educational\nattainment level" 
  ) +
  scale_linetype_manual( 
    labels = c( "Less than primary", "Primary", 
                "Secondary/Tertiary", "Tertiary" ), 
    values = c( "solid", "longdash", 
                "dashed", "dotted" ),
    name = "Male educational\nattainment level" 
  ) +
  theme_bw() +
  theme(
    legend.position  = "bottom",
    axis.text.y      = element_text( size = 12, color = 'black'  ),
    axis.text.x      = element_text( size = 12, angle = 90,
                                     vjust = 0.5, color = 'black' ),
    axis.title       = element_text( size = 13, color = 'black', face = 'bold' ),
    legend.title     = element_text( size = 13, color = 'black', face = 'bold' ),
    legend.text      = element_text( size = 12, color = 'black'  ),
    strip.text       = element_text( size = 12, color = 'black' ),
    strip.background = element_rect( fill = 'gray90', 
                                     color = 'white' ),
    panel.grid.major = element_line( colour = "gray65", 
                                     size = 0.15, linetype = 'dotted' ),
    panel.grid.minor = element_line( colour = "gray65", 
                                     size = 0.15, linetype = 'dotted' ),
    legend.direction = 'horizontal',
    legend.background = element_rect( color = 'white', 
                                      fill = 'transparent', size = 0.21 )
  ) +
  scale_y_continuous( 
    breaks = seq( 1, 15, 1 ),
    limits = c( 1.5, 7 )
  ) +
  scale_x_continuous( 
    breaks = seq( 1925, 1965, 5 ),
    limits = c( 1925, 1965 )
  ) 


shape_legend <- 
  data.table(
    EducFemale = c( "Female: Secondary/Tertiary", 
                    "Female: Secondary/Tertiary", 
                    "Female: Primary", 
                    "Female: Primary" ),
    FemCohort5 = c( 1965, 1965, 1965, 1965 ),
    CFR5 = c( 8.5, 8, 8.5, 8 ),
    lab  = c( 'All women by education category', 
              'Women in union by education category',
              'All women (overall)',
              'Women in union (overall)' )
  )

plot_cfr5_nne <- 
  ggplot() +
  # 1: replacement fertility
  geom_hline(
    yintercept = 2.1,
    size = 0.5,
    color = 'skyblue',
    linetype = 'dashed',
    alpha = 0.90 
  ) +
  # 2: Educational Pairing CFR
  geom_line( data = cfr5_pair_tab[ Region == 'North-Northeast' ], 
             aes( 
               x = FemCohort5,
               y = CFR5,
               color    = EducMale,
               linetype = EducMale,
               group    = EducMale
             ),
             size = 1 ) +
  # 3: All Married Females CFR
  geom_point( data = cfr5_married_fem_tab[ Region == 'North-Northeast' ], 
              aes( 
                x = FemCohort5,
                y = CFR5
              ),
              size  = 2,
              color = 'gray51',
              shape = 16 ) +
  # 3: Married Females CFR legend
  geom_point( 
    data = data.table( x = 1930, y = 8.5,
                       EducFemale = "Female: Secondary/Tertiary" ),
    aes( 
      x = x,
      y = y 
    ),
    size  = 2,
    color = 'gray51',
    shape = 4
  ) +
  # 4: All females CFR by educ group
  geom_point( data = cfr5_educ_fem_tab[ Region == 'North-Northeast' ], 
              aes( 
                x = FemCohort5,
                y = CFR5
              ),
              size  = 2,
              color = 'gray51',
              shape = 4 ) +
  # 4: All females CFR by educ group legend
  geom_point( 
    data = data.table( x = 1925, y = 8,
                       EducFemale = "Female: Secondary/Tertiary" ),
    aes( 
      x = x,
      y = y 
    ),
    size  = 2,
    color = 'gray51',
    shape = 16
  ) +
  # 5: All females CFR 
  geom_line( data = cfr5_all_fem_tab[ Region == 'North-Northeast' ], 
             aes( 
               x = FemCohort5,
               y = CFR5
             ),
             size  = 1.25,
             color = 'steelblue4',
             alpha = 0.25 ) +
  # 5: All females CFR legend
  geom_segment( 
    data = data.table( x1 = 1940, x2 = 1944,
                       y1 = 8.5, y2 = 8.5,
                       EducFemale = "Female: Primary" ),
    aes( 
      x    = x1,
      xend = x2,
      y    = y1,
      yend = y2
    ),
    size  = 1.25,
    color = 'steelblue4',
    alpha = 0.25
  ) +
  # 5: All married females CFR 
  geom_line( data = cfr5_all_marfem_tab[ Region == 'North-Northeast' ], 
             aes( 
               x = FemCohort5,
               y = CFR5
             ),
             size  = 1.25,
             color = 'orange',
             alpha = 0.40,
             linetype = 'dashed') +
  # 6: All married females CFR legend
  geom_segment( 
    data = data.table( x1 = 1935, x2 = 1938,
                       y1 = 8, y2 = 8,
                       EducFemale = "Female: Primary" ),
    aes( 
      x    = x1,
      xend = x2,
      y    = y1,
      yend = y2
    ),
    size  = 1.25,
    color = 'orange',
    alpha = 0.40
  ) +
  geom_text(
    data = shape_legend,
    aes( x = FemCohort5,
         y = CFR5,
         label = lab 
    ),
    hjust = 1,
    size = 3
  ) +
  facet_wrap( ~ EducFemale, nrow = 1 ) + 
  labs( 
    x = "\nFemale birth cohort", 
    y = "Cohort fertility rates (CFR)\n", 
    color = ""
  ) +
  scale_color_manual( 
    labels = c( "Less than primary", "Primary", 
                "Secondary/Tertiary", "Tertiary" ), 
    values = c( "black", "gray35", "tomato3", "navyblue" ),
    name = "Male educational\nattainment level" 
  ) +
  scale_linetype_manual( 
    labels = c( "Less than primary", "Primary", 
                "Secondary/Tertiary", "Tertiary" ), 
    values = c( "solid", "longdash", 
                "dashed", "dotted" ),
    name = "Male educational\nattainment level" 
  ) +
  theme_bw() +
  theme(
    legend.position  = "bottom",
    axis.text.y      = element_text( size = 12, color = 'black'  ),
    axis.text.x      = element_text( size = 12, angle = 90,
                                     vjust = 0.5, color = 'black' ),
    axis.title       = element_text( size = 13, color = 'black', face = 'bold' ),
    legend.title     = element_text( size = 13, color = 'black', face = 'bold' ),
    legend.text      = element_text( size = 12, color = 'black'  ),
    strip.text       = element_text( size = 12, color = 'black' ),
    strip.background = element_rect( fill = 'gray90', 
                                     color = 'white' ),
    panel.grid.major = element_line( colour = "gray65", 
                                     size = 0.15, linetype = 'dotted' ),
    panel.grid.minor = element_line( colour = "gray65", 
                                     size = 0.15, linetype = 'dotted' ),
    legend.direction = 'horizontal',
    legend.background = element_rect( color = 'white', 
                                      fill = 'transparent', size = 0.21 )
  ) +
  scale_y_continuous( 
    breaks = seq( 1, 15, 1 ),
    limits = c( 1.5, 9 )
  ) +
  scale_x_continuous( 
    breaks = seq( 1925, 1965, 5 ),
    limits = c( 1925, 1965 )
  ) 

plot_cfr5_mid <- 
  ggplot() +
  # 1: replacement fertility
  geom_hline(
    yintercept = 2.1,
    size = 0.5,
    color = 'skyblue',
    linetype = 'dashed',
    alpha = 0.90 
  ) +
  # 2: Educational Pairing CFR
  geom_line( data = cfr5_pair_tab[ Region == 'Midwest' ], 
             aes( 
               x = FemCohort5,
               y = CFR5,
               color    = EducMale,
               linetype = EducMale,
               group    = EducMale
             ),
             size = 1 ) +
  # 3: All Married Females CFR
  geom_point( data = cfr5_married_fem_tab[ Region == 'Midwest' ], 
              aes( 
                x = FemCohort5,
                y = CFR5
              ),
              size  = 2,
              color = 'gray51',
              shape = 16 ) +
  # 3: Married Females CFR legend
  geom_point( 
    data = data.table( x = 1930, y = 8.5,
                       EducFemale = "Female: Secondary/Tertiary" ),
    aes( 
      x = x,
      y = y 
    ),
    size  = 2,
    color = 'gray51',
    shape = 4
  ) +
  # 4: All females CFR by educ group
  geom_point( data = cfr5_educ_fem_tab[ Region == 'Midwest' ], 
              aes( 
                x = FemCohort5,
                y = CFR5
              ),
              size  = 2,
              color = 'gray51',
              shape = 4 ) +
  # 4: All females CFR by educ group legend
  geom_point( 
    data = data.table( x = 1925, y = 8,
                       EducFemale = "Female: Secondary/Tertiary" ),
    aes( 
      x = x,
      y = y 
    ),
    size  = 2,
    color = 'gray51',
    shape = 16
  ) +
  # 5: All females CFR 
  geom_line( data = cfr5_all_fem_tab[ Region == 'Midwest' ], 
             aes( 
               x = FemCohort5,
               y = CFR5
             ),
             size  = 1.25,
             color = 'steelblue4',
             alpha = 0.25 ) +
  # 5: All females CFR legend
  geom_segment( 
    data = data.table( x1 = 1940, x2 = 1944,
                       y1 = 8.5, y2 = 8.5,
                       EducFemale = "Female: Primary" ),
    aes( 
      x    = x1,
      xend = x2,
      y    = y1,
      yend = y2
    ),
    size  = 1.25,
    color = 'steelblue4',
    alpha = 0.25
  ) +
  # 5: All married females CFR 
  geom_line( data = cfr5_all_marfem_tab[ Region == 'Midwest' ], 
             aes( 
               x = FemCohort5,
               y = CFR5
             ),
             size  = 1.25,
             color = 'orange',
             alpha = 0.40,
             linetype = 'dashed') +
  # 6: All married females CFR legend
  geom_segment( 
    data = data.table( x1 = 1935, x2 = 1938,
                       y1 = 8, y2 = 8,
                       EducFemale = "Female: Primary" ),
    aes( 
      x    = x1,
      xend = x2,
      y    = y1,
      yend = y2
    ),
    size  = 1.25,
    color = 'orange',
    alpha = 0.40
  ) +
  geom_text(
    data = shape_legend,
    aes( x = FemCohort5,
         y = CFR5,
         label = lab 
    ),
    hjust = 1,
    size = 3
  ) +
  facet_wrap( ~ EducFemale, nrow = 1 ) + 
  labs( 
    x = "\nFemale birth cohort", 
    y = "Cohort fertility rates (CFR)\n", 
    color = ""
  ) +
  scale_color_manual( 
    labels = c( "Less than primary", "Primary", 
                "Secondary/Tertiary", "Tertiary" ), 
    values = c( "black", "gray35", "tomato3", "navyblue" ),
    name = "Male educational\nattainment level" 
  ) +
  scale_linetype_manual( 
    labels = c( "Less than primary", "Primary", 
                "Secondary/Tertiary", "Tertiary" ), 
    values = c( "solid", "longdash", 
                "dashed", "dotted" ),
    name = "Male educational\nattainment level" 
  ) +
  theme_bw() +
  theme(
    legend.position  = "bottom",
    axis.text.y      = element_text( size = 12, color = 'black'  ),
    axis.text.x      = element_text( size = 12, angle = 90,
                                     vjust = 0.5, color = 'black' ),
    axis.title       = element_text( size = 13, color = 'black', face = 'bold' ),
    legend.title     = element_text( size = 13, color = 'black', face = 'bold' ),
    legend.text      = element_text( size = 12, color = 'black'  ),
    strip.text       = element_text( size = 12, color = 'black' ),
    strip.background = element_rect( fill = 'gray90', 
                                     color = 'white' ),
    panel.grid.major = element_line( colour = "gray65", 
                                     size = 0.15, linetype = 'dotted' ),
    panel.grid.minor = element_line( colour = "gray65", 
                                     size = 0.15, linetype = 'dotted' ),
    legend.direction = 'horizontal',
    legend.background = element_rect( color = 'white', 
                                      fill = 'transparent', size = 0.21 )
  ) +
  scale_y_continuous( 
    breaks = seq( 1, 15, 1 ),
    limits = c( 1.5, 9 )
  ) +
  scale_x_continuous( 
    breaks = seq( 1925, 1965, 5 ),
    limits = c( 1925, 1965 )
  ) 

ggsave(
  filename = 'OUTPUTS/FIGS/fig3.png',
  dpi = 300,
  width = 8,
  height = 4,
  plot = plot_cfr5_br
)

ggsave(
  filename = 'OUTPUTS/FIGS/figb1.png',
  dpi = 300,
  width = 8,
  height = 4,
  plot = plot_cfr5_mid
)

ggsave(
  filename = 'OUTPUTS/FIGS/figb2.png',
  dpi = 300,
  width = 8,
  height = 4,
  plot = plot_cfr5_nne
)

ggsave(
  filename = 'OUTPUTS/FIGS/figb3.png',
  dpi = 300,
  width = 8,
  height = 4,
  plot = plot_cfr5_sse
)
################################################################################

### 5. Tables educational pairing prevalence and CFR - 15 year cohorts  #-------
pairing_tab <- 
  datBRMatched[ , 
                list(
                  Region,
                  EducFemale = dic_educ[ as.character( EducFemale ) ],
                  EducMale   = dic_educ[ as.character( EducMale ) ],
                  ParityCor,
                  FemCohort15 = as.numeric( paste0( FemCohort15 ) ),
                  SampWeight
                ) ] %>%
  .[ , 
     list(
       NPair = sum( SampWeight )
     ),
     .( Region, FemCohort15, EducMale, EducFemale ) ] %>%
  .[ , 
     list(
       EducMale, EducFemale,
       PrevPar = NPair / sum( NPair )
     ),
     .( Region, FemCohort15 ) ] %>%
  setorder( Region, FemCohort15, EducMale, EducFemale ) %>%
  dcast( Region + EducMale + EducFemale ~ FemCohort15,
         value.var = 'PrevPar' )

cfr15_tab <- 
  datBRMatched[ ParityFlag == 0, 
                list(
                  Region,
                  EducFemale = dic_educ[ as.character( EducFemale ) ],
                  EducMale   = dic_educ[ as.character( EducMale ) ],
                  ParityCor,
                  FemCohort15 = as.numeric( paste0( FemCohort15 ) ),
                  SampWeight
                ) ] %>%
  .[ , 
     list(
       CFR15 = sum( ParityCor * SampWeight ) / sum( SampWeight )
     ),
     .( Region, FemCohort15, EducMale, EducFemale ) ]  %>%
  setorder( Region, FemCohort15, EducMale, EducFemale ) %>%
  dcast( Region + EducMale + EducFemale ~ FemCohort15,
         value.var = 'CFR15' )

tab_prev_cfr <-  
  merge(
    pairing_tab[ , .( Region, EducFemale, EducMale,
                      p1 = round( 100 * `1925`, 2 ),
                      p2 = round( 100 * `1940`, 2 ), 
                      p3 = round( 100 * `1955`, 2 ) ) ],
    cfr15_tab[ , .( Region, EducFemale, EducMale,
                    cfr1 = round( `1925`, 2 ),
                    cfr2 = round( `1940`, 2 ),
                    cfr3 = round( `1955`, 2 ) ) ],
    by = c( 'Region', 'EducMale', 'EducFemale' )
  )

tab_prev_cfr_br <- 
  tab_prev_cfr[ Region == 'Brazil' ] %>%
  setnames(
    c( 'Region', 'Educ-Female', 'Educ-Male',
       'Prevalence 1925-1939', 'Prevalence 1940-1954', 'Prevalence 1955-1969',
       'CFR 1925-1939', 'CFR 1940-1954', 'CFR 1955-1969')
  )

tab_prev_cfr_mid <- 
  tab_prev_cfr[ Region == 'Midwest' ] %>%
  setnames(
    c( 'Region', 'Educ-Female', 'Educ-Male',
       'Prevalence 1925-1939', 'Prevalence 1940-1954', 'Prevalence 1955-1969',
       'CFR 1925-1939', 'CFR 1940-1954', 'CFR 1955-1969')
  )

tab_prev_cfr_nne <- 
  tab_prev_cfr[ Region == 'North-Northeast' ] %>%
  setnames(
    c( 'Region', 'Educ-Female', 'Educ-Male',
       'Prevalence 1925-1939', 'Prevalence 1940-1954', 'Prevalence 1955-1969',
       'CFR 1925-1939', 'CFR 1940-1954', 'CFR 1955-1969')
  )

tab_prev_cfr_sse <- 
  tab_prev_cfr[ Region == 'South-Southeast' ] %>%
  setnames(
    c( 'Region', 'Educ-Female', 'Educ-Male',
       'Prevalence 1925-1939', 'Prevalence 1940-1954', 'Prevalence 1955-1969',
       'CFR 1925-1939', 'CFR 1940-1954', 'CFR 1955-1969')
  )

write.table( tab_prev_cfr_br,
             file = 'OUTPUTS/TABLES/tab1.csv',
             row.names = FALSE,
             sep = ';' )

write.table( tab_prev_cfr_mid,
             file = 'OUTPUTS/TABLES/tabb1.csv',
             row.names = FALSE,
             sep = ';' )

write.table( tab_prev_cfr_nne,
             file = 'OUTPUTS/TABLES/tabb2.csv',
             row.names = FALSE,
             sep = ';' )

write.table( tab_prev_cfr_sse,
             file = 'OUTPUTS/TABLES/tabb3.csv',
             row.names = FALSE,
             sep = ';' )
##################################################################

### 6. Decomposition #--------------------------------------------

dat_decomp <- 
  merge(
    pairing_tab[ , .( Region, EducMale, EducFemale,
                      p1 = `1925`, p2 = `1940`, p3 = `1955` ) ],
    cfr15_tab[ , .( Region, EducMale, EducFemale,
                    r1 = `1925`, r2 = `1940`, r3 = `1955` ) ],
    by = c( 'Region', 'EducMale', 'EducFemale' )
  )

# Kitagawa decomposition function
kitagawa_decomp <- 
  function( cfr1, p1, cfr2, p2 ){
    
    overall_diff <-   
      cfr2 * p2 - cfr1 * p1
    
    rate_eff <- ( cfr2 - cfr1 ) * ( p1 + p2 ) / 2    
    
    structure_eff <- ( p2 - p1 ) * ( cfr1 + cfr2 ) / 2
    
    out <- 
      data.table(
        overall_diff,
        rate_eff,
        structure_eff
      )
    
    return( out )
  }

# Plot decomposition function
plot_decomp <- 
  function( reg ){
    r1 <- dat_decomp[ Region == reg ]$r1
    r2 <- dat_decomp[ Region == reg ]$r2
    r3 <- dat_decomp[ Region == reg ]$r3
    
    s1 <- dat_decomp[ Region == reg ]$p1
    s2 <- dat_decomp[ Region == reg ]$p2
    s3 <- dat_decomp[ Region == reg ]$p3
    
    
    decomp_res <- 
      rbind(
        cbind(
          dat_decomp[ Region == reg ],
          kitagawa_decomp( r1, s1, r2, s2 ) ) %>%
          melt(
            id.vars = c( 'Region', 'EducMale', 'EducFemale' ),
            measure.vars = c( 'rate_eff', 'structure_eff' )
          ) %>%
          .[ , Period := 'CFR(1925-1939)-CFR(1940-1954)' ] %>%
          .[ , EducPair := paste0( EducMale, '\n', EducFemale ) ] %>%
          .[ , valueP := value / sum( value ) ],
        cbind(
          dat_decomp[ Region == reg ],
          kitagawa_decomp( r1, s1, r3, s3 ) ) %>%
          melt(
            id.vars = c( 'Region', 'EducMale', 'EducFemale' ),
            measure.vars = c( 'rate_eff', 'structure_eff' )
          ) %>%
          .[ , Period := 'CFR(1925-1939)-CFR(1955-1969)' ] %>%
          .[ , EducPair := paste0( EducMale, '\n', EducFemale ) ] %>%
          .[ , valueP := value / sum( value ) ],
        cbind(
          dat_decomp[ Region == reg ],
          kitagawa_decomp( r2, s2, r3, s3 ) ) %>%
          melt(
            id.vars = c( 'Region', 'EducMale', 'EducFemale' ),
            measure.vars = c( 'rate_eff', 'structure_eff' )
          ) %>%
          .[ , Period := 'CFR(1940-1954)-CFR(1955-1969)' ] %>%
          .[ , EducPair := paste0( EducMale, '\n', EducFemale ) ] %>%
          .[ , valueP := value / sum( value ) ]
      )
    
    text_dat <- 
      rbind(
        decomp_res[ , .( value = sum( value ) ),
                    .( Period, variable ) ] %>%
          .[ , Lab := ifelse( variable == 'rate_eff',
                              paste0( 'Total Rate Effect: ', 
                                      format( round( value, 2 ) ),
                                      ' (', 
                                      format( round( 100 * value/sum(value), 1 ),
                                              nsmall = 1 ), '%)' ),
                              paste0( 'Total Structure Effect : ',
                                      format( round( value, 2 ) ),
                                      ' (', 
                                      format( round( 100 * value/sum(value), 1 ),
                                              nsmall = 1 ), 
                                      '%)' ) ),
             .( Period ) ] %>%
          .[ , .(
            Period,
            xlab = 'S+\nS+',
            ylab = ifelse( variable == 'rate_eff',
                           -3.75,
                           -4.00 ),
            Lab
          ) ],
        decomp_res[ , .( value = format( round( sum( value ), 
                                                2 ), 
                                         nsmall = 2 ) ),
                    .( Period ) ] %>%
          .[ , Lab := paste0( 'CFR Difference: ', value ) ] %>%
          .[ , .(
            Period,
            xlab = 'S+\nS+',
            ylab = -4.25,
            Lab
          ) ],
        data.table(
          Period = c( 'CFR(1925-1939)-CFR(1940-1954)',
                      'CFR(1925-1939)-CFR(1955-1969)',
                      'CFR(1940-1954)-CFR(1955-1969)' ),
          xlab = 'S+\nS+',
          ylab = -3.50,
          Lab  = c( paste0( 'CFR(1940-1954): ', 
                            format( round( sum( r2 * s2 ), 2 ),
                                    nsmall = 2 ) ),
                    paste0( 'CFR(1955-1969): ', 
                            format( round( sum( r3 * s3 ), 2 ),
                                    nsmall = 2 ) ),
                    paste0( 'CFR(1955-1969): ', 
                            format( round( sum( r3 * s3 ), 2 ),
                                    nsmall = 2 ) ) )
        ),
        data.table(
          Period = c( 'CFR(1925-1939)-CFR(1940-1954)',
                      'CFR(1925-1939)-CFR(1955-1969)',
                      'CFR(1940-1954)-CFR(1955-1969)' ),
          xlab = 'S+\nS+',
          ylab = -3.25,
          Lab  = c( paste0( 'CFR(1925-1939): ', 
                            format( round( sum( r1 * s1 ), 2 ),
                                    nsmall = 2 ) ),
                    paste0( 'CFR(1925-1939): ', 
                            format( round( sum( r1 * s1 ), 2 ),
                                    nsmall = 2 ) ),
                    paste0( 'CFR(1940-1954): ', 
                            format( round( sum( r2 * s2 ), 2 ),
                                    nsmall = 2 ) ) )
        )
      )
    
    plot_out <- 
      ggplot() +
      geom_col( data = decomp_res,
                aes( x = EducPair, y = value, 
                     fill = variable ),
                color = 'gray15') +
      scale_y_continuous( breaks = seq( -5, 5, 0.5 ),
                          labels = format( seq( -5, 5, 0.5 ),
                                           nsmall = 1 ),
                          limits = c( -4.55, 0.55 ) ) +
      geom_text( data = text_dat,
                 aes( x = xlab, y = ylab, label = Lab ),
                 size  = 3,
                 hjust = 1 ) +
      labs( 
        y = 'CFR Difference',
        x = '',
        subtitle = 'Educational Pairing: M (Male)\n                                F (Female)'
      ) +
      scale_fill_manual( labels = c( 'rate_eff' = ' Rate Effect ',
                                     'structure_eff' = ' Educational Pairing\n Structure Effect' ),
                         values = c( 'rate_eff' = 'gray42',
                                     'structure_eff' = 'tomato3' ),
                         name = '' ) +
      facet_wrap( ~ Period, ) +
      theme_bw() +
      theme(
        legend.position  = "bottom",
        axis.text.y      = element_text( size = 12, color = 'black'  ),
        axis.text.x      = element_text( size = 11, color = 'black' ),
        axis.title       = element_text( size = 12, color = 'black', face = 'bold' ),
        legend.title     = element_text( size = 0, color = 'black', face = 'bold' ),
        legend.text      = element_text( size = 12, color = 'black'  ),
        plot.subtitle    = element_text( size = 10, color = 'black' ),  
        strip.text       = element_text( size = 10, color = 'black' ),
        strip.background = element_rect( fill = 'gray90', 
                                         color = 'white' ),
        panel.grid.major = element_line( colour = "gray65", 
                                         size = 0.15, linetype = 'dotted' ),
        panel.grid.minor = element_line( colour = "gray65", 
                                         size = 0.15, linetype = 'dotted' ),
        legend.direction = 'horizontal',
        legend.background = element_rect( color = 'white', 
                                          fill = 'transparent', size = 0.21 )
      ) 
    
    return( plot_out )
  }

ggsave(
  filename = 'OUTPUTS/FIGS/fig4.png',
  dpi = 300,
  width = 10,
  height = 5,
  plot = plot_decomp( reg = 'Brazil' )
)

ggsave(
  filename = 'OUTPUTS/FIGS/figc1.png',
  dpi = 300,
  width = 10,
  height = 5,
  plot = plot_decomp( reg = 'Midwest' )
)

ggsave(
  filename = 'OUTPUTS/FIGS/figc2.png',
  dpi = 300,
  width = 10,
  height = 5,
  plot = plot_decomp( reg = 'North-Northeast' )
)

ggsave(
  filename = 'OUTPUTS/FIGS/figc3.png',
  dpi = 300,
  width = 10,
  height = 5,
  plot = plot_decomp( reg = 'South-Southeast' )
)

#################################################################

### 7. The End