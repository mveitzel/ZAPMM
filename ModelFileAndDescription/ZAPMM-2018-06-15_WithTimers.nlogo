;; Note that this model is meant to run in NetLogo 5.2.1

;;---------------------------------------------------------------
;;---------------------------------------------------------------
;;                   VARIABLE DECLARATIONS
;;---------------------------------------------------------------
;;---------------------------------------------------------------


;;---------------------------------------------------------------
;; VARIABLES WHICH WILL BE SET BY THE USER THROUGH THE INTERFACE
;;---------------------------------------------------------------

  ;;-------------------------------------------
  ;; Modeling Variables
  ;;-------------------------------------------

  ;; model-mode              ;;What are you using the model for?
  ;;                         ;;  "Demonstration" has no burn-in, intended for showing to others/investigating model behavior
  ;;                         ;;  "Experiment" has no patch symbology/display updates, and is for using the behaviorspace
  ;;                         ;;  "SoftwareTests" is for testing the basic functions used in the model
  ;;
  ;; use-muonde-thresholds   ;; true = use the sustainability thresholds established by Muonde from workshop
                             ;; false = use minimal thresholds so most simulations will run

  ;;-------------------------------------------
  ;; Crop Management
  ;;-------------------------------------------

  ;; proportion-crops        ;; What proportion of the land area is crop/arable?
  ;;
  ;; clumpiness              ;; How clumped together are those crops? 1 = one clump, 0 = almost checkerboard
  ;;
  ;; muonde-projects         ;; What proportion of crop patches have some kind of Muonde project which enhances their growth?
  ;;                         ;;   for example, water harvesting or drought-resistant small grains
  ;; how-long-to-store-grain ;; How many years can farmers store their surplus grain (bumper crops could help
  ;;                         ;;   get through bad years). Doesn't include this year's harvest.

  ;;-------------------------------------------
  ;; Woodland Management
  ;;-------------------------------------------

  ;; key-resources           ;; What percentage of the woodlands grow 4 times faster than their neighbors and are
  ;;                         ;;    key resources in times of drought?
  ;; invincible-fences       ;; Whether or not fences are stone walls - cows can't get in through them, they don't require wood
  ;;                         ;;    to build, and don't decay

  ;;-------------------------------------------
  ;; Cow Management
  ;;-------------------------------------------

  ;; times-per-day-farmers-move-cows  ;; Number of times per day that farmers move desperate cows to high-biomass
  ;;                                  ;;   woodlands (or 0 if farmers never do so and cows must find it themselves)
  ;; subsidy                          ;; Do farmers subsidise cows with other resources?
  ;;                                  ;;   "no" = farmers do not subsidize
  ;;                                  ;;   "transport" = farmers drive cows to a distant village outside the drought,
  ;;                                  ;;        where they stay for the season
  ;;                                  ;;   "feed" = farmers feed cows from resources outside the model
  ;; cow-proportion-to-save           ;; What proportion of cows should be saved with subsidy?

  ;;-------------------------------------------
  ;; Rainfall Variables
  ;;-------------------------------------------

  ;; rainfall-type          ;; What model of rainfall do we use?
  ;;                        ;;   constant: mean rainfall
  ;;                        ;;   historical: the actual timeseries
  ;;                        ;;   random: randomly chosen rainfall from the timeseries
  ;;                        ;;   extreme: randomly chosen rainfall from 1st and 4th quartiles of timeseries
  ;;                        ;;   statistical-random: rainfall drawn from normal distribution with mean/sd based on timeseries
  ;;                        ;;   statistical-extreme: rainfall drawn from normal distribution with 2x the sd
  ;; rain-site              ;; Use data from Chivi, Zvishavane, Mberengwa, or their average

;;---------------------------------------------------------------
;;  VARIABLES WHICH WILL BE SET PROCEEDURALLY/RANDOMLY
;;---------------------------------------------------------------

  ;;-------------------------------------------
  ;; DEFINE COW/LIVESTOCK "BREED"
  ;;-------------------------------------------
breed [cows cow]

  ;;-------------------------------------------
  ;; COW VARIABLES
  ;;-------------------------------------------
cows-own [
  satiety                        ;; Satiety is a scaled variable between 0 and 1 which allows us to color it red when it's below a
                                 ;;   hunger-threshold and to determine how desperate it is to get into crops. A cow's hunger
                                 ;;   depends on its body-mass.  Cows at max-cow-mass aren't hungry at all, cows at
                                 ;;   min-cow-mass are as hungry as possible, and we interpolate linearly for cows in
                                 ;;   between.  A cow's hunger (equal to 1 - "satiety") determines its probability of
                                 ;;   being able to break through a fence due to desperation (in combination with how
                                 ;;   intact the fence is). For illustrative purposes we color cows red if their
                                 ;;   satiety is less than a hunger-threshold (set during update-cows).

  body-mass                      ;; Body mass of cow (kg)
  energy-this-tick               ;; Keeping track of energy costs from working/metabolizing/moving and gains from eating (kcal)
  is-subsidized?                 ;; Has this cow been marked for subsidy (true/false)
  is-calf?                       ;; Calves can't work or reproduce; die below min-calf-mass (not min-cow-mass, only
                                 ;; applies to adults) Calves become adults when they reach min-cow-mass (plus a day's reserve)
                                 ;; Calves are born with a week's worth of reserves so they don't die if they can't eat immediately
]

  ;;-------------------------------------------
  ;; PATCH VARIABLES
  ;;-------------------------------------------
patches-own [
  standing-available-biomass     ;; State of growth: primary productivity available in the patch (kg)
  growth-rate                    ;; How fast the biomass on this patch grows (kg/tick)
  growth-multiplier              ;; Growth-rate modifier for key resources or Muonde projects (which grow faster)
  fence                          ;; How much of the fence is present? ranges from 0 (not fenced) and 1 (fully fenced)
  is-crop?                        ;; Boolean (true/false) for fast access of patch type
  is-woodland?                    ;; Boolean (true/false) for fast access of patch type
  how-many-fences                ;; For crops, how many sides border woodland, requiring fences? 0, 1, 2 , 3, or 4
                                 ;;    mostly determines amount of woodland each patch uses.
  moran-numerator                ;; Moran's I numerator for each patch (unitless)
  geary-numerator                ;; Geary's C numerator for each patch (unitless)
  moran-geary-denominator        ;; Moran's I or Geary's C denominator for each patch (same value for both metrics, unitless)
]

  ;;-------------------------------------------
  ;; GLOBAL VARIABLES
  ;;-------------------------------------------
  ;; Note that global variables in this model fall into four categories:
  ;;   1) agentsets that make searching more efficient (needed for better performance)
  ;;   2) tracking variables for monitoring model outcomes
  ;;   3) constants from literature and field data (will be perturbed for sensitivity analysis)
  ;;   4) derived variables calculated from the constants used by several functions (needed for better performance)

globals [

  ;;-----------------------------------------------
  ;; 1) AGENTSETS TO MAKE SEARCHING MORE EFFICIENT
  ;;-----------------------------------------------
  crops                          ;; Agentset for crop patches
  crops-with-fences              ;; Agentset for only crops with fences (for setup and fence repair)
  woodlands                      ;; Agentset for woodland patches
  cows-available-to-work         ;; Agentset for cows that have enough energy to work
  cows-available-to-reproduce    ;; Agentset for cows that have enough biomass to reproduce

  ;;----------------------------------
  ;; 2) TRACKING/MONITORING VARIABLES
  ;;----------------------------------

  running                        ;; Boolean (true/false) flag indicating whether the simulation is running
  termination-reason             ;; Indicates why the simulation stopped (simulation finished, too little woodland/cows/harvest)

  years-gone                     ;; How many model years have gone since the beginning of the simulation (in years)
  calendar-year                  ;; Actual model year (burn-in part of simulation runs with no 'actual year' and
                                 ;;    the first year of the actual rainfall timeseries is 1952

  burn-in                        ;; A true/false flag used to indicate whether to track variables or check stopping conditions
                                 ;;    (burn-in = TRUE means don't track them), lets transient behaviors dependent on
                                 ;;    initial conditions/spatial configurations die out
  burn-in-length                 ;; This is how long to let the model run in 'burn-in' mode (in model-years)

  current-harvest                ;; Crop harvested in this year (metric tons)
  previous-harvests-list         ;; A moving window of the last 'how-long-to-store-grain' years' harvest totals
                                 ;;  along with the current harvest
                                 ;; An additional how-long-to-store-grain harvests are recorded, and
                                 ;;    the mean is checked for sustainability (all in metric tons)
  percentage-harvest-eaten       ;; Each year, what percentage of the harvest was eaten by livestock?
  max-percentage-harvest-eaten   ;; At the end of the run, what was the maximum percentage of yearly harvest eaten by livestock?
  count-cows-in-crops            ;; how many cows spent a tick in a crop patch?
  crop-eaten                     ;; crop eaten during one year

  ;; accumulated outcomes
  total-harvest                  ;; Accumulated harvest over the whole simulation (in metric tons)
  total-crop-eaten               ;; Accumulated amount of crop that has been eaten (in metric tons)
  woodland-eaten                 ;; Accumulated amount of woodland that has been eaten (in metric tons)
  subsidy-used                   ;; Accumulated amount of subsidy that has been used to keep cows alive (in $)
  dead-cows                      ;; Accumulated amount of cows that have died since the begginning of the simulation
  total-number-of-births         ;; Accumulated number of cows born, for calculating the actual reproductive rate
  total-cows                     ;; Accumulated number of cows over the entire model run, for calculating the actual reproductive rate

  ;; max/min trackers
  max-livestock-number           ;; Monitors the highest number of cows that were ever alive at the same time
  min-livestock-number           ;; Monitors the lowest number of cows that were ever alive at the same time
  max-woodland-mass              ;; Monitors the highest amount of woodland ever available (in metric tons)
  min-woodland-mass              ;; Monitors the lowest amount of woodland ever available (in metric tons)

  ;; Landscape ecology variables
  morans-i                               ;; Spatial autocorrelation (unitless, between -1 and 1)
  gearys-c                               ;; Spatial autocorrelation (unitless, between 0 and infinity)
  average-contiguous-crop-cluster-size   ;; Stores the average size of a contiguous cluster of crops in ha
  total-crop-perimeter                   ;; Total length of fence/crop perimeter in m

  ;; Calculations at the end of the run (has to be a global to be reported through the behaviorspace)
  crop-eaten-per-cow-per-half-hour       ;; How much cows ate when they broke into the crops (kg/cow/0.5 hr, to compare with field data)
  actual-reproductive-rate               ;; Reproductive rate measured from the model to be compared with nominal rate (cows/cow/year)

  ;; trackers for running software tests
  number-of-tests-run
  error-count

  ;;----------------------------------------------------------------------------
  ;; 3) CONSTANTS FROM LITERATURE/FIELD DATA & 4) VARIABLES DERIVED FROM THEM
  ;;----------------------------------------------------------------------------

  ;;--------------------------------
  ;; spatial and temporal variables
  ;;--------------------------------

  world-size                            ;; How many cells by how many cells should the world be?
  ticks-per-day                         ;; How many model time steps (ticks) in a day?
  ticks-per-year                        ;; how many ticks per year?
  ha-per-patch                          ;; how large is a patch in ha

  ;;-------------------
  ;; cow variables
  ;;-------------------

  livestock-not-reproduction-rate-per-year;; What probability does a cow have of NOT reproducting in a given year
  production-efficiency                   ;; Efficiency of synthesizing cow protein/fat from digested plant matter, unitless
  catabolism-efficiency                   ;; Efficiency of breaking down fat (or degrading protein) for metabolic energy, unitless
  cow-maintenance-energy-rate             ;; How much energy cows spend to maintain basal metabolism, movement, etc (kcal/kg/day)
  cow-working-energy-per-hour             ;; How much energy does it take a cow to plough, per hour (kcal/hr)
  cow-maintenance-energy-per-tick-per-kg  ;; Energy cost for maintenance per tick (kcal/tick/kg)

  max-cow-mass                            ;; Biggest cow mass allowed (kg)
  min-cow-mass                            ;; Cow mass at which a cow dies (kg)
  min-calf-mass                           ;; Mass below which a calf dies (kg)
  calf-birth-mass                         ;; Mass at which a calf is born (kg)

  hours-to-plough-ha                     ;; How many cow-hours does it take to plough a hectare of field (hours)
  cost-of-supplemental-feed              ;; How much it costs to buy feed for cow ($/kg of feed)

  kcal-per-kg-of-cow                     ;; How much energy is contained in cow biomass (kcal/kg)
  kcal-per-kg-of-browse                  ;; How much energy is contained in woodland biomass?  (kcal/kg)
  kcal-per-kg-of-crop                    ;; How much energy is contained in crop biomass?  (kcal/kg)

  ;;-------------------
  ;; woodland/fencing variables
  ;;-------------------

  wood-to-build-fence-per-meter          ;; Wood necessary to extract to build a fence (kg/m/year)
  termite-activity                       ;; Determines how fast fences need to be rebuilt
  total-mud-crop-perimeter               ;; Current total fence length in Mudhomori village in 2013 in m
  woodland-growth-slope                  ;; How much woodland grows with more rain (kg/ha/yr/mm)

  ;;-------------------
  ;; crop variables
  ;;-------------------

  zero-crop-growth-intercept             ;; From data, how much rainfall is needed to produce any crops at all (mm)
  crop-growth-slope                      ;; From data, how much crops grow with more rain (kg/ha/mm/yr)
  muonde-efficiency                      ;; How much faster do crops with Muonde projects grow? (unitless)

  ;;--------------------
  ;; rainfall variables
  ;;--------------------

  extremity-factor                       ;; Multiplier for the standard deviation of rainfall - how extremely will climate change
                                         ;;   exaggerate rainfall variation? (in "statistical-extreme" rainfall mode)
  rainfall                               ;; Current value for rainfall (mm)
  rainfall-data                          ;; Sequence of rainfall values (read from file, in mm)
  extreme-rainfall-data                  ;; Sorted sequence of rainfall values to be sampled from (created from rainfall-data, mm)
  rainfall-index                         ;; Position on the sequence of rainfall

  ;;--------------------------
  ;; sustainability variables
  ;;--------------------------

  woodland-sustain-factor                ;; How many times the current amount of fencing's worth of woodland biomass do
                                         ;;   we need in order to be sustainable?
  minimum-woodland-for-sustainability    ;; Calculated from the sustain-factor, the amount of wood to build fence, and the amount
                                         ;;    of fence needed, how much do we need to have to be sustainable (in kg)
  minimum-cows-for-sustainability        ;; From Muonde, what is the minimum number of cows needed in a village like Mudhomori for
                                         ;;   sustainability?
  minimum-harvest-for-sustainability     ;; From Muonde, what is the minimum harvest in a year in a village like Mudhomori for
                                         ;;   sustainability? (in metric tons)


;; these will be removed in the final version posted online ***
  model-run-time                 ;; Will report at the end of the run how long the simulation ran on the computer (in seconds)
  model-setup-time               ;; reports how long setup took (in seconds)

]  ;; end of globals

;;---------------------------------------------------------------
;;---------------------------------------------------------------
;;                       SETUP PROCEDURES
;;---------------------------------------------------------------
;;---------------------------------------------------------------

;; Function: set-random-parameter-value-for-sensitivity-analysis
;; Setup procedure: only called during initialization when user clicks setup button
;; Action: takes one of the global variables and varies it around the set value by
;;           "amount-to-vary-it-by", e.g. 0.10 is 10% on either side of the set value
;; Note: this exists because NetLogo has a weird way of drawing from a uniform distribution
to-report set-random-parameter-value-for-sensitivity-analysis [ parameter-value amount-to-vary-it-by ]
  let new-value parameter-value * ( (1 - amount-to-vary-it-by) + random-float (2 * amount-to-vary-it-by ) )
   report new-value
end


;; Function: set-global-variables
;; Setup procedure: only called during initialization when user clicks setup button
;; Action: Initializes the values of all global variables and parameters
to set-global-variables

;-----------------------------
; TEMPORAL AND SPATIAL VARIABLES
;-----------------------------

  set ha-per-patch 600 / (world-size * world-size)  ;; Mudhomori is 600 ha so divide up the number of patches into 600 ha
;; we don't test sensitivity of the area of Mudhomori because the calibration targets are based on the assumption that it's 600 ha

  set ticks-per-year ticks-per-day * 365
  ;; derived variable, no sensitivity analysis (this variable is used extensively throughout the code so making it global gives us
  ;; a big performance boost)

;-----------------------------
; RAINFALL DATA AND VARIABLES
;-----------------------------
;; no sensitivity analysis is done with these variables, as we can test different sites and different ways
;; to permute the rainfall values or randomly generate them
  ;; Load the list with rain data depending on rain site chosen (from rain gauges near Mudhomori)
  ;; Originally this was a separate rainfall file, but this is faster and doesn't rely on external input files
  if rain-site = "Chivi" [
    set rainfall-data (list 1112.7 382.3 736.5 544.5 586.6 505.5 790.6 411.9 629.2 467.5 604.2 384.1 445.8
      488.3 696.3 236.3 607.7 388.4 493.6 698.9 216 779.8 827.2 500.4 784.8 568.6 487 555.2 636.8 408.5 132.6
      364.2 778.2 415.875711781643 258.270904218341 762.8 388.1 498.1 300.9 143 567.9 425.3 554.5 261.8 749.9
      155.2 445 1057.8 628.2 459.5 417.5 575.5 294.8 466.1 477.8 495 466.8 530.1 457.6 278.5 )
  ]
  if rain-site = "Mberengwa" [
    set rainfall-data (list 780.3 373.4 769.2 540.4 529.6 684.1 469.3 402.8 632.5 424.5 491.1 396.7 516.9 404.5
      598.1 250.5 457.4 316.3 371.2 696.4 239.8 619.5 850.1 582.8 642.1 635.9 457.7 628.6 765.9 241.1 148 409
      685.5 433.7 284.5 562 367.5 350.6 309.1 121.5 408.3 323 457.7 650.1 739.5 411.5 484.1115 986 605.7 499.6
      745 651.3 404.2 453.3 443.6 837.5 575.6 658.2 580.2 368.9 )
  ]
  if rain-site = "Zvishavane" [
    set rainfall-data (list 1041.8 427.3 758.6 579.4 619.3 702.1 550.4 413 659.8 537.7 718.2 349.3 547.6 505.8
       615.4 342.6 642.1 462.8 352.2 825.5 175.6 934.7 938.4 544.2 738.6 1034.9 584 639 855.4 360.4 285 476.4 762.4
       451.3 253.4 825.7 412.5 466.4 290.5 200.9 582.3 466.9 474.5 801.7 853.5 394.2 393.8 837.2 569.9 601.2 559.9
       660.8 491.5 727.8 430.6 645 609.2 449.5 734 464.3 )
  ]
  if rain-site = "Average" [
    set rainfall-data (list 978.266666666667 394.333333333333 754.766666666667 554.766666666667 578.5 630.566666666667
      603.433333333333 409.233333333333 640.5 476.566666666667 604.5 376.7 503.433333333333 466.2 636.6 276.466666666667
      569.066666666667 389.166666666667 405.666666666667 740.266666666667 210.466666666667 778 871.9 542.466666666667
      721.833333333333 746.466666666667 509.566666666667 607.6 752.7 336.666666666667 188.533333333333 416.533333333333
      742.033333333333 433.625237260548 265.390301406114 716.833333333333 389.366666666667 438.366666666667 300.166666666667
      155.133333333333 519.5 405.066666666667 495.566666666667 571.2 780.966666666667 320.3 440.9705 960.333333333333
      601.266666666667 520.1 574.133333333333 629.2 396.833333333333 549.066666666667 450.666666666667 659.166666666667
      550.533333333333 545.933333333333 590.6 370.566666666667 )
  ]

;;create the upper and lower quartiles of the rainfall data for the non-parametric boostrapping of more extreme rainfall
  ;  sort the rainfall by quantity
  set extreme-rainfall-data sort rainfall-data
  ;  cut out the middle quartiles
  set extreme-rainfall-data sentence ( sublist extreme-rainfall-data 0 14 ) ( sublist extreme-rainfall-data 45 60 )
  set extremity-factor 1.5                          ;;  Multiplier on the sd for rainfall normal distribution

;-----------------------------
; FENCE PARAMETERS FROM DATA
;-----------------------------
  set total-mud-crop-perimeter 50000                ;; 50,000 m of fence in Mudhomori in 2013 measured by research team
  if model-mode = "Experiment" [                    ;; set it to +/- 5% of its value
    set total-mud-crop-perimeter set-random-parameter-value-for-sensitivity-analysis total-mud-crop-perimeter 0.05 ]

  set wood-to-build-fence-per-meter 5.77            ;; 5.7 kg/m/year to build fence made of matyatya (brushwood) and
                                                    ;;   fence posts weighed in the field by the research team.
                                                    ;;   (this is corrected for the 0.67 loss from termites and
                                                    ;;   will be multiplied by how-many-fences later).
  if model-mode = "Experiment" [                    ;; set it to +/- 5% of its value
    set wood-to-build-fence-per-meter set-random-parameter-value-for-sensitivity-analysis wood-to-build-fence-per-meter 0.05]

  set termite-activity 0.67                         ;; We lose 67% of the fences every year as measured by field team
  if model-mode = "Experiment" [                    ;; set it to +/- 5% of its value
    set termite-activity set-random-parameter-value-for-sensitivity-analysis termite-activity 0.05 ]


;-----------------------------
; CROP PARAMETERS FROM DATA
;-----------------------------
  set hours-to-plough-ha 8                            ;; 4 cows take 1 day to plough a ha, from interviews with farmers, where
                                                    ;;   a work-day is about 2 hours, therefore 8 cow-hours to plough 1 ha
  if model-mode = "Experiment" [;; set it to +/- 5% of its value
    set hours-to-plough-ha set-random-parameter-value-for-sensitivity-analysis hours-to-plough-ha 0.05 ]

  set crop-growth-slope 0.8731                      ;; From a linear fit to field data from Mudhomori.  kg/mm/ha/yr
                                                    ;; These will get converted to ticks and patches later
  if model-mode = "Experiment" [                    ;; set it to +/- 5% of its value
    set crop-growth-slope set-random-parameter-value-for-sensitivity-analysis crop-growth-slope 0.05 ]

  set zero-crop-growth-intercept 232                ;; From a linear fit to field data from Mudhomori, the y-intercept is
                                                    ;;   -202.6552 kg/ha/yr, so with a slope of 0.8731, that means the minimum
                                                    ;;  rainfall to get any crop growth is 232 mm
  if model-mode = "Experiment" [                    ;; set it to +/- 5% of its value
    set zero-crop-growth-intercept set-random-parameter-value-for-sensitivity-analysis zero-crop-growth-intercept 0.05 ]

  ;; muonde-efficiency value is a guess.  In particular it may be scale-dependent. Ken says: "Those watching contours
  ;;  produce a heap of grass. The problem is what area to calculate as more productive: if it is the whole field
  ;;  and not just where the pond and contours are then i think we need to keep down to say three.  Locally within a
  ;;  field it is of course much higher." So if ha-per-patch becomes small enough, this factor might need to be higher.
  ;;  And in general it is an ongoing project of Muonde's to measure the effectiveness of the water harvesting techniques.
  set muonde-efficiency 3.0
    if model-mode = "Experiment" [                  ;; set it to +/- 10% of its value
    set muonde-efficiency set-random-parameter-value-for-sensitivity-analysis muonde-efficiency 0.10 ]


;-----------------------------
; WOODLAND PARAMETERS FROM DATA
;-----------------------------

  set woodland-growth-slope 3320 / mean ( rainfall-data )                ;; Ken estimates 3320 kg/ha/year for an average
                                                                         ;;  rainfall year.  Compare with Rutherford 1978:
                                                                         ;;  3000 kg/ha/yr.  Assuming linear growth
                                                                         ;;  accumulation with rainfall accumulation
                                                                         ;;  throughout the year gives us kg/ha/yr/mm of rain;
                                                                         ;;  see ODD for more details.
                                                                         ;; Later on we'll convert to patches and ticks
  if model-mode = "Experiment" [                    ;; set it to +/- 10% of its value
    set woodland-growth-slope set-random-parameter-value-for-sensitivity-analysis woodland-growth-slope 0.10]


;-----------------------------
; COW PARAMETERS FROM DATA/LITERATURE
;-----------------------------
  set cow-maintenance-energy-rate 980  ;; kcal/kg/day - Molden 2013 gives 11,000 kcal/day for a Tropical Livestock Unit)^0.75, so we
                                       ;;   raise to the inverse 0.75 power and divide by a TLU's mass(250 kg, by definition)
                                       ;;   to get kcal/kg/day. We multiply by body mass later in the code.
                                       ;; Note that this definition of maintenance energy includes energy for feeding/drinking,
                                       ;;   thermoregulation, energy lost to urine, a modest amount of movement, etc.
  if model-mode = "Experiment" [       ;; set it to +/- 5% of its value
    set cow-maintenance-energy-rate set-random-parameter-value-for-sensitivity-analysis cow-maintenance-energy-rate 0.05]

  set cow-maintenance-energy-per-tick-per-kg cow-maintenance-energy-rate / ticks-per-day
                                       ;; this is a derived variable, but it decreases computation time in many functions.
                                       ;; cow-maintenance-energy-rate is in (kcal expended by cow) / (kg body mass) / day.
                                       ;; Note that this will be multiplied by cow mass when we use it.

  set cow-working-energy-per-hour 920  ;; kcal/hr, from Astatke, Reed, and Butterworth 1986.  For a native zebu-type animal
                                       ;;   in Ethiopia they give 3.85 MJ/hr, we convert to kcal.
                                       ;; When we use this value, we will multiply it by hours-to-plough-ha and ha-per-patch.
                                       ;; It will then be in units of kcal per patch, a *work event* (ploughing of a whole patch),
                                       ;; not per tick or per hour. When we use the "to plough" function, we find a cow with this
                                       ;; much energy, and deduct the entire amount in one tick. The cow represents some combination
                                       ;; of cow biomass used for enabling the field to grow.
  if model-mode = "Experiment" [       ;; set it to +/- 5% of its value
    set cow-working-energy-per-hour set-random-parameter-value-for-sensitivity-analysis cow-working-energy-per-hour 0.05]

  set kcal-per-kg-of-browse 1931       ;; kcal/kg, from Astake, Reed, and Butterworth. This is a metabolizable energy, and so
                                       ;;   includes assimilation efficiency
  if model-mode = "Experiment" [       ;; set it to +/- 5% of its value
    set kcal-per-kg-of-browse set-random-parameter-value-for-sensitivity-analysis kcal-per-kg-of-browse 0.05]

  set kcal-per-kg-of-crop 2325         ;; From Ohio State University Extension, corn stover has a metabolizable energy of 0.79
                                       ;;   Mcal/lb and grain has an ME of 1.42 Mcal/lb.  From FAO's "Maize in human nutrition,"
                                       ;;   grain is 42% of the plant by dry weight, so collectively ME should be 1.05 Mcal/lb,
                                       ;;   or 2325 kcal/kg.  Because this is metabolizable energy, assimilation efficiency is included.
                                       ;;   https://dairy.osu.edu/newsletter/buckeye-dairy-news/volume-9-issue-3/full
  if model-mode = "Experiment" [       ;; set it to +/- 5% of its value
    set kcal-per-kg-of-crop set-random-parameter-value-for-sensitivity-analysis kcal-per-kg-of-crop 0.05]

  set kcal-per-kg-of-cow 1360          ;; kcal/kg for 5% fat ;; 1210-3320 kcal/1000 g; for 3-30% fat from USDA when a cow burns off
                                       ;;   body weight, how much energy does it get? also, when it builds more cow,
                                       ;;   how much cow does it make? http://ndb.nal.usda.gov/ndb/beef/show
  if model-mode = "Experiment" [       ;; set it to +/- 5% of its value
    set kcal-per-kg-of-cow set-random-parameter-value-for-sensitivity-analysis kcal-per-kg-of-cow 0.05 ]

  set production-efficiency 0.49       ;; Johnson et al 2012 give a value for building protein (0.48) and fat (0.71)
                                       ;;   with assumption 5% body fat, we weight them 95% and 5% to get 0.49
  if model-mode = "Experiment" [       ;; set it to +/- 5% of its value
    set production-efficiency set-random-parameter-value-for-sensitivity-analysis production-efficiency 0.05 ]

  set catabolism-efficiency 0.9025     ;; Johnson et al 2012 give a value for fat catabolism (0.95) and degrading
                                       ;;   protein (0.90), with 5% body fat we weight them 5% and 95% to get 0.9025
  if model-mode = "Experiment" [       ;; set it to +/- 5% of its value
    set catabolism-efficiency set-random-parameter-value-for-sensitivity-analysis catabolism-efficiency 0.05 ]

  set max-cow-mass 296                 ;; kg, from Machila et al (2008), though we've never weighed cattle in Mazvihwa
  if model-mode = "Experiment" [       ;; set it to +/- 5% of its value
    set max-cow-mass set-random-parameter-value-for-sensitivity-analysis max-cow-mass 0.05 ]

  set min-cow-mass 63                  ;; kg, from Machila et al (2008)
  if model-mode = "Experiment" [       ;; set it to +/- 5% of its value
    set min-cow-mass set-random-parameter-value-for-sensitivity-analysis min-cow-mass 0.05 ]

  set calf-birth-mass 18               ;; kg, mean weight of calves from Nicholson (1983); Machila's minimum is 29.5 kg
                                       ;;   but interviews indicate that's too high
  if model-mode = "Experiment" [       ;; set it to +/- 5% of its value
    set calf-birth-mass set-random-parameter-value-for-sensitivity-analysis calf-birth-mass 0.05 ]

  set min-calf-mass (                  ;; it takes seven days for a calf to starve (this is a derived variable used in multiple places)
    calf-birth-mass - 7 * ticks-per-day * (cow-maintenance-energy-per-tick-per-kg) * calf-birth-mass / kcal-per-kg-of-cow
  )

  ;; Cows have a .25-.35 percent chance of reproducing each year (50-60% of livestock are fertile females achieving
  ;; 80% pregnancy in high rainfall years down to 50% in poor years and few in drought years)
  ;; These numbers are from interviews with farmers
  ;; Chance of reproducing: 25% per year, so chance of NOT reproducing over a full year: 75%
  ;; Chance of not reproducing for one tick: 0.75 ^ (1 / ticks-per-year)
  set livestock-not-reproduction-rate-per-year 0.75
  if model-mode = "Experiment" [       ;; set it to +/- 5% of its value
    set livestock-not-reproduction-rate-per-year set-random-parameter-value-for-sensitivity-analysis
          livestock-not-reproduction-rate-per-year 0.05]

  set cost-of-supplemental-feed 14.50  ;; from farmer's interviews in 2015.
  ;; Used largely as a unit conversion, so we don't test its sensitivity

;-----------------------------
;; SUSTAINABILITY THRESHOLDS
;-----------------------------
;; no sensitivity analysis is done here, because we're using a scenario with and without the muonde thresholds

;; these are the Muonde-based thresholds, if use-muonde-thresholds is TRUE, then use these
ifelse use-muonde-thresholds [
 set minimum-cows-for-sustainability 50     ;; from Muonde, what is the minimum number of cows needed in a village like
                                            ;;   Mudhomori for sustainability?
 set minimum-harvest-for-sustainability 48  ;; from Muonde, what is the minimum harvest in a year in a village like Mudhomori
                                            ;;   for sustainability? (if how-long-to-store-grain is > 0, we check the average)
 set woodland-sustain-factor 1.00           ;; from Muonde, how many times the amount of fence in mudhomori do we need to keep
                                            ;;   in the woodlands? basically 1 times; they have 1/20th what is required now and
]                                           ;;   want to have 20 times that
[
;; if use-muonde-thresholds = FALSE, use these, which are thresholds that still allow many of the models to run.  The model needs
;;   1 cow, minimum, and because these thresholds are arbitrary, we use the same relative proportions as in the thresholds given
;;   by Muonde.  This means, because we are using 1/50th of the cows, we also use 1/50th of the woodland and harvest.
 set minimum-cows-for-sustainability 1       ;; min cow weighs 63 kg, minimum where the model will run. 1/50 of Muonde's threshold.
 set minimum-harvest-for-sustainability 0.96 ;; metric tons. 1/50 of Muonde's suggested threshold.
 set woodland-sustain-factor 0.02            ;; this results in a biomass of ~ 5.6 metric tons. 1/50 of Muonde's suggested threshold.

]
 ; minimum-woodland-for-sustainability is given in kg of biomass (per year):
 ;  total perimeter of all Mudhomori village crop fences (m) * / ( 100 * sqrt ( ha-per-patch ) ) (m/unit)
 ;     * wood to build fence (kg/unit/year) * sustainability factor
 set minimum-woodland-for-sustainability (total-mud-crop-perimeter * wood-to-build-fence-per-meter * woodland-sustain-factor)

;-----------------------------
;TRACKING VARIABLES
;-----------------------------
;; These variables are largely set to zero and will be updated after burn-in is over.

  set burn-in-length 5   ;; allows for up to 5 years of harvest to be averaged
  set burn-in true ;; assume the model is still burning-in until we know otherwise
  set termination-reason "Simulation has not yet terminated"

;; time trackers
  set years-gone 0
  set calendar-year 0
  set rainfall-index 0

;; crop trackers
  set crop-eaten 0 ;; crop eaten in current year
  set total-crop-eaten 0 ;; crop eaten over entire model run
  set percentage-harvest-eaten 0 ;; in previous year, how much crop was eaten
  set max-percentage-harvest-eaten 0
  set count-cows-in-crops 0 ;; keeps track of how many ticks a cow was in a crop

;; woodland trackers
  set woodland-eaten 0
  set max-woodland-mass 0 ;; this is definitely smaller than the max count of cows
  set min-woodland-mass 999999999 ;; this is definitely larger than the minimum amount of woodland

;; cow trackers
  set max-livestock-number 0 ;; this is definitely smaller than the max count of cows
  set min-livestock-number 999999 ;; this is definitely larger than the minimum count of cows
  set dead-cows 0
  set total-number-of-births 0 ;; count total number of cows born
  set total-cows 0  ;; keep track of every cow in every tick
  set subsidy-used 0

;; harvest trackers
  set current-harvest 0
  set previous-harvests-list []

;; reproductive rate and crop eaten trackers
  set crop-eaten-per-cow-per-half-hour 0
  set actual-reproductive-rate 0


end
;; end "set-global-variables"

;; Function: calculate-initial-number-of-livestock
;; Setup procedure: only called during initialization when user clicks setup button
;; Action: Calculate the carrying capacity of cows based on the proportion crops and key resources
to-report calculate-initial-number-of-livestock
  ;;-----------------------------
  ;; CARRYING CAPACITY CALCULATION
  ;;-----------------------------
  ;; The woodland squares of the setting produce a certain amount of biomass per unit time.
  ;; If the cows consume more than that much, they'll eventually go through any existing static resource pools, and die.
  ;; If the cows consume less, then they're still under the carrying capacity and could safely increase.
  ;; The carrying capacity is the number of (minumum-mass) cows for which the woodland biomass production is equal to the
  ;;   energy expended by the cows.
  let reference-cow-mass (max-cow-mass + min-cow-mass) / 2
  let woodland-count round ((1 - proportion-crops / 100) * (count patches))  ;; can't "count woodland" -- not set up yet!

  ;; supported cow mass per tick =
  ;;    (# woodland patches) * (average growth multiplier) * (reference woodland growth rate per tick) *
  ;;    (kcal-per-kg-of-browse) / (reference cow mass)
  ;; multiplier for normal woodland is 1, but for key resources is 4 (3 greater)
  ;; result is kg biomass per tick for the whole setting
  let cow-energy-per-tick-supported-by-biomass (
    woodland-count * (1 + 3 * (key-resources / 100)) *  ;; total patches' worth of biomass
    (mean(rainfall-data) * woodland-growth-slope * ha-per-patch / ticks-per-year ) * ;; how much woodland biomass (kg) in setting
    (kcal-per-kg-of-browse)  ;; now in kcal of energy available for a cow (per tick)
  )

  ;; this ignores energy to plough or give birth, so it's a little low -- but those are relatively rare events in a given year
  let cow-energy-per-tick-required-by-one-reference-cow (
    reference-cow-mass * cow-maintenance-energy-per-tick-per-kg ;; energy for maintenance for a reference cow
  )
  let carrying-capacity cow-energy-per-tick-supported-by-biomass / cow-energy-per-tick-required-by-one-reference-cow

  ;; just starting it at carrying-capacity, but make sure there's at least one cow
  report max (list 1 ( round ( carrying-capacity ) ) )
end

;; Function: setup
;; Setup procedure: only called during initialization when user clicks setup button
;; Action: Clear screen, set world size and parameters, and create crops and woodlands based on spatial parameters
to setup

  if model-mode = "SoftwareTests" [
    run-software-tests
    stop
  ]

  reset-timer                             ;; start the timer in seconds at the beginning of setup
  clear-all                               ;; clear output, patches, plots, turtles, globals
  reset-ticks                             ;; Reset tick counter
  set running false                       ;; we're not running yet (and if we're in SoftwareTests mode, we never will be)

  ;; Note that Mudhomori village is 600 hectares in size.  Also note that ticks-per-year must be an integer because
  ;;   we rely on the modulus operator to determine when the calendar year should change.  a world size of 50 means
  ;;   each patch is approximately a quarter hectare, and 3 ticks per day means a tick is 8 hours.  We get realistic
  ;;   results for these parameters.  The sensitivity of the temporal and spatial scaling could be tested, but the
  ;;   user should be aware that the rate at which a cow moves across the landscape when grazing should remain
  ;;   realistic, as should the amount of time it takes to plough a field (though the energy is deducted all at once).
  set world-size 50
  set ticks-per-day 3
  resize-world 0 world-size 0 world-size  ;; set world size

  ;; In Experiment mode, we vary proportion crops randomly from 1 to 97 percent. (above this cows don't survive past 3 ticks)
  if model-mode = "Experiment" [
    set proportion-crops 1 + random 96
  ]

  set-global-variables    ;; do this after setting the proportion crops in order to get the correct carrying-capacity

  make-crops

  set-patch-variables

  ;; crops-with-fences is actually "crops that *should* have fences, but
  ;;   termites happen".  We'll iterate over all these, have termites eat their
  ;;   fences, and try to rebuild.  we'll iterate over them to set up the fences initially too.
  set crops-with-fences crops with [any? neighbors4 with [is-woodland? = true]]

  configure-fences

  calculate-landscape-metrics

  set-faster-growing-patches

  ;; Create the given number of cows (from the carrying capacity calculation reporter)
  create-cows calculate-initial-number-of-livestock [setup-cow false ]
  ;; calf = false, initally set up cows are mean weight

  set running true                 ;; we are now allowed to run when "go" is executed
  set model-setup-time timer
  reset-timer                      ;; start the timer in seconds at the end of setup


end

;; Function: set-patch-variables
;; Setup procedure: only called during initialization when user clicks setup button
;; Action: Set initial patch values for crops and woodlands
to set-patch-variables

  ask patches [
    set fence 0
    set growth-multiplier 1
    set moran-numerator 0
    set geary-numerator 0
    set moran-geary-denominator 0
    set plabel ""
  ]

  ;; Differentiate crops from woodland: color, growth rate, fences.  Note that is-crop? was set while creating crops.
  ask crops [
    set pcolor 55
    set standing-available-biomass 0 ;; start at 0, but in experiments there will be 5 years stored from burn-in
    set is-woodland? false
    set how-many-fences 0
  ]

  ask woodlands [
    set pcolor 35
    set standing-available-biomass (random-float 0.05 )  ;; cows at carrying capacity efficiently remove woodland, keeping
        * woodland-growth-slope * mean(rainfall-data) * ha-per-patch                         ;;    it pretty low. we set it to 5% of one average year's growth.
    set is-woodland? true
  ]

end

;; Function: configure-fences
;; Setup procedure: only called during initialization when user clicks setup button
;; Action: Set the how-many-fences patch variable of crop patches, set symbology appropriately
to configure-fences
  ask crops-with-fences [
    set how-many-fences count neighbors4 with [is-woodland? = true]
    ifelse invincible-fences [
      set fence 1
      set plabel "s"
    ] [
      ;; else not invincible fence, set it randomly and label appropriately
      set fence random-float 1
      ;; start fences somewhere between 0 and full, so there isn't a cycle of fence rebuilding based on initial conditions
      if fence > 0 [
        set plabel-color yellow
        set plabel "X"
      ]
    ]
  ]
end

;; Function: calculate-landscape-metrics
;; Setup procedure: only called during initialization when user clicks setup button
;; Action: Calculate the four landscape metrics -- total crop perimeter, Moran's I, Geary's C, and average landscape patch size
to calculate-landscape-metrics

  if count woodlands = 0 or count crops = 0 [
    ;; Aside from not reflecting reality, this would cause Moran's I calculations to divide by zero.
    error "Landscape cannot be all woodlands or all crops"
  ]

  ;; total length of fence/perimeter of crop
  set total-crop-perimeter (sum [how-many-fences] of crops-with-fences) * 100 * sqrt ( ha-per-patch )


  ;; calculate summand of Moran's I and Geary's C calculation using rook's neighborhood
  ;;   Moran's I = (count patches) / (count neighbors-of-all-patches) *
  ;;               sum over all patches of {
  ;;                   sum over all this-patch's 2-or-3-or-4 neighbors of {
  ;;                       (this-patch's numeric-is-crop - proportion-crops/100) *
  ;;                       (neighbor-patch's numeric-is-crop - proportion-crops/100)
  ;;               }} /
  ;;               sum-over-all-patches of { (this-patch's numeric-is-crop - proportion-crops/100)^2 }
  ;;   Geary's C = (count patches - 1) / (2 * count neighbors-of-all-patches) *
  ;;               sum over all patches of {
  ;;                   sum over all this-patch's 2-or-3-or-4 neighbors of {
  ;;                       (this-patch's numeric-is-crop - other-patch's numeric-is-crop)^2
  ;;               }} /
  ;;               sum-over-all-patches of { (this-patch's numeric-is-crop - proportion-crops/100)^2 }
  ;;
  ;;  We'll have each patch calculate its own numerator (sum over my neighbors of...), and its own denominator,
  ;;  (numeric-is-crop - proportion-crops/100)^2 -- and we'll sum these all up at the end.
  ;;
  ask patches [
    let numeric-is-crop (ifelse-value is-crop? [1] [0])
    let my-moran numeric-is-crop - (proportion-crops / 100)
        ;; x_i - x-bar (x_i = 1 for crop, 0 for woodland, and x-bar is the proportion-crops)
    set moran-geary-denominator my-moran * my-moran
    let temp-moran-numerator 0
    let temp-geary-numerator 0

    ask neighbors4 [
      let your-numeric-is-crop (ifelse-value is-crop? [1] [0])
      let your-moran your-numeric-is-crop - (proportion-crops / 100)
      set temp-geary-numerator temp-geary-numerator + (numeric-is-crop - your-numeric-is-crop) ^ 2
      set temp-moran-numerator temp-moran-numerator + my-moran * your-moran
    ]
    set moran-numerator temp-moran-numerator
    set geary-numerator temp-geary-numerator
  ]

  ;; Moran's I is multiplied by N (here world-size squared) and divided by the sum of the weights (sum of the neighbor-counts).
  ;; Most patches have 4 neighbors, except for edges which have one fewer, and corners which have two fewer.
  ;; So the total neighbor count is 4*world-size*world-size - (4*world-size - 4) (edges) - 4 (corners)
  ;;    = 4*world-size*world-size - 4*world-size
  ;;    = world-size * (4*(world-size - 1))
  ;;
  ;; And the fraction (count patches) / (count neighbors-of-all-patches) is equal to
  ;;     (world-size * world-size) / (world-size * (4*(world-size - 1)))
  ;;     = world-size / (4*(world-size - 1))
  ;;
  let temp-world-size 1 + max-pxcor  ; unfortunately, we can't manually set world-size during testing...
  set morans-i (
    (temp-world-size / (4 * (temp-world-size - 1 ))) *
        (sum [moran-numerator] of patches) / (sum [moran-geary-denominator] of patches))

  ;; Geary's C has a leading factor of N-1 (here world-size squared minus 1) over double the sum of the weights (double the
  ;; sum of the neighbor-counts):
  ;;   (N-1) / 2W = (world-size * world-size - 1) / (2 * world-size * (4*(world-size - 1)))
  ;;              = (world-size + 1)(world-size - 1) / 8(world-size)(world-size - 1) = (world-size + 1) / 8*world-size
  set gearys-c (temp-world-size + 1) / (8 * temp-world-size) * (
      (sum [geary-numerator] of patches) / (sum [moran-geary-denominator] of patches))

  set average-contiguous-crop-cluster-size calculate-average-contiguous-crop-cluster-size * ha-per-patch
  ;; call the reporter function calculate-average-contiguous-crop-cluster-size and store it in average-contiguous-crop-cluster-size
  ;;   Note that the result is in hectares

end

;; Function (reporter): calculate-average-contiguous-crop-cluster-size
;; Update procedure: called during "Go" phase of simulation
;; Note: In landscape ecology, "patch" means a contiguous region of land of the same type.  However, in NetLogo, "patch" refers
;;       to the smallest spatial unit.  To avoid confusion, all uses of "patch" mean "NetLogo patch", except where we specifically
;;       say "landscape patch".
;; Return value: the average size, in number of NetLogo patches, of all contiguous clusters of crops
;                A cluster is a set of all contiguous crops.  This function identifies all clusters, determines their
;                sizes, and reports the mean of the sizes.
to-report calculate-average-contiguous-crop-cluster-size
  if count crops = 0 [ report 0 ]

  let cluster-sizes []
  let remaining-crops crops
  while [remaining-crops != no-patches] [
    ; While there are remaining crops that haven't been identified as part of a cluster yet:
    ;  * pick one of those remaining crops
    ;  * the "current cluster" is now a one-element list of that crop
    ;  * grow the current cluster until it contains all contiguous crops (see below)
    ;  * add the current cluster size to the list of sizes
    ;  * remove all crops in the current cluster from the remaining-crops
    let current-cluster (list one-of remaining-crops)
    let index 0
    while [index < length current-cluster] [
      ; In this loop, we grow the current cluster until it contains all contiguous crops.
      ; Starting with a one-crop list, and an index pointing to the first/only crop:
      ;  * find all contiguous crop patches to the current crop
      ;  * from that list, select only crop patches that are not already in the current cluster
      ;  * add each new contiguous crop patch to the current cluster, lengthening the list without moving the index
      ;  * now increment the index
      ;  * if the index reaches the end of the list, there are no more contiguous crops to process, so we're done
      let current-crop (item index current-cluster)
      let contiguous-patches [neighbors4 with [is-crop?]] of current-crop
      let new-contiguous-patches contiguous-patches with [not member? self current-cluster]
      ask new-contiguous-patches [set current-cluster lput self current-cluster]
      set index index + 1
    ]
    set cluster-sizes lput (length current-cluster) cluster-sizes
    set remaining-crops remaining-crops with [not member? self current-cluster]
  ]
  report mean cluster-sizes
end

;; Function: set-faster-growing-patches
;; Setup procedure: only called during initialization when user clicks setup button
;; Action: Set key-resources in woodlands and muonde projects in crops to faster growth rate and update symbology
to set-faster-growing-patches

  ;; Change the growth rate of crop patches with Muonde projects on them
  let n_muonde round ( (0.499 + (count crops) * muonde-projects / 100 ) )
  ask n-of n_muonde crops [
    set growth-multiplier muonde-efficiency ;; crops with Muonde projects grow faster than the rest of the farmland
    set plabel "M" ;; indicate the Muonde projects on the map with "M"
  ]

  ;; Changes the growth rate of key resources
  let n_key round ( (0.499 + (count woodlands) * key-resources / 100 ) ) ;; round truncates, so add 0.499 to make it round up?
                                                                         ;; see below for number of cows
  ask n-of n_key woodlands [
    set growth-multiplier 4   ;; key resources grow 4 times faster than the rest of the woodland, see Scoones et al
    set plabel "*"            ;; display the key resources on the map with a "*"
  ]

end


;; Function: make-crops
;; Setup procedure: only called during initialization when user clicks setup button
;; Action: Distributes crops and woodland according to proportion and clumpiness
to make-crops

  ;; Calculates the number of crops
  let number-of-crops round (proportion-crops * (count patches) / 100)

  ;; set all patches to woodland
  ask patches [ set is-crop? FALSE ]

  ;; define how many clumped and non-clumped fields, based on clumpiness
  let clumped round (number-of-crops * clumpiness)
  let non-clumped number-of-crops - clumped

  make-crop-clumps clumped non-clumped

  set crops patches with [is-crop? = TRUE]
  set woodlands patches with [is-crop? = FALSE]

end

;; Function: make-crop-clumps
;; Setup procedure: only called during initialization when user clicks setup button
;; Action: Creates clumps of crop patches based on clumpiness by 'growing' clumps from neighbors.
;;         This will turn some patches green (making them crop fields).
;; Input parameters:
;;   clumped (integer): (approximate) number of crop patches which will be contiguous (adjacent)
;;   non-clumped (integer): (approximate) number of crop patches which will be NON-contiguous (disjoint)
;; Prerequisites: Caller must execute "ask patches [ set is-crop? FALSE ]"
;; The number of crop patches will be the sum <clumped-fields> + <unclumped-fields>
;; The clumped fields will all be adjacent.
;; The unclumped fields will start disjoint, but may become adjacent during clumping.
;;
to make-crop-clumps [clumped non-clumped]
  ;; put the non-clumped fields first. they're all, if possible, put in places where no
  ;; neighbors are crops: neighbors4 [is-crop? = FALSE] as long as possible. these do not
  ;; increase auto-correlation: their neighbors are not crops
  let i 0
  while [i < non-clumped] [
    ;; locate woodland patches with at least one woodland neighbor
    let woodland-with-at-least-one-woodland-neighbor patches with
        [ is-crop? = FALSE and any? neighbors4 with [is-crop? = FALSE] ]
    ;; and among those, woodland patches with all woodland neighbors/no crop neighbors
    let woodland-with-no-crop-neighbors woodland-with-at-least-one-woodland-neighbor with
        [ all? neighbors4 [is-crop? = FALSE] ]
      ;; if there are any woodland patches with only woodland neighbors, pick one and make it crop.
      ;;   This creates the chessboard-like pattern.
    ifelse any? woodland-with-no-crop-neighbors [
        ask one-of woodland-with-no-crop-neighbors [ set is-crop? TRUE ]
      ] [
        ;; otherwise, if there are any woodland patches with *at least one* woodland neighbor,
        ;;   pick one and make it crop.  This fills in the checkerboard if there are more than
        ;;   50% crops, but is evenly distributed in order to keep it as non-clumpy as possible.
        ifelse any? woodland-with-at-least-one-woodland-neighbor [
          ask one-of woodland-with-at-least-one-woodland-neighbor [set is-crop? TRUE ]
        ] [
          ;; and if you still haven't got through all the 'non-clumped', just make a woodland patch crop
          ask one-of patches with [ is-crop? = FALSE ] [ set is-crop? TRUE ]
        ]
      ]
      set i i + 1
  ]
  ;; now, put all the clumped fields, that is, fields whose neighbors are crops: neighbors4 [is-crop? TRUE]
  ;; if possible, they go to places where all neighbors are crops, otherwise, they go where at least one
  ;; neighbor is crop too. These increase the auto-correlation, cause they're put together with similar neighbors
  ;; (Note: same structure as above, only this time we're choosing patches where neighbors *are* crops)
  set i 0
  while [i < clumped] [
    ;; find woodlands with at least one crop neighbor
    let woodlands-with-any-crop-neighbor patches with
      [ is-crop? = FALSE and any? neighbors4 with [is-crop? = TRUE] ]
    ;; and among those, woodlands with all crop neighbors
    let woodlands-with-all-crop-neighbors woodlands-with-any-crop-neighbor with
      [ is-crop? = FALSE and all? neighbors4 [is-crop? = TRUE] ]
    ;; find a woodland with all crop neighbors and make it a crop
    ifelse any? ( woodlands-with-all-crop-neighbors ) [
      ask one-of woodlands-with-all-crop-neighbors [ set is-crop? TRUE ]
    ] [
    ;; if you don't have any crop-surrounded woodlands, find one with at least one crop neighbor and make it crop.
    ifelse any? ( woodlands-with-any-crop-neighbor ) [
      ask one-of woodlands-with-any-crop-neighbor [ set is-crop? TRUE ]
        ] [
          ;; and if you still haven't got through all the 'clumped', just make a woodland patch into crop
          ask one-of patches with [ is-crop? = FALSE ] [ set is-crop? TRUE ]
        ]
      ]
      set i i + 1
  ]
end

;; Function: setup-cow
;; Setup procedure: only called during initialization when user clicks setup button
;; Action: Initializes variables for a cow
;; Input parameters:
;;   newborn-cow (boolean): true if we're creating a calf (smaller body mass), false if we're creating an adult cow
;; Context: Must be called in a cow context
;;
to setup-cow [newborn-cow]
  ;; Sets initial position, shape, color, and mass state
  move-to one-of woodlands
  set shape "cow"
  set color white
  set size 4
  set satiety 0.51
  set is-subsidized? false
  ifelse newborn-cow = true [
    set is-calf? true
    set size 2 ;; calves are visually half-sized
    set body-mass calf-birth-mass
  ][
    set is-calf? false
    set body-mass ( max-cow-mass + min-cow-mass ) / 2 ;; initially set up cows are average mass
  ]
    set energy-this-tick body-mass * cow-maintenance-energy-rate ;; start them with a tick's worth of maintenance energy
end

;;---------------------------------------------------------------
;;---------------------------------------------------------------
;;                   UPDATE PROCEDURES
;;---------------------------------------------------------------
;;---------------------------------------------------------------

;; Function: finish-running-and-clean-up
;; Update procedure: called during "Go" phase of simulation
;; Action: When simulation is ended either by reaching the end of the run or hitting a stopping condition,
;;         display reasons and values of tracking variables, make sure model won't run unless 'setup' is clicked again.
;; Input parameters:
;;   reason (string): text to display to user explaining why the simulation has ended
;;
to finish-running-and-clean-up [reason]

  set termination-reason reason

  ;; calculate the actual reproductive rate based on variables tracked throughout the model run: accumulated total cows each tick
  ;;   and actual births each tick, corrected for ticks-per-year
  ;; this variable only makes sense if we terminate at the end of a year, which we always do
 if (years-gone > 0 and total-cows > 0) [
    set actual-reproductive-rate calculate-actual-reproductive-rate
 ]
 ;; and calculate the amount of crops cows ate per half hour per cow for comparison with field data
 if (count-cows-in-crops > 0) [
   set crop-eaten-per-cow-per-half-hour calculate-crop-eaten-per-cow-per-half-hour
 ]

  if model-mode = "Demonstration" [
    output-print (word "END: Halting simulation: " reason)
    ;; write out some values we want to record for any run
    ;;
    output-print "   Final variable values:"
    output-print (word "    *  calendar-year:                   " calendar-year)
    output-print (word "    *  years-gone:                      " years-gone)
    output-print (word "    *  total-harvest:                   " total-harvest)
    output-print (word "    *  subsidy-used:                    " subsidy-used)
    output-print (word "    *  dead-cows:                       " dead-cows)
    output-print (word "    *  max-livestock-number:            " max-livestock-number)
    output-print (word "    *  min-livestock-number:            " min-livestock-number)
    output-print (word "    *  max-woodland-mass:               " max-woodland-mass)
    output-print (word "    *  min-woodland-mass:               " min-woodland-mass)
    output-print (word "    *  actual-reproductive-rate:        " actual-reproductive-rate)
    output-print (word "    *  crop-eaten-per-cow-per-half-hour " crop-eaten-per-cow-per-half-hour)
    if length (previous-harvests-list) > 0 [
      output-print (word "    *  mean previous-harvests-list      " mean (previous-harvests-list) )
    ]
  ]

  ;; make sure that clicking GO again won't cause the simulation to keep going
  set running false  ;; you'll have to run 'setup' again if you want to run the model again
  display ;; if you turned off the display because you were in experiment mode, make sure to turn it back on
end

;; Function: calculate-actual-reproductive-rate
;; Finalizing procedure: called during "Go" phase of simulation by "finish-running-and-clean-up"
;; Action: called at the end of a run to calculate the actual reproductive rate of the cows
;;    which can be compared with the nominal rate, which is:
;;    (1 - (livestock-not-reproduction-rate-per-year ^ (1.0 / ticks-per-year)) ) * ticks-per-year
;;    This is worth checking because not every cow is available to reproduce in every year.
to-report calculate-actual-reproductive-rate
  set actual-reproductive-rate 0
    report total-number-of-births / (total-cows / ( ticks-per-year * years-gone ) ) / years-gone
end

;; Function: calculate-crop-eaten-per-cow-per-half-hour
;; Finalizing procedure: called during "Go" phase of simulation by "finish-running-and-clean-up"
;; Action: called at the end of a run to calculate the amount of crop eaten per half hour per cow, for
;;     checking against Muonde's estimates of how much livestock eat when they break into crop fields
to-report calculate-crop-eaten-per-cow-per-half-hour
    report (total-crop-eaten * 1000) * (1 / count-cows-in-crops) * ticks-per-day * (1 / 24) * (1 / 2)
end

;; Function: go
;; Update procedure: called during "Go" phase of simulation
;; Action: Updates the simulation. Each step is done through a separate procedure
to go
  if not running [
    ;; some condition caused the simulation to terminate: out of rainfall data, all cows died, etc.
    ;; can't "go" again without re-running setup
    ;; using 'stop' to exit from the 'go' function is the only way to stop the entire simulation
    stop
  ]

  ;; we won't wait patiently for burn-in to finish when people are watching us. :)
  ifelse model-mode = "Demonstration" [set burn-in false][no-display]
  ;; If they're not watching us, don't bother updating the display.

  update-max-min                  ;; updating reporters every tick

  if (ticks mod ticks-per-year = 0) [
    check-burn-in-and-update-year
    check-timeseries-length     ;; when we reach 60 years, stop.
    if not running [stop] ;; if we've reached the end of the timeseries, pass the 'stop' command back to 'go'

    update-yearly-time-events       ;; check number of cows and amount of woodland for sustainability,
                                    ;;   change rainfall and growth rates, set subsidized cows, flag
                                    ;;   burn-in false after burn-in-length is complete
     if not running [stop] ;; in case there weren't enough cows or woodland, carry the 'stop' command back to 'go'
  ]
  update-cows                       ;; cows move, consume, are subsidized, reproduce; must be called before
                                    ;;   update-crops in order to set-available-cows
  update-crops                      ;; get ploughed, grow, harvest (once per year)
    if not running [stop]  ;; if there isn't enough harvest, pass the 'stop' command back to 'go'
  update-woodland-available-biomass ;; update the growth state of the woodlands

  advance-time-step                 ;; tick, and check ticks mod ticks-per-year in order to advance years-gone

end

;; Function: advance-time-step
;; Update procedure: called during "Go" phase of simulation
;; Action: advances the model time step using NetLogo primitive 'tick' and checks to see if the calendar year should be advanced
to advance-time-step
  tick ;; advance the model one time step

  if (ticks mod ticks-per-year = 0) [ ;; update the years-gone at the end of the year
    set years-gone years-gone + 1
  ]
end

;; Function: check-timeseries-length
;; Update procedure: called during "Go" phase of simulation
;; Action: checks to see if we've run the model for 60 years or reached the end of the historical timeseries
;;         stops the simulation if we've reached 60 years.
to check-timeseries-length
  ;; Stop immediately if we've reached the end of the timeseries
  if ( years-gone >= length ( rainfall-data ) ) [
    finish-running-and-clean-up "rainfall time-series finished"
    set model-run-time timer
    if(model-mode != "Experiment") [
      output-print (word "    *  model-run-time:                  " model-run-time)
    ]
    stop
  ]
end

;; Function: update-yearly-time-events
;; Update procedure: called during "Go" phase of simulation
;; Action: Updates the time, including things that change yearly -- rainfall/growth rate, subsidy,
;;         calendar year, and checks stopping conditions.
to update-yearly-time-events

    check-cow-woodland-thresholds
    if not running [stop] ;; in case there weren't enough cows or woodlands, carry the 'stop' back to 'go'
    get-new-rainfall
    set-new-growth-rates

    ;; this is also the place to assign a proportion of the cows to subsidize; note that "subsidized"
    ;;   will always be false if subsidy is 'no' because cows are set up as is-subsidized?=FALSE
    if subsidy != "no" [
      set-subsidized-cows
    ]
end

;; Function: check-burn-in-and-update-year
;; Update procedure: called during "Go" phase of simulation
;; Action: checks if we've reached burn-in-length and updates years-gone and calendar year accordingly
to check-burn-in-and-update-year
  ;; note that we run the simulation for (burn-in-length) years to let the transients fade
  ;; before starting the actual simulation (especially important with variable rainfall)
  if burn-in = true and years-gone >= burn-in-length [
    set burn-in false
    set years-gone 0
  ]
  if burn-in = false [
    set calendar-year 1952 + ( years-gone )  ;; years based on the rainfall timeseries
  ]
end

;; Function: check-cow-woodland-thresholds
;; Update procedure: called during "Go" phase of simulation
;; Action: checks if cows and woodlands are sustainable (abundant enough) and stops the simulation if they aren't.
to check-cow-woodland-thresholds
  if (burn-in = false) [ ;; only check these conditions if burn-in is over
    ;; check stopping conditions for too few cows
    if ( (count cows) < minimum-cows-for-sustainability ) [
      finish-running-and-clean-up "Too few cows"
      set model-run-time timer
      if(model-mode != "Experiment") [
        output-print (word "    *  model-run-time:                  " model-run-time)
      ]
      stop
    ]
    ;; check stopping conditions for too little woodland (in kg)
    if ( sum [standing-available-biomass] of woodlands < minimum-woodland-for-sustainability) [
      finish-running-and-clean-up "too little woodland"
      set model-run-time timer
      if(model-mode != "Experiment") [
        output-print (word "    *  model-run-time:                  " model-run-time)
      ]
      stop
    ]
  ]
end

;; Function: set-subsidized-cows
;; Update procedure: called during "Go" phase of simulation
;; Action: Chooses a number of cows and marks them for subsidy (setting subsidy=FALSE when rainfall is high enough)
to set-subsidized-cows
  ask cows [ set is-subsidized? false ] ;; if we are subsidizing, start by resetting them to not be subsidized
  if rainfall < 400 [   ;; Ken says we only subsidize in a bad year when rainfall < 400 mm
    let num-cows-to-save round ( 0.499 + cow-proportion-to-save * (count cows) )
    ;; rounding up to the next whole cow (whole cows are where you get whole milk from)
    ;; but don't round up to 1 cow if 0 cows are alive!  (thus 0.499 instead of 0.5)
    ;; "round" actually truncates ( round 5.01 == round 5.99 == 5 ) so by adding 0.499, we turn it into true rounding
    ask n-of num-cows-to-save cows [
      set is-subsidized? true
    ]
  ]
end

;; Function: get-new-rainfall
;; Update procedure: called during "Go" phase of simulation
;; Action: Advances the rainfall index to get the next rainfall level from the data sequence, uses mean
;;         rainfall, or resamples rainfall from historical sequence
to get-new-rainfall
  ifelse burn-in = true [; use the mean rainfall
    set rainfall mean rainfall-data
  ] [
    ;if not burn-in, then do something interesting with the rainfall
    if rainfall-type = "historical" [ ; use the actual rainfall time-series
      set rainfall (item rainfall-index rainfall-data)
    ]
    if rainfall-type = "constant" [
      set rainfall mean rainfall-data ; use the mean rainfall
    ]
    if rainfall-type = "random" [ ; sampling with replacement from the historical data
      set rainfall one-of rainfall-data
    ]
    if rainfall-type = "statistical-random" [
      set rainfall random-normal (mean rainfall-data ) ( standard-deviation rainfall-data )  ; use a normal distribution
      if rainfall < 0 [set rainfall 0]  ;; we do truncate the distribution at zero
    ]
    if rainfall-type = "extreme" [ ; sampling with replacement from just the upper and lower quartiles of the historical data
      set rainfall one-of extreme-rainfall-data
    ]
    if rainfall-type = "statistical-extreme" [
      ; use a normal distribution with extremity-factor times the SD of the real data --
      ;; climate change could make extreme behavior more likely
      set rainfall random-normal (mean rainfall-data ) (extremity-factor * ( standard-deviation rainfall-data ) )
      if rainfall < 0 [set rainfall 0]  ;; we do truncate the distribution at zero
    ]
    set rainfall-index rainfall-index + 1
  ]
end

;; Function: set-new-growth-rates
;; Update procedure: called during "Go" phase of simulation
;; Action: Set the growth rates for crops and woodlands appropriately, based on the current rainfall
;;         converting raw rainfall into (net) primary productivity kg/ha (biomass is accumulated plant capital)
to set-new-growth-rates

  ask woodlands [
     set growth-rate growth-multiplier * (rainfall * woodland-growth-slope ) * ha-per-patch / ticks-per-year
  ]

  ask crops [
    ifelse rainfall > zero-crop-growth-intercept [
      set growth-rate growth-multiplier * (
        (rainfall - zero-crop-growth-intercept) * crop-growth-slope ) * ha-per-patch / ticks-per-year
    ][
      ;; if too little rainfall, no crop growth
      set growth-rate 0
    ]
  ]

end

;; Function: update-max-min
;; Update procedure: called during "Go" phase of simulation
;; Action: Update all the max-min monitor variables
to update-max-min
  ;; Start only after the (burn-in-length)th year to allow the transients to die out
  if (burn-in = false) [

    set max-livestock-number max (list max-livestock-number (count cows))
    set min-livestock-number min (list min-livestock-number (count cows))

    ;; divide by 1000 to get metric tons
    set max-woodland-mass ( max (list max-woodland-mass ((sum [standing-available-biomass] of woodlands) / 1000)) )
    set min-woodland-mass ( min (list min-woodland-mass ((sum [standing-available-biomass] of woodlands) / 1000)) )
  ]
end

;; Function: update-woodland-available-biomass
;; Update procedure: called during "Go" phase of simulation
;; Action: Update woodlands' growth states
to update-woodland-available-biomass
  let max-woodland-biomass-color  ( 5 * woodland-growth-slope * mean(rainfall-data) * ha-per-patch )
  ask woodlands [
    grow
    if model-mode != "Experiment" [
      set pcolor 30 + 5 * (min (list standing-available-biomass  max-woodland-biomass-color) /
       max-woodland-biomass-color)
      ;; this means that the color will saturate when the woodland has more than 5 years of growth built up
      ;; We think it actually takes more than 5 years' worth of accumulated yearly growth before we hit
      ;;  the max (based on 2013 satellite images of Rio Tinto's Murowa diamond mine where exclusion has
      ;;  facilitated regrowth since approximately 2004), and on local knowledge.
    ]
  ]
end

;; Function (reporter): cow-mass-change-from-current-energy
;; Update procedure: called during "Go" phase of simulation
;; Action: Converts the cow's energy pool to a mass change (but doesn't actually change the cow's mass)
;;         Makes use of the cows-own energy pool and converts to mass, penalized properly depending on if it's gain/loss
;; Context: Must be called in a cow context
to-report cow-mass-change-from-current-energy
    ;; if energy pool is positive, add mass (subject to production efficiency)
    ;; if energy pool is negative, subtract mass (subject to catabolism efficiency)
    let change-in-kg 0
    ifelse energy-this-tick > 0 [
      ; production efficiency (< 1.0) is kcal that will actually add weight / kcal intended to add weight
      ; energy-this-tick (> 0) is kcal intended to add weight
      ; change-in-kg should always be worse (less positive) than it would be with perfect production-efficiency of 1.0
      set change-in-kg energy-this-tick * production-efficiency / kcal-per-kg-of-cow
    ] [
      ; catabolism-efficiency (< 1.0) is kcal produced from body mass / kcal theoretically available in body mass
      ; kcal-per-kg-of-cow is kcal theoretically available in body mass
      ; change-in-kg should always be worse (more negative) than it would be with perfect catabolism-efficiency of 1.0
      set change-in-kg energy-this-tick / (catabolism-efficiency * kcal-per-kg-of-cow)
    ]
    report change-in-kg
end

;; Function: convert-cow-energy-pool-to-body-mass-change
;; Update procedure: called during "Go" phase of simulation
;; Action: Change the cow's body-mass and satiety simultaneously.
;;         Since changing a cow's body mass should change its satiety level, we should always call
;;            convert-cow-energy-pool-to-body-mass-change to ensure that both variables are set.
;;            This is done at the same time as the energy-pool accounting.
;;         This is done at the end of a tick to take the results of the energy accounting and apply it to weight gain/loss
;;         Calls convert-cow-energy-pool-to-mass-change to properly penalize depending on weight gain/loss
;; Context: Must be called in a cow context
to convert-cow-energy-pool-to-body-mass-change
  ;; calculate how much the cow's current energy surplus/deficit should change the mass
  let change-in-kg cow-mass-change-from-current-energy
  set body-mass body-mass + change-in-kg   ;; actually change the cow's mass
  set energy-this-tick 0  ;; reset the energy accounting
  let min-mass ifelse-value is-calf? [min-calf-mass] [min-cow-mass] ;; to properly scale satiety based on cow/calf status
  if (body-mass > max-cow-mass) [ set body-mass max-cow-mass ]  ;; cows and calves have the same max mass, can't go higher
  set satiety ( body-mass - min-mass ) / ( max-cow-mass - min-mass )  ;; set satiety here
  if satiety < 0 [set satiety 0]  ;; make sure it's not less than 0
end

;; Function: adjust-energy
;; Update procedure: called during "Go" phase of simulation
;; Action: Change the cow's energy pool
;; Input parameters:
;;   change-in-kcal (float): value to add to the cow's energy pool (positive for eating, negative for expending energy)
;; Context: Must be called in a cow context
to adjust-energy [change-in-kcal]
  set energy-this-tick energy-this-tick + change-in-kcal
end

;; Function: update-crops
;; Update procedure: called during "Go" phase of simulation
;; Action: Run all procedures that affect crop patches
to update-crops

 let max-crop-biomass-color 784 * ha-per-patch
 ;; In kg/patch, 784 kg/ha is max harvest in actual data, so in the best rainfall year with no interference, crops should
 ;;  reach this level by the end of the year. with muonde projects, it can go higher.

 ;; termites act on fences, fences get repaired using woodland biomass
  if not invincible-fences [
    update-fences
  ]
  ask crops [
    ;; If standing-available-biomass is 0, a cow with enough energy to work is needed to plough it
    ifelse standing-available-biomass <= 0 [
      plough
    ][
      grow
    ]
    if model-mode != "Experiment" [
      update-crop-color-symbol (max-crop-biomass-color)
    ]
  ]
  ;; If it's the end of a year, harvest whatever is there (but don't do it in the first tick)
  if ( (ticks mod ticks-per-year = 0 ) and (ticks != 0) ) [
    harvest
    if not running [stop] ;; if there isn't enough harvest, pass the 'stop' command back to 'go'
  ]
end


;; Function: grow
;; Update procedure: called during "Go" phase of simulation
;; Action: Increase available biomass by growth rate
;; Context: must be called in an ask crops or ask woodlands loop
to grow
  set standing-available-biomass standing-available-biomass + growth-rate
end

;; Function: update-crop-color-symbol
;; Update procedure: called during "Go" phase of simulation
;; Action: Update crops' color to reflect growth state, and the symbol displayed to make sure fences are shown
;;         and Muonde projects are shown.  Sorts out any conflicts between muonde project and fence being
;;         displayed; fence has priority over muonde project.
;; Context: Must be called in a crop (patch) context
to update-crop-color-symbol [maxcolor]
  ;; Update intensity of color to reflect growth state
  set pcolor 50 + 5 * (min (list standing-available-biomass maxcolor ) / ( maxcolor ) )
  ;; if we're past the max harvest, e.g. on a Muonde project, just keep it max green.

  ;; Set label to X if fenced, M if Muonde projects are present (and unfenced), blank otherwise
  set plabel-color yellow
  if not invincible-fences [  ;; note that with stone walls we don't update symbology at all
    ifelse fence > 0 [
      set plabel "X"
    ][
      set plabel ifelse-value (growth-multiplier = muonde-efficiency) [ "M" ] [ "" ]
    ]
  ]
end

;; Function: plough
;; Update procedure: called during "Go" phase of simulation
;; Action: Pick a cow that is available to work, reduce its energy pool, set the standing-available-biomass of the
;;         crop patch in the 'ask crops' loop to non-zero (it will keep growing on its own after that)
;; Context: Must be called in a crop (patch) context
to plough

  let worker-cow one-of cows-available-to-work
  if is-cow? worker-cow [
    ask worker-cow [
      adjust-energy (-1 * cow-working-energy-per-hour * hours-to-plough-ha * ha-per-patch )
      update-available-cows
    ]
    ;; Must set standing-available-biomass larger than 0 immediately:
    ;; If there was a worker-cow available, then this patch is ploughed and starts growing this tick, so that next tick
    ;; its biomass will be > 0 and it won't need to be ploughed again.
    ;;
    ;; scoping note, this refers to the biomass of the patch in the 'ask crops' loop which calls this function
    grow
  ]
end

;; Function: update-available-cows
;; Update procedure: called during "Go" phase of simulation
;; Action: update agentsets of cows available to work or reproduce
;; Context: must be called in a cow context
to update-available-cows
      ;; To produce another calf, body mass has to be enough to produce a calf-birth-mass,
      ;; plus the mother's own moving and metabolizing (maintenance) needs
      if (member? self cows-available-to-reproduce) and body-mass + cow-mass-change-from-current-energy <= (min-cow-mass
        + calf-birth-mass + cow-maintenance-energy-per-tick-per-kg * body-mass / kcal-per-kg-of-cow ) [
        set cows-available-to-reproduce other cows-available-to-reproduce
      ]
      ;; also if you don't have enough mass to work, make sure you don't get asked to work.
      if (member? self cows-available-to-work) and body-mass + cow-mass-change-from-current-energy <= ( min-cow-mass
          + (body-mass * cow-maintenance-energy-per-tick-per-kg + cow-working-energy-per-hour * hours-to-plough-ha * ha-per-patch) / kcal-per-kg-of-cow) [
        set cows-available-to-work other cows-available-to-work
      ]
end

;; Function: harvest
;; Update procedure: called during "Go" phase of simulation
;; Action: Harvest the crops and add to total-harvest, should be called at the end of the year
to harvest
  update-annual-crop-trackers

  ask crops [ set standing-available-biomass 0 ]

  ;; Start tracking only after the (burn-in-length)th year
  if burn-in = false [
    update-and-check-total-harvest
    if not running [stop] ;; if there isn't enough harvest, pass the 'stop' command back to 'go'
  ]
end

;; Function: update-annual-crop-trackers
;; Update procedure: called during "Go" phase of simulation
;; Action: Keep track of a running window how-long-to-store-grain long and update the percentage
;;          of crop eaten by the cows
to update-annual-crop-trackers
  ; keep track of previous harvests in order to do a running average
  ; how-long-to-store-grain is how many *previous* years -- we always have access to this year's harvest
  if length previous-harvests-list > how-long-to-store-grain [
    set previous-harvests-list but-last previous-harvests-list
  ]

  set current-harvest sum [ standing-available-biomass ] of crops  * 0.001  ;; in metric tons
  set previous-harvests-list fput current-harvest previous-harvests-list

  ;; to display the percentage harvest that the cows ate, use the current harvest and current crop eaten
  ;;    (which was accumulated during the individual 'consume' calls each cow * tick during the year
  ;;    Note that this is crop eaten / current harvest + crop eaten; so its maximum value is 100% (all the crop)
  if ( current-harvest + crop-eaten ) > 0 [
    set percentage-harvest-eaten 100 * crop-eaten / ( current-harvest + crop-eaten ) ]
  ;; track the maximum percent-harvest eaten for our reporters
  set max-percentage-harvest-eaten max ( list max-percentage-harvest-eaten percentage-harvest-eaten )
  ;; accumulate the total crop eaten by adding this year's crop-eaten
  set total-crop-eaten total-crop-eaten + crop-eaten
  ;; set this year's crop eaten back to zero
  set crop-eaten 0
end

;; Function: update-and-check-total-harvest
;; Update procedure: called during "Go" phase of simulation
;; Action: Updates accumulated harvest and checks that the mean harvest over the last several years is adequate
to update-and-check-total-harvest
  set total-harvest total-harvest + current-harvest
  ;; if our average over the last 'how-long-to-store-grain' years (plus the current year) isn't at
  ;;  least the minimum harvest required for sustainability (in metric tons), stop the simulation
  if mean previous-harvests-list < minimum-harvest-for-sustainability [
    finish-running-and-clean-up "too little average harvest"
    set model-run-time timer
    if(model-mode != "Experiment") [
      output-print (word "    *  model-run-time:                  " model-run-time)
    ]
    stop
  ]
end

;; Function: update-cows
;; Update procedure: called during "Go" phase of simulation
;; Action: Updates cows every tick, metabolize, move, consume, choose those that are available for work/reproduction.
to update-cows

  let hunger-threshold 0.5 ;; Used to turn the cow red; reflects greater probability of being able to break into the crops
                           ;;   due to desperation

  ;; calculate the time in ticks between farmers moving cows: set to 1 if it's less than 1 tick, and set to 1 if farmers aren't moving cows
  let ticks-between-farmers-moving-cows 1
  if (times-per-day-farmers-move-cows != 0) [
    set ticks-between-farmers-moving-cows (ticks-per-day / times-per-day-farmers-move-cows)
    if ticks-per-day < times-per-day-farmers-move-cows [
      ;; when the user asks to have farmers move cows more often than there are ticks, just have farmers move them every tick
      set ticks-between-farmers-moving-cows 1
    ]
  ]

  ;; check to see if it's time to move cows
  let time-to-move-cows? FALSE
  if (times-per-day-farmers-move-cows != 0) [
    set time-to-move-cows? ticks mod ticks-between-farmers-moving-cows < 1
  ]

  ;; set woodland-destinations-for-cows to no-patches to indicate it doesn't need to be updated in the ask-cows loop
  let woodland-destinations-for-cows no-patches
  ;; if farmers are moving cows, and it's time to move them, find the best patches to move them to.
  if ( time-to-move-cows? ) [
    set woodland-destinations-for-cows globally-find-suitable-patches
  ]

  ask cows [
    ifelse subsidy = "transport" and is-subsidized? [
      set color blue ;; and don't move, consume, subsidize with feed, or get hungrier.
    ][
      ;; Transported, subsidized cows don't get hungrier, or move/consume.  But they might reproduce, because right now
      ;; reproduction happens after cows are updated
      ;; let 'move' know if farmers should move cows and give it the current list of destinations
      move time-to-move-cows? woodland-destinations-for-cows
      consume
      get-hungrier  ;; incur maintenance energy cost, subsidize with feed, and finalize energy accounting for this tick

      if (time-to-move-cows?) [ ;; if farmers are moving cows, remove the lowest-biomass patch from the list of possible locations
        set woodland-destinations-for-cows update-globally-suitable-patches ( woodland-destinations-for-cows )
      ]

      ;; turn cow red if it goes below a certain satiety level
      let cow-color ifelse-value (satiety < hunger-threshold) [red] [white]
      set color cow-color
    ]
  ]
  find-available-cows  ;; who can be used for work or to reproduce
  reproduce-cows       ;; Make new cows!

end

;; Function: calculate-livestock-reproduction-rate-per-tick
;; Setup procedure: only called during initialization when user clicks setup button
;; Action: takes an annual probability of reproducing and calculates the per-tick probability
to-report calculate-livestock-reproduction-rate-per-tick
  report 1 - (livestock-not-reproduction-rate-per-year ^ (1.0 / ticks-per-year))
end

;; Function: reproduce-cows
;; Update procedure: called during "Go" phase of simulation
;; Action: Adds new cows organically
to reproduce-cows
  let livestock-reproduction-rate-per-tick calculate-livestock-reproduction-rate-per-tick
  set total-cows total-cows + count cows
  ;; cows with enough satiety, with a small chance, hatch a new cow
  if any? cows-available-to-reproduce [
    ask cows-available-to-reproduce [
      if ( random-float 1 < livestock-reproduction-rate-per-tick ) [
        give-birth
        update-available-cows
      ]
    ]
  ]
end

;; Function: give-birth
;; Update procedure: called during "Go" phase of simulation
;; Action: subtract calf mass from parent and create new cow
;; Context: must be called in a cow context
to give-birth
  ;; subtract the calf's birth mass from the parent
  set body-mass body-mass - calf-birth-mass ;; don't call adjust-energy because we keep it in terms of mass
  ;; birth a cow with is-calf?=TRUE: [setup-cow true] sets up a calf with body-mass = calf-birth-mass
  ;; Note that hatched cows inherit instance variables from their parent,
  ;; so a transport-subsidized parent cow will produce a transport-subsidized calf.
  hatch-cows 1 [ setup-cow true ]
  set total-number-of-births total-number-of-births + 1
end

;; Function: update-globally-suitable-patches
;; Update procedure: called during "Go" phase of simulation
;; Action: When farmers are moving cows, after we've moved a cow,
to-report update-globally-suitable-patches [woodland-destinations-for-cows]
      if any? woodland-destinations-for-cows [
        ;; If "any? woodland-destinations-for-cows" is true, then farmers are moving cows, and the "move" function above
        ;; just moved a cow to a destination patch.  In order to search fewer patches for the next cow, we'll remove the
        ;; patch with the least available biomass.  (This cow has already consumed, adjusting its patch's biomass.)
        let num-patches-needed max (list 1 (count woodland-destinations-for-cows - 1))
        set woodland-destinations-for-cows (
            max-n-of num-patches-needed woodland-destinations-for-cows [standing-available-biomass])
      ]
      report woodland-destinations-for-cows
end

;; Function: globally-find-suitable-patches
;; Update procedure: called during "Go" phase of simulation
;; Action: When farmers are moving cows, find the right number of possibly suitable patches
to-report globally-find-suitable-patches
  ;; discover a (set of) ideal new location(s) -- only to be used if farmers are moving cows
  ;; If farmers are moving cows, we'll want to know what the best locations are in the woodland in this tick
  ;; farmers are moving cows -- initialize woodland-destinations-for-cows
  let woodland-destinations-for-cows no-patches
  let woodland-dest-count min (list count cows count woodlands)
  set woodland-destinations-for-cows max-n-of woodland-dest-count woodlands [ standing-available-biomass ]
  report woodland-destinations-for-cows
end

;; Function: find-available-cows
;; Update procedure: called during "Go" phase of simulation
;; Action: Find cows that have enough energy to work or reproduce
to find-available-cows
  set cows-available-to-work cows with [ ;; first choose only cows with enough energy/mass to work
    is-calf? = false and (
        body-mass + cow-mass-change-from-current-energy > (  ;; enough to metabolize, move, plough, and still be alive afterward!
          min-cow-mass + ( body-mass * cow-maintenance-energy-per-tick-per-kg + cow-working-energy-per-hour * hours-to-plough-ha * ha-per-patch ) / kcal-per-kg-of-cow )
      ) and (
        ;; also, this cow must be excluded if it is living elsewhere for the season
        ;; (the cow is not living elsewhere if either we're using a different type of subsidy, or this is an unsubsidized cow)
        subsidy != "transport" or not is-subsidized?  ;; this means we don't allow 'feed' subsidized cows to work either
      )
  ]
  set cows-available-to-reproduce cows with [
    ;; the only limitations on cows to reproduce are (1) they can't be calves, and (2) they must have sufficient mass
    is-calf? = false and body-mass + cow-mass-change-from-current-energy >
     (min-cow-mass + calf-birth-mass + cow-maintenance-energy-per-tick-per-kg * body-mass / kcal-per-kg-of-cow)
  ]
end


;; Function: move
;; Update procedure: called during "Go" phase of simulation
;; Action: Moves a cow to a new patch
;; Context: Must be called in a cow context
to move [ do-farmers-move-cows? woodland-destinations-for-cows ]
  let new-location patch-here
  ;; If (1) farmers are moving cows at all, and (2) it's time to do so, and (3) there's a good place to move them to,
  ;;   then the cow gets moved there
  ifelse ( do-farmers-move-cows? ) [
    ;; Farmer identifies a new and better woodland location for this cow
    set new-location max-one-of woodland-destinations-for-cows [standing-available-biomass]
  ][
    ;; either cows are moving by themselves, or there's no great place for farmers to move them
    set new-location find-nearby-patch-to-eat
  ]
  if new-location != patch-here [
    ;; if there wasn't a good location, or you tried to get into a crop and couldn't, new-location could be your current location
    move-to new-location
  ]
end

;; Function: find-nearby-patch-to-eat
;; Update procedure: called during "Go" phase of simulation
;; Action: Cow tries to select a nearby patch to move to, and checks for fences
;; Context: Must be called in a cow context
to-report find-nearby-patch-to-eat
    ;; choose the patch with the most food, from the current patch and its four rook-neighborhood ("Manhattan") neighbors
    let new-location max-one-of (patch-set patch-here neighbors4) [standing-available-biomass]
    ; cows can share a patch
    if [fence] of new-location > 0 [
      ;; Cow wants to go here, but does a fence keep it out?
      ;; If the cow is more desperate, and if the fence is worse, the cow is more likely to be able to get in.
      ;; See the fence-effectiveness-per-tick function for the details of how a fence's effectiveness
      ;; depends on how much fence has been eaten by termites ([fence] of new-location) and how hungry the cow
      ;; has gotten (satiety).

      let fence-effectiveness fence-effectiveness-per-tick ([fence] of new-location) satiety
      if invincible-fences or fence-effectiveness > random-float 1 [
        ;; If you can't break through the fence because it's invincible (stone) or because the fence is too effective,
        ;; then stay where you are.
        ;; Note that there may be a patch next to you that's somewhat better than patch-here, but you're stuck here
        ;; dreaming about that wonderful unreachable patch.  Collaborators confirm that cows often behave this way.
        set new-location patch-here
      ]
    ]
    report new-location
end

;; Function (reporter): fence-effectiveness-per-tick
;; Input parameters:
;;   fence-quality (float between 0 and 1): a number indicating to what degree the fence has decayed (0 = decayed)
;;   cow-satiety (float between 0 and 1): a number indicating the cow's satiety level (0 = starving)
;; Return value: A number between 0 and 1 indicating the effectiveness of this fence.
;; Return value depends on fence quality (1 = new, 0 = completely degraded)
;;   and also on cow satiety (hungrier (lower-satiety) cows are more motivated to get through fences).
;;
;; Since this research does not include an investigation of fence effectiveness, we extract those considerations to this
;; function, which currently uses a relatively simple model:
;;   probability of cow getting through fence (per day) is linear in satiety and linear in fence quality
;;     (healthy, high-satiety cows aren't hungry enough to fight fences; strong fences discourage all cows)
;;   P_cow_gets_through (per day) =  (1 - satiety) * (1 - fence-quality)
;;   P_fence_holds (per day)      = 1 - (1 - satiety) * (1 - fence-quality)
;;   P_fence_holds (per tick)     = (1 - (1 - satiety) * (1 - fence-quality)) ^ (1 / ticks-per-day)
to-report fence-effectiveness-per-tick [fence-quality cow-satiety]
  report (1 - (1 - cow-satiety) * (1 - fence-quality)) ^ (1 / ticks-per-day)
end

;; Function: consume
;; Update procedure: called during "Go" phase of simulation
;; Action: Simulate the cow eating the patch
;; Context: Must be called in a cow context
to consume
  ;; Note that cows eating subsidized feed happens in get-hungrier (so subsidized cows don't die before they're subsidized).

  ;; Increase energy pool and decrease standing biomass of patch-here
  ;; a hungry cow will always eat as if it were maximum weight; so if it's under-weight it has a chance of gaining it back
  let cow-energy-change ( cow-maintenance-energy-per-tick-per-kg ) * max-cow-mass
  ifelse standing-available-biomass > 0 [
    ;; there's *something* to eat here -- is it enough to fully satisfy the cow?  if not, the cow can only eat what's there.
    let plant-energy-density ifelse-value is-crop? [kcal-per-kg-of-crop][kcal-per-kg-of-browse]
    let plant-biomass-eaten min(list standing-available-biomass (cow-energy-change / plant-energy-density))
    set cow-energy-change plant-biomass-eaten * plant-energy-density
    adjust-energy cow-energy-change

    ;; In NetLogo, turtles (cows) can read and set patch variables of the patch they're on.  We make use of
    ;; standing-available-biomass here, but no patch context is required.
    set standing-available-biomass standing-available-biomass - plant-biomass-eaten

    ;; Only adjust crop-eaten and woodland-eaten totals after burn-in
    if (burn-in = false) [
      update-crop-woodland-eaten plant-biomass-eaten
    ]
  ][
    ;; nothing to eat on this patch!  can't increase this cow's energy and don't need to decrease woodland/crop biomass
  ]
end

;; Function: update-crop-woodland-eaten
;; Update procedure: called during "Go" phase of simulation
;; Action: update variables monitoring the amount of crop or woodland eaten and the amount of time cows are in crops
;; Context: Must be called in a cow context
to update-crop-woodland-eaten [plant-biomass-eaten]
  ifelse is-crop? = true [
    set crop-eaten crop-eaten + plant-biomass-eaten * 0.001  ;; crop-eaten is in metric tons, not kg
    set count-cows-in-crops count-cows-in-crops + 1
  ][
    set woodland-eaten woodland-eaten + plant-biomass-eaten * 0.001  ;; woodland-eaten is in metric tons, not kg
  ]
end

;; Function: get-hungrier
;; Update procedure: called during "Go" phase of simulation
;; Action: Decreases the satiety of the cows, adjusts body mass, subsidizes 'feed' animals
;;         updates cow/calf status, and then asks the cow to die if its mass is too low
;; Context: Must be called in a cow context
to get-hungrier
  ;; Cows use energy due to metabolism, moving, etc.  (proportional to body mass)
  let cow-energy-change cow-maintenance-energy-per-tick-per-kg * body-mass
  adjust-energy (-1 * cow-energy-change)

  ;; do the accounting based on the cow's current energy pool (includes ploughing from last tick
  ;;   and moving/consuming from this tick)
  convert-cow-energy-pool-to-body-mass-change

  ;; subsidized cows get saved at this point, after mass has been updated
  if is-subsidized? = true and subsidy = "feed" [
    subsidize-with-supplemental-feed
    ;; and update the body mass one more time with this additional feed - it's always a mass gain.
    convert-cow-energy-pool-to-body-mass-change
  ]
  if is-calf? = TRUE [
    check-calf-status
  ]
  check-for-starvation
end

;; Function: check-for-starvation
;; Update procedure: called during "Go" phase of simulation
;; Action: change cow's status from alive to dead, if its mass is too low
;; Context: Must be called in a cow context
to check-for-starvation
  let min-mass ifelse-value is-calf? [min-calf-mass ] [min-cow-mass] ;; if you're a calf, use the calf min-mass
  if body-mass < min-mass [
    ;; This cow has starved to death.
    if (burn-in = false) [ set dead-cows dead-cows + 1 ]
    die
  ]
end

;; Function: check-calf-status
;; Update procedure: called during "Go" phase of simulation
;; Action: check to see if a calf graduates to being a cow.
;;         This means you have the min mass of a cow, plus a day of reserves.
;; Context: Must be called in a cow context
to check-calf-status
  if (body-mass > min-cow-mass + (1 * ticks-per-day * cow-maintenance-energy-per-tick-per-kg * min-cow-mass / kcal-per-kg-of-cow) ) [
    set is-calf? false
    set size 4
  ]
end

;; Function: subsidize-with-supplemental-feed
;; Update procedure: called during "Go" phase of simulation
;; Action: 'feed' subsidy is applied to cows that couldn't eat (after move and consume), must happen before get-hungrier
;; Context: Must be called in a cow context
to subsidize-with-supplemental-feed
  let min-mass ifelse-value is-calf? [min-calf-mass][min-cow-mass] ;; if you're a calf, use the calf minimum weight
  ;; a hungry cow will always eat as if it were maximum weight; so if it's under-weight it has a chance of gaining it back
  ;;   (this is how they consume when they graze normally)
  let how-much-energy-cow-wants cow-maintenance-energy-per-tick-per-kg  *  max-cow-mass
  if body-mass <= min-mass [ ;; we only subsidize when they're about to die
    ;; We're using subsidy feed to feed this cow, and it needs feeding.  Increment the total subsidy used.
    set subsidy-used ( subsidy-used + cost-of-supplemental-feed * (
      how-much-energy-cow-wants / kcal-per-kg-of-crop) )  ;; tracking this in dollars
    ;; feed that hungry cow!
    adjust-energy ( how-much-energy-cow-wants )
  ]
end

;; Function: update-fences
;; Update procedure: called during "Go" phase of simulation
;; Action: Termites eat fences, some fences are rebuilt
to update-fences

  ;; Collect the list of all woodland squares from which we could harvest
  ;;   material for at least one fence.  For efficiency, we'll only look at
  ;;   this subset when trying to rebuild fences.  (Profiling has shown that
  ;;   this performance improvement is a big win.)
  let woodlands-with-fence-material woodlands with [standing-available-biomass > (wood-to-build-fence-per-meter *  100 * sqrt ( ha-per-patch ))]

  ask crops-with-fences [
    get-eaten-by-termites
    if fence = 0 [ ;; Rebuild fences!
      ;; we skip this entirely if woodlands-with-fence-material is empty in this tick or at this point in ask crops-with-fences.
      ;; then we don't do something futile up to four times.
      if any? woodlands-with-fence-material [
        set woodlands-with-fence-material repair-fences (woodlands-with-fence-material)
      ] ;; end 'if any? woodlands-with-fence-material'
    ] ;; end 'if fence <= 0'
  ] ;; end ask crops-with-fences loop
end

;; Function: get-eaten-by-termites
;; Update procedure: called during "Go" phase of simulation
;; Action: Termites eat the fences regardless of all else. Fences
;;          all decay at the same rate, regardless of how-many-fences
to get-eaten-by-termites
  set fence fence - termite-activity / ticks-per-year
  if fence < 0 [ set fence 0 ]   ;; Note that negative values for fence act the same as zero.
end

;; Function: repair-fences
;; Update procedure: called during "Go" phase of simulation
;; Action: rebuild fences, one side of a crop at a time
to-report repair-fences [woodlands-with-fence-material]
  repeat how-many-fences [
    ;; repair one fence-side (repeating as necessary, as many times as how-many-fences for that crop patch)
    let wood-source one-of woodlands-with-fence-material
    if is-patch? wood-source [
      ;; if at least one patch remains in the patchset, it must have enough to build this fence, and if no patches are
      ;; left, then quit the loop.
      ask wood-source [
        set standing-available-biomass standing-available-biomass - (wood-to-build-fence-per-meter *  100 * sqrt ( ha-per-patch ))
        if standing-available-biomass < (wood-to-build-fence-per-meter *  100 * sqrt ( ha-per-patch )) [
          ;; We've harvested the last fence from this patch (at least for
          ;;   this tick).  Remove this patch from our list.
          set woodlands-with-fence-material other woodlands-with-fence-material
        ]
      ]
      ;; fence = *average* value (over how-many-fences for this patch)
      ;; This correctly handles harvesting the last woodland squares and
      ;;   only being able to repair some of this patch's fences.
      set fence fence + 1.0 / how-many-fences
    ] ;; end 'if is-patch? wood-source'
  ] ;; end 'repeat how-many-fences'
  report woodlands-with-fence-material
end



;;------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------
; ------------------------------ SOFTWARE TESTS --------------------------------------------
;;------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------

;; Function: clear-everything-but-output
;; Software test procedure: only called during sofware tests
;; Action: clears everything but the output window so the results of all the tests remain in
;;   the output area of the interface
to clear-everything-but-output
  clear-globals
  clear-patches
  clear-turtles
  clear-all-plots
  clear-ticks
end

;; Function: run-software-tests
;; Software test procedure: only called during software tests
;; Action: execute all software test functions, report results
to run-software-tests
  ;; keep track of the number of tests (calls to 'assert' functions) run, and the number of errors
  set number-of-tests-run 0
  set error-count 0

  ;; run each testing function (which has multiple 'assert' tests in each function)
  test-set-global-variables
  test-setup
  test-set-patch-variables
  test-configure-fences
  test-calculate-landscape-metrics
  test-set-faster-growing-patches
  test-make-crops
  test-make-crop-clumps
  test-setup-cow
  test-finish-running-and-clean-up
  test-go
  test-update-yearly-time-events
  test-set-subsidized-cows
  test-get-new-rainfall
  test-set-new-growth-rates
  test-update-max-min
  test-update-woodland-available-biomass
  test-convert-cow-energy-pool-to-body-mass-change
  test-update-crops
  test-update-crop-color-symbol
  test-plough
  test-harvest
  test-update-cows
  test-calculate-livestock-reproduction-rate-per-tick
  test-find-available-cows
  test-move
  test-find-nearby-patch-to-eat
  test-fence-effectiveness-per-tick
  test-consume
  test-get-hungrier
  test-update-fences
  test-reproduce-cows
  test-calculate-average-contiguous-crop-cluster-size

  ;; at the end, display in command center the number of tests run and passed.
  output-print (word number-of-tests-run " tests executed")
  ifelse error-count = 0 [
    output-print "No errors."
    output-print "**All tests passed**"
  ][
    output-print (word error-count " errors.")
    output-print "**Some tests failed**"
  ]
end


;; Function: assert
;; Software test procedure: only called during software tests
;; Action: Run a task, show an error if the return value of the task is not true
;; Input parameters:
;;   testval (task returning a boolean): the assertion, a task which should return true
;;   errmsg (string): the error to show if the assertion does not return true
;; This function typically takes a logical statement like "proportion crops = 80" and checks
;;  to see if it's true, and if not, returns the error message specified.  It also updates the
;;  number of tests run and errors returned.
to assert [testval errmsg]
  set number-of-tests-run number-of-tests-run + 1
  if not runresult testval [
    output-print (word "At test " number-of-tests-run ":")
    output-print errmsg
    set error-count error-count + 1
  ]
end


;; Function: assert-equal
;; Software test procedure: only called during software tests
;; Action: Run a task, show an error if the return value of the task is not equal to the expected value
;; Input parameters:
;;   testval (task returning a value): the assertion, a task which should return a known value
;;   expectedval (type depends on the task): the value that the task is expected to return
;;   testvalname (string): a meaningful description of what this value represents, e.g. the name of a variable
;; A meaningful error message is constructed from the three inputs for the user running the test.
;; This function works like 'assert' but also gives more detail, specifying the expected value (specified in the
;; assert-equal function call) and the actual value.
to assert-equal [testval expectedval testvalname]
  set number-of-tests-run number-of-tests-run + 1
  let actualval runresult testval
  if actualval != expectedval [
    output-print (word "At test " number-of-tests-run ":")
    output-print (word "Bad value for " testvalname ":")
    output-print (word "   Expected: " expectedval)
    output-print (word "   Actual: " actualval)
    set error-count error-count + 1
  ]
end


;; Function: assert-float-equal
;; Software test procedure: only called during software tests
;; Action: Run a task, show an error if the return value of the task is not within epsilon of the expected float value
;; Input parameters:
;;   testval (task returning a float): the assertion, a task which should return a known floating-point value
;;   expectedval (float): the floating-point value that the task is expected to return
;;   testvalname (string): a meaningful description of what this value represents, e.g. the name of a variable
;; A meaningful error message is constructed from the three inputs for the user running the test.
;; This function works like 'assert-equal' but accounts for rounding errors.
to assert-float-equal [testval expectedval testvalname]
  set number-of-tests-run number-of-tests-run + 1
  let actualval runresult testval
  let epsilon 0.000001
  if abs(actualval - expectedval) > epsilon [
    output-print (word "At test " number-of-tests-run ":")
    output-print (word "Bad value for " testvalname ":")
    output-print (word "   Expected: " expectedval)
    output-print (word "   Actual: " actualval)
    set error-count error-count + 1
  ]
end

to test-set-global-variables
;; this set of tests mainly just checks that the variables have the values specified in the function.
;; it is most useful to remind you that you've changed a variable and means you have a second chance to
;; confirm that the new value was what you intended to change it to.

;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-set-global-variables"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  set rain-site "Average"
  set world-size 50
  set ticks-per-day 5
  set times-per-day-farmers-move-cows 0

;; call the function we're testing
  set-global-variables

;; test some specific aspects of the results of running that function
  assert task [ length (rainfall-data) = 60 ] (word "Unexpected amount of rainfall data: " (length (rainfall-data)))
  assert task [ length (extreme-rainfall-data) = 29 ] "Unexpected amount of extreme rainfall data"
  assert task [ ha-per-patch = 600 / 2500 ] "ha-per-patch was not 600 / (world-size * world-size)"
  assert task [ ticks-per-year = 365 * 5 ] "ticks-per-year wasn't set"

  ;; this tests whether farmers will move cows once every other tick when ticks-per-day is greater than 1
  set ticks-per-day 2
  set times-per-day-farmers-move-cows 1
  set-global-variables

  ;; this tests whether the code properly realizes that farmers can't move cows more often than there are ticks
  set times-per-day-farmers-move-cows 3
  set-global-variables

  assert task [ wood-to-build-fence-per-meter = 5.77 ] "bad value for wood-to-build-fence-per-meter"
  assert-equal task [ woodland-growth-slope ] (3320 / mean(rainfall-data)) "woodland-growth-slope"
  assert task [ cow-maintenance-energy-per-tick-per-kg = cow-maintenance-energy-rate / ( ticks-per-day ) ]
      "cow-maintenance-energy-per-tick-per-kg not set correctly"

  ; Verify that all variables that should be explicitly set to constant values *are* set
  assert task [ termite-activity = 0.67 ] "termite-activity not set correctly"
  assert task [ total-mud-crop-perimeter = 50000 ] "total-mud-crop-perimeter not set correctly"
  assert task [ hours-to-plough-ha = 8 ] "hours-to-plough-ha not set correctly"
  assert task [ zero-crop-growth-intercept = 232 ] "zero-crop-growth-intercept not set correctly"
  assert task [ crop-growth-slope = 0.8731 ] "crop-growth-slope not set correctly"
  assert task [ muonde-efficiency = 3.0 ] "muonde-efficiency not set correctly"
  assert task [ cow-maintenance-energy-rate = 980 ] "cow-maintenance-energy-rate not set correctly"
  assert-equal task [ kcal-per-kg-of-browse ] 1931 "kcal-per-kg-of-browse"
  assert task [ kcal-per-kg-of-cow = 1360 ] "kcal-per-kg-of-cow not set correctly"
  assert task [ kcal-per-kg-of-crop = 2325 ] "kcal-per-kg-of-crop not set correctly"
  assert task [ max-cow-mass = 296 ] "max-cow-mass not set correctly"
  assert task [ min-cow-mass = 63 ] "min-cow-mass not set correctly"
  assert task [ calf-birth-mass = 18 ] "calf-birth-mass not set correctly"
  assert task [ cost-of-supplemental-feed = 14.50 ] "cost-of-supplemental-feed not set correctly"
  ifelse use-muonde-thresholds [
    assert task [ minimum-cows-for-sustainability = 50 ] "minimum-cows-for-sustainability not set correctly"
    assert task [ minimum-harvest-for-sustainability = 48 ] "minimum-harvest-for-sustainability not set correctly"
    assert task [ woodland-sustain-factor = 1.0 ] "woodland-sustain-factor not set correctly"
  ][
    assert task [ minimum-cows-for-sustainability = 1 ] "minimum-cows-for-sustainability not set correctly"
    assert task [ minimum-harvest-for-sustainability = 0.96 ] "minimum-harvest-for-sustainability not set correctly"
    assert task [ woodland-sustain-factor = 0.02 ] "woodland-sustain-factor not set correctly"
  ]

  assert task [ calculate-initial-number-of-livestock > 0 and calculate-initial-number-of-livestock < 300 ] (word
   "Unlikely value for initial-livestock-number: " calculate-initial-number-of-livestock)
  assert task [ minimum-woodland-for-sustainability > 50 and minimum-woodland-for-sustainability < 500000 ] (word
      "Unlikely value for minimum-woodland-for-sustainability: " minimum-woodland-for-sustainability)

  ; Verify some basic sanity checks about the values of these parameters
  assert task [ min-cow-mass > 0 ] "min-cow-mass <= 0"
  assert-equal task [min-calf-mass] ( calf-birth-mass
    - 7  * ticks-per-day * cow-maintenance-energy-per-tick-per-kg * calf-birth-mass / kcal-per-kg-of-cow )  "min-calf-mass"
  assert task [ woodland-growth-slope > 0 ] "woodland-growth-slope <= 0"
  assert task [ cow-maintenance-energy-per-tick-per-kg > 0 ] "cow-maintenance-energy-per-tick-per-kg <= 0"
  assert task [ cow-working-energy-per-hour * hours-to-plough-ha * ha-per-patch > 0 ] "cow-working-energy-per-hour * hours-to-plough-ha * ha-per-patch <= 0"

;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end


to test-calculate-livestock-reproduction-rate-per-tick
;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-calculate-livestock-reproduction-rate-per-tick"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  set ticks-per-day 3
  set ticks-per-year 365 * ticks-per-day
  set livestock-not-reproduction-rate-per-year 0.75

  ;; call the function we're testing
  let livestock-reproduction-rate-per-tick calculate-livestock-reproduction-rate-per-tick

;; test some specific aspects of the results of running that function
  assert task [ livestock-reproduction-rate-per-tick = 1 - 0.75 ^ (1 / ticks-per-year) ] "livestock-reproduction-rate-per-tick not set correctly"
  assert task [ livestock-reproduction-rate-per-tick > 0 ] "livestock-reproduction-rate-per-tick <= 0"
  assert task [ livestock-reproduction-rate-per-tick <= 1 ] "livestock-reproduction-rate-per-tick > 1"

  ;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-setup
;; setup doesn't do anything other than calling other functions, so this function is here for completeness
;; to indicate that we didn't forget to test setup.

;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-setup"
  let start-test-number number-of-tests-run
  ; output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

  ; No testing for setup: it just calls other functions, and handles SoftwareTests mode specially.

;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  ; output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-set-patch-variables
;; this test is to verify:
;;  pcolor is green (55) for crops, brown (35) for woodlands
;;  is-crop? is true for crops, false for woodlands; is-woodland? is the reverse
;;  fence is 0 (crops and woodlands)
;;  growth-multiplier is 1
;;  moran-numerator, geary-numerator, and moran-geary-denominator are both 0
;;  how-many-fences is 0 (crops)

;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-set-patch-variables"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  resize-world 0 9 0 9  ;; 10-by-10, not 9-by-9
  set crops n-of 40 patches
  set woodlands patches with [not member? self crops]
  let max-crop-biomass 100
  set rainfall-data (list 1)

;; call the function we're testing
  set-patch-variables

;; test some specific aspects of the results of running that function
  assert task [ all? crops [ pcolor = 55 ] ]  "Failed to set pcolor for crops"
  assert task [ all? woodlands [ pcolor = 35 ] ]  "Failed to set pcolor for woodlands"
  assert task [ all? crops [ standing-available-biomass = 0 ] ]  "Failed to set standing-available-biomass for crops"
  assert task [ all? woodlands [ standing-available-biomass >= 0 ] ]  "Failed to set biomass >= 0% max for woodlands"
  assert task [ all? woodlands [ standing-available-biomass <= 200 ] ]  "Failed to set biomass <= 5% max for woodlands"
  assert task [ all? patches [ growth-multiplier = 1 ] ]  "Failed to set growth-multiplier for patches"
  assert task [ all? patches [ moran-numerator = 0 ] ]  "Failed to set moran-numerator for patches"
  assert task [ all? patches [ geary-numerator = 0 ] ]  "Failed to set geary-numerator for patches"
  assert task [ all? patches [ moran-geary-denominator = 0 ] ]  "Failed to set moran-geary-denominator for patches"
  assert task [ all? patches [ fence = 0 ] ]  "Failed to set fence for patches"
  assert task [ all? crops [ how-many-fences = 0 ] ]  "Failed to set how-many-fences for crops"

;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-configure-fences
;; This test is to verify:
;;  * how-many-fences set correctly
;;  * if invincible-fences true, fence set to 1, plabel set to "s"
;;  * if invincible-fences false, fence set to random value 0.0-1.0, plabel set to "X"  (we won't test the color)

;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-configure-fences"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  resize-world 0 4 0 4  ;; 5-by-5, not 4-by-4
  set crops (
      patch-set
          patch 0 0    ; CC...  Note that crop (3,3) must have 4 fences, (1,2) must have 3, (2,1) must have 2,
          patch 0 1    ; CCC..  (0,1) and (2,0) must have 1, and (0,0), (1,0), and (1,1) must have 0 fences.
          patch 1 0    ; CC...
          patch 1 1    ; ...C.
          patch 1 2    ; .....
          patch 2 0
          patch 2 1
          patch 3 3
      )
  set woodlands patches with [not member? self crops]
  set rainfall-data (list 1)
  set-patch-variables
  set crops-with-fences crops with [any? neighbors4 with [is-woodland? = true]]
  set invincible-fences false


;; call the function we're testing
  configure-fences

;; test some specific aspects of the results of running that function
  ;making sure we get the right count of fences for patches on corners, interiors, and edges
  assert-equal task [[how-many-fences] of patch 0 0] 0 "how-many-fences for patch 0 0"
  assert-equal task [[how-many-fences] of patch 0 1] 1 "how-many-fences for patch 0 1"
  assert-equal task [[how-many-fences] of patch 1 0] 0 "how-many-fences for patch 1 0"
  assert-equal task [[how-many-fences] of patch 1 1] 0 "how-many-fences for patch 1 1"
  assert-equal task [[how-many-fences] of patch 1 2] 3 "how-many-fences for patch 1 2"
  assert-equal task [[how-many-fences] of patch 2 0] 1 "how-many-fences for patch 2 0"
  assert-equal task [[how-many-fences] of patch 2 1] 2 "how-many-fences for patch 2 1"
  assert-equal task [[how-many-fences] of patch 3 3] 4 "how-many-fences for patch 3 3"

  ;checking to see if the value of fence quality/effectiveness is in the right range
  let some-fence-status [fence] of patch 3 3
  assert task [ some-fence-status > 0.0 and some-fence-status < 1.0 ] "Expected fence status between 0.0 and 1.0"
  ;making sure the labels are correct
  assert-equal task [[plabel] of patch 3 3] "X" "plabel of fenced crop"
  assert-equal task [[plabel] of patch 0 0] ""  "plabel of interior crop"


;; some variables are required to be set in order to run the function
  ; checking all the above, but for invincible fences ("s"tone walls)
  set invincible-fences true

;; call the function we're testing
  configure-fences

;; test some specific aspects of the results of running that function
  ;making sure we get the right count of fences for patches on corners, interiors, and edges
  assert-equal task [[how-many-fences] of patch 1 1] 0 "how-many-fences for patch 1 1"
  assert-equal task [[how-many-fences] of patch 1 2] 3 "how-many-fences for patch 1 2"
  assert-equal task [[how-many-fences] of patch 2 0] 1 "how-many-fences for patch 2 0"
  assert-equal task [[how-many-fences] of patch 2 1] 2 "how-many-fences for patch 2 1"
  assert-equal task [[how-many-fences] of patch 3 3] 4 "how-many-fences for patch 3 3"

  ;checking to see if the value of fence quality/effectiveness is 1.0 (invincible)
  set some-fence-status [fence] of patch 3 3
  assert-equal task [some-fence-status] 1.0 "invincible fence status"
  ;making sure the labels are correct
  assert-equal task [[plabel] of patch 3 3] "s" "plabel of fenced crop patch"
  assert-equal task [[plabel] of patch 0 0] ""  "plabel of interior crop patch"

;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-calculate-landscape-metrics
;; this test compares the manually calculated landscape metrics for known configurations of crops to those
;; calculated by our functions, including checking for errors due to selecting all crop or all woodland

;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-calculate-landscape-metrics"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  resize-world 0 3 0 3  ;; 4-by-4, not 3-by-3
  set proportion-crops 50
  set ha-per-patch 48.9
  set rainfall-data (list 1)

  set average-contiguous-crop-cluster-size 99999  ;; dummy value -- we'll just verify that it gets overwritten
  set crops (
    patch-set
    patch 0 0
    patch 0 2
    patch 1 1
    patch 1 3
    patch 2 0
    patch 2 2
    patch 3 1
    patch 3 3
    )
  set woodlands patches with [not member? self crops]
  set-patch-variables
  ask crops [set is-crop? TRUE]
  ask woodlands [set is-crop? FALSE]
  set crops-with-fences crops with [any? neighbors4 with [is-woodland? = true]]
  configure-fences

;; call the function we're testing
  calculate-landscape-metrics

;; test some specific aspects of the results of running that function

  ;; test total-crop-perimeter
  ;; N^2 patches, checkerboard pattern -> 4 with 2 neighbors, (4N-8) with 3 neighbors, (N^2-4N+4) with 4 neighbors
  ;;   -> but only half of these are croplands: N is even, so half of (corners, edges, middle) are crops
  ;; total # fences: 0.5 * { 4(N^2-4N+4) + 3(4N-8) + 2(4) = 4N^2-16N+16 + 12N-24 + 8 = 4N^2-4N }
  ;; = 0.5 * 4 * (count patches - sqrt(count patches))
  let expected-perimeter 2 * ((count patches) - sqrt (count patches)) * 100 * sqrt ( ha-per-patch )
  assert-equal task [total-crop-perimeter] expected-perimeter "total-crop-perimeter"

  ;; test that average-contiguous-crop-cluster-size is set in calculate-landscape-metrics
  ;; (we'll test the calculate-average-contiguous-crop-cluster-size function later)
  assert task [average-contiguous-crop-cluster-size != 99999] "average-contiguous-crop-cluster-size was not set"

  ;; now we test the Moran's-I calculations
  ;; these are much more complicated and take up the bulk of this test function...

  assert task [not any? patches with [moran-geary-denominator != 0.25]] "A patch had an unexpected moran-geary-denominator"
  assert-equal task [[moran-numerator] of patch 0 0] -0.5 "moran-numerator of (0,0)"
  assert-equal task [[moran-numerator] of patch 0 1] -0.75 "moran-numerator of (0,1)"
  assert-equal task [[moran-numerator] of patch 0 2] -0.75 "moran-numerator of (0,2)"
  assert-equal task [[moran-numerator] of patch 0 3] -0.5 "moran-numerator of (0,3)"
  assert-equal task [[moran-numerator] of patch 1 0] -0.75 "moran-numerator of (1,0)"
  assert-equal task [[moran-numerator] of patch 1 1] -1 "moran-numerator of (1,1)"
  assert-equal task [[moran-numerator] of patch 1 2] -1 "moran-numerator of (1,2)"
  assert-equal task [[moran-numerator] of patch 1 3] -0.75 "moran-numerator of (1,3)"
  assert-equal task [sum [moran-numerator] of patches] -12 "sum(moran-numerator)"
  assert-equal task [sum [moran-geary-denominator] of patches] 4 "sum(moran-geary-denominator)"
  assert-equal task [morans-i] -1 "Moran's I for checkerboard"

;; some variables are required to be set in order to run the function
  set woodlands patch-set patch 1 1
  set crops patches with [not member? self woodlands]
  set proportion-crops 93.75
  set crops-with-fences crops with [any? neighbors4 with [is-woodland? = true]]
  set-patch-variables
  assert-equal task [count crops] 15 "count crops"
  assert-equal task [count woodlands] 1 "count woodlands"
  ask crops [set is-crop? TRUE]
  ask woodlands [set is-crop? FALSE]

;; call the function we're testing
  calculate-landscape-metrics

;; test some specific aspects of the results of running that function

  ; Moran's-I numerator sum:
  ;   16 patches: 4 with 4 neighbors, 8 with 3 neighbors, 4 with 2 neighbors.
  ;   = 16+24+8 = 48 neighbor-pairs
  ;   8 of these (4 inbound + 4 outbound) are between a common (crop) patch and a rare (woodland) patch
  ;   40 are between two common (crop) patches
  ;   40 * (1.0 - 0.9375)^2 + 8 * (1.0 - 0.9375)(0.0 - 0.9375)
  ;    = 40 * 1/256 + 8 * (1/16)(-15/16)
  ;    = 40/256 + -120/256 = -80/256 = -5/16
  assert-equal task [sum [moran-numerator] of patches] -5 / 16 "sum(moran-numerator)"
  assert-equal task [sum [moran-geary-denominator] of patches] 0.9375 "sum(moran-geary-denominator)"

  ; You might think that having 15 crop patches and only 1 woodland patch would give a Moran's-I near +1.0, maybe around +0.9...
  ; You would be wrong.
  ; Turns out that for a single non-edge patch that is different, Moran's I evaluates to -1/(N-1)^2.
  ; For this 4x4 example, that's -1/9.
  assert-equal task [morans-i] -1 / 9 "Moran's I for only-1-woodland"

;; some variables are required to be set in order to run the function

  ; Try a nearly-optimal landscape for high Moran's I / low Geary's C
  let this-size 50
  resize-world 0 (this-size - 1) 0 (this-size - 1)
  set crops patches with [pxcor < (this-size / 2)]
  set woodlands patches with [not member? self crops]
  set proportion-crops 50
  set crops-with-fences crops with [any? neighbors4 with [is-woodland? = true]]
  set-patch-variables
  assert-equal task [count crops] (this-size * this-size / 2) "count crops"
  assert-equal task [count woodlands] (this-size * this-size / 2) "count woodlands"
  ask crops [set is-crop? TRUE]
  ask woodlands [set is-crop? FALSE]

;; call the function we're testing
  calculate-landscape-metrics

;; test some specific aspects of the results of running that function
  assert-float-equal task [morans-i] ((this-size - 2) / (this-size - 1)) "Moran's I for half woodland / half crops"
  let expected-gearys-c ((this-size + 1) / (this-size * this-size))
  assert-float-equal task [gearys-c] expected-gearys-c "Geary's C for half woodland / half crops"

;; some variables are required to be set in order to run the function

  ; Verify that calculate-landscape-metrics raises errors if tried with all crops or all woodlands
  resize-world 0 3 0 3
  set crops patches
  set proportion-crops 100
  set crops-with-fences crops with [any? neighbors4 with [is-woodland? = true]]
  set woodlands patches with [not member? self crops]
  set-patch-variables
  assert-equal task [count crops] 16 "count crops"
  assert-equal task [count woodlands] 0 "count woodlands"
  ask crops [set is-crop? TRUE]
  ask woodlands [set is-crop? FALSE]
  carefully [
;; call the function we're testing
    calculate-landscape-metrics
;; test some specific aspects of the results of running that function
    assert task [false] "Moran's I calculation failed to raise an error for all-crops"
  ][
    assert task [true] "Moran's I calculation raised the correct error for all-crops"
  ]

;; some variables are required to be set in order to run the function
  set woodlands patches
  set crops patches with [not member? self woodlands]
  set proportion-crops 0
  set crops-with-fences crops with [any? neighbors4 with [is-woodland? = true]]
  set-patch-variables
  carefully [
;; call the function we're testing
    calculate-landscape-metrics
;; test some specific aspects of the results of running that function
    assert task [false] "Moran's I calculation failed to raise an error for all-woodlands"
  ][
    assert task [true] "Moran's I calculation raised the correct error for all-woodlands"
  ]

;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-set-faster-growing-patches
;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-set-faster-growing-patches"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  ; Setup for testing
  resize-world 0 4 0 4  ;; 5-by-5, not 4-by-4
  set crops n-of 10 patches
  set woodlands patches with [not member? self crops]
  set muonde-projects 20   ; 20% of 10 patches = 2 patches
  set muonde-efficiency 3.0
  set key-resources 25     ; 25% of 15 patches = 3.75 = 4 patches

;; call the function we're testing
  set-faster-growing-patches

;; test some specific aspects of the results of running that function
  assert-equal task [ count crops with [ plabel = "M" ] ] 2  "faster-growing crops (with label 'M')"
  assert-equal task [ count crops with [ growth-multiplier = muonde-efficiency ] ] 2  "faster-growing crops (with high growth)"
  assert-equal task [ count woodlands with [ plabel = "*" ] ] 4  "faster-growing woodlands (with label '*')"
  assert-equal task [ count woodlands with [ growth-multiplier = 4 ] ] 4  "faster-growing woodlands (with high growth)"

;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-make-crops

;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-make-crops"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  resize-world 0 9 0 9  ;; 10-by-10
  set proportion-crops 40
  set clumpiness 1.0
  ; We should have a single clump of 40 crops (0 crops with no crop neighbors)
  set rainfall-data (list 1)

;; call the function we're testing
  make-crops

;; test some specific aspects of the results of running that function
  set-patch-variables
  assert-equal task [ count crops ] 40  "count crops"
  assert-equal task [ count woodlands ] 60  "count woodlands"
  assert-equal task [ count crops with [ not any? neighbors4 with [is-crop? = TRUE] ] ] 0 "Clumped crops with no crop neighbor"
  assert task [ all? crops [ is-crop? ] ]  "Failed to set is-crop? for crops"
  assert task [ all? crops [ not is-woodland? ] ]  "Failed to set is-woodland? for crops"
  assert task [ all? woodlands [ not is-crop? ] ]  "Failed to set is-crop? for woodlands"
  assert task [ all? woodlands [ is-woodland? ] ]  "Failed to set is-woodland? for woodlands"

;; some variables are required to be set in order to run the function

  ; NOTE: Since we generate unclumped crop fields by picking random woodland patches and turning them into crops, it is
  ; possible to choose the first N/5 patches pathologically such that all 4N/5 remaining patches are adjacent to a crop
  ; patch, making it impossible to select another disjoint patch.  So in order to test for disjointness, it is essential
  ; to request no more than 20% proportion crops.  (The pathological arrangement contains crop squares that are all a knight-move
  ; apart, creating a grid of diagonal squares of side sqrt(5).)
  ;
  ; However, empirically it hardly ever fails when proportion-crops is as high as 33%, so we'll use that more aggressive
  ; number, and keep in mind that it may occasionally fail.
  ;
  set proportion-crops 33
  set clumpiness 0.0
  ; We should have 33 crop patches which are all disjoint

;; call the function we're testing
  make-crops

;; test some specific aspects of the results of running that function
  assert-equal task [ count crops ] 33  "count crops"
  assert-equal task [ count woodlands ] 67  "count woodlands"
  assert-equal task [ count crops with [ any? neighbors4 with [is-crop? = TRUE] ] ] 0  "Unclumped crops with crop neighbor"

;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-make-crop-clumps

;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-make-crop-clumps"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  resize-world 0 3 0 3  ;; 4-by-4, not 3-by-3

  ; If we request 9 clumped fields and 0 unclumped fields, then we should get:
  ;  * 9 crop patches and 7 woodland patches
  ;  * 0 crop patches with no crop neighbors (0 disjoint patches)
  ask patches [ set is-crop? FALSE ]

;; call the function we're testing
  make-crop-clumps 9 0

;; test some specific aspects of the results of running that function
  set crops patches with [is-crop? = TRUE]
  set woodlands patches with [is-crop? = FALSE]
  assert-equal task [ count crops ] 9  "count crops"
  assert-equal task [ count woodlands ] 7  "count woodlands"
  assert-equal task [ count crops with [ not any? neighbors4 with [is-crop? = TRUE] ] ] 0  "Clumped crops no crop neighbor"

;; some variables are required to be set in order to run the function

  ; If we request 0 clumped fields and 6 unclumped fields, then we should get:
  ;  * 6 crop patches and 9 woodland patches
  ;  * 0 crop patches with any crop neighbors (all disjoint patches)
  ; NOTE: Since we generate unclumped crop fields by picking random woodland patches and turning them into crops, it is
  ; possible to choose the first N/5 patches pathologically such that all 4N/5 remaining patches are adjacent to a crop
  ; patch, making it impossible to select another disjoint patch.  So in order to test for disjointness, it is essential
  ; to request no more than N/5 unclumped patches.
  ; Here we'll test N/3 unclumped patches, and accept the occasional (very rare) pathological case.
  ask patches [ set is-crop? FALSE ]

;; call the function we're testing
  make-crop-clumps 0 6

;; test some specific aspects of the results of running that function
  set crops patches with [is-crop? = TRUE]
  set woodlands patches with [is-crop? = FALSE]
  assert-equal task [ count crops ] 6  "count crops"
  assert-equal task [ count woodlands ] 10  "count woodlands"
  assert-equal task [ count crops with [ any? neighbors4 with [is-crop? = TRUE] ] ] 0  "Unclumped crops with crop neighbor"

;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-setup-cow

;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-setup-cow"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  resize-world 0 3 0 3  ;; 4-by-4, not 3-by-3
  set woodlands (patch-set patch 1 1)  ; cows must start on a woodland patch
  set crops patches with [not member? self woodlands]
  set rainfall-data (list 1)
  set-patch-variables

  ; define params needed to initialize cow weight
  set max-cow-mass 200
  set min-cow-mass 100
  set calf-birth-mass 50
  set ticks-per-day 5
  set cow-maintenance-energy-per-tick-per-kg 1

  ask cows [die]
  create-cows 1
  let uninitialized-cow one-of cows  ; the only cow
  ask uninitialized-cow [
;; test some specific aspects of the results of running that function
    assert-equal task [ [pxcor] of patch-here ] 0 "pxcor of uninitialized cow"
    assert-equal task [ [pycor] of patch-here ] 0 "pycor of uninitialized cow"
    assert-equal task [ body-mass ] 0 "body-mass of uninitialized cow"
  ]

  ask uninitialized-cow [
;; call the function we're testing
    setup-cow false   ;; false = "adult cow, not newborn calf"
    ;; "uninitialized-cow" is now initialized
;; test some specific aspects of the results of running that function
    assert-equal task [ [pxcor] of patch-here ] 1 "pxcor of initialized cow"
    assert-equal task [ [pycor] of patch-here ] 1 "pycor of initialized cow"
    assert-equal task [ body-mass ] (max-cow-mass + min-cow-mass) / 2 "body-mass of initialized cow"
  ]

  ask cows [die]
  create-cows 1
  set uninitialized-cow one-of cows  ; the only cow
  ask uninitialized-cow [
;; call the function we're testing
    setup-cow true   ;; true = "newborn calf, not adult cow"
    ;; "uninitialized-cow" is now initialized
;; test some specific aspects of the results of running that function
    assert-equal task [ [pxcor] of patch-here ] 1 "pxcor of initialized cow"
    assert-equal task [ [pycor] of patch-here ] 1 "pycor of initialized cow"
    let expected-calf-weight calf-birth-mass
    assert-equal task [ body-mass ] expected-calf-weight "body-mass of initialized cow"
  ]
  ask cows [die]

;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-finish-running-and-clean-up

;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-finish-running-and-clean-up"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  set total-cows 100
  set total-number-of-births 500
  set ticks-per-day 3
  set ticks-per-year 365 * ticks-per-day
  set years-gone 50
  set total-crop-eaten 5000  ; in metric tons
  set count-cows-in-crops 10

  set running true
  set termination-reason ""

;; call the function we're testing
  finish-running-and-clean-up "testing"

;; test some specific aspects of the results of running that function

  ; test that actual-reproductive-rate and crop-eaten-per-cow-per-half-hour are set correctly
  set actual-reproductive-rate calculate-actual-reproductive-rate
  assert-equal task [actual-reproductive-rate] 500 / 100 * 365 * 3 "actual-reproductive-rate"
  set crop-eaten-per-cow-per-half-hour calculate-crop-eaten-per-cow-per-half-hour
  assert-equal task [crop-eaten-per-cow-per-half-hour] 1000 * 5000 / 10 * ticks-per-day / (24 * 2) "crop-eaten-per-cow-per-half-hour"

  ; test that termination-reason and running are set correctly
  assert-equal task [termination-reason] "testing" "termination-reason"
  assert task [not running] "failed to set running to false"

  ; test that if count-cows-in-crops is 0, crop-eaten-per-cow-per-half-hour remains untouched
  set crop-eaten-per-cow-per-half-hour 99999
  set count-cows-in-crops 0

;; call the function we're testing
  finish-running-and-clean-up "testing with no cows in crops ever"
;; test some specific aspects of the results of running that function
  assert-equal task [crop-eaten-per-cow-per-half-hour] 99999 "crop-eaten-per-cow-per-half-hour with no cows in crops"

;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-go
;; "go" is too complicated to test by itself, so this function is here for completeness
;; to indicate that we didn't forget to test "go".

;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-go"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

  ; No testing for go: unfortunately, it's too complicated to test in isolation.

;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-update-yearly-time-events

;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-update-yearly-time-events"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function

  ; Variables to ensure that get-new-rainfall works
  set burn-in false
  set rainfall-type "constant"
  set rainfall-data (list 600 600 600 600 600)

  ; Variables to ensure that set-new-growth-rates works
  set woodland-growth-slope 3.456
  set ha-per-patch 23.45
  set ticks-per-year 5 * 365
  set zero-crop-growth-intercept 200
  set crop-growth-slope 1.2345

  ; Variables required by update-yearly-time-events
  set rainfall-index 0
  set minimum-cows-for-sustainability -1
  set minimum-woodland-for-sustainability -1
  set cow-proportion-to-save 0.4
  set running TRUE

  ; update-yearly-time-events is only triggered when ticks mod ticks-per-year is 0
  set crops (patch-set patch 1 1)  ; cows must start on a woodland patch
  set woodlands patches with [not member? self crops]
  set-patch-variables
  reset-ticks
  tick-advance 100
  set calendar-year 0
  set years-gone 3
  set subsidy "no"

;; call the function we're testing
 update-yearly-time-events

;; test some specific aspects of the results of running that function
  ; update-yearly-time-events should do nothing (e.g. not set calendar-year) in the middle of the year
  assert-equal task [ calendar-year ] 0 "calendar-year (not set mid-year)"

;; some variables are required to be set in order to run the function
  reset-ticks

  ask cows [ die ]
  create-cows 5 [
    setup-cow false
    set is-subsidized? true   ; override default value, so we can detect if update-yearly-time-events resets it
  ]
  assert-equal task [count cows with [is-subsidized?]] 5 "subsidized cows"
  set subsidy "feed"
  assert-equal task [ticks mod ticks-per-year] 0 "ticks mod ticks-per-year"
;; call the function we're testing
  update-yearly-time-events

;; test some specific aspects of the results of running that function
  ; rainfall is not low (< 400), so no cows should be subsidized
  assert-equal task [count cows with [is-subsidized?]] 0 "subsidized cows"

;; some variables are required to be set in order to run the function
  set rainfall-index 0
  set rainfall-data (list 390 390 390)
  assert-equal task [ticks mod ticks-per-year] 0 "ticks mod ticks-per-year"

;; call the function we're testing
  update-yearly-time-events
;; test some specific aspects of the results of running that function
  assert-equal task [count cows with [is-subsidized?]] 2 "subsidized cows"

  ask cows [die]
;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-set-subsidized-cows

;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-set-subsidized-cows"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  set woodlands patches
  ask cows [die]
  create-cows 20 [setup-cow false]
  set cow-proportion-to-save 0.4
  assert-equal task [count cows with [is-subsidized?]] 0 "subsidized cows"

;; call the function we're testing
  set-subsidized-cows
;; test some specific aspects of the results of running that function
  assert-equal task [count cows with [is-subsidized?]] 8 "subsidized cows"

  ask cows [die]
;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-get-new-rainfall

;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-get-new-rainfall"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  set rainfall-index 0
  set rainfall-data (list 100 200 300 400 500 600)

  set burn-in true
;; call the function we're testing
  get-new-rainfall
;; test some specific aspects of the results of running that function
  assert-equal task [rainfall] 350 "rainfall"  ; if burn-in is true, we should get the mean

;; some variables are required to be set in order to run the function
  set burn-in false
  set rainfall-index 0
  set rainfall -1
  set rainfall-type "constant"
;; call the function we're testing
  get-new-rainfall
;; test some specific aspects of the results of running that function
  assert-equal task [rainfall] 350 "rainfall"; if rainfall-type is "constant", we should get the mean

;; some variables are required to be set in order to run the function
  set rainfall-index 0
  set rainfall -1
  set rainfall-type "historical"
;; call the function we're testing
  get-new-rainfall
;; test some specific aspects of the results of running that function
  assert-equal task [rainfall] 100 "rainfall"
;; call the function we're testing
  get-new-rainfall
  assert-equal task [rainfall] 200 "rainfall"
;; call the function we're testing
  get-new-rainfall
  assert-equal task [rainfall] 300 "rainfall"

  ; testing rainfall-type "extreme" depends mostly on correct setting of extreme-rainfall-data
  ; (which is done in set-global-variables) -- we'll skip that here

;; some variables are required to be set in order to run the function
  set rainfall-type "statistical-extreme"
  set rainfall-index 0
  ; create a rainfall data set with mean 200 and st.dev. 100
  set rainfall-data (list 100 300 100 300 100 300)
  set extremity-factor 100
  let extreme-rainfall []
  repeat 20 [
;; call the function we're testing
    get-new-rainfall
;; test some specific aspects of the results of running that function
    set extreme-rainfall lput rainfall extreme-rainfall
  ]
  assert-equal task [min extreme-rainfall] 0 "min(extreme-rainfall)"
  assert task [max extreme-rainfall > 500] "max(extreme-rainfall) didn't exceed 500"

;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-set-new-growth-rates
;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-set-new-growth-rates"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  resize-world 0 3 0 3  ;; 4-by-4, not 3-by-3
  set crops (patch-set patch 1 1 patch 1 2 patch 2 1 patch 2 2 patch 2 3)
  set woodlands patches with [not member? self crops]
  set muonde-projects 20
  set muonde-efficiency 5.5
  let key-resource-efficiency 4   ; hard-coded in set-faster-growing-patches
  set key-resources 25
  set-faster-growing-patches

  set rainfall 500
  set zero-crop-growth-intercept 200
  set woodland-growth-slope 1.1
  set crop-growth-slope 0.87
  set ha-per-patch 600 / 2500
  set ticks-per-year 5 * 365
  set rainfall-data (list 1)

  set-patch-variables
  set-faster-growing-patches
;; call the function we're testing
  set-new-growth-rates

;; test some specific aspects of the results of running that function
  assert-equal task [count crops with [plabel = "M"]] 1 "count of Muonde-improved crops"
  assert-equal task [count crops with [plabel = ""]] 4 "count of unimproved crops"
  assert-equal task [count woodlands with [plabel = "*"]] 3 "count of woodlands with key resources"
  assert-equal task [count woodlands with [plabel = ""]] 8 "count of woodlands without key resources"
  ask crops with [plabel = "M"] [assert-equal task [growth-multiplier] muonde-efficiency "Muonde crop growth multiplier"]
  ask crops with [plabel = ""] [assert-equal task [growth-multiplier] 1 "standard crop growth multiplier"]
  ask woodlands with [plabel = "*"] [assert-equal task [growth-multiplier] key-resource-efficiency "key-resource growth"]
  ask woodlands with [plabel = ""] [assert-equal task [growth-multiplier] 1 "standard woodland growth"]

  ask n-of 3 woodlands [assert-equal task [growth-rate] growth-multiplier * 500 * 1.1 * 0.24 / 1825 "woodland growth rate"]
  ask crops [assert-equal task [growth-rate] growth-multiplier * 300 * 0.87 * 0.24 / 1825 "crop growth rate"]

  set rainfall 190
;; call the function we're testing
  set-new-growth-rates
;; test some specific aspects of the results of running that function
  ask n-of 3 woodlands [assert-equal task [growth-rate] growth-multiplier * 190 * 1.1 * 0.24 / 1825 "woodland growth rate"]
  ask crops [assert-equal task [growth-rate] 0 "crop growth rate"]

;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-update-max-min
;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-update-max-min"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  resize-world 0 3 0 3  ;; 4-by-4, not 3-by-3
  set burn-in false
  set woodlands patches  ; all 16 patches are woodlands
  set max-livestock-number 0
  set min-livestock-number 5  ;; paradoxical, but it will let us verify that both are set

  create-cows 1 [setup-cow false]
  let only-cow one-of cows
  let test-cow-mass 0
  ask only-cow [set test-cow-mass body-mass]

  ask woodlands [set standing-available-biomass 50 * 1000]  ; sum(biomass) = 16 patches * 50 kg / patch = 800 kg
  set max-woodland-mass 799
  set min-woodland-mass 801

;; call the function we're testing
  update-max-min

;; test some specific aspects of the results of running that function
  assert-equal task [max-livestock-number] 1 "max-livestock-number"
  assert-equal task [min-livestock-number] 1 "min-livestock-number"
  assert-equal task [max-woodland-mass] 800 "max-woodland-mass"
  assert-equal task [min-woodland-mass] 800 "min-woodland-mass"

;; display the number of tests run during this test function
  ask cows [die]
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-update-woodland-available-biomass
;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-update-woodland-available-biomass"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  resize-world 0 1 0 1  ;; 2-by-2, not 1-by-1
  set woodlands patches
  ask woodlands [
    set standing-available-biomass 700
    set growth-rate 80
  ]
  ask woodlands [ assert-equal task [standing-available-biomass] 700 "standing-available-biomass" ]
  set rainfall-data (list 1)
  set ha-per-patch 1
  set woodland-growth-slope 800 / 5
;; call the function we're testing
  update-woodland-available-biomass

;; test some specific aspects of the results of running that function
  ask woodlands [
    assert-equal task [standing-available-biomass] 780 "standing-available-biomass"
    assert-equal task [pcolor] 30 + 5 * (780 / 800) "pcolor"
  ]

;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-convert-cow-energy-pool-to-body-mass-change
;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-convert-cow-energy-pool-to-body-mass-change"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  set kcal-per-kg-of-cow 10
  set production-efficiency 0.5
  set catabolism-efficiency 0.9
  set min-cow-mass 100
  set max-cow-mass 200
  set min-calf-mass 0  ; for easy satiety computation

  set woodlands patches
  create-cows 1 [setup-cow true]  ; sure, we'll test a calf, why not...
  let only-cow one-of cows
  ask only-cow [
    set body-mass 140
    adjust-energy 10
    ;; this test also checks the function 'adjust-energy'
    assert-equal task [energy-this-tick] 10 "energy-this-tick"
    let expected-mass-change 10 / kcal-per-kg-of-cow * production-efficiency
    ;; this test also checks the reporter 'cow-mass-change-from-current-energy'
    assert-equal task [cow-mass-change-from-current-energy] expected-mass-change "cow-mass-change-from-current-energy"

;; call the function we're testing
    convert-cow-energy-pool-to-body-mass-change

;; test some specific aspects of the results of running that function
    assert-equal task [body-mass] 140.5 "body-mass"
    assert-equal task [satiety] 0.7025 "satiety"
    adjust-energy -50
    assert-equal task [energy-this-tick] -50 "energy-this-tick"
    set expected-mass-change -50 / kcal-per-kg-of-cow / catabolism-efficiency
    assert-equal task [cow-mass-change-from-current-energy] expected-mass-change "cow-mass-change-from-current-energy"
;; call the function we're testing
    convert-cow-energy-pool-to-body-mass-change
    assert-equal task [body-mass] 140.5 + expected-mass-change "body-mass"
    assert-equal task [satiety] (140.5 + expected-mass-change - min-calf-mass) / (max-cow-mass - min-calf-mass) "satiety"
;; call the function we're testing
    convert-cow-energy-pool-to-body-mass-change
    adjust-energy 2000
    assert-equal task [energy-this-tick] 2000 "energy-this-tick"
    set expected-mass-change 2000 / kcal-per-kg-of-cow * production-efficiency
    assert-equal task [cow-mass-change-from-current-energy] expected-mass-change "cow-mass-change-from-current-energy"
;; call the function we're testing
    convert-cow-energy-pool-to-body-mass-change
    assert-equal task [body-mass] 200 "body-mass"
    assert-equal task [satiety] 1.0 "satiety"
  ]

;; display the number of tests run during this test function
  ask cows [die]
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-update-crops
;; update-crops doesn't do anything other than calling other functions, so this function is here for completeness
;; to indicate that we didn't forget to test update-crops.

;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-update-crops"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

  ; No testing for update-crops: it just calls other functions, which are each pretty complicated.

;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-update-crop-color-symbol
;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-update-crop-color-symbol"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  set crops patches
  set woodlands patches with [not member? self crops]
  ask crops [set is-crop? TRUE]
  ask woodlands [set is-crop? FALSE]
  set rainfall-data (list 1)
  set-patch-variables  ; initializes growth-multiplier
  let fenced-crop one-of crops
  ask fenced-crop [set fence 1.0]
  set muonde-efficiency 5.5
  let muonde-crop one-of crops with [fence = 0.0]
  ask muonde-crop [set growth-multiplier muonde-efficiency]
  let normal-crop one-of crops with [fence = 0.0 and growth-multiplier = 1.0]

  let max-crop-biomass 200
  set invincible-fences false
  ask crops [
    set standing-available-biomass 0
;; call the function we're testing
    update-crop-color-symbol (max-crop-biomass)
  ]
;; test some specific aspects of the results of running that function
  ask normal-crop [
    assert-equal task [plabel] "" "normal-crop plabel"
    assert-equal task [pcolor] 50 "normal-crop pcolor"
  ]
  ask fenced-crop [
    assert-equal task [plabel] "X" "fenced-crop plabel"
    assert-equal task [pcolor] 50 "fenced-crop pcolor"
  ]
  ask muonde-crop [
    assert-equal task [plabel] "M" "muonde-crop plabel"
    assert-equal task [pcolor] 50 "muonde-crop pcolor"
  ]

;; some variables are required to be set in order to run the function
  set invincible-fences true
  ask fenced-crop [
    set plabel "s"
;; call the function we're testing
    update-crop-color-symbol (max-crop-biomass)
;; test some specific aspects of the results of running that function
    assert-equal task [plabel] "s" "fenced-crop plabel"
  ]

;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-plough
;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-plough"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  resize-world 0 1 0 1  ;; 2-by-2, not 1-by-1
  set crops (patch-set patch 0 0 patch 1 1)
  set woodlands (patch-set patch 0 1 patch 1 0)
  ask crops [set is-crop? TRUE]
  ask woodlands [set is-crop? FALSE]
  set rainfall-data (list 1)
  set-patch-variables
  set max-cow-mass 200
  set min-cow-mass 100
  set kcal-per-kg-of-cow 10
  set catabolism-efficiency 0.9

  create-cows 5 [
    setup-cow false
    set body-mass 200
  ]
  set cow-working-energy-per-hour 100
  set hours-to-plough-ha 1
  set ha-per-patch 1
  set cows-available-to-work no-turtles   ;; welcome to NetLogo, where a cow is a type of turtle
  set cows-available-to-reproduce no-turtles
  ask crops [
    set standing-available-biomass 50
    set growth-rate 10
  ]

  ; This should do nothing, since there are no worker cows
  ask crops [
;; call the function we're testing
    plough
;; test some specific aspects of the results of running that function
    assert-equal task [standing-available-biomass] 50 "crop standing-available-biomass"
  ]
  ask cows [
    assert-equal task [body-mass] 200 "cow body mass"
  ]

;; some variables are required to be set in order to run the function
  set cows-available-to-work cows  ; we're not testing find-available-cows here
  set cows-available-to-reproduce cows
  ask crops [
;; call the function we're testing
    plough
;; test some specific aspects of the results of running that function
    assert-equal task [standing-available-biomass] 60 "crop standing-available-biomass after ploughing"
  ]
  ask cows [
    convert-cow-energy-pool-to-body-mass-change
  ]
  let mass-lost sum [200 - body-mass] of cows with [body-mass != 200]
  ; either two cows ploughed (expending 9 body-mass of energy each, in this artificial example), or else one cow ploughed twice
  let expected-mass-lost (count crops) * cow-working-energy-per-hour * hours-to-plough-ha * ha-per-patch / catabolism-efficiency / kcal-per-kg-of-cow
  assert-float-equal task [mass-lost] expected-mass-lost "mass-lost"

  assert task [count cows with [body-mass = 200] <= 4] "No cows ploughed"

;; display the number of tests run during this test function
  ask cows [die]
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-harvest
;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-harvest"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  resize-world 0 9 0 9  ;; 10-by-10, not 9-by-9
  set burn-in false
  set running TRUE
  set crops n-of 80 patches
  set woodlands patches with [not member? self crops]
  ask crops [set is-crop? TRUE]
  ask woodlands [set is-crop? FALSE]
  set rainfall-data (list 1)
  set-patch-variables
  set previous-harvests-list []
  set total-harvest 0
  set minimum-harvest-for-sustainability 0

  set how-long-to-store-grain 2
  ask crops [set standing-available-biomass 1000]
  ask one-of crops [assert-equal task [standing-available-biomass] 1000 "crop biomass"]
  assert-equal task [length previous-harvests-list] 0 "previous harvests count"
  assert-equal task [total-harvest] 0 "total-harvest"
;; call the function we're testing
  harvest  ;; total-harvest increases by 80
;; test some specific aspects of the results of running that function
  assert-equal task [total-harvest] 80 "total-harvest"
  ask one-of crops [assert-equal task [standing-available-biomass] 0 "crop biomass"]
  assert-equal task [length previous-harvests-list] 1 "previous harvests count"
  assert-equal task [mean previous-harvests-list] 80 "previous harvests mean"  ;; mean of (80)

;; some variables are required to be set in order to run the function
  ask crops [set standing-available-biomass 2000]
  ask one-of crops [assert-equal task [standing-available-biomass] 2000 "crop biomass"]
;; call the function we're testing
  harvest  ;; total-harvest increases by 160
;; test some specific aspects of the results of running that function
  assert-equal task [total-harvest] 240 "total-harvest"
  ask one-of crops [assert-equal task [standing-available-biomass] 0 "crop biomass"]
  assert-equal task [length previous-harvests-list] 2 "previous harvests count"
  assert-equal task [mean previous-harvests-list] 120 "previous harvests mean"  ;; mean of (80, 160)

;; some variables are required to be set in order to run the function
  ask crops [set standing-available-biomass 3000]
  ask one-of crops [assert-equal task [standing-available-biomass] 3000 "crop biomass"]
;; call the function we're testing
  harvest  ;; total-harvest increases by 240
;; test some specific aspects of the results of running that function
  assert-equal task [total-harvest] 480 "total-harvest"
  ask one-of crops [assert-equal task [standing-available-biomass] 0 "crop biomass"]
  assert-equal task [length previous-harvests-list] 3 "previous harvests count"
  assert-equal task [mean previous-harvests-list] 160 "previous harvests mean"  ;; mean of (80, 160, 240)

;; some variables are required to be set in order to run the function
  ask crops [set standing-available-biomass 4000]
  ask one-of crops [assert-equal task [standing-available-biomass] 4000 "crop biomass"]
;; call the function we're testing
  harvest  ;; total-harvest increases by 320
;; test some specific aspects of the results of running that function
  assert-equal task [total-harvest] 800 "total-harvest"
  ask one-of crops [assert-equal task [standing-available-biomass] 0 "crop biomass"]
  assert-equal task [length previous-harvests-list] 3 "previous harvests count"
  assert-equal task [mean previous-harvests-list] 240 "previous harvests mean"  ;; mean of (160, 240, 320)

;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-update-cows
;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-update-cows"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  resize-world 0 9 0 9  ;; 10-by-10, not 9-by-9
  reset-ticks
  set ticks-per-day 5
  set ticks-per-year 365 * ticks-per-day
  ask cows [die]
  set burn-in false
  set crops patches with [pxcor < 5]   ; all woodland squares must allow cows to move to another woodland square!
                                       ; otherwise the cow might not burn the expected move energy -> assertion fail
  set woodlands patches with [ not member? self crops ]
  ask crops [set is-crop? TRUE]
  ask woodlands [set is-crop? FALSE]
  set rainfall-data (list 1)
  set-patch-variables
  set subsidy "transport"
  set livestock-not-reproduction-rate-per-year 1   ; for this test, cows don't reproduce (so we know their mass)

  ;; because of these vars, update-cows will NOT set woodland-destinations-for-cows
  set times-per-day-farmers-move-cows 1
  tick-advance 5

  set min-cow-mass 100
  set max-cow-mass 299
  set calf-birth-mass 80
  set cow-maintenance-energy-per-tick-per-kg 0.015
  let cow-energy-cost-to-work 0.020
  set kcal-per-kg-of-browse 1 / 7.5
  set kcal-per-kg-of-cow 1000
  set catabolism-efficiency 0.9

  create-cows 10 [
    setup-cow false
    set body-mass 150
  ]
  let cows-250 n-of 3 cows
  ask cows-250 [set body-mass 250]
  let cows-105 one-of cows with [body-mass = 150]
  ask cows-105 [set body-mass 105]
  let cows-150 cows with [body-mass = 150]

  ask woodlands [set standing-available-biomass 100]
  ask n-of 5 woodlands [set standing-available-biomass 200]
  ask n-of 5 woodlands with [standing-available-biomass = 100] [set standing-available-biomass 150]

  ; First, just test woodland-destinations-for-cows
  ask cows [set is-subsidized? true]
;; call the function we're testing
  update-cows

;; test some specific aspects of the results of running that function
  ask cows [
    assert-equal task [color] blue "subsidized cow color"
    assert task [body-mass = 105 or body-mass = 150 or body-mass = 250] (word "Unexpected body mass (" body-mass ") for cow which shouldn't have moved")
  ]

  ;; technically this is testing find-available-cows, but we'll do it anyway
  assert-equal task [count cows-available-to-work] 0 "count cows-available-to-work"  ; transport-subsidized, not available
  assert-equal task [count cows-available-to-reproduce] 3 "count cows-available-to-reproduce"

;; some variables are required to be set in order to run the function

  ask cows [set is-subsidized? false]
  ask n-of 2 cows-150 [set is-subsidized? true]
  ask cows with [not is-subsidized?] [set standing-available-biomass 99]  ;; make sure cows want to move for better grazing
;; call the function we're testing
  update-cows

;; test some specific aspects of the results of running that function
  assert-equal task [count cows with [is-subsidized? = false]] 8 "unsubsidized cows"
  assert-equal task [count cows-available-to-work] 8 "count cows-available-to-work"
  ask cows with [is-subsidized? = true] [
    assert-equal task [color] blue "subsidized cow color"
  ]
  ask cows with [is-subsidized? = false] [
    assert-equal task [color] (ifelse-value (body-mass < 200) [red] [white]) "unsubsidized cow color"
  ]

  ;; Test the three weight classes of cows, making sure that they've gained and lost weight proportionally.
  ;; Note that proportional weight loss due to movement happens after  weight loss due to maintenance, so it's proportional
  ;;   to the cow's now-slightly-lower weight.
  ;;
  let expected-maintenance-kg cow-maintenance-energy-per-tick-per-kg * 250
  let expected-grazing-kg  ( 0 + cow-maintenance-energy-per-tick-per-kg ) * max-cow-mass
  let expected-net-gain-kg expected-grazing-kg - expected-maintenance-kg
  let expected-production-inefficiency-kg
      ifelse-value (expected-net-gain-kg > 0) [(1 - production-efficiency) * expected-net-gain-kg] [0]
  ask cows-250 [
    ifelse not is-subsidized? [
      assert-equal task [body-mass] 250 + expected-net-gain-kg - expected-production-inefficiency-kg "cow-250 body mass"
    ][
      assert-equal task [body-mass] 250 "cow-250 body mass"
    ]
  ]
  set expected-maintenance-kg cow-maintenance-energy-per-tick-per-kg * 150
  set expected-net-gain-kg expected-grazing-kg - expected-maintenance-kg
  set expected-production-inefficiency-kg
      ifelse-value (expected-net-gain-kg > 0) [(1 - production-efficiency) * expected-net-gain-kg] [0]
  ask cows-150 [
    ifelse not is-subsidized? [
      assert-equal task [body-mass] 150 + expected-net-gain-kg - expected-production-inefficiency-kg "cow-150 body mass"
    ][
      assert-equal task [body-mass] 150 "cow-150 body mass"
    ]
  ]
  set expected-maintenance-kg cow-maintenance-energy-per-tick-per-kg * 105
  set expected-net-gain-kg expected-grazing-kg - expected-maintenance-kg
  set expected-production-inefficiency-kg
      ifelse-value (expected-net-gain-kg > 0) [(1 - production-efficiency) * expected-net-gain-kg] [0]
  ask cows-105 [
    ifelse not is-subsidized? [
      assert-float-equal task [body-mass] 105 + expected-net-gain-kg - expected-production-inefficiency-kg "cow-105 body mass"
    ][
      assert-float-equal task [body-mass] 105 "cow-105 body mass"
    ]
  ]

;; display the number of tests run during this test function
  ask cows [die]
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-find-available-cows
;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-find-available-cows"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  resize-world 0 9 0 9  ;; 10-by-10, not 9-by-9
  reset-ticks
  ask cows [die]
  set crops n-of 50 patches
  set woodlands patches with [not member? self crops]
  ask crops [set is-crop? TRUE]
  ask woodlands [set is-crop? FALSE]
  set rainfall-data (list 1)
  set-patch-variables
  set subsidy "transport"

  set min-cow-mass 100
  set max-cow-mass 299
  set calf-birth-mass 80
  set cow-maintenance-energy-per-tick-per-kg 0.1
  let cow-energy-cost-to-work 9
  set kcal-per-kg-of-cow 10
  set catabolism-efficiency 0.9
  ;; for these paramerts, cows have to have body-mass at least about 180 in order to reproduce.
  ;; and a body-mass of at least about 102.1 or so
  create-cows 3 [
    setup-cow true
    set body-mass 299
  ]   ;; calves -- shouldn't be available to work or reproduce, even if they're the fattest, healthiest cows around
  let calves cows with [is-calf? = true]

  create-cows 4 [
    setup-cow false
    set body-mass 250
  ]
  let cows-250 cows with [body-mass = 250]  ;; should be available to work or reproduce

  create-cows 2 [
    setup-cow false
    set body-mass 175
  ]
  let working-non-parent-cows cows with [body-mass = 175] ;; should be able to work but not reproduce

  create-cows 2 [
    setup-cow false
    set body-mass 100
  ]
  let cows-100 cows with [body-mass = 100]  ;; should NOT be available to work or reproduce -- too thin

  create-cows 4 [
    setup-cow false
    set is-subsidized? true
    set body-mass 250
  ]
  let cows-subsidized cows with [is-subsidized? = true]  ;; should NOT be available to work (since subsidy == "transport")
                                                        ;; can still reproduce

;; call the function we're testing
  find-available-cows

;; test some specific aspects of the results of running that function
  ask calves [
    assert task [not member? self cows-available-to-work] "cows-available-to-work included a calf"
    assert task [not member? self cows-available-to-reproduce] "cows-available-to-reproduce included a calf"
  ]
  ask cows-100 [
    assert task [not member? self cows-available-to-work] "cows-available-to-work included a scrawny cow"
    assert task [not member? self cows-available-to-reproduce] "cows-available-to-reproduce included a scrawny cow"
  ]
  ask cows-subsidized [
    assert task [not member? self cows-available-to-work] "cows-available-to-work included a subsidized cow"
    assert task [member? self cows-available-to-reproduce] "cows-available-to-reproduce excluded a healthy subsidized cow"
  ]
  ask working-non-parent-cows [
    assert task [member? self cows-available-to-work] "cows-available-to-work excluded a healthy non-parent cow"
    assert task [not member? self cows-available-to-reproduce] "cows-available-to-reproduce included a scrawny working cow"
  ]
  ask cows-250 [
    assert task [member? self cows-available-to-work] "cows-available-to-work excluded a healthy cow"
    assert task [member? self cows-available-to-reproduce] "cows-available-to-reproduce excluded a healthy cow"
  ]

;; some variables are required to be set in order to run the function
  ; min-work-weight is not exactly correct, but it's close enough to be within 0.1kg
  let min-work-weight min-cow-mass + cow-working-energy-per-hour * hours-to-plough-ha * ha-per-patch / kcal-per-kg-of-cow + (
      cow-maintenance-energy-per-tick-per-kg * min-cow-mass / kcal-per-kg-of-cow)
  ask cows [die]
  create-cows 1 [
    setup-cow false
    set body-mass min-work-weight - 0.1
  ]
  let cow-barely-cant-work cows with [body-mass = min-work-weight - 0.1]
  create-cows 1 [
    setup-cow false
    set body-mass min-work-weight + 0.1
  ]
  let cow-barely-can-work cows with [body-mass = min-work-weight + 0.1]

;; call the function we're testing
  find-available-cows

;; test some specific aspects of the results of running that function
  ask cow-barely-cant-work [ assert task [not member? self cows-available-to-work] "Cow wasn't barely excluded from work" ]
  ask cow-barely-can-work [ assert task [member? self cows-available-to-work] "Cow wasn't barely included in work" ]

;; some variables are required to be set in order to run the function

  ; min-repro-weight is not exactly correct, but it's close enough to be within 0.1kg
  let min-repro-weight min-cow-mass + calf-birth-mass + cow-maintenance-energy-per-tick-per-kg * (
    min-cow-mass + calf-birth-mass) / kcal-per-kg-of-cow
  ask cows [die]
  create-cows 1 [
    setup-cow false
    set body-mass min-repro-weight - 0.1
  ]
  let cow-barely-cant-repro cows with [body-mass = min-repro-weight - 0.1]
  create-cows 1 [
    setup-cow false
    set body-mass min-repro-weight + 0.1
  ]
  let cow-barely-can-repro cows with [body-mass = min-repro-weight + 0.1]

;; call the function we're testing
  find-available-cows

;; test some specific aspects of the results of running that function
  ask cow-barely-cant-repro [
      assert task [not member? self cows-available-to-reproduce] "Cow wasn't barely excluded from repro"
  ]
  ask cow-barely-can-repro [ assert task [member? self cows-available-to-reproduce] "Cow wasn't barely included in repro" ]

;; display the number of tests run during this test function
  ask cows [die]
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-move

;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-move"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

  resize-world 0 9 0 9  ;; 10-by-10, not 9-by-9
  reset-ticks
  set crops patches with [pxcor < 5]
  set woodlands patches with [ not member? self crops ]
  ask crops [set is-crop? TRUE]
  ask woodlands [set is-crop? FALSE]

  create-cows 10 [
    setup-cow false
    set body-mass 150
  ]

  ask woodlands [set standing-available-biomass 100]
  ask n-of 5 woodlands [set standing-available-biomass 200]
  ask n-of 5 woodlands with [standing-available-biomass = 100] [set standing-available-biomass 150]
  let woodland-destinations-for-cows globally-find-suitable-patches

  ;; woodland-destinations-for-cows should get 10 patches (because 10 cows)
  ;; and they should be the patches with the most standing-available-biomass
  assert-equal task [count woodland-destinations-for-cows] 10 "woodland dests"
  assert-equal task [count woodland-destinations-for-cows with [standing-available-biomass = 200]] 5 "woodland dests (200)"
  assert-equal task [count woodland-destinations-for-cows with [standing-available-biomass = 150]] 5 "woodland dests (150)"

  ; checking that with a different number of cows, we find a smaller number of patches when we globally find them
  ask n-of 4 cows [die]
  set woodland-destinations-for-cows globally-find-suitable-patches
  assert-equal task [count woodland-destinations-for-cows] 6 "woodland dests when fewer cows"
  ; now remove the lowest-biomass one, leaving all five 200 biomass patches
  set woodland-destinations-for-cows update-globally-suitable-patches (woodland-destinations-for-cows)
  assert-equal task [count woodland-destinations-for-cows] 5 "woodland dests after single update"
  assert-equal task [mean ([standing-available-biomass] of woodland-destinations-for-cows)] 200 "mean woodland biomass after update"
  ask woodlands with [standing-available-biomass = 200] [set standing-available-biomass 50]
  ask one-of woodlands with [standing-available-biomass = 50] [set standing-available-biomass 10]
  ;now it should be the four 50 patches chosen
  set woodland-destinations-for-cows update-globally-suitable-patches (woodland-destinations-for-cows)
  assert-equal task [mean ([standing-available-biomass] of woodland-destinations-for-cows)] 50 "mean woodland biomass after further update"

;; some variables are required to be set in order to run the function
  resize-world 0 5 0 5  ;; 6-by-6, not 5-by-5
  ;; C  C  w- w  w  w
  ;; C  C  C  w  w  w    Key:
  ;; C  C  w  w  w  w      C  = crop
  ;; w  w  w  w  w  w      w  = woodland with average biomass
  ;; w  w  w  w  w  w      w- = woodland with below-average biomass
  ;; w  w  w  w  w  w+     w+ = woodland with above-average biomass
  set crops (patch-set patch 0 0 patch 0 1 patch 1 0 patch 1 1 patch 1 2 patch 2 0 patch 2 1)
  set woodlands patches with [not member? self crops]
  set invincible-fences true
  ask crops [set is-crop? TRUE]
  ask woodlands [set is-crop? FALSE]
  set rainfall-data (list 1)
  set-patch-variables
  set crops-with-fences crops with [any? neighbors4 with [is-woodland? = true]]
  configure-fences
  ask woodlands [ set standing-available-biomass 400 ]
  ask patch 0 2 [ set standing-available-biomass 100 ]
  ask patch 5 5 [ set standing-available-biomass 700 ]
  assert task [member? (patch 0 1) crops-with-fences] "patch 0,1 isn't in crops-with-fences"

  set max-cow-mass 300
  set min-cow-mass 100
  set times-per-day-farmers-move-cows 1
  let ticks-between-farmers-moving-cows 5
  reset-ticks
  let farmers-moving-cows (ticks mod ticks-between-farmers-moving-cows) < 1
  set woodland-destinations-for-cows woodlands   ;; normally reduced in update-cows, but we don't care about efficiency here

  ask cows [die]
  create-cows 1 [ setup-cow false ]   ; body-mass will be 200 = (max-cow-mass + min-cow-mass) / 2
  let only-cow one-of cows
  ask only-cow [move-to patch 0 2]


;; call the function we're testing
  ask only-cow [ move farmers-moving-cows woodland-destinations-for-cows ]  ; ticks = 0, so farmers move this cow to optimal grazing

;; test some specific aspects of the results of running that function
  ;; cow body mass shouldn't change from moving because it's included in maintenance energy.
  assert-equal task [[body-mass] of only-cow] 200 "body-mass of cow after moving"
  assert-equal task [[patch-here] of only-cow] patch 5 5 "location of cow after moving"

;; call the function we're testing
  ask only-cow [ move farmers-moving-cows woodland-destinations-for-cows  ]  ; farmers attempt to move cow, but it's already at the optimal location
;; test some specific aspects of the results of running that function
  assert-equal task [[patch-here] of only-cow] patch 5 5 "location of cow after moving"
  ; cow should not lose any mass, since it didn't expend any energy moving
  assert-equal task [[body-mass] of only-cow] 200 "body-mass of cow after moving"

;; some variables are required to be set in order to run the function
  ask cows [die]
  create-cows 1 [ setup-cow false ]   ; body-mass will be 200 = (max-cow-mass + min-cow-mass) / 2
  set only-cow one-of cows
  reset-ticks
  tick-advance 2
  set farmers-moving-cows (ticks mod ticks-between-farmers-moving-cows) < 1
  ask only-cow [move-to patch 0 2]

;; call the function we're testing
  ask only-cow [ move farmers-moving-cows woodland-destinations-for-cows  ]  ; ticks = 2, so cow moves itself locally

;; test some specific aspects of the results of running that function
  assert-equal task [[body-mass] of only-cow] 200 "body-mass of cow after moving"
  assert-equal task [[patch-here] of only-cow] patch 0 3 "location of cow after moving"

  ask only-cow [move-to patch 5 5]
;; call the function we're testing
  ask only-cow [ move farmers-moving-cows woodland-destinations-for-cows ]  ; cow thinks about moving itself locally, but realizes that the grass is greener where it is
;; test some specific aspects of the results of running that function
  assert-equal task [[patch-here] of only-cow] patch 5 5 "location of cow after moving"
  ; cow should not lose any mass, since it didn't expend any energy moving
  assert-equal task [[body-mass] of only-cow] 200 "body-mass of cow after moving"

  ask cows [die]
;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-find-nearby-patch-to-eat
;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-find-nearby-patch-to-eat"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  resize-world 0 5 0 5  ;; 6-by-6, not 5-by-5
  ;; C  C  w- w  w  w
  ;; C  C  C  w  w  w    Key:
  ;; C  C  w  w  w  w      C  = crop
  ;; w  w  w  w  w  w      w  = woodland with average biomass
  ;; w  w  w  w  w  w      w- = woodland with below-average biomass
  ;; w  w  w  w  w  w+     w+ = woodland with above-average biomass
  set crops (patch-set patch 0 0 patch 0 1 patch 1 0 patch 1 1 patch 1 2 patch 2 0 patch 2 1)
  set woodlands patches with [not member? self crops]
  set invincible-fences true
  ask crops [set is-crop? TRUE]
  ask woodlands [set is-crop? FALSE]
  set rainfall-data (list 1)
  set-patch-variables
  set crops-with-fences crops with [any? neighbors4 with [is-woodland? = true]]
  configure-fences
  ask woodlands [ set standing-available-biomass 400 ]
  ask patch 0 2 [ set standing-available-biomass 100 ]
  ask patch 5 5 [ set standing-available-biomass 700 ]
  ask patch 1 2 [ set standing-available-biomass 800 ]  ;; that is one delicious-looking crop field, hope it has good fences...

  assert task [member? (patch 0 1) crops-with-fences] "patch 0,1 isn't in crops-with-fences"

  set max-cow-mass 300
  set min-cow-mass 100
  set ticks-per-day 5
  set times-per-day-farmers-move-cows 1
  let woodland-destinations-for-cows woodlands   ;; normally reduced in update-cows, but we don't care about efficiency here

  ask cows [die]
  create-cows 1 [ setup-cow false ]   ; body-mass will be 200 = (max-cow-mass + min-cow-mass) / 2
  reset-ticks
  let only-cow one-of cows
  ask only-cow [move-to patch 0 2]
  let new-location nobody

;; call the function we're testing
  ask only-cow [
    set new-location find-nearby-patch-to-eat
    ]

;; test some specific aspects of the results of running that function
  ;; Cow is on a low-biomass woodland patch, but can't move to nearby average-biomass woodland, because it's too busy
  ;; dreaming about those delicious crops behind the invincible fence
  assert-equal task [new-location] patch 0 2  "new-location"

;; some variables are required to be set in order to run the function
  set new-location nobody
  set invincible-fences false
  ask patch 1 2 [set fence 0]
;; call the function we're testing
  ask only-cow [
    set new-location find-nearby-patch-to-eat
    ]
;; test some specific aspects of the results of running that function
  assert-equal task [new-location] patch 1 2  "new-location"  ; With no fence, the cow can now live its dream.

;; some variables are required to be set in order to run the function
  set new-location nobody
  ask patch 1 2 [set fence 0.1]
  ask only-cow [ set satiety 1 ]
;; call the function we're testing
  ask only-cow [
    set new-location find-nearby-patch-to-eat
    ]
  ;; Cow is fat and happy, so not inclined to break through even a weak fence.  It stays on its current patch.
;; test some specific aspects of the results of running that function
  assert-equal task [new-location] patch 0 2  "new-location"

  ask cows [die]
;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-fence-effectiveness-per-tick
;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-fence-effectiveness-per-tick"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  set ticks-per-day 3

  ;; Function arguments: fence-effectiveness-per-tick fence-quality cow-satiety

  ;; Test: returns 1 when fence-quality and cow-satiety are both 1
;; call the function we're testing
  let testval fence-effectiveness-per-tick 1.0 1.0
;; test some specific aspects of the results of running that function
  assert-equal task [testval] 1.0 "fence-effectiveness-per-tick"

  ;; Test: returns < 0.8 when fence-quality and cow-satiety are both 0.1
  ;; Note that this is per-tick, so at 3 ticks/day, probability per day is less than 0.512
  ;; (This qualitatively indicates that decayed fences and hungry cows make fences ineffective, without putting
  ;; severe restrictions on the mathematical details.)
;; call the function we're testing
;; test some specific aspects of the results of running that function
  assert task [fence-effectiveness-per-tick 0.2 0.2 < 0.8] "fence-effectiveness-per-tick 0.2 0.2 >= 0.8"

  ;; Test: returns 0 when fence-quality and cow-satiety are both 0
;; call the function we're testing
;; test some specific aspects of the results of running that function
  assert-equal task [fence-effectiveness-per-tick 0.0 0.0] 0.0 "fence-effectiveness-per-tick"

;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-consume
;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-consume"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  resize-world 0 1 0 1  ;; 2-by-2, not 1-by-1
  set crops (patch-set patch 0 0)
  set woodlands patches with [not member? self crops]
  set rainfall-data (list 1)
  set-patch-variables
  ask crops [set is-crop? TRUE]
  ask woodlands [set is-crop? FALSE]
  ask patch 0 0 [ set standing-available-biomass 400 ]
  ask patch 0 1 [ set standing-available-biomass 0 ]
  ask patch 1 0 [ set standing-available-biomass 20 ]
  ask patch 1 1 [ set standing-available-biomass 400 ]

  set cow-maintenance-energy-per-tick-per-kg 0.02
  set min-cow-mass 100
  set max-cow-mass 300
  let starting-cow-mass (max-cow-mass + min-cow-mass) / 2
  set kcal-per-kg-of-browse 1 / 5
  set kcal-per-kg-of-crop 1 / 2
  set crop-eaten 10
  set woodland-eaten 20
  set burn-in false

  ask cows [die]
  create-cows 1 [setup-cow false]
  let only-cow one-of cows

  ;; if a cow can eat as much as it wants, it gains the following number of kcal:
  ;; ( cow-maintenance-energy-per-tick-per-kg ) * max-cow-mass
  ;; = (.02) * 300 = 6
  ;; unrestricted means it's not restricted by the biomass on the current patch
  let unrestricted-cow-energy-change 6
  let unrestricted-woodland-biomass-consumed unrestricted-cow-energy-change / kcal-per-kg-of-browse
  let unrestricted-crop-biomass-consumed unrestricted-cow-energy-change / kcal-per-kg-of-crop

  ;; Test: cows consume crops
  ask only-cow [
    move-to patch 0 0
;; call the function we're testing
    consume
;; test some specific aspects of the results of running that function
    assert-equal task [[standing-available-biomass] of patch 0 0] 400 - unrestricted-crop-biomass-consumed "biomass on patch 0,0"
    assert-equal task [energy-this-tick] (unrestricted-cow-energy-change) "cow energy after eating crops"
    assert-equal task [crop-eaten] (10 + 0.001 * unrestricted-crop-biomass-consumed) "total crop eaten"
  ]

  ;; Test: correct behavior when woodland has no biomass
  ask only-cow [
    move-to patch 0 1
    set energy-this-tick 0
;; call the function we're testing
    consume
;; test some specific aspects of the results of running that function
    assert-equal task [energy-this-tick] 0 "cow energy after grazing on empty woodland"
    assert-equal task [[standing-available-biomass] of patch 0 1] 0 "biomass on patch 0,1"
    assert-equal task [woodland-eaten] 20 "total woodland eaten after grazing on empty woodland"
  ]

  ;; Test: cow eats correct amount when woodland only has a little biomass
  ask only-cow [
    move-to patch 1 0
    set energy-this-tick 0
;; call the function we're testing
    consume
;; test some specific aspects of the results of running that function
    let expected-energy-change 20 * kcal-per-kg-of-browse
    assert-equal task [[standing-available-biomass] of patch 1 0] 0 "biomass on patch 1,0"
    assert-equal task [energy-this-tick] (expected-energy-change) "cow energy after eating sparse woodland"
    assert-equal task [woodland-eaten] 20.02 "total woodland eaten after eating sparse woodland"
  ]

  ;; Test: cow eats correct amount when woodland has plenty of biomass
  ask only-cow [
    move-to patch 1 1
    set energy-this-tick 0
;; call the function we're testing
    consume
;; test some specific aspects of the results of running that function
    assert-equal task [[standing-available-biomass] of patch 1 1] 400 - unrestricted-woodland-biomass-consumed "biomass on patch 1,1"
    assert-equal task [woodland-eaten] (20.02 + 0.001 * unrestricted-woodland-biomass-consumed)
    "total woodland eaten after eating unrestricted woodland"
  ]

  ask cows [die]
;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-get-hungrier
;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-get-hungrier"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  resize-world 0 1 0 1  ;; 2-by-2, not 1-by-1
  set ticks-per-day 3   ;; affects min-calf-mass
  set crops no-patches
  set woodlands patches with [not member? self crops]
  ask crops [set is-crop? TRUE]
  ask woodlands [set is-crop? FALSE]
  set rainfall-data (list 1)
  set-patch-variables

  set cow-maintenance-energy-per-tick-per-kg 0.02
  set kcal-per-kg-of-browse 1 / 5
  set kcal-per-kg-of-crop 1 / 2
  set kcal-per-kg-of-cow 10
  set min-cow-mass 100
  set max-cow-mass 300
  set calf-birth-mass 20
  set min-calf-mass 18
  set dead-cows 10
  set burn-in false
  set production-efficiency 0.5
  set catabolism-efficiency 0.9
  let starting-cow-mass (max-cow-mass + min-cow-mass) / 2

  ;; Test: Common case, cow gets hungrier
  ask cows [die]
  set subsidy "no"
  create-cows 1 [
    setup-cow false
  ]
  let only-cow one-of cows
  ask only-cow [
    assert-equal task [body-mass] starting-cow-mass "cow mass before getting hungrier"
    assert-equal task [energy-this-tick] 0 "cow energy before getting hungrier"
;; call the function we're testing
    get-hungrier
;; test some specific aspects of the results of running that function
    let expected-mass-loss cow-maintenance-energy-per-tick-per-kg * starting-cow-mass / kcal-per-kg-of-cow / catabolism-efficiency
    assert-equal task [body-mass]  starting-cow-mass - expected-mass-loss "cow mass after getting hungrier"
    assert-equal task [energy-this-tick] 0 "cow energy after getting hungrier"
  ]

  ;; Test: Common case, calf gets hungrier and doesn't die
  ask cows [die]
  create-cows 1 [
    setup-cow true
    set body-mass calf-birth-mass * 1.1
  ]
  set only-cow one-of cows
  ask only-cow [
;; call the function we're testing
    get-hungrier
;; test some specific aspects of the results of running that function
    let expected-mass-loss cow-maintenance-energy-per-tick-per-kg * calf-birth-mass * 1.1 / kcal-per-kg-of-cow / catabolism-efficiency
    assert-equal task [body-mass] calf-birth-mass * 1.1 - expected-mass-loss "calf mass after getting hungrier"
    assert task [body-mass < min-cow-mass] "calf body-mass wasn't less than min-cow-mass"
    assert task [only-cow != nobody] "calf died after getting hungrier"
  ]

  ;; Test: Cow gets hungrier and dies
  ask cows [die]
  set subsidy "no"
  create-cows 1 [
    setup-cow false
    set body-mass min-cow-mass * 1.001
    adjust-energy 0
  ]
  set only-cow one-of cows
  ask only-cow [
;; call the function we're testing
    get-hungrier
;; test some specific aspects of the results of running that function
    assert-equal task [self] nobody "cow failed to starve"
  ]

  ;; Test: Calf gets hungrier and dies
  ask cows [die]
  set subsidy "no"
  create-cows 1 [
    setup-cow true
    set body-mass min-calf-mass * 1.001
    adjust-energy 0
  ]
  set only-cow one-of cows
  ask only-cow [
;; call the function we're testing
    get-hungrier
;; test some specific aspects of the results of running that function
    assert-equal task [self] nobody "calf failed to starve"
  ]

  ;; Test: Feed subsidy works for cows
  ask cows [die]
  set subsidy "feed"
;  set subsidy-level 1.0
  set subsidy-used 100
  set cost-of-supplemental-feed 10.00
  create-cows 1 [
    setup-cow false
    set is-subsidized? true
    set body-mass min-cow-mass
  ]
  set only-cow one-of cows
  let full-portion (cow-maintenance-energy-per-tick-per-kg) * max-cow-mass
  ask only-cow [
;; call the function we're testing
    get-hungrier
;; test some specific aspects of the results of running that function
    assert task [body-mass > min-cow-mass] "subsidized cow didn't end up over min-cow-mass"
    let mass-change (- min-cow-mass * cow-maintenance-energy-per-tick-per-kg / catabolism-efficiency +
       full-portion * production-efficiency) / kcal-per-kg-of-cow
    assert-float-equal task [body-mass] (min-cow-mass + mass-change) "cow body-mass"
    assert task [only-cow != nobody] "subsidized cow died"
    assert-equal task [subsidy-used] (100 + cost-of-supplemental-feed * full-portion
        / kcal-per-kg-of-crop) "subsidy-used"
  ]

  ;; Test: Feed subsidy works for calves
  ask cows [die]
  set subsidy "feed"
;  set subsidy-level 1.0
  set subsidy-used 100
  create-cows 1 [
    setup-cow true
    set is-subsidized? true
    adjust-energy 0
    set body-mass min-calf-mass
  ]
  set only-cow one-of cows
  ask only-cow [
;; call the function we're testing
    get-hungrier
;; test some specific aspects of the results of running that function
    assert task [body-mass > min-calf-mass] "subsidized calf didn't end up over min-calf-mass"
    let mass-change (- min-calf-mass * cow-maintenance-energy-per-tick-per-kg / catabolism-efficiency +
       full-portion * production-efficiency) / kcal-per-kg-of-cow
    assert-float-equal task [body-mass] (min-calf-mass + mass-change) "calf body-mass"
    assert task [only-cow != nobody] "subsidized calf died"
    assert-equal task [subsidy-used] (100 + cost-of-supplemental-feed * full-portion
        / kcal-per-kg-of-crop) "subsidy-used"
  ]

  ask cows [die]
;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-update-fences
;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-update-fences"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  resize-world 0 2 0 2  ;; 3-by-3, not 2-by-2
  set crops (patch-set patch 0 0 patch 0 1 patch 1 0)
  set woodlands patches with [not member? self crops]
  ask crops [set is-crop? TRUE]
  ask woodlands [set is-crop? FALSE]
  set rainfall-data (list 1)
  set-patch-variables
  set crops-with-fences crops with [any? neighbors4 with [is-woodland? = true]]
  configure-fences

  assert-equal task [sum [how-many-fences] of crops-with-fences] 4 "total fence count"  ;; 2 on (1,0) + 2 on (0,1)

  set termite-activity 3.65
  set ticks-per-day 3
  set ticks-per-year ticks-per-day * 365
  let expected-fence-reduction termite-activity / ticks-per-year

  ;; configure-fences initializes fence values for crops-with-fences to random numbers between 0 and 1.
  assert-equal task [count crops-with-fences] 2 "count of crops-with-fences"
  ask crops-with-fences [set fence 1]

  ; Verify expected starting values
  assert-equal task [[fence] of patch 0 0] 0 "fence value for interior crop"
  assert-equal task [[fence] of patch 0 1] 1 "fence value for perimeter crop"
  assert-equal task [[fence] of patch 1 0] 1 "fence value for perimeter crop"
  assert-equal task [[fence] of patch 1 1] 0 "fence value for woodland"

;; call the function we're testing
  update-fences

;; test some specific aspects of the results of running that function

  ; Test that unfenced patches are unaffected
  assert-equal task [[fence] of patch 0 0] 0 "fence value for interior crop"
  assert-equal task [[fence] of patch 1 1] 0 "fence value for woodland"

  ; Test that fenced patches are reduced as expected
  assert-equal task [[fence] of patch 0 1] 1 - expected-fence-reduction "fence value for perimeter crop after decay"
  assert-equal task [[fence] of patch 1 0] 1 - expected-fence-reduction "fence value for perimeter crop after decay"

;; some variables are required to be set in order to run the function

  ; Test that fences are rebuilt when they decay, when there is plenty of fencing material
  set wood-to-build-fence-per-meter 0.1
  set ha-per-patch 100

  ask woodlands [set standing-available-biomass (wood-to-build-fence-per-meter * 100 * sqrt ( ha-per-patch )) - 10]  ;; most woodlands can't be harvested for fences
  ask patch 1 1 [set standing-available-biomass 1000]  ;; ... but this woodland can -- we'll know where the wood comes from
  ask crops-with-fences [set fence 0.9 * expected-fence-reduction]

;; call the function we're testing
  update-fences

;; test some specific aspects of the results of running that function
  assert-equal task [[standing-available-biomass] of patch 1 1] 600 "biomass on patch (1,1)"
  assert-equal task [[fence] of patch 0 1] 1.0 "fence value for (0,1)"
  assert-equal task [[fence] of patch 1 0] 1.0 "fence value for (1,0)"

;; some variables are required to be set in order to run the function

  ; Test that fences are only rebuilt while there is material to build them
  ask crops-with-fences [set fence 0]
  ask patch 1 1 [set standing-available-biomass 307]  ;; enough for 3 of the 4 fences -> one patch with fence=1.0, one with 0.5
;; call the function we're testing
  update-fences
;; test some specific aspects of the results of running that function
  assert-equal task [sum [fence] of crops-with-fences] 1.5 "total fence values after partial repair"
  assert-equal task [[standing-available-biomass] of patch 1 1] 7 "biomass on patch (1,1) after partial repair"

;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-reproduce-cows
;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-reproduce-cows"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  resize-world 0 1 0 1  ;; 2-by-2, not 1-by-1
  set ticks-per-day 3   ;; affects min-calf-mass
  set ticks-per-year ticks-per-day * 365
  set crops no-patches
  set woodlands patches with [not member? self crops]
  ask crops [set is-crop? TRUE]
  ask woodlands [set is-crop? FALSE]
  set rainfall-data (list 1)
  set-patch-variables

  set cow-maintenance-energy-per-tick-per-kg 0.005
  set cow-working-energy-per-hour 1500
  set hours-to-plough-ha 1
  set ha-per-patch 1
  set kcal-per-kg-of-cow 1000
  set min-cow-mass 100
  set max-cow-mass 300
  let starting-cow-mass (max-cow-mass + min-cow-mass) / 2
  set calf-birth-mass 20
  set livestock-not-reproduction-rate-per-year 0  ;; for this test, cows will ALWAYS reproduce
  let livestock-reproduction-rate-per-tick calculate-livestock-reproduction-rate-per-tick
  set catabolism-efficiency 0.9

  ;; Note: test-find-available-cows verifies that cows-available-to-reproduce is set correctly, so we don't test that here.

  ;; Test: nothing outside the cows-available-to-reproduce list is affected
  ask cows [die]
  create-cows 5 [setup-cow false]
  set cows-available-to-reproduce no-turtles
  set cows-available-to-work no-turtles
;; call the function we're testing
  reproduce-cows
;; test some specific aspects of the results of running that function
  assert-equal task [count cows] 5 "count of cows after non-reproduction"

  ;; Test: every cow in cows-available-to-reproduce makes the attempt
  ask cows [die]
  create-cows 5 [setup-cow false]
  set cows-available-to-reproduce cows
;; call the function we're testing
  reproduce-cows
;; test some specific aspects of the results of running that function
  assert-equal task [count cows] 10 "count of cows after reproduction"

  ;; Test: reproduction results in calves
  assert-equal task [count cows with [is-calf?]] 5 "count of calves after reproduction"
  assert-equal task [count cows with [not is-calf?]] 5 "count of adult cows after reproduction"

  ;; Test: body mass is reduced after birth
  let expected-weight-loss calf-birth-mass
  ask cows with [not is-calf?] [
    assert-equal task [body-mass] (starting-cow-mass - expected-weight-loss) "body-mass of cow after birth"
  ]

;; some variables are required to be set in order to run the function

  ;; Starting-weight cows have enough mass to reproduce twice in a row.
  ;; Test: cows that weigh too little are removed from cows-available-to-reproduce and cows-available-to-work
  set cows-available-to-reproduce cows with [not is-calf?]
  ;; If we "let copy-of-current-cows cows", then when a new cow is born, the copy gets the new cow also!
  ;; So we use this hack to stash a copy of the current list, before reproducing.
  let copy-of-current-cows cows with [true]
  set cows-available-to-work copy-of-current-cows  ;; in normal operation, calves are not available to work
  assert-equal task [count cows-available-to-reproduce] 5 "count of reproducing cows"
  assert-equal task [count cows-available-to-work] 10 "count of worker cows"
  ask n-of 3 cows-available-to-reproduce [
    set body-mass 1.00000001 * (min-cow-mass + calf-birth-mass +
      cow-maintenance-energy-per-tick-per-kg * body-mass / kcal-per-kg-of-cow)
    update-available-cows
  ]
  assert-equal task [count cows-available-to-work] 10 "count of worker cows before repro"
;; call the function we're testing
  reproduce-cows
;; test some specific aspects of the results of running that function
  assert-equal task [count cows] 15 "count of cows"
  ; note that cows being born doesn't add any cows to cows-available-to-work or cows-available-to-reproduce
  assert-equal task [count cows-available-to-work] 7 "count of worker cows after repro"
  assert-equal task [count cows-available-to-reproduce] 2 "count of reproducing cows"

;; some variables are required to be set in order to run the function

  ; Test: If a cow that reproduces has enough mass left to work but not to reproduce, it gets removed from one list only.
  ask cows [die]
  create-cows 1 [
    setup-cow false
    set body-mass 1.00001 * min-cow-mass + calf-birth-mass + (
      cow-maintenance-energy-per-tick-per-kg * min-cow-mass + cow-working-energy-per-hour * hours-to-plough-ha * ha-per-patch) / kcal-per-kg-of-cow
  ]
  let only-cow one-of cows
  set cows-available-to-work (turtle-set only-cow)
  set cows-available-to-reproduce (turtle-set only-cow)
;; call the function we're testing
  reproduce-cows
;; test some specific aspects of the results of running that function
  assert-equal task [cows-available-to-work] cows with [not is-calf?] "cows-available-to-work"
  assert-equal task [cows-available-to-reproduce] no-turtles "cows-available-to-reproduce"

  ask cows [die]
;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end

to test-calculate-average-contiguous-crop-cluster-size
;; preamble that keeps track of the current error count and test count (clear-all will remove the variables
;; in memory, but not those with local scope defined using 'let' -- so just after clearing everything, re-set
;; the test and error count), also print to the command center the name of the test.
  let test-name "test-calculate-average-contiguous-crop-cluster-size"
  let start-test-number number-of-tests-run
  output-print (word "Test: " test-name)
  let temp-number-of-tests-run number-of-tests-run
  let temp-error-count error-count
  clear-everything-but-output
  set number-of-tests-run temp-number-of-tests-run
  set error-count temp-error-count

;; some variables are required to be set in order to run the function
  resize-world 0 0 0 9
  set woodlands (patch-set patch 0 3 patch 0 6)  ;; contiguous patches: 012 - 45 - 789
  set crops patches with [not member? self woodlands]
  ask crops [set is-crop? TRUE]
  ask woodlands [set is-crop? FALSE]
  set rainfall-data (list 1)
  set-patch-variables
  assert-equal task [calculate-average-contiguous-crop-cluster-size] (8 / 3) "avg contiguous patch size (3, 2, 3)"

  set woodlands no-patches
  set crops patches with [not member? self woodlands]
  ask crops [set is-crop? TRUE]
  ask woodlands [set is-crop? FALSE]
  set rainfall-data (list 1)
  set-patch-variables
;; call the function we're testing
;; test some specific aspects of the results of running that function
  assert-equal task [calculate-average-contiguous-crop-cluster-size] 10 "avg contiguous patch size (10 patches, all crops)"

;; some variables are required to be set in order to run the function

  resize-world 0 3 0 3
  ;;   C C w w
  ;;   C C C C
  ;;   w C w C
  ;;   w C C C
  set woodlands (
    patch-set patch 0 2 patch 0 3 patch 2 0 patch 2 2 patch 3 0)  ;; one contiguous patch of 11 crops
  set crops patches with [not member? self woodlands]
  ask crops [set is-crop? TRUE]
  ask woodlands [set is-crop? FALSE]
  set rainfall-data (list 1)
  set-patch-variables
;; call the function we're testing
;; test some specific aspects of the results of running that function
  assert-equal task [calculate-average-contiguous-crop-cluster-size] 11 "avg contiguous patch size (11 patches in a loop)"

;; some variables are required to be set in order to run the function
  resize-world 0 4 0 4
  ;;   C C w w w
  ;;   C C w C C
  ;;   w w w w w
  ;;   w C C C C
  ;;   C C C C C
  set woodlands (
    patch-set patch 0 2 patch 0 3 patch 0 4 patch 1 2 patch 3 0
    patch 2 0 patch 2 1 patch 2 2 patch 2 3 patch 2 4)  ;; 3 patches, size (2, 4, 9)
  set crops patches with [not member? self woodlands]
  ask crops [set is-crop? TRUE]
  ask woodlands [set is-crop? FALSE]
  set rainfall-data (list 1)
  set-patch-variables
;; call the function we're testing
;; test some specific aspects of the results of running that function
  assert-equal task [calculate-average-contiguous-crop-cluster-size] 5 "avg contiguous patch size (2, 4, 9)"

;; display the number of tests run during this test function
  let tests-here number-of-tests-run - start-test-number
  output-print (word "Finished " test-name " (" tests-here " tests)")
end



; ------------------------------ END OF CODE ------------------------------
@#$#@#$#@
GRAPHICS-WINDOW
388
10
857
500
-1
-1
9.0
1
10
1
1
1
0
0
0
1
0
50
0
50
1
1
0
ticks
30.0

BUTTON
4
15
74
48
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
196
185
370
218
proportion-crops
proportion-crops
1
99
12
1
1
%
HORIZONTAL

PLOT
880
72
1040
192
Total Harvest
Time
Tons
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if (burn-in = false) [ plot total-harvest ]"

PLOT
1055
8
1233
129
Total Livestock
Time
Cows
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if (burn-in = false) [plot count cows]"
"pen-1" 1.0 0 -7500403 true "" "if (burn-in = false) [plot minimum-cows-for-sustainability]"

PLOT
1055
175
1235
296
Woodland Availability
Time
Tons
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if (burn-in = false) [plot (sum [standing-available-biomass] of woodlands) / 1000 ]"
"pen-1" 1.0 0 -7500403 true "" "if (burn-in = false) [plot (minimum-woodland-for-sustainability) / 1000 ]"

BUTTON
80
15
173
48
go forever
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
1117
128
1173
173
Maximum
max-livestock-number
17
1
11

MONITOR
1055
128
1107
173
Minimum
min-livestock-number
17
1
11

MONITOR
1155
295
1235
340
Maximum
max-woodland-mass ;; metric tons/ha
2
1
11

MONITOR
1055
295
1134
340
Minimum
min-woodland-mass ;; metric tons/ha
2
1
11

PLOT
1054
339
1235
460
Rainfall
Time
Rainfall
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if (burn-in = false) [plot rainfall]"
"Mean rainfall" 1.0 0 -7500403 true "" "if (burn-in = false) [plot mean (rainfall-data) ]"
"Subsidy threshold" 1.0 0 -2674135 true "" "if (burn-in = false) [plot 400 ]"

SLIDER
200
113
374
146
clumpiness
clumpiness
0
1
0
0.1
1
NIL
HORIZONTAL

PLOT
882
208
1042
328
Harvest Eaten
Time
%
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if (burn-in = false) [plot percentage-harvest-eaten]"

CHOOSER
3
290
173
335
key-resources
key-resources
0 10 25
1

PLOT
881
340
1041
460
Subsidy
Time
$
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if (burn-in = false) [plot subsidy-used ]"

MONITOR
1183
128
1233
173
Died
dead-cows
0
1
11

MONITOR
881
8
960
53
Calendar Year
calendar-year
1
1
11

TEXTBOX
204
80
367
110
How much should crops be grouped together?
12
0.0
1

TEXTBOX
200
152
366
183
What proportion of land should be crop/arable?
12
0.0
1

TEXTBOX
4
338
176
400
How often do farmers move cows to better grazing?  (0 means never)
12
0.0
1

TEXTBOX
200
222
376
286
Should farmers supplement feed or transport their cows out of the village?
12
0.0
1

TEXTBOX
190
317
396
363
If so, what portion of the cows should they supplement/move?
12
0.0
1

TEXTBOX
1
81
198
129
Should rainfall be constant, random, historical, or extreme?
12
0.0
1

TEXTBOX
5
163
177
209
Which place should we use rainfall records from?
12
0.0
1

TEXTBOX
5
242
172
292
What percentage of grazing land grows very quickly (\"Key Resources\")?
12
0.0
1

SWITCH
195
419
372
452
invincible-fences
invincible-fences
0
1
-1000

TEXTBOX
199
388
349
427
What happens if we use stone walls?
12
0.0
1

CHOOSER
2
193
174
238
rain-site
rain-site
"Average" "Chivi" "Mberengwa" "Zvishavane"
0

CHOOSER
199
269
371
314
subsidy
subsidy
"no" "feed" "transport"
2

MONITOR
966
8
1043
53
NIL
years-gone
17
1
11

CHOOSER
199
10
370
55
model-mode
model-mode
"Demonstration" "Experiment" "SoftwareTests"
1

CHOOSER
2
385
182
430
times-per-day-farmers-move-cows
times-per-day-farmers-move-cows
0 1 2 3
1

TEXTBOX
7
55
352
81
Questions to ask about our system
19
102.0
0

CHOOSER
2
115
178
160
rainfall-type
rainfall-type
"constant" "random" "extreme" "historical" "statistical-random" "statistical-extreme"
0

TEXTBOX
4
439
170
485
What percentage of crops have Muonde projects, which grow more quickly?
12
0.0
1

CHOOSER
3
485
164
530
muonde-projects
muonde-projects
0 10 20
1

CHOOSER
195
486
373
531
how-long-to-store-grain
how-long-to-store-grain
0 1 2 3 4
3

SWITCH
689
505
875
538
use-muonde-thresholds
use-muonde-thresholds
1
1
-1000

TEXTBOX
197
453
370
482
How many years can we store harvest?\n
12
0.0
1

TEXTBOX
384
506
693
541
Should we use Muonde's rules for how much harvest, livestock, and woodland is sustainable?
12
0.0
1

SLIDER
198
351
373
384
cow-proportion-to-save
cow-proportion-to-save
0.0
1.0
0.7
0.1
1
NIL
HORIZONTAL

OUTPUT
877
461
1236
542
12

@#$#@#$#@
## SEE OVERVIEW, DESIGN, AND DETAILS DOCUMENT FOR MORE INFORMATION

This model is available at (***link***) on CoMSES.net, along with an Overview, Design, and Details document which gives a complete description of how the model works and all the data underlying its calibration.  The description below is just enough to get you started.

## WHAT IS IT?

This model has been created with and for the researcher-farmers of the Muonde Trust, a registered Zimbabwean non-governmental organization dedicated to fostering indigenous innovation in Mazvihwa Communal Area.  Model behaviors and parameters (*mashandiro nemisiyano nedzimwe model*) derive from a combination of literature review and the collected datasets from Muonde’s long-term (over 30 years) community-based research.  The goals of this model are three-fold (*muzvikamu zvitatu*):

A) To represent three components of a Zimbabwean agro-pastoral system (crops, woodland grazing area, and livestock) along with their key interactions and feedbacks and some of the human management decisions that may affect these components and their interactions.
We model the following feedbacks:

* Cows (*mombe*) eat woodland biomass to survive.

* Cows try to get into crop fields and eat those as well, if they can.

* Cows are required to plough crop fields (*kurima*) so that the fields’ biomass can increase and be harvested eventually.

* Woodland biomass (*matemwa/masanzu*) is used to make fences to keep cows out of crops.

* Farmers make a variety of decisions affecting the interactions between and sustainability of these three components.

B) To assess how climate variation (implemented in several different ways) and human management may affect the sustainability of the system as measured by the continued provisioning of crops, livestock, and woodland grazing area.

C) To provide a discussion tool for the community and local leaders to explore different management strategies for the agro-pastoral system (*hwaro / nzira yekudyidzana kwavanhu, zvipfuo nezvirimwa*), particularly in the face of climate change.

## HOW IT WORKS

* Rainfall (which can change from year to year in different ways based on the model chosen by the user) determines the growth rates of crops and woodlands.

* In a bad (low-rainfall) year, farmers can choose to subsidize cows by feeding them extra food or by transporting them to another village to graze.

* Farmers dictate how much land is allocated to crop production and how 'clumped' the crops are (scattered throughout the area or all together in one place).

* Farmers use woodland biomass to make fences around crops which decay over time due to termite activity.

* Cows try to move to a nearby patch if it has more biomass, succeeding in getting through a fence depending on how hungry they are and how intact the fence is.  They eat both woodland biomass and crop biomass depending on what they can find.

* Cows get hungrier via a basic maintenance energy from moving and metabolizing.  Cows also reproduce by splitting into two cows: a calf and an adult cow.  Calves have a lower minimum weight and can't be used to plough or reproduce.

* Cows die when their mass gets too low; they also can't be used to plough or reproduce when their mass is too low.

* Farmers can move cows to better woodland patches.

* Farmers can improve the quantity of patches that grow faster: in woodland, increasing key resources through forest conservation projects, and in crops through The Muonde Trust's water harvesting and native grains projects.

* Farmers can choose to build stone walls which never decay due to termites.

* Every year crops are harvested and cows will be needed to plough them so they can grow again.

## HOW TO USE IT

Begin by reading the questions above each switch and slider in the Interface tab and select answers that are of interest to you and then click 'setup' followed by 'go' -- the model should terminate on its own once it has run for 60 years or if one of the sustainability criteria is not met.  More detail on the Interface options is given in the Overview, Design, and Details document and in comments in the Code tab. In particular, there is more detail the different rainfall climate variation models.  No input files are required to run the model.

If you are interested in making changes to the code, make sure to change model-mode to "SoftwareTests" and click 'setup' in order to make sure you haven't broken one of the existing functions or introduced unintended behavior into the model. The SoftwareTesting functions give feedback in the output area on how a test was failed and where to look in the testing code to figure out what happened.  "Experiment" mode is recommended for running experiments using NetLogo's BehaviorSpace tool. "Demonstration" mode is recommended for showing the model to others.  Use the Profiling tool by clicking the button in order to find out which functions take the most time to run, especially if you make changes to the code.

## THINGS TO NOTICE

* Woodlands tend to track available rainfall quite closely, while cows respond more slowly.

* Cows tend to keep the woodland grazed to a fairly low biomass level; if you increase rainfall or key resources, unless the cow population has just crashed previously, the cows adapt to greater food by increasing in population to the new carrying capacity rather than the woodland being able to grow back.

* Cows sometimes collect at the edge of the crop fields because the highest-biomass patches are on the other side of a fence or stone wall.

## THINGS TO TRY

Every combination of management choices is available to the user via the switches and sliders.  Try anything!  See if you can use a combination of management practices to keep the model running for all 60 years, especially under extreme climate variation via the 'extreme' or 'statistical-extreme' rainfall models.  See what happens if you limit yourself to only one or two management choices (for example, stone walls or feed subsidy but not both).  Are there particular choices that make it difficult to maintain all three resources (cows, woodland, and crops)?

## EXTENDING THE MODEL

1) This model would be more accurate and potentially more useful if it included within-year variation in both rainfall and cow access to patches.  In the real system in Mazvihwa, rainfall comes very unpredictably and climate change may cause this within-year unpredictableness to increase.  In addition, farmers are allowed to graze their cattle on the remainders of the crops after harvest (residues or stover), so there is an additional source of food for the livestock during the off-harvest season.

2) Another thing that isn't represented is the tradeoffs a farmer must make between all of the management strategies.  If there were a constraint on how many of these the farmer could choose (for example, you can feed-subsidize your cows but then you can't afford to make stone walls), that might represent reality more closely or might make the model a better discussion tool.

3) There are many other benefits of woodlands to farmers in addition to providing grazing land and forage for livestock and fencing material for crops.  We have not modeled these, but the model could extend to include these, especially if point 2) were developed.

4) It would be interesting to allow farmers to change the land-use or other management strategies interactively throughout the model run.  For example, increase or decrease the crop proportion or fragmentation, or switch from feed subsidy to transport subsidy halfway through.

## NETLOGO FEATURES

* We created some functions for calculating Moran's I and Geary's C.

* We created functions required for doing software unit tests ("assert", "assert-equal", "assert-float-equal"), which is a much more systematic way of testing the basic functions of your ABM than Railsback and Grimm suggest in their ABM textbook (2012); unit tests are also important software development best practices.

## RELATED MODELS

Kulayijana models a similar system in Zimbabwe, with the addition of wildlife interactions with crops and livestock.  It is designed to be played as a game and models the decisions of the farmers at a finer scale than our model, and its parameters are not calibrated to real units. It does, however, share many of the same underlying ecology and management questions as well as the principle of community-based or companion modelling.
https://www.comses.net/codebases/5221/releases/1.0.0/
https://www.ecologyandsociety.org/vol22/iss1/art35/

## CREDITS AND REFERENCES

(These are the references from the ODD document; many but not all of them are referred to in the Code tab as well.)

Astatke, A, Reed JD, and Butterworth MH. (1986) “Effect of diet restriction on work performance and weight loss of local zebu and Friesian x Boran crossbred oxen.” ILCA Bulletin 23: 11-14.

Barnes, D. L. (1972) “Defoliation effects on perennial grasses - continuing confusion.” Proc. Grassld. Soc. Sth. Afr. 7:138-145.

Bourliere, F. & Hadley, M. (1970) “The ecology of tropical savannas”. Ann. Rev. Ecol. Syst. 1:125-152

Dambe, L. M., Mogotsi, K., Odubeng, M., & Kgosikoma, O. E. (2015) “Nutritive value of some important indigenous livestock browse species in semi-arid mixed Mopane bushveld, Botswana.” Livestock Research for Rural Development. Article, 209(27), 10.

Eastridge, M. (2007) “Feeding Corn Stover to Ruminants.” Buckeye Dairy News: Volume 9 Issue 3. Available: https://dairy.osu.edu/newsletter/buckeye-dairy-news/volume-9-issue-3/full (Accessed February 14, 2018)

Food and Agriculture Organization of the United Nations. (1992) “Maize in human nutrition.” (FAO Food and Nutrition Series, No. 25) ISBN 92-5-103013-8 Available: http://www.fao.org/docrep/t0395e/T0395E01.htm (Accessed January 29, 2018)

Food and Agriculture Organization of the United Nations. (2003) “Food energy – methods of analysis and conversion factors.” Fao Food And Nutrition Paper 77. Available: http://www.fao.org/tempref/docrep/fao/006/y5022e/y5022e00.pdf (Accessed February 3, 2018)

Geary, R. C. (1954). “The contiguity ratio and statistical mapping.” The incorporated statistician, 5(3), 115-146.

Johnson, I. R., France, J., Thornley, J. H. M., Bell, M. J., & Eckard, R. J. (2012) “A generic model of growth, energy metabolism, and body composition for cattle and sheep.” Journal of animal science, 90(13), 4741-4751.

Jury, M.R. (2013) “Climate trends in southern Africa.” South African Journal of Science, 109(1-2), pp.1-11.

Kelly, R.D. (1973) “A comparative study of primary productivity under different kinds of land use in southeastern Rhodesia”, PhD Thesis, University of London.

Knapp, R. (1965) “Pfianzenarten - Zusammensetzung, Entwicklung und natiirliche Produktivitiit der Weide-Vegetation in Trockengebieten in verschiedenen Klima-Bereichen der Erde.” In: Knapp, R. (ed.), Weide-Wirtschaft in Trockengebieten. pp. 71-97. Gustav Fischer Verlag, Stuttgart.

Makhado, r. A., Potgieter, m. J., & luus-powell, w. J. (2016) “Nutritional value of colophospermum mopane as source of browse and its chemical defences against browsers: a review.” Journal of animal and plant sciences, 26(3), 569-576.

Machila, N., Fèvre, E.M., Maudlin, I. and Eisler, M.C. (2008) “Farmer estimation of live bodyweight of cattle: Implications for veterinary drug dosing in East Africa.” Preventive Veterinary Medicine, 87(3), pp.394-403.

McGarigal, K., SA Cushman, and E Ene. (2012) “FRAGSTATS v4: Spatial Pattern Analysis Program for Categorical and Continuous Maps.” Computer software program produced by the authors at the University of Massachusetts, Amherst. Available at the following web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html

Molden, D. ed. (2013) “Water for food, water for life: a comprehensive assessment of water management in agriculture.” Routledge.

Moran, P. A. (1950). “Notes on continuous stochastic phenomena.” Biometrika, 37(1/2), 17-23.

Nicholson M J. (1983) “Calf growth, milk offtake and estimated lactation yields of Borana cattle in the southern rangelands of Ethiopia.” Joint Ethiopian Pastoral Systems Study, Research Report 6.

Pandey, C.B. and Singh J.S. (1992) “Rainfall and grazing effects on net primary productivity in a tropical savannah, India” Ecology, 73(6), pp. 2007-2021

Railsback, S.F. and Grimm, V. (2012) Agent-based and individual-based modeling: a practical introduction. Princeton university press.

Rutherford, M.C. (1978) “Primary production ecology in Southern Africa”, in M. J. A. Werger (ed.), Biogeography and Ecology of Southern Africa Dr W. Junk, The Hague, p638

Shongwe, M.E., Van Oldenborgh, G.J., Van Den Hurk, B.J.J.M., De Boer, B., Coelho, C.A.S. and Van Aalst, M.K., (2009) “Projected changes in mean and extreme precipitation in Africa under global warming.” Part I: Southern Africa. Journal of climate, 22(13), pp.3819-3837.

Scoones, I.C. “Patch use by cattle in dryland Zimbabwe: farmer knowledge and ecological theory” Overseas Development Institute, Paper 28b, 1989.

World Bank Group. Climate Change Knowledge Portal.  Available: http://sdwebx.worldbank.org/climateportal/index.cfm?page=country_future_climate_down&ThisRegion=Africa&ThisCcode=ZWE (Accessed Feb 3, 2018)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.2.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="RunSoftwareTests" repetitions="1" runMetricsEveryStep="false">
    <go>setup</go>
    <metric>number-of-tests-run</metric>
    <metric>error-count</metric>
    <enumeratedValueSet variable="how-long-to-store-grain">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy">
      <value value="&quot;transport&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-mode">
      <value value="&quot;SoftwareTests&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-proportion-to-save">
      <value value="0.45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-muonde-thresholds">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-site">
      <value value="&quot;Average&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rainfall-type">
      <value value="&quot;statistical-extreme&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="key-resources">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="muonde-projects">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="invincible-fences">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proportion-crops">
      <value value="33"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="times-per-day-farmers-move-cows">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clumpiness">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="MT_nosub" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>total-mud-crop-perimeter</metric>
    <metric>wood-to-build-fence-per-meter</metric>
    <metric>termite-activity</metric>
    <metric>hours-to-plough-ha</metric>
    <metric>crop-growth-slope</metric>
    <metric>zero-crop-growth-intercept</metric>
    <metric>muonde-efficiency</metric>
    <metric>woodland-growth-slope</metric>
    <metric>cow-maintenance-energy-rate</metric>
    <metric>cow-working-energy-per-hour</metric>
    <metric>kcal-per-kg-of-browse</metric>
    <metric>kcal-per-kg-of-crop</metric>
    <metric>kcal-per-kg-of-cow</metric>
    <metric>production-efficiency</metric>
    <metric>catabolism-efficiency</metric>
    <metric>min-cow-mass</metric>
    <metric>max-cow-mass</metric>
    <metric>calf-birth-mass</metric>
    <metric>livestock-not-reproduction-rate-per-year</metric>
    <metric>proportion-crops</metric>
    <metric>years-gone</metric>
    <metric>morans-i</metric>
    <metric>gearys-c</metric>
    <metric>total-crop-perimeter</metric>
    <metric>average-contiguous-crop-cluster-size</metric>
    <metric>max-percentage-harvest-eaten</metric>
    <metric>crop-eaten-per-cow-per-half-hour</metric>
    <metric>subsidy-used</metric>
    <metric>total-harvest</metric>
    <metric>mean previous-harvests-list</metric>
    <metric>max-livestock-number</metric>
    <metric>min-livestock-number</metric>
    <metric>actual-reproductive-rate</metric>
    <metric>max-woodland-mass</metric>
    <metric>min-woodland-mass</metric>
    <metric>termination-reason</metric>
    <metric>model-setup-time</metric>
    <metric>model-run-time</metric>
    <enumeratedValueSet variable="model-mode">
      <value value="&quot;Experiment&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="times-per-day-farmers-move-cows">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="invincible-fences">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="key-resources">
      <value value="0"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy">
      <value value="&quot;no&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-proportion-to-save">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rainfall-type">
      <value value="&quot;constant&quot;"/>
      <value value="&quot;random&quot;"/>
      <value value="&quot;extreme&quot;"/>
      <value value="&quot;historical&quot;"/>
      <value value="&quot;statistical-random&quot;"/>
      <value value="&quot;statistical-extreme&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="muonde-projects">
      <value value="0"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-site">
      <value value="&quot;Average&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="how-long-to-store-grain">
      <value value="0"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-muonde-thresholds">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clumpiness">
      <value value="0"/>
      <value value="0.07"/>
      <value value="0.22"/>
      <value value="0.54"/>
      <value value="0.85"/>
      <value value="0.95"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="MT_sub" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>total-mud-crop-perimeter</metric>
    <metric>wood-to-build-fence-per-meter</metric>
    <metric>termite-activity</metric>
    <metric>hours-to-plough-ha</metric>
    <metric>crop-growth-slope</metric>
    <metric>zero-crop-growth-intercept</metric>
    <metric>muonde-efficiency</metric>
    <metric>woodland-growth-slope</metric>
    <metric>cow-maintenance-energy-rate</metric>
    <metric>cow-working-energy-per-hour</metric>
    <metric>kcal-per-kg-of-browse</metric>
    <metric>kcal-per-kg-of-crop</metric>
    <metric>kcal-per-kg-of-cow</metric>
    <metric>production-efficiency</metric>
    <metric>catabolism-efficiency</metric>
    <metric>min-cow-mass</metric>
    <metric>max-cow-mass</metric>
    <metric>calf-birth-mass</metric>
    <metric>livestock-not-reproduction-rate-per-year</metric>
    <metric>proportion-crops</metric>
    <metric>years-gone</metric>
    <metric>morans-i</metric>
    <metric>gearys-c</metric>
    <metric>total-crop-perimeter</metric>
    <metric>average-contiguous-crop-cluster-size</metric>
    <metric>max-percentage-harvest-eaten</metric>
    <metric>crop-eaten-per-cow-per-half-hour</metric>
    <metric>subsidy-used</metric>
    <metric>total-harvest</metric>
    <metric>mean previous-harvests-list</metric>
    <metric>max-livestock-number</metric>
    <metric>min-livestock-number</metric>
    <metric>actual-reproductive-rate</metric>
    <metric>max-woodland-mass</metric>
    <metric>min-woodland-mass</metric>
    <metric>termination-reason</metric>
    <metric>model-setup-time</metric>
    <metric>model-run-time</metric>
    <enumeratedValueSet variable="model-mode">
      <value value="&quot;Experiment&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="times-per-day-farmers-move-cows">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="invincible-fences">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="key-resources">
      <value value="0"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy">
      <value value="&quot;feed&quot;"/>
      <value value="&quot;transport&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-proportion-to-save">
      <value value="0.7"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rainfall-type">
      <value value="&quot;random&quot;"/>
      <value value="&quot;extreme&quot;"/>
      <value value="&quot;historical&quot;"/>
      <value value="&quot;statistical-random&quot;"/>
      <value value="&quot;statistical-extreme&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="muonde-projects">
      <value value="0"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-site">
      <value value="&quot;Average&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="how-long-to-store-grain">
      <value value="0"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-muonde-thresholds">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clumpiness">
      <value value="0"/>
      <value value="0.07"/>
      <value value="0.22"/>
      <value value="0.54"/>
      <value value="0.85"/>
      <value value="0.95"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="OT_nosub_vary" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>total-mud-crop-perimeter</metric>
    <metric>wood-to-build-fence-per-meter</metric>
    <metric>termite-activity</metric>
    <metric>hours-to-plough-ha</metric>
    <metric>crop-growth-slope</metric>
    <metric>zero-crop-growth-intercept</metric>
    <metric>muonde-efficiency</metric>
    <metric>woodland-growth-slope</metric>
    <metric>cow-maintenance-energy-rate</metric>
    <metric>cow-working-energy-per-hour</metric>
    <metric>kcal-per-kg-of-browse</metric>
    <metric>kcal-per-kg-of-crop</metric>
    <metric>kcal-per-kg-of-cow</metric>
    <metric>production-efficiency</metric>
    <metric>catabolism-efficiency</metric>
    <metric>min-cow-mass</metric>
    <metric>max-cow-mass</metric>
    <metric>calf-birth-mass</metric>
    <metric>livestock-not-reproduction-rate-per-year</metric>
    <metric>proportion-crops</metric>
    <metric>years-gone</metric>
    <metric>morans-i</metric>
    <metric>gearys-c</metric>
    <metric>total-crop-perimeter</metric>
    <metric>average-contiguous-crop-cluster-size</metric>
    <metric>max-percentage-harvest-eaten</metric>
    <metric>crop-eaten-per-cow-per-half-hour</metric>
    <metric>subsidy-used</metric>
    <metric>total-harvest</metric>
    <metric>mean previous-harvests-list</metric>
    <metric>max-livestock-number</metric>
    <metric>min-livestock-number</metric>
    <metric>actual-reproductive-rate</metric>
    <metric>max-woodland-mass</metric>
    <metric>min-woodland-mass</metric>
    <metric>termination-reason</metric>
    <metric>model-setup-time</metric>
    <metric>model-run-time</metric>
    <enumeratedValueSet variable="model-mode">
      <value value="&quot;Experiment&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="times-per-day-farmers-move-cows">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="invincible-fences">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="key-resources">
      <value value="0"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy">
      <value value="&quot;no&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-proportion-to-save">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rainfall-type">
      <value value="&quot;random&quot;"/>
      <value value="&quot;extreme&quot;"/>
      <value value="&quot;historical&quot;"/>
      <value value="&quot;statistical-random&quot;"/>
      <value value="&quot;statistical-extreme&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="muonde-projects">
      <value value="0"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-site">
      <value value="&quot;Average&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="how-long-to-store-grain">
      <value value="0"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-muonde-thresholds">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clumpiness">
      <value value="0"/>
      <value value="0.07"/>
      <value value="0.22"/>
      <value value="0.54"/>
      <value value="0.85"/>
      <value value="0.95"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="OT_nosub_const" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>total-mud-crop-perimeter</metric>
    <metric>wood-to-build-fence-per-meter</metric>
    <metric>termite-activity</metric>
    <metric>hours-to-plough-ha</metric>
    <metric>crop-growth-slope</metric>
    <metric>zero-crop-growth-intercept</metric>
    <metric>muonde-efficiency</metric>
    <metric>woodland-growth-slope</metric>
    <metric>cow-maintenance-energy-rate</metric>
    <metric>cow-working-energy-per-hour</metric>
    <metric>kcal-per-kg-of-browse</metric>
    <metric>kcal-per-kg-of-crop</metric>
    <metric>kcal-per-kg-of-cow</metric>
    <metric>production-efficiency</metric>
    <metric>catabolism-efficiency</metric>
    <metric>min-cow-mass</metric>
    <metric>max-cow-mass</metric>
    <metric>calf-birth-mass</metric>
    <metric>livestock-not-reproduction-rate-per-year</metric>
    <metric>proportion-crops</metric>
    <metric>years-gone</metric>
    <metric>morans-i</metric>
    <metric>gearys-c</metric>
    <metric>total-crop-perimeter</metric>
    <metric>average-contiguous-crop-cluster-size</metric>
    <metric>max-percentage-harvest-eaten</metric>
    <metric>crop-eaten-per-cow-per-half-hour</metric>
    <metric>subsidy-used</metric>
    <metric>total-harvest</metric>
    <metric>mean previous-harvests-list</metric>
    <metric>max-livestock-number</metric>
    <metric>min-livestock-number</metric>
    <metric>actual-reproductive-rate</metric>
    <metric>max-woodland-mass</metric>
    <metric>min-woodland-mass</metric>
    <metric>termination-reason</metric>
    <metric>model-setup-time</metric>
    <metric>model-run-time</metric>
    <enumeratedValueSet variable="model-mode">
      <value value="&quot;Experiment&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="times-per-day-farmers-move-cows">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="invincible-fences">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="key-resources">
      <value value="0"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy">
      <value value="&quot;no&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-proportion-to-save">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rainfall-type">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="muonde-projects">
      <value value="0"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-site">
      <value value="&quot;Average&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="how-long-to-store-grain">
      <value value="0"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-muonde-thresholds">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clumpiness">
      <value value="0"/>
      <value value="0.07"/>
      <value value="0.22"/>
      <value value="0.54"/>
      <value value="0.85"/>
      <value value="0.95"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="OT_feed_vary" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>total-mud-crop-perimeter</metric>
    <metric>wood-to-build-fence-per-meter</metric>
    <metric>termite-activity</metric>
    <metric>hours-to-plough-ha</metric>
    <metric>crop-growth-slope</metric>
    <metric>zero-crop-growth-intercept</metric>
    <metric>muonde-efficiency</metric>
    <metric>woodland-growth-slope</metric>
    <metric>cow-maintenance-energy-rate</metric>
    <metric>cow-working-energy-per-hour</metric>
    <metric>kcal-per-kg-of-browse</metric>
    <metric>kcal-per-kg-of-crop</metric>
    <metric>kcal-per-kg-of-cow</metric>
    <metric>production-efficiency</metric>
    <metric>catabolism-efficiency</metric>
    <metric>min-cow-mass</metric>
    <metric>max-cow-mass</metric>
    <metric>calf-birth-mass</metric>
    <metric>livestock-not-reproduction-rate-per-year</metric>
    <metric>proportion-crops</metric>
    <metric>years-gone</metric>
    <metric>morans-i</metric>
    <metric>gearys-c</metric>
    <metric>total-crop-perimeter</metric>
    <metric>average-contiguous-crop-cluster-size</metric>
    <metric>max-percentage-harvest-eaten</metric>
    <metric>crop-eaten-per-cow-per-half-hour</metric>
    <metric>subsidy-used</metric>
    <metric>total-harvest</metric>
    <metric>mean previous-harvests-list</metric>
    <metric>max-livestock-number</metric>
    <metric>min-livestock-number</metric>
    <metric>actual-reproductive-rate</metric>
    <metric>max-woodland-mass</metric>
    <metric>min-woodland-mass</metric>
    <metric>termination-reason</metric>
    <metric>model-setup-time</metric>
    <metric>model-run-time</metric>
    <enumeratedValueSet variable="model-mode">
      <value value="&quot;Experiment&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="times-per-day-farmers-move-cows">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="invincible-fences">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="key-resources">
      <value value="0"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy">
      <value value="&quot;feed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-proportion-to-save">
      <value value="0.7"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rainfall-type">
      <value value="&quot;random&quot;"/>
      <value value="&quot;historical&quot;"/>
      <value value="&quot;statistical-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="muonde-projects">
      <value value="0"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-site">
      <value value="&quot;Average&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="how-long-to-store-grain">
      <value value="0"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-muonde-thresholds">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clumpiness">
      <value value="0"/>
      <value value="0.07"/>
      <value value="0.22"/>
      <value value="0.54"/>
      <value value="0.85"/>
      <value value="0.95"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="OT_feed_vary_extr" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>total-mud-crop-perimeter</metric>
    <metric>wood-to-build-fence-per-meter</metric>
    <metric>termite-activity</metric>
    <metric>hours-to-plough-ha</metric>
    <metric>crop-growth-slope</metric>
    <metric>zero-crop-growth-intercept</metric>
    <metric>muonde-efficiency</metric>
    <metric>woodland-growth-slope</metric>
    <metric>cow-maintenance-energy-rate</metric>
    <metric>cow-working-energy-per-hour</metric>
    <metric>kcal-per-kg-of-browse</metric>
    <metric>kcal-per-kg-of-crop</metric>
    <metric>kcal-per-kg-of-cow</metric>
    <metric>production-efficiency</metric>
    <metric>catabolism-efficiency</metric>
    <metric>min-cow-mass</metric>
    <metric>max-cow-mass</metric>
    <metric>calf-birth-mass</metric>
    <metric>livestock-not-reproduction-rate-per-year</metric>
    <metric>proportion-crops</metric>
    <metric>years-gone</metric>
    <metric>morans-i</metric>
    <metric>gearys-c</metric>
    <metric>total-crop-perimeter</metric>
    <metric>average-contiguous-crop-cluster-size</metric>
    <metric>max-percentage-harvest-eaten</metric>
    <metric>crop-eaten-per-cow-per-half-hour</metric>
    <metric>subsidy-used</metric>
    <metric>total-harvest</metric>
    <metric>mean previous-harvests-list</metric>
    <metric>max-livestock-number</metric>
    <metric>min-livestock-number</metric>
    <metric>actual-reproductive-rate</metric>
    <metric>max-woodland-mass</metric>
    <metric>min-woodland-mass</metric>
    <metric>termination-reason</metric>
    <metric>model-setup-time</metric>
    <metric>model-run-time</metric>
    <enumeratedValueSet variable="model-mode">
      <value value="&quot;Experiment&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="times-per-day-farmers-move-cows">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="invincible-fences">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="key-resources">
      <value value="0"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy">
      <value value="&quot;feed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-proportion-to-save">
      <value value="0.7"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rainfall-type">
      <value value="&quot;extreme&quot;"/>
      <value value="&quot;statistical-extreme&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="muonde-projects">
      <value value="0"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-site">
      <value value="&quot;Average&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="how-long-to-store-grain">
      <value value="0"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-muonde-thresholds">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clumpiness">
      <value value="0"/>
      <value value="0.07"/>
      <value value="0.22"/>
      <value value="0.54"/>
      <value value="0.85"/>
      <value value="0.95"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="OT_tran_vary" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>total-mud-crop-perimeter</metric>
    <metric>wood-to-build-fence-per-meter</metric>
    <metric>termite-activity</metric>
    <metric>hours-to-plough-ha</metric>
    <metric>crop-growth-slope</metric>
    <metric>zero-crop-growth-intercept</metric>
    <metric>muonde-efficiency</metric>
    <metric>woodland-growth-slope</metric>
    <metric>cow-maintenance-energy-rate</metric>
    <metric>cow-working-energy-per-hour</metric>
    <metric>kcal-per-kg-of-browse</metric>
    <metric>kcal-per-kg-of-crop</metric>
    <metric>kcal-per-kg-of-cow</metric>
    <metric>production-efficiency</metric>
    <metric>catabolism-efficiency</metric>
    <metric>min-cow-mass</metric>
    <metric>max-cow-mass</metric>
    <metric>calf-birth-mass</metric>
    <metric>livestock-not-reproduction-rate-per-year</metric>
    <metric>proportion-crops</metric>
    <metric>years-gone</metric>
    <metric>morans-i</metric>
    <metric>gearys-c</metric>
    <metric>total-crop-perimeter</metric>
    <metric>average-contiguous-crop-cluster-size</metric>
    <metric>max-percentage-harvest-eaten</metric>
    <metric>crop-eaten-per-cow-per-half-hour</metric>
    <metric>subsidy-used</metric>
    <metric>total-harvest</metric>
    <metric>mean previous-harvests-list</metric>
    <metric>max-livestock-number</metric>
    <metric>min-livestock-number</metric>
    <metric>actual-reproductive-rate</metric>
    <metric>max-woodland-mass</metric>
    <metric>min-woodland-mass</metric>
    <metric>termination-reason</metric>
    <metric>model-setup-time</metric>
    <metric>model-run-time</metric>
    <enumeratedValueSet variable="model-mode">
      <value value="&quot;Experiment&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="times-per-day-farmers-move-cows">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="invincible-fences">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="key-resources">
      <value value="0"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy">
      <value value="&quot;transport&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-proportion-to-save">
      <value value="0.7"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rainfall-type">
      <value value="&quot;random&quot;"/>
      <value value="&quot;historical&quot;"/>
      <value value="&quot;statistical-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="muonde-projects">
      <value value="0"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-site">
      <value value="&quot;Average&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="how-long-to-store-grain">
      <value value="0"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-muonde-thresholds">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clumpiness">
      <value value="0"/>
      <value value="0.07"/>
      <value value="0.22"/>
      <value value="0.54"/>
      <value value="0.85"/>
      <value value="0.95"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="OT_tran_vary_extr" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>total-mud-crop-perimeter</metric>
    <metric>wood-to-build-fence-per-meter</metric>
    <metric>termite-activity</metric>
    <metric>hours-to-plough-ha</metric>
    <metric>crop-growth-slope</metric>
    <metric>zero-crop-growth-intercept</metric>
    <metric>muonde-efficiency</metric>
    <metric>woodland-growth-slope</metric>
    <metric>cow-maintenance-energy-rate</metric>
    <metric>cow-working-energy-per-hour</metric>
    <metric>kcal-per-kg-of-browse</metric>
    <metric>kcal-per-kg-of-crop</metric>
    <metric>kcal-per-kg-of-cow</metric>
    <metric>production-efficiency</metric>
    <metric>catabolism-efficiency</metric>
    <metric>min-cow-mass</metric>
    <metric>max-cow-mass</metric>
    <metric>calf-birth-mass</metric>
    <metric>livestock-not-reproduction-rate-per-year</metric>
    <metric>proportion-crops</metric>
    <metric>years-gone</metric>
    <metric>morans-i</metric>
    <metric>gearys-c</metric>
    <metric>total-crop-perimeter</metric>
    <metric>average-contiguous-crop-cluster-size</metric>
    <metric>max-percentage-harvest-eaten</metric>
    <metric>crop-eaten-per-cow-per-half-hour</metric>
    <metric>subsidy-used</metric>
    <metric>total-harvest</metric>
    <metric>mean previous-harvests-list</metric>
    <metric>max-livestock-number</metric>
    <metric>min-livestock-number</metric>
    <metric>actual-reproductive-rate</metric>
    <metric>max-woodland-mass</metric>
    <metric>min-woodland-mass</metric>
    <metric>termination-reason</metric>
    <metric>model-setup-time</metric>
    <metric>model-run-time</metric>
    <enumeratedValueSet variable="model-mode">
      <value value="&quot;Experiment&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="times-per-day-farmers-move-cows">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="invincible-fences">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="key-resources">
      <value value="0"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy">
      <value value="&quot;transport&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-proportion-to-save">
      <value value="0.7"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rainfall-type">
      <value value="&quot;extreme&quot;"/>
      <value value="&quot;statistical-extreme&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="muonde-projects">
      <value value="0"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-site">
      <value value="&quot;Average&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="how-long-to-store-grain">
      <value value="0"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-muonde-thresholds">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clumpiness">
      <value value="0"/>
      <value value="0.07"/>
      <value value="0.22"/>
      <value value="0.54"/>
      <value value="0.85"/>
      <value value="0.95"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ShortRunForTesting" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>total-mud-crop-perimeter</metric>
    <metric>wood-to-build-fence-per-meter</metric>
    <metric>termite-activity</metric>
    <metric>hours-to-plough-ha</metric>
    <metric>crop-growth-slope</metric>
    <metric>zero-crop-growth-intercept</metric>
    <metric>muonde-efficiency</metric>
    <metric>woodland-growth-slope</metric>
    <metric>cow-maintenance-energy-rate</metric>
    <metric>cow-working-energy-per-hour</metric>
    <metric>kcal-per-kg-of-browse</metric>
    <metric>kcal-per-kg-of-crop</metric>
    <metric>kcal-per-kg-of-cow</metric>
    <metric>production-efficiency</metric>
    <metric>catabolism-efficiency</metric>
    <metric>min-cow-mass</metric>
    <metric>max-cow-mass</metric>
    <metric>calf-birth-mass</metric>
    <metric>livestock-not-reproduction-rate-per-year</metric>
    <metric>proportion-crops</metric>
    <metric>years-gone</metric>
    <metric>morans-i</metric>
    <metric>gearys-c</metric>
    <metric>total-crop-perimeter</metric>
    <metric>average-contiguous-crop-cluster-size</metric>
    <metric>max-percentage-harvest-eaten</metric>
    <metric>crop-eaten-per-cow-per-half-hour</metric>
    <metric>subsidy-used</metric>
    <metric>total-harvest</metric>
    <metric>mean previous-harvests-list</metric>
    <metric>max-livestock-number</metric>
    <metric>min-livestock-number</metric>
    <metric>actual-reproductive-rate</metric>
    <metric>max-woodland-mass</metric>
    <metric>min-woodland-mass</metric>
    <metric>termination-reason</metric>
    <metric>model-setup-time</metric>
    <metric>model-run-time</metric>
    <enumeratedValueSet variable="model-mode">
      <value value="&quot;Experiment&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="times-per-day-farmers-move-cows">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="invincible-fences">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="key-resources">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy">
      <value value="&quot;transport&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-proportion-to-save">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rainfall-type">
      <value value="&quot;constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="muonde-projects">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-site">
      <value value="&quot;Average&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="how-long-to-store-grain">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-muonde-thresholds">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clumpiness">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
