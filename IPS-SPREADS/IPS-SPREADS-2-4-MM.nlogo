;...load extensions..................................................................................
extensions[GIS]

;...define global variables..........................................................................
globals[
  beetle_dead            ; # of beetles which died (total)
  beetle_killed          ; # of beetles died attacking a tree
  beetle_lost            ; # of beetles moved out of the world
  beetle_emerged         ; # of beetles emerged from source (total)
  beetle_infesting       ; # of beetle successfully infesting
  beetle_dispersing      ; # of beetles currently dispersing
  beetle_staying         ; # of beetle staying on trees
  beetle_starved         ; # of starved beetles
  beetle_locked          ; # of beetles locked through detention devices
  nhost                  ; # of trees infested
  nright                 ; # of trees infested in model and reality
  nshould                ; # of trees infested in reality

  beetle_percentsucc1    ; % of successful beetle (from total beetle emerged)
  max_flightdist_inf1    ; max dispersal distance (infester beetle distance)
  max_distance_host1     ; max infestation distance (infested trees)
  mean_flightdist_inf1   ; mean dispersal distance (infester beetle distance)
  mean_distance_host1    ; mean infestation distance (infested trees)
  max_flightdist_all1    ; mean flightdistance of beetles
  mean_flightdist_all1   ; max flightdistance of beetles
  mean_traveldist1       ; mean traveldistance (flight and wind driven) of beetles
  max_traveldist1        ; max traveldistance (flight and wind driven) of beetles
  mean_airline1          ; mean bark beetle bee line between their source and final destination
  max_airline1           ; max bark beetle bee line between their source and final destination
  nright1                ; # of correctly infested trees in generation 1 (if calibration data is provided)
  nhost1                 ; # of trees infested in generation 1
  prima1                 ; mean primattract of spruces after first generation
  ncut1                   ; # of trees infested that were removed after first beetle generation

  beetle_percentsucc2    ; % of successful beetle (from total beetle emerged)
  max_flightdist_inf2    ; max dispersal distance (infester beetle distance)
  max_distance_host2     ; max infestation distance (infested trees)
  mean_flightdist_inf2   ; mean dispersal distance (infester beetle distance)
  mean_distance_host2    ; mean infestation distance (infested trees)
  max_flightdist_all2    ; mean flightdistance of beetles
  mean_flightdist_all2   ; max flightdistance of beetles
  mean_traveldist2       ; mean traveldistance (flight and wind driven) of beetles
  max_traveldist2        ; max traveldistance (flight and wind driven) of beetles
  mean_airline2          ; mean bark beetle bee line between their source and final destination
  max_airline2           ; max bark beetle bee line between their source and final destination
  nright2                ; # of correctly infested trees in generation 2 (if calibration data is provided)
  nhost2                 ; # of trees infested in generation 2
  prima2                 ; mean primattract of spruces after second generation
  ncut2                   ; # of trees infested that were removed after first beetle generation

  beetle_percentsucc3    ; % of successful beetle (from total beetle emerged)
  max_flightdist_inf3    ; max dispersal distance (infester beetle distance)
  max_distance_host3     ; max infestation distance (infested trees)
  mean_flightdist_inf3   ; mean dispersal distance (infester beetle distance)
  mean_distance_host3    ; mean infestation distance (infested trees)
  max_flightdist_all3    ; mean flightdistance of beetles
  mean_flightdist_all3   ; max flightdistance of beetles
  mean_traveldist3       ; mean traveldistance (flight and wind driven) of beetles
  max_traveldist3        ; max traveldistance (flight and wind driven) of beetles
  mean_airline3          ; mean bark beetle bee line between their source and final destination
  max_airline3           ; max bark beetle bee line between their source and final destination
  nright3                ; # of correctly infested trees in generation 3 (if calibration data is provided)
  nhost3                 ; # of trees infested in generation 3
  prima3                 ; mean primattract of spruces after third generation
  ncut3                   ; # of trees infested that were removed after first beetle generation

  infest-dist            ; flight distance of all beetles infesting
  b_flightdist           ; list of flightdistance of beetles
  b_airline              ; list of bark beetles bee line between their source and final destination
  b_traveldist           ; list of traveldistance (flight and wind driven) of beetles
  maxpercept             ; max perception distance of beetles

  spruce                 ; all patches depicting a spruce tree
  host                   ; all infested spruce trees
  waiting-tree           ; all spruce trees with waiting beetles on them
  device                 ; all patches containing detention devices (e.g. a pheromonon trap): pcolor white, infestlev 1, primattract 0, secattract device-attract, nmin 0
  source                 ; all patches containing a beetle source

  data1                  ; loaded GIS data - stand shapes with data on proportion of spruces (shapefile)
  data2                  ; loaded GIS data - calculated PAS (raster)
  data3                  ; loaded GIS data - calculated delay of first flight wave (raster)
  data4                  ; loaded GIS data - location of infestations of given experiment (some time before, this year and the following year or month)
  data5                  ; loaded GIS data - calculated host capacity
  data6                  ; loaded GIS data - calculated source capacity
  data7                  ; loaded GIS data - calculated tree height
  room                   ; size of loaded GIS world

  day                    ; number of current day starting from April 1st
  min-delay              ; first day of beetle flight
  ngeneration            ; number of current generation dispersing

  export-raster          ; data to store as asc raster file
]

;...define properties of patches.....................................................................
patches-own [
  infestlev          ; infestation level of tree: 0 = non-infested, 1 = infested, 2 = fully occupied, 3 = non-susceptible and 4 = detention device
  primattract        ; primary attractiveness of tree
  secattract         ; secondary attractiveness of tree
  totalattract       ; total attractiveness of tree
  nmin               ; lower threshold: minimum # of beetles necessary for infestation, depending on primary attractiveness
  nmax               ; upper threshold: maximum # of beetles possible for infestation, depending on precalculated tree size
  height             ; tree height in Meter
  nstart             ; maximum # of beetles possible to start their dispersal flight, depending on precalculated tree size
  nstay              ; # of beetles staying at a tree
  ninf               ; # of beetles infesting a tree
  nlock              ; # of beetles locked through a detention device
  tree_infestdist    ; net distance of trees infested or fully occupied from nearest beetle source
  spruceprop         ; spruce proportion of stand the given patch belongs to
  delay              ; defines how long the first beetle flightwave is delayed (in ticks); calculated based on aspect, height and canopy closure within R beforehand
  wave-count         ; counts how many flight waves each beetle source produced so far
  inf                ; informaton if given patch has no history (0), is source (1), will be infested in the next period (2) or was infested some time before (3)
  infestday          ; day of infestation
  local-speed        ; calculated windspeed on the patch
  roughness          ; calculated roughness of the patch
  neighbor-height    ; mean height of neighboring trees
]

;... define breeds...................................................................................
breed[beetles beetle]
breed[volatiles volatile]

;...define properties of beetles.....................................................................
beetles-own[
  status             ; status of beetle: dispersing or staying
  energy             ; energy level
  t_dispers          ; time step counter during flight
  staytime           ; time which beetles with  "staying" remain at a tree
  efficiency         ; efficiency of using energy (=1/consumption)
  flightdist         ; flight distance from source to the actual point
  starttime          ; tick of start time
  driftdist          ; distance driven by wind
  traveldist         ; distance traveled over ground (flightdist + wind driven)
  airline            ; bee line between source and final destination
  origin             ; location of beetles source tree
]

;...define properties of volatiles...................................................................
volatiles-own[
  flightdist         ; distance the volatile flew during diffusion movement
  driftdist          ; distance traveled by wind
  traveldist         ; distance traveld over ground ( flightdist + driftdist)
  voattract          ; total attractiveness of the source tree at the time of release
  origin             ; source patch ID
]

;...general setup....................................................................................
to setup
  clear-all ; clear everything
  reset-ticks ; reset ticks

  set host no-patches ; clear agent-set host
  set waiting-tree no-patches ; clear agent-set waiting-tree
  set source no-patches ; clear agent-set source
  set spruce no-patches ; clear agent-set spruce
  set device no-patches ; clear agent-set device
  set b_airline (list) ; clear result list b_beeling
  set b_traveldist (list) ; clear result list b_traveldist
  set b_flightdist (list) ; clear result list b_flightdist
  set day 0 ; set day number to 0 (1st of April)
  set ngeneration 1 ; set generation counter to 1

  import-GIS-data ; load GIS data and set world size
  create-environment ; assign patch variables
  distribute-source-trees ; place beetle sources
  set spruce (patches with [primattract > 0]) ; add patches with spruces to agent set spruce
  draw-plots ; draw plots

end

;...running processes................................................................................
to go

  if day = 0 and ticks = 0 [ ; check if beginning of simulation
    set min-delay (min [delay] of source) ; calculate day number with first flight wave
    set day min-delay ; set day to day number with first flightwave of all the source patches
    repeat min-delay [draw-plots] ; update / draw plots accordingly
    ask source [
      if day >= [delay] of self [ ; ask sources if start day is reached
        spawn-beetles ; spawn beetles
      ]
    ]
  ]

  tick

  if ticks mod 200 = 0 [ ; start new day after 200 ticks
    set day day + 1 ; start another day
    ask source [
      if day >= [delay] of self [ ; ask sources if start day is reached
        spawn-beetles ; spawn beetles
      ]
    ]
    draw-plots ; update / draw plots accordingly
  ]

  if ((mean [wave-count] of source) = swarming) [ ; check if all sources deployed their last flightwave
    if (beetle_dispersing = 0) and (beetle_staying = 0)  [ ; check if any beetles are flying or staying
      cut-infested-trees ; sanitation felling taking place
      calculate-results ; calculate global results
      ifelse (ngeneration = generations) or (count host = 0) [ ; check if maximum number of generations is reached or if no trees were infested
        stop ; end simulation
      ][
        set day (day + 1)
        draw-plots
        set ngeneration ngeneration + 1 ; set generation counter to next step
        ask source [
          set source (source with [self != myself]) ; remove old sources from agent set source
        ]
        ask host [
          ifelse inf = 2 [
            set pcolor blue ; color host trees blue if they match real world infestation pattern
            ][
            set pcolor red ; color the remaining host trees red
          ]
          set wave-count 0 ; set wave count to 0
          set delay (day + infestday - (ngeneration - 2) * swarming) ; calculate delay based on day of infestation
          set primattract 0 ; set primary attractiveness to 0
          set secattract 0
          set totalattract 0
          set source (patch-set source self) ; add patch to agent set source
          set host (host with [self != myself]) ; remove current patch from agent set host
          set infestlev 3
        ]
      ]
    ]
  ]

  spawn-volatiles ; detention devices and host trees spawn volatiles

  move-volatiles ; volatiles drift with the wind and perform diffusion movement

  ask beetles [
    if (status = "dispersing")[
      move-beetles ; move all dispersing beetles
      if (([totalattract] of patch-here - energy) >= 0) [ ; check if energy <= attractiveness here
        try-to-infest ; try to infest current patch
      ]
    ]

    if (status = "staying")[
      set staytime (staytime + 1) ; update staying time of waiting beetles
      if (staytime >= 200) [ ; check if maximum staying time is reached
        set beetle_killed (beetle_killed + 1) ; add one to the killed counter
        set beetle_staying (beetle_staying - 1) ; remove one from the staying counter
        ask patch-here [
          set nstay (nstay - 1) ; update the staying counter of the current patch
        ]
        set b_airline  fput airline b_airline          ; reporting airline
        set b_traveldist  fput traveldist b_traveldist ; reporting traveldist
        set b_flightdist fput flightdist b_flightdist  ; reporting flightdist
        die ; kill the beetle
        ]
      ]
    ]

  ask host [
    set secattract ((0.1 * nstay)  + ninf) ; update secondary attractiveness according to Kautz et al. (2014)
    set totalattract (primattract + secattract) ; calculate total attractiveness
    if (ninf >= nmax) [ ; check if capacity of current patch is reached
      set totalattract 0 ; set total attractiveness to 0
      set infestlev 2 ; set infestlev to 2
    ]
  ]

  ask waiting-tree [
    tree-defense ; tree sided infestation procedure
  ]

end

;...load GIS data....................................................................................
to import-GIS-data

  carefully [
    set data2 (gis:load-dataset word path-to-input "PAS.asc") ; import primary attractiveness from raster file (asc)
  ][
    error "No PAS data found! Use PAS.asc for import from entered file path."
  ]
  resize-world 0 gis:width-of data2 0 gis:height-of data2 ; transform NetLogo world size
  gis:set-world-envelope-ds gis:envelope-of data2 ; set NetLogo world size
  gis:apply-raster data2 primattract ; assign primary attractiveness to each patch (calculated PAS in R beforehand)
  ifelse max-pxcor >= max-pycor [
    set-patch-size (705 / max-pxcor) ; set displayed patch size in dependency of the given GIS data for similar NetLogo world sizes
  ][
    set-patch-size (705 / max-pycor) ; set displayed patch size in dependency of the given GIS data for similar NetLogo world sizes
  ]

  carefully [
    set data3 (gis:load-dataset word path-to-input "firstwave.asc") ; import precalculated flight wave delay (asc)
  ][
    error "No first flightwave data found! Use 'firstwave.asc' as file name for import from entered file path."
  ]
  gis:apply-raster data3 delay ; assign start time (ticks) of first flight wave for each patch

  if source-data = TRUE [
    carefully [
      set data4 (gis:load-dataset word path-to-input "inf.asc") ; import infestation location (asc)
    ][
      error "No infestation data found! Use 'inf.asc' as file name for import from entered file path."
    ]
    gis:apply-raster data4 inf ; assign information on infestation time (1 = source, 2 = goal, 0 = some time before)
  ]

  carefully [
    set data5 gis:load-dataset word path-to-input "hostcapacity.asc" ; import host capacities
  ][
    error "No host capacity data found! Use 'hostcapacity.asc' as file name for import from entered file path."
  ]
  gis:apply-raster data5 nmax

  carefully [
    set data6 gis:load-dataset word path-to-input "sourcecapacity.asc" ; import source capacities
  ][
    error "No source capacity data found! Use 'sourcecapacity.asc' as file name for import from entered file path."
  ]
  gis:apply-raster data6 nstart

  carefully [
    set data7 gis:load-dataset word path-to-input "tree-height.asc" ; import tree height
  ][
    error "No tree height data found! Use 'tree-height.asc' as file name for import from entered file path."
  ]
  gis:apply-raster data7 height

end

;...create environment...............................................................................
to create-environment

  ask patches [
    ifelse (primattract <= 0) or (primattract >= 0) [ ; check for NaNs in data (patches outside given research area)
      set infest-dist [] ; create empty list for storing infestation distance values of successfully infesting beetles on the corresponding patch
      set nmin ((-21.25 * primattract) + 221.25) ; calculate infestation threshold according to Kautz et al. (2014)
      if nmin = 221.25 [
        set infestlev 3 ; not susceptible spruce tree (primattract = 0)
      ]
      set secattract 0 ; set secundary attractiveness to 0
      set totalattract (primattract + secattract) ; calculate total attractiveness
      ifelse primattract > 0 [
        set pcolor (78 - (primattract / 1.5)) ; green colors for patches (spruces)
      ][
        set pcolor black ; black color not suceptible patches
      ]
    ][; if no primattract is available (no data provided)
      set primattract 0 ; set primary attractiveness to 0
      set secattract 0 ; set secundary attractriveness to 0
      set totalattract 0 ; set total attractiveness to 0
      set pcolor black ; give NaN patches a black color
      set infestlev 3 ; set infestation level to not susceptible
    ]
  ]

  ; calculate canopy flow index and wind speed on each patch
  ask patches [
    set neighbor-height mean [height] of neighbors ; calculate mean height of neighboring patches for each patch
    loop [
      if roughness > 0 [stop] ; prevent negative canopy flow indexes (roughness)
      ifelse mean [spruceprop] of neighbors >= 0.5 [
        set roughness random-normal 2.74 1.29  ; canopy flow index a for spruce according to Cionco (1978)
      ][
        set roughness random-normal 2.68 0.66  ; canopy flow index a for oak according to Cionco (1978)
      ]
    ]
  ]
  ask patches [
    ifelse neighbor-height <= 6.3 [
      ; full wind speed if flight height of beetles (6.3 m according to Duelli (1986)) exceeds vegetation height
      set local-speed windspeed
    ][
      ; if vegetation height exceeds beetle flight height windspeed reduction due to height and canopy flow index
      ; according to Mursch-Radlgruber & Kovacic (1988) is applied
      set local-speed (windspeed * exp ((-1 * roughness) * (1 - (6.3 / neighbor-height))))
    ]
  ]

end

;...place beetle sources.............................................................................
to distribute-source-trees

  if source-data = TRUE [ ; data of infestation is used
    ask patches [
      ;if inf = 1 and primattract > 0 [
      if inf = 1  [
        set primattract 0 ; set primary attractiveness to 0
        set totalattract 0 ; set total attractiveness to 0
        set source (patch-set source self) ; add patch to agent set source
      ]
;      if inf = 2 and primattract > 0 [
;        set pcolor white
;      ]
      if inf = 3 [
        set pcolor black
        set infestlev 3
        set primattract 0
        set secattract 0
        set totalattract 0
      ]
    ]
  ]

  ask source [
    set pcolor brown ; give beetle sources a brown color
    set infestlev 3 ; set infestation level to not susceptible
  ]

end

;...drawing the plots................................................................................
to draw-plots

;  set-current-plot "beetle energy"
;  histogram [energy] of beetles
;
;  set-current-plot "tree attractiveness"
;  histogram [primattract] of spruce
;
;  set-current-plot "beetle status"
;  set-current-plot-pen "killed"
;  plot beetle_killed
;  set-current-plot-pen "staying"
;  plot beetle_staying
;  set-current-plot-pen "infesting"
;  plot beetle_infesting
;  set-current-plot-pen "dispersing"
;  plot beetle_dispersing
;  set-current-plot-pen "emerged"
;  plot beetle_emerged
;  set-current-plot-pen "starved"
;  plot beetle_starved
;  set-current-plot-pen "locked"
;  plot beetle_locked
;
;  set-current-plot "beetle success"
;  set-current-plot-pen "hnatdie"
;  ifelse beetle_emerged = 0 [
;    plot beetle_emerged
;  ][
;    let hnatdie (beetle_infesting / beetle_emerged * 100)
;    plot hnatdie
;  ]

end

;...setup of starting beetles........................................................................
to spawn-beetles

  ifelse (swarming = [wave-count] of self) [stop] [ ; check if maximum number of flight waves for given patch are reached
    sprout-beetles (nstart / swarming) [ ; create beetle number depending on tree size and wave amount
      set color red ; beetles are red
      set status "dispersing" ; set beetle status to dispersing
      set size 0.5 ; set beetle size to 0.5
      set starttime ticks ; set beetle starting time to current tick number
      set energy (random-normal meanenergy 2) ; give each beetle a random normal distributed energy with set mean
      set efficiency (random-exponential 20) ; give each beetle a random negative exponential distributed energy efficiency
      set t_dispers 0 ; set beetle dispersal time to 0
      set beetle_emerged (beetle_emerged + 1) ; add one to the beetle emerged counter
      set beetle_dispersing (beetle_dispersing + 1) ; add one to the beetle dispersing counter
      while [(energy < 3) or (energy > 30)] [ ; correct beetle energy, if it is too low or too high (according to Kautz et al., 2014)
        set energy (random-normal meanenergy 2)
      ]
      while [(efficiency < 1)  or (efficiency  > 100)] [ ; correct beetle efficiency, if it is too low or too high (according to Kautz et al., 2014)
        set efficiency (random-exponential 20)
      ]
      set origin patch-here ; store source location for later calculations
    ]
    set wave-count (wave-count + 1) ; add one to the flight wave counter of the given beetle source
  ]

end

;...Creation of Volatiles...........................................................................
to spawn-volatiles

  ask host [ ;volatiles are released by spruces
    if (infestlev = 1)[ ; only spruces capable of hosting beetles are required tu release volatiles
      sprout-volatiles 1 [ ; release of volatiles 3 to minimize calculationtime but also getting at least nearly a plume
        set voattract [totalattract] of patch-here ; setting level of attractiveness
        set origin  patch-here ; setting origin --> possible destination for beetles
        set size 0.3 ; setting size
        set color yellow ; setting color
        set shape "dot" ; setting shape
      ]
    ]
  ]

  ask device [
    sprout-volatiles 1 [
      set voattract [totalattract] of patch-here ; set level of attractiveness accordingly
      set origin  patch-here ; set source patch as origin
      set size 0.3 ; set size
      set color yellow ; set color
      set shape "dot" ; set shape
    ]
  ]
end

;...Drift and Dispersion of volatiles................................................................
to move-volatiles

  ask volatiles [
    carefully [
      let sp (local-speed * 0.6)
      let dir (winddirection + 180)
      set xcor (xcor + ((sin dir) * sp))
      set ycor (ycor + ((cos dir) * sp))
      set driftdist (driftdist + sp)
    ][
      die
    ]

    set heading (random-float 360)
    fd 0.03 ;
    set flightdist (flightdist + 0.03 * 5) ;
    set traveldist (flightdist + driftdist) ;

    if traveldist >= (diffusionrate / 5) [
      die
    ]
  ]

end

;...movement of beetles..............................................................................
to move-beetles

  carefully [
    let sp (local-speed * 0.6)
    let dir (winddirection + 180)
    set xcor (xcor + ((sin dir) * sp))
    set ycor (ycor + ((cos dir) * sp))

    set driftdist (driftdist + sp)
  ][
    set beetle_lost (beetle_lost + 1) ; add one to the lost counter
    set beetle_dispersing (beetle_dispersing - 1) ; remove one from the dispersing counter
    set b_airline fput airline b_airline          ; reporting airline
    set b_traveldist fput traveldist b_traveldist ; reporting traveldist
    set b_flightdist fput flightdist b_flightdist  ;reporting flightdist
    die ; kill the lost beetle
  ]

  ifelse perceptdist = 0 [ ; no perception distance (= random movement)
    rt random moveangle ; turn randomly to the right
    lt random moveangle ; turn randomly to the left
    ifelse can-move? 1 = false [
      set beetle_lost (beetle_lost + 1) ; add one to the lost counter
      set beetle_dispersing (beetle_dispersing - 1) ; remove one from the dispersing counter
      set b_airline fput airline b_airline          ; reporting airline
      set b_traveldist fput traveldist b_traveldist ; reporting traveldist
      set b_flightdist fput flightdist b_flightdist  ;reporting flightdist
      die ; kill the lost beetle
    ][
      fd 1 ; move 1 forward (equals 5 m)
      set t_dispers (t_dispers + 1) ; update dispersal time
      set energy (energy - (1 / efficiency)) ; update energy level
      set flightdist (flightdist + 1) ; update flight distance
      set traveldist (flightdist + driftdist) ; update travel distance
      set airline (distance origin) ; update airline
    ]
  ][
    ifelse random-float 1 < 0.1 [
      ifelse any? volatiles in-radius (perceptdist / 5) [
        let np volatiles in-radius (perceptdist / 5) ; detect all volatiles inside perception distance
        let bnp max-one-of np [voattract] ; detect most attractive volatile inside perception distance
        let ah [totalattract] of patch-here ; store total attractiveness of current patch for calculation
        let abnp [voattract] of bnp ; extract total attractiveness of most attractive volatile inside perception distance
        let flightdistnow distance [origin] of bnp ; calculate distance from former location
        ifelse abnp - ah > 2 [ ; decision for directed flight to most attractive tree (difference > 2 & 10 % probability)
          move-to [origin] of bnp ; fly to origin of most attractive volatile
          set energy (energy - (flightdistnow / efficiency)) ; update energy level
          set t_dispers (t_dispers + flightdistnow) ; update dispersal time
          set flightdist (flightdist + flightdistnow) ; update flight distance
          set traveldist (flightdist + driftdist) ; update travel distance
          set airline (distance origin) ; update bee line
        ][
          rt random moveangle ; turn to the right with a random angle inside given range
          lt random moveangle ; turn to the left with a random angle inside given range
          ifelse can-move? 1 = false [
            set beetle_lost (beetle_lost + 1) ; add one to the lost counter
            set beetle_dispersing (beetle_dispersing - 1) ; remove one from the dispersing counter
            set b_airline fput airline b_airline          ; reporting airline
            set b_traveldist fput traveldist b_traveldist ; reporting traveldist
            set b_flightdist fput flightdist b_flightdist  ;reporting flightdist
            die ; kill the lost beetle
          ][
            fd 1 ; move 1 forward (equals 5 m)
            set t_dispers (t_dispers + 1) ; update dispersal time
            set energy (energy - (1 / efficiency)) ; update energy level
            set flightdist (flightdist + 1) ; update flight distance
            set traveldist (flightdist + driftdist) ; update travel distance
            set airline (distance origin) ; update airline
          ]
        ]
      ][
        rt random moveangle ; turn to the right with a random angle inside given range
        lt random moveangle ; turn to the left with a random angle inside given range
        ifelse can-move? 1 = false [
          set beetle_lost (beetle_lost + 1) ; add one to the lost counter
          set beetle_dispersing (beetle_dispersing - 1) ; remove one from the dispersing counter
          set b_airline fput airline b_airline          ; reporting airline
          set b_traveldist fput traveldist b_traveldist ; reporting traveldist
          set b_flightdist fput flightdist b_flightdist  ;reporting flightdist
          die ; kill the lost beetle
        ][
          fd 1 ; move 1 forward (equals 5 m)
          set t_dispers (t_dispers + 1) ; update dispersal time
          set energy (energy - (1 / efficiency)) ; update energy level
          set flightdist (flightdist + 1) ; update flight distance
          set traveldist (flightdist + driftdist) ; update travel distance
          set airline (distance origin) ; update airline
        ]
      ]
    ][
      rt random moveangle ; turn to the right with a random angle inside given range
      lt random moveangle ; turn to the left with a random angle inside given range
      ifelse can-move? 1 = false [
        set beetle_lost (beetle_lost + 1) ; add one to the lost counter
        set beetle_dispersing (beetle_dispersing - 1) ; remove one from the dispersing counter
        set b_airline fput airline b_airline          ; reporting airline
        set b_traveldist fput traveldist b_traveldist ; reporting traveldist
        set b_flightdist fput flightdist b_flightdist  ;reporting flightdist
        die ; kill the lost beetle
      ][
        fd 1 ; move 1 forward (equals 5 m)
        set t_dispers (t_dispers + 1) ; update dispersal time
        set energy (energy - (1 / efficiency)) ; update energy level
        set flightdist (flightdist + 1) ; update flight distance
        set traveldist (flightdist + driftdist) ; update travel distance
        set airline (distance origin) ; update airline
      ]
    ]
  ]

  if (energy <= 0) [ ; check if energy <= 0 (starved)
    set beetle_starved (beetle_starved + 1) ; add one to starved counter
    set beetle_dispersing (beetle_dispersing - 1) ; remove one from dispersing counter
    set b_airline  fput airline b_airline          ; reporting airline
    set b_traveldist  fput traveldist b_traveldist ; reporting traveldist
    set b_flightdist fput flightdist b_flightdist  ; reporting flightdist
    die ; kill beetle
  ]

end

;...infestation of beetles...........................................................................
to try-to-infest

  if [ninf] of patch-here >= [nmax] of patch-here [stop] ; end sub-model if capacity of this patch is already reached

  if [infestlev] of patch-here = 0 [ ; check if patch is currently not infested
    set status "staying" ; set status to staying
    set beetle_dispersing (beetle_dispersing - 1) ; remove one from dispersing counter
    set beetle_staying (beetle_staying + 1) ; add one to staying counter
    set color orange ; set color to orange
    set staytime 0 ; set staytime to 0
    set waiting-tree (patch-set waiting-tree patch-here) ; add current patch to agent set waiting-tree
    ask patch-here [
      set nstay (nstay + 1) ; update counter for staying beetles of current patch
    ]
  ]

  if [infestlev] of patch-here = 1 [ ; check if current patch is infested and not a detention device
    set status "infesting" ; set status infesting
    set beetle_infesting (beetle_infesting + 1) ; add one to the infesting counter
    set beetle_dispersing (beetle_dispersing - 1) ; remove one from the dispersing counter
    let fdist [flightdist] of self ; extract flight distance of current beetle
    ask patch-here [
      set ninf (ninf + 1) ; update infestation counter of current patch
      set infest-dist lput fdist infest-dist ; add flight dist of current beetle to current patch's list
    ]
    set b_airline fput airline b_airline ; reporting bee line
    set b_traveldist fput traveldist b_traveldist ; reporting travel distance
    set b_flightdist fput flightdist b_flightdist ; reporting flight distance
    die ; kill the beetle
  ]

  if [infestlev] of patch-here = 4 [ ; check if the current patch is a detention device
    set beetle_locked (beetle_locked + 1) ; add one to the locked counter
    set beetle_dispersing (beetle_dispersing - 1) ; remove one from the dispersing counter
    set b_airline fput airline b_airline ; reporting bee line
    set b_traveldist fput traveldist b_traveldist ; reporting travel distance
    set b_flightdist fput flightdist b_flightdist ; reporting flight distance
    ask patch-here [
      set nlock nlock + 1 ; update the current patch's counter on locked beetles
    ]
    die ; kill the beetle
  ]

end

;...infestation of trees.............................................................................
to tree-defense

  if infestlev = 0 [ ; check if patch is susceptible and not infested
    if nstay >= nmin [ ; check if minimum threshold is reached / overcome
      set infestlev 1 ; set infestation level to 1 (infested)
      set host (patch-set host self) ; add patch to agent set host
      ask waiting-tree [
        set waiting-tree (waiting-tree with [self != myself]) ; remove patch from agent set waiting-tree
      ]
      set pcolor red ; set color red
      set ninf nstay ; update counter for infesting beetles on this patch
      set nstay 0 ; set counter for staying beetles to 0
      set infestday (day + 1 - min-delay) ; save number of days passed since first flight wave (1 = same day)
      ask beetles-here with [status = "staying"][ ; ask beetles staying on this patch
        set beetle_infesting (beetle_infesting + 1) ; add one to infesting counter
        set beetle_staying (beetle_staying - 1) ; remove one from staying counter
        let fdist [flightdist] of self ; extract flight distance of current turtle
        ask patch-here [
          set infest-dist fput fdist infest-dist ; store flight distance of current turtle in list of current patch
        ]
        set b_airline fput airline b_airline ; reporting bee line
        set b_traveldist fput traveldist b_traveldist ; reporting travel distance
        set b_flightdist fput flightdist b_flightdist ; reporting flight distance
        die ; kill beetle
      ]
    ]
    set secattract ((0.1 * nstay)  + ninf) ; calculate secondary attractiveness according to Kautz et al. (2014)
    set totalattract (primattract + secattract) ; calculate total attractiveness
    if nstay = 0 [ ; check if there are any waiting beetles left
      ask waiting-tree [
        set waiting-tree (waiting-tree with [self != myself]) ; remove patch from agent set waiting-tree
      ]
    ]
  ]

end

;...cut infested trees...............................................................................
to cut-infested-trees
  let sfn round ((count host) * sanitation-felling / 100) ; calculate number of trees to be cut
  ask n-of sfn host [
    set host (host with [self != myself]) ; remove felled trees from hosts
    set beetle_infesting (beetle_infesting - [ninf] of self) ; remove beetles in felled trees from infesting beetles
    set beetle_killed (beetle_killed + [ninf] of self) ; add beetles in felled trees to killed beetles
    set pcolor yellow
    set primattract 0
    set secattract 0
    set totalattract 0
    set infestlev 3 ; make cut trees "invisible" to beetles
  ]
end

;...calculations of globals..........................................................................
to calculate-results
  if ngeneration = 1 [
    set beetle_percentsucc1 ((beetle_infesting / beetle_emerged) * 100) ; calculate proportion of successful beetles

    ask host with [inf = 2] [
      set pcolor blue
    ]

    ifelse count host = 0 [ ; if no spruce trees were infested, set all following global results to 0
      set nright1 0 ; # of correctly infested tree
      set max_flightdist_inf1 0
      set mean_flightdist_inf1 0
      set max_distance_host1 0
      set mean_distance_host1 0
      set nhost1 0
    ][
      set nhost1 count host
      ask host [
        set tree_infestdist (distance min-one-of source [distance myself])
      ]
      set max_flightdist_inf1 precision (max infest-dist * 5) 2 ; calculate maximum flight distance of infesting beetles
      set mean_flightdist_inf1 precision (mean infest-dist * 5) 2 ; calculate mean flight distance of inftesting beetles
      set max_distance_host1 precision (max [tree_infestdist] of host * 5) 2 ; calculate maximum distance of infested trees to nearest beetle source
      set mean_distance_host1 precision (mean [tree_infestdist] of host * 5) 2 ; calculate mean distance of infested trees to nearest beetle source
    ]
    set max_traveldist1 (max b_traveldist * 5) ; calculate beetles max travel distance (wind driven and beetle flight)
    set mean_traveldist1 (mean b_traveldist * 5) ; calculate beetles mean traveldistance (wind driven and beetle flight)
    set max_airline1 (max b_airline * 5) ; calculate max bee line of all beetles
    set mean_airline1 (mean b_airline * 5) ; calculate mean bee line of all beetles
    set max_flightdist_all1 (max b_flightdist * 5) ; calculate max flight distance of all beetles
    set mean_flightdist_all1 (mean b_flightdist * 5) ; calculate mean flight distance of all beetles
    set nright1 count host with [inf = 2] ; # of correctly infested trees
    set prima1 (mean [primattract] of spruce)
    set ncut1 (count patches with [pcolor = yellow])
  ]
  if ngeneration = 2 [
    set beetle_percentsucc2 ((beetle_infesting / beetle_emerged) * 100) ; calculate proportion of successful beetles

    ask host with [inf = 2] [
      set pcolor blue
    ]

    ifelse count host = 0 [ ; if no spruce trees were infested, set all following global results to 0
      set nright2 0
      set max_flightdist_inf2 0
      set mean_flightdist_inf2 0
      set max_distance_host2 0
      set mean_distance_host2 0
      set nhost2 0
    ][
      set nhost2 count host
      ask host [
        set tree_infestdist (distance min-one-of source [distance myself])
      ]
      set max_flightdist_inf2 precision (max infest-dist * 5) 2 ; calculate maximum flight distance of infesting beetles
      set mean_flightdist_inf2 precision (mean infest-dist * 5) 2 ; calculate mean flight distance of inftesting beetles
      set max_distance_host2 precision (max [tree_infestdist] of host * 5) 2 ; calculate maximum distance of infested trees to nearest beetle source
      set mean_distance_host2 precision (mean [tree_infestdist] of host * 5) 2 ; calculate mean distance of infested trees to nearest beetle source
    ]
    set max_traveldist2 (max b_traveldist * 5) ; calculate beetles max travel distance (wind driven and beetle flight)
    set mean_traveldist2 (mean b_traveldist * 5) ; calculate beetles mean traveldistance (wind driven and beetle flight)
    set max_airline2 (max b_airline * 5) ; calculate max bee line of all beetles
    set mean_airline2 (mean b_airline * 5) ; calculate mean bee line of all beetles
    set max_flightdist_all2 (max b_flightdist * 5) ; calculate max flight distance of all beetles
    set mean_flightdist_all2 (mean b_flightdist * 5) ; calculate mean flight distance of all beetles
    set nright2 count host with [inf = 2] ; # of correctly infested trees
    set prima2 (mean [primattract] of spruce)
    set ncut2 ((count patches with [pcolor = yellow]) - ncut1)
  ]
  if ngeneration = 3 [
    set beetle_percentsucc3 ((beetle_infesting / beetle_emerged) * 100) ; calculate proportion of successful beetles
    ask host with [inf = 2] [
      set pcolor blue
    ]
    ifelse count host = 0 [ ; if no spruce trees were infested, set all following global results to 0
      set nright3 0
      set max_flightdist_inf3 0
      set mean_flightdist_inf3 0
      set max_distance_host3 0
      set mean_distance_host3 0
      set nhost3 0
    ][
      set nhost3 count host
      ask host [
        set tree_infestdist (distance min-one-of source [distance myself])
      ]
      set max_flightdist_inf3 precision (max infest-dist * 5) 2 ; calculate maximum flight distance of infesting beetles
      set mean_flightdist_inf3 precision (mean infest-dist * 5) 2 ; calculate mean flight distance of inftesting beetles
      set max_distance_host3 precision (max [tree_infestdist] of host * 5) 2 ; calculate maximum distance of infested trees to nearest beetle source
      set mean_distance_host3 precision (mean [tree_infestdist] of host * 5) 2 ; calculate mean distance of infested trees to nearest beetle source
    ]
    set max_traveldist3 (max b_traveldist * 5) ; calculate beetles max travel distance (wind driven and beetle flight)
    set mean_traveldist3 (mean b_traveldist * 5) ; calculate beetles mean traveldistance (wind driven and beetle flight)
    set max_airline3 (max b_airline * 5) ; calculate max bee line of all beetles
    set mean_airline3 (mean b_airline * 5) ; calculate mean bee line of all beetles
    set max_flightdist_all3 (max b_flightdist * 5) ; calculate max flight distance of all beetles
    set mean_flightdist_all3 (mean b_flightdist * 5) ; calculate mean flight distance of all beetles
    set nright3 count host with [inf = 2] ; # of correctly infested trees
    set prima3 (mean [primattract] of spruce)
    set ncut3 ((count patches with [pcolor = yellow]) - ncut2 - ncut1)
  ]
  if ngeneration = generations or count host = 0 [
    set nhost (nhost1 + nhost2 + nhost3) ; # infested in total
    ;set ncut (count patches with [pcolor = yellow])

    ; value to optimize (minimize) inside calibration
    set nright (nright1 + nright2 + nright3) ; # infested correctly
    set nshould count patches with [inf = 2 and pcolor != black]  ; # infested in reality

    ifelse path-to-output = "" [

    ][
      export-infestation-pattern
    ]
  ]

end

;...export result as raster..........................................................................
to export-infestation-pattern

;    ; calculate spruce proportion
;  let spr-prop ((count spruce with [pcolor != red and pcolor != blue and pcolor != white]) / count patches * 100)
;  ; count beetle sources
;  let nsource (count host)
;  ; calculate mean primattract
;  let mprim (mean [primattract] of spruce with [pcolor != red and pcolor != blue and pcolor != white])
;
;  ; write these condition together with parameter settings to external file
;  file-open (word path-to-output "conditions/results.txt")
;  file-type behaviorspace-run-number
;  file-write ngeneration
;  file-write sanitation-felling
;  file-write (word substring path-to-input 6 11)

;  file-write spr-prop
;  file-write nsource
;  file-type " "
;  file-print mprim
;  file-close
;
;  set export-raster gis:patch-dataset infestday

;  set export-raster gis:patch-dataset infestday
;
;  if length (word sanitation-felling) = 1 [
;    set folder (word substring path-to-input 6 11 "gen-" ngeneration "-m-" 0 sanitation-felling "/ID-")
;    if length (word behaviorspace-run-number) = 1 [
;      gis:store-dataset export-raster (word path-to-output folder 0 0 0 behaviorspace-run-number "-g-" ngeneration "-m-" 0 sanitation-felling ".asc")
;    ]
;    if length (word behaviorspace-run-number) = 2 [
;      gis:store-dataset export-raster (word path-to-output folder 0 0 behaviorspace-run-number "-g-" ngeneration "-m-" 0 sanitation-felling ".asc")
;    ]
;    if length (word behaviorspace-run-number) = 3 [
;      gis:store-dataset export-raster (word path-to-output folder 0 behaviorspace-run-number "-g-" ngeneration "-m-" 0 sanitation-felling ".asc")
;    ]
;    if length (word behaviorspace-run-number) = 4 [
;      gis:store-dataset export-raster (word path-to-output folder behaviorspace-run-number "-g-" ngeneration "-m-" 0 sanitation-felling ".asc")
;    ]
;  ]
;
;  if length (word sanitation-felling) = 2 [
;    set folder (word substring path-to-input 6 11 "gen-" ngeneration "-m-" sanitation-felling "/ID-")
;    if length (word behaviorspace-run-number) = 1 [
;      gis:store-dataset export-raster (word path-to-output folder 0 0 0 behaviorspace-run-number "-g-" ngeneration "-m-" sanitation-felling ".asc")
;    ]
;    if length (word behaviorspace-run-number) = 2 [
;      gis:store-dataset export-raster (word path-to-output folder 0 0 behaviorspace-run-number "-g-" ngeneration "-m-" sanitation-felling ".asc")
;    ]
;    if length (word behaviorspace-run-number) = 3 [
;      gis:store-dataset export-raster (word path-to-output folder 0 behaviorspace-run-number "-g-" ngeneration "-m-" sanitation-felling ".asc")
;    ]
;    if length (word behaviorspace-run-number) = 4 [
;      gis:store-dataset export-raster (word path-to-output folder behaviorspace-run-number "-g-" ngeneration "-m-" sanitation-felling ".asc")
;    ]
;  ]

end

;...deploy detention devices.........................................................................
to deploy-detention-devices

  if mouse-down? [ ; check if left click is performed
    ask patch mouse-xcor mouse-ycor [ ; ask patch where mouce is at
      set infestlev 4 ; set infestlevel to 4 (detention device)
      set pcolor grey ; set patch color to grey
      set primattract 0 ; set primary attractiveness to 0
      set secattract 15 ; set secondary attractiveness to predefined pheromone dispenser attractiveness
      set totalattract (primattract + secattract) ; calculate total attractiveness
      set nmin 0 ; no tree defense
      set nmax 1000000000 ; no capacity
      set nstart 0 ; no beetle source
      set device (patch-set self device) ; add patch to agentset "device"
    ]
  ]
  display


end

;...deploy beetle sources manually...................................................................
to deploy-beetle-source

  if mouse-down? [ ; check if left click is performed
   ask patch mouse-xcor mouse-ycor [ ; ask patch where mouce is at
      set primattract 0 ; set primary attractiveness to 0
      set secattract 0 ; set secondary attractiveness to 0
      set totalattract 0 ; set total attractiveness to 0
      set source (patch-set source self) ; add patch to agent set source
      set pcolor brown ; give beetle sources a brown color
      set infestlev 3 ; set infestation level to not susceptible
    ]
  ]
  display

end
@#$#@#$#@
GRAPHICS-WINDOW
496
13
1223
741
-1
-1
14.1
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
1
ticks
30.0

BUTTON
395
50
475
83
go
go
T
1
T
OBSERVER
NIL
W
NIL
NIL
1

BUTTON
395
85
476
118
go once
go
NIL
1
T
OBSERVER
NIL
D
NIL
NIL
1

BUTTON
395
10
475
43
NIL
setup
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

PLOT
1565
570
1725
720
beetle energy
energy
beetles [n]
0.0
21.0
0.0
500.0
true
false
"" ""
PENS
"distpatch" 1.0 1 -2674135 true "" ""
"distbeetle" 1.0 0 -16777216 true "" ""

MONITOR
1325
65
1407
110
# infesting
beetle_infesting
17
1
11

MONITOR
1225
65
1322
110
# dispersing
beetle_dispersing
17
1
11

MONITOR
1300
15
1377
60
# starved
beetle_starved
17
1
11

PLOT
1225
570
1396
720
tree attractiveness
primattract
trees [n]
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -2674135 true "" ""

SLIDER
10
45
145
78
moveangle
moveangle
0
180
45.0
0.1
1
°
HORIZONTAL

MONITOR
1225
15
1295
60
# killed
beetle_killed
17
1
11

MONITOR
1380
15
1450
60
# lost
beetle_lost
17
1
11

SLIDER
10
10
145
43
meanenergy
meanenergy
6
20
10.0
0.1
1
NIL
HORIZONTAL

MONITOR
1410
65
1497
110
# staying
beetle_staying
17
1
11

MONITOR
1500
65
1642
110
# emerged
beetle_emerged
17
1
11

PLOT
1225
168
1725
563
beetle status
time [d]
beetles [n]
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"killed" 1.0 0 -16777216 true "" ""
"staying" 1.0 0 -7500403 true "" ""
"infesting" 1.0 0 -955883 true "" ""
"dispersing" 1.0 0 -13840069 true "" ""
"starved" 1.0 0 -13791810 true "" ""
"emerged" 1.0 0 -5825686 true "" ""
"locked" 1.0 0 -6459832 true "" ""

SLIDER
10
81
145
114
perceptdist
perceptdist
0
25
3.0
0.1
1
m
HORIZONTAL

PLOT
1400
570
1560
720
beetle success
time [d]
survival [%]
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"hnatdie" 1.0 0 -7500403 true "" ""

SLIDER
10
120
145
153
swarming
swarming
1
56
40.0
1
1
d
HORIZONTAL

MONITOR
1452
15
1519
60
# locked
beetle_locked
17
1
11

BUTTON
10
570
143
603
detention-devices
deploy-detention-devices
T
1
T
OBSERVER
NIL
A
NIL
NIL
1

INPUTBOX
10
240
490
319
path-to-input
input/2698/
1
0
String

SLIDER
10
155
145
188
generations
generations
1
3
3.0
1
1
n
HORIZONTAL

SLIDER
10
405
160
438
diffusionrate
diffusionrate
0
50
12.0
0.1
1
m
HORIZONTAL

SLIDER
10
365
160
398
windspeed
windspeed
0
5.5
0.0
0.1
1
m/s
HORIZONTAL

SWITCH
10
195
145
228
source-data
source-data
0
1
-1000

BUTTON
10
530
141
563
beetle-source
deploy-beetle-source
T
1
T
OBSERVER
NIL
Y
NIL
NIL
1

TEXTBOX
150
571
472
609
AFTER SETUP: place detention device by left clicking inside world while this procedure is running
12
0.0
1

TEXTBOX
148
531
463
560
AFTER SETUP: place beetle sources by left clicking inside world while this procedure is running
12
0.0
1

TEXTBOX
150
195
471
230
\"On\" = use provided data; \"Off\" = place beetle sources manually after SETUP
12
0.0
1

SLIDER
10
325
160
358
winddirection
winddirection
0
360
136.0
0.1
1
°
HORIZONTAL

TEXTBOX
165
325
490
355
direction from which the wind is blowing during the whole simulation unless winddata is set to ON
12
0.0
1

TEXTBOX
165
365
490
400
speed with which the wind is blowing during the whole simulation unless winddata is set to ON
12
0.0
1

TEXTBOX
165
405
480
435
maximum distance volatiles are capable of traveling
12
0.0
1

TEXTBOX
150
10
380
40
mean energy level of each beetle (sd = 2)
12
0.0
1

TEXTBOX
150
45
400
85
angle from which the heading is chosen during undirected beetle flight
12
0.0
1

TEXTBOX
150
80
370
110
distance in which beetles percieve volatiles
12
0.0
1

TEXTBOX
150
120
385
150
days during which each beetle source is emitting flight waves
12
0.0
1

TEXTBOX
150
160
410
186
number of generations to be simulated
12
0.0
1

MONITOR
1590
15
1647
60
NIL
day
0
1
11

MONITOR
1520
15
1585
60
# source
count source
0
1
11

MONITOR
1315
115
1422
160
# host 1st gen
nhost1
17
1
11

MONITOR
1425
115
1537
160
# host 2nd gen
nhost2
17
1
11

MONITOR
1540
115
1645
160
# host 3rd gen 
nhost3
17
1
11

MONITOR
1225
115
1312
160
# host sum
nhost
17
1
11

INPUTBOX
10
445
490
525
path-to-output
NIL
1
0
String

SLIDER
10
610
175
643
sanitation-felling
sanitation-felling
0
100
50.0
1
1
%
HORIZONTAL

MONITOR
1650
115
1725
160
# cut trees
ncut1 + ncut2 + ncut3
17
1
11

TEXTBOX
180
610
480
651
proportion of host trees cut during sanitation (one cutting after each beetle generation)
12
0.0
1

@#$#@#$#@
## WHAT IS IT?

This is version 1.0 of **IPS-SPREADS** (Infestation Pattern Simulation Supporting PREdisposition Assessment DetailS) model, an individual-based spatially explicit dispersal and host selection model for a host-bark beetle system. The model was introduced and firstly applied by Kautz et al., Ecol Model (2014), see ´How to cite´ section, and further enhanced (detention devices) by Pietzsch (2016)

## HOW TO USE IT

Habitat and beetle-related parameters can either be chosen by sliders or be defined directly in the code (e.g. plasticity settings). Monitors and plots provide visual information about the dispersal and infestation progress over time and space.

## THINGS TO TRY

The model can be used for all kind of issues concerning local interactions and their resulting spatio-temporal patterns. This version includes only one-source scenarios surrounded by isotropic habitats, however the model may be extended to different scenarios depending on your specific research question.
The authors want to encourage you to use and extend the model for your own research as well as for teaching. They would be grateful for any suggestions, ideas or cooperations.

## HOW TO CITE

If you mention this model, the authors kindly ask that you include the following citation referring to the model:
  
- Kautz, M; Schopf, R.; Imron, M.A. (2014). Individual traits as drivers of spatial dispersal and infestation patterns in a host-bark beetle system. Ecological Modelling 273, 264-276.
- Pietzsch, B. (2016): Überprüfung und Erweiterung des individuen-basierten Modells IPS (Infestation Pattern Simulation) zur Beantwortung waldschutzrelevanter Fragestellungen - eine Simulationsstudie. Masterarbeit, TU Dresden, Dresden.

To cite NetLogo software, please use: 
 
- Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.
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

ring
true
0
Circle -7500403 false true -1 -1 301

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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="UPS" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>nhost1</metric>
    <metric>nhost2</metric>
    <metric>nhost3</metric>
    <metric>prima1</metric>
    <metric>prima2</metric>
    <metric>prima3</metric>
    <metric>count spruce</metric>
    <metric>ncut1</metric>
    <metric>ncut2</metric>
    <metric>ncut3</metric>
    <enumeratedValueSet variable="sanitation-felling">
      <value value="0"/>
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="path-to-input">
      <value value="&quot;input/1614/&quot;"/>
      <value value="&quot;input/1632/&quot;"/>
      <value value="&quot;input/2565/&quot;"/>
      <value value="&quot;input/2698/&quot;"/>
      <value value="&quot;input/3888/&quot;"/>
      <value value="&quot;input/4563/&quot;"/>
      <value value="&quot;input/4698/&quot;"/>
      <value value="&quot;input/4704/&quot;"/>
      <value value="&quot;input/4822/&quot;"/>
      <value value="&quot;input/5240/&quot;"/>
      <value value="&quot;input/5264/&quot;"/>
      <value value="&quot;input/5265/&quot;"/>
      <value value="&quot;input/5368/&quot;"/>
      <value value="&quot;input/5397/&quot;"/>
      <value value="&quot;input/5498/&quot;"/>
      <value value="&quot;input/5500/&quot;"/>
      <value value="&quot;input/5520/&quot;"/>
      <value value="&quot;input/5884/&quot;"/>
      <value value="&quot;input/6037/&quot;"/>
      <value value="&quot;input/6041/&quot;"/>
      <value value="&quot;input/6170/&quot;"/>
      <value value="&quot;input/6185/&quot;"/>
      <value value="&quot;input/6187/&quot;"/>
      <value value="&quot;input/6197/&quot;"/>
      <value value="&quot;input/6309/&quot;"/>
      <value value="&quot;input/6443/&quot;"/>
      <value value="&quot;input/6575/&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="UPS1" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>nhost1</metric>
    <metric>nhost2</metric>
    <metric>nhost3</metric>
    <metric>prima1</metric>
    <metric>prima2</metric>
    <metric>prima3</metric>
    <metric>count spruce</metric>
    <metric>ncut1</metric>
    <metric>ncut2</metric>
    <metric>ncut3</metric>
    <enumeratedValueSet variable="sanitation-felling">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="path-to-input">
      <value value="&quot;input/1614/&quot;"/>
      <value value="&quot;input/1632/&quot;"/>
      <value value="&quot;input/2565/&quot;"/>
      <value value="&quot;input/2698/&quot;"/>
      <value value="&quot;input/3888/&quot;"/>
      <value value="&quot;input/4563/&quot;"/>
      <value value="&quot;input/4698/&quot;"/>
      <value value="&quot;input/4704/&quot;"/>
      <value value="&quot;input/4822/&quot;"/>
      <value value="&quot;input/5240/&quot;"/>
      <value value="&quot;input/5264/&quot;"/>
      <value value="&quot;input/5265/&quot;"/>
      <value value="&quot;input/5368/&quot;"/>
      <value value="&quot;input/5397/&quot;"/>
      <value value="&quot;input/5498/&quot;"/>
      <value value="&quot;input/5500/&quot;"/>
      <value value="&quot;input/5520/&quot;"/>
      <value value="&quot;input/5884/&quot;"/>
      <value value="&quot;input/6037/&quot;"/>
      <value value="&quot;input/6041/&quot;"/>
      <value value="&quot;input/6170/&quot;"/>
      <value value="&quot;input/6185/&quot;"/>
      <value value="&quot;input/6187/&quot;"/>
      <value value="&quot;input/6197/&quot;"/>
      <value value="&quot;input/6309/&quot;"/>
      <value value="&quot;input/6443/&quot;"/>
      <value value="&quot;input/6575/&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="UPS2" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>nhost1</metric>
    <metric>nhost2</metric>
    <metric>nhost3</metric>
    <metric>prima1</metric>
    <metric>prima2</metric>
    <metric>prima3</metric>
    <metric>count spruce</metric>
    <metric>ncut1</metric>
    <metric>ncut2</metric>
    <metric>ncut3</metric>
    <enumeratedValueSet variable="sanitation-felling">
      <value value="25"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="path-to-input">
      <value value="&quot;input/1614/&quot;"/>
      <value value="&quot;input/1632/&quot;"/>
      <value value="&quot;input/2565/&quot;"/>
      <value value="&quot;input/2698/&quot;"/>
      <value value="&quot;input/3888/&quot;"/>
      <value value="&quot;input/4563/&quot;"/>
      <value value="&quot;input/4698/&quot;"/>
      <value value="&quot;input/4704/&quot;"/>
      <value value="&quot;input/4822/&quot;"/>
      <value value="&quot;input/5240/&quot;"/>
      <value value="&quot;input/5264/&quot;"/>
      <value value="&quot;input/5265/&quot;"/>
      <value value="&quot;input/5368/&quot;"/>
      <value value="&quot;input/5397/&quot;"/>
      <value value="&quot;input/5498/&quot;"/>
      <value value="&quot;input/5500/&quot;"/>
      <value value="&quot;input/5520/&quot;"/>
      <value value="&quot;input/5884/&quot;"/>
      <value value="&quot;input/6037/&quot;"/>
      <value value="&quot;input/6041/&quot;"/>
      <value value="&quot;input/6170/&quot;"/>
      <value value="&quot;input/6185/&quot;"/>
      <value value="&quot;input/6187/&quot;"/>
      <value value="&quot;input/6197/&quot;"/>
      <value value="&quot;input/6309/&quot;"/>
      <value value="&quot;input/6443/&quot;"/>
      <value value="&quot;input/6575/&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="UPS3" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>nhost1</metric>
    <metric>nhost2</metric>
    <metric>nhost3</metric>
    <metric>prima1</metric>
    <metric>prima2</metric>
    <metric>prima3</metric>
    <metric>count spruce</metric>
    <metric>ncut1</metric>
    <metric>ncut2</metric>
    <metric>ncut3</metric>
    <enumeratedValueSet variable="sanitation-felling">
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="path-to-input">
      <value value="&quot;input/1614/&quot;"/>
      <value value="&quot;input/1632/&quot;"/>
      <value value="&quot;input/2565/&quot;"/>
      <value value="&quot;input/2698/&quot;"/>
      <value value="&quot;input/3888/&quot;"/>
      <value value="&quot;input/4563/&quot;"/>
      <value value="&quot;input/4698/&quot;"/>
      <value value="&quot;input/4704/&quot;"/>
      <value value="&quot;input/4822/&quot;"/>
      <value value="&quot;input/5240/&quot;"/>
      <value value="&quot;input/5264/&quot;"/>
      <value value="&quot;input/5265/&quot;"/>
      <value value="&quot;input/5368/&quot;"/>
      <value value="&quot;input/5397/&quot;"/>
      <value value="&quot;input/5498/&quot;"/>
      <value value="&quot;input/5500/&quot;"/>
      <value value="&quot;input/5520/&quot;"/>
      <value value="&quot;input/5884/&quot;"/>
      <value value="&quot;input/6037/&quot;"/>
      <value value="&quot;input/6041/&quot;"/>
      <value value="&quot;input/6170/&quot;"/>
      <value value="&quot;input/6185/&quot;"/>
      <value value="&quot;input/6187/&quot;"/>
      <value value="&quot;input/6197/&quot;"/>
      <value value="&quot;input/6309/&quot;"/>
      <value value="&quot;input/6443/&quot;"/>
      <value value="&quot;input/6575/&quot;"/>
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
1
@#$#@#$#@
