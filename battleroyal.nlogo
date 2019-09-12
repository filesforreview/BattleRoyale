;this code executes simulations of Battle Royale games

;load packages
;extensions[stats]

;define players as the acting agents
breed [players player]

;define global variables
globals[
  skill_rankings ;table that includes players' skill and result rank
  correl ;spearman correlation matrix
  list_nr_encounters ;how many confrontations
  circle_center ;where is the middle of the shrinking arena
  spearman_skill_value ;spearman correlation value
  winner_skill_rank ;skill rank of survivor
  nr_at_died ;number of attacking/incoming players dying
  nr_def_died
  nr_worse_positioned ;number of players with unfavourable position dying
  nr_better_positioned
  advantage ;temporary variable noting if dying player was attacking or defending
  winning_chance; temporary variable storing the attackers chance of survival
]

patches-own[
  battle_advantage ;patches (i.e. locations) can be favourable on a continous scale
]


players-own[
  skill ;players have a skill value
  positioning ;players have a temporary position whicch can be advantageous or not
  skill_rank ;rank of skill value
  result_rank ;result from current game
  nr_encounters; how many confrontations (min 1)
]

to go ;what happens when starting the simulation
  tick ;next iteration
  if ticks = 1[ ;when first iteration
    do_spawn ;create players (see function below)
    do_rescale_skill ;generate and scale skills (see function below)
    ifelse terrain?[ ;if the terrain matters
      ask patches[set battle_advantage random-float 1]][ask patches[set battle_advantage 1] ;each location gets an advantage score
    ]
    do_rank ;assign skill ranks
    if remove_circle?[ ;when there is no circle then make evreything the circle interior for the whole game
      ask patches[
        set pcolor white
      ]
    ]
    if not remove_circle?[ ;when there is a circle
      do_initialize_circle] ;set circle center (see function below)
    do_plots ;make descriptive plots in netlogo
  ]
  if ticks mod 2 = 1 and not remove_circle?[
      do_update_circle ;shrink circle (see function below)

  ]

  do_move_and_battle ;main function (see below)
  do_plots ;update plots

  if (count players = 1 and ticks > 2)[ ;end of game
    ask one-of players[
      ;stats:add skill_rankings (list skill_rank count players) ; add last player to results
      set list_nr_encounters insert-item 0 list_nr_encounters nr_encounters ; add last player to results
      set winner_skill_rank skill_rank ;note the true rank of the winner
    ]
    do_spearman ;generate spearman correlation between skill and results rank (see function below)
    do_checks ;outputs for debugging (see function below)
    stop
  ]
end

to setup ;when restarting the game (we run this simulation 10000 times for each format)
  random-seed behaviorspace-run-number ;set seed for replicability
  clear-all ;clear all variables
  reset-ticks ;restart step counter
  ;set skill_rankings stats:newtable ;new table
  set list_nr_encounters (list) ;new table
end

to do_spawn
  create-players nr_players [
    setxy random-xcor random-ycor ;spawn in random location
    set nr_encounters 0
    if skill_distribution = "uniform"[ ;assign skill value depeding on chosen distribution
      set skill random-float 1]
    if skill_distribution = "normal"[
      set skill random-normal 0.5 skill_spread]
    if skill_distribution = "exponential"[
      set skill (random-exponential 0.1)]
  ]
end

to do_rescale_skill
  let max_skill [skill] of max-one-of players [skill]
  let min_skill [skill] of min-one-of players [skill]

  ask players[
    set skill ((skill - min_skill) / max_skill) ;rescale skill values to go from 0 to 1 (easier to keep track of and estimate the effect of external factors)
  ]
end

to do_rank ;compute skill ranks from skill values
  let rank-list sort-on [(- skill)] players
  let counter 0
  foreach rank-list[
    x -> ask x [set skill_rank counter ]
    set counter (counter + 1)
  ]
end

to do_initialize_circle ;initialize the shrinking arena (here characterized by white tiles)
  set circle_center one-of patches
  ask circle_center[ask patches in-radius 20 [set pcolor white]]
end

to-report patches-outside-radius [ radius ] ;helper function to obtain locations within a certai radius
  let inrad patches in-radius radius
  report patches with [ not member? self inrad ]
end

to do_update_circle ;shrink arena (by turning outermost tiles black)
  ask circle_center[ask (patches-outside-radius (20 - (min list 20 (ticks / 2)))) [set pcolor black]]
end

to do_move_and_battle
  ask players[
    if any? patches in-radius 1 with [pcolor = black][ ;when (almost) outside of circle
      face circle_center
      fd 1.5 ;move with the shrinking arena
    ]
    if [pcolor] of patch-here = white[ ;when inside arena
      ifelse random-float 1 < skill and skillful_movement?[ ;if skill determines movement
        let options patches in-radius 1
        move-to max-one-of options [battle_advantage] ;go to (stay at) best option
        ][rt random-float 360 fd 1.5 ;go to random option
      ]
    ]
    if count other players in-radius 1.5 > 0[ ;when meeting a competitor
      set nr_encounters (nr_encounters + 1) ;increase number of encounters
      let encounter one-of other players in-radius 1.5 ;define the competitor as "encounter"
      ask encounter [
        set nr_encounters (nr_encounters + 1)
        set positioning [battle_advantage] of patch-here ;how good is the encounter's position
      ]
      set positioning [battle_advantage] of patch-here ;how good is my position
      ifelse positioning > ([positioning] of encounter) [set advantage "a"][set advantage "d"] ;keeping track of advantage
      if positioning = ([positioning] of encounter)[set advantage "n"] ;keeping track of advantage

      if skill_and_positioning = "addition"[ ;one way to compute probability of winning (less effect of skill)
      set winning_chance (((1 - movem_penalty) * (imp_skill_over_positioning * skill + (1 - imp_skill_over_positioning) * positioning)) / (((1 - movem_penalty) * (imp_skill_over_positioning * skill + (1 - imp_skill_over_positioning) * positioning)) + (imp_skill_over_positioning * [skill] of encounter + (1 - imp_skill_over_positioning) * [positioning] of encounter) + 0.0000000001))
     ]
      if skill_and_positioning = "multiplication"[ ;another way to compute probability of winning (more effect of skill; last reported in manuscript)
      set winning_chance (((1 - movem_penalty * skill) * (imp_skill_over_positioning * skill + (1 - imp_skill_over_positioning) * positioning * skill)) / (((1 - movem_penalty) * (imp_skill_over_positioning * skill + (1 - imp_skill_over_positioning) * positioning * skill)) + (imp_skill_over_positioning * [skill] of encounter + (1 - imp_skill_over_positioning) * [positioning] of encounter  * [skill] of encounter) + 0.0000000001))
      ]

      let temp random-float 1
      if temp > winning_chance[   ;attacker dies
        ;stats:add skill_rankings (list skill_rank count players) ;add skill and result of player
        set list_nr_encounters insert-item 0 list_nr_encounters nr_encounters
        set nr_at_died (nr_at_died + 1) ;increase the number of attackers that died
        if advantage = "d" [set nr_worse_positioned (nr_worse_positioned + 1)] ;increase the number of players with position disadvantage that died
        if advantage = "a" [set nr_better_positioned (nr_better_positioned + 1)]

        if looting_and_injuries?[ ;consequences for the winner of the fight if resources are activated
          ask encounter[

            let outcome random-float 1
            if skill_affects_looting_and_injuries?[ ;if skill affects resources set consequences accordingly
              ifelse outcome < skill [
                set skill (skill + random-float (1 - skill))
                ][set skill (skill - random-float (skill))
              ]

            ]

            if not skill_affects_looting_and_injuries?[ ;if skill does not affect resources set consequences randomly
              ifelse outcome < random-float 1 [
                set skill (skill + random-float (1 - skill))
                ][set skill (skill - random-float (skill))
              ]
            ]
          ]
        ]
        die ;remove attacker
      ]


       if temp < winning_chance[ ;defender dies (the steps below are the same as above but now executed from the perspective of the defender
         if looting_and_injuries?[
           let outcome random-float 1
           if skill_affects_looting_and_injuries?[
             ifelse outcome < skill [
               set skill (skill + random-float (1 - skill))
               ][set skill (skill - random-float (skill))]
           ]

           if not skill_affects_looting_and_injuries?[
             ifelse outcome < random-float 1 [
               set skill (skill + random-float (1 - skill))
               ][set skill (skill - random-float (skill))
             ]
           ]
         ]


         ask encounter[
           ;stats:add skill_rankings (list skill_rank count players)
           set list_nr_encounters insert-item 0 list_nr_encounters nr_encounters
           set nr_def_died (nr_def_died + 1)
           if advantage = "a" [set nr_worse_positioned (nr_worse_positioned + 1)]
           if advantage = "d" [set nr_better_positioned (nr_better_positioned + 1)]
           die
         ]
      ]
    ]
  ]
end

to do_plots ;update netlogo plots
  set-current-plot "skills"
  set-current-plot-pen "skills"
  histogram [skill] of players
  set-current-plot "encounters"
  set-current-plot-pen "enc"
  histogram list_nr_encounters
end

to do_spearman ;compute correlation
  ;set correl stats:correlation skill_rankings
  ;output-print stats:get-observations skill_rankings 0
  ;output-print stats:get-observations skill_rankings 1
  ;set spearman_skill_value item 0 item 1 correl
end

to do_checks ;debugging assistance
  ;show ""
  ;show ""
  ;show "percent attackers died:"
  ;show (nr_at_died / (nr_def_died + nr_at_died)) * 100
  ;show "percent worse_positioned died:"
  ;show nr_worse_positioned / nr_players
  ;show "percent better_positioned died:"
  ;show nr_better_positioned / nr_players
  ;show "spearman skill & result:"
  ;show spearman_skill_value
end
@#$#@#$#@
GRAPHICS-WINDOW
782
2
1243
464
-1
-1
13.73
1
10
1
1
1
0
0
0
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
18
17
81
50
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

BUTTON
114
15
177
48
go
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

CHOOSER
40
108
178
153
skill_distribution
skill_distribution
"uniform" "normal" "exponential"
2

SLIDER
25
177
197
210
skill_spread
skill_spread
0
1
0.2
0.05
1
NIL
HORIZONTAL

PLOT
21
304
221
454
skills
NIL
NIL
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"skills" 0.05 1 -16777216 true "" ""

SLIDER
327
88
499
121
movem_penalty
movem_penalty
0
1
0.3
0.05
1
NIL
HORIZONTAL

SWITCH
345
42
483
75
remove_circle?
remove_circle?
1
1
-1000

SWITCH
363
144
466
177
terrain?
terrain?
1
1
-1000

SLIDER
317
193
509
226
imp_skill_over_positioning
imp_skill_over_positioning
0
1
0.66
0.05
1
NIL
HORIZONTAL

SWITCH
338
250
496
283
skillful_movement?
skillful_movement?
0
1
-1000

SLIDER
21
64
193
97
nr_players
nr_players
10
1000
100.0
10
1
NIL
HORIZONTAL

SWITCH
529
88
772
121
skill_affects_looting_and_injuries?
skill_affects_looting_and_injuries?
0
1
-1000

SWITCH
566
42
738
75
looting_and_injuries?
looting_and_injuries?
0
1
-1000

PLOT
243
307
443
457
encounters
NIL
NIL
1.0
10.0
0.0
10.0
true
false
"" ""
PENS
"enc" 1.0 1 -16777216 true "" ""

CHOOSER
606
218
744
263
skill_and_positioning
skill_and_positioning
"addition" "multiplication"
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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
NetLogo 6.1.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="basic_survival" repetitions="10000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>spearman_skill_value</metric>
    <metric>winner_skill_rank</metric>
    <enumeratedValueSet variable="imp_skill_over_positioning">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skill_spread">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="terrain?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remove_circle?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skillful_movement?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr_players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skill_distribution">
      <value value="&quot;uniform&quot;"/>
      <value value="&quot;normal&quot;"/>
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movem_penalty">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="looting_and_injuries?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skill_affects_looting_and_injuries?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skill_and_positioning">
      <value value="&quot;addition&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="shrinking_arena" repetitions="10000" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>spearman_skill_value</metric>
    <metric>winner_skill_rank</metric>
    <enumeratedValueSet variable="imp_skill_over_positioning">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skill_spread">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="terrain?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remove_circle?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skillful_movement?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr_players">
      <value value="10"/>
      <value value="100"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skill_distribution">
      <value value="&quot;normal&quot;"/>
      <value value="&quot;uniform&quot;"/>
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movem_penalty">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="looting_and_injuries?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skill_affects_looting_and_injuries?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skill_and_positioning">
      <value value="&quot;addition&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="advanced features" repetitions="10000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>spearman_skill_value</metric>
    <metric>winner_skill_rank</metric>
    <enumeratedValueSet variable="imp_skill_over_positioning">
      <value value="1"/>
      <value value="0.66"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skill_spread">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="terrain?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remove_circle?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skillful_movement?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr_players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skill_affects_looting_and_injuries?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skill_distribution">
      <value value="&quot;exponential&quot;"/>
      <value value="&quot;uniform&quot;"/>
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="looting_and_injuries?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movem_penalty">
      <value value="0"/>
      <value value="0.33"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skill_and_positioning">
      <value value="&quot;addition&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="advanced features extended" repetitions="10000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>spearman_skill_value</metric>
    <metric>winner_skill_rank</metric>
    <enumeratedValueSet variable="imp_skill_over_positioning">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skill_spread">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="terrain?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remove_circle?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skillful_movement?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr_players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skill_affects_looting_and_injuries?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skill_distribution">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="looting_and_injuries?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movem_penalty">
      <value value="0.33"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skill_and_positioning">
      <value value="&quot;multiplication&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="malte" repetitions="10000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>spearman_skill_value</metric>
    <metric>winner_skill_rank</metric>
    <enumeratedValueSet variable="imp_skill_over_positioning">
      <value value="0.66"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skill_spread">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skill_and_positioning">
      <value value="&quot;multiplication&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="terrain?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remove_circle?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skillful_movement?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr_players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skill_distribution">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skill_affects_looting_and_injuries?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="looting_and_injuries?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movem_penalty">
      <value value="0.3"/>
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
