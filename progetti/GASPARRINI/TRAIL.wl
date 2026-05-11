(* ::Package:: *)

ClearAll[FindTrail]
FindTrail[ddtList_, latList_, dlctList_] :=
 Module[
  {validMiddle, bestTrail},

  Print["--- Step 1: selezione middle pairs ---"];

  validMiddle =
   Select[
    dlctList,
    Abs[#[[4]]] < 1 &&
     #[[1]] != ConstantArray[0, Length[#[[1]]]] &
    ];

  Print["Numero di coppie middle : ", Length[validMiddle]];

  Print["--- Step 2: generazione TUTTI i trails ---"];

  bestTrail =
   Fold[
    Function[{acc, mid},
     Module[
      {dm, lm, forward, backward, trails},

      dm = mid[[1]];
      lm = mid[[2]];

      forward =
       Select[
        ddtList,
        #[[2]] == dm &&
         #[[4]] < 1 &&
         #[[1]] != ConstantArray[0, Length[#[[1]]]] &
        ];

      backward =
       Select[
        latList,
        #[[1]] == lm &&
         Abs[#[[4]]] < 1 &&
         #[[2]] != ConstantArray[0, Length[#[[2]]]] &
        ];

      trails =
       Flatten[
        Table[
         Module[
          {di, lo, score},

          di = fd[[1]];
          lo = bw[[2]];

          score =
           fd[[4]] *
            Abs[mid[[4]]] *
            bw[[4]]^2;

          {di, dm, lm, lo, score}
          ],
         {fd, forward},
         {bw, backward}
         ],
        1
        ];

      Fold[
       Function[{a, t},
        If[t[[5]] > a[[2]], {t, t[[5]]}, a]
        ],
       acc,
       trails
       ]
      ]
     ],
    {{}, 0},
    validMiddle
    ][[1]];

  Print["--- Step 3: miglior trail trovato ---"];

  bestTrail
  ]
  
PrintTrailList[trailList_] := Module[{i = 1},
  If[trailList === {} || trailList === Null,
    Print["Nessun trail da stampare!"];
    Return[];
  ];
  
  Do[
    Print[
      "Trail ", i, ":",
      "\n  \[CapitalDelta]i = ", fd[[1]],
      "\n  \[CapitalDelta]m = ", fd[[2]],
      "\n  \[Lambda]m = ", fd[[3]],
      "\n  \[Lambda]o = ", fd[[4]],
      "\n  Score = ", fd[[5]], "\n"
    ];
    i++
    ,
    {fd, trailList}
  ]
]
PrintTrailGrid[trailList_] := Module[{},
  If[trailList === {} || trailList === Null,
    Print["Nessun trail da stampare!"];
    Return[];
  ];
  
  Grid[
    Prepend[
      trailList,  (* la lista contiene gi\[AGrave] {\[CapitalDelta]i, \[CapitalDelta]m, \[Lambda]m, \[Lambda]o, score} *)
      {"\[CapitalDelta]i", "\[CapitalDelta]m", "\[Lambda]m", "\[Lambda]o", "Score"}
    ],
    Frame -> All,
    Alignment -> Center
  ]
]
