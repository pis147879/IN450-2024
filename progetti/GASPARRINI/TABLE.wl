(* ::Package:: *)

(* ---DDT--- *)
ClearAll[CalcoloDDT]
CalcoloDDT[f_, n_] := Module[{N = 2^(2 n)},
  Select[
    Flatten[
      Table[
        Module[{countTbl, dy},
          countTbl = ConstantArray[0, N];
          countTbl[[# + 1]]++ & /@ (BitXor[f[#], f[BitXor[#, dx]]] & /@ Range[0, N - 1]);

          Table[
            {IntegerDigits[dx, 2, 2 n], IntegerDigits[dy, 2, 2 n],
             countTbl[[dy + 1]], SetPrecision[countTbl[[dy + 1]]/N, 5]},
            {dy, 0, N - 1}
          ]
        ],
        {dx, 0, N - 1}
      ],
      1
    ],
    #[[3]] =!= 0 &
  ]
];
TabellaDDT[ddt_List] := Module[{header},
  header = {"\[CapitalDelta]x (bit)", "\[CapitalDelta]y (bit)", "Count", "Prob"};
  Grid[
    Prepend[ddt, header],
    Frame -> All,
    Alignment -> Center,
    Background -> {None, {None, {LightGray}}},
    ItemStyle -> {FontFamily -> "Consolas", FontSize -> 12}
  ]
];
(* ---DLCT--- *)
ClearAll[CalcoloDLCT]
CalcoloDLCT[f_, n_] := Module[
  {N = 2^(2*n), inputs, table},

  inputs = IntegerDigits[#, 2, 2*n] & /@ Range[0, N - 1];

  table = Flatten[
    Table[
      Module[{corr},
        
        corr = Total[
          Table[
            (-1)^BitXor[
              Mod[Dot[lambda, IntegerDigits[f[FromDigits[inputs[[i]], 2]], 2, 2*n]], 2],
              Mod[
                Dot[lambda, 
                 IntegerDigits[
                  f[BitXor[FromDigits[inputs[[i]], 2], deltaX]], 2, 2*n]], 2]
            ],
            {i, N}
          ]
        ];

        {IntegerDigits[deltaX, 2, 2*n], lambda, corr, SetPrecision[corr/N, 5]}
      ],

      {deltaX, 0, N - 1},
      {lambda, inputs}
    ],
    1
  ];

  Select[table, #[[3]] != 0 &]
]
TabellaDLCT[dlct_List] := Module[{header},
  header = {"\[Lambda]i (bit)", "\[Lambda]o (bit)", "DLCT", "Correlazione"};
  Grid[
    Prepend[dlct, header],
    Frame -> All,
    Alignment -> Center,
    Background -> {None, {None, {LightGray}}},
    ItemStyle -> {FontFamily -> "Consolas", FontSize -> 12}
  ]
];
(* ---LAT--- *)
ClearAll[CalcoloLAT]
CalcoloLAT[f_, n_] := Module[
  {N = 2^(2*n), inputs, outputs, table},
  
  
  inputs = IntegerDigits[#, 2, 2*n] & /@ Range[0, N - 1];
  outputs = (IntegerDigits[f[FromDigits[#, 2]], 2, 2*n] & /@ inputs);
  
  
  table = Flatten[
    Table[
      Module[{latVal, corr},
      

        latVal = Total[
          Table[
            (-1)^(BitXor[Mod[Dot[lambdaI, inputs[[i]]], 2], Mod[Dot[lambdaO, outputs[[i]]], 2]]),
            {i, N}
          ]
        ];
        corr = SetPrecision[latVal/N, 5] ;
        {lambdaI, lambdaO, latVal, corr}
      ],
      {lambdaI, inputs}, 
      {lambdaO, inputs}
    ],
    1
  ];
  
  
  Select[table, #[[3]] != 0 &]
]
TabellaLAT[lat_List] := Module[{header},
  header = {"\[Lambda]i (bit)", "\[Lambda]o (bit)", "LAT", "Correlazione"};
  Grid[
    Prepend[lat, header],
    Frame -> All,
    Alignment -> Center,
    Background -> {None, {None, {LightGray}}},
    ItemStyle -> {FontFamily -> "Consolas", FontSize -> 12}
  ]
];
