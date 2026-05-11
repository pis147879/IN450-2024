(* ::Package:: *)

BitParity[x_Integer] := Mod[Total[IntegerDigits[x, 2]], 2];
BoomerangAttack[keys_, r_, s_, bestTrail_, numQuartetti_, n_] := Module[
 {\[CapitalDelta]i, \[CapitalDelta]m, \[Lambda]m, \[Lambda]o, quartetti, results, hits, skippedDM, skippedLM},

 \[CapitalDelta]i = FromDigits[bestTrail[[1]], 2];
 \[CapitalDelta]m = FromDigits[bestTrail[[2]], 2];
 \[Lambda]m = FromDigits[bestTrail[[3]], 2];
 \[Lambda]o = FromDigits[bestTrail[[4]], 2];

 Print["Trail usato: \[CapitalDelta]i=", \[CapitalDelta]i, " \[CapitalDelta]m=", \[CapitalDelta]m, " \[Lambda]m=", \[Lambda]m, " \[Lambda]o=", \[Lambda]o];

 quartetti = RandomInteger[{0, 2^(2 n) - 1}, numQuartetti];

 results = ParallelMap[
   Function[P,
    Module[{Q, X, Xp, Y, Yp, C, Cp},

      Q = BitXor[P, \[CapitalDelta]i];

      X = E0[P, keys, r];
      Xp = E0[Q, keys, r];

      If[BitXor[X, Xp] =!= \[CapitalDelta]m,
        Return[ConstantArray[0, n]]   
      ];

      Y = Em[X, keys, r, s];
      Yp = Em[Xp, keys, r, s];

      If[BitParity[BitAnd[BitXor[Y, Yp], \[Lambda]m]] =!= 0,
        Return[ConstantArray[0, n]]   
      ];

      C = E1[Y, keys, r, s];
      Cp = E1[Yp, keys, r, s];

      If[BitParity[BitAnd[BitXor[C, Cp], \[Lambda]o]] === 0,
        ConstantArray[1, n],           (* hit *)
        ConstantArray[0, n]           
      ]
    ]
   ],
   quartetti
 ];

 hits = Count[results, ConstantArray[1, n]];
 skippedDM = Count[results, ConstantArray[0, n]]; 

 Print["Quartetti totali: ", numQuartetti];
 Print["Hit finali: ", hits];

 <|
  "TrailUsed" -> bestTrail,
  "MeasuredCorrelation" -> N[hits/numQuartetti]
 |>
]
