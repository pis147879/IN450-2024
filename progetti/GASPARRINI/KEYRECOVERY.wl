(* ::Package:: *)

Get["/home/gabriele/IN450/Progetto IN450/SIMECK.wl"]
BitParity[x_Integer] := Mod[Total[IntegerDigits[x, 2]], 2]
StateToInt[state_List, n_] := FromDigits[Join[IntegerDigits[state[[1]], 2, n], IntegerDigits[state[[2]], 2, n]], 2]
ClearAll[InvLastRound]
InvLastRound[c_Integer, k_Integer, n_Integer] := Module[
  {L, R, prevL, prevR},

  
  L = BitShiftRight[c, n];
  R = BitAnd[c, 2^n - 1];

  
  prevR = L;
  prevL = BitXor[R, fSimeck[L], k];

  FromDigits[
    Join[
      IntegerDigits[prevL, 2, n],
      IntegerDigits[prevR, 2, n]
    ],
    2
  ]
]
KeyRecovery[keys_, totalRound_, Trail_, quartetti_, n_] :=
Module[
 {
  \[CapitalDelta]i, \[CapitalDelta]m, \[Lambda]m, \[Lambda]o,
  guesses, scores
 },

 (* conversione trail *)
 \[CapitalDelta]i = FromDigits[Trail[[1]], 2];
 \[CapitalDelta]m = FromDigits[Trail[[2]], 2];
 \[Lambda]m = FromDigits[Trail[[3]], 2];
 \[Lambda]o = FromDigits[Trail[[4]], 2];

 guesses = Range[0, 2^n - 1];

 scores =
  ParallelTable[
   Module[
    {
     results, count, prob, bias,
     \[Lambda]oBits = IntegerDigits[\[Lambda]o, 2, 2*n]
    },

    
    results =
     Table[
      Module[
       {
        P, Pp, Q, Qp,
        L, R, L1, R1, L2, R2, L3, R3,
        K, K1, K2, K3,
        C, Cp, D, Dp,
        Y, Yp, Z, Zp,
        a, b
       },

       
       P  = RandomInteger[{0, 2^(2*n) - 1}];
       Pp = BitXor[P, \[CapitalDelta]i];

       Q  = RandomInteger[{0, 2^(2*n) - 1}];
       Qp = BitXor[Q, \[CapitalDelta]i];

       
       L = BitShiftRight[P, n];
       R = BitAnd[P, 2^n - 1];

       L1 = BitShiftRight[Pp, n];
       R1 = BitAnd[Pp, 2^n - 1];

       L2 = BitShiftRight[Q, n];
       R2 = BitAnd[Q, 2^n - 1];

       L3 = BitShiftRight[Qp, n];
       R3 = BitAnd[Qp, 2^n - 1];

       (* encryption *)
       K  = EncryptBlock[L,  R,  keys, 1, totalRound];
       K1 = EncryptBlock[L1, R1, keys, 1, totalRound];
       K2 = EncryptBlock[L2, R2, keys, 1, totalRound];
       K3 = EncryptBlock[L3, R3, keys, 1, totalRound];

       (* ricostruzione ciphertext *)
       C  = FromDigits[Join[IntegerDigits[K[[1]], 2, n], IntegerDigits[K[[2]], 2, n]], 2];
       Cp = FromDigits[Join[IntegerDigits[K1[[1]], 2, n], IntegerDigits[K1[[2]], 2, n]], 2];

       D  = FromDigits[Join[IntegerDigits[K2[[1]], 2, n], IntegerDigits[K2[[2]], 2, n]], 2];
       Dp = FromDigits[Join[IntegerDigits[K3[[1]], 2, n], IntegerDigits[K3[[2]], 2, n]], 2];

       (* inversione ultimo round *)
       Y  = InvLastRound[C , k, n];
       Yp = InvLastRound[Cp, k, n];

       Z  = InvLastRound[D , k, n];
       Zp = InvLastRound[Dp, k, n];

       
       a = Mod[
         Dot[IntegerDigits[Y, 2, 2*n], \[Lambda]oBits],
         2
       ];

       b = Mod[
         Dot[IntegerDigits[Z, 2, 2*n], \[Lambda]oBits],
         2
       ];

       
       If[a == b, 1, 0]
      ],
      {quartetti}
     ];

    (* statistiche *)
    count = Total[results];
    prob  = N[count/quartetti];
    bias  = N[prob - 1/2];

    {k, count, prob, bias}
   ],
   {k, guesses}
  ];

 (* ordiniamo per bias *)
 TakeLargestBy[scores, Abs[#[[4]]] &, 5]
]

ClearAll[PrintKeyRecovery]

PrintKeyRecovery[result_List, n_] := Module[
  {header},

  If[result === {} || result === Null,
    Print["Nessun risultato da stampare!"];
    Return[];
  ];

  header = {"Key (dec)", "Key (bit)", "Count", "Probabilit\[AGrave]", "Bias"};

  Grid[
    Prepend[
      ( {
          #[[1]],                                   (* k decimale *)
          IntegerDigits[#[[1]], 2, n],               (* k in bit *)
          #[[2]],                                   (* count *)
          #[[3]],                                   (* prob *)
          #[[4]]                                    (* bias *)
        } & /@ result ),

      header
    ],
    Frame -> All,
    Alignment -> Center,
    Background -> {None, {None, {LightGray}}},
    ItemStyle -> {FontFamily -> "Consolas", FontSize -> 12}
  ]
]
