(* ::Package:: *)

(* --- Shift e funzione Simeck --- *)
shiftSX[x_Integer, r_Integer, n_Integer] := Module[{m = 2^n - 1, sx, dx},
  sx = BitAnd[BitShiftLeft[x, r], m];
  dx = BitShiftRight[x, n - r];
  BitOr[sx, dx]
]

fSimeck[x_Integer] := BitXor[BitAnd[x, shiftSX[x, 5, n]], shiftSX[x, 1, n]];

(* --- LFSR z-bits grado 6 --- *)
ZLFSR[round_Integer] := Module[{state = Table[1, {6}], zbits, next},
  zbits = Table[
    next = BitXor[state[[-1]], state[[1]]];
    state = Prepend[Most[state], next];
    state[[-1]],  
    {round}
  ];
  zbits
]

(* --- Key schedule --- *)
SplitKey[masterKey_Integer] := Module[{m = 2^n - 1},
  {
    BitAnd[BitShiftRight[masterKey, 3 n], m],
    BitAnd[BitShiftRight[masterKey, 2 n], m],
    BitAnd[BitShiftRight[masterKey, n], m],
    BitAnd[masterKey, m]
  }
]

keySchedule[masterKey_Integer, round_Integer] := Module[
  {t = SplitKey[masterKey], zbit, C, RC, newt3},
  zbit = ZLFSR[round];
  C = 2^n - 4;
  Table[
    With[{out = t[[4]]},
      RC = BitXor[C, zbit[[i]]];
      newt3 = BitXor[t[[4]], BitXor[fSimeck[t[[1]]], RC]];
      t = {t[[2]], t[[3]], t[[4]], newt3};
      out
    ],
    {i, 1, round}
  ]
]


EncryptBlock[L_Integer, R_Integer, roundKeys_List, Start_Integer, End_Integer] := Module[
  {rounds, K},
  
  K = {L, R};
  
  
  rounds = FoldList[
    Function[{state, i},
      Module[{temp},
        temp = BitXor[BitXor[fSimeck[state[[1]]], roundKeys[[i]]], state[[1]]];
        {state[[2]], temp}
      ]
    ],
    K,
    Range[Start, End]
  ];
  
  (* Ritorna lo stato finale *)
  Last[rounds]
]


(* --- E0 --- *)
E0[plain_Integer, roundKeys_List, r_Integer] := Module[
  {L, R, K, cipherBit},
  L = BitShiftRight[plain, n];
  R = BitAnd[plain, 2^n - 1];
  K = EncryptBlock[L, R, roundKeys, 1, r];
  FromDigits[Join[IntegerDigits[K[[1]], 2, n], IntegerDigits[K[[2]], 2, n]], 2]
]

(* --- Em --- *)
Em[E0_Integer, roundKeys_List, r_Integer, s_Integer] := Module[
  {L, R, LR},
  L = BitShiftRight[E0, n];
  R = BitAnd[E0, 2^n - 1];
  LR = EncryptBlock[L, R, roundKeys, r + 1, s + r];
  FromDigits[Join[IntegerDigits[LR[[1]], 2, n], IntegerDigits[LR[[2]], 2, n]], 2]
]

(* --- E1 --- *)
E1[Em_Integer, roundKeys_List, r_Integer, s_Integer] := Module[
  {L, R, LR},
  L = BitShiftRight[Em, n];
  R = BitAnd[Em, 2^n - 1];
  LR = EncryptBlock[L, R, roundKeys, s + r + 1, Length[roundKeys]];
  FromDigits[Join[IntegerDigits[LR[[1]], 2, n], IntegerDigits[LR[[2]], 2, n]], 2]
]

(* --- TotalEncrypt --- *)
TotalEncrypt[plain_Integer, roundKeys_List] := Module[
  {L, R, LR},
  L = BitShiftRight[plain, n];
  R = BitAnd[plain, 2^n - 1];
  LR = EncryptBlock[L, R, roundKeys, 1, Length[roundKeys]];
  FromDigits[Join[IntegerDigits[LR[[1]], 2, n], IntegerDigits[LR[[2]], 2, n]], 2]
]

(* --- DecryptBlock --- *)
DecryptBlock[cipher_Integer, roundKeys_List] := Module[
  {L, R, K, LR},
  L = BitShiftRight[cipher, n];
  R = BitAnd[cipher, 2^n - 1];
  
  
  K = FoldList[
    Function[{state, i},
      Module[{temp},
        temp = BitXor[BitXor[fSimeck[state[[2]]], roundKeys[[i]]], state[[2]]];
        {temp, state[[1]]}  
      ]
    ],
    {L, R},
    Reverse[Range[Length[roundKeys]]]
  ];
  
  (* Restituisco l'ultimo stato *)
  FromDigits[Join[IntegerDigits[Last[K][[1]], 2, n], IntegerDigits[Last[K][[2]], 2, n]], 2]
]
