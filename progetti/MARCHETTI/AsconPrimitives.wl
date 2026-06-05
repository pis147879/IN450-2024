(* ::Package:: *)

(* ::Subtitle:: *)
(*ASCON PRIMITIVES AND UTILITIES*)


(*Parameters*)
wordBits = 64;
stateWords = 5;
RC={16^^1E,16^^1D,16^^1C,16^^1B,16^^1A,16^^19,16^^18,16^^17,16^^16,16^^15,16^^14,16^^13};

(*Utilities*)
BitRotateRight64[n_, k_] := BitAnd[BitOr[BitShiftRight[n, k], BitShiftLeft[n, 64 - k]], 2^64 - 1];

XorState[a_List, b_List] := MapThread[BitXor, {a, b}];

(*Ascon S-box and inverse lists*)
sbox = {4, 11, 31, 20, 26, 21, 9, 2, 27, 5, 8, 18, 29, 3, 6, 28, 30, 19, 7, 14, 0, 13, 17, 24, 16, 12, 1, 25, 22, 10, 15, 23};
sboxInv = {20, 26, 7, 13, 0, 9, 14, 18, 10, 6, 29, 1, 25, 21, 19, 30, 24, 22, 11, 17, 3, 5, 28, 31, 23, 27, 4, 8, 15, 12, 16, 2};
AsconSbox[state_List] := Module[{inVal,outBits,columns, bitcol,initst=state},
  columns = Table[
    bitcol = Table[BitGet[state[[j]], i], {j, 1, 5}];
    inVal = FromDigits[Reverse[bitcol], 2];  
    outBits = IntegerDigits[sbox[[inVal + 1]], 2, 5];
    Reverse[outBits], 
    {i, 0, 63}
  ];
  Table[
    FromDigits[Reverse[columns[[All, j]]], 2],
    {j, 1, 5}
  ]
];

AsconSboxInv[state_] := Module[{inVal,outBits,columns, bitcol},
  columns = Table[
    bitcol = Table[BitGet[state[[j]], i], {j, 1, 5}];
    inVal = FromDigits[Reverse[bitcol], 2];
    outBits = IntegerDigits[sboxInv[[inVal + 1]], 2, 5];
    Reverse[outBits],
    {i, 0, 63}
  ];
  Table[
    FromDigits[Reverse[columns[[All, j]]], 2],
    {j, 1, 5}
  ]
];

(*Ascon Linear layer and his inverse*)
AsconLinear[state_]:=Module[{x0,x1,x2,x3,x4},{x0,x1,x2,x3,x4}=state;
x0=BitXor[x0,BitXor[BitRotateRight64[x0,19],BitRotateRight64[x0,28]]]//BitAnd[#,2^64-1]&;
x1=BitXor[x1,BitXor[BitRotateRight64[x1,61],BitRotateRight64[x1,39]]]//BitAnd[#,2^64-1]&;
x2=BitXor[x2,BitXor[BitRotateRight64[x2,1],BitRotateRight64[x2,6]]]//BitAnd[#,2^64-1]&;
x3=BitXor[x3,BitXor[BitRotateRight64[x3,10],BitRotateRight64[x3,17]]]//BitAnd[#,2^64-1]&;
x4=BitXor[x4,BitXor[BitRotateRight64[x4,7],BitRotateRight64[x4,41]]]//BitAnd[#,2^64-1]&;
{x0,x1,x2,x3,x4}];

(*XOR with Round constant*)
x2Xor[state_, RC_] := Module[{newstate = state},
  newstate[[3]] = BitXor[state[[3]], RC] // BitAnd[#, 2^64 - 1] &;
  newstate
];

(*Ascon forward and backward round*)
AsconRound[state_List, RC_Integer] :=
  AsconLinear[AsconSbox[x2Xor[state, RC]]];
