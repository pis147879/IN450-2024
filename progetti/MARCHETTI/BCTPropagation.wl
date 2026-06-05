(* ::Package:: *)

(* ::Subtitle:: *)
(*Differential Propagation Analysis Using the Boomerang Connectivity Table (Direct Approach)*)


(*IMPORT PRIMITIVES*)
SetDirectory[NotebookDirectory[]];
Get["AsconPrimitives.wl"];

S[x_]:= sbox[[x+1]];
Sinv[x_]:=sboxInv[[x+1]];


(*Classical Boomerang connctivity table *)
BCTclassic[delta_,Nabla_]:= Count[Table[BitXor[Sinv[BitXor[S[x],Nabla]],Sinv[BitXor[S[BitXor[x,delta]],Nabla]]],{x,0,31}],delta]
BCTmatrix= Table[BCTclassic[delta,Nabla],{delta,0,31},{Nabla,0,31}]//MatrixForm


(*Direct Boomerang connctivity table *)
BCTdirect[delta_,Nabla_] := Module[{outputs, validCases},
  outputs = 
    Flatten[Table[
        With[
          {
            f1 = S[x],
            f2 = S[BitXor[x, delta]],
            f3 = S[BitXor[x, deltaP]],
            f4 = S[BitXor[BitXor[x, delta], deltaP]]
          },
          {BitXor[f1, f2], BitXor[f3, f4], BitXor[f1, f3]} ],{x, 0, 31}, {deltaP, 0, 31}],1];
  validCases = Count[outputs, {Delta1_, Delta2_, Nabla1_} /; Delta1 == Delta2 && Nabla1 != 0 && Nabla1==Nabla]
];
BCTdirectMatrix=Table[BCTdirect[delta,Delta], {delta, 0, 31},{Delta,0,31}];
BCTdirectMatrix//MatrixForm


(* Builds an Ascon128 state from a list of 5-bits columns*) 
NablaToStateMulti[Nabla_, cols_List] := Module[{bits},
  bits = IntegerDigits[Nabla, 2, 5];  
  Table[
    If[bits[[w]] == 1,
      Total[BitSet[0, #] & /@ cols],  
      0
    ],
    {w, 1, 5}
  ]
];


(*Gets the 5-bits columns from an Ascon128 state *)
GetColumnFromState[state_List, col_Integer] := Module[{bits},
  bits = Table[ BitGet[state[[w]], col], {w, 1, 5} ]; 
  FromDigits[bits, 2]  
];


(* Starting from a candidate it applies the rotations of Ascon128 and returns the actives columns*)

Rotations[candidate_] := Module[
  {HW, BCTval, Nabla, col, nablaState, pnabla, nextBetas, activeCols},
  
  HW = candidate[[1]];
  BCTval = candidate[[2]];
  Nabla = candidate[[3]];  
  col = candidate[[4]]; 
     
  nablaState = NablaToStateMulti[Nabla, col];
  pnabla = AsconLinear[nablaState];
  nextBetas = Table[ GetColumnFromState[pnabla, c], {c, 0, 63} ];
  activeCols = Flatten[ Position[nextBetas, _?(# != 0 &)] ] - 1; 
  Map[ {#, nextBetas[[#+1]]} &, activeCols ]
];


(*Propagates the input difference in the chosen columns of the state for one round os Ascon encryption using the direct BCT *)
PropagateDifferentials[node_] :=
 Module[{l,col, delta, tmp, candidates, targets, probs,nCols, targetsGrouped},
 l=5;
  col = node[[2]];
  delta = node[[1]];
  nCols = Length[col]; 
  tmp = Table[
    {
      DigitCount[beta, 2, 1],                     
      Rationalize[(BCTdirect[delta, beta]/32)^nCols],          
      beta,                                      
      col                                       
    },
    {beta, 0, 2^l - 1}
  ];

  candidates = Select[tmp, (#[[1]] == 1) && (#[[2]] != 0) &]; (*Selects outputs with non-zero probabability and Hamming weight = 1*)

  targets = Map[Rotations[#]&,candidates];
  
  targetsGrouped = Map[
  Function[t, {t[[1, 2]], t[[All, 1]]}],
  targets];

  probs = Map[#[[2]] &, candidates];

  MapThread[Labeled[node -> #1, #2] &, {targetsGrouped, probs}]
]


(* ::Subtitle:: *)
(*Tests *)


(*Input difference in chosen column*)
node1 = {16, {0}};
edges1 =PropagateDifferentials[node1]


g1=GraphPlot[edges1,VertexLabels->"Name"]


sorted = SortBy[edges1, -Last[#] &]; (*Choosing the output with highest probability*)
node2=Last@First[sorted][[1]];
edges2 =PropagateDifferentials[node2]


g2=GraphPlot[Join[edges1,edges2],VertexLabels->"Name",ImageSize->{1000,500}]


node3={2,{0,30,44}};
edges3 =PropagateDifferentials[node3]


g3=GraphPlot[Join[edges1,edges2,edges3],VertexLabels->"Name",ImageSize->{1700,500}]
