(* ::Package:: *)

(* ::Subtitle:: *)
(*ASCON ENCRYPTION AND DECRYPTION*)


(*IMPORT PRIMITIVES*)
SetDirectory[NotebookDirectory[]];
Get["AsconPrimitives.wl"];

(*Complete Ascon permutation*)
AsconPermutation[state_,RC_]:=Module[{tmp},
tmp=x2Xor[state,RC];
tmp=AsconSbox[tmp];
tmp=AsconLinear[tmp];
Map[BitAnd[#,2^64-1]&,tmp]];

(*Ascon Encryption*)
AsconEnc[ptx_,key_,nonce_,AD_,a_,b_]:=Module[{state,IV,keyword1,nonce1,keyInt,nonceInt,keyword2,nonce2,RC,ADbits,Ptxbits,r=64,s,t,A,Ptx,Ctx,pad,ptxInt,ADint,tag,CtxHex},

(*Convert inputs to integers*)
keyInt=FromDigits[key,16];
nonceInt=FromDigits[nonce,16];
ptxInt=If[ptx==="",Null,FromDigits[ptx,16]];
ADint=If[AD==="",Null,FromDigits[AD,16]];

(*Split key and nonce into 2 64-bit words*)
keyword1=BitAnd[BitShiftRight[keyInt,64],2^64-1];
keyword2=BitAnd[keyInt,2^64-1];
nonce1=BitAnd[BitShiftRight[nonceInt,64],2^64-1];
nonce2=BitAnd[nonceInt,2^64-1];

(*Initialize state*)

IV=FromDigits["080c061010000000",16];
state={IV,keyword1,keyword2,nonce1,nonce2};
RC={16^^1E,16^^1D,16^^1C,16^^1B,16^^1A,16^^19,16^^18,16^^17,16^^16,16^^15,16^^14,16^^13};

(*INITIALIZATION*)

Do[state=AsconPermutation[state,RC[[i]]],{i,a}];
state[[4]]=BitXor[state[[4]],keyword1];
state[[5]]=BitXor[state[[5]],keyword2];

(*ASSOCIATED DATA*)

If[ADint=!=Null,
(*Divide AD in 64-bits blocks and add pad*)
ADbits=IntegerDigits[ADint,2];
pad=Mod[r-Mod[Length[ADbits]+1,r],r];
ADbits=Join[ADbits,{1},ConstantArray[0,pad]];
s=Length[ADbits]/r;
A=FromDigits[#,2]&/@Partition[ADbits,r];

Do[state[[1]]=BitXor[state[[1]],A[[j]]];
Do[state=AsconPermutation[state,RC[[i]]],{i,b}],{j,s}];
state[[5]]=BitXor[state[[5]],1];];

(*PLAINTEXT*)

If[ptxInt=!=Null,
(*Divide plaintexts in 64-bits blocks and add pad*)
Ptxbits=IntegerDigits[ptxInt,2];
pad=Mod[r-Mod[Length[Ptxbits]+1,r],r];
Ptxbits=Join[Ptxbits,{1},ConstantArray[0,pad]];
t=Length[Ptxbits]/r;
Ptx=FromDigits[#,2]&/@Partition[Ptxbits,r];
(*Generate ciphertext*)
Ctx=Table[{},{t}];
Do[state[[1]]=BitXor[state[[1]],Ptx[[j]]];
Ctx[[j]]=state[[1]];
Do[state=AsconPermutation[state,RC[[i]]],{i,b}],{j,t}],
Ctx={}];

(*FINALIZATION*)

state[[2]]=BitXor[state[[2]],keyword1];
state[[3]]=BitXor[state[[3]],keyword2];
Do[state=AsconPermutation[state,RC[[i]]],{i,a}];
state[[2]]=BitXor[state[[2]],keyword1];
state[[3]]=BitXor[state[[3]],keyword2];

(*Authentication tag*)
tag= StringJoin[IntegerString[BitAnd[state[[3]],2^64-1],16,16],
IntegerString[BitAnd[state[[4]],2^64-1],16,16]];

(*Ciphertext*)
CtxHex=If[Ctx==={},"",StringJoin[IntegerString[#,16,16]&/@Ctx]];

{CtxHex,tag}]

(*Ascon Decryption*)
AsconDec[CtxHex_,key_,tagRecv_,nonce_,AD_,a_,b_]:=Module[{state,IV,keyword1,nonce1,keyInt,nonceInt,keyword2,nonce2,RC,ADbits,Ctx,Ctxbits,r=64,s,t,A,Ptx,pad,ADint,tagCalc,PtxHex},

(*Convert inputs to integers*)
keyInt=FromDigits[key,16];
nonceInt=FromDigits[nonce,16];
ADint=If[AD==="",Null,FromDigits[AD,16]];

(*Split key and nonce into 2 64-bit words*)
keyword1=BitAnd[BitShiftRight[keyInt,64],2^64-1];
keyword2=BitAnd[keyInt,2^64-1];
nonce1=BitAnd[BitShiftRight[nonceInt,64],2^64-1];
nonce2=BitAnd[nonceInt,2^64-1];

(*Initialize state*)
IV=FromDigits["080c061010000000",16];
state={IV,keyword1,keyword2,nonce1,nonce2};
RC={16^^1E,16^^1D,16^^1C,16^^1B,16^^1A,16^^19,16^^18,16^^17,16^^16,16^^15,16^^14,16^^13};

(*INITIALIZATION*)

Do[state=AsconPermutation[state,RC[[i]]],{i,a}];
state[[4]]=BitXor[state[[4]],keyword1];
state[[5]]=BitXor[state[[5]],keyword2];

(*ASSOCIATED DATA*)

If[ADint=!=Null,ADbits=IntegerDigits[ADint,2];
pad=Mod[r-Mod[Length[ADbits]+1,r],r];
ADbits=Join[ADbits,{1},ConstantArray[0,pad]];
s=Length[ADbits]/r;
A=FromDigits[#,2]&/@Partition[ADbits,r];
Do[state[[1]]=BitXor[state[[1]],A[[j]]];
Do[state=AsconPermutation[state,RC[[i]]],{i,b}],{j,s}];
state[[5]]=BitXor[state[[5]],1];];

(*CIPHERTEXT*)

If[CtxHex==="",Ctx={},
t=StringLength[CtxHex]/16;
Ctx=Table[FromDigits[StringTake[CtxHex,{16 (i-1)+1,16 i}],16],{i,t}];];
(*Extract Plaintext*)
If[Ctx==={},Ptx={},Ptx=Table[0,{Length[Ctx]}];
Do[
Ptx[[j]]=BitAnd[BitXor[state[[1]],Ctx[[j]]],2^64-1];
state[[1]]=BitAnd[Ctx[[j]],2^64-1];
Do[state=AsconPermutation[state,RC[[i]]],{i,b}];
state=Map[BitAnd[#,2^64-1]&,state];,{j,Length[Ctx]}];];

(*FINALIZATION*)

state[[2]]=BitXor[state[[2]],keyword1];
state[[3]]=BitXor[state[[3]],keyword2];
Do[state=AsconPermutation[state,RC[[i]]],{i,a}];
state[[2]]=BitXor[state[[2]],keyword1];
state[[3]]=BitXor[state[[3]],keyword2];

(*Authentication tag*)
tagCalc=StringJoin[IntegerString[BitAnd[state[[3]],2^64-1],16,16],
IntegerString[BitAnd[state[[4]],2^64-1],16,16]];

(*Tag verification*)
  If[tagCalc =!= tagRecv, Return["Errore: tag non corrisponde!"]];

  If[Ptx === {}, Return[{"", tagCalc}]];
  
(*Padding remove*)
  Ptxbits = Flatten[IntegerDigits[#, 2, 64] & /@ Ptx];
  beforeLength = Length[Ptxbits];
  
  While[Length[Ptxbits] > 0 && Last[Ptxbits] == 0, Ptxbits = Most[Ptxbits]];
  If[Length[Ptxbits] > 0 && Last[Ptxbits] == 1, Ptxbits = Most[Ptxbits]];

  afterLength = Length[Ptxbits];
  
  neededHexDigits = Ceiling[afterLength/4];
  If[afterLength == 0,
   PtxHex = "",
   PtxHex = IntegerString[FromDigits[Ptxbits, 2], 16, neededHexDigits];
  ];

  Return[{PtxHex, tagCalc}];
 ]
