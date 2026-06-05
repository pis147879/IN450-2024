(* ::Package:: *)

(* ::Input:: *)
(**)


Get["SIMECK.wl"];
Get["TABLE.wl"];
Get["TRAIL.wl"];
Get["BOOMERANG.wl"];
Get["KEYRECOVERY.wl"]
(*parametri modificabili*)
n=3; (*dimensione del blocco *)
masterKey = RandomInteger[{0, 2^(4*n)-1}];
totalRound = 12;
R = 4; (* Split1 *)
S = 4; (* Split2 *)
numQuartetti = 20000;
keys = keySchedule[masterKey, totalRound];
Print["\n KEYSCHEDULE: "];
Print[keys];

(* 1) Calcolo DDT, DLCT e LAT *)

Print["\n STEP 1: Calcolo DDT "];

E0fun[x_] := E0[x, keys, R];
Emfun[x_] := Em[E0[x,keys,R], keys, R, S];
E1fun[x_] := E1[Em[E0[x,keys,R],keys,R,S], keys, R, S];
ddt = CalcoloDDT[E0fun,n];
Print[TabellaDDT[ddt]];
Print["\n STEP 2: Calcolo DLCT "];
dlct= CalcoloDLCT[Emfun,n];
TabellaDLCT[dlct]
Print["\n STEP 3: Calcolo LAT "];
lat= CalcoloLAT[E1fun,n];
Print[TabellaLAT[lat]];




(* 2) calcolo i trails*)

Print["\n STEP 4: Calcolo TRAILS "];

Trails = FindTrail[ddt,lat,dlct];
PrintTrailList[{Trails}]
PrintTrailGrid[{Trails}] 

(* 3)Boomerang *)

Print["\n STEP 5: BOOMERANG ATTACK "];

(* Mostra risultati in tabella *)
res = BoomerangAttack[keys, R, S, Trails, numQuartetti, n];
Print["Correlazione misurata: ", res["MeasuredCorrelation"]];

(* 4) Key recovery *) (*opzionale*)

Print["\n STEP 6: KEY RECOVERY"];

result = KeyRecovery[keys,totalRound,Trails, numQuartetti, n];
PrintKeyRecovery[result,n]
Print[result]
Print["\n Esperimento completato"];































































































