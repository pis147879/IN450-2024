(* ::Package:: *)

ClearAll["Global`*"]

(*
Progetto IN450 di Elisa Tarsi
Implementazione del Boomerang Attack su PRESENT

Paper di riferimento:
https://eprint.iacr.org/2024/255


Descrizione:
Questo codice implementa il boomerang attack, come descritto nel paper, 
su cifrario PRESENT (64 bit block, 80 bit key).
*)


(* ============================== *)
(* SBOX DI PRESENT                *)
(* ============================== *)

sbox = {12, 5, 6, 11, 9, 0, 10, 13, 3, 14, 15, 8, 4, 7, 1, 2};
(* \[EGrave] la S-box del cifrario PRESENT *)

(* Questa funzione applica la S-box a ciascun nibble dello stato.
   nNibbles indica il numero di nibble a cui applicare la trasformazione. *)

ApplicaSbox[stato_, nNibbles_] := Module[{y = 0, i, n, n2},
  For[i = 0, i < nNibbles, i++,
   
   (* estraggo il nibble i-esimo (4 bit) *)
   n = BitAnd[BitShiftRight[stato, 4*i], 15];
   
   (* applico la S-box *)
   n2 = sbox[[n + 1]];
   
   (* reinserisco il nibble trasformato nella posizione corretta *)
   y = BitOr[y, BitShiftLeft[n2, 4*i]];
   
   ];
  
  y
]


(* ============================== *)
(* PERMUTAZIONE DEI BIT (P-LAYER) *)
(* ============================== *)

(* NewBitPosition \[EGrave] la funzione che definisce la posizione del bit dopo la permutazione *)

NewBitPosition[i_] := If[i == 63, 63, Mod[16*i, 63]]


(* PermutaBit \[EGrave] la funzione che applica la permutazione dei bit allo stato *)

PermutaBit[stato_] := Module[{y = 0, i, b, j},
  
  For[i = 0, i < 64, i++,
   
   (* estraggo il bit i-esimo *)
   b = BitAnd[BitShiftRight[stato, i], 1];
   
   (* calcolo la nuova posizione *)
   j = NewBitPosition[i];
   
   (* inserisco il bit nella nuova posizione *)
   y = BitOr[y, BitShiftLeft[b, j]];
   
   ];
  
  y
]


(* ============================== *)
(* ROUND DI PRESENT               *)
(* ============================== *)

AddRoundKey[stato_, roundKey_] := BitXor[stato, roundKey];

(* Un round di PRESENT \[EGrave] formato da: AddRoundKey 
poi applica la sbox su 16 nibble e poi applica la permutazione dei bit *)
RoundPRESENT[stato_, roundKey_] := 
  PermutaBit[ApplicaSbox[AddRoundKey[stato, roundKey], 16]];

(* Inizialmente fisso la chiave per testare la funzione *)
Encrypt1Round[plaintext_, roundkey_] := RoundPRESENT[plaintext, roundkey];

EncryptNRounds[plaintext_, roundkey_, n_] := Module[{stato = plaintext, i},
  For[i = 1, i <= n, i++,
    stato = RoundPRESENT[stato, roundkey];
  ];
  stato
];


(* ============================== *)
(* KEY SCHEDULE DI PRESENT (80 bit)*)
(* ============================== *)

(* Creo le maschere per tagliare a 80 o 64 bit *)
mask80 = 2^80 - 1;
mask64 = 2^64 - 1;

(* La funzione RoundKeyFrom80 estrae la round key da 64 bit: 
i 64 bit pi\[UGrave] significativi della chiave da 80 bit *)
RoundKeyFrom80[K_] := BitAnd[BitShiftRight[BitAnd[K, mask80], 16], mask64];

(* Rotazione a sinistra di 61 bit su 80 bit (circolare) *)
RotateLeft80[K_] := Module[{k = BitAnd[K, mask80]},
  BitAnd[
    BitOr[
      BitShiftLeft[k, 61],
      BitShiftRight[k, 19]   (* 80-61 = 19 *)
    ],
    mask80
  ]
];

(* Estrae il nibble pi\[UGrave] significativo (4 bit) dalla chiave *)
TopNibble[K_] := Module[{k},
  k = BitAnd[K, mask80];
  BitAnd[BitShiftRight[k, 76], 15]
];
(* Applica la sbox al nibble pi\[UGrave] significativo *)
sboxTopNibble[n_] := sbox[[n + 1]];

(* Cancella il nibble pi\[UGrave] significativo mettendolo a 0 *)
ClearTopNibble[K_] := Module[{k},
  k = BitAnd[K, mask80];
  BitAnd[k, BitNot[BitShiftLeft[15, 76]]]
];

(* Applica la S-box al nibble pi\[UGrave] significativo e lo reinserisce *)
ApplySboxToFirstNibble[K_] := Module[{top, top2, cleared},
  top = TopNibble[K];
  top2 = sboxTopNibble[top];
  cleared = ClearTopNibble[K];
  BitAnd[BitOr[cleared, BitShiftLeft[top2, 76]], mask80]
];

(* XOR con round counter: fa lo XOR bit a bit con i 5 bit che si trovano nelle posizioni 19..15 *)
XorRoundCounter[K_, r_] := Module[{k, roundcounter},
  k = BitAnd[K, mask80];
  roundcounter = BitShiftLeft[BitAnd[r, 31], 15];  (* 31 = 11111 *)
  BitAnd[BitXor[k, roundcounter], mask80]
];

(* Aggiorna la chiave per passare al round successivo *)
UpdateKey[K_, r_] := Module[{k1, k2, k3},
  k1 = RotateLeft80[K];
  k2 = ApplySboxToFirstNibble[k1];
  k3 = XorRoundCounter[k2, r];
  k3
];

(* Genera la lista delle round keys (64-bit) per n round *)
RoundKeys80[Key_, n_] := Module[{K = BitAnd[Key, mask80], roundkeys = {}, r},
  For[r = 1, r <= n, r++,
    AppendTo[roundkeys, RoundKeyFrom80[K]];
    K = UpdateKey[K, r];
  ];
  roundkeys
];


(* ============================== *)
(* ENCRYPT DI PRESENT             *)
(* ============================== *)

(* La funzione EncryptPRESENT prende in input da:
   - plaintext: 64 bit
   - Key: 80 bit
   - nRounds: numero round (\[EGrave] implementato PRESENT completo con 31 completo,
   ne useremo poi una versione con 9 round per l'attacco)
   - finalXor: se True, esegue XOR finale con round key extra (come PRESENT completo)
*)

EncryptPRESENT[plaintext_, Key_, nRounds_, finalXor_ : False] := Module[
  {stato, roundkeys, i},

  stato = BitAnd[plaintext, mask64];

  roundkeys = RoundKeys80[Key, nRounds + If[finalXor, 1, 0]];

  For[i = 1, i <= nRounds, i++,
    stato = RoundPRESENT[stato, roundkeys[[i]]];
  ];

  If[finalXor,
    stato = AddRoundKey[stato, roundkeys[[nRounds + 1]]];
  ];

  stato
];


(* ============================== *)
(* BOOMERANG ATTACK               *)
(* ============================== *)

(* La funzione Parity mi restituisce 0 se il numero di bit 1 \[EGrave] pari, 1 se \[EGrave] dispari *)
Parity[x_] := Mod[DigitCount[x, 2, 1], 2];

(* Calcola statistiche a partire dal conteggio di z=0 e z=1 *)
CalcolaStatisticheDL[conteggioZero_, conteggioUno_] := Module[
  {numeroCampioni, probZero, correlazioneStimata, log2Correlazione},

  numeroCampioni = conteggioZero + conteggioUno;

  probZero = conteggioZero/numeroCampioni;

  correlazioneStimata = (conteggioZero - conteggioUno)/numeroCampioni;

  log2Correlazione = If[correlazioneStimata == 0, -Infinity, Log2[Abs[correlazioneStimata]]];

  {
    "N" -> numeroCampioni,
    "Probabilit\[AGrave] di 0" -> N[probZero],
    "Sbilanciamento" -> N[correlazioneStimata],
    "log2|Correlazione|" -> N[log2Correlazione]
  }
];

(* Parametri del paper per PRESENT (9 round) *)
DeltaPaper = 16^^0000009000000000;
GammaPaper = 16^^0020000000200020;

(* Esperimento DL su 9 round: genera coppie (P, P XOR delta) e misura lo sbilanciamento *)
EsperimentoDL9[key_, N_, delta_, gamma_] := Module[
  {i, plaintext, plaintext2, ciphertext1, ciphertext2, bit1, bit2, z,
   conteggioZero = 0, conteggioUno = 0},

  For[i = 1, i <= N, i++,
    plaintext = RandomInteger[{0, 2^64 - 1}];
    plaintext2 = BitXor[plaintext, delta];

    ciphertext1 = EncryptPRESENT[plaintext, key, 9, False];
    ciphertext2 = EncryptPRESENT[plaintext2, key, 9, False];

    bit1 = Parity[BitAnd[ciphertext1, gamma]];
    bit2 = Parity[BitAnd[ciphertext2, gamma]];

    z = BitXor[bit1, bit2];

    If[z == 0, conteggioZero++, conteggioUno++];
  ];

  CalcolaStatisticheDL[conteggioZero, conteggioUno]
];

(* Maschera casuale a 64 bit con peso w (numero di bit a 1 esattamente w) 
per fare test con delta e gamma casuali *)
MascheraCasualePeso[w_] := Module[{posizioni},
  posizioni = RandomSample[Range[0, 63], w];
  Total[BitShiftLeft[1, #] & /@ posizioni]
];


(* ============================== *)
(* TEST                           *)
(* ============================== *)

(* Test su Parity *)
Print["Test Parity: ", {Parity[0], Parity[1], Parity[3], Parity[7]}];

(* Test  su CalcolaStatisticheDL *)
Print["Test Statistiche (520/480): ", CalcolaStatisticheDL[520, 480]];

(* Chiave fissata a 0 per semplicit\[AGrave] (si pu\[OGrave] usare anche una chiave casuale) *)
chiave = 16^^00000000000000000000;

Print["--- Esperimento PAPER (DeltaPaper, GammaPaper) ---"];
Print[Table[EsperimentoDL9[chiave, 25000, DeltaPaper, GammaPaper], {3}]];

Print["--- Esperimento RANDOM (delta e gamma casuali, gamma peso 3) ---"];
Print[
  Table[
    EsperimentoDL9[
      chiave,
      25000,
      RandomInteger[{0, 2^64 - 1}],
      MascheraCasualePeso[3]
    ],
    {3}
  ]
];

Print["--- Esperimento RANDOM (delta e gamma completamente casuali) ---"];
Print[
  Table[
    EsperimentoDL9[
      chiave,
      25000,
      RandomInteger[{0, 2^64 - 1}],
      RandomInteger[{0, 2^64 - 1}]
    ],
    {3}
  ]
];

