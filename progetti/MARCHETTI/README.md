# Implementazione e Analisi del Cifrario Ascon128

Questa cartella contiene tre file sviluppati per lo studio e l’implementazione del cifrario **Ascon128**.

## Contenuto

- **AsconPrimitives.wl**  
  Contiene le funzioni di base e le primitive necessarie per l’implementazione del cifrario Ascon128, incluse le operazioni fondamentali di permutazione e manipolazione dei bit.

- **AsconEnc_Dec.wl**  
  Comprende le funzioni di **encryption** e **decryption** per Ascon128, costruite a partire dalle primitive definite nel file precedente.  
  L’obiettivo è fornire un’implementazione completa del cifrario per la cifratura e decifratura di messaggi.

- **BCTPropagation.wl**  
  Contiene un tentativo di **crittanalisi** del cifrario Ascon128 basato sull’utilizzo della **Boomerang Connectivity Table (BCT)**.  
  L’analisi mira a esplorare la propagazione delle differenze attraverso tre round.

## Requisiti

I file sono scritti in **Wolfram Language (.wl)** e sono quindi eseguibili in **Mathematica** o in ambienti compatibili.

---

