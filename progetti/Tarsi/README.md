Questo progetto implementa l'attacco Boomerang al cifrario PRESENT (64-bit block, 80-bit key) su 9 rounds, seguendo il paper:
https://eprint.iacr.org/2024/255
Il codice implementa il cifrario PRESENT (S-box, permutazione, key schedule) e un esperimento statistico che genera coppie di plaintext P e P ⊕ Δ, 
cifra entrambe e misura la parità dei ciphertext tramite una maschera lineare Γ, dove Δ e Γ sono dati dalla tabella presente nel paper.
L'esperimento mostra lo sbilanciamento statistico previsto dal distinguisher su 9 round. 
