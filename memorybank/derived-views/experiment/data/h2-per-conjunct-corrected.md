| schema | conjunct | rule | tables or views it reads | N = 1000 (ms) | N = 4000 (ms) | N = 16000 (ms) | N = 64000 (ms) |
| --- | --- | --- | --- | ---: | ---: | ---: | ---: |
| stored | conj_0 | Compute7183785395459209921 | stored tables | 1.5 | 6.4 | 26.6 | 120.4 |
| stored | conj_24 | Compute6553865532586864385 | stored tables | 2.9 | 12.3 | 52.0 | 256.9 |
| stored | conj_25 | DoorsnedeInR | stored tables | 0.8 | 3.2 | 14.2 | 59.1 |
| stored | conj_26 | BereikViaVoorouder | stored tables | 117.9 | 462.1 | 1 834.1 | 9 023.2 |
| stored | conj_27 | VoorouderTransitief | stored tables | 32.1 | 109.6 | 407.8 | 2 764.8 |
| stored | conj_29 | Compute1606628096763440212 | stored tables | 35.1 | 254.2 | 1 229.2 | 7 159.9 |
| stored | conj_47 | Compute1264232570234658788 | stored tables | 0.7 | 2.7 | 10.8 | 49.6 |
| stored | conj_48 | Compute3592443640697477715 | stored tables | 1.0 | 4.2 | 17.7 | 79.0 |
| stored | conj_49 | Compute1112769180579683618 | stored tables | 1.3 | 5.5 | 23.9 | 107.5 |
| stored | conj_50 | VerschilInVereniging | stored tables | 0.8 | 3.1 | 13.3 | 58.7 |
| stored | conj_51 | Compute406751365419016596 | stored tables | 0.3 | 1.0 | 4.3 | 18.0 |
| stored | conj_52 | Compute3928429934757163150 | stored tables | 0.2 | 1.0 | 4.1 | 17.3 |
| stored | conj_53 | Compute8803521138298286776 | stored tables | 5.4 | 23.5 | 101.1 | 512.2 |
| stored | conj_54 | Compute4202463742555561688 | stored tables | 1.4 | 5.8 | 27.7 | 108.4 |
| stored | conj_55 | Compute3857801034329956019 | stored tables | 0.5 | 2.1 | 8.8 | 38.6 |
| stored | conj_56 | Compute3355456426292665871 | stored tables | 106.4 | 604.7 | 3 042.5 | 15 985.4 |
| views | conj_22 | DoorsnedeInR | ri | 2.3 | 8.8 | 53.9 | 166.9 |
| views | conj_23 | BereikViaVoorouder | rs, voorouder | 53 975.5 | 60 000.0 | 60 000.0 | 60 000.0 |
| views | conj_24 | VoorouderTransitief | voorouder | 22 090.0 | 60 000.0 | 60 000.0 | 60 000.0 |
| views | conj_40 | VerschilInVereniging | rd | 101.6 | 1 617.6 | 28 151.6 | 60 000.0 |
