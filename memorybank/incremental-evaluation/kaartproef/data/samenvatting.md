## Wandkloktijd per aanroep, zonder tracing (mediaan in ms)

| N | soort | off | off+skip | shadow | on | on+skip |
|---|---|---|---|---|---|---|
| 250 | stempel | 209 (n=20) | 171 (n=20) | 300 (n=20) | 264 (n=20) | 180 (n=20) |
| 250 | afhankelijkheid | 870 (n=18) | 632 (n=18) | 1059 (n=18) | 991 (n=18) | 642 (n=18) |
| 500 | stempel | 389 (n=10) | 298 (n=10) | 557 (n=10) | 465 (n=10) | 308 (n=10) |
| 500 | afhankelijkheid | 2413 (n=10) | 1627 (n=10) | 2799 (n=10) | 2731 (n=10) | 1651 (n=10) |
| 1000 | stempel | 666 (n=10) | 584 (n=10) | 1064 (n=10) | 863 (n=10) | 490 (n=10) |
| 1000 | afhankelijkheid | 8628 (n=10) | 5813 (n=10) | 9355 (n=10) | 9058 (n=10) | 5770 (n=10) |

## Schaduwstand: vergelijkingen van delta en volledig

- N=250: 482 identiek, 0 mismatch
- N=250 (met tracing): 482 identiek, 0 mismatch
- N=500: 244 identiek, 0 mismatch
- N=500 (met tracing): 244 identiek, 0 mismatch
- N=1000: 269 identiek, 0 mismatch
- N=1000 (met tracing): 269 identiek, 0 mismatch

## Tijdverdeling per aanroep, met tracing (mediaan in ms)

| N | soort | stand | totaal | app init | session init | execengine run | transaction close | conj_in_ee | conj_in_close | conj_in_ee_n | conj_in_close_n | sql_n |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 250 | stempel | off | 227 | 39 | 7 | 91 | 60 | 86 | 50 | 24 | 51 | 200 |
| 250 | afhankelijkheid | off | 897 | 36 | 7 | 589 | 250 | 581 | 241 | 36 | 53 | 201 |
| 250 | stempel | off+skip | 178 | 36 | 6 | 90 | 16 | 87 | 8 | 24 | 42 | 191 |
| 250 | afhankelijkheid | off+skip | 660 | 37 | 6 | 578 | 21 | 573 | 12 | 36 | 40 | 188 |
| 250 | stempel | shadow | 320 | 37 | 7 | 91 | 167 | 86 | 49 | 24 | 51 | 330 |
| 250 | afhankelijkheid | shadow | 1117 | 37 | 7 | 590 | 438 | 580 | 248 | 36 | 53 | 292 |
| 250 | stempel | on | 280 | 37 | 7 | 94 | 114 | 88 | 7 | 24 | 34 | 278 |
| 250 | afhankelijkheid | on | 1050 | 37 | 7 | 602 | 364 | 591 | 178 | 36 | 44 | 264 |
| 250 | stempel | on+skip | 183 | 37 | 7 | 91 | 22 | 86 | 6 | 24 | 31 | 255 |
| 250 | afhankelijkheid | on+skip | 647 | 36 | 7 | 569 | 24 | 563 | 11 | 36 | 34 | 242 |
| 500 | stempel | off | 379 | 38 | 8 | 168 | 92 | 154 | 81 | 24 | 51 | 228 |
| 500 | afhankelijkheid | off | 2387 | 38 | 6 | 1547 | 773 | 1538 | 760 | 32 | 53 | 215 |
| 500 | stempel | off+skip | 292 | 37 | 8 | 161 | 17 | 149 | 9 | 24 | 42 | 218 |
| 500 | afhankelijkheid | off+skip | 1612 | 37 | 6 | 1530 | 25 | 1519 | 16 | 32 | 40 | 202 |
| 500 | stempel | shadow | 610 | 36 | 11 | 161 | 325 | 148 | 79 | 24 | 51 | 386 |
| 500 | afhankelijkheid | shadow | 2752 | 37 | 7 | 1541 | 1099 | 1526 | 751 | 32 | 53 | 313 |
| 500 | stempel | on | 453 | 38 | 9 | 159 | 177 | 144 | 8 | 24 | 34 | 334 |
| 500 | afhankelijkheid | on | 2623 | 36 | 7 | 1537 | 982 | 1516 | 637 | 32 | 44 | 285 |
| 500 | stempel | on+skip | 299 | 38 | 11 | 161 | 25 | 147 | 6 | 24 | 31 | 310 |
| 500 | afhankelijkheid | on+skip | 1636 | 36 | 7 | 1541 | 27 | 1530 | 14 | 32 | 34 | 261 |
| 1000 | stempel | off | 686 | 39 | 8 | 375 | 207 | 372 | 196 | 24 | 51 | 222 |
| 1000 | afhankelijkheid | off | 8732 | 37 | 7 | 5776 | 2862 | 5762 | 2847 | 32 | 53 | 214 |
| 1000 | stempel | off+skip | 505 | 38 | 8 | 388 | 19 | 381 | 10 | 24 | 42 | 212 |
| 1000 | afhankelijkheid | off+skip | 5899 | 37 | 6 | 5794 | 37 | 5785 | 25 | 32 | 40 | 201 |
| 1000 | stempel | shadow | 1076 | 43 | 11 | 386 | 589 | 382 | 197 | 24 | 51 | 372 |
| 1000 | afhankelijkheid | shadow | 9556 | 38 | 7 | 5829 | 3584 | 5807 | 2868 | 32 | 53 | 322 |
| 1000 | stempel | on | 885 | 38 | 11 | 377 | 410 | 372 | 12 | 24 | 34 | 320 |
| 1000 | afhankelijkheid | on | 9159 | 37 | 8 | 5753 | 3292 | 5738 | 2581 | 32 | 44 | 294 |
| 1000 | stempel | on+skip | 501 | 39 | 9 | 374 | 25 | 368 | 8 | 24 | 31 | 298 |
| 1000 | afhankelijkheid | on+skip | 5876 | 36 | 7 | 5767 | 42 | 5758 | 23 | 32 | 34 | 270 |

## Duurste conjuncts per aanroep (mediaan in ms over de aanroepen waarin de conjunct in de top zes stond)

- N=250, afhankelijkheid, off: conj_182 537 ms (18x, recursive, geen delta, ['Compute1620968772356522804']); conj_203 205 ms (18x, scan, delta, ['Compute8102290529258527276']); conj_135 17 ms (18x, recursive, geen delta, ['Compute7199776112205604801']); conj_10 11 ms (18x, scan, delta, ['Compute1702054088406944109']); conj_28 8 ms (1x, scan, delta, ['Compute8178624393589622171'])
- N=250, afhankelijkheid, on: conj_182 546 ms (18x, recursive, geen delta, ['Compute1620968772356522804']); conj_203 147 ms (18x, scan, delta, ['Compute8102290529258527276']); conj_135 17 ms (18x, recursive, geen delta, ['Compute7199776112205604801']); conj_10 7 ms (18x, scan, delta, ['Compute1702054088406944109']); conj_28 6 ms (1x, scan, delta, ['Compute8178624393589622171'])
- N=250, stempel, off: conj_203 109 ms (19x, scan, delta, ['Compute8102290529258527276']); conj_10 9 ms (19x, scan, delta, ['Compute1702054088406944109']); conj_226 5 ms (20x, scan, delta, ['Compute6374799154260412689']); conj_220 2 ms (20x, scan, delta, ['Compute3612212633974545590']); conj_205 1 ms (19x, scan, delta, ['Compute4281394834202854732'])
- N=250, stempel, on: conj_203 76 ms (19x, scan, delta, ['Compute8102290529258527276']); conj_10 6 ms (19x, scan, delta, ['Compute1702054088406944109']); conj_226 5 ms (20x, scan, delta, ['Compute6374799154260412689']); conj_220 2 ms (20x, scan, delta, ['Compute3612212633974545590']); conj_205 1 ms (18x, scan, delta, ['Compute4281394834202854732'])
- N=500, afhankelijkheid, off: conj_182 1872 ms (10x, recursive, geen delta, ['Compute1620968772356522804']); conj_203 346 ms (10x, scan, delta, ['Compute8102290529258527276']); conj_135 46 ms (10x, recursive, geen delta, ['Compute7199776112205604801']); conj_10 27 ms (10x, scan, delta, ['Compute1702054088406944109']); conj_187 7 ms (10x, scan, delta, ['Compute5161379235318087746'])
- N=500, afhankelijkheid, on: conj_182 1845 ms (10x, recursive, geen delta, ['Compute1620968772356522804']); conj_203 229 ms (10x, scan, delta, ['Compute8102290529258527276']); conj_135 40 ms (10x, recursive, geen delta, ['Compute7199776112205604801']); conj_10 17 ms (10x, scan, delta, ['Compute1702054088406944109']); conj_187 7 ms (10x, scan, delta, ['Compute5161379235318087746'])
- N=500, stempel, off: conj_203 230 ms (8x, scan, delta, ['Compute8102290529258527276']); conj_10 16 ms (8x, scan, delta, ['Compute1702054088406944109']); conj_226 8 ms (10x, scan, delta, ['Compute6374799154260412689']); conj_220 2 ms (10x, scan, delta, ['Compute3612212633974545590']); conj_205 1 ms (8x, scan, delta, ['Compute4281394834202854732'])
- N=500, stempel, on: conj_203 149 ms (8x, scan, delta, ['Compute8102290529258527276']); conj_10 11 ms (8x, scan, delta, ['Compute1702054088406944109']); conj_226 8 ms (10x, scan, delta, ['Compute6374799154260412689']); conj_90 2 ms (1x, structural, delta, ['UNI4647532235993835343']); conj_220 2 ms (10x, scan, delta, ['Compute3612212633974545590'])
- N=1000, afhankelijkheid, off: conj_182 7609 ms (10x, recursive, geen delta, ['Compute1620968772356522804']); conj_203 613 ms (10x, scan, delta, ['Compute8102290529258527276']); conj_135 217 ms (10x, recursive, geen delta, ['Compute7199776112205604801']); conj_10 136 ms (10x, scan, delta, ['Compute1702054088406944109']); conj_187 13 ms (10x, scan, delta, ['Compute5161379235318087746'])
- N=1000, afhankelijkheid, on: conj_182 7551 ms (10x, recursive, geen delta, ['Compute1620968772356522804']); conj_203 399 ms (10x, scan, delta, ['Compute8102290529258527276']); conj_135 216 ms (10x, recursive, geen delta, ['Compute7199776112205604801']); conj_10 91 ms (10x, scan, delta, ['Compute1702054088406944109']); conj_187 13 ms (10x, scan, delta, ['Compute5161379235318087746'])
- N=1000, stempel, off: conj_203 406 ms (9x, scan, delta, ['Compute8102290529258527276']); conj_10 131 ms (9x, scan, delta, ['Compute1702054088406944109']); conj_226 15 ms (10x, scan, delta, ['Compute6374799154260412689']); conj_220 2 ms (10x, scan, delta, ['Compute3612212633974545590']); conj_62 1 ms (2x, structural, delta, ['UNI1246886053602533902'])
- N=1000, stempel, on: conj_203 267 ms (9x, scan, delta, ['Compute8102290529258527276']); conj_129 245 ms (1x, structural, delta, ['UNI7186331288433972065']); conj_10 86 ms (9x, scan, delta, ['Compute1702054088406944109']); conj_226 14 ms (10x, scan, delta, ['Compute6374799154260412689']); conj_220 2 ms (10x, scan, delta, ['Compute3612212633974545590'])
