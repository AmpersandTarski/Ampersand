#!/usr/bin/env python3
"""Maakt een verzonnen populatie voor de kaartproef: één project met N documenten in lagen.

Elk document in laag l steunt op K documenten van laag l-1, via een Afhankelijkheid waarvan
`gezien` gelijk is aan de stempel van de bron. De populatie is dus overtredingsvrij op
`verouderd`, en `bereikt` (:= hangtAf+) groeit ruwweg lineair met N bij een vaste diepte.

Gebruik: zaai.py N [--lagen 5] [--k 2] [--zaad 1] > populatie.json
"""
import argparse
import json
import random

p = argparse.ArgumentParser()
p.add_argument("n", type=int)
p.add_argument("--lagen", type=int, default=5)
p.add_argument("--k", type=int, default=2)
p.add_argument("--zaad", type=int, default=1)
a = p.parse_args()
rnd = random.Random(a.zaad)

PROJECT = "Proef"
per_laag = a.n // a.lagen
docs = [[f"{PROJECT}.d{l}.{i:05d}" for i in range(per_laag)] for l in range(a.lagen)]
alle = [d for laag in docs for d in laag]
stempel = {d: f"s0.{d}" for d in alle}

links = {
    "naam[Artefact*Naam]": [(d, f"Document {d}") for d in alle],
    "hoortBij[Artefact*Project]": [(d, PROJECT) for d in alle],
    "stempel[Artefact*Stempel]": [(d, stempel[d]) for d in alle],
    "van[Afhankelijkheid*Artefact]": [],
    "op[Afhankelijkheid*Artefact]": [],
    "gezien[Afhankelijkheid*Stempel]": [],
    "reden[Afhankelijkheid*Tekst]": [],
}
afh = []
for l in range(1, a.lagen):
    for d in docs[l]:
        for bron in rnd.sample(docs[l - 1], min(a.k, per_laag)):
            x = f"afh.{d}.{bron.split('.', 1)[1]}"
            afh.append(x)
            links["van[Afhankelijkheid*Artefact]"].append((x, d))
            links["op[Afhankelijkheid*Artefact]"].append((x, bron))
            links["gezien[Afhankelijkheid*Stempel]"].append((x, stempel[bron]))
            links["reden[Afhankelijkheid*Tekst]"].append((x, "verzonnen voor de proef"))

pop = {
    "atoms": [
        {"concept": "Project", "atoms": [PROJECT]},
        {"concept": "Document", "atoms": alle},
        {"concept": "Afhankelijkheid", "atoms": afh},
    ],
    "links": [{"relation": r, "links": [{"src": s, "tgt": t} for s, t in ps]} for r, ps in links.items()],
}
print(json.dumps(pop, ensure_ascii=False))
