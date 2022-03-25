from ast import literal_eval
from unittest import result
from quine_mccluskey.qm import QuineMcCluskey

# td = {
#     1: "ence",
#     2: "faze",
#     3: "furia",
#     4: "outsiders",
#     5: "sprout",
#     6: "vitality"
# }

td = {
    1: "BIG",
    2: "GODSENT",
    3: "Liquid",
    4: "Movistar Riders",
    5: "Party Astronauts",
    6: "Players"
}


with open("output.txt") as f:
    results = literal_eval(f.read())

print(results[0])

# generate positions for variables
lts = [(r[:2]) for r in results[0]]

l = []
for solution in results:
    bin = ""
    for term in solution:
        bin += "1" if term[2] == term[0] else "0"
        bin += "1" if term[-2:] == [2, 0] else "0"
    l.append(bin)

# print(l)

qm = QuineMcCluskey()
simplification = qm.simplify_los(l)
print(simplification)

final = []
for s in simplification:
    human_readable = []
    for ts, win, score in zip(lts, s[0::2], s[1::2]):
        if win != "-":
            t1, t2 = ts
            winner = td[t1] if win == "1" else td[t2]
            loser  = td[t2] if win == "1" else td[t1]
            hr = f"{winner} beats {loser}"
            if score != "-":
                hr += f" {'in 2 maps' if score == '1' else 'in 3 maps'}"
            human_readable.append(hr)
            continue
        if score != "-":
            t1, t2 = ts
            human_readable.append(f"{td[t1]} vs {td[t2]} is a {'2-map series' if score == '1' else '3-map series'}")
    # print(human_readable)
    final.append(" & ".join(human_readable))

for i, p in enumerate(final):
    print(f"{i + 1}. ", p)