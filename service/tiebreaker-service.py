from pyswip import Prolog
from pyeda.inter import *


def flatten(container):
    for i in container:
        if isinstance(i, (list, tuple)):
            for j in flatten(i):
                yield j
        else:
            yield i


class Tiebreaker:
    def __init__(self) -> None:
        self.prolog_script_path = "../tiebreaker/run.pl"
        self.prolog = Prolog()

    def get_solutions(self) -> None:
        self.prolog.consult(
            "~/Code/logic/tiebreaker-logic/tiebreaker/tiebreaker")
        standings = self.prolog.query("standings(S, Rs), maplist(label, Rs)")
        standing_dict = {}
        for standing in standings:
            flattened_standing = flatten(standing["S"])
            standing_tuple = tuple(flattened_standing)
            data = [standing["S"], standing["Rs"]]
            if standing_tuple in standing_dict:
                standing_dict[standing_tuple].append(data)
            else:
                standing_dict[standing_tuple] = [data]

        print(standing_dict)


Tiebreaker().get_solutions()
