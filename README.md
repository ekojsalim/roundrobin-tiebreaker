# Round-robin Tiebreaker

Depends on `scryer-prolog`. Install [it](https://github.com/mthom/scryer-prolog) first or adjust dependencies accordingly.

To install scryer-prolog in windows, use wsl2, run the below commands.

```
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
cargo install --git https://github.com/mthom/scryer-prolog
```

## Problem Description

The problem below is adapted from ESL Pro Tour of Counter Strike: Global Offensive Tournament group stage tiebreaking rules, as stated in https://cdn.eslgaming.com/misc/media/lo/ESL%20Pro%20Tour%20-%20CSGO%20Game%20Specific%20Rules.pdf. There are some predictions done by the fans to predict who will qualify from the group stage, and this inspires us to take this problem's solver as our group project https://twitter.com/StrikerHLTVorg/status/1505310228426166272, https://www.hltv.org/events/6137/esl-pro-league-season-15, which we think suits the logic paradigm programming.

This problem is based on a group stage tournament matching tiebreaking mechanism. Group stage is used to rank **n** teams, based on their performance. Define the teams are in set **S**, each **n** teams will compete against each other. In total, there will be **C(n, 2)** match conducted, (Best of 3 Rounds) between those **n** teams. For example, if there are **6** teams, then there will be **15** matches.

Each match consists of **at most 3 rounds**, in one match of team **x** and team **y**, there will be **4** possible results, define the results of those rounds won by x : y as:

- 2 : 1 (**x wins**, and gains 3 points in the group stage)
- 2 : 0 (**x wins**, and gains 3 points in the group stage)
- 0 : 2 (**y wins**, and gains 3 points in the group stage)
- 1 : 2 (**y wins**, and gains 3 points in the group stage)

The teams then will be sorted based on their total points gained from the group stage. But, a tie will happen on most of the case. So, a tiebreaking rules are needed. If a number of teams are tied by points at the end of a group stage, their ranking order will be decided as described below.

If after any point from **1. to 5.**, the number of tied teams is **reduced or divided into several groups of teams**, the still tied teams will in each case be compared again starting from the first point of the tiebreaking rules. Let's define a set **T** which contains **m** tied teams.

1. Points amassed between the tied teams (direct match win > direct match loss)
   - Each team's points will be recounted based only on the set **T**, not on the whole set **S**. Let's define a new score **p<sub>t</sub>** for a team **t** in **T**. **p<sub>t</sub> = sum of the points gained only from winning against other team in the set T** . The tied teams then will be sorted by the new score system **p**.
   - If another tie occurs, the tied teams will be separated and compared again starting from the first point of the tiebreaking rules.
2. Round difference between the tied teams (3:2 rounds > 3:3 rounds)
   - Each team's points will be recounted based only on the set **T**, not on the whole set **S**. Let's define a new score **p<sub>t</sub>** for a team **t** in **T**. **p<sub>t</sub> = sum of the round difference against other team in the set T**. Pay attention that a team will get negative score if the match is a lost for them, and will get positive score if the match is a win for them, with the amount of score is the round difference between each team. The tied teams then will be sorted by the new score system **p**.
   - If another tie occurs, the tied teams will be separated and compared again starting from the first point of the tiebreaking rules.
3. Number of round wins between the tied teams (3:3 rounds > 2:2 rounds)
   - This will rarely happen, each team's points will be recounted based only on the set **T**, not on the whole set **S**. Let's define a new score **p<sub>t</sub>** for a team **t** in **T**. **p<sub>t</sub> = sum of the round win against other team in the set T**. The tied teams then will be sorted by the new score system **p**.
   - If another tie occurs, the tied teams will be separated and compared again starting from the first point of the tiebreaking rules.
4. Overall round difference
   - This will rarely happen, each tied team's points will be recounted based on the whole set **S**. Let's define a new score **p<sub>t</sub>** for a team **t** in **T**. **p<sub>t</sub> = sum of the round difference against other team in the set S**. The tied teams then will be sorted by the new score system **p**.
   - If another tie occurs, the tied teams will be separated and compared again starting from the first point of the tiebreaking rules.
5. Overall number of round wins
   - This will rarely happen, each tied team's points will be recounted based on the whole set **S**. Let's define a new score **p<sub>t</sub>** for a team **t** in **T**. **p<sub>t</sub> = sum of the round win against other team in the set S**. The tied teams then will be sorted by the new score system **p**.
   - If another tie occurs, the solver shall return false, which mean the tiebreaking system fails.

For a sketch illustration of the first and second tiebreak rule:

![img](README.assets/tiebreak.png)

