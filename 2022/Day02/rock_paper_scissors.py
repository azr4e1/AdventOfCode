import pandas as pd

# === First Part ===
strategy_guide = pd.read_csv("./input.csv")

type_points = {
        "A": 1,
        "B": 2,
        "C": 3,
        'X': 1,
        'Y': 2,
        'Z': 3
        }

winning_rule = {
        ('A', 'X'): 3,
        ('B', 'Y'): 3,
        ('C', 'Z'): 3,
        ('A', 'Y'): 6,
        ('B', 'Z'): 6,
        ('C', 'X'): 6,
        ('A', 'Z'): 0,
        ('B', 'X'): 0,
        ('C', 'Y'): 0,
        }

opponent = strategy_guide['Opponent'].to_list()
me = strategy_guide['Me'].to_list()

combinations = list(zip(opponent, me))

points = []
for round in combinations:
    result = winning_rule[round]
    type_played = type_points[round[1]]

    round_points = result + type_played
    points.append(round_points)

print("Total points according to strategy guide:")
print(sum(points))

# === Second Part === 

wins = {
        'A': 'Z',
        'B': 'X',
        'C': 'Y'
        }

ties = {
        'A': 'X',
        'B': 'Y',
        'C': 'Z'
        }

loses = {

        'A': 'Y',
        'B': 'Z',
        'C': 'X'
        }

strategy = {
        'X': wins,
        'Y': ties,
        'Z': loses
        }

my_moves = [strategy[result][their_move] for their_move, result in combinations]

actual_combinations = list(zip(opponent, my_moves))
actual_points = []
for round in actual_combinations:
    result = winning_rule[round]
    type_played = type_points[round[1]]

    round_points = result + type_played
    actual_points.append(round_points)

print("Total points according to strategy guide for real this time:")
print(sum(actual_points))
