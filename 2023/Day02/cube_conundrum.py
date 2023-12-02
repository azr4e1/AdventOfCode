# --- Day 2: Cube Conundrum ---

# You're launched high into the atmosphere! The apex of your trajectory just
# barely reaches the surface of a large island floating in the sky. You
# gently land in a fluffy pile of leaves. It's quite cold, but you don't see
# much snow. An Elf runs over to greet you.

# The Elf explains that you've arrived at Snow Island and apologizes for the
# lack of snow. He'll be happy to explain the situation, but it's a bit of a
# walk, so you have some time. They don't get many visitors up here; would
# you like to play a game in the meantime?

# As you walk, the Elf shows you a small bag and some cubes which are either
# red, green, or blue. Each time you play this game, he will hide a secret
# number of cubes of each color in the bag, and your goal is to figure out
# information about the number of cubes.

# To get information, once a bag has been loaded with cubes, the Elf will
# reach into the bag, grab a handful of random cubes, show them to you, and
# then put them back in the bag. He'll do this a few times per game.

# You play several games and record the information from each game (your
# puzzle input). Each game is listed with its ID number (like the 11 in Game
# 11: ...) followed by a semicolon-separated list of subsets of cubes that
# were revealed from the bag (like 3 red, 5 green, 4 blue).

# For example, the record of a few games might look like this:

# Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
# Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
# Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
# Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
# Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

# In game 1, three sets of cubes are revealed from the bag (and then put
# back again). The first set is 3 blue cubes and 4 red cubes; the second set
# is 1 red cube, 2 green cubes, and 6 blue cubes; the third set is only 2
# green cubes.

# The Elf would first like to know which games would have been possible if
# the bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes?

# In the example above, games 1, 2, and 5 would have been possible if the
# bag had been loaded with that configuration. However, game 3 would have
# been impossible because at one point the Elf showed you 20 red cubes at
# once; similarly, game 4 would also have been impossible because the Elf
# showed you 15 blue cubes at once. If you add up the IDs of the games that
# would have been possible, you get 8.

# Determine which games would have been possible if the bag had been loaded
# with only 12 red cubes, 13 green cubes, and 14 blue cubes. What is the sum
# of the IDs of those games?

from __future__ import annotations
from typing import TypedDict
from dataclasses import dataclass, fields


# First Puzzle
class GameConfiguration(TypedDict):
    red: int
    green: int
    blue: int


@dataclass
class GameDraw:
    red: int = 0
    green: int = 0
    blue: int = 0


class Game:
    def __eq__(self, other_game: Game) -> bool:
        return self.configuration == other_game.configuration

    def __init__(self, configuration: GameConfiguration) -> None:
        self.configuration = configuration

    def _single_check(self, draw: GameDraw) -> int:
        colors = [f.name for f in fields(draw)]
        for color in colors:
            if getattr(draw, color) > self.configuration.get(color, 0):
                return 0
        return 1

    def check(self, draws: list[GameDraw]) -> bool:
        possible = 1
        for draw in draws:
            possible *= self._single_check(draw)

        return bool(possible)

    def power(self) -> int:
        val = 1
        for el in self.configuration.values():
            val *= el

        return val


class GameReader:
    def __init__(self, line: str) -> None:
        self.line = line
        self.draws = self._parse()

    def _parse_into_color(self, draw: str) -> GameDraw:
        colors = [color.strip() for color in draw.split(",")]
        parsed_color = {}
        for color in colors:
            color_nr, color_name = tuple(color.split(" "))
            match color_name:
                case "red":
                    parsed_color['red'] = int(color_nr)
                case 'green':
                    parsed_color['green'] = int(color_nr)
                case 'blue':
                    parsed_color['blue'] = int(color_nr)

        return GameDraw(**parsed_color)

    def _parse(self) -> list[GameDraw]:
        draws = self.line.split(':')[-1].strip()
        games = [self._parse_into_color(game.strip())
                 for game in draws.split(';')]

        return games

    def valid_configuration(self) -> Game:
        min_config: GameConfiguration = {'red': 0,
                                         'green': 0,
                                         'blue': 0}
        for draw in self.draws:
            min_config['red'] = max(min_config['red'], draw.red)
            min_config['green'] = max(min_config['green'], draw.green)
            min_config['blue'] = max(min_config['blue'], draw.blue)

        return Game(min_config)


# Second Puzzle
# The Elf says they've stopped producing snow because they aren't getting
# any water! He isn't sure why the water stopped; however, he can show you
# how to get to the water source to check it out for yourself. It's just up
# ahead!

# As you continue your walk, the Elf poses a second question: in each game
# you played, what is the fewest number of cubes of each color that could
# have been in the bag to make the game possible?

# Again consider the example games from earlier:

# Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
# Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
# Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
# Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
# Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

#     In game 1, the game could have been played with as few as 4 red, 2
# green, and 6 blue cubes. If any color had even one fewer cube, the game
# would have been impossible.
#     Game 2 could have been played with a minimum of 1 red, 3 green, and 4
# blue cubes.
#     Game 3 must have been played with at least 20 red, 13 green, and 6
# blue cubes.
#     Game 4 required at least 14 red, 3 green, and 15 blue cubes.
#     Game 5 needed no fewer than 6 red, 3 green, and 2 blue cubes in the
# bag.

# The power of a set of cubes is equal to the numbers of red, green, and
# blue cubes multiplied together. The power of the minimum set of cubes in
# game 1 is 48. In games 2-5 it was 12, 1560, 630, and 36, respectively.
# Adding up these five powers produces the sum 2286.

# For each game, find the minimum set of cubes that must have been present.
# What is the sum of the power of these sets?

if __name__ == "__main__":
    games = {}
    configuration: GameConfiguration = {'red': 12,
                                        'green': 13,
                                        'blue': 14}
    with open("./input.txt") as f:
        for index, line in enumerate(f):
            reader = GameReader(line)
            game = Game(configuration)
            games[index+1] = game.check(reader.draws)

    possible_games = filter(lambda x: x[1], games.items())
    possible_games_index = [x[0] for x in possible_games]

    print("The sum of the IDs of the possible games is:",
          sum(possible_games_index))

    games = []
    with open("./input.txt") as f:
        for line in f:
            reader = GameReader(line)
            game = reader.valid_configuration()
            games.append(game.power())

    print("The sum of the power of the minimum games is:", sum(games))
