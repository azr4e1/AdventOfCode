from typing import TypeAlias
from itertools import cycle, product


Network: TypeAlias = dict[str, dict[str, str]]


class Navigator:
    def __init__(self, directions: list[str], network: Network) -> None:
        self.directions = directions
        self.network = network

    def navigate(self) -> list[str]:
        steps: list[str] = ["AAA"]
        for direction in cycle(self.directions):
            prev_node = steps[-1]
            next_node = self.network[prev_node][direction]
            steps.append(next_node)
            if next_node == "ZZZ":
                break

        self._steps = steps

    @property
    def steps(self) -> list[str]:
        if not hasattr(self, "_steps"):
            raise ValueError("Navigate the network first")
        return self._steps


class GhostlyNavigator:
    def __init__(self, directions: list[str], network: Network) -> None:
        self.directions = directions
        self.network = network

    def _single_navigator(self, starting_node: str) -> list[str]:
        steps: list[str] = [starting_node]
        for direction in cycle(self.directions):
            prev_node = steps[-1]
            next_node = self.network[prev_node][direction]
            steps.append(next_node)
            if next_node.endswith("Z"):
                break

        return steps

    def _navigate(self) -> int:
        prev_nodes = list(filter(
            lambda x: x.endswith("A"), self.network.keys()))
        # get steps it takes to move from A to Z
        solutionsA = []
        for node in prev_nodes:
            solutionsA.append(self._single_navigator(node))

        # get steps it takes to move from Z to Z
        prev_nodes = list(filter(
            lambda x: x.endswith("Z"), self.network.keys()))
        solutionsZ = []
        for node in prev_nodes:
            solutionsZ.append(self._single_navigator(node))

        # check start and end are the same for Z paths
        startZ = []
        endZ = []
        for path in solutionsZ:
            startZ.append(path[0])
            endZ.append(path[-1])

        initial_steps = list(map(lambda x: len(x)-1, solutionsA))
        zToZ_steps = list(map(lambda x: len(x)-1, solutionsZ))

        assert startZ == endZ

        momentum = 1
        while True:
            length = initial_steps[0] + momentum * zToZ_steps[0]
            print(length, end="\r")
            for i in range(1, len(initial_steps)):
                check = (length - initial_steps[i]) % zToZ_steps[i]
                if check != 0:
                    break
            else:
                break
            momentum += 1

        self._steps = length

    def navigate(self):
        pass

    @property
    def steps(self) -> int:
        if not hasattr(self, "_steps"):
            raise ValueError("Navigate the network first")
        return self._steps
