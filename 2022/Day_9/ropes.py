import numpy as np

lines = []
with open('./input.txt') as f:
    for line in f:
        lines.append(line.strip())


# === First Part ===
class Tail:
    def __init__(self, height, width):
        self.height = height
        self.width = width
        self.grid = np.zeros(shape=(height, width)).astype(int)
        self.grid[height//2, width//2] = 1
        self.rel_pos_head = [0, 0]
        self.tail_pos = [height//2, width//2]

    def pull_rope(self, move):
        if isinstance(move, str):
            direction, magnitude = move.split(' ')
            prev_tail_pos = self.tail_pos.copy()
            for mag in range(int(magnitude)):
                if direction == "D":
                    self.rel_pos_head[1] += 1
                elif direction == "U":
                    self.rel_pos_head[1] -= 1
                elif direction == "R":
                    self.rel_pos_head[0] += 1
                elif direction == "L":
                    self.rel_pos_head[0] -= 1
                tail_move = self._move_tail()
                self._update_grid(tail_move)
        elif isinstance(move, list):
            prev_tail_pos = self.tail_pos.copy()
            self.rel_pos_head[0] += move[0]
            self.rel_pos_head[1] += move[1]
            tail_move = self._move_tail()
            self._update_grid(tail_move)
            # if move[0] != 0:
            #     for increment in range(int(abs(move[0]))):
            #         self.rel_pos_head[0] += abs(move[0]) / move[0]
            #         tail_move = self._move_tail()
            #         self._update_grid(tail_move)

            # if move[1] != 0:
            #     for increment in range(int(abs(move[1]))):
            #         self.rel_pos_head[1] += abs(move[1]) / move[1]
            #         tail_move = self._move_tail()
            #         self._update_grid(tail_move)

        total_tail_move_x = self.tail_pos[0] - prev_tail_pos[0]
        total_tail_move_y = self.tail_pos[1] - prev_tail_pos[1]
        return [total_tail_move_y, total_tail_move_x]

    def _move_tail(self):
        if abs(self.rel_pos_head[0]) < 2 and abs(self.rel_pos_head[1]) < 2:
            tail_move = (0, 0)
        elif abs(self.rel_pos_head[0]) >= 2 and abs(self.rel_pos_head[1]) >= 2:
            tail_move = (abs(self.rel_pos_head[0])/self.rel_pos_head[0],
                         abs(self.rel_pos_head[1])/self.rel_pos_head[1])
        elif abs(self.rel_pos_head[0]) >= 2:
            tail_move = (abs(self.rel_pos_head[0])/self.rel_pos_head[0],
                         self.rel_pos_head[1])
        elif abs(self.rel_pos_head[1]) >= 2:
            tail_move = (self.rel_pos_head[0],
                         abs(self.rel_pos_head[1])/self.rel_pos_head[1])

        self.rel_pos_head[0] -= tail_move[0]
        self.rel_pos_head[1] -= tail_move[1]

        return tail_move

    def _update_grid(self, tail_move):
        self.tail_pos[0] += tail_move[1]
        self.tail_pos[1] += tail_move[0]
        self.grid[int(self.tail_pos[0]), int(self.tail_pos[1])] += 1

    def pos_visualize(self):
        grid = np.zeros(shape=self.grid.shape).astype(int)
        head_position = self.tail_pos.copy()
        head_position[0] += self.rel_pos_head[1]
        head_position[1] += self.rel_pos_head[0]
        grid[int(head_position[0]), int(head_position[1])] = 1
        pos = self.tail_pos
        grid[int(pos[0]), int(pos[1])] = 1

        return grid


max_height = 0
max_width = 0
converted = []
for line in lines:
    direction, magnitude = line.split(' ')
    if direction == "D":
        max_height += int(magnitude)
        converted.append([0, int(magnitude)])
    elif direction == "R":
        max_width += int(magnitude)
        converted.append([int(magnitude), 0])
    elif direction == "U":
        max_width += int(magnitude)
        converted.append([0, -int(magnitude)])
    elif direction == "L":
        max_width += int(magnitude)
        converted.append([-int(magnitude), 0])

tail = Tail(max_height * 2, max_width * 2)
for line in lines:
    _ = tail.pull_rope(line)
for line in converted:
    _ = tail.pull_rope(line)

print("Places visited: ", np.count_nonzero(tail.grid))


# === Second Part ===
class LongTail(Tail):
    def __init__(self, height, width):
        self.height = height
        self.width = width
        self.chain = [
                      Tail(height, width),
                      Tail(height, width),
                      Tail(height, width),
                      Tail(height, width),
                      Tail(height, width),
                      Tail(height, width),
                      Tail(height, width),
                      Tail(height, width),
                      Tail(height, width),
                      ]
        self.grid = self.chain[-1].grid

    def pull_rope(self, move):
        direction, magnitude = move.split(' ')
        for i in range(int(magnitude)):
            unit_move = ' '.join([direction, '1'])
            for index in range(len(self.chain)):
                unit_move = self.chain[index].pull_rope(unit_move)

        self.grid = self.chain[-1].grid

    def pos_visualize(self):
        grid = np.zeros(shape=self.grid.shape).astype(int)
        head_position = self.chain[0].tail_pos.copy()
        head_position[0] += self.chain[0].rel_pos_head[1]
        head_position[1] += self.chain[0].rel_pos_head[0]
        for index in range(len(self.chain)-1, -1, -1):
            pos = self.chain[index].tail_pos
            grid[int(pos[0]), int(pos[1])] = index+2
        grid[int(head_position[0]), int(head_position[1])] = 1

        return grid


# longtail = LongTail(max_height * 2, max_width * 2)
# for line in lines:
#     longtail.pull_rope(line)
longtail = LongTail(30, 30)
print(longtail.pos_visualize())
for line in lines:
    print(line)
    longtail.pull_rope(line)
    print(longtail.pos_visualize())
    input()

longtail = LongTail(max_height * 2, max_width * 2)
for line in lines:
    longtail.pull_rope(line)
print("Places visited: ", np.count_nonzero(longtail.grid))
