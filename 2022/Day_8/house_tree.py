import numpy as np

# === First Part ===

lines = []
with open('./input.txt') as f:
    for line in f:
        lines.append([int(num) for num in line.strip()])

grid = np.array(lines)
visible_from_left = np.zeros(shape=grid.shape)
visible_from_right = np.zeros(shape=grid.shape)
visible_from_above = np.zeros(shape=grid.shape)
visible_from_below = np.zeros(shape=grid.shape)

visible = 0
non_visible = 1

# from left
for i in range(visible_from_left.shape[0]):
    for j in range(visible_from_left.shape[1]):
        current_tree = grid[i, j]
        for n in range(j):
            if grid[i, n] >= current_tree:
                visible_from_left[i, j] = non_visible

# from right
for i in range(visible_from_right.shape[0]):
    for j in range(visible_from_right.shape[1]):
        current_tree = grid[i, j]
        for n in range(j+1, visible_from_right.shape[1]):
            if grid[i, n] >= current_tree:
                visible_from_right[i, j] = non_visible

# from above
for j in range(visible_from_above.shape[0]):
    for i in range(visible_from_above.shape[1]):
        current_tree = grid[i, j]
        for n in range(i):
            if grid[n, j] >= current_tree:
                visible_from_above[i, j] = non_visible

# from below
for j in range(visible_from_below.shape[0]):
    for i in range(visible_from_below.shape[1]):
        current_tree = grid[i, j]
        for n in range(i+1, visible_from_below.shape[1]):
            if grid[n, j] >= current_tree:
                visible_from_below[i, j] = non_visible


visible_tree = visible_from_left * visible_from_right * visible_from_above * visible_from_below

print("Total number of trees visible: ", (1 - visible_tree).sum().sum())

# === Second Part ===

trees_from_left = np.zeros(shape=grid.shape)
trees_from_right = np.zeros(shape=grid.shape)
trees_from_above = np.zeros(shape=grid.shape)
trees_from_below = np.zeros(shape=grid.shape)

# from left
for i in range(trees_from_left.shape[0]):
    for j in range(trees_from_left.shape[1]):
        current_tree = grid[i, j]
        for tree in range(j-1, -1, -1):
            trees_from_left[i, j] += 1
            if grid[i, tree] >= current_tree:
                break

# from right
for i in range(trees_from_right.shape[0]):
    for j in range(trees_from_right.shape[1]):
        current_tree = grid[i, j]
        for tree in range(j+1, trees_from_right.shape[1]):
            trees_from_right[i, j] += 1
            if grid[i, tree] >= current_tree:
                break

# from above
for j in range(trees_from_above.shape[0]):
    for i in range(trees_from_above.shape[1]):
        current_tree = grid[i, j]
        for tree in range(i-1, -1, -1):
            trees_from_above[i, j] += 1
            if grid[tree, j] >= current_tree:
                break

# from below
for j in range(trees_from_below.shape[0]):
    for i in range(trees_from_below.shape[1]):
        current_tree = grid[i, j]
        for tree in range(i+1, trees_from_below.shape[1]):
            trees_from_below[i, j] += 1
            if grid[tree, j] >= current_tree:
                break

tree_score = trees_from_left * trees_from_right * trees_from_above * trees_from_below
print("Max Scenic Score: ", tree_score.max())
