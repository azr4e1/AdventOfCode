def read_instructions(file_name):
    with open(file_name, 'r') as file:
        instructions = [line.strip() for line in file.readlines()]
    return instructions


def apply_instructions(instructions):
    grid = [[0 for _ in range(1000)] for _ in range(1000)]

    for instruction in instructions:
        parts = instruction.split()
        if parts[0] == "toggle":
            action = "toggle"
            start_coords = tuple(map(int, parts[1].split(',')))
            end_coords = tuple(map(int, parts[3].split(',')))
        else:
            action = parts[1]
            start_coords = tuple(map(int, parts[2].split(',')))
            end_coords = tuple(map(int, parts[4].split(',')))

        for i in range(start_coords[0], end_coords[0] + 1):
            for j in range(start_coords[1], end_coords[1] + 1):
                if action == "on":
                    grid[i][j] = 1
                elif action == "off":
                    grid[i][j] = 0
                else:  # toggle
                    grid[i][j] = 1 - grid[i][j]

    return sum(sum(row) for row in grid)


instructions = read_instructions("input.txt")
result = apply_instructions(instructions)
print(result)
