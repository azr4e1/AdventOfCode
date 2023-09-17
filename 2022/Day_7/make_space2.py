import re
from collections import defaultdict
from copy import deepcopy

# === First Part ===
lines = []
with open("./input.txt") as f:
    for line in f:
        lines.append(line.strip())


def collect_outputs(history):
    outputs = defaultdict(list)
    stack = []
    is_output = 0
    for count, line in enumerate(history):
        if re.match('^\$ cd[^\.]*$', line):
            in_output = 0
            _, _, dir = line.split(' ')
            stack.append(dir)
            continue
        if re.match('^\$ cd \.\.$', line):
            in_output = 0
            _ = stack.pop()
            continue
        if re.match('^\$ ls.*$', line):
            current_dir = history[count-1].split(' ')[2]
            is_output = 1
            continue
        if re.match('^\$.*$', line):
            is_output = 0
        if is_output:
            outputs['_'.join(stack)].append(line)

    return outputs


def determine_flat_size(outputs: dict):
    sizes = {}
    for key, values in outputs.items():
        size = 0
        for single_output in values:
            first_part, second_part = single_output.split(' ')
            if first_part == "dir":
                continue
            else:
                size += int(first_part)
        sizes[key] = size

    return sizes


def calculate_deep_size(original_flat_size):
    flat_size = deepcopy(original_flat_size)
    for path in flat_size:
        stack = []
        for el in path.split('_')[:-1]:
            stack.append(el)
            flat_size['_'.join(stack)] += flat_size[path]

    return flat_size

outputs = collect_outputs(lines)
flat_size = determine_flat_size(outputs)
deep_size = calculate_deep_size(flat_size)

sizes_less_than_100K = list(filter(lambda x: x <= 100000, deep_size.values()))
print(sum(sizes_less_than_100K))

# === Second Part ===
total_size = 70000000
update_space = 30000000
free_space = total_size - deep_size['/']
to_free = update_space - free_space
sizes_greater_than_to_free = list(filter(lambda x: x >= to_free, deep_size.values()))
print("Total size of directory to delete: ", min(sizes_greater_than_to_free))
