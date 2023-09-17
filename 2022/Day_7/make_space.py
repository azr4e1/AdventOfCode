import re
from collections import defaultdict
from copy import deepcopy

lines = []
count_ls = 0
count_cd_in_dir_before_ls = 0
with open("./input.txt") as f:
    for line in f:
        if re.match('^\$ ls.*$', line):
            count_ls += 1
            if re.match('^\$ cd[^\.]*$', lines[-1]):
                count_cd_in_dir_before_ls += 1
        lines.append(line.strip())

if count_ls == count_cd_in_dir_before_ls:
    print("ls command is always preceded by a cd into a directory.\n")


def collect_outputs(history):
    outputs = defaultdict(list)
    is_output = 0
    for count, line in enumerate(history):
        if re.match('^\$.*$', line):
            is_output = 0
        if re.match('^\$ ls.*$', line):
            current_dir = history[count-1].split(' ')[2]
            is_output = 1
            continue
        if is_output:
            outputs[current_dir].append(line)

    return outputs


def determine_size(outputs: dict):
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


def determine_dirs(outputs: dict):
    dirs = {}
    for key, values in outputs.items():
        dirs[key] = set()
        for single_output in values:
            first_part, second_part = single_output.split(' ')
            if first_part == "dir":
                dirs[key].add(second_part)
            else:
                continue

    return dirs


def flatten_nested_dirs(directory: str, dirs: dict[str, set], already_explored: list):
    """
    Depth first algorithm
    """

    to_explore = deepcopy(dirs[directory])
    for dir in to_explore:
        if dir in already_explored:
            continue
        already_explored.append(dir)
        flatten_nested_dirs(dir, dirs, already_explored)
        dirs[directory] = dirs[directory].union(dirs[dir])


outputs = collect_outputs(lines)
first_level_sizes = determine_size(outputs)
dirs = determine_dirs(outputs)
already_explored = ['/']
flatten_nested_dirs('/', dirs, already_explored)
deep_sizes = {}
for dir in dirs:
    size = first_level_sizes[dir]
    for nested_dir in dirs[dir]:
        size += first_level_sizes[nested_dir]
    deep_sizes[dir] = size

sizes_less_than_100K = list(filter(lambda x: x <= 100000, deep_sizes.values()))
print(sum(sizes_less_than_100K))
