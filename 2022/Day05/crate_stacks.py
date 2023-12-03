from copy import deepcopy
from collections import defaultdict
import string

# === First Part ===

file = []
breaking_point = 0
with open("./input.txt") as f:
    line_number = 1
    for line in f:
        if '1' in line and '[' in file[-1]:
            breaking_point = line_number
        file.append(line.rstrip())
        line_number += 1


def parify_length(crates):
    # make sure evey line has same length
    max_stacks = int(''.join(crates[-1].split(' '))[-1])
    for count, line in enumerate(crates):
        if len(line) < max_stacks * 4:
            crates[count] += ' ' * (max_stacks * 4 - len(line))

    return max_stacks, crates

def create_parser(crates):
    max_stacks, crates = parify_length(crates)
    parsed_crates = defaultdict(list)
    for line in crates:
        for stack in range(0, max_stacks):
            letter_range = line[4*stack: 4*stack + 4]
            if letter_range[1] in string.ascii_uppercase:
                parsed_crates[stack+1].append(letter_range[1])

    # reverse order
    for stack in parsed_crates:
        parsed_crates[stack].reverse()

    return parsed_crates


def move_parser(move):
    moves = {}
    move_split = move.split(' ')
    moves['quantity'] = int(move_split[1])
    moves['from'] = int(move_split[3])
    moves['to'] = int(move_split[5])

    return moves


def crane_movement(move, parsed_crates: list):
    new_parsed_crates = deepcopy(parsed_crates)
    quantity = move['quantity']
    from_stack = move['from']
    to_stack = move['to']

    for movement in range(quantity):
        crate = new_parsed_crates[from_stack].pop()
        new_parsed_crates[to_stack].append(crate)

    return new_parsed_crates


def get_top_of_stack(crates):
    top = []
    for stack_no in range(1, len(crates)+1):
        stack = crates[stack_no]
        if stack:
            top_stack = stack[-1]
        else:
            top_stack = ' '
        top.append(top_stack)

    return ''.join(top)


def pretty_print(crates):
    for stack_no in range(1, len(crates)+1):
        print(stack_no, end='\t')
        for letter in crates[stack_no]:
            print(letter, end=' ')
        print()


crates = file[:breaking_point]
for crate in crates:
    print(crate)
moves = file[breaking_point+1:]

original_parsed_crates = create_parser(crates)
parsed_moves = [move_parser(move) for move in moves]

# pretty_print(parsed_crates)
# print()
# for count, move in enumerate(parsed_moves):
#     print(moves[count])
#     print()
#     parsed_crates = crane_movement(move, parsed_crates)
#     pretty_print(parsed_crates)
#     a = input("Next step")
#     print()

parsed_crates = deepcopy(original_parsed_crates)
for count, move in enumerate(parsed_moves):
    parsed_crates = crane_movement(move, parsed_crates)

final_arrangement = get_top_of_stack(parsed_crates)
print("Top of stack is:")
print(final_arrangement)
print()

# === Second Part ===


def crane_movement_9001(move, parsed_crates):
    new_parsed_crates = deepcopy(parsed_crates)
    quantity = move['quantity']
    from_stack = move['from']
    to_stack = move['to']

    removed_part = new_parsed_crates[from_stack][-quantity:]
    new_parsed_crates[from_stack] = new_parsed_crates[from_stack][:-quantity]
    new_parsed_crates[to_stack] += removed_part

    return new_parsed_crates


parsed_crates_9001 = deepcopy(original_parsed_crates)
for count, move in enumerate(parsed_moves):
    parsed_crates_9001 = crane_movement_9001(move, parsed_crates_9001)
# pretty_print(parsed_crates_9001)
# print()
# for count, move in enumerate(parsed_moves):
#     print(moves[count])
#     print()
#     parsed_crates_9001 = crane_movement_9001(move, parsed_crates_9001)
#     pretty_print(parsed_crates_9001)
#     a = input("Next step")
#     print()

final_arrangement_9001 = get_top_of_stack(parsed_crates_9001)
print("Top of stack with crane 9001 is:")
print(final_arrangement_9001)
print()
