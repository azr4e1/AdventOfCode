from collections import defaultdict
from tqdm import tqdm


reducer = 9699690


# === First Part ===
def test(obj, divisible, monkey1, monkey2, reducer=reducer):
    if obj % divisible == 0:
        monkey = monkey1
    else:
        monkey = monkey2

    obj %= reducer
    return obj, monkey


class Monkey:
    worry_levels = 3

    def __init__(self, id: int,
                 starting_items: list,
                 operation,
                 test):
        self.id = id
        self.starting_items = starting_items
        self.operation = operation
        self.test = test
        self.inspection_number = 0

    def inspect(self):
        for index, obj in enumerate(self.starting_items):
            self.starting_items[index] = self.operation(obj) // self.worry_levels
            self.inspection_number += 1

    def throw(self):
        throwing_list = self.starting_items.copy()
        monkey_receiver_dict = defaultdict(list)
        for index in range(len(throwing_list)):
            obj = self.starting_items.pop(0)
            obj, monkey_catcher = self.test(obj)
            monkey_receiver_dict[monkey_catcher].append(obj)

        return monkey_receiver_dict

    def receive(self, objects_received):
        self.starting_items.extend(objects_received)


monkey0 = Monkey(0,
                 [61],
                 lambda x: x*11,
                 lambda x: test(x, 5, 7, 4))
monkey1 = Monkey(1,
                 [76, 92, 53, 93, 79, 86, 81],
                 lambda x: x+4,
                 lambda x: test(x, 2, 2, 6))
monkey2 = Monkey(2,
                 [91, 99],
                 lambda x: x*19,
                 lambda x: test(x, 13, 5, 0))
monkey3 = Monkey(3,
                 [58, 67, 66],
                 lambda x: x*x,
                 lambda x: test(x, 7, 6, 1))
monkey4 = Monkey(4,
                 [94, 54, 62, 73],
                 lambda x: x+1,
                 lambda x: test(x, 19, 3, 7))
monkey5 = Monkey(5,
                 [59, 95, 51, 58, 58],
                 lambda x: x+3,
                 lambda x: test(x, 11, 0, 4))
monkey6 = Monkey(6,
                 [87, 69, 92, 56, 91, 93, 88, 73],
                 lambda x: x+8,
                 lambda x: test(x, 3, 5, 2))
monkey7 = Monkey(7,
                 [71, 57, 86, 67, 96, 95],
                 lambda x: x+7,
                 lambda x: test(x, 17, 3, 1))

monkeys = {
           0: monkey0,
           1: monkey1,
           2: monkey2,
           3: monkey3,
           4: monkey4,
           5: monkey5,
           6: monkey6,
           7: monkey7,
           }

n_rounds = 20
for round in range(n_rounds):
    for monkey in monkeys.values():
        monkey.inspect()
        thrown_objects = monkey.throw()
        for id in thrown_objects:
            monkeys[id].receive(thrown_objects[id])

inspection_numbers = sorted([monkey.inspection_number for monkey in monkeys.values()], reverse=True)

business_values = inspection_numbers[0]*inspection_numbers[1]
print('Business Value: ', business_values)

# === Second Part ===
Monkey.worry_levels = 1

monkey0 = Monkey(0,
                 [61],
                 lambda x: x*11,
                 lambda x: test(x, 5, 7, 4))
monkey1 = Monkey(1,
                 [76, 92, 53, 93, 79, 86, 81],
                 lambda x: x+4,
                 lambda x: test(x, 2, 2, 6))
monkey2 = Monkey(2,
                 [91, 99],
                 lambda x: x*19,
                 lambda x: test(x, 13, 5, 0))
monkey3 = Monkey(3,
                 [58, 67, 66],
                 lambda x: x*x,
                 lambda x: test(x, 7, 6, 1))
monkey4 = Monkey(4,
                 [94, 54, 62, 73],
                 lambda x: x+1,
                 lambda x: test(x, 19, 3, 7))
monkey5 = Monkey(5,
                 [59, 95, 51, 58, 58],
                 lambda x: x+3,
                 lambda x: test(x, 11, 0, 4))
monkey6 = Monkey(6,
                 [87, 69, 92, 56, 91, 93, 88, 73],
                 lambda x: x+8,
                 lambda x: test(x, 3, 5, 2))
monkey7 = Monkey(7,
                 [71, 57, 86, 67, 96, 95],
                 lambda x: x+7,
                 lambda x: test(x, 17, 3, 1))

monkeys = {
           0: monkey0,
           1: monkey1,
           2: monkey2,
           3: monkey3,
           4: monkey4,
           5: monkey5,
           6: monkey6,
           7: monkey7,
           }

n_rounds = 10000
for round in tqdm(range(n_rounds)):
    for monkey in monkeys.values():
        monkey.inspect()
        thrown_objects = monkey.throw()
        for id in thrown_objects:
            monkeys[id].receive(thrown_objects[id])

inspection_numbers = sorted([monkey.inspection_number for monkey in monkeys.values()], reverse=True)
business_values = inspection_numbers[0]*inspection_numbers[1]
print('Business Value: ', business_values)
