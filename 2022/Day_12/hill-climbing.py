import string

translation = {letter: ord(letter) - ord('a') + 1
               for letter in string.ascii_lowercase}
translation['S'] = 0
translation['E'] = 100
lines = []
with open('./input.txt') as f:
    for line in f:
        row = [translation[letter] for letter in line.strip()]
        lines.append(row)
