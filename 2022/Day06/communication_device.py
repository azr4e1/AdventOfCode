# === First Part ===
with open("./input.txt") as f:
    buffer = f.read().strip()


length = len(buffer)

for marker in range(4, length):
    packet_start = set(buffer[marker - 4: marker])
    if len(packet_start) == 4:
        break

print(marker)

# === Second Part ===
for message_marker in range(14, length):
    packet_start = set(buffer[message_marker - 14: message_marker])
    if len(packet_start) == 14:
        break

print(message_marker)
