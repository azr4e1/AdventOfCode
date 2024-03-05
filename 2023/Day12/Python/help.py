def count_arrangements(conditions, rules):
    if not rules:
        return 0 if "#" in conditions else 1
    if not conditions:
        return 1 if not rules else 0

    result = 0

    if conditions[0] in ".?":
        result += count_arrangements(conditions[1:], rules)
    if conditions[0] in "#?":
        if (
            rules[0] <= len(conditions)
            and "." not in conditions[: rules[0]]
            and (rules[0] == len(conditions) or conditions[rules[0]] != "#")
        ):
            result += count_arrangements(conditions[rules[0] + 1 :], rules[1:])

    return result
