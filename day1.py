def part1(filepath: str) -> int:
    instructions = open(filepath, "r").read()
    result = len(instructions.replace(")", "")) - len(instructions.replace("(", ""))

    return result


def part2(filepath: str) -> int:
    floor = 0
    idx = 1
    instructions = open(filepath, "r").read()
    for i in instructions:
        if i == "(":
            floor += 1
        elif i == ")":
            floor -= 1
        if floor < 0:
            return idx
        elif floor >= 0:
            idx += 1

    return idx


if __name__ == "__main__":
    filepath = "./input1.txt"
    # print(part1(filepath))
    print(part2(filepath))
