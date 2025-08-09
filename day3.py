from typing import List, Tuple


def part1(filepath: str) -> int:
    instructions = open(filepath, "r").read().strip()
    result = 0
    x, y = 0, 0
    visited = []
    visited.append((x, y))

    for instruction in instructions:
        if instruction == "^":
            x += 1
        elif instruction == "v":
            x -= 1
        elif instruction == ">":
            y += 1
        elif instruction == "<":
            y -= 1

        print(x, y)
        visited.append((x, y))

    print(set(visited))
    return len(list(set(visited)))


def part2(filepath: str) -> int:
    instructions = open(filepath, "r").read().strip()
    result = 0
    x1, y1, x2, y2 = 0, 0, 0, 0
    visited = []
    visited.append((x1, y1))
    visited.append((x2, y2))

    for i in range(len(instructions)):
        instruction = instructions[i]
        if i % 2 == 0:
            if instruction == "^":
                x1 += 1
            elif instruction == "v":
                x1 -= 1
            elif instruction == ">":
                y1 += 1
            elif instruction == "<":
                y1 -= 1
        else:
            if instruction == "^":
                x2 += 1
            elif instruction == "v":
                x2 -= 1
            elif instruction == ">":
                y2 += 1
            elif instruction == "<":
                y2 -= 1

        visited.append((x1, y1))
        visited.append((x2, y2))

    return len(list(set(visited)))


if __name__ == "__main__":
    filepath = "./input3.txt"
    # print(part1(filepath))
    print(part2(filepath))
