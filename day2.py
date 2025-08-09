def part1(filepath: str) -> int:
    instructions = open(filepath, "r").read().strip().split("\n")
    result = 0
    for i in instructions:
        sizes = [int(x) for x in i.split("x")]
        result += (
            2 * sizes[0] * sizes[1] + 2 * sizes[1] * sizes[2] + 2 * sizes[0] * sizes[2]
        )
        sizes.sort()
        result += sizes[0] * sizes[1]

    return result


def part2(filepath: str) -> int:
    instructions = open(filepath, "r").read().strip().split("\n")
    result = 0
    for i in instructions:
        sizes = [int(x) for x in i.split("x")]
        sizes.sort()
        result += 2 * sizes[0] + 2 * sizes[1] + sizes[0] * sizes[1] * sizes[2]

    return result


if __name__ == "__main__":
    filepath = "./input2.txt"
    print(part1(filepath))
    print(part2(filepath))
