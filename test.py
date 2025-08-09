from pathlib import Path


def parse_blocks(text: str):
    blocks, cur = [], []
    for line in text.splitlines():
        if line.strip():
            cur.append(line.strip())
        elif cur:
            blocks.append(cur)
            cur = []
    if cur:
        blocks.append(cur)
    return blocks


def classify(block):
    top, bottom = block[0], block[-1]
    if set(top) == {"#"} and set(bottom) == {"."}:
        return "lock"
    if set(top) == {"."} and set(bottom) == {"#"}:
        return "key"
    raise ValueError("Unexpected block type")


def lock_heights(block):
    h, w = len(block), len(block[0])
    out = []
    for c in range(w):
        k = 0
        for r in range(1, h):  # contiguous # starting just below the top row
            if block[r][c] == "#":
                k += 1
            else:
                break
        out.append(k)
    return out


def key_heights(block):
    h, w = len(block), len(block[0])
    out = []
    for c in range(w):
        k = 0
        for r in range(
            h - 2, -1, -1
        ):  # contiguous # starting just above the bottom row
            if block[r][c] == "#":
                k += 1
            else:
                break
        out.append(k)
    return out


def solve(path="text.txt"):
    text = Path(path).read_text()
    blocks = parse_blocks(text)
    interior = len(blocks[0]) - 2  # rows minus top and bottom

    locks, keys = [], []
    for b in blocks:
        (locks if classify(b) == "lock" else keys).append(
            lock_heights(b) if classify(b) == "lock" else key_heights(b)
        )

    total = 0
    for L in locks:
        for K in keys:
            if all(l + k <= interior for l, k in zip(L, K)):
                total += 1
    return total


if __name__ == "__main__":
    print(solve("text.txt"))  # -> 3114 for your input
