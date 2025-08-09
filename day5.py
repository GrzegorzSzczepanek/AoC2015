import re


class Day5:
    def __init__(self, filepath: str):
        self.vowels = ["a", "e", "i", "o", "u"]
        self.forbidden_strings = ["ab", "cd", "pq", "xy"]
        self.filepath = filepath

    def _check_double_char(self, word: str) -> bool:
        for i in range(1, len(word)):
            if word[i - 1] == word[i]:
                return True
        return False

    def _check_vowels(self, word: str) -> bool:
        new_word = re.sub(r"[aeiou]", "", word)

        if len(word) - len(new_word) >= 3:
            return True

        return False

    def _check_forbidden_strings(self, word: str) -> bool:
        for fs in self.forbidden_strings:
            if len(word.replace(fs, "")) < len(word):
                return False

        return True

    def _check_double_pairs(self, word: str) -> bool:
        if len(word) < 4:
            return False

        for i in range(1, len(word)):
            pair = word[i - 1 : i + 1]

            if len(word) - len(word.replace(pair, "")) >= 4:
                # print("condition: ", word.replace(pair, ""))
                return True

        return False

    def _check_between(self, word: str) -> bool:
        for i in range(2, len(word)):
            if (
                word[i]
                == word[i - 2]
                # and word[i - 1] != word[i]
                # and word[i - 1] != word[i - 2]
            ):
                return True

        return False

    def part1(self) -> int:
        instructions = open(self.filepath, "r").read().strip().split("\n")
        res = 0
        for i in instructions:
            if (
                self._check_double_char(i)
                and self._check_forbidden_strings(i)
                and self._check_vowels(i)
            ):
                res += 1

        return res

    def part2(self) -> int:
        instructions = open(self.filepath, "r").read().strip().split("\n")
        res = 0
        for i in instructions:
            if self._check_double_pairs(i) and self._check_between(i):
                print(i)
                res += 1

        return res


if __name__ == "__main__":
    filepath = "./input5.txt"
    day = Day5(filepath)
    # print(day.part1())
    print(day.part2())
