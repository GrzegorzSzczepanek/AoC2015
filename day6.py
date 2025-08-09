import re
from dataclasses import dataclass
from enum import Enum
from typing import List


class Action(Enum):
    TURN_ON = 1
    TURN_OFF = 2
    TOGGLE = 3


@dataclass
class Instruction:
    action: Action
    start_x: int
    start_y: int
    end_x: int
    end_y: int


class Day6:
    def __init__(self, filepath: str):
        self.filepath = filepath
        self.base_grid: List[List[bool | int]] = []

    def _fill_base_grid_bool(self) -> None:
        for i in range(1000):
            self.base_grid.append([])
            for j in range(1000):
                self.base_grid[i].append(False)

    def _fill_base_grid_int(self) -> None:
        for i in range(1000):
            self.base_grid.append([])
            for j in range(1000):
                self.base_grid[i].append(0)

    def _toggle_lights(self, x1: int, y1: int, x2: int, y2: int) -> None:
        if isinstance(self.base_grid[0][0], bool):
            for i in range(y1, y2 + 1):
                for j in range(x1, x2 + 1):
                    self.base_grid[i][j] = not self.base_grid[i][j]
        else:
            for i in range(y1, y2 + 1):
                for j in range(x1, x2 + 1):
                    self.base_grid[i][j] += 2

    def _turn_on_lights(self, x1: int, y1: int, x2: int, y2: int) -> None:

        if isinstance(self.base_grid[0][0], bool):
            for i in range(y1, y2 + 1):
                for j in range(x1, x2 + 1):
                    self.base_grid[i][j] = True
        else:
            for i in range(y1, y2 + 1):
                for j in range(x1, x2 + 1):
                    self.base_grid[i][j] += 1

    def _turn_off_lights(self, x1: int, y1: int, x2: int, y2: int) -> None:

        if isinstance(self.base_grid[0][0], bool):
            for i in range(y1, y2 + 1):
                for j in range(x1, x2 + 1):
                    self.base_grid[i][j] = False

        else:
            for i in range(y1, y2 + 1):
                for j in range(x1, x2 + 1):
                    if self.base_grid[i][j] > 0:
                        self.base_grid[i][j] -= 1

    def _count_lit(self) -> int:
        count = 0
        for i in range(len(self.base_grid)):
            for j in range(len(self.base_grid)):
                if self.base_grid[i][j]:
                    count += 1

        return count

    def _count_total_brightness(self) -> int:
        total = 0
        for i in range(len(self.base_grid)):
            for j in range(len(self.base_grid)):
                total += self.base_grid[i][j]

        return total

    def _parse_instructions(self) -> List[Instruction]:
        instructions = open(self.filepath, "r").read().strip().split("\n")
        parsed: List[Instruction] = []

        for i in instructions:
            match = re.match(
                r"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)", i
            )
            if not match:
                raise ValueError("Invalid instruction format")

            action_str, start_x_str, start_y_str, end_x_str, end_y_str = match.groups()

            action = None
            if action_str == "turn on":
                action = Action.TURN_ON
            elif action_str == "turn off":
                action = Action.TURN_OFF
            elif action_str == "toggle":
                action = Action.TOGGLE

            if action is None:
                print(action_str)
                raise ValueError("Invalid action")

            parsed.append(
                Instruction(
                    action=action,
                    start_x=int(start_x_str),
                    start_y=int(start_y_str),
                    end_x=int(end_x_str),
                    end_y=int(end_y_str),
                )
            )

        return parsed

    def part1(self) -> int:
        instructions = self._parse_instructions()
        self._fill_base_grid_bool()
        for i in instructions:
            if i.action == Action.TOGGLE:
                self._toggle_lights(x1=i.start_x, y1=i.start_y, x2=i.end_x, y2=i.end_y)
            elif i.action == Action.TURN_ON:
                self._turn_on_lights(x1=i.start_x, y1=i.start_y, x2=i.end_x, y2=i.end_y)
            elif i.action == Action.TURN_OFF:
                self._turn_off_lights(
                    x1=i.start_x, y1=i.start_y, x2=i.end_x, y2=i.end_y
                )

        res = self._count_lit()
        self.base_grid = []
        return res

    def part2(self) -> int:
        instructions = self._parse_instructions()
        self._fill_base_grid_int()
        for i in instructions:
            if i.action == Action.TOGGLE:
                self._toggle_lights(x1=i.start_x, y1=i.start_y, x2=i.end_x, y2=i.end_y)
            elif i.action == Action.TURN_ON:
                self._turn_on_lights(x1=i.start_x, y1=i.start_y, x2=i.end_x, y2=i.end_y)
            elif i.action == Action.TURN_OFF:
                self._turn_off_lights(
                    x1=i.start_x, y1=i.start_y, x2=i.end_x, y2=i.end_y
                )

        res = self._count_total_brightness()

        self.base_grid = []
        return res


if __name__ == "__main__":
    filepath = "./input6.txt"
    day = Day6(filepath)
    # print(day.part1())
    print(day.part2())
