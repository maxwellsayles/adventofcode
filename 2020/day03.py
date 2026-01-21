import math

from pathlib import Path

def traverse(input, dx, dy):
    x, y = 0, 0
    h = len(input)
    w = len(input[0])
    c = 0
    while y < h:
        if input[y][x] == '#':
            c += 1
        x = (x + dx) % w
        y += dy
    return c

def part1(input):
    return traverse(input, 3, 1)

def part2(input):
    return math.prod([traverse(input, 1, 1),
                      traverse(input, 3, 1),
                      traverse(input, 5, 1),
                      traverse(input, 7, 1),
                      traverse(input, 1, 2),
                      ])

if __name__ == '__main__':
    input = Path('day03.txt').read_text().splitlines()
    print(part1(input))
    print(part2(input))
