from pathlib import Path

def part1(input):
    x, y = 0, 0
    h = len(input)
    w = len(input[0])
    c = 0
    while y < h:
        if input[y][x] == '#':
            c += 1
        x = (x + 3) % w
        y += 1
    return c

if __name__ == '__main__':
    input = Path('day03.txt').read_text().splitlines()
    print(part1(input))
