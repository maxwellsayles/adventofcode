from pathlib import Path

def seatID(code):
    b = code.replace('F', '0').replace('B', '1').replace('L', '0').replace('R', '1')
    return int(b, 2)

def part1(input):
    return max(map(seatID, input))

def part2(input):
    ids = list(map(seatID, input))
    x = min(ids)
    y = max(ids)
    h = y * (y + 1) // 2
    l = x * (x - 1) // 2
    return h - l - sum(ids)

if __name__ == '__main__':
    input = Path('day05.txt').read_text().splitlines()
    print(part1(input))
    print(part2(input))
