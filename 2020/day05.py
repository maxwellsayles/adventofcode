from pathlib import Path

def seatID(code):
    b = code.replace('F', '0').replace('B', '1').replace('L', '0').replace('R', '1')
    return int(b, 2)

def part1(input):
    return max(map(seatID, input))

if __name__ == '__main__':
    input = Path('day05.txt').read_text().splitlines()
    print(part1(input))
