import re

def parse(line):
    pattern = r"(\d+)-(\d+) (.): (.*)"
    match = re.match(pattern, line)
    if match:
        return (int(match.group(1)),
                int(match.group(2)),
                match.group(3),
                match.group(4))
    return None

def part1(input):
    def is_valid(line):
        x, y, c, s = line
        return x <= s.count(c) <= y

    # return len(list(filter(is_valid, input)))
    return sum(map(is_valid, input))

def part2(input):
    def is_valid(line):
        x, y, c, s = line
        return (s[x - 1] == c) ^ (s[y - 1] == c)
    return sum(map(is_valid, input))

if __name__ == '__main__':
    input = [parse(l) for l in open('day02.txt')]
    print(part1(input))
    print(part2(input))
