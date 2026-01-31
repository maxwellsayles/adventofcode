def parse_groups(filename):
    with open(filename, 'r') as f:
        content = f.read().strip()
        return [block.splitlines() for block in content.split('\n\n')]

def count_group1(grp):
    return len(set.union(*map(set, grp)))

def part1(input):
    return sum(map(count_group1, input))

def count_group2(grp):
    return len(set.intersection(*map(set, grp)))

def part2(input):
    return sum(map(count_group2, input))

if __name__ == '__main__':
    input = parse_groups('day06.txt')
    print(part1(input))
    print(part2(input))
