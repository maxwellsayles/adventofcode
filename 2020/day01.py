def part1(input):
    h = {2020 - x: x for x in input}
    for x in input:
        if x in h:
            return x * h[x]

if __name__ == '__main__':
    input = [int(x) for x in open("day01.txt")]
    print(part1(input))

