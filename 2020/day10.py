def part1(input):
    input = sorted(input)
    input.insert(0, 0) # prepend a 0 for the outlet
    a, b = 0, 0
    for x, y in zip(input, input[1:]):
        d = y - x
        match d:
            case 1:
                a += 1
            case 3:
                b += 1
    return a * (b + 1) # + 1 for the device itself

if __name__ == '__main__':
    input = [int(x) for x in open("day10.txt")]
    print(part1(input))
