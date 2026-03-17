def part1(input):
    a, b = 0, 0
    for x, y in zip(input, input[1:]):
        d = y - x
        match d:
            case 1:
                a += 1
            case 3:
                b += 1
    return a * b

def part2(input):
    opts = [1]
    n = len(input)
    for i in range(1, n):
        opt = 0
        for j in [i - 1, i - 2, i - 3]:
            if j >= 0 and input[j] >= input[i] - 3:
                opt += opts[j]
        opts.append(opt)
    return opts[-1]

if __name__ == '__main__':
    input = [int(x) for x in open("day10.txt")]
    input.sort()
    input.insert(0, 0) # prepend a 0 for the outlet
    input.append(input[-1] + 3) # the device is 3 greater than the highest
    print(part1(input))
    print(part2(input))
