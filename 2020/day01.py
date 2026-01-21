T = 2020

def part1(input):
    h = {T - x: x for x in input}
    for x in input:
        if x in h:
            return x * h[x]
    return None

def part2(input):
    xs = sorted(input)
    for k, x in enumerate(xs):
        i = 0
        j = len(xs) - 1
        while i != j:
            s = x + xs[i] + xs[j]
            if s == T and i != k and j != k:
                return x * xs[i] * xs[j]
            if s > T:
                j -= 1
            if s < T:
                i += 1
    return None

if __name__ == '__main__':
    input = [int(x) for x in open("day01.txt")]
    print(part1(input))
    print(part2(input))

