import itertools

def is2sum(active, t):
    active = set(active)
    for x in active:
        if t - x in active and t - x != x:
            return True
    return False

def part1(input):
    active = input[:25]
    i = 25
    while is2sum(active, input[i]):
        active = active[1:]
        active.append(input[i])
        i += 1
    return input[i]

def part2(input, t):
    sums = list(itertools.accumulate(input))
    n = len(input)
    for i in range(n):
        for j in range(i + 1, n):
            s = sums[j] - sums[i]
            if s == t:
                sub = input[i : j + 1]
                return min(sub) + max(sub)
            elif s > t:
                break
    return None

if __name__ == '__main__':
    input = [int(x) for x in open("day09.txt")]
    t = part1(input)
    print(t)
    print(part2(input, t))
