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

if __name__ == '__main__':
    input = [int(x) for x in open("day09.txt")]
    print(part1(input))
