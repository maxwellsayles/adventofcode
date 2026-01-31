from pathlib import Path

def parse(lines):
    def parse_line(line):
        return line[:3], int(line[4:])
    return list(map(parse_line, lines))

def run(ops):
    acc = 0
    ip = 0
    v = set()
    while ip not in v and ip < len(ops):
        v.add(ip)
        match ops[ip][0]:
            case 'acc':
                acc += ops[ip][1]
                ip += 1
            case 'jmp':
                ip += ops[ip][1]
            case 'nop':
                ip += 1
    return acc, ip in v

def part1(ops):
    return run(ops)[0]

def part2(ops):
    ip = 0
    while ip < len(ops):
        op = ops[ip][0]
        if op != 'acc':
            flip_op = 'jmp' if op == 'nop' else 'nop'
            ops[ip] = flip_op, ops[ip][1]
            acc, inf = run(ops)
            if not inf:
                return acc
            ops[ip] = op, ops[ip][1]
        ip += 1
    return None

if __name__ == '__main__':
    lines = Path('day08.txt').read_text().splitlines()
    ops = parse(lines)
    print(part1(ops))
    print(part2(ops))
