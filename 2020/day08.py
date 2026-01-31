from pathlib import Path

def parse(lines):
    def parse_line(line):
        return line[:3], int(line[4:])
    return list(map(parse_line, lines))

def part1(ops):
    acc = 0
    ip = 0
    v = set()
    while ip not in v:
        v.add(ip)
        match ops[ip][0]:
            case 'acc':
                acc += ops[ip][1]
                ip += 1
            case 'jmp':
                ip += ops[ip][1]
            case 'nop':
                ip += 1
    return acc

if __name__ == '__main__':
    lines = Path('day08.txt').read_text().splitlines()
    ops = parse(lines)
    print(part1(ops))
