def parse_key_value_file(filepath):
    """
    Parses a file containing groups of key:value pairs.
    Groups are separated by blank lines.
    Pairs within a group are separated by spaces or newlines.
    """
    data = []
    current_group = {}

    try:
        with open(filepath, 'r') as f:
            for line in f:
                line = line.strip()

                # If line is empty, it marks the end of a group
                if not line:
                    if current_group:
                        data.append(current_group)
                        current_group = {}
                    continue

                # Split line by spaces to handle multiple pairs on one line
                # e.g., "key1:val1 key2:val2"
                pairs = line.split()

                for pair in pairs:
                    if ':' in pair:
                        # Split only on the first colon to allow colons in values
                        k, v = pair.split(':', 1)
                        current_group[k] = v

        # Append the last group if it exists (no trailing newline in file)
        if current_group:
            data.append(current_group)

    except FileNotFoundError:
        print(f"Error: The file '{filepath}' was not found.")
        return []

    return data

def has_required_keys(d):
    required = {
        'byr',
        'iyr',
        'eyr',
        'hgt',
        'hcl',
        'ecl',
        'pid',
        # 'cid', # cid is optional
    }
    return required.issubset(d)

def part1(input):
    return sum(map(has_required_keys, input))

def maybe_int(value):
    try:
        return int(value)
    except (ValueError, TypeError):
        return None

import re

def is_valid_hgt(hgt):
    match = re.fullmatch(r'(\d+)(cm|in)', hgt)
    if not match:
        return False

    num, unit = int(match.group(1)), match.group(2)
    return (unit == 'cm' and 150 <= num <= 193) or \
           (unit == 'in' and 59 <= num <= 76)

def part2(input):
    def is_valid(d):
        byr = maybe_int(d.get('byr')) or 0
        iyr = maybe_int(d.get('iyr')) or 0
        eyr = maybe_int(d.get('eyr')) or 0
        hgt = d.get('hgt')
        hcl = d.get('hcl')
        ecl = d.get('ecl')
        pid = d.get('pid')

        return has_required_keys(d) and \
            1920 <= byr <= 2002 and \
            2010 <= iyr <= 2020 and \
            2020 <= eyr <= 2030 and \
            is_valid_hgt(hgt) and \
            bool(re.fullmatch(r'#[0-9a-f]{6}', hcl)) and \
            ecl in {'amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'} and \
            len(pid) == 9 and pid.isdigit()

    return sum(map(is_valid, input))

if __name__ == '__main__':
    input = parse_key_value_file('day04.txt')
    print(part1(input))
    print(part2(input))

