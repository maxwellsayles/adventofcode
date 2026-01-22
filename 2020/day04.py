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

def part1(input):
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
    return sum(map(lambda grp: required.issubset(grp), input))

if __name__ == '__main__':
    input = parse_key_value_file('day04.txt')
    print(part1(input))

