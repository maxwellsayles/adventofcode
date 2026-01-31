import re

from collections import defaultdict
from pathlib import Path

def parse_bag_rules(lines):
    rules = {}
    for line in lines:
        outer, inner = line.strip('.').split(' contain ')
        outer_color = outer.replace(' bags', '')

        if inner == 'no other bags':
            rules[outer_color] = {}
        else:
            rules[outer_color] = {
                color: int(qty)
                for qty, color in re.findall(r'(\d+) (.*?) bag', inner)
            }
    return rules

def part1(rules):
    # Invert the containers so we can quickly see which colors contain a given color
    contained_by = defaultdict(set)
    for outer_color, contains in rules.items():
        for inner_color in contains:
            contained_by[inner_color].add(outer_color)

    # Do a DFS over the above map to find all the colors that contain 'shiny gold'
    contains_gold = set()
    def rec(c):
        contains_gold.add(c)
        for x in contained_by[c]:
            rec(x)
    rec('shiny gold')
    return len(contains_gold) - 1

# lines = """light red bags contain 1 bright white bag, 2 muted yellow bags.
# dark orange bags contain 3 bright white bags, 4 muted yellow bags.
# bright white bags contain 1 shiny gold bag.
# muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
# shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
# dark olive bags contain 3 faded blue bags, 4 dotted black bags.
# vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
# faded blue bags contain no other bags.
# dotted black bags contain no other bags.""".splitlines()

if __name__ == '__main__':
    lines = Path('day07.txt').read_text().splitlines()
    rules = parse_bag_rules(lines)
    print(part1(rules))
