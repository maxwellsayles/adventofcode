def count_adj(grid, x, y):
    count = 0
    rows = len(grid)
    cols = len(grid[0]) if rows > 0 else 0

    for dy in [-1, 0, 1]:
        for dx in [-1, 0, 1]:
            if dx == 0 and dy == 0:
                continue
            nx, ny = x + dx, y + dy
            if 0 <= ny < rows and 0 <= nx < cols:
                if grid[ny][nx] == '#':
                    count += 1
    return count

def step1(grid):
    new_grid = []
    for y, row_str in enumerate(grid):
        new_row = []
        for x, char in enumerate(row_str):
            adj_count = count_adj(grid, x, y)
            if char == 'L' and adj_count == 0:
                new_row.append('#')
            elif char == '#' and adj_count >= 4:
                new_row.append('L')
            else:
                new_row.append(char)
        new_grid.append("".join(new_row))
    return new_grid

def part1(grid):
    grid1 = grid
    grid2 = None
    while grid1 != grid2:
        grid2 = grid1
        grid1 = step1(grid1)
    return sum(x.count('#') for x in grid1)

def count_visible(grid, x, y):
    count = 0
    rows = len(grid)
    cols = len(grid[0])

    directions = [
        (-1, -1), (-1, 0), (-1, 1),
        ( 0, -1),          ( 0, 1),
        ( 1, -1), ( 1, 0), ( 1, 1)
    ]

    for dy, dx in directions:
        nx, ny = x + dx, y + dy
        while 0 <= ny < rows and 0 <= nx < cols:
            char = grid[ny][nx]
            if char == '#':
                count += 1
                break
            if char == 'L':
                break

            nx += dx
            ny += dy

    return count

def step2(grid):
    new_grid = []
    for y, row_str in enumerate(grid):
        new_row = []
        for x, char in enumerate(row_str):
            cnt = count_visible(grid, x, y)
            if char == 'L' and cnt == 0:
                new_row.append('#')
            elif char == '#' and cnt >= 5:
                new_row.append('L')
            else:
                new_row.append(char)
        new_grid.append("".join(new_row))
    return new_grid

def part2(grid):
    grid1 = grid
    grid2 = None
    while grid1 != grid2:
        grid2 = grid1
        grid1 = step2(grid1)
    return sum(x.count('#') for x in grid1)

if __name__ == '__main__':
    input = [x.rstrip() for x in open('day11.txt')]
    print(part1(input))
    print(part2(input))
