use gcd::Gcd;
use itertools::max;
use std::collections::HashSet;
use std::fs;

type Point = (i32, i32);

#[derive(Clone)]
struct AsteroidField {
    locs: HashSet<Point>,
    max_x: i32,
    max_y: i32,
}

fn load_asteroids() -> AsteroidField {
    let contents = fs::read_to_string("day10.txt")
        .unwrap();
    let input: Vec<&str> = contents.lines().collect();

    let mut locs = HashSet::new();
    for (y, row) in input.iter().enumerate() {
	for (x, c) in row.chars().enumerate() {
	    if c == '#' {
		locs.insert((x as i32, y as i32));
	    }
	}
    }

    let max_x = max(locs.iter().map(|(x, _)| *x)).unwrap_or(0);
    let max_y = max(locs.iter().map(|(_, y)| *y)).unwrap_or(0);

    AsteroidField {
	locs,
	max_x,
	max_y,
    }
}

fn remove_blocked(field: &mut AsteroidField, pos: &Point, loc: &Point) {
    // Yikes!
    let dx = loc.0 - pos.0;
    let dy = loc.1 - pos.1;
    let g = (dx as u32).gcd(dy as u32) as i32;
    let d = (dx / g, dy / g);

    let mut p = loc.clone();
    // Update `p` first so that visible `loc` isn't removed.
    p.0 += d.0;
    p.1 += d.1;
    while p.0 >= 0 && p.0 <= field.max_x && p.1 >= 0 && p.1 <= field.max_y {
	field.locs.remove(&p);
	p.0 += d.0;
	p.1 += d.1;
    }
}

fn count_visible(input_field: &AsteroidField, pos: &Point) -> usize {
    let mut field = input_field.clone();
    field.locs.remove(&pos);
    for loc in field.locs.clone().iter() {
	if field.locs.contains(&loc) {
	    remove_blocked(&mut field, pos, loc);
	}
    }

    field.locs.len()
}

fn part1(field: &AsteroidField) -> usize {
    max(field.locs.iter().map(|loc| count_visible(field, loc))).unwrap_or(0)
}

fn main() {
    let field = load_asteroids();
    println!("{}", part1(&field));
}
