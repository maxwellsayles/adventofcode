use itertools::max;
use std::collections::HashSet;
use std::fs;

type Point = (i32, i32);

fn load_asteroids() -> HashSet<Point> {
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
    locs
}

fn dist(x: i32, y: i32) -> f64 {
    let fx = x as f64;
    let fy = y as f64;
    (fx * fx + fy * fy).sqrt()
}

/**
 * For all pairs of points (excluding the input point), if the pair
 * create a congruent triangle, then remove the point further from the input
 * (since it would not be visible). The remaining count is visible.
 */
fn count_visible(input_field: &HashSet<Point>, p: &Point) -> usize {
    let mut field = input_field.clone();
    field.remove(&p);
    for p1 in field.clone().iter() {
	if !field.contains(p1) {
	    continue;
	}
	for p2 in field.clone().iter() {
	    if p1 == p2 {
		continue;
	    }
	    if !field.contains(p2) {
		continue;
	    }
	    let d1x = p1.0 - p.0;
	    let d1y = p1.1 - p.1;
	    let d2x = p2.0 - p.0;
	    let d2y = p2.1 - p.1;
	    // Check congruency.
	    if d1x * d2y == d2x * d1y {
		// Check signs.
		if d1x * d2x >= 0 && d1y * d2y >= 0 {
		    // Remove the point that is further away from the input.
		    if dist(d1x, d1y) > dist(d2x, d2y) {
			field.remove(p1);
			break;
		    } else {
			field.remove(p2);
		    }
		}
	    }
	}
    }

    field.len()
}

fn part1(field: &HashSet<Point>) -> usize {
    max(field.iter().map(|p| count_visible(field, p))).unwrap_or(0)
}

fn main() {
    let field = load_asteroids();
    println!("{}", part1(&field));
}
