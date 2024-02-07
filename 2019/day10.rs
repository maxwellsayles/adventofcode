use std::cmp::Ordering;
use std::collections::{ HashSet, VecDeque };
use std::f64::consts::PI;
use std::fs;

type Point = (i32, i32);

fn dist(p: &Point) -> f64 {
    let x = p.0 as f64;
    let y = p.1 as f64;
    (x * x + y * y).sqrt()
}

fn rad(p: &Point) -> f64 {
    let x = p.0 as f64;
    let n = dist(p);
    let s = (x / n).asin();
    let r = if p.1 < 0 { s } else { PI - s };
    if r < 0. { r + 2. * PI }
    else if r > 2. * PI { r - 2. * PI }
    else { r }
}

fn cmp_points(p1: &Point, p2: &Point) -> Ordering {
    let r1 = rad(p1);
    let r2 = rad(p2);
    match r1.total_cmp(&r2) {
	Ordering::Less => Ordering::Less,
	Ordering::Greater => Ordering::Greater,
	Ordering::Equal => dist(p1).total_cmp(&dist(p2)),
    }
}

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

fn dist2(x: i32, y: i32) -> f64 {
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
		    if dist2(d1x, d1y) > dist2(d2x, d2y) {
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

fn part1(field: &HashSet<Point>) -> (&Point, usize) {
    let mut best = (&(0, 0), 0);
    for p in field.iter() {
	let cnt = count_visible(field, p);
	if cnt > best.1 {
	    best = (p, cnt);
	}
    }
    best
}


/**
 * Idea is to get the angle (and distance) between each point and the input,
 * sort the list, then move through the list skipping equal angles until
 * a subsequent pass through the list.
 */
fn part2(field: &HashSet<Point>, refp: &Point) -> Point {
    let mut rel_field: HashSet<Point> = field
	.iter()
	.map(|p| { (p.0 - refp.0, p.1 - refp.1) })
	.collect();
    rel_field.remove(&(0, 0));

    let mut sorted: Vec<&Point> = rel_field.iter().collect();
    sorted.sort_by(|a, b| cmp_points(a, b));

    let mut ps = VecDeque::from(sorted);

    let mut cnt = 0; // Starting at 1 wasn't right, but I don't know why?!?
    let mut p = ps.pop_front().unwrap_or(&(0, 0));
    let mut last_rad = rad(&p);
    while cnt < 200 {
	p = ps.pop_front().unwrap_or(&(0, 0));
	let next_rad = rad(&p);
	if next_rad == last_rad {
	    ps.push_back(p);
	} else {
	    last_rad = next_rad;
	    cnt += 1;
	}
    }
    (p.0 + refp.0, p.1 + refp.1)
}

fn main() {
    let field = load_asteroids();

    let (best_pos, best_cnt) = part1(&field);
    println!("{:?} {}", best_pos, best_cnt);

    println!("{:?}", part2(&field, &best_pos));
}
