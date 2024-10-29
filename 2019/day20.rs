use std::collections::{ HashMap, HashSet, VecDeque };
use std::fs;

type Cells = HashMap<Point, char>;
type Label = [char; 2];
type Point = (i32, i32);

fn read_cells(filename: &str) -> Cells {
    let content = fs::read_to_string(filename)
	.unwrap();
    let mut cells = HashMap::new();
    for (y, row) in content.lines().enumerate() {
	for (x, c) in row.chars().enumerate() {
	    cells.insert((x as i32, y as i32), c);
	}
    }
    cells
}

fn maybe_portal(cells: &Cells, p: &Point) -> Option<(Label, Point)> {
    let &c1 = cells.get(&p).unwrap_or(&'#');
    if !c1.is_ascii_uppercase() {
	return None;
    }
    let &c2 = cells.get(&(p.0 + 1, p.1)).unwrap_or(&'#');
    if c2.is_ascii_uppercase() {
	if cells.get(&(p.0 - 1, p.1)) == Some(&'.') {
	    return Some(([c1, c2], (p.0 - 1, p.1)));
	}
	if cells.get(&(p.0 + 2, p.1)) == Some(&'.') {
	    return Some(([c1, c2], (p.0 + 2, p.1)));
	}
    }
    let &c3 = cells.get(&(p.0, p.1 + 1)).unwrap_or(&'#');
    if c3.is_ascii_uppercase() {
	if cells.get(&(p.0, p.1 - 1)) == Some(&'.') {
	    return Some(([c1, c3], (p.0, p.1 - 1)));
	}
	if cells.get(&(p.0, p.1 + 2)) == Some(&'.') {
	    return Some(([c1, c3], (p.0, p.1 + 2)));
	}
    }
    None
}

fn find_portals(cells: &Cells) -> HashMap<Point, Point> {
    let mut portals = HashMap::new();
    let mut ps: HashMap<Label, Point> = HashMap::new();
    for p in cells.keys() {
	if let Some((label, p1)) = maybe_portal(&cells, &p) {
	    if let Some(p2) = ps.get(&label) {
		// Got a portal pair.
		portals.insert(p1.clone(), p2.clone());
		portals.insert(p2.clone(), p1.clone());
	    } else {
		// Found first pair of a portal.
		ps.insert(label, p1);
	    }
	}
    }
    portals
}

fn find_portal_by_label(cells: &Cells, label: &Label) -> Option<Point> {
    for p in cells.keys() {
	if let Some((l, p)) = maybe_portal(&cells, &p) {
	    if l == *label {
		return Some(p);
	    }
	}
    }
    None
}

fn part1(cells: &Cells) {
    let portals = find_portals(&cells);
    let start = find_portal_by_label(&cells, &['A', 'A']).unwrap();
    let end = find_portal_by_label(&cells, &['Z', 'Z']).unwrap();

    let mut qbox = Box::new(VecDeque::from([start]));
    let mut v = HashSet::new();
    let mut d = 0;
    loop {
	let mut q = *qbox;
	let mut q2 = VecDeque::new();
	while let Some(p) = q.pop_front() {
	    if p == end {
		println!("{}", d);
		return;
	    }
	    if cells.get(&p).unwrap_or(&'#') != &'.' {
		continue;
	    }
	    if v.contains(&p) {
		continue;
	    }
	    q2.push_back((p.0 - 1, p.1));
	    q2.push_back((p.0 + 1, p.1));
	    q2.push_back((p.0, p.1 - 1));
	    q2.push_back((p.0, p.1 + 1));
	    if let Some(p2) = portals.get(&p) {
		q2.push_back(p2.clone());
	    }
	    v.insert(p);

	}
	*qbox = q2;
	d += 1;
    }
}

// NOTE: These values are hardcoded from my puzzle input. YMMV.
fn is_outside_portal(p: &Point) -> bool {
    p.0 == 2 || p.1 == 2 || p.0 == 124 || p.1 == 120
}

// NOTE: These values are hardcoded from my puzzle input. YMMV.
fn is_inside_portal(p: &Point) -> bool {
    p.0 == 34 || p.0 == 92 || p.1 == 34 || p.1 == 88
}

fn part2(cells: &Cells) {
    let portals = find_portals(&cells);
    let start = find_portal_by_label(&cells, &['A', 'A']).unwrap();
    let end = find_portal_by_label(&cells, &['Z', 'Z']).unwrap();

    let mut qbox = Box::new(VecDeque::from([(start.0, start.1, 0)]));
    let mut v = HashSet::new();
    let mut d = 0;
    loop {
	let mut q = *qbox;
	let mut q2 = VecDeque::new();
	while let Some(p) = q.pop_front() {
	    if p.0 == end.0 && p.1 == end.1 && p.2 == 0 {
		println!("{}", d);
		return;
	    }
	    if p.2 < 0 {
		continue;
	    }
	    let p1 = (p.0, p.1);
	    if cells.get(&p1).unwrap_or(&'#') != &'.' {
		continue;
	    }
	    if v.contains(&p) {
		continue;
	    }
	    q2.push_back((p.0 - 1, p.1, p.2));
	    q2.push_back((p.0 + 1, p.1, p.2));
	    q2.push_back((p.0, p.1 - 1, p.2));
	    q2.push_back((p.0, p.1 + 1, p.2));
	    if let Some(p2) = portals.get(&p1) {
		if is_outside_portal(&p1) {
		    q2.push_back((p2.0, p2.1, p.2 - 1));
		} else if is_inside_portal(&p1) {
		    q2.push_back((p2.0, p2.1, p.2 + 1));
		} else {
		    panic!("Portal is neither on the inside nor the outside: {},{}", p.0, p.1);
		}
	    }
	    v.insert(p);

	}
	*qbox = q2;
	d += 1;
    }
}

fn main() {
    let cells = read_cells("day20.txt");
    part1(&cells);
    part2(&cells);
}
