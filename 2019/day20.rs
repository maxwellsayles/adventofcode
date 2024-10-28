use std::collections::HashMap;
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

fn main() {
    let cells = read_cells("day20.txt");
    let portals = find_portals(&cells);
    for (p1, p2) in portals.iter() {
	println!("{},{} -> {},{}", p1.0, p1.1, p2.0, p2.1);
    }
}
