use std::collections::HashMap;
use std::fs;

type Point = (i32, i32);
type Cells = HashMap<Point, char>;

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

fn find_portal(cells: &Cells, p: &Point) -> Option<([char; 2], Point)> {
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
    for p in cells.keys() {
	if let Some((pair, point)) = find_portal(&cells, &p) {
	    println!("{}{} {},{}", pair[0], pair[1], point.0, point.1);
	}
    }
    HashMap::new()
}

fn main() {
    let cells = read_cells("day20.txt");
    find_portals(&cells);
    // for y in 0..=122 {
    // 	for x in 0..=126 {
    // 	    print!("{}", cells[&(x, y)]);
    // 	}
    // 	println!();
    // }
}
