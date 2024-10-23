use std::char;
use std::collections::{ HashMap, HashSet, VecDeque };
use std::fs;

type Point = (i32, i32);

#[derive(Eq, Hash, PartialEq)]
struct State {
    pos: Point,
    keys: u32,
}

impl State {
    fn new(cells: &HashMap<Point, char>) -> Self {
	let start = cells
	    .iter()
	    .filter(|(_, &v)| v == '@')
	    .collect::<Vec<_>>()[0].0;
	Self {
	    pos: start.clone(),
	    keys: 0,
	}
    }

    fn add_key(&self, c: char) -> Self {
	let mask = 1 << (c as u32 - 'a' as u32);
	Self {
	    pos: self.pos,
	    keys: self.keys | mask,
	}
    }

    fn has_key(&self, c: char) -> bool {
	let mask = 1 << (c as u32 - 'A' as u32);
	self.keys & mask != 0
    }

    fn step(&self, dx: i32, dy: i32) -> Self {
	Self {
	    pos: (self.pos.0 + dx, self.pos.1 + dy),
	    keys: self.keys,
	}
    }
}

fn all_keys(cells: &HashMap<Point, char>) -> u32 {
    let mut mask = 0;
    for &c in cells.values() {
	if c >= 'a' && c <= 'z' {
	    mask = mask | (1 << (c as u32 - 'a' as u32));
	}
    }
    mask
}

fn read_cells(filename: &str) -> HashMap<Point, char> {
    let content = fs::read_to_string(filename)
	.unwrap();
    let mut cells: HashMap<Point, char> = HashMap::new();
    for (y, row) in content.lines().enumerate() {
	for (x, c) in row.chars().enumerate() {
	    cells.insert((x as i32, y as i32), c);
	}
    }
    cells
}

fn part1(cells: &HashMap<Point, char>) {
    let keys = all_keys(cells);
    let mut q1 = VecDeque::new();
    let mut q2 = VecDeque::from([State::new(cells)]);
    let mut v = HashSet::new();
    let mut d = 0;
    while !q2.is_empty() {
	while let Some(s) = q2.pop_front() {
	    q1.push_back(s);
	}
	while let Some(mut s) = q1.pop_front() {
	    let c = cells[&s.pos];
	    if c == '#' {
		continue;
	    }
	    if v.contains(&s) {
		continue;
	    }
	    if c >= 'a' && c <= 'z' {
		s = s.add_key(c);
		if s.keys == keys {
		    println!("{}", d);
		    return;
		}
	    } else if c >= 'A' && c <= 'Z' && !s.has_key(c) {
		continue;
	    }
	    q2.push_back(s.step(0, -1));
	    q2.push_back(s.step(0, 1));
	    q2.push_back(s.step(-1, 0));
	    q2.push_back(s.step(1, 0));
	    v.insert(s);
	}
	d += 1;
    }
}

fn main() {
    let cells = read_cells("day18.txt");
    part1(&cells);
}
