use std::char;
use std::cmp::Ordering;
use std::collections::{ BinaryHeap, HashMap, HashSet, VecDeque };
use std::fs;
use std::hash::{Hash, Hasher};

fn is_key(c: char) -> bool {
    c >= 'a' && c <= 'z'
}

fn key_to_mask(c: char) -> u32 {
    1 << (c as u32 - 'a' as u32)
}

fn is_lock(c: char) -> bool {
    c >= 'A' && c <= 'Z'
}

fn lock_to_mask(c: char) -> u32 {
    1 << (c as u32 - 'A' as u32)
}

type Point = (i32, i32);

#[derive(Eq, Hash, PartialEq)]
struct State {
    pos: Point,
    mask: u32,
}

impl State {
    fn with_cells(cells: &HashMap<Point, char>) -> Self {
	let start = cells
	    .iter()
	    .filter(|(_, &v)| v == '@')
	    .collect::<Vec<_>>()[0].0;
	Self {
	    pos: start.clone(),
	    mask: 0,
	}
    }

    fn with_start(p: &Point) -> Self {
	Self {
	    pos: p.clone(),
	    mask: 0,
	}
    }

    fn add_key(&self, c: char) -> Self {
	assert!(is_key(c));
	Self {
	    pos: self.pos,
	    mask: self.mask | key_to_mask(c),
	}
    }

    fn has_lock(&self, c: char) -> bool {
	assert!(is_lock(c));
	self.mask & lock_to_mask(c) != 0
    }

    fn add_lock(&self, c: char) -> Self {
	assert!(is_lock(c));
	Self {
	    pos: self.pos,
	    mask: self.mask | lock_to_mask(c),
	}
    }

    fn step(&self, dx: i32, dy: i32) -> Self {
	Self {
	    pos: (self.pos.0 + dx, self.pos.1 + dy),
	    mask: self.mask,
	}
    }
}

fn all_keys(cells: &HashMap<Point, char>) -> u32 {
    let mut mask = 0;
    for &c in cells.values() {
	if is_key(c) {
	    mask = mask | key_to_mask(c);
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
    let mut qbox = Box::new(VecDeque::from([State::with_cells(cells)]));
    let mut v = HashSet::new();
    let mut d = 0;
    loop {
	let mut q = *qbox;
	let mut q2 = VecDeque::new();
	while let Some(mut s) = q.pop_front() {
	    let c = cells[&s.pos];
	    if c == '#' {
		continue;
	    }
	    if v.contains(&s) {
		continue;
	    }
	    if is_key(c) {
		s = s.add_key(c);
		if s.mask == keys {
		    println!("{}", d);
		    return;
		}
	    } else if is_lock(c) && !s.has_lock(c) {
		continue;
	    }
	    q2.push_back(s.step(0, -1));
	    q2.push_back(s.step(0, 1));
	    q2.push_back(s.step(-1, 0));
	    q2.push_back(s.step(1, 0));
	    v.insert(s);
	}
	*qbox = q2;
	d += 1;
    }
}

struct Reachable {
    p: Point,
    key_mask: u32,
    locks_mask: u32,
    dist: usize,
}

impl Reachable {
    fn with_extra(p: &Point, k: char, locks_mask: u32, dist: usize) -> Self {
	Self {
	    p: p.clone(),
	    key_mask: key_to_mask(k),
	    locks_mask: locks_mask,
	    dist: dist,
	}
    }
}

fn find_reachable(cells: &HashMap<Point, char>, p: &Point) -> Vec<Reachable> {
    let mut reachable = Vec::new();
    let mut qbox = Box::new(VecDeque::from([State::with_start(&p)]));
    let mut v = HashSet::new();
    let mut d = 0;
    // while !qbox.is_empty() {
    loop {
	let mut q = *qbox;
	if q.is_empty() {
	    break;
	}
	let mut q2 = VecDeque::new();
	while let Some(mut s) = q.pop_front() {
	    let c = cells[&s.pos];
	    if c == '#' {
		continue;
	    }
	    if v.contains(&s.pos) {
		continue;
	    }
	    if is_key(c) && s.pos != *p {
		reachable.push(Reachable::with_extra(&s.pos, c, s.mask, d));
		continue;
	    } else if is_lock(c) {
		s = s.add_lock(c);
	    }
	    q2.push_back(s.step(0, -1));
	    q2.push_back(s.step(0, 1));
	    q2.push_back(s.step(-1, 0));
	    q2.push_back(s.step(1, 0));
	    v.insert(s.pos);
	}
	*qbox = q2;
	d += 1;
    }
    reachable
}

struct StateP2 {
    ps: [Point; 4],
    d: usize, // distance travelled
    ks: u32, // key mask
}

impl Hash for StateP2 {
    fn hash<H: Hasher>(&self, state: &mut H) {
	self.ps.hash(state);
	self.ks.hash(state);
    }
}

impl Eq for StateP2 {}

impl PartialEq for StateP2 {
    fn eq(&self, other: &Self) -> bool {
	self.ps == other.ps && self.ks == other.ks
    }
}

impl Ord for StateP2 {
    fn cmp(&self, other: &Self) -> Ordering {
	other.d.cmp(&self.d)
    }
}

impl PartialOrd for StateP2 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
	Some(self.cmp(other))
    }
}

impl StateP2 {
    fn advance_to_reachable(&self, i: usize, r: &Reachable) -> Option<Self> {
	// Don't have all the keys needed for all the locks encountered on the path.
	if (r.locks_mask & self.ks) != r.locks_mask {
	    return None;
	}
	let new_d = self.d + r.dist;
	let new_ks = self.ks | r.key_mask;
	let mut new_ps = self.ps.clone();
	new_ps[i] = r.p;
	Some(
	    Self {
		ps: new_ps,
		d: new_d,
		ks: new_ks,
	    }
	)
    }
}

fn part2(input_cells: &HashMap<Point, char>) {
    // Modify the input cells to separate the starting points.
    let mut cells = input_cells.clone();
    let &(px, py) = cells
	.iter()
	.filter(|(_, &v)| v == '@')
	.collect::<Vec<_>>()[0].0;
    cells.insert((px - 1, py - 1), '@');
    cells.insert((px    , py - 1), '#');
    cells.insert((px + 1, py - 1), '@');
    cells.insert((px - 1, py    ), '#');
    cells.insert((px    , py    ), '#');
    cells.insert((px + 1, py    ), '#');
    cells.insert((px - 1, py + 1), '@');
    cells.insert((px    , py + 1), '#');
    cells.insert((px + 1, py + 1), '@');

    // Build the map of all keys reachable (and locks needed) from every other
    // key / start.
    let mut reachable_from = HashMap::new(); // Point -> Vec<Reachable>
    for (&p, &c) in cells.iter() {
	if is_key(c) || c == '@' {
	    let r = find_reachable(&cells, &p);
	    reachable_from.insert(p, r);
	}
    }

    // BFS until all keys are reached.
    let all_keys = all_keys(&cells);
    let init_state = StateP2 {
	ps: [(px - 1, py - 1), (px + 1, py - 1), (px - 1, py + 1), (px + 1, py + 1)],
	d: 0,
	ks: 0,
    };
    let mut q = BinaryHeap::from([init_state]);
    let mut v = HashSet::new();
    while let Some(s) = q.pop() {
	if s.ks == all_keys {
	    println!("{}", s.d);
	    return;
	}
	if v.contains(&s) {
	    continue;
	}

	// Queue up all valid moves from here.
	for (i, p) in s.ps.iter().enumerate() {
	    let reachable = &reachable_from[p];
	    for r in reachable {
		if let Some(next_s) = s.advance_to_reachable(i, r) {
		    q.push(next_s);
		}
	    }
	}

	v.insert(s);
    }
}

fn main() {
    let cells = read_cells("day18.txt");
    part1(&cells);
    part2(&cells);
}
