use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::vec::Vec;

use crate::intcode_computer::IntcodeComputer;

mod intcode_computer;

#[derive(Clone, Copy, Debug)]
enum Dir {
    North = 1,
    South = 2,
    West = 3,
    East = 4,
}

fn dir_delta(dir: Dir) -> (i32, i32) {
    match dir {
        Dir::North => (0, -1),
        Dir::South => (0, 1),
        Dir::West => (-1, 0),
        Dir::East => (1, 0),
    }
}

fn opposite_dir(dir: Dir) -> Dir {
    match dir {
        Dir::North => Dir::South,
        Dir::South => Dir::North,
        Dir::West => Dir::East,
        Dir::East => Dir::West,
    }
}

fn move_pos(pos: &Pos, dir: Dir) -> Pos {
    let d = dir_delta(dir);
    (pos.0 + d.0, pos.1 + d.1)
}

#[derive(Debug)]
enum Status {
    Wall = 0,
    Moved = 1,
    MovedToOxygen = 2,
}

fn status_from_i64(x: i64) -> Status {
    match x {
        0 => Status::Wall,
        1 => Status::Moved,
        2 => Status::MovedToOxygen,
        _ => panic!("Unrecognized status {}", x),
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Cell {
    Open,
    Oxygen,
    Wall,
}

type Pos = (i32, i32);

struct State {
    moves: Vec<Dir>,
    pos: Pos,
    cells: HashMap<Pos, Cell>,
    comp: IntcodeComputer,
}

impl State {
    fn new(code: &Vec<i64>) -> Self {
        Self {
            moves: Vec::new(),
            pos: (0, 0),
            cells: HashMap::from([((0, 0), Cell::Open)]),
            comp: IntcodeComputer::new(Vec::new(), &code),
        }
    }

    fn generate_cells(&mut self) {
	while self.step() {}
	assert!(self.pos.0 == 0 && self.pos.1 == 0);
    }

    fn is_cell_unvisited(&self, d: Dir) -> bool {
        !self.cells.contains_key(&move_pos(&self.pos, d))
    }

    fn get_next_move(&self) -> Option<Dir> {
        for d in [Dir::North, Dir::East, Dir::South, Dir::West] {
            if self.is_cell_unvisited(d) {
                return Some(d);
            }
        }
        None
    }

    fn execute_move(&mut self, dir: Dir) -> Status {
        self.comp.add_input(dir as i64);
        self.comp.run();
        status_from_i64(self.comp.remove_output().unwrap())
    }

    fn update_move(&mut self, dir: Dir) {
        assert!(self.is_cell_unvisited(dir));
        let next_pos = move_pos(&self.pos, dir);
        match self.execute_move(dir) {
            Status::Wall => {
                self.cells.insert(next_pos, Cell::Wall);
            }

            Status::Moved => {
                self.cells.insert(next_pos, Cell::Open);
                self.moves.push(dir);
                self.pos = next_pos;
            }

            Status::MovedToOxygen => {
                self.cells.insert(next_pos, Cell::Oxygen);
                self.moves.push(dir);
                self.pos = next_pos;
            }
        }
    }

    fn step(&mut self) -> bool {
        match self.get_next_move() {
            Some(next_move) => self.update_move(next_move),
            None => match self.moves.pop() {
                None => return false,
                Some(last_move) => {
                    let back_dir = opposite_dir(last_move);
                    let back_pos = move_pos(&self.pos, back_dir);
                    match self.execute_move(back_dir) {
                        Status::Wall => panic!("Not expecting to hit a wall when backtracking!"),
                        Status::Moved => self.pos = back_pos,
                        Status::MovedToOxygen => self.pos = back_pos,
                    }
                }
            },
        }
        true
    }
}

fn part1(cells: &HashMap<Pos, Cell>) {
    let mut q = VecDeque::from([((0, 0), 0)]);
    let mut v = HashSet::new();
    while let Some((p, d)) = q.pop_front() {
	v.insert(p);
	if cells.get(&p) == Some(&Cell::Oxygen) {
	    println!("{}", d);
	    return;
	}
	let moves = [Dir::North, Dir::East, Dir::South, Dir::West]
	    .map(|d| move_pos(&p, d));
	for m in moves {
	    if !v.contains(&m) && cells.get(&m) != Some(&Cell::Wall) {
		q.push_back((m, d + 1));
	    }
	}
    }
}

fn part2(input_cells: &HashMap<Pos, Cell>) {
    let mut cells_queue = Vec::from([input_cells.clone()]);
    let mut open_cnt =
	input_cells.values().filter(|c| *c == &Cell::Open).count();
    let mut round = 0;

    while open_cnt > 0 {
	open_cnt = 0;
	let cells = cells_queue.pop().unwrap();
	let mut cells2 = HashMap::new();

	let is_adjacent_to_oxygen = |p| {
            [Dir::North, Dir::East, Dir::South, Dir::West].iter().any(|d| {
		let c = cells.get(&move_pos(p, *d)).unwrap_or(&Cell::Open);
		c == &Cell::Oxygen
	    })
	};

	for (p, v) in cells.iter() {
	    let new_v = match v {
		Cell::Open => {
		    if is_adjacent_to_oxygen(&p) {
			Cell::Oxygen
		    } else {
			open_cnt += 1;
			Cell::Open
		    }
		},
		Cell::Oxygen => Cell::Oxygen,
		Cell::Wall => Cell::Wall,
	    };
	    cells2.insert(p.clone(), new_v);
	}
	round += 1;
	cells_queue.push(cells2);
    }
    println!("{}", round);
}

fn main() {
    let contents = fs::read_to_string("day15.txt").unwrap();
    let code: Vec<i64> = contents
        .split(',')
        .map(|s| s.trim().parse::<i64>().unwrap())
        .collect();

    let mut state = State::new(&code);
    state.generate_cells();

    part1(&state.cells);
    part2(&state.cells);
}
