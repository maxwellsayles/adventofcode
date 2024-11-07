use std::collections::{ HashMap, HashSet };

const INPUT: [&str; 5] = [
    "##.#.",
    ".#.##",
    ".#...",
    "#..#.",
    ".##..",
];

#[inline(always)]
fn point_to_mask(x: usize, y: usize) -> u32 {
    1u32 << (x + y * 5)
}

#[inline(always)]
fn is_bug(state: u32, x: usize, y: usize) -> bool {
    if x >= 5 || y >= 5 {
	return false;
    }
    state & point_to_mask(x, y) != 0
}

#[inline(always)]
fn is_bug_i32(state: u32, x: i32, y: i32) -> bool {
    return is_bug(state, x as usize, y as usize);
}

fn input_to_state(input: &[&str; 5]) -> u32 {
    let mut res = 0u32;
    for (y, s) in input.iter().enumerate() {
	for (x, c) in s.chars().enumerate() {
	    if c == '#' {
		let i = point_to_mask(x, y);
		res |= i;
	    }
	}
    }
    res
}

#[allow(dead_code)]
fn print_state(state: u32) {
    for y in 0..5 {
	for x in 0..5 {
	    print!("{}", if is_bug(state, x, y) { '#' } else { '.' });
	}
	println!();
    }
}

fn step(state: u32) -> u32 {
    let mut res = 0;
    for y in 0..5 {
	for x in 0..5 {
	    let d = is_bug(state, x, y + 1);
	    let u = is_bug(state, x, y - 1);
	    let l = is_bug(state, x - 1, y);
	    let r = is_bug(state, x + 1, y);
	    let c = is_bug(state, x, y);
	    let cnt =
		if d { 1 } else { 0 } +
		if u { 1 } else { 0 } +
		if l { 1 } else { 0 } +
		if r { 1 } else { 0 };
	    let mask = point_to_mask(x, y);
	    if c {
		res |= if cnt == 1 { mask } else { 0 };
	    } else {
		res |= if cnt == 1 || cnt == 2 { mask } else { 0 };
	    }
	}
    }
    res
}

fn part1() {
    let mut state = input_to_state(&INPUT);
    let mut visited = HashSet::new();
    while !visited.contains(&state) {
	visited.insert(state);
	state = step(state);
    }
    println!("{}", state);
}

// Map from Z depth to X,Y mask of bug values.
type State3 = HashMap<i32, u32>;

fn is_bug3(state: &State3, x: i32, y: i32, z: i32) -> bool {
    match state.get(&z) {
	None => false,
	Some(state2) => is_bug(*state2, x as usize, y as usize),
    }
}

fn count_adjacent3(state: &State3, x: i32, y: i32, z: i32) -> i32 {
    let mut res = 0;

    // Count adjacent on the current level.
    {
	let state2 = *state.get(&z).unwrap_or(&0);
	res += if is_bug_i32(state2, x - 1, y) { 1 } else { 0 };
	res += if is_bug_i32(state2, x + 1, y) { 1 } else { 0 };
	res += if is_bug_i32(state2, x, y - 1) { 1 } else { 0 };
	res += if is_bug_i32(state2, x, y + 1) { 1 } else { 0 };
    }

    // Count adjacent on level below.
    {
	let state2 = *state.get(&(z + 1)).unwrap_or(&0);
	let ps = match (x, y) {
	    (2, 1) => (0..5).map(|x| (x, 0)).collect::<Vec<_>>(),
	    (2, 3) => (0..5).map(|x| (x, 4)).collect::<Vec<_>>(),
	    (1, 2) => (0..5).map(|y| (0, y)).collect::<Vec<_>>(),
	    (3, 2) => (0..5).map(|y| (4, y)).collect::<Vec<_>>(),
	    _ => vec![],
	};
	for p in ps {
	    res += if is_bug_i32(state2, p.0, p.1) { 1 } else { 0 };
	}
    }

    // Count adjacent on level above.
    {
	let state2 = *state.get(&(z - 1)).unwrap_or(&0);
	let ps = match (x, y) {
	    (0, 0) => vec![(2, 1), (1, 2)],
	    (4, 0) => vec![(2, 1), (3, 2)],
	    (0, 4) => vec![(2, 3), (1, 2)],
	    (4, 4) => vec![(2, 3), (3, 2)],
	    (0, _) => vec![(1, 2)],
	    (4, _) => vec![(3, 2)],
	    (_, 0) => vec![(2, 1)],
	    (_, 4) => vec![(2, 3)],
	    _ => vec![],
	};
	for p in ps {
	    res += if is_bug_i32(state2, p.0, p.1) { 1 } else { 0 };
	}
    }

    res
}

fn step3(state: &State3) -> State3 {
    let minz = state.keys().min().unwrap_or(&0);
    let maxz = state.keys().max().unwrap_or(&0);
    let mut res = HashMap::new();
    for z in (minz - 1) ..= (maxz + 1) {
	let mut state2 = 0;
	for y in 0 .. 5 {
	    for x in 0 .. 5 {
		let cnt = count_adjacent3(&state, x, y, z);
		let mask = point_to_mask(x as usize, y as usize);
		if is_bug3(&state, x, y, z) {
		    state2 |= if cnt == 1 { mask } else { 0 };
		} else {
		    state2 |= if cnt == 1 || cnt == 2 { mask } else { 0 };
		}
	    }
	}
	// Clear the center tile (coz it shouldn't have a bug).
	state2 &= !point_to_mask(2, 2);
	res.insert(z as i32, state2);
    }
    res
}

fn count_bugs(state: u32) -> usize {
    let mut res = 0;
    let mut mask = 1;
    for _ in 0..25 {
	res += if state & mask == 0 { 0 } else { 1 };
	mask <<= 1;
    }
    res
}

fn count_bugs3(state: &State3) -> usize {
    let mut res = 0;
    for v in state.values() {
	res += count_bugs(*v);
    }
    res
}

fn part2() {
    let init_state = input_to_state(&INPUT);
    let mut state = HashMap::from([(0, init_state)]);
    for _ in 0..200 {
	state = step3(&state);
    }
    println!("{}", count_bugs3(&state));
}

fn main() {
    part1();
    part2();
}
