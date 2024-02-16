type V3 = (i32, i32, i32);

// Example 1 input.
// const INIT_MOONS: [V3; 4] = [
//     (-1, 0, 2),
//     (2, -10, -7),
//     (4, -8, 8),
//     (3, 5, -1),
// ];

// My puzzle input
const INIT_MOONS: [V3; 4] = [
    (5, -1, 5),
    (0, -14, 2),
    (16, 4, 0),
    (18, 1, 16),
];

#[derive(Clone, Debug)]
struct Moon {
    pos: V3,
    vel: V3,
}

impl Moon {
    fn total_energy(&self) -> i32 {
	let p = self.pos.0.abs() + self.pos.1.abs() + self.pos.2.abs();
	let v = self.vel.0.abs() + self.vel.1.abs() + self.vel.2.abs();
	p * v
    }
}

fn force(a: i32, b: i32) -> i32 {
    if a < b { 1 } else if a > b { -1 } else { 0 }
}

fn step_vel(moons: &Vec<Moon>) -> Vec<Moon> {
    let mut res = Vec::new();
    for moon1 in moons {
	let pos1 = moon1.pos;
	let mut vel = moon1.vel;
	for moon2 in moons {
	    let pos2 = moon2.pos;
	    vel.0 += force(pos1.0, pos2.0);
	    vel.1 += force(pos1.1, pos2.1);
	    vel.2 += force(pos1.2, pos2.2);
	}
	res.push(Moon { pos: moon1.pos, vel: vel });
    }
    res
}

fn step_pos(moons: &Vec<Moon>) -> Vec<Moon> {
    moons.iter().map(|moon| {
	let x = moon.pos.0 + moon.vel.0;
	let y = moon.pos.1 + moon.vel.1;
	let z = moon.pos.2 + moon.vel.2;
	Moon { pos: (x, y, z), vel: moon.vel }
    }).collect()
}

fn step(moons: &Vec<Moon>) -> Vec<Moon> {
    step_pos(&step_vel(moons))
}

fn total_energy(moons: &Vec<Moon>) -> i32 {
    moons.iter().map(|m| m.total_energy()).sum()
}

fn part1(input_moons: &Vec<Moon>) {
    let mut moons = input_moons.clone();
    for _ in 0..1000 {
	moons = step(&moons);
    }
    println!("{}", total_energy(&moons));
}

fn main() {
    let init_moons: Vec<Moon> = INIT_MOONS
	.iter()
	.map(|pos| { Moon { pos: *pos, vel: (0, 0, 0) } })
	.collect();

    part1(&init_moons);
}
