use std::cmp::Ordering;
use std::collections::HashMap;
use std::fs;
use std::io::{stdout, Write};
use std::thread;
use std::time::{Duration};
use termion::async_stdin;
use termion::event::Key;
use termion::input::TermRead;
use termion::raw::IntoRawMode;

use crate::intcode_computer::IntcodeComputer;

mod intcode_computer;

const MAX_X: i32 = 41;
const MAX_Y: i32 = 23;

#[allow(dead_code)]
fn part1(code: &Vec<i64>) {
    let mut comp = IntcodeComputer::new(Vec::new(), &code);
    comp.run();
    let mut i = 0;
    let mut cnt = 0;
    while let Some(v) = comp.remove_output() {
	if i == 2 && v == 2 {
	    cnt += 1;
	}
	i = (i + 1) % 3;
    }
    println!("{}", cnt);
}

fn render(comp: &mut IntcodeComputer) -> HashMap<(i32, i32), i32> {
    let mut display = HashMap::new();
    while let Some(x) = comp.remove_output() {
	let y = comp.remove_output().unwrap();
	let v = comp.remove_output().unwrap();
	let p = (x as i32, y as i32);
	display.insert(p, v as i32);
    }
    display
}

fn print_display(stdout: &mut dyn Write, display: &HashMap<(i32, i32), i32>) {
    write!(stdout, "{}", termion::cursor::Goto(1, 1)).unwrap();
    for y in 0i32..=MAX_Y {
	for x in 0i32..=MAX_X {
	    if let Some(v) = display.get(&(x, y)) {
		let c = match v {
		    1 => '#',
		    2 => '*',
		    3 => '-',
		    4 => 'o',
		    _ => ' ',
		};
		write!(
		    stdout,
		    "{}{}",
		    termion::cursor::Goto((x + 1) as u16, (y + 3) as u16),
		    c,
		).unwrap();
	    }
	}
    }

    let opt_score = display.get(&(-1, 0));
    match opt_score {
	Some(score) => {
	    write!(
		stdout,
		"{}{}Score: {}",
		termion::cursor::Goto(1, 1),
		termion::clear::CurrentLine,
		score,
	    ).unwrap();
	},
	_ => {}
    }

    write!(stdout, "{}", termion::cursor::Goto(1, 2)).unwrap();
    stdout.flush().unwrap();
}

#[allow(dead_code)]
fn part2(code: &Vec<i64>) {
    let mut stdin = async_stdin().keys();
    let mut stdout = stdout().lock().into_raw_mode().unwrap();

    write!(stdout,
           "{}{}",
           termion::clear::All,
           termion::cursor::Goto(1, 1))
            .unwrap();
    stdout.flush().unwrap();

    let mut comp = IntcodeComputer::new(Vec::new(), &code);
    comp.poke_mem(0, 2);

    while !comp.is_halted() {
	comp.run();

	let display = render(&mut comp);
	print_display(&mut stdout, &display);

	match stdin.next() {
	    Some(Ok(Key::Esc)) => break,
	    Some(Ok(Key::Left)) => comp.add_input(-1i64),
	    Some(Ok(Key::Right)) => comp.add_input(1i64),
	    _ => comp.add_input(0i64),
	}

	thread::sleep(Duration::from_millis(250));
    }
}

/**
 * Trick I got from:
 * https://github.com/berkgulmus/AdventOfCode2019/blob/master/Day13.2.py
 *
 * Automate the movement of the paddle to track the ball. Run full speed.
*/
#[allow(dead_code)]
fn part2_no_hud(code: &Vec<i64>) {
    let mut comp = IntcodeComputer::new(Vec::new(), &code);
    comp.poke_mem(0, 2);

    let mut score = 0;
    while !comp.is_halted() {
	comp.run();

	let mut ball = -1;
	let mut paddle = -1;
	while let Some(x) = comp.remove_output() {
	    let y = comp.remove_output().unwrap();
	    let v = comp.remove_output().unwrap();
	    if x == -1 && y == 0 {
		score = v;
	    } else if v == 3 {
		paddle = x;
	    } else if v == 4 {
		ball = x;
	    }
	}

	let input = match paddle.cmp(&ball) {
	    Ordering::Less => 1,
	    Ordering::Greater => -1,
	    Ordering::Equal => 0,
	};
	comp.add_input(input);
    }
    println!("{}", score);
}

fn main() {
    let contents = fs::read_to_string("day13.txt")
        .unwrap();
    let code: Vec<i64> = contents
        .split(',')
        .map(|s| s.trim().parse::<i64>().unwrap())
        .collect();

    // NOTE: part2 clears the screen so you cannot see the result of part1.
    part1(&code);
    // part2(&code);
    part2_no_hud(&code);
}
