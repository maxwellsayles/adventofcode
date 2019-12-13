use std::fs;

#[derive(Debug)]
enum Dir {
    Up,
    Down,
    Right,
    Left,
}

type Point = (i32, i32);
type Step = (Dir, i32);

fn char_to_dir(c: char) -> Dir {
    match c {
        'U' => Dir::Up,
        'D' => Dir::Down,
        'L' => Dir::Left,
        'R' => Dir::Right,
        _ => panic!("Unrecognized char {}", c),
    }
}

fn parse_step(s: &str) -> Step {
    let d = char_to_dir(s.chars().next().unwrap());
    let i = s.get(1..)
        .unwrap()
        .to_string()
        .parse::<i32>()
        .unwrap();
    (d, i)
}

fn parse_line(s: &str) -> Vec<Step> {
    s.to_string()
        .split(",")
        .map(parse_step)
        .collect()
}

fn move_point(p: Point, s: Step) -> Point {
    match s.0 {
        Dir::Up => (p.0, p.1 - s.1),
        Dir::Down => (p.0, p.1 + s.1),
        Dir::Left => (p.0 - s.1, p.1),
        Dir::Right => (p.0 + s.1, p.1),
    }
}

fn main() {
    let f = fs::read_to_string("day03.txt")
        .unwrap();
    let lines = f.lines()
        .map(parse_line);

    for x in lines {
        println!("{:?}", x);
    }
}
