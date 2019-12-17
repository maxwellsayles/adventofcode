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

fn make_points(p: &Point, s: &Step) -> Vec<Point> {
    match s.0 {
        Dir::Up => ((p.1 - s.1)..=p.1).rev()
            .map(|y| (p.0, y))
            .collect(),
        Dir::Down => (p.1..=(p.1 + s.1))
            .map(|y| (p.0, y))
            .collect(),
        Dir::Left => ((p.0 - s.1)..=p.0).rev()
            .map(|x| (x, p.1))
            .collect(),
        Dir::Right => (p.0..=(p.0 + s.1))
            .map(|x| (x, p.1))
            .collect(),
    }
}

fn main() {
    let f = fs::read_to_string("day03.txt")
        .unwrap();
    let lines = f.lines()
        .map(parse_line);

    for x in (2..=10).rev() {
        println!("{:?}", x);
    }
}
