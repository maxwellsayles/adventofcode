use std::collections::HashSet;
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

fn dist(p: &Point) -> i32 {
    p.0.abs() + p.1.abs()
}

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
        Dir::Up => ((p.1 - s.1)..p.1).rev()
            .map(|y| (p.0, y))
            .collect(),
        Dir::Down => ((p.1 + 1)..=(p.1 + s.1))
            .map(|y| (p.0, y))
            .collect(),
        Dir::Left => ((p.0 - s.1)..p.0).rev()
            .map(|x| (x, p.1))
            .collect(),
        Dir::Right => ((p.0 + 1)..=(p.0 + s.1))
            .map(|x| (x, p.1))
            .collect(),
    }
}

fn make_path(steps: &Vec<Step>) -> Vec<Point> {
    let mut p = (0, 0);
    let mut path: Vec<Point> = Vec::new();
    for s in steps {
        let mut ps = make_points(&p, &s);
        p = *ps.last().unwrap();
        path.append(&mut ps);
    }
    path
}

fn solve1(path0: &Vec<Point>, path1: &Vec<Point>) -> i32 {
    let hash0: HashSet<&Point> = path0.iter().collect();
    let hash1: HashSet<&Point> = path1.iter().collect();
    let common_points = hash0.intersection(&hash1);
    let p = common_points.min_by(|a, b| dist(a).cmp(&dist(b)))
        .unwrap();
    dist(p)
}

fn main() {
    let f = fs::read_to_string("day03.txt")
        .unwrap();
    let input: Vec<Vec<Step>> = f.lines()
        .map(parse_line)
        .collect();

    let path0 = make_path(&input[0]);
    let path1 = make_path(&input[1]);

    println!("{}", solve1(&path0, &path1));
}
