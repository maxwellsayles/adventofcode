use std::fs;

fn main() {
    let contents: Vec<i32> = fs::read_to_string("day16.txt")
	.unwrap()
	.trim()
	.chars()
	.map(|c| c.to_digit(10).unwrap() as i32)
	.collect();
    println!("{:?}", contents);
}
