const INPUT: (i64, i64) = (356261, 846303);

fn is_valid(x: i64) -> bool {
    let digits: Vec<_> = x.to_string()
        .chars()
        .map(|d| d.to_digit(10).unwrap())
        .collect();
    let n = digits.len();
    let mut neighbor = false;
    for i in 1..n {
        if digits[i-1] > digits[i] {
            return false;
        }
        if digits[i-1] == digits[i] {
            neighbor = true;
        }
    }
    neighbor
}

fn is_valid2(x: i64) -> bool {
    let digits: Vec<_> = x.to_string()
        .chars()
        .map(|d| d.to_digit(10).unwrap())
        .collect();
    let n = digits.len();
    let mut neighbor = std::u32::MAX;
    for i in 1..n {
        if digits[i-1] > digits[i] {
            return false;
        }
        if digits[i-1] == digits[i] && neighbor == std::u32::MAX {
            neighbor = digits[i];
        }
        if i >= 2 &&
            digits[i-1] == digits[i] &&
            digits[i-2] == digits[i-1] &&
            digits[i] == neighbor {
                neighbor = std::u32::MAX;
            }
    }
    neighbor != std::u32::MAX
}

fn main() {
    let mut count: i64 = 0;
    for x in INPUT.0..=INPUT.1 {
        if is_valid(x) {
            count += 1
        }
    }
    println!("{}", count);

    count = 0;
    for x in INPUT.0..=INPUT.1 {
        if is_valid2(x) {
            count += 1
        }
    }
    println!("{}", count);
}
