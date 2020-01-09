//const input: (i64, i64) = (356261, 846303);
//const input1: (i64, i64) = (356666, 777777);

fn get_counts(xs: &[usize; 6]) -> i64 {
    let mut opt = [[0i64; 6]; 10];

    for n in 0..=xs[0] {
        opt[n][0] = 1 + n as i64;
    }
    for i in 1..=5 {
        opt[0][i] = 1;
    }
    for n in 1..=xs[1] {        
        let a = opt[n - 1][1];
        let b = if n <= xs[0] { 1 } else { 0 };
        let c = 0;
        opt[n][1] = a + b + c;
    }
    for n in (xs[1] + 1)..=9 {
        opt[n][1] = opt[n - 1][1];
    }
    for i in 2..=5 {
        for n in 1..=xs[i] {
            let a = opt[n - 1][i];
            let b = if n <= xs[i - 1] {
                opt[n][i - 2]
            } else {
                0
            };
            let c = opt[n - 1][i - 1];
            opt[n][i] = a + b + c;
        }
        for n in (xs[i] + 1)..=9 {
            opt[n][i] = opt[n - 1][i];
        }
    }
    // for n in 0..=9 {
    //     println!("{:?}", opt[n]);
    // }
    opt[9][5]
}

fn main() {
    let xs = [3, 5, 6, 6, 6, 6];
    let x = get_counts(&xs);
    println!("{}", x);
    let ys = [8, 8, 8, 8, 8, 8];
    let y = get_counts(&ys);
    println!("{}", y);
    println!("{}", y - x + 1);
}
