//const input: (i64, i64) = (356261, 846303);
//const input1: (i64, i64) = (356666, 777777);

fn get_counts() -> [[i64; 6]; 10] {
    let mut opt = [[0i64; 6]; 10];
    for i in 0..5 {
        opt[9][i] = 1;
    }
    for n in 0..8 {
        opt[n][0] = 10 - n as i64;
    }
    for n in (0..8).rev() {
        let a = opt[n + 1][1];
        let b = 1;
        let c = opt[n + 1][0];
        opt[n][1] = a + b + c;
    }
    for i in 2..5 {
        for n in (0..8).rev() {
            let a = opt[n + 1][i];
            let b = opt[n][i - 2];
            let c = opt[n + 1][i - 1];
            opt[n][i] = a + b + c;
        }
    }
    opt
}

fn main() {
    let opt = get_counts();
    println!("{:?}", opt);
}
