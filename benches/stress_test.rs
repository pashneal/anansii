use criterion::{criterion_group, criterion_main, Criterion};
use anansii::generator::undoer::Undoer;
use anansii::piece::*;


fn bench_stress_test_queen(c: &mut Criterion) {
    use PieceType::*;
    use PieceColor::*;

    let mut undoer = Undoer::new();
    let untimed_run = undoer.untimed_run(200, 46);
    let tracked_run = undoer.track_piece(untimed_run, Piece::new(Queen, White));

    c.bench_function("stress_test_queen", |b| 
        b.iter(|| undoer.queen_run(tracked_run.clone()))
    );
}

criterion_group!(benches, bench_stress_test_queen);
criterion_main!(benches);
