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

fn bench_stress_test_beetle(c: &mut Criterion) {
    use PieceType::*;
    use PieceColor::*;

    let mut undoer = Undoer::new();
    let untimed_run = undoer.untimed_run(200, 47);
    let tracked_run = undoer.track_piece(untimed_run, Piece::new(Beetle, White));

    c.bench_function("stress_test_beetle", |b| 
        b.iter(|| undoer.beetle_run(tracked_run.clone()))
    );
}

fn bench_stress_test_ant(c: &mut Criterion) {
    use PieceType::*;
    use PieceColor::*;

    let mut undoer = Undoer::new();
    let untimed_run = undoer.untimed_run(200, 48);
    let tracked_run = undoer.track_piece(untimed_run, Piece::new(Ant, White));

    c.bench_function("stress_test_ant", |b| 
        b.iter(|| undoer.ant_run(tracked_run.clone()))
    );
}
fn bench_stress_test_spider(c: &mut Criterion) {
    use PieceType::*;
    use PieceColor::*;

    let mut undoer = Undoer::new();
    let untimed_run = undoer.untimed_run(200, 49);
    let tracked_run = undoer.track_piece(untimed_run, Piece::new(Spider, White));

    c.bench_function("stress_test_spider", |b| 
        b.iter(|| undoer.spider_run(tracked_run.clone()))
    );
}
fn bench_stress_test_pillbug(c: &mut Criterion) {
    use PieceType::*;
    use PieceColor::*;

    let mut undoer = Undoer::new();
    let untimed_run = undoer.untimed_run(200, 50);
    let tracked_run = undoer.track_piece(untimed_run, Piece::new(Pillbug, White));

    c.bench_function("stress_test_pillbug", |b| 
        b.iter(|| undoer.pillbug_run(tracked_run.clone()))
    );
}
fn bench_stress_test_ladybug(c: &mut Criterion) {
    use PieceType::*;
    use PieceColor::*;

    let mut undoer = Undoer::new();
    let untimed_run = undoer.untimed_run(200, 51);
    let tracked_run = undoer.track_piece(untimed_run, Piece::new(Ladybug, White));

    c.bench_function("stress_test_ladybug", |b| 
        b.iter(|| undoer.ladybug_run(tracked_run.clone()))
    );
}
fn bench_stress_test_mosquito(c: &mut Criterion) {
    use PieceType::*;
    use PieceColor::*;

    let mut undoer = Undoer::new();
    let untimed_run = undoer.untimed_run(200, 52);
    let tracked_run = undoer.track_piece(untimed_run, Piece::new(Mosquito, White));

    c.bench_function("stress_test_mosquito", |b| 
        b.iter(|| undoer.mosquito_run(tracked_run.clone()))
    );
}
fn bench_stress_test_grasshopper(c: &mut Criterion) {
    use PieceType::*;
    use PieceColor::*;

    let mut undoer = Undoer::new();
    let untimed_run = undoer.untimed_run(200, 53);
    let tracked_run = undoer.track_piece(untimed_run, Piece::new(Grasshopper, White));

    c.bench_function("stress_test_grasshopper", |b| 
        b.iter(|| undoer.grasshopper_run(tracked_run.clone()))
    );
}







criterion_group!(
    benches,
    bench_stress_test_queen, 
    bench_stress_test_beetle,
    bench_stress_test_spider,
    bench_stress_test_ladybug,
    bench_stress_test_mosquito,
    bench_stress_test_pillbug,
    bench_stress_test_grasshopper,
    bench_stress_test_ant,
);
criterion_main!(benches);
