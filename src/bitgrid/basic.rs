use super::*; 
pub const GRID_LENGTH: usize = 16;
pub const GRID_SENTINEL: GridLocation = 0b10000;
pub type GridLocation = usize;

/// Represents only legal states of Hive with Pillbug Mosquito and Ladybug grids 
/// correctly. TODO: may have to refactor tests to account for this.
///
/// See the documentation of the bit grid's AxialBitboard to
/// understand how the grid is represented at the bit level
///
/// Zooming out, the grid is represented instead as
/// a 4x4 grid of AxialBitboards
///
/// The grid is represented as follows:
/// ```
///   15 14 13 12
///   11 10 09 08
///   07 06 05 04
///   03 02 01 00
/// ```
///
/// The center is assigned to board 10 at the 28th bit
pub struct BasicBitGrid {
    pub queens: [(GridLocation, AxialBitboard); 2],
    pub beetles: [(GridLocation, AxialBitboard); 4],
    pub spiders: [(GridLocation, AxialBitboard); 4],
    pub grasshoppers: [(GridLocation, AxialBitboard); 6],
    pub ants: [(GridLocation, AxialBitboard); 6],
    pub pillbugs: [(GridLocation, AxialBitboard); 2],
    pub ladybugs: [(GridLocation, AxialBitboard); 2],
    pub mosquitos: [(GridLocation, AxialBitboard); 2],
    pub stacks : BasicBitStack
}


impl BasicBitGrid {
    pub fn new() -> Self {
        BasicBitGrid {
            queens: [(GRID_SENTINEL, AxialBitboard::from_u64(0)); 2],
            beetles: [(GRID_SENTINEL, AxialBitboard::from_u64(0)); 4],
            spiders: [(GRID_SENTINEL, AxialBitboard::from_u64(0)); 4],
            grasshoppers: [(GRID_SENTINEL, AxialBitboard::from_u64(0)); 6],
            ants: [(GRID_SENTINEL, AxialBitboard::from_u64(0)); 6],
            pillbugs: [(GRID_SENTINEL, AxialBitboard::from_u64(0)); 2],
            ladybugs: [(GRID_SENTINEL, AxialBitboard::from_u64(0)); 2],
            mosquitos: [(GRID_SENTINEL, AxialBitboard::from_u64(0)); 2],
            stacks: BasicBitStack::new()
        }
    }

}
// TODO: 
// add piece
// remove piece
// direction (without overflow)
// direction (with overflow)

#[test]
fn test_basic_bitgrid_size() {
    // Test that the size of the basic bitboard is kinda big
    // but isn't insane
    assert_eq!(std::mem::size_of::<BasicBitGrid>(), 464);
}
