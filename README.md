# Dexterws Chess
## General information
This library makes use of bitboards for the move generation, this is not really necessary as speed is a negligible factor in the upcoming stages, however I have worked with them a bit before when creating an [MCTS Ataxx Engine](https://github.com/BlueKossa/mcts-ataxx) ([Original](https://github.com/crippa1337/GYARB)) so I thought it would be fun to implement in chess too.

## Install
Add the library to your project by adding the following to your `Cargo.toml`:
```toml
[dependencies]
dexterws-chess = { git = "https://github.com/INDA24PlusPlus/dexterws-chess.git" }
```

## Documentation
The documentation can be found [here](https://chess.trattkatt.se/dexterws_chess/index.html)

## Usage
The main thing this library consists of is the `Board` struct, which contains all the necessary functions to create a chess game.

### Examples
```rs
use dexterws_chess::game::{Board, GameResult};

// new creates a default chess board
let mut board = Board::new();

// Get all playable moves in a Vec
let playable_moves = board.all_moves();

// Get one of the playable moves and play it
let mv = playable_moves[3];
// As we know it is a legal move, this should never error and we can unwrap
// if you try to play an illegal move, it will return and Error
let result = board.play_move(mv).unwrap();

match result {
    GameResult::InProgress => { println!("Game still in progress!") },
    _ => { println!("Game is over!") }
}
```

```rs
use dexterws::game::{Board, Piece, Color};

let board = Board::new();

// Get all the pieces on the board, in a [Option<Piece, Color>; 64]
let pieces = board.get_all_pieces();
```

## Credits
A large portion of the logic for the bitboard logic etc can be found on the [Chess Programming Wiki](https://www.chessprogramming.org/Main_Page).

The [Stockfish Engine Dev Discord](https://discord.com/invite/GWDRS3kU6R) has also been very helpful.

## Questions
If you have any questions regarding the library, do not hesitate to either open an issue or email me at `dexterws@kth.se`