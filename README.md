# Dexterws Chess
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
