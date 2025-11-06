

# Myg Chess Game


### Getting the code

```smalltalk
Metacello new
	repository: 'github://UnivLille-Meta/Chess:main';
	baseline: 'MygChess';
	onConflictUseLoaded;
	load.
```

### Using it

```smalltalk
board := MyChessGame freshGame.
board size: 800@600.
space := BlSpace new.
space root addChild: board.
space pulse.
space resizable: true.
space show.
```
# Task - Sofia Demchuk and Yuliia Los

## Game Replay

**Goal:** Practice refactoring and debugging

A common practice between chess players is to study old games.
Fortunately, many old games exist digitalized in PGN format, and the engine has initial support for it!
You have to implement a replay feature, where a game is imported and the player move the game forward/backwards given the list of moves.
As any *crazy* feature, the original developer (Guille P) did not prepare the engine for this.
But you can do it.

Questions and ideas that can help you in the process:
- How should you extend the UI to implement this feature?
- What would happen if the PGN support is not complete/perfect? How can you manage to improve it?

# Initial Situation

At the beginning of this task, the project already contained a functional chess engine and a basic visual board implemented with Bloc and Toplo. The core game logic — including piece movement, turns, and basic rule validation — was already present and working correctly for manual play.

However, there was no functionality to load or replay existing games. The application could only start new matches and handle player moves in real time. Studying and rewatching past games, which is a key feature for chess analysis, was not supported at all.

The codebase included an early version of a PGN (Portable Game Notation) parser, but it was incomplete and unreliable. Some token types were missing, and the parsing logic did not handle special cases or malformed input gracefully. As a result, even valid PGN strings sometimes caused errors or partial imports.

The user interface also lacked separation between different interaction modes. It only supported a single “Game” mode where players could move pieces manually. There were no buttons, menus, or controls to step through a recorded match or to manage replay navigation.

Before starting the implementation, we had to spend time understanding the existing codebase — its structure, class hierarchy, and interaction between the model and UI. This deep analysis was essential for identifying where to integrate the new functionality. It also helped us build a stronger mental model of the project’s architecture, which made it much easier to refactor and extend the existing code later on.

# Refactoring and Implementation Tasks

When implementing the Game Replay feature, the most important step was refactoring the existing codebase to make it modular and extendable.To support the new functionality, we had to restructure several parts of the system — mainly the MyChessGame class and the UI initialization logic.

**Splitting Game and Replay Modes**

Originally, the initializeModeMenu method contained only a “Game” mode.
We refactored it to support a second mode — Replay.
This required both new menu entries and new event handlers.

_Old version (simplified):_
```smalltalk
MyChessGame >> initializeModeMenu
	| menuBar modeMenu gameItem |
	menuBar := ToMenuBar new.
	modeMenu := ToMenu new labelText: 'Mode'.
	gameItem := ToMenuItem new
		labelText: 'Game';
		whenClickedDo: [ self newGame ].
	modeMenu addItem: gameItem.
	self addChild: menuBar.
```
_Refactored version with Replay support:_
```smalltalk
MyChessGame >> initializeModeMenu
	| menuBar modeMenu modeGroup autoItem replayItem |
	menuBar := ToMenuBar new
		hMatchParent;
		background: (Color veryLightGray alpha: 0.95);
		yourself.

	modeMenu := ToMenu new
		labelText: 'Mode';
		padding: (BlInsets all: 10).

	modeGroup := ToCheckableGroup new.

	autoItem := ToRadioMenuItem new
		labelText: 'Game';
		id: #game;
		checked: mode = #game;
		whenClickedDo: [
			mode := #game.
			self newGame ].

	replayItem := ToRadioMenuItem new
		labelText: 'Replay';
		id: #replay;
		checked: mode = #replay;
		whenClickedDo: [
			mode := #replay.
			self buildReplayButtons.
			lastReplayPGN
				ifNil: [ self askAndLoadReplay ]
				ifNotNil: [
					self initializeFromPGN: (MyPGNParser forString: lastReplayPGN).
					self toEndOfReplay.
					self updateReplayList ] ].

	modeGroup registerAll: { autoItem. replayItem }.
	modeMenu addAllItems: modeGroup registeredCheckables.
	menuBar addMenu: modeMenu.
	self addChild: menuBar.
```
**Adding Input and Replay Logic**

We added new helper methods to make the replay system work.
Prompts the user to paste a PGN or SAN game manually:
```smalltalk
MyChessGame >> askAndLoadReplay
	| input pgn |
	input := UIManager default multiLineRequest: 'Paste PGN or SAN:'.
	input ifNil: [ ^ self ].

	pgn := self class normalizeToPGN: input.
	self initializeFromPGN: (MyPGNParser forString: pgn).
	replayString := pgn.
	lastReplayPGN := pgn.
	self ensureGrid8x8.
	self toEndOfReplay.
	self updateReplayList.
	self space ifNotNil: [ self space pulse ].
```
**Centralizing Replay Control**

Instead of hardcoding button logic in the UI, we created clean, self-contained methods:
```smalltalk
MyChessGame >> replayNext
	replayIndex < self replayMoves size ifTrue: [
		replayIndex := replayIndex + 1.
		self applyMove: (self replayMoves at: replayIndex) ].

MyChessGame >> replayPrevious
	replayIndex > 0 ifTrue: [
		replayIndex := replayIndex - 1.
		self rebuildPositionUpTo: replayIndex ].

MyChessGame >> toEndOfReplay
	1 to: (self replayMoves size) do: [ :i | self replayNext ].
```
Introducing Normalization for Unstable PGN Input

The PGN parser often failed on minimal SAN strings like
1. e4 e5 2. Nf3 Nc6 3. Bb5 a6.

We added a new class-side method to wrap incomplete inputs in valid PGN format automatically:
```smalltalk
MyChessGame class >> normalizeToPGN: aString
	(aString includesSubstring: '[Event') ifTrue: [ ^ aString ].
	^ String streamContents: [ :s |
		s nextPutAll: '[Event "Manual"]'; cr;
		  nextPutAll: '[Result "*"]'; cr; cr;
		  nextPutAll: aString;
		  (aString includesSubstring: '1-0')
			  ifFalse: [ s space; nextPutAll: '1-0' ] ].
```
**Creating a Clear UI Layout**

```smalltalk
MyChessGame >> buildReplayButtons
	controlPane := BlElement new.
	controlPane layout: BlLinearLayout horizontal.
	controlPane padding: (BlInsets all: 8).

	controlPane addChild: (ToButton new
		labelText: 'Previous';
		whenClickedDo: [ self replayPrevious ]).
	controlPane addChild: (ToButton new
		labelText: 'Next';
		whenClickedDo: [ self replayNext ]).
	self addChild: controlPane.
```
# Challenges and Limitations

During the development of the replay feature, several technical and design difficulties appeared.
Some of them were expected (like missing parser features), while others came from how the game state was originally implemented.

**Rebuilding the Board on Each “Previous” Step**

One of the most important trade-offs was how to handle moving backward in the replay.
Currently, every time the user clicks “Previous”, the program recreates a new board from the initial position and replays all moves up to the required index.
_In other words, each backward step works like this:_
```smalltalk
MyChessGame >> replayPrevious
	replayIndex > 0 ifTrue: [
		replayIndex := replayIndex - 1.
		self rebuildPositionUpTo: replayIndex ].
```
This approach is not very efficient, because the board state is rebuilt from scratch every time.
However, it has one big advantage: we don’t need to store separate snapshots of the board after each move.
It keeps the implementation simple and avoids writing additional code for saving intermediate states or piece positions.

From a software-engineering point of view, this is a clear trade-off between optimization and maintainability — the system is slower, but much easier to reason about and debug.
Unrecognized PGN Symbols and Special Moves

Another challenge came from the PGN notation itself.
When testing real PGN examples or even the sample data inside MyPGNParser, the parser encountered several tokens that were not supported by the base implementation — such as:
	•	O-O or O-O-O (castling),
	•	moves with checks, like Qh5+ or Re1+.

The original code was not able to recognize or process these tokens, which caused the parser to stop or raise an exception.
I tried to extend the parser to handle castling moves, but it turned out to be quite complex — it required modifying both the token parser and the move generator to update both the king and rook positions simultaneously.

As a temporary solution, I focused on simplified PGN inputs that only use standard moves (without castling, checks, or promotions).
This allowed the replay system to stay stable and demonstrate the main functionality without rewriting the entire move engine.

**Missing File Dialog Support**

Another limitation is related to file input.
Ideally, users should be able to load a .pgn file directly using a file picker dialog, something like:
```smalltalk
UIManager default chooseFileMatching: '*.pgn'
```
Unfortunately, such functionality is not working reliably in Pharo 12.The dialog either did not open or returned invalid file references in the Bloc environment.
Because of that, the replay system currently uses a hardcoded PGN example stored directly in the method startReplay,
or alternatively prompts the user for text input through a simple multiline dialog.
