"
# Example

```
board := MyChessGame freshGame.
board size: 800@600.
space := BlSpace new.
space root addChild: board.
space pulse.
space resizable: true.
space show.
```
"
Class {
	#name : 'MyChessGame',
	#superclass : 'BlElement',
	#instVars : [
		'board',
		'currentPlayer',
		'whitePlayer',
		'blackPlayer',
		'moveCount',
		'movesLabel',
		'moveCountLabel',
		'moves',
		'dsp',
		'mode',
		'gamePane',
		'controlPane',
		'replayMoves',
		'currentMoveIndex',
		'replayIndex',
		'replayString',
		'lastReplayPGN'
	],
	#category : 'Myg-Chess-Core',
	#package : 'Myg-Chess-Core'
}

{ #category : 'instance creation' }
MyChessGame class >> freshGame [

	^ self fromFENString: 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1'
]

{ #category : 'instance creation' }
MyChessGame class >> fromFENString: aFENString [

	^ super new
		  initializeFromFENGame: (MyFENParser parse: aFENString);
		  yourself
]

{ #category : 'instance creation' }
MyChessGame class >> fromPGNString: aString [ [
	^ super new
		initializeFromPGN: (MyPGNParser forString: (self normalizeToPGN: aString));
		yourself
]
]

{ #category : 'instance creation' }
MyChessGame class >> new [
	
	self error: 'Please use one of the other constructors'
]

{ #category : 'instance creation' }
MyChessGame class >> normalizeToPGN: aString [
	| txt trimmed movesOnly resultToken |
	txt := (aString ifNil: ['']) asString.

	"Если это уже полноценный PGN (есть теги) — отдаем как есть"
	(txt includes: $[) ifTrue: [ ^ txt ].

	trimmed := txt trimBoth.
	"Если пользователь дописал результат — оставим, иначе подставим '1-0'"
	(#('1-0' '0-1' '1/2-1/2')
		detect: [:r | trimmed endsWith: r]
		ifNone: [ nil ])
		ifNil: [
			movesOnly := trimmed.
			resultToken := '1-0'  "можешь сменить на '0-1' или '1/2-1/2' по вкусу"
		]
		ifNotNil: [:r |
			movesOnly := (trimmed copyFrom: 1 to: trimmed size - r size) trimRight.
			resultToken := r
		].

	^ String streamContents: [:s |
		s nextPutAll: '[Event "Manual"]'; cr.
		s nextPutAll: '[Site "Local"]'; cr.
		s nextPutAll: '[Date "????.??.??"]'; cr.
		s nextPutAll: '[Round "-"]'; cr.
		s nextPutAll: '[White "White"]'; cr.
		s nextPutAll: '[Black "Black"]'; cr.
		s nextPutAll: '[Result "*"]'; cr; cr.
		s nextPutAll: movesOnly; space; nextPutAll: resultToken ]
]

{ #category : 'instance creation' }
MyChessGame class >> stripTrailingResultFrom: aString [
	| t res |
	t := aString trimRight.
	res := (#('1-0' '0-1' '1/2-1/2' '*')
		detect: [ :r | t endsWith: r ]
		ifNone: [ nil ]).
	res ifNil: [ ^ t ].
	^ (t copyFrom: 1 to: t size - res size) trimRight
]

{ #category : 'instance creation' }
MyChessGame >> askAndLoadReplay [
	| input pgn |
	input := UIManager default
		request: 'Paste PGN or SAN (e.g., "1. e4 e5 2. Nf3 Nc6")'
		initialAnswer: ''.
	input ifNil: [ ^ self ].  "Отмена"

	pgn := self class normalizeToPGN: input.
	self initializeFromPGN: (MyPGNParser forString: pgn).

	"сначала убеждаемся, что у доски корректная сетка 8×8"
	self ensureGrid8x8.

	"если хочешь стартовую позицию — закомментируй строку ниже"
	self toEndOfReplay.

	self space ifNotNil: [ self space pulse ].
]

{ #category : 'playing' }
MyChessGame >> checkForMate [

	whitePlayer king isCheckMated ifTrue: [ self halt: 'Black wins!' ].
	blackPlayer king isCheckMated ifTrue: [ self halt: 'White wins!' ].
]

{ #category : 'as yet unclassified' }
MyChessGame >> ensureGrid8x8 [ [
	| grid |
	grid := self layout.
	grid isNil ifTrue: [
		self layout: BlGridLayout new.
		grid := self layout ].

	(grid isKindOf: BlGridLayout) ifFalse: [
		self layout: BlGridLayout new.
		grid := self layout ].

	grid
		columnCount: 8;
		rowCount: 8.
]
]

{ #category : 'initialization' }
MyChessGame >> initialize [

	super initialize.
	self initializeFonts.
	mode := #game.
	moves := OrderedCollection new.
	whitePlayer := MyPlayer new white game: self.
	blackPlayer := MyPlayer new black game: self.
	currentPlayer := whitePlayer.
	self initializeModeMenu
]

{ #category : 'initialization' }
MyChessGame >> initializeControlPane [

	| rightButton leftButton |
	controlPane := ToElement new.
	controlPane layout: BlLinearLayout horizontal.
	controlPane layout cellSpacing: 10.
	controlPane hMatchParent.
	controlPane margin: (BlInsets all: 10).

	mode = #game
		ifTrue: [
			leftButton := ToButton new
				              labelText: 'New Game';
				              whenClickedDo: [ self newGame ].
			rightButton := ToButton new
				               labelText: 'Play';
				               whenClickedDo: [ self play ] ]
		ifFalse: [
			leftButton := ToButton new
				              labelText: 'Previous';
				              whenClickedDo: [ self replayPrevious ].
			rightButton := ToButton new
				               labelText: 'Next';
				               whenClickedDo: [ self replayNext ] ].

	controlPane
		addChild: leftButton;
		addChild: rightButton
]

{ #category : 'initialization' }
MyChessGame >> initializeFonts [

	"We use fonts to render the characters, and they should be loaded (and downloaded if not there yet)"
	| library |
	library := AeFTLibrary newInitialized.
	MyOpenChessDownloadedFont new
		downloadDirectory: AeFilesystemResources downloadedFontsDirectory;
		ensureDownloaded;
		firstFaceUsing: library.


	AeFontManager resetGlobalInstance.
	AeFontManager globalInstance scanDirectory:
		AeFilesystemResources fontsDirectory
]

{ #category : 'initialization' }
MyChessGame >> initializeFromFENGame: aFENGame [

   gamePane ifNotNil: [ self removeChild: gamePane ].
	controlPane ifNotNil: [ self removeChild: controlPane ].
	moveCount := aFENGame moveCount.
	board := MyChessBoard fromFENBoard: aFENGame board.
	board game: self.

	gamePane := ToElement new.
	gamePane constraintsDo: [ :c |
		c vertical fitContent.
		c horizontal matchParent ].
	gamePane layout: BlLinearLayout horizontal.
	gamePane layout cellSpacing: 10.
	gamePane margin: (BlInsets all: 20).
	gamePane matchParent.

	movesLabel := ToLabel new beMultiLine.
	movesLabel matchParent.

	gamePane addChild: board.
	gamePane addChild: movesLabel.

	self addChild: gamePane.
	self initializeControlPane.
	self addChild: controlPane
]

{ #category : 'initialization' }
MyChessGame >> initializeFromPGN: aPGN [

	| parsed tags movesList |
	parsed := aPGN parse.
	tags := parsed first.
	movesList := parsed second.

	replayMoves := OrderedCollection new.

	movesList do: [ :moveData |
		moveData size >= 2 ifTrue: [
			replayMoves add: moveData second.
			moveData size = 3 ifTrue: [ replayMoves add: moveData third ] ] ].

	replayIndex := 0.
	currentPlayer := whitePlayer.
	moves := OrderedCollection new.
	self initializeFromFENGame: (MyFENParser parse:
			 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1').
]

{ #category : 'initialization' }
MyChessGame >> initializeModeMenu [

	| menuBar modeMenu modeGroup autoItem replayItem |
	menuBar := ToMenuBar new
		           hMatchParent;
		           background: (Color veryLightGray alpha: 0.95);
		           border:
			           (BlBorder paint: (Color gray alpha: 0.3) width: 1);
		           vFitContent;
		           yourself.
	modeMenu := ToMenu new
		            labelText: 'Mode';
		            padding: (BlInsets all: 10).
	modeGroup := ToCheckableGroup new.
	autoItem := ToRadioMenuItem new
		            labelText: 'Game';
		            id: #auto;
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
			              replayString := '[Event "F/S Return Match"]
[Site "Belgrade, Serbia JUG"]
[Date "1992.11.04"]
[Round "29"]
[White "Fischer, Robert J."]
[Black "Spassky, Boris V."]
[Result "1/2-1/2"]

1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. Ba4 Nf6 5. O-O Be7 6. Re1 b5 7. Bb3 d6 8. c3
O-O 9. h3 Nb8 10. d4 Nbd7 11. c4 c6 12. cxb5 axb5 13. Nc3 Bb7 14. Bg5 b4 15.
Nb1 h6 16. Bh4 c5 17. dxe5 Nxe4 18. Bxe7 Qxe7 19. exd6 Qf6 20. Nbd2 Nxd6 21.
Nc4 Nxc4 22. Bxc4 Nb6 23. Ne5 Rae8 24. Bxf7+ Rxf7 25. Nxf7 Rxe1+ 26. Qxe1 Kxf7
27. Qe3 Qg5 28. Qxg5 hxg5 29. b3 Ke6 30. a3 Kd6 31. axb4 cxb4 32. Ra5 Nd5 33.
f3 Bc8 34. Kf2 Bf5 35. Ra7 g6 36. Ra6+ Kc5 37. Ke1 Nf4 38. g3 Nxh3 39. Kd2 Kb5
40. Rd6 Kc5 41. Ra6 Nf2 42. g4 Bd3 43. Re6 1/2-1/2'.
			              self initializeFromPGN: (MyPGNParser forString: replayString) ].
	modeGroup registerAll: {
			autoItem.
			replayItem }.

	modeMenu addAllItems: modeGroup registeredCheckables.
	menuBar addMenu: modeMenu.
	self addChild: menuBar.

	self layout: BlLinearLayout vertical.
	self constraintsDo: [ :aLayoutConstraints |
		aLayoutConstraints horizontal fitContent.
		aLayoutConstraints vertical fitContent ]
]

{ #category : 'public' }
MyChessGame >> move: piece to: square [

	piece moveTo: square.
	self recordMovementOf: piece to: square
]

{ #category : 'instance creation' }
MyChessGame >> newGame [

   self initializeFromFENGame: (MyFENParser parse: 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1').
]

{ #category : 'accessing' }
MyChessGame >> pieces [

	^ board pieces
]

{ #category : 'playing' }
MyChessGame >> play [

	currentPlayer play.
	currentPlayer = whitePlayer
		ifTrue: [ currentPlayer := blackPlayer ]
		ifFalse: [ 
			moveCount := moveCount + 1.
			currentPlayer := whitePlayer ].
	self checkForMate
]

{ #category : 'moves' }
MyChessGame >> recordMovementOf: aPiece to: aSquare [

	| whiteMove blackMove moveNumber tableText |
	moves add: aPiece id , aSquare name. "Формуємо табличку по два ходи на рядок"
	moveNumber := 1.
	tableText := String streamContents: [ :stream |
		             1 to: moves size by: 2 do: [ :i |
			             whiteMove := moves at: i.
			             blackMove := i + 1 <= moves size
				                          ifTrue: [ moves at: i + 1 ]
				                          ifFalse: [ '' ].
			             stream
				             nextPutAll: moveNumber asString;
				             tab;
				             nextPutAll: whiteMove;
				             tab;
				             nextPutAll: blackMove;
				             cr.
			             moveNumber := moveNumber + 1 ] ].
	movesLabel text: tableText
]

{ #category : 'instance creation' }
MyChessGame >> replayMoves [
	^ replayMoves ifNil: [ #() ]
]

{ #category : 'as yet unclassified' }
MyChessGame >> replayNext [

	| moveStr piece destSquare destName pieceId |
	replayIndex < replayMoves size ifFalse: [ ^ self ].
	moveStr := replayMoves at: replayIndex + 1.
	replayIndex := replayIndex + 1.
	moveStr size = 2
		ifTrue: [
			pieceId := 'P'.
			destName := moveStr ]
		ifFalse: [
			pieceId := moveStr first asString.
			destName := moveStr allButFirst ].
	destSquare := board at: destName ifAbsent: [ nil ].
	destSquare ifNil: [ ^ self ].
	piece := self pieces
		         detect: [ :p |
			         p notNil and: [
				         p isWhite = currentPlayer isWhite and: [
					         p id = pieceId and: [
						         p legalTargetSquares anySatisfy: [ :sq |
							         sq name = destName ] ] ] ] ]
		         ifNone: [ nil ].
	(piece notNil and: [ destSquare notNil ]) ifFalse: [ ^ self ].
	piece moveTo: destSquare.
	self recordMovementOf: piece to: destSquare.
	self space ifNotNil: [ self space pulse ].
	currentPlayer := currentPlayer = whitePlayer
		                 ifTrue: [ blackPlayer ]
		                 ifFalse: [ whitePlayer ]
]

{ #category : 'as yet unclassified' }
MyChessGame >> replayPrevious [

	| newIndex |
	replayIndex > 0 ifFalse: [ ^ self ].
	newIndex := replayIndex - 1.
	self initializeFromPGN: (MyPGNParser forString: replayString).
	1 to: newIndex do: [ :i | self replayNext ]
]

{ #category : 'instance creation' }
MyChessGame >> toEndOfReplay [
	"Проигрывает все ходы, чтобы оказаться в конце партии"
	1 to: (self replayMoves size) do: [ :i |
		self replayNext ].
]
