Class {
	#name : 'MyChessReplayTest',
	#superclass : 'TestCase',
	#instVars : [
		'game',
		'pgnSimple',
		'sanSimple'
	],
	#category : 'Myg-Chess-Tests-replay',
	#package : 'Myg-Chess-Tests',
	#tag : 'replay'
}

{ #category : 'running' }
MyChessReplayTest >> setUp [
    game := MyChessGame freshGame.
    sanSimple := '1. a3 a6 2. h3 h6 3. a4 a5 4. h4 h5 5. b3 b6 6. g3 g6 7. b4 b5 8. g4 g5 9. c3 c6 10. f3 f6 11. c4 c5 12. f4 f5 13. d3 d6 14. e3 e6 15. d4 d5 16. e4 e5 17. Nc3 Nc6 18. Nf3 Nf6 19. Be2 Be7 20. Qc2 Qc7 1/2-1/2'.
    pgnSimple := MyChessGame normalizeToPGN: sanSimple.
]

{ #category : 'running' }
MyChessReplayTest >> testInitializeFromPGN_FillsReplayMoves [
    game initializeFromPGN: (MyPGNParser forString: pgnSimple).
    self assert: game replayMoves size = 40.
]

{ #category : 'running' }
MyChessReplayTest >> testNormalizeToPGN_AddsDefaultResult [
    | out |
    out := MyChessGame normalizeToPGN: '1. e4 e5'.
    self assert: (out includesSubstring: '[Event "Manual"]').
    self assert: (out endsWith: '1-0').
]

{ #category : 'running' }
MyChessReplayTest >> testReplayMoves_NotMutatedByToEnd [
	| before after |
	game initializeFromPGN: (MyPGNParser forString: pgnSimple).
	before := game replayMoves copy.
	game toEndOfReplay.
	after := game replayMoves.
	self assert: after size = before size.
	self assert: after equals: before.
]

{ #category : 'running' }
MyChessReplayTest >> testReplayPrevious_FromStartIsSafe [
    game initializeFromPGN: (MyPGNParser forString: pgnSimple).
    game replayPrevious.  "на самом начале"
    game replayPrevious.  "ещё раз"
    self assert: true.     "если дошли — всё ок"
]

{ #category : 'running' }
MyChessReplayTest >> testRoundTrip_BackThenForwardRestores [
	| fenAfterN fenRoundTrip n m |
	game initializeFromPGN: (MyPGNParser forString: pgnSimple).
	(game respondsTo: #board) ifFalse: [ ^ self skip ].
	(game board respondsTo: #asFENString) ifFalse: [ ^ self skip ].

	n := 15. m := 7.  "произвольно, но внутри границ"

	1 to: n do: [ :i | game replayNext ].
	fenAfterN := game board asFENString.

	1 to: m do: [ :i | game replayPrevious ].
	1 to: m do: [ :i | game replayNext ].

	fenRoundTrip := game board asFENString.
	self assert: fenRoundTrip equals: fenAfterN.
]

{ #category : 'running' }
MyChessReplayTest >> testToEndOfReplay_ReachesEndSafely [
    game initializeFromPGN: (MyPGNParser forString: pgnSimple).
    game toEndOfReplay.
    "ещё один next поверх конца не должен падать"
    game replayNext.
    self assert: game replayMoves size = 40.
]
