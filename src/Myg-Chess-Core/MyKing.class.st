Class {
	#name : 'MyKing',
	#superclass : 'MyPiece',
	#category : 'Myg-Chess-Core',
	#package : 'Myg-Chess-Core'
}

{ #category : 'as yet unclassified' }
MyKing >> attackingSquares [

	^ self basicTargetSquares
]

{ #category : 'rendering' }
MyKing >> basicTargetSquares [

	"The king can move one square on each direction including diagonals"
	^ {
		square ifNotNil: #right.
		square up ifNotNil: #right.
		square ifNotNil: #up.
		square up ifNotNil: #left.
		square ifNotNil: #left.
		square left ifNotNil: #down.
		square ifNotNil: #down.
		square down ifNotNil: #right
	}
]

{ #category : 'testing' }
MyKing >> canCastleKingside [ 
    | rook f g |
    color isWhite
        ifTrue: [
            rook := square board at: 'h1'.
            f := square board at: 'f1'.
            g := square board at: 'g1'. ]
        ifFalse: [
            rook := square board at: 'h8'.
            f := square board at: 'f8'.
            g := square board at: 'g8'. ].

    ^ rook notNil and: [
        rook class = MyRook
        and: [ f hasPiece not
        and: [ g hasPiece not
        and: [ self isInCheck not ] ] ] ]
]

{ #category : 'testing' }
MyKing >> canCastleQueenside [ 
    | rook b c d |
    color isWhite
        ifTrue: [
            rook := square board at: 'a1'.
            b := square board at: 'b1'.
            c := square board at: 'c1'.
            d := square board at: 'd1'. ]
        ifFalse: [
            rook := square board at: 'a8'.
            b := square board at: 'b8'.
            c := square board at: 'c8'.
            d := square board at: 'd8'. ].

    ^ rook notNil and: [
        rook class = MyRook
        and: [ b hasPiece not
        and: [ c hasPiece not
        and: [ d hasPiece not
        and: [ self isInCheck not ] ] ] ] ]
]

{ #category : 'accessing' }
MyKing >> id [
	
	^ 'K'
]

{ #category : 'as yet unclassified' }
MyKing >> isCastlingMove: aSquare [

	| name |
	name := aSquare name.
	^ (color isWhite and: [ name = 'g1' or: [ name = 'c1' ] ]) or: [
		  color isBlack and: [ name = 'g8' or: [ name = 'c8' ] ] ]
]

{ #category : 'testing' }
MyKing >> isCheckMated [
	"We check if the opponent pieces target all my potential movements and myself"

	| threatenedSquares |
	threatenedSquares := self opponentPieces flatCollect: [ :e |
		                     e attackingSquares ].

	^ self legalTargetSquares isEmpty and: [
		  threatenedSquares includes: self square ]
]

{ #category : 'testing' }
MyKing >> isInCheck [
	"We check if the opponent pieces target all my potential movements and myself"

	| threatenedSquares |
	threatenedSquares := self opponentPieces flatCollect: [ :e |
		                     e attackingSquares ].

	^ threatenedSquares includes: self square
]

{ #category : 'testing' }
MyKing >> isKing [
	
	^ true
]

{ #category : 'accessing' }
MyKing >> opponentPieces [

	^ self board pieces select: [ :e |
		  e notNil and: [ e color = color negated ] ]
]

{ #category : 'as yet unclassified' }
MyKing >> performCastlingTo: aSquare [ 
    | board rook rookTo |
    board := square board.

    (aSquare name = 'g1' or: [ aSquare name = 'g8' ]) ifTrue: [
        rook := board at: (color isWhite ifTrue: [ 'h1' ] ifFalse: [ 'h8' ]).
        rookTo := board at: (color isWhite ifTrue: [ 'f1' ] ifFalse: [ 'f8' ]).
    ] ifFalse: [
        rook := board at: (color isWhite ifTrue: [ 'a1' ] ifFalse: [ 'a8' ]).
        rookTo := board at: (color isWhite ifTrue: [ 'd1' ] ifFalse: [ 'd8' ]).
    ].

    rook square emptyContents.
    rookTo contents: rook.
    super moveTo: aSquare
]

{ #category : 'rendering' }
MyKing >> renderPieceOn: aSquare [

	^ aSquare renderKing: self
]

{ #category : 'rendering' }
MyKing >> targetSquaresLegal: aBoolean [ 
    | normal castle |
    normal := super targetSquaresLegal: aBoolean.
    castle := OrderedCollection new.

    (self canCastleKingside) ifTrue: [
        castle add: (square board at: (color isWhite ifTrue: [ 'g1' ] ifFalse: [ 'g8' ])) ].
    (self canCastleQueenside) ifTrue: [
        castle add: (square board at: (color isWhite ifTrue: [ 'c1' ] ifFalse: [ 'c8' ])) ].

    ^ normal , castle
]
