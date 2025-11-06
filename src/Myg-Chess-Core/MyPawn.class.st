Class {
	#name : 'MyPawn',
	#superclass : 'MyPiece',
	#category : 'Myg-Chess-Core',
	#package : 'Myg-Chess-Core'
}

{ #category : 'accessing' }
MyPawn >> id [

	^ 'P'
]

{ #category : 'rendering' }
MyPawn >> renderPieceOn: aSquare [ 
	
	^ aSquare renderPawn: self
]

{ #category : 'rendering' }
MyPawn >> targetSquaresLegal: aBoolean [ 
	| forward1 forward2 captures |
	
	forward1 := self isWhite
		ifTrue: [ square up ]
		ifFalse: [ square down ].

	forward2 := nil.
	(self isWhite and: [ square name second = $2 ])
		ifTrue: [ forward2 := forward1 ifNotNil: #up ].
	(self isWhite not and: [ square name second = $7 ])
		ifTrue: [ forward2 := forward1 ifNotNil: #down ].

	captures := {
		self isWhite
			ifTrue: [ square up ifNotNil: #left ]
			ifFalse: [ square down ifNotNil: #left ].
		self isWhite
			ifTrue: [ square up ifNotNil: #right ]
			ifFalse: [ square down ifNotNil: #right ]
	} select: [ :s |
		s notNil and: [ s hasPiece and: [ s contents color ~= color ] ] ].

	^ ({ forward1. forward2 } select: [ :s |
			s notNil and: [ s hasPiece not ] ]) , captures
]
