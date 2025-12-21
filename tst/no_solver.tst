gap> OldHeLPInfoLevel := InfoLevel(HeLP_Info);;
gap> SetInfoLevel(HeLP_Info, 0);

gap> C := CharacterTable("A5");;
gap> HeLP_WriteTrivialSolution(C, 5);
[ [ [ 1, 0 ] ], [ [ 0, 1 ] ] ]
gap> HeLP_WriteTrivialSolution(C, 2);
[ [ [ 1 ] ] ]
gap> HeLP_PossiblePartialAugmentationsOfPowers(10);
[ [ [ 1 ], [ 1, 0 ] ], [ [ 1 ], [ 0, 1 ] ] ]
gap> HeLP_CharacterValue(Irr(C)[2], 5, HeLP_sol[2]);
[ [ -E(5)-E(5)^4 ] ]

gap> C := CharacterTable("M22");;
gap> HeLP_WagnerTest(12, [ [ [1], [1], [1,0], [0,0,1], [-3,3,2,3,-4] ] ],C);
[  ]
gap> HeLP_WriteTrivialSolution(C, 6);
[ [ [ 1 ], [ 1 ], [ 0, 0, 1 ] ] ]
gap> HeLP_MultiplicitiesOfEigenvalues(Irr(C)[3], 6, HeLP_sol[6][1]);
[ 7, 8, 7, 8, 7, 8 ]


gap> C := CharacterTable("A6");;
gap> pa := [[1], [0,1], [-2,2,1]];;
gap> pa2 := [[1], [1,0], [-2,2,1]];;
gap> HeLP_sol[6] := [pa,pa2];;
gap> HeLP_VerifySolution(C, 6);
[ [ [ 1 ], [ 0, 1 ], [ -2, 2, 1 ] ] ]
gap> HeLP_UnitSatisfiesKP(C, 6, HeLP_sol[6][1]);
false

gap> G := SymmetricGroup(5);;
gap> C := CharacterTable(G);;
gap> HeLP_WriteTrivialSolution(C, 2);;
gap> HeLP_IsOneModuloN(C, 2, HeLP_sol[2][1][1], G, AlternatingGroup(5));
false
gap> HeLP_IsOneModuloN(C, 2, HeLP_sol[2][2][1], G, AlternatingGroup(5));
true

gap> HeLP_Reset();
gap> HeLP_sol;
 [ [ [ [ 1 ] ] ] ]

gap> SetInfoLevel(HeLP_Info, OldHeLPInfoLevel);
