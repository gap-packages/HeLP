gap> OldHeLPInfoLevel := InfoLevel(HeLP_Info);;
gap> SetInfoLevel(HeLP_Info, 1);

gap> C := CharacterTable("M22");;
gap> HeLP_WagnerTest(12, [ [ [1], [1], [1,0], [0,0,1], [-3,3,2,3,-4] ] ],C);
[  ]

gap> HeLP_WriteTrivialSolution(C, 6);
[ [ [ 1 ], [ 1 ], [ 0, 0, 1 ] ] ]

gap> HeLP_MultiplicitiesOfEigenvalues(Irr(C)[3], 6, HeLP_sol[6][1]);
[ 7, 8, 7, 8, 7, 8 ]

gap> HeLP_Reset();
gap> HeLP_sol;
 [ [ [ [ 1 ] ] ] ]

gap> SetInfoLevel(HeLP_Info, OldHeLPInfoLevel);
