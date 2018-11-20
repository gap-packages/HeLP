gap> OldHeLPInfoLevel := InfoLevel(HeLP_Info);;
gap> SetInfoLevel(HeLP_Info, 1);

gap> HeLP_ZC(CyclicGroup(6));
#I  Since the given group is nilpotent the Zassenhaus Conjecture holds by a result of Al Weiss.
 true

gap> C := CharacterTable("M22");;
gap> HeLP_WagnerTest(12, [ [ [1], [1], [1,0], [0,0,1], [-3,3,2,3,-4] ] ],C);
[  ]

gap> HeLP_Reset();
gap> HeLP_sol;
 [ [ [ [ 1 ] ] ] ]

gap> SetInfoLevel(HeLP_Info, OldHeLPInfoLevel);
