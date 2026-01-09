gap> HeLP_Solver("normaliz");;
#I  'normaliz' will be used from now on.

gap> OldHeLPInfoLevel := InfoLevel(HeLP_Info);;
gap> SetInfoLevel(HeLP_Info, 0);

#
gap> HeLP_ZC(CyclicGroup(6));
true

#
gap> G := AlternatingGroup(5);;
gap> HeLP_ZC(G);
true

#
gap> C := CharacterTable("A5");;
gap> HeLP_ZC(C);
true
gap> List(HeLP_sol, x -> Set(x));
 [ [ [ [ 1 ] ] ], [ [ [ 1 ] ] ], [ [ [ 1 ] ] ],, 
   [ [ [ 0, 1 ] ], [ [ 1, 0 ] ] ], [  ],,,, [  ],,,,, [  ],,,,,,,,,,,,,,, [  ] 
  ]

gap> C := CharacterTable( "A6" );;
gap> HeLP_ZC(C);
 false
gap> Set(HeLP_sol[6]);                                          
 [ [ [ 1 ], [ 0, 1 ], [ -2, 2, 1 ] ], [ [ 1 ], [ 1, 0 ], [ -2, 1, 2 ] ] ]
gap> HeLP_sol[12];
[ ]
gap> HeLP_WithGivenOrderAllTables(C, 6);
[ [ [ 1 ], [ 0, 1 ], [ -2, 2, 1 ] ], [ [ 1 ], [ 1, 0 ], [ -2, 1, 2 ] ] ]
gap> HeLP_SP(C);
false

gap> G := SmallGroup(48,30);;
gap> C := CharacterTable(G);;
gap> HeLP_WithGivenOrderAllTables(C, 2);
 [ [ [ 0, 0, 1 ] ], [ [ 0, 1, 0 ] ], [ [ 1, 0, 0 ] ] ]
gap> HeLP_ZC(G);
 false
gap> Size(HeLP_sol[4]);
 10

gap> C1 := CharacterTable(PSL(2,7));;
gap> HeLP_PQ(C1);
 false
gap> C2 := CharacterTable("L2(7)");;  
gap> HeLP_PQ(C2);                  
 true

gap> C := CharacterTable("L2(49).2_1");;
gap> HeLP_WithGivenOrder(Irr(C), 7);;
gap> HeLP_WithGivenOrder(Irr(C){[2]}, 14);
 "infinite"
gap> HeLP_WithGivenOrder(Irr(C){[2,44]}, 14);
 [  ]

gap> C := CharacterTable("A5");;
gap> chi := Irr(C)[2];; psi := Irr(C)[4];;
gap> HeLP_WithGivenOrderAndPAAndSpecificSystem([[chi, 1], [chi, 2]],  5, [ ], true);   # Ist das eindeutig? Oder muss ein Set rein?
 [ [ [ [ 0, 1 ] ], [ [ 1, 0 ] ] ], [ [ -3/5, 2/5 ], [ 2/5, -3/5 ] ], [ 3/5, 3/5 ] ]
gap> sol5 := HeLP_WithGivenOrderAndPAAndSpecificSystem([[chi, 1], [chi, 2]],  5, [ ]);      # Hier eindeutig?
 [ [ [ 0, 1 ] ], [ [ 1, 0 ] ] ]
gap> HeLP_WithGivenOrderAndPAAndSpecificSystem([psi], 2*5, [[1], sol5[1][1]], true);     
 [ [  ], [ [ 0, -2/5, -2/5 ], [ 0, -1/10, -1/10 ], [ 0, 1/10, 1/10 ],
  [ 0, -1/10, -1/10 ], [ 0, 1/10, 1/10 ], [ 0, 2/5, 2/5 ], 
  [ 0, 1/10, 1/10 ], [ 0, -1/10, -1/10 ], [ 0, 1/10, 1/10 ], 
 [ 0, -1/10, -1/10 ] ], [ 0, 1/2, 1/2, 1/2, 1/2, 0, 1/2, 1/2, 1/2, 1/2 ] ]
gap> HeLP_WithGivenOrderAndPAAndSpecificSystem([[psi, 0], [psi, 2], [psi, 5]], 2*5, [[1], sol5[2][1]], true); 
 [ [  ], [ [ 0, -2/5, -2/5 ], [ 0, 1/10, 1/10 ], [ 0, 2/5, 2/5 ] ], [ 0, 1/2, 0 ] ]

gap> C := CharacterTable("A6");;
gap> Set(HeLP_WithGivenOrder(C, 6));            
 [ [ [ 1 ], [ 0, 1 ], [ -2, 2, 1 ] ], [ [ 1 ], [ 1, 0 ], [ -2, 1, 2 ] ] ]
gap> Set(HeLP_WithGivenOrderSConstant(C, 2, 3));
 [ [ [ 0, 1 ], [ -2, 2, 1 ] ], [ [ 1, 0 ], [ -2, 1, 2 ] ] ]
gap> HeLP_WithGivenOrderSConstant(C, 3, 2);
 [ [ [ 1 ], [ 3, -2 ] ] ]
gap> HeLP_WithGivenOrderAndPA(C, 6, [[1],[1,0]]);
 [ [ [ 1 ], [ 1, 0 ], [ -2, 1, 2 ] ] ]


gap> C := CharacterTable("Sz(32)");;
gap> HeLP_WithGivenOrderSConstant(C mod 2, 31, 5);
 [  ]
gap> IsBound(HeLP_sol[31]);
 false 

gap> C := CharacterTable("L2(7)");;
gap> HeLP_WithGivenOrder(C,6);
 [ [ [ 1 ], [ 1 ], [ -2, 3 ] ] ]
gap> HeLP_AllOrders(C);
 true

gap> C := CharacterTable("A12");;
gap> HeLP_WithGivenOrder(Irr(C){[2, 4, 7]}, 2);;
gap> HeLP_WithGivenOrderSConstant(C mod 3,11,2);
 [  ]
gap> HeLP_WithGivenOrder(Irr(C mod 2){[2, 3, 4, 6]}, 3);;
gap> HeLP_WithGivenOrderSConstant(C mod 2, 11, 3);
 [  ]
gap> HeLP_AllOrdersPQ(C);
 true

gap> C := CharacterTable("M11");;
gap> HeLP_WithGivenOrder(C,8);;
gap> HeLP_sol[8] := HeLP_WagnerTest(8);;
gap> Size(HeLP_sol[8]);
 24
gap> HeLP_WithGivenOrder(C,12);;
gap> HeLP_sol[12] := HeLP_WagnerTest(12);;
gap> HeLP_sol[4] := HeLP_WagnerTest(4);;
gap> HeLP_WithGivenOrder(C,12);;
gap> HeLP_sol[12] := HeLP_WagnerTest(12);
 [  ]

gap> C := CharacterTable("M22");;
gap> HeLP_WagnerTest(12, [ [ [1], [1], [1,0], [0,0,1], [-3,3,2,3,-4] ] ],C);
 [  ]
gap> G := SmallGroup(96,187);;
gap> C := CharacterTable(G);;
gap> HeLP_WithGivenOrder(C,4);;
gap> Size(HeLP_WagnerTest(4)); 
 4

gap> C := CharacterTable("A6");;
gap> HeLP_WithGivenOrder(C, 4);;
gap> Size(HeLP_VerifySolution(C mod 3, 4));
 2
gap> Size(HeLP_sol[4]);
 2

gap> C := CharacterTable(SmallGroup(160,91));;
gap> HeLP_WithGivenOrder(C,4);;
gap> HeLP_WithGivenOrder(C,10);;
gap> LP := HeLP_PossiblePartialAugmentationsOfPowers(20);;
gap> Size(LP);
44

gap> C := CharacterTable("A6");;
gap> HeLP_WithGivenOrder(C, 6);;                         
gap> HeLP_sol[6] := Set(HeLP_sol[6]);;
gap> chi := Irr(C)[2];;
gap> HeLP_MultiplicitiesOfEigenvalues(chi, 6, Set(HeLP_sol[6])[2]);
 [ 1, 0, 1, 2, 1, 0 ]
gap> HeLP_CharacterValue(chi, 6, Set(HeLP_sol[6])[2][3]);          
 -2
gap> HeLP_CharacterValue(chi, 6, [-2,1,2]);
 -2
gap> HeLP_CharacterValue(chi, 6, [-2,2,1]);
 1

gap> G := SmallGroup(48, 30);;
gap> HeLP_AllOrdersSP(G);
true
gap> HeLP_KP(G);
true
gap> HeLP_ZC(G);
false

gap> G := SmallGroup(72, 40);;
gap> HeLP_AllOrdersKP(G);
true

gap> G := AlternatingGroup(7);;
gap> C := CharacterTable(G);;
gap> sols := HeLP_WithGivenOrderAllTables(C, 4);;
gap> [[1], [-2,3]] in sols;
true
gap> [[1], [2,-1]] in sols;
true
gap> [[1], [3,-2]] in sols;
false

gap> HeLP_Reset();
gap> HeLP_sol;
 [ [ [ [ 1 ] ] ] ]

gap> SetInfoLevel(HeLP_Info, OldHeLPInfoLevel);
