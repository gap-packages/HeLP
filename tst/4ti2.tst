gap> G := AlternatingGroup(5);;
HeLP_ZC(G);
true
gap> C := CharacterTable("A5");;
gap> HeLP_ZC(C);
true
gap> HeLP_sol;
 [ [ [ [ 1 ] ] ], [ [ [ 1 ] ] ], [ [ [ 1 ] ] ],, 
   [ [ [ 0, 1 ] ], [ [ 1, 0 ] ] ], [  ],,,, [  ],,,,, [  ],,,,,,,,,,,,,,, [  ] 
  ]
gap> HeLP_PrintSolution(); 
 Solutions for elements of order 2:
 [ [         u ],
   [  [ "2a" ] ],
   [       --- ],
   [     [ 1 ] ] ]
 Solutions for elements of order 3:
 [ [         u ],
   [  [ "3a" ] ],
   [       --- ],
   [     [ 1 ] ] ]
 Solutions for elements of order 5:
 [ [               u ],
   [  [ "5a", "5b" ] ],
   [             --- ],
  [        [ 0, 1 ] ],
   [        [ 1, 0 ] ] ]
 There are no admissible partial augmentations for elements of order 6.
 There are no admissible partial augmentations for elements of order 10.
 There are no admissible partial augmentations for elements of order 15.
 There are no admissible partial augmentations for elements of order 30.

gap> C := CharacterTable( "A6" );;
gap> SetInfoLevel(HeLP_Info, 2);
HeLP_ZC(C);
 #I  Checking order 2.
 #I  Checking order 3.
 #I  Checking order 4.
 #I  Checking order 5.
 #I  Checking order 6.
 #I  Checking order 10.
 #I  Checking order 12.
 #I  Checking order 15.
 #I  Checking order 20.
 #I  Checking order 30.
 #I  Checking order 60.
 #I  ZC can't be solved, using the given data, for the orders: [ 6 ].
 false
gap> HeLP_sol[6];                                          
 [ [ [ 1 ], [ 0, 1 ], [ -2, 2, 1 ] ], [ [ 1 ], [ 1, 0 ], [ -2, 1, 2 ] ] ]
gap> HeLP_PrintSolution(6);                                
 Solutions for elements of order 6:
 [ [                   u^3,                   u^2,                     u ],
   [              [ "2a" ],        [ "3a", "3b" ],  [ "2a", "3a", "3b" ] ],
   [                   ---,                   ---,                   --- ],
   [                 [ 1 ],              [ 0, 1 ],          [ -2, 2, 1 ] ],
   [                 [ 1 ],              [ 1, 0 ],          [ -2, 1, 2 ] ] ]
gap> SetInfoLevel(HeLP_Info, 1);

gap> G := SmallGroup(48,30);;
gap> HeLP_ZC(G);
 #I  ZC can't be solved, using the given data, for the orders: [ 4 ].
 false
gap> Size(HeLP_sol[4]);
 20

gap> G := SmallGroup(96,65);;
gap> HeLP_ZC(G);
#! #I  (ZC) can't be solved, using the given data, for the orders: [ 8 ].
#! false
Size(HeLP_sol[8]);
#! 40

gap> G := SmallGroup(160,13);;
gap> HeLP_ZC(G);
 true

gap> C1 := CharacterTable(SymmetricGroup(5));;
gap> HeLP_ZC(C1);
 #I  The Brauer tables for the following primes are not available: [ 2, 3, 5 ].
 #I  ZC can't be solved, using the given data, for the orders: [ 4, 6 ].
 false
gap> C2 := CharacterTable("S5");;
gap> HeLP_ZC(C2); 
 true

gap> C := CharacterTable("M11");;
gap> HeLP_ZC(C);
 #I  ZC can't be solved, using the given data, for the orders: [ 4, 6, 8 ].
 false
gap> HeLP_sol[12];
 [  ]
gap> HeLP_PrintSolution(8);
 Solutions for elements of order 8:
 [ [      u^4,                         u^2,                           u ],
   [ [ "2a" ],              [ "2a", "4a" ],  [ "2a", "4a", "8a", "8b" ] ],
   [      ---,                         ---,                         --- ],
   [    [ 1 ],                    [ 0, 1 ],              [ 0, 0, 0, 1 ] ],
   [    [ 1 ],                    [ 0, 1 ],              [ 0, 0, 1, 0 ] ],
   [    [ 1 ],                    [ 0, 1 ],             [ 0, 2, -1, 0 ] ],
   [    [ 1 ],                    [ 0, 1 ],             [ 0, 2, 0, -1 ] ],
   [    [ 1 ],                   [ 2, -1 ],              [ 0, 0, 0, 1 ] ],
   [    [ 1 ],                   [ 2, -1 ],              [ 0, 0, 1, 0 ] ],
   [    [ 1 ],                   [ 2, -1 ],             [ 0, 2, -1, 0 ] ],
   [    [ 1 ],                   [ 2, -1 ],             [ 0, 2, 0, -1 ] ] ]

gap> C := CharacterTable("A7");;
gap> HeLP_PQ(C);
 true

gap> C := CharacterTable("L2(19)");;
gap> HeLP_PQ(C);                   
 true
gap> HeLP_ZC(C);
 #I  For the following orders ZC can not be solved, using the given data: [ 10 ].
 false
gap> HeLP_sol[10];
 [ [ [ 1 ], [ 0, 1 ], [ 0, -1, 1, 0, 1 ] ], 
   [ [ 1 ], [ 0, 1 ], [ 0, 0, 0, 1, 0 ] ], 
   [ [ 1 ], [ 1, 0 ], [ 0, 0, 0, 0, 1 ] ], 
   [ [ 1 ], [ 1, 0 ], [ 0, 1, -1, 1, 0 ] ] ]

gap> C1 := CharacterTable(PSL(2,7));;
gap> HeLP_PQ(C1);
 #I  The Brauer tables for the following primes are not available: [ 2, 3, 7 ].
 #I  PQ can't be solved, using the given data, for the orders: [ 6 ].
 false
gap> C2 := CharacterTable("L2(7)");;  
 CharacterTable( "L3(2)" )
gap> HeLP_PQ(C2);                  
 true

gap> SetInfoLevel(HeLP_Info,2);
gap> C := CharacterTable("A6");;
gap> HeLP_PQ(C);
 #I  Checking order 2.
 #I  Checking order 3.
 #I  Checking order 5.
 #I  Checking order 6.
 #I  Checking order 10.
 #I  Checking order 15.
 #I  PQ can't be solved, using the given data, for the orders: [ 6 ].
 false
gap> SetInfoLevel(HeLP_Info,1);

gap> C := CharacterTable("L2(49)");;
gap> HeLP_PQ(C);
 #I  The Brauer tables for the following primes are not available: [ 7 ].
 #I  (PQ) can't be solved, using the given data, for the orders: [ 10, 15 ].
 false

gap> C := CharacterTable("A5");;
gap> HeLP_WithGivenOrder(C, 5);
 #I  Number of solutions for elements of order 5: 2; stored in HeLP_sol[5].
 [ [ [ 0, 1 ] ], [ [ 1, 0 ] ] ]
gap> HeLP_PrintSolution(5);
 Solutions for elements of order 5:
 [ [               u ],
   [  [ "5a", "5b" ] ],
   [             --- ],
   [        [ 0, 1 ] ],
   [        [ 1, 0 ] ] ]

gap> C := CharacterTable("A6");;
gap> HeLP_WithGivenOrder(C, 4);
 #I  Number of solutions for elements of order 4: 4; stored in HeLP_sol[4].
 [ [ [ 1 ], [ -1, 2 ] ], [ [ 1 ], [ 2, -1 ] ], [ [ 1 ], [ 1, 0 ] ], 
   [ [ 1 ], [ 0, 1 ] ] ]
gap> HeLP_sol[4];              
 [ [ [ 1 ], [ -1, 2 ] ], [ [ 1 ], [ 2, -1 ] ], [ [ 1 ], [ 1, 0 ] ], 
   [ [ 1 ], [ 0, 1 ] ] ]
gap> HeLP_WithGivenOrder(C mod 3, 4);                                                                    
 #I  Number of solutions for elements of order 4: 2; stored in HeLP_sol[4].
 [ [ [ 1 ], [ 1, 0 ] ], [ [ 1 ], [ 0, 1 ] ] ]
gap> HeLP_ZC(C);               
 #I  ZC can't be solved, using the given data, for the orders: [ 6 ].
 false
gap> HeLP_sol[4]; HeLP_sol[6];
 [ [ [ 1 ], [ 0, 1 ] ] ]
 [ [ [ 1 ], [ 0, 1 ], [ -2, 2, 1 ] ], [ [ 1 ], [ 1, 0 ], [ -2, 1, 2 ] ] ]

gap> C := CharacterTable("L2(49).2_1");;
gap> HeLP_WithGivenOrder(Irr(C){[2]}, 14);
 #I  The given data admit infinitely many solutions for elements of order 14.
gap> HeLP_WithGivenOrder(Irr(C){[44]}, 14);
 #I  The given data admit infinitely many solutions for elements of order 14.
gap> HeLP_WithGivenOrder(Irr(C){[2,44]}, 14);
 #I  Number of solutions for elements of order 14: 0; stored in HeLP_sol[14].
 [  ]

gap> C := CharacterTable("J1");; 
gap> HeLP_WithGivenOrder(C, 6);;
 #I  Number of solutions for elements of order 6: 73; stored in HeLP_sol[6].
gap> B := C mod 11;;
gap> HeLP_WithGivenOrder(B, 6);;       
 #I  Number of solutions for elements of order 6: 6; stored in HeLP_sol[6].
gap> HeLP_WithGivenOrder(Irr(B){[2,3]}, 6);;
 #I  Number of solutions for elements of order 6: 6; stored in HeLP_sol[6].
gap> HeLP_PrintSolution(6);
 Solutions for elements of order 6:
 [ [                   u^3,                   u^2,                     u ],
   [              [ "2a" ],              [ "3a" ],  [ "2a", "3a", "6a" ] ],
   [                   ---,                   ---,                   --- ],
   [                 [ 1 ],                 [ 1 ],          [ -2, 0, 3 ] ],
   [                 [ 1 ],                 [ 1 ],          [ 2, 0, -1 ] ],
   [                 [ 1 ],                 [ 1 ],           [ 0, 0, 1 ] ],
   [                 [ 1 ],                 [ 1 ],          [ -4, 3, 2 ] ],
   [                 [ 1 ],                 [ 1 ],          [ 0, 3, -2 ] ],
   [                 [ 1 ],                 [ 1 ],          [ -2, 3, 0 ] ] ]

gap> C := CharacterTable("L2(27)");;
gap> HeLP_WithGivenOrder(C,7);;
 #I  Number of solutions for elements of order 7: 78; stored in HeLP_sol[7].
gap> SetInfoLevel(HeLP_Info,4);
gap> HeLP_WithGivenOrder(C,3*7); 
 #I      Solutions for order 3 not yet calculated.  Restart for this order.
 #I  Number of solutions for elements of order 21: 0; stored in HeLP_sol[21].  
 [  ]
gap> SetInfoLevel(HeLP_Info,1);

gap> G := SmallGroup(48,33);;
gap> C := CharacterTable(G);;
gap> HeLP_WithGivenOrder(C, 4);;
 #I  Number of solutions for elements of order 4: 4; stored in HeLP_sol[4].
gap> HeLP_WithGivenOrder(C, 6);;
 #I  Number of solutions for elements of order 6: 2; stored in HeLP_sol[6].
gap> HeLP_sol[4]; HeLP_sol[6];
 [ [ [ 1, 0 ], [ 0, 1, 0, 0, 0 ] ], [ [ 1, 0 ], [ 0, 0, 0, 0, 1 ] ], 
   [ [ 1, 0 ], [ 0, 0, 0, 1, 0 ] ], [ [ 1, 0 ], [ 0, 0, 1, 0, 0 ] ] ]
 [ [ [ 1, 0 ], [ 0, 1 ], [ 0, 0, 0, 0, 1, 0 ] ], 
   [ [ 1, 0 ], [ 1, 0 ], [ 0, 0, 0, 0, 0, 1 ] ] ]
gap> HeLP_WithGivenOrderAndPA(C, 12, [ [ 1, 0 ],  [ 0, 1 ], [ 0, 0, 0, 0, 1 ], 
    [ 0, 0, 0, 0, 1, 0 ] ]); 
 #I  Number of solutions for elements of order 12 with these partial augmentation
 s for the powers: 1.
 [ [ [ 1, 0 ], [ 0, 1 ], [ 0, 0, 0, 0, 1 ], [ 0, 0, 0, 0, 1, 0 ], 
       [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 ] ] ]
gap> HeLP_WithGivenOrderAndPA(C, 12, [ [ 1, 0 ],  [ 0, 1 ], [ 0, 0, 0, 1, 0 ],
    [ 0, 0, 0, 0, 1, 0 ] ]);
 #I  Number of solutions for elements of order 12 with these partial augmentation
 s for the powers: 0.
 [  ]

gap> C := CharacterTable("A5");;
gap> chi := Irr(C)[2];; psi := Irr(C)[4];;
gap> HeLP_WithGivenOrderAndPAAndSpecificSystem([[chi, 1], [chi, 2]],
 5, [ ], true);
 [ [ [ [ 0, 1 ] ], [ [ 1, 0 ] ] ], [ [ -3/5, 2/5 ], [ 2/5, -3/5 ] ], [ 3/5, 3/5 ] ]
gap> sol5 := HeLP_WithGivenOrderAndPAAndSpecificSystem([[chi, 1], [chi, 2]], 
 5, [ ]);      
 [ [ [ 0, 1 ] ], [ [ 1, 0 ] ] ]
gap> HeLP_WithGivenOrderAndPAAndSpecificSystem([psi], 
 2*5, [[1], sol5[1][1]], true);     
 [ [  ], [ [ 0, -2/5, -2/5 ], [ 0, -1/10, -1/10 ], [ 0, 1/10, 1/10 ],
  [ 0, -1/10, -1/10 ], [ 0, 1/10, 1/10 ], [ 0, 2/5, 2/5 ], 
  [ 0, 1/10, 1/10 ], [ 0, -1/10, -1/10 ], [ 0, 1/10, 1/10 ], 
 [ 0, -1/10, -1/10 ] ], [ 0, 1/2, 1/2, 1/2, 1/2, 0, 1/2, 1/2, 1/2, 1/2 ] ]
gap> HeLP_WithGivenOrderAndPAAndSpecificSystem([[psi, 0], [psi, 2], [psi, 5]], 
 2*5, [[1], sol5[2][1]], true); 
 [ [  ], [ [ 0, -2/5, -2/5 ], [ 0, 1/10, 1/10 ], [ 0, 2/5, 2/5 ] ], [ 0, 1/2, 0 ] ]

gap> C := CharacterTable("A6");;
gap> HeLP_WithGivenOrder(C, 6);            
 #I  Number of solutions for elements of order 6: 2; stored in HeLP_sol[6].
 [ [ [ 1 ], [ 0, 1 ], [ -2, 2, 1 ] ], [ [ 1 ], [ 1, 0 ], [ -2, 1, 2 ] ] ]
gap> HeLP_WithGivenOrderSConstant(C, 2, 3);
 [ [ [ 0, 1 ], [ -2, 2, 1 ] ], [ [ 1, 0 ], [ -2, 1, 2 ] ] ]
gap> HeLP_WithGivenOrderSConstant(C, 3, 2);     
 [ [ [ 1 ], [ 3, -2 ] ] ]

gap> C := CharacterTable("Sz(8)");;
gap> SetInfoLevel(HeLP_Info, 4);
gap> HeLP_WithGivenOrderSConstant(C, 7, 13);
 #I    Partial augmentations for elements of order 13 not yet calculated.  Restar
 t for this order.
 #I    Number of non-trivial 7-constant characters in the list: 7.
 [  ]
gap> SetInfoLevel(HeLP_Info, 1);

gap> C := CharacterTable("Sz(32)");;
gap> L := Filtered(OrdersClassRepresentatives(C), x-> x = 31);; Size(L);;
gap> HeLP_WithGivenOrderSConstant(C mod 2, 31, 5);
 [  ]
gap> IsBound(HeLP_sol[31]);
 false 

gap> C := CharacterTable("L2(7)");;
gap> HeLP_WithGivenOrder(C,6);
 #I  Number of solutions for elements of order 6: 1; stored in HeLP_sol[6].
 [ [ [ 1 ], [ 1 ], [ -2, 3 ] ] ]
gap> HeLP_AllOrders(C);
 #I  (ZC) can't be solved, using the given data, for the orders: [ 6 ].
 false
gap> HeLP_ZC(C);
 true

gap> C := CharacterTable("A12");;
gap> HeLP_WithGivenOrder(Irr(C){[2, 4, 7]}, 2);;
 #I  Number of solutions for elements of order 2: 37; stored in HeLP_sol[2].
gap> HeLP_WithGivenOrderSConstant(C mod 3,11,2);
 [  ]
gap> HeLP_WithGivenOrder(Irr(C mod 2){[2, 3, 4, 6]}, 3);;
 #I  Number of solutions for elements of order 3: 99; stored in HeLP_sol[3].
gap> HeLP_WithGivenOrderSConstant(C mod 2, 11, 3);
 [  ]
gap> HeLP_AllOrdersPQ(C);
 true

gap> C := CharacterTable("M11");;
gap> HeLP_WithGivenOrder(C,8);;
 #I  Number of solutions for elements of order 8: 36; stored in HeLP_sol[8].
gap> HeLP_sol[8] := HeLP_WagnerTest(8);;
gap> Size(HeLP_sol[8]);
 24
gap> HeLP_WithGivenOrder(C,12);
 #I  Number of solutions for elements of order 12: 7; stored in HeLP_sol[12].
 [ [ [ 1 ], [ 1 ], [ 2, -1 ], [ 0, 3, -2 ], [ 1, 0, -1, 1 ] ], 
   [ [ 1 ], [ 1 ], [ 1, 0 ], [ 0, 3, -2 ], [ 0, 0, 0, 1 ] ], 
   [ [ 1 ], [ 1 ], [ -1, 2 ], [ 0, 3, -2 ], [ 0, 0, 2, -1 ] ], 
   [ [ 1 ], [ 1 ], [ 0, 1 ], [ 0, 3, -2 ], [ 1, 0, 1, -1 ] ], 
   [ [ 1 ], [ 1 ], [ 0, 1 ], [ 0, 3, -2 ], [ -1, 0, 1, 1 ] ], 
   [ [ 1 ], [ 1 ], [ 1, 0 ], [ 0, -3, 4 ], [ 0, 0, 0, 1 ] ], 
   [ [ 1 ], [ 1 ], [ -1, 2 ], [ 0, -3, 4 ], [ 1, 0, -1, 1 ] ] ]
gap> HeLP_sol[12] := HeLP_WagnerTest(12);
 [ [ [ 1 ], [ 1 ], [ 1, 0 ], [ 0, 3, -2 ], [ 0, 0, 0, 1 ] ], 
   [ [ 1 ], [ 1 ], [ -1, 2 ], [ 0, 3, -2 ], [ 0, 0, 2, -1 ] ], 
   [ [ 1 ], [ 1 ], [ 1, 0 ], [ 0, -3, 4 ], [ 0, 0, 0, 1 ] ] ]
gap> HeLP_sol[4] := HeLP_WagnerTest(4);;
gap> HeLP_WithGivenOrder(C,12);
! #I  Number of solutions for elements of order 12: 3; stored in HeLP_sol[12].
 [ [ [ 1 ], [ 1 ], [ 2, -1 ], [ 0, 3, -2 ], [ 1, 0, -1, 1 ] ], 
   [ [ 1 ], [ 1 ], [ 0, 1 ], [ 0, 3, -2 ], [ 1, 0, 1, -1 ] ], 
   [ [ 1 ], [ 1 ], [ 0, 1 ], [ 0, 3, -2 ], [ -1, 0, 1, 1 ] ] ]
gap> HeLP_sol[12] := HeLP_WagnerTest(12);
 [  ]

gap> C := CharacterTable("M22");;
 CharacterTable( "M22" )
gap> HeLP_WagnerTest(12, [ [ [1], [1], [1,0], [0,0,1], [-3,3,2,3,-4] ] ],C);
 [  ]
gap> G := SmallGroup(96,187);;
gap> C := CharacterTable(G);;
gap> HeLP_WithGivenOrder(C,4);;
 #I  Number of solutions for elements of order 4: 34; stored in HeLP_sol[4].
gap> HeLP_WagnerTest(4);       
 [ [ [ 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 0, 0, 1 ] ],
   [ [ 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 1, 0, 0 ] ],
   [ [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1, 0, 0, 0 ] ], 
   [ [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 0, 1, 0 ] ] ]
gap> HeLP_ZC(C);
 true

gap> SetInfoLevel(HeLP_Info, 5);
gap> C := CharacterTable(SymmetricGroup(4));; 
gap> HeLP_WithGivenOrder(C mod 2, 3); 
 #I  USED CHARACTER TABLE CHANGED TO BrauerTable( SymmetricGroup( [ 1 .. 4 ] ), 2
  ), ALL GLOBAL VARIABLES RESET.
 #I  Number of solutions for elements of order 3: 1; stored in HeLP_sol[3].
 [ [ [ 1 ] ] ]
gap> HeLP_WithGivenOrder(C, 2*3);    
 #I  USED CHARACTER TABLE CHANGED TO CharacterTable( SymmetricGroup( [ 1 .. 4 ] )
  ), ALL GLOBAL VARIABLES RESET.
 #I      Solutions for order 2 not yet calculated.  Restart for this order.
 #I      Solutions for order 3 not yet calculated.  Restart for this order.
 #I  Number of solutions for elements of order 6: 0; stored in HeLP_sol[6].
 [  ]
gap> D := CharacterTable(SymmetricGroup(4));;
gap> HeLP_WithGivenOrder(D mod 2, 3);       
 #I  USED CHARACTER TABLE CHANGED TO BrauerTable( SymmetricGroup( [ 1 .. 4 ] ), 2
  ), ALL GLOBAL VARIABLES RESET.
 #I  Number of solutions for elements of order 3: 1; stored in HeLP_sol[3].
 [ [ [ 1 ] ] ]
gap> HeLP_ChangeCharKeepSols(D);
 #I  WARNING: Change used character table without checking if the character table
 s have the same underlying groups and the ordering of the conjugacy classes are 
 the same!
gap> HeLP_WithGivenOrder(D, 2*3);    
 #I  Using same character table as until now; all known solutions kept.
 #I      Solutions for order 2 not yet calculated.  Restart for this order.
 #I  Number of solutions for elements of order 6: 0; stored in HeLP_sol[6].
 [  ]

gap> CA := CharacterTable("A5");;
gap> HeLP_WithGivenOrder(CA mod 2, 5);
 #I  USED CHARACTER TABLE CHANGED TO BrauerTable( "A5", 2 ), ALL GLOBAL VARIABLES
  RESET.
 #I  Testing possibility 1 out of 1.
 #I  Number of solutions for elements of order 5: 2; stored in HeLP_sol[5].
 [ [ [ 0, 1 ] ], [ [ 1, 0 ] ] ]
gap> HeLP_WithGivenOrder(CA, 2*5);    
 #I  Using character table of the same group; all known solutions kept.
 #I      Solutions for order 2 not yet calculated.  Restart for this order.
 #I  Number of solutions for elements of order 10: 0; stored in HeLP_sol[10].
 [  ]
gap> SetInfoLevel(HeLP_Info, 1);

gap> C := CharacterTable("A6");;
gap> HeLP_WithGivenOrder(C, 4);
 #I  Number of solutions for elements of order 4: 4; stored in HeLP_sol[4].
 [ [ [ 1 ], [ -1, 2 ] ], [ [ 1 ], [ 2, -1 ] ], [ [ 1 ], [ 1, 0 ] ], 
   [ [ 1 ], [ 0, 1 ] ] ]
gap> HeLP_VerifySolution(C mod 3, 4);
 [ [ [ 1 ], [ 1, 0 ] ], [ [ 1 ], [ 0, 1 ] ] ]
gap> HeLP_sol[4];
 [ [ [ 1 ], [ 1, 0 ] ], [ [ 1 ], [ 0, 1 ] ] ]

gap> C := CharacterTable("S12");;
gap> HeLP_WithGivenOrder(Irr(C mod 5){[2..6]}, 2);;
 #I  Number of solutions for elements of order 2: 563; stored in HeLP_sol[2].
gap> HeLP_VerifySolution(C mod 5, 2);;
gap> Size(HeLP_sol[2]);
 387
gap> HeLP_VerifySolution(C mod 3, 2);;
gap> Size(HeLP_sol[2]);
 324

gap> SetInfoLevel(HeLP_Info,4);
gap> C := CharacterTable(SmallGroup(160,91));;
gap> HeLP_WithGivenOrder(C,4);;
 #I      Solutions for order 2 not yet calculated.  Restart for this order.
 #I  Number of solutions for elements of order 4: 22; stored in HeLP_sol[4].   
gap> HeLP_WithGivenOrder(C,10);;
 #I      Solutions for order 5 not yet calculated.  Restart for this order.
 #I  Number of solutions for elements of order 10: 6; stored in HeLP_sol[10].  
gap> LP := HeLP_PossiblePartialAugmentationsOfPowers(20);;
gap> HeLP_WithGivenOrderAndPA(Irr(C){[2..20]},20,LP[1]);
 #I  Number of solutions for elements of order 20 with these partial augmentations
  for the powers: 0.
 [  ]

gap> C := CharacterTable("A5");;
gap> HeLP_ZC(C);          
 true
gap> HeLP_PrintSolution();
 Solutions for elements of order 2:
 [ [         u ],
   [  [ "2a" ] ],
   [       --- ],
   [     [ 1 ] ] ]
 Solutions for elements of order 3:
 [ [         u ],
   [  [ "3a" ] ],
   [       --- ],
   [     [ 1 ] ] ]
 Solutions for elements of order 5:
 [ [               u ],
   [  [ "5a", "5b" ] ],
   [             --- ],
   [        [ 0, 1 ] ],
   [        [ 1, 0 ] ] ]
 There are no admissible partial augmentations for elements of order 6.
 There are no admissible partial augmentations for elements of order 10.
 There are no admissible partial augmentations for elements of order 15.
 There are no admissible partial augmentations for elements of order 30.
gap> C := CharacterTable("A6");;
gap> HeLP_ZC(C);           
 #I  ZC can't be solved, using the given data, for the orders: [ 6 ].
 false
gap> HeLP_PrintSolution(6);
 Solutions for elements of order 6:
 [ [                   u^3,                   u^2,                     u ],
   [              [ "2a" ],        [ "3a", "3b" ],  [ "2a", "3a", "3b" ] ],
   [                   ---,                   ---,                   --- ],
   [                 [ 1 ],              [ 0, 1 ],          [ -2, 2, 1 ] ],
   [                 [ 1 ],              [ 1, 0 ],          [ -2, 1, 2 ] ] ]

gap> C := CharacterTable("A6");;
gap> HeLP_WithGivenOrder(C, 6);                               
 #I  Number of solutions for elements of order 6: 2; stored in HeLP_sol[6].
 [ [ [ 1 ], [ 0, 1 ], [ -2, 2, 1 ] ], [ [ 1 ], [ 1, 0 ], [ -2, 1, 2 ] ] ]
gap> chi := Irr(C)[2];;
gap> HeLP_MultiplicitiesOfEigenvalues(chi, 6, HeLP_sol[6][2]);
 [ 1, 0, 1, 2, 1, 0 ]
gap> HeLP_CharacterValue(chi, 6, HeLP_sol[6][2][3]);          
 -2
gap> HeLP_CharacterValue(chi, 6, [-2,1,2]);
 -2
gap> HeLP_CharacterValue(chi, 6, [-2,2,1]);
 1
