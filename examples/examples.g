#! @BeginChunk ZCExample
#! @BeginExample
G := AlternatingGroup(5);
#! Alt( [ 1 .. 5 ] )
HeLP_ZC(G);
#! true
C := CharacterTable("A5");
#! CharacterTable( "A5" )
HeLP_ZC(C);
#! true
HeLP_sol;
#! [ [ [ [ 1 ] ] ], [ [ [ 1 ] ] ], [ [ [ 1 ] ] ],, 
#!   [ [ [ 0, 1 ] ], [ [ 1, 0 ] ] ], [  ],,,, [  ],,,,, [  ],,,,,,,,,,,,,,, [  ] 
#!  ]
HeLP_PrintSolution(); 
#! Solutions for elements of order 2:
#! [ [         u ],
#!   [  [ "2a" ] ],
#!   [       --- ],
#!   [     [ 1 ] ] ]
#! Solutions for elements of order 3:
#! [ [         u ],
#!   [  [ "3a" ] ],
#!   [       --- ],
#!   [     [ 1 ] ] ]
#! Solutions for elements of order 5:
#! [ [               u ],
#!   [  [ "5a", "5b" ] ],
#!   [             --- ],
#!   [        [ 0, 1 ] ],
#!   [        [ 1, 0 ] ] ]
#! There are no admissible partial augmentations for elements of order 6.
#! There are no admissible partial augmentations for elements of order 10.
#! There are no admissible partial augmentations for elements of order 15.
#! There are no admissible partial augmentations for elements of order 30.

#! @EndExample

#!  This is the classical example of Luthar and Passi to verify the Zassenhaus
#!  Conjecture for the alternating group of degree 5, cf. <Cite Key="LP"/>.
#!  In the first call of <K>HeLP_ZC</K> this is checked using the character table computed
#!  by GAP using the given group, the second call uses the character table from the
#!  character table library.
#!  The entries of <K>HeLP_sol</K> are 
#!   * lists with entries 0 and 1 (at the spots 1, 2, 3 and 5, which correspond to torsion units that are conjugate to group elements),
#!   * empty lists (at the spots 6, 10, 15 and 30, stating that there are no admissible partial augmentations for these orders),
#!   * or are not bound (these orders were not checked as they don't divide the exponent of the group).
#!   
#! The function <Ref Func='HeLP_PrintSolution'/> can be used to display the result in a pretty way.

#! @BeginExample
C := CharacterTable( "A6" );
#! CharacterTable( "A6" )
SetInfoLevel(HeLP_Info, 2);
HeLP_ZC(C);
#! #I  Checking order 2.
#! #I  Checking order 3.
#! #I  Checking order 4.
#! #I  Checking order 5.
#! #I  Checking order 6.
#! #I  Checking order 10.
#! #I  Checking order 12.
#! #I  Checking order 15.
#! #I  Checking order 20.
#! #I  Checking order 30.
#! #I  Checking order 60.
#! #I  ZC can't be solved, using the given data, for the orders: [ 6 ].
#! false
HeLP_sol[6];                                          
#! [ [ [ 1 ], [ 0, 1 ], [ -2, 2, 1 ] ], [ [ 1 ], [ 1, 0 ], [ -2, 1, 2 ] ] ]
HeLP_PrintSolution(6);                                
#! Solutions for elements of order 6:
#! [ [                   u^3,                   u^2,                     u ],
#!   [              [ "2a" ],        [ "3a", "3b" ],  [ "2a", "3a", "3b" ] ],
#!   [                   ---,                   ---,                   --- ],
#!   [                 [ 1 ],              [ 0, 1 ],          [ -2, 2, 1 ] ],
#!   [                 [ 1 ],              [ 1, 0 ],          [ -2, 1, 2 ] ] ]
SetInfoLevel(HeLP_Info, 1);
#! @EndExample

#!  This is the example M. Hertweck deals with in his article <Cite Key="HerA6"/>.  The HeLP-method is not
#!  sufficient to verify the Zassenhaus Conjecture for this group.  There are two tuples of
#!  possible partial augmentations for torsion units of order 6 which are admissible by the HeLP method.
#!  M. Hertweck used a different argument to eliminate these possibilities.

#! @BeginExample
G := SmallGroup(48,30);;
StructureDescription(G);
#! "A4 : C4"
HeLP_ZC(G);
#! #I  ZC can't be solved, using the given data, for the orders: [ 4 ].
#! false
Size(HeLP_sol[4]);
#! 10
#! @EndExample

#!  The group SmallGroup(48,30) is the smallest group for which the HeLP method does not suffice to prove the Zassenhaus Conjecture. However
#!  (ZC) was proved for this group in <Cite Key="HoefertKimmerle"/>, Proposition 4.2.

#! @BeginExample
C1 := CharacterTable(SymmetricGroup(5));
#! CharacterTable( Sym( [ 1 .. 5 ] ) )
HeLP_ZC(C1);
#! #I  The Brauer tables for the following primes are not available: [ 2, 3, 5 ].
#! #I  ZC can't be solved, using the given data, for the orders: [ 4, 6 ].
#! false
C2 := CharacterTable("S5");
#! CharacterTable( "A5.2" )
HeLP_ZC(C2); 
#! true
#! @EndExample

#! This example demonstrates the advantage of using the GAP character table library: Since GAP can't
#! compute the Brauer tables from the ordinary table of $S_5$ in the current implementation, they are not used
#! in the first calculation. But in the second 
#! calculation <K>HeLP_ZC</K> accesses the Brauer tables from the library and can prove the Zassenhaus 
#! Conjecture for this group, see <Cite Key="HertweckBrauer"/>, Section 5.  This example might of course change
#! as soon as GAP will be able to compute the needed Brauer tables.

#! @BeginExample
C := CharacterTable("M11");
#! CharacterTable( "M11" )
HeLP_ZC(C);
#! #I  ZC can't be solved, using the given data, for the orders: [ 4, 6, 8 ].
#! false
HeLP_sol[12];
#! [  ]
HeLP_PrintSolution(8);
#! Solutions for elements of order 8:
#! [ [      u^4,                         u^2,                           u ],
#!   [ [ "2a" ],              [ "2a", "4a" ],  [ "2a", "4a", "8a", "8b" ] ],
#!   [      ---,                         ---,                         --- ],
#!   [    [ 1 ],                    [ 0, 1 ],              [ 0, 0, 0, 1 ] ],
#!   [    [ 1 ],                    [ 0, 1 ],              [ 0, 0, 1, 0 ] ],
#!   [    [ 1 ],                    [ 0, 1 ],             [ 0, 2, -1, 0 ] ],
#!   [    [ 1 ],                    [ 0, 1 ],             [ 0, 2, 0, -1 ] ],
#!   [    [ 1 ],                   [ 2, -1 ],              [ 0, 0, 0, 1 ] ],
#!   [    [ 1 ],                   [ 2, -1 ],              [ 0, 0, 1, 0 ] ],
#!   [    [ 1 ],                   [ 2, -1 ],             [ 0, 2, -1, 0 ] ],
#!   [    [ 1 ],                   [ 2, -1 ],             [ 0, 2, 0, -1 ] ] ]
#! @EndExample

#! Comparing this example to the result in <Cite Key="KonovalovM11"/> one sees, that the existence of elements
#! of order 12 in $\mathrm{V}(\mathbb{Z}M_{11})$ may not be eliminated using only the HeLP method.
#! This may be done however by applying also the Wagner test, cf. Section <Ref Sect='Chapter_Background_Section_The_Wagner_test'/> and the example for the function <Ref Func='HeLP_WagnerTest'/>.<P/>
#! This example also demonstrates, why also the partial augmentations of the powers of $u$ must be stored (and not only the partial augmentations of $u$).
#! To prove that all elements of order $8$ in $\mathrm{V}(\mathbb{Z}M_{11})$ are rationally conjugate to group elements, it is not 
#! enough to prove that all elements $u$ of order $8$ in $\mathrm{V}(\mathbb{Z}M_{11})$ have all partial augmentations $1$ and $0$, as
#! the fifth and sixth possibility from above still could exist in $\mathrm{V}(\mathbb{Z}M_{11})$, which would not be rationally conjugate to group elements.
#! @EndChunk

#! @BeginChunk PQExample
#! @BeginExample
C := CharacterTable("A7");
#! CharacterTable( "A7" )
HeLP_PQ(C);
#! true
#! @EndExample
#! The Prime Graph Question for the alternating group of degree 7 was first proved by M. Salim <Cite Key="SalimA7A8"/>.
#! @BeginExample
C := CharacterTable("L2(19)");
#! CharacterTable( "L2(19)" )
HeLP_PQ(C);                   
#! true
HeLP_ZC(C);
#! #I  (ZC) can't be solved, using the given data, for the orders: [ 10 ].
#! false
HeLP_sol[10];
#! [ [ [ 1 ], [ 0, 1 ], [ 0, -1, 1, 0, 1 ] ], 
#!   [ [ 1 ], [ 0, 1 ], [ 0, 0, 0, 1, 0 ] ], 
#!   [ [ 1 ], [ 1, 0 ], [ 0, 0, 0, 0, 1 ] ], 
#!   [ [ 1 ], [ 1, 0 ], [ 0, 1, -1, 1, 0 ] ] ]
#! @EndExample
#! The HeLP method provides an affirmative answer to the Prime Graph Question
#! for the group L2(19), although the method doesn't solve the Zassenhaus Conjecture for that
#! group, as there are two sets  of possible partial augmentations for units of order 10 left, which do not correspond
#! to elements which are rationally conjugate to group elements. The Zassenhaus Conjecture for this group is proved in <Cite Key="BaMaM10"/>.
#! @BeginExample
C1 := CharacterTable(PSL(2,7));                                  
#! CharacterTable( Group([ (3,7,5)(4,8,6), (1,2,6)(3,4,8) ]) )
HeLP_PQ(C1);
#! #I  The Brauer tables for the following primes are not available: [ 2, 3, 7 ].
#! #I  PQ can't be solved, using the given data, for the orders: [ 6 ].
#! false
C2 := CharacterTable("L2(7)");  
#! CharacterTable( "L3(2)" )
HeLP_PQ(C2);                  
#! true
#! @EndExample

#! This example demonstrates the advantage of using tables from the GAP character table library: Since GAP can not
#! compute the Brauer tables corresponding to <K>C1</K> they are not used in the first calculation. But in the second 
#! calculation <K>HeLP_PQ</K> accesses the Brauer tables from the library and can prove the Prime
#! Graph Question for this group, see <Cite Key="HertweckBrauer"/>, Section 6. This example might change, 
#! as soon as GAP will be able to compute the Brauer tables needed.

#! @BeginExample
SetInfoLevel(HeLP_Info,2);
C := CharacterTable("A6");
#! CharacterTable( "A6" )
HeLP_PQ(C);
#! #I  Checking order 2.
#! #I  Checking order 3.
#! #I  Checking order 5.
#! #I  Checking order 6.
#! #I  Checking order 10.
#! #I  Checking order 15.
#! #I  PQ can't be solved, using the given data, for the orders: [ 6 ].
#! false
SetInfoLevel(HeLP_Info,1);
#! @EndExample

#! The Prime Graph Question can not be confirmed for the alternating group of degree 6 with the HeLP-method.
#! This group is handled in <Cite Key="HerA6"/> by other means.

#! @BeginExample
C := CharacterTable("L2(49)");
#! CharacterTable( "L2(49)" )
HeLP_PQ(C);
#! #I  The Brauer tables for the following primes are not available: [ 7 ].
#! #I  (PQ) can't be solved, using the given data, for the orders: [ 10, 15 ].
#! false
#! @EndExample
#! This example shows the limitations of the program. Using the Brauer table for the prime 7 one can prove (PQ) for PSL(2,49), but this data is not available in GAP at the moment.
#! The fact that there are no torsion units of order 10 and 15 was proved in <Cite Key="HertweckBrauer"/>, Proposition 6.7. See also the example in Section <Ref Sect='Chapter_Extended_examples_Section_Non-standard_characters'/>. The other critical orders were handled in a more general context in <Cite Key="BaMa4prI"/>.
#! @EndChunk


#! @BeginChunk GOExample
#! @BeginExample
C := CharacterTable("A5");
#! CharacterTable( "A5" )
HeLP_WithGivenOrder(C, 5);
#! #I  Number of solutions for elements of order 5: 2; stored in HeLP_sol[5].
#! [ [ [ 0, 1 ] ], [ [ 1, 0 ] ] ]
HeLP_PrintSolution(5);
#! Solutions for elements of order 5:
#! [ [               u ],.
#!   [  [ "5a", "5b" ] ],
#!   [             --- ],
#!   [        [ 0, 1 ] ],
#!   [        [ 1, 0 ] ] ]
#! @EndExample
#! Tests which partial augmentations for elements of order 5 are admissible.

#! @BeginExample
C := CharacterTable("A6");
#! CharacterTable( "A6" )
HeLP_WithGivenOrder(C, 4);
#! #I  Number of solutions for elements of order 4: 4; stored in HeLP_sol[4].
#! [ [ [ 1 ], [ -1, 2 ] ], [ [ 1 ], [ 2, -1 ] ], [ [ 1 ], [ 1, 0 ] ], 
#!   [ [ 1 ], [ 0, 1 ] ] ]
HeLP_sol[4];              
#! [ [ [ 1 ], [ -1, 2 ] ], [ [ 1 ], [ 2, -1 ] ], [ [ 1 ], [ 1, 0 ] ], 
#!   [ [ 1 ], [ 0, 1 ] ] ]
#! @EndExample

#! Two of the non-trivial partial augmentations can be eliminated by using the 
#! Brauer table modulo the prime $3$:

#! @BeginExample
HeLP_WithGivenOrder(C mod 3, 4);                                                                    
#! #I  Number of solutions for elements of order 4: 2; stored in HeLP_sol[4].
#! [ [ [ 1 ], [ 1, 0 ] ], [ [ 1 ], [ 0, 1 ] ] ]
#! @EndExample

#! When using <K>HeLP_ZC</K> also the last remaining non-trivial partial augmentation disappears,
#! as this function applies the Wagner test, cf. <Ref Sect='Chapter_Background_Section_The_Wagner_test'/> and <Ref Func='HeLP_WagnerTest'/>:

#! @BeginExample
HeLP_ZC(C);               
#! #I  ZC can't be solved, using the given data, for the orders: [ 6 ].
#! false
HeLP_sol[4]; HeLP_sol[6];
#! [ [ [ 1 ], [ 0, 1 ] ] ]
#! [ [ [ 1 ], [ 0, 1 ], [ -2, 2, 1 ] ], [ [ 1 ], [ 1, 0 ], [ -2, 1, 2 ] ] ]
#! @EndExample

#! The following example demonstrates how one can use lists of characters to 
#! obtain constraints for partial augmentations:

#! @BeginExample
C := CharacterTable("L2(49).2_1");   
#! CharacterTable( "L2(49).2_1" )
HeLP_WithGivenOrder(Irr(C), 7);;
#! #I  Number of solutions for elements of order 7: 1; stored in HeLP_sol[7].
HeLP_WithGivenOrder(Irr(C){[2]}, 14);
#! #I  The given data admit infinitely many solutions for elements of order 14.
HeLP_WithGivenOrder(Irr(C){[44]}, 14);
#! #I  The given data admit infinitely many solutions for elements of order 14.
HeLP_WithGivenOrder(Irr(C){[2,44]}, 14);
#! #I  Number of solutions for elements of order 14: 0; stored in HeLP_sol[14].
#! [  ]
#! @EndExample

#! Brauer tables can provide more restrictions on partial augmentations of certain torsion units:

#! @BeginExample
C := CharacterTable("J1");       
#! CharacterTable( "J1" )
HeLP_WithGivenOrder(C, 6);;
#! #I  Number of solutions for elements of order 6: 73; stored in HeLP_sol[6].
B := C mod 11;
#! BrauerTable( "J1", 11 )
HeLP_WithGivenOrder(B, 6);;       
#! #I  Number of solutions for elements of order 6: 6; stored in HeLP_sol[6].
HeLP_WithGivenOrder(Irr(B){[2,3]}, 6);;
#! #I  Number of solutions for elements of order 6: 6; stored in HeLP_sol[6].
HeLP_PrintSolution(6);
#! Solutions for elements of order 6:
#! [ [                   u^3,                   u^2,                     u ],
#!   [              [ "2a" ],              [ "3a" ],  [ "2a", "3a", "6a" ] ],
#!   [                   ---,                   ---,                   --- ],
#!   [                 [ 1 ],                 [ 1 ],          [ -2, 0, 3 ] ],
#!   [                 [ 1 ],                 [ 1 ],          [ 2, 0, -1 ] ],
#!   [                 [ 1 ],                 [ 1 ],           [ 0, 0, 1 ] ],
#!   [                 [ 1 ],                 [ 1 ],          [ -4, 3, 2 ] ],
#!   [                 [ 1 ],                 [ 1 ],          [ 0, 3, -2 ] ],
#!   [                 [ 1 ],                 [ 1 ],          [ -2, 3, 0 ] ] ]
#! @EndExample

#! The result of the previous example can be found in <Cite Key="BJK"/>.  <P/>
#! When dealing with many variables using lists of characters instead of a complete character table might also speed up the calculations a lot,
#! see Section <Ref Sect='Chapter_Extended_examples_Section_Saving_time'/>.
#! @BeginExample
C := CharacterTable("L2(27)");
#! CharacterTable( "L2(27)" )
HeLP_WithGivenOrder(C,7);;
#! #I  Number of solutions for elements of order 7: 78; stored in HeLP_sol[7].
SetInfoLevel(HeLP_Info,4);
HeLP_WithGivenOrder(C,3*7); 
#! #I      Solutions for order 3 not yet calculated.  Restart for this order.
#! #I  Number of solutions for elements of order 21: 0; stored in HeLP_sol[21].  
#! [  ]
SetInfoLevel(HeLP_Info,1);
#! @EndExample
#! <K>HeLP_WithGivenOrder</K> often needs to consider many cases.  Set the info class HeLP_Info to a level 4 or higher to keep track
#! of the progress, see Section <Ref Sect='Chapter_Extended_examples_Section_Using_InfoLevels'/> on info levels. 
#! @EndChunk


#! @BeginChunk PAExample
#! @BeginExample
G := SmallGroup(48,33);; StructureDescription(G);
#! "SL(2,3) : C2"
C := CharacterTable(G);;
HeLP_WithGivenOrder(C, 4);;
#! #I  Number of solutions for elements of order 4: 4; stored in HeLP_sol[4].
HeLP_WithGivenOrder(C, 6);;
#! #I  Number of solutions for elements of order 6: 2; stored in HeLP_sol[6].
HeLP_sol[4]; HeLP_sol[6];
#! [ [ [ 1, 0 ], [ 0, 1, 0, 0, 0 ] ], [ [ 1, 0 ], [ 0, 0, 0, 0, 1 ] ], 
#!   [ [ 1, 0 ], [ 0, 0, 0, 1, 0 ] ], [ [ 1, 0 ], [ 0, 0, 1, 0, 0 ] ] ]
#! [ [ [ 1, 0 ], [ 0, 1 ], [ 0, 0, 0, 0, 1, 0 ] ], 
#!   [ [ 1, 0 ], [ 1, 0 ], [ 0, 0, 0, 0, 0, 1 ] ] ]
HeLP_WithGivenOrderAndPA(C, 12, [ [ 1, 0 ],  [ 0, 1 ], [ 0, 0, 0, 0, 1 ],
    [ 0, 0, 0, 0, 1, 0 ] ]); 
#! #I  Number of solutions for elements of order 12 with these partial augmentation
#! s for the powers: 1.
#! [ [ [ 1, 0 ], [ 0, 1 ], [ 0, 0, 0, 0, 1 ], [ 0, 0, 0, 0, 1, 0 ], 
#!       [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 ] ] ]
HeLP_WithGivenOrderAndPA(C, 12, [ [ 1, 0 ],  [ 0, 1 ], [ 0, 0, 0, 1, 0 ],
    [ 0, 0, 0, 0, 1, 0 ] ]);
#! #I  Number of solutions for elements of order 12 with these partial augmentation
#! s for the powers: 0.
#! [  ]
#! @EndExample

#! In the calls of <K>HeLP_WithGivenOrderAndPA</K> the function uses the following partial augmentations:
#!  * <K>[ 1, 0 ]</K> for the element <M>u^6</M> of order 2,
#!  * <K>[ 0, 1 ]</K> for the element <M>u^4</M> of order 3,
#!  * <K>[ 0, 0, 0, 0, 1 ]</K> and <K>[ 0, 0, 0, 1, 0 ]</K> for the element <M>u^3</M> of order 4 respectively,
#!  * <K>[ 0, 0, 0, 0, 1, 0 ]</K> for the element <M>u^2</M> of order 6.
#! @EndChunk


#! @BeginChunk PASSExample
#! @BeginExample
C := CharacterTable("A5");
#! CharacterTable( "A5" )
chi := Irr(C)[2];; psi := Irr(C)[4];
#! Character( CharacterTable( "A5" ), [ 4, 0, 1, -1, -1 ] )
HeLP_WithGivenOrderAndPAAndSpecificSystem([[chi, 1], [chi, 2]],
 5, [ ], true);
#! [ [ [ [ 0, 1 ] ], [ [ 1, 0 ] ] ], [ [ -3/5, 2/5 ], [ 2/5, -3/5 ] ], [ 3/5, 3/5 ] ]
sol5 := HeLP_WithGivenOrderAndPAAndSpecificSystem([[chi, 1], [chi, 2]], 
 5, [ ]);      
#! [ [ [ 0, 1 ] ], [ [ 1, 0 ] ] ]
#! @EndExample
#! The inequalities in the above examples are:
#! $$\frac{-3}{5}\varepsilon_{5a}(u) + \frac{2}{5}\varepsilon_{5b}(u) + \frac{3}{5} \in \mathbb{Z}_{\geq 0} \ \ {\rm{and}} \ \ \frac{2}{5}\varepsilon_{5a}(u) + \frac{-3}{5}\varepsilon_{5b}(u) + \frac{3}{5} \in \mathbb{Z}_{\geq 0}. $$
#! Continuing the above example:
#! @BeginExample
HeLP_WithGivenOrderAndPAAndSpecificSystem([psi], 
 2*5, [[1], sol5[1][1]], true);     
#! [ [  ], [ [ 0, -2/5, -2/5 ], [ 0, -1/10, -1/10 ], [ 0, 1/10, 1/10 ],
#!  [ 0, -1/10, -1/10 ], [ 0, 1/10, 1/10 ], [ 0, 2/5, 2/5 ], 
#!  [ 0, 1/10, 1/10 ], [ 0, -1/10, -1/10 ], [ 0, 1/10, 1/10 ], 
#! [ 0, -1/10, -1/10 ] ], [ 0, 1/2, 1/2, 1/2, 1/2, 0, 1/2, 1/2, 1/2, 1/2 ] ]
HeLP_WithGivenOrderAndPAAndSpecificSystem([[psi, 0], [psi, 2], [psi, 5]], 
 2*5, [[1], sol5[2][1]], true); 
#! [ [  ], [ [ 0, -2/5, -2/5 ], [ 0, 1/10, 1/10 ], [ 0, 2/5, 2/5 ] ], [ 0, 1/2, 0 ] ]
#! @EndExample
#! @EndChunk

#! @BeginChunk SCExample

#! @BeginExample
C := CharacterTable("A6");;
HeLP_WithGivenOrder(C, 6);            
#! #I  Number of solutions for elements of order 6: 2; stored in HeLP_sol[6].
#! [ [ [ 1 ], [ 0, 1 ], [ -2, 2, 1 ] ], [ [ 1 ], [ 1, 0 ], [ -2, 1, 2 ] ] ]
HeLP_WithGivenOrderSConstant(C, 2, 3);
#! [ [ [ 0, 1 ], [ -2, 2, 1 ] ], [ [ 1, 0 ], [ -2, 1, 2 ] ] ]
HeLP_WithGivenOrderSConstant(C, 3, 2);     
#! [ [ [ 1 ], [ 3, -2 ] ] ]
#! @EndExample

#! @BeginExample
C := CharacterTable("Sz(8)");;
SetInfoLevel(HeLP_Info, 4);
HeLP_WithGivenOrderSConstant(C, 7, 13);
#! #I    Partial augmentations for elements of order 13 not yet calculated.  Restar
#! t for this order.
#! #I    Number of non-trivial 7-constant characters in the list: 7.
#! [  ]
SetInfoLevel(HeLP_Info, 1);
#! @EndExample
#! The last example can also be checked by using all characters in <K>C</K>, but this takes notably longer.
#! @BeginExample
C := CharacterTable("Sz(32)");
#! CharacterTable( "Sz(32)" )
L := Filtered(OrdersClassRepresentatives(C), x-> x = 31);; Size(L);
#! 15           # I.e. HeLP_WithGivenOrder(C,31) would take hopelessly long
HeLP_WithGivenOrderSConstant(C mod 2, 31, 5);
#! [  ]
IsBound(HeLP_sol[31]);
#! false 
#! @EndExample
#! We still have no clue about elements of order 31, but there are none of order 5*31.
#! @EndChunk

#! @BeginChunk AllOrdersExample
#! @BeginExample
C := CharacterTable(PSL(2,7));         
#! CharacterTable( Group([ (3,7,5)(4,8,6), (1,2,6)(3,4,8) ]) )
HeLP_ZC(C);        
#! #I  The Brauer tables for the following primes are not available: [ 2, 3, 7 ].
#! #I  (ZC) can't be solved, using the given data, for the orders: [ 6 ].
#! false
HeLP_sol[6] := [ ];
#! [  ]
HeLP_AllOrders(C);
#! true
#! @EndExample
#! @EndChunk

#! @BeginChunk AllOrdersExamplePQ
#! @BeginExample
C := CharacterTable("A12");
#! CharacterTable( "A12" )
HeLP_WithGivenOrder(Irr(C){[2, 4, 7]}, 2);;
#! #I  Number of solutions for elements of order 2: 37; stored in HeLP_sol[2].
HeLP_WithGivenOrderSConstant(C mod 3,11,2);
#! [  ]
HeLP_WithGivenOrder(Irr(C mod 2){[2, 3, 4, 6]}, 3);;
#! #I  Number of solutions for elements of order 3: 99; stored in HeLP_sol[3].
HeLP_WithGivenOrderSConstant(C mod 2, 11, 3);
#! [  ]
HeLP_AllOrdersPQ(C);
#! true
#! @EndExample
#! Thus the Prime Graph Question holds for the alternating group of degree 12. 
#! Just using <K>HeLP_PQ(C)</K> would take hopelessly long.
#! @EndChunk


#! @BeginChunk WTExample
#! @BeginExample
C := CharacterTable("M11");
#! CharacterTable( "M11" )
HeLP_WithGivenOrder(C,8);;
#! #I  Number of solutions for elements of order 8: 36; stored in HeLP_sol[8].
HeLP_sol[8] := HeLP_WagnerTest(8);;
Size(HeLP_sol[8]);
#! 24
#! @EndExample
#! Thus the Wagner-Test eliminates 12 possible partial augmentations for elements of order 8.
#! Continuing the example:
#! @BeginExample
HeLP_WithGivenOrder(C,12);
#! #I  Number of solutions for elements of order 12: 7; stored in HeLP_sol[12].
#! [ [ [ 1 ], [ 1 ], [ 2, -1 ], [ 0, 3, -2 ], [ 1, 0, -1, 1 ] ], 
#!   [ [ 1 ], [ 1 ], [ 1, 0 ], [ 0, 3, -2 ], [ 0, 0, 0, 1 ] ], 
#!   [ [ 1 ], [ 1 ], [ -1, 2 ], [ 0, 3, -2 ], [ 0, 0, 2, -1 ] ], 
#!   [ [ 1 ], [ 1 ], [ 0, 1 ], [ 0, 3, -2 ], [ 1, 0, 1, -1 ] ], 
#!   [ [ 1 ], [ 1 ], [ 0, 1 ], [ 0, 3, -2 ], [ -1, 0, 1, 1 ] ], 
#!   [ [ 1 ], [ 1 ], [ 1, 0 ], [ 0, -3, 4 ], [ 0, 0, 0, 1 ] ], 
#!   [ [ 1 ], [ 1 ], [ -1, 2 ], [ 0, -3, 4 ], [ 1, 0, -1, 1 ] ] ]
HeLP_sol[12] := HeLP_WagnerTest(12);
#! [ [ [ 1 ], [ 1 ], [ 1, 0 ], [ 0, 3, -2 ], [ 0, 0, 0, 1 ] ], 
#!   [ [ 1 ], [ 1 ], [ -1, 2 ], [ 0, 3, -2 ], [ 0, 0, 2, -1 ] ], 
#!   [ [ 1 ], [ 1 ], [ 1, 0 ], [ 0, -3, 4 ], [ 0, 0, 0, 1 ] ] ]
HeLP_sol[4] := HeLP_WagnerTest(4);;
HeLP_WithGivenOrder(C,12);
#! #I  Number of solutions for elements of order 12: 3; stored in HeLP_sol[12].
#! [ [ [ 1 ], [ 1 ], [ 2, -1 ], [ 0, 3, -2 ], [ 1, 0, -1, 1 ] ], 
#!   [ [ 1 ], [ 1 ], [ 0, 1 ], [ 0, 3, -2 ], [ 1, 0, 1, -1 ] ], 
#!   [ [ 1 ], [ 1 ], [ 0, 1 ], [ 0, 3, -2 ], [ -1, 0, 1, 1 ] ] ]
HeLP_sol[12] := HeLP_WagnerTest(12);
#! [  ]
#! @EndExample
#! Thus there are no normalized units of order 12 in the integral group ring of $M_{11}.$
#! @BeginExample
C := CharacterTable("M22");
#! CharacterTable( "M22" )
HeLP_WagnerTest(12, [ [ [1], [1], [1,0], [0,0,1], [-3,3,2,3,-4] ] ],C);
#! [  ]
#! @EndExample
#! This example is taken from the appendix of <Cite Key="KonovalovM22"/>.<P/>
#! Sometimes the Wagner-Test may even prove the Zassenhaus Conjecture:
#! @BeginExample
G := SmallGroup(96,187);
#! <pc group of size 96 with 6 generators>
C := CharacterTable(G);
#! CharacterTable( <pc group of size 96 with 6 generators> )
HeLP_WithGivenOrder(C,4);;
#! #I  Number of solutions for elements of order 4: 34; stored in HeLP_sol[4].
HeLP_WagnerTest(4);       
#! [ [ [ 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 0, 0, 1 ] ],
#!   [ [ 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 1, 0, 0 ] ],
#!   [ [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1, 0, 0, 0 ] ], 
#!   [ [ 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 0, 1, 0 ] ] ]
HeLP_ZC(C);
#! true
#! @EndExample
#! @EndChunk

#! @BeginChunk AOExample
#! @BeginExample
C := CharacterTable("A6");;
HeLP_WithGivenOrder(C, 6);
#! #I  Number of solutions for elements of order 6: 2; stored in HeLP_sol[6].
#! [ [ [ 1 ], [ 0, 1 ], [ -2, 2, 1 ] ], [ [ 1 ], [ 1, 0 ], [ -2, 1, 2 ] ] ]
#! gap> HeLP_AutomorphismOrbits(C, 6);
#! [ [ [ 1 ], [ 0, 1 ], [ -2, 2, 1 ] ] ]
#! @EndExample
#! @EndChunk


#! @BeginChunk CCExample
#! To keep track of the change of the character tables one can set HeLP_Info to level 5.
#! In this first example it is not realized that the character tables belong to the same group, so the
#! solutions for elements of order 2 are recalculated (they have been reset, as another character table
#! is used).
#! @BeginExample
SetInfoLevel(HeLP_Info, 5);
C := CharacterTable(SymmetricGroup(4)); 
#! CharacterTable( Sym( [ 1 .. 4 ] ) )

HeLP_WithGivenOrder(C mod 2, 3); 
#! #I  USED CHARACTER TABLE CHANGED TO BrauerTable( SymmetricGroup( [ 1 .. 4 ] ), 2
#!  ), ALL GLOBAL VARIABLES RESET.
#! #I  Number of solutions for elements of order 3: 1; stored in HeLP_sol[3].
#! [ [ [ 1 ] ] ]
HeLP_WithGivenOrder(C, 2*3);    
#! #I  USED CHARACTER TABLE CHANGED TO CharacterTable( SymmetricGroup( [ 1 .. 4 ] )
#!  ), ALL GLOBAL VARIABLES RESET.
#! #I      Solutions for order 2 not yet calculated.  Restart for this order.
#! #I      Solutions for order 3 not yet calculated.  Restart for this order.
#! #I  Number of solutions for elements of order 6: 0; stored in HeLP_sol[6].
#! [  ]
#! @EndExample

#! The recalculations of the solutions can be avoided by calling <K>HeLP_ChangeCharKeepSols</K>
#! before using another character table.
#! @BeginExample
D := CharacterTable(SymmetricGroup(4));
#! CharacterTable( Sym( [ 1 .. 4 ] ) )
HeLP_WithGivenOrder(D mod 2, 3);       
#! #I  USED CHARACTER TABLE CHANGED TO BrauerTable( SymmetricGroup( [ 1 .. 4 ] ), 2
#!  ), ALL GLOBAL VARIABLES RESET.
#! #I  Number of solutions for elements of order 3: 1; stored in HeLP_sol[3].
#! [ [ [ 1 ] ] ]
HeLP_ChangeCharKeepSols(D);
#! #I  WARNING: Change used character table without checking if the character table
#! s have the same underlying groups and the ordering of the conjugacy classes are 
#! the same!
HeLP_WithGivenOrder(D, 2*3);    
#! #I  Using same character table as until now; all known solutions kept.
#! #I      Solutions for order 2 not yet calculated.  Restart for this order.
#! #I  Number of solutions for elements of order 6: 0; stored in HeLP_sol[6].
#! [  ]
#! @EndExample

#! When using tables from the ATLAS this is done automatically:
#! @BeginExample
CA := CharacterTable("A5");
#! CharacterTable( "A5" )
HeLP_WithGivenOrder(CA mod 2, 5);
#! #I  USED CHARACTER TABLE CHANGED TO BrauerTable( "A5", 2 ), ALL GLOBAL VARIABLES
#!  RESET.
#! #I  Testing possibility 1 out of 1.
#! #I  Number of solutions for elements of order 5: 2; stored in HeLP_sol[5].
#! [ [ [ 0, 1 ] ], [ [ 1, 0 ] ] ]
HeLP_WithGivenOrder(CA, 2*5);    
#! #I  Using character table of the same group; all known solutions kept.
#! #I      Solutions for order 2 not yet calculated.  Restart for this order.
#! #I  Number of solutions for elements of order 10: 0; stored in HeLP_sol[10].
#! [  ]
SetInfoLevel(HeLP_Info, 1);
#! @EndExample

#! @EndChunk






#! @BeginChunk CSExample
#! @BeginExample
C := CharacterTable("A6");;
HeLP_WithGivenOrder(C, 4);
#! #I  Number of solutions for elements of order 4: 4; stored in HeLP_sol[4].
#! [ [ [ 1 ], [ -1, 2 ] ], [ [ 1 ], [ 2, -1 ] ], [ [ 1 ], [ 1, 0 ] ], 
#!   [ [ 1 ], [ 0, 1 ] ] ]
HeLP_VerifySolution(C mod 3, 4);
#! [ [ [ 1 ], [ 1, 0 ] ], [ [ 1 ], [ 0, 1 ] ] ]
HeLP_sol[4];
#! [ [ [ 1 ], [ 1, 0 ] ], [ [ 1 ], [ 0, 1 ] ] ]
#! @EndExample

#! @BeginExample
C := CharacterTable("S12");;
HeLP_WithGivenOrder(Irr(C mod 5){[2..6]}, 2);;
#! #I  Number of solutions for elements of order 2: 563; stored in HeLP_sol[2].
HeLP_VerifySolution(C mod 5, 2);;
Size(HeLP_sol[2]);
#! 387
HeLP_VerifySolution(C mod 3, 2);;
Size(HeLP_sol[2]);
#! 324
#! @EndExample
#! Using <K>HeLP_WithGivenOrder(C mod 5, 2)</K> or <K>HeLP_WithGivenOrder(C mod 3, 2)</K> takes much longer
#! since in that case a bigger system of inequalities must be solved.
#! @EndChunk

#! @BeginChunk PPExample
#! @BeginExample
SetInfoLevel(HeLP_Info,4);
C := CharacterTable(SmallGroup(160,91));
#! CharacterTable( <pc group of size 160 with 6 generators> )
HeLP_WithGivenOrder(C,4);;
#! #I      Solutions for order 2 not yet calculated.  Restart for this order.
#! #I  Number of solutions for elements of order 4: 22; stored in HeLP_sol[4].   
HeLP_WithGivenOrder(C,10);;
#! #I      Solutions for order 5 not yet calculated.  Restart for this order.
#! #I  Number of solutions for elements of order 10: 6; stored in HeLP_sol[10].  
LP := HeLP_PossiblePartialAugmentationsOfPowers(20);;
HeLP_WithGivenOrderAndPA(Irr(C){[2..20]},20,LP[1]);
#! #I  Number of solutions for elements of order 20 with these partial augmentations
#!  for the powers: 0.
#! [  ]
#! @EndExample
#! @EndChunk

#! @BeginChunk TSExample
#! With the character tables that are currently available in GAP, the Zassenhaus Conjecture
#! for elements of order $4$ in $\text{PSL}(2,49)$ cannot be solved. However it was proved in
#! <Cite Key="HertweckBrauer"/> using the Brauer table modulo $7$.
#! @BeginExample
C := CharacterTable("L2(49)");
#! CharacterTable( "L2(49)" )
HeLP_WithGivenOrder(C, 4);
#! #I  Number of solutions for elements of order 4: 14; stored in HeLP_sol[4].
#! [ [ [ 1 ], [ -6, 7 ] ], [ [ 1 ], [ -5, 6 ] ], [ [ 1 ], [ -4, 5 ] ], 
#!   [ [ 1 ], [ -3, 4 ] ], [ [ 1 ], [ -2, 3 ] ], [ [ 1 ], [ -1, 2 ] ], 
#!   [ [ 1 ], [ 0, 1 ] ], [ [ 1 ], [ 1, 0 ] ], [ [ 1 ], [ 2, -1 ] ], 
#!   [ [ 1 ], [ 3, -2 ] ], [ [ 1 ], [ 4, -3 ] ], [ [ 1 ], [ 5, -4 ] ], 
#!   [ [ 1 ], [ 6, -5 ] ], [ [ 1 ], [ 7, -6 ] ] ]
C mod 7;
#! fail
HeLP_WriteTrivialSolution(C, 4);;
HeLP_sol[4];
#! [ [ [ 1 ], [ 0, 1 ] ] ]
#! @EndExample
#! @EndChunk

#! @BeginChunk PSExample
#! @BeginExample
C := CharacterTable("A5");;
HeLP_ZC(C);          
#! true
HeLP_PrintSolution();
#! Solutions for elements of order 2:
#! [ [         u ],
#!   [  [ "2a" ] ],
#!   [       --- ],
#!   [     [ 1 ] ] ]
#! Solutions for elements of order 3:
#! [ [         u ],
#!   [  [ "3a" ] ],
#!   [       --- ],
#!   [     [ 1 ] ] ]
#! Solutions for elements of order 5:
#! [ [               u ],
#!   [  [ "5a", "5b" ] ],
#!   [             --- ],
#!   [        [ 0, 1 ] ],
#!   [        [ 1, 0 ] ] ]
#! There are no admissible partial augmentations for elements of order 6.
#! There are no admissible partial augmentations for elements of order 10.
#! There are no admissible partial augmentations for elements of order 15.
#! There are no admissible partial augmentations for elements of order 30.
C := CharacterTable("A6");;
HeLP_ZC(C);           
#! #I  ZC can't be solved, using the given data, for the orders: [ 6 ].
#! false
HeLP_PrintSolution(6);
#! Solutions for elements of order 6:
#! [ [                   u^3,                   u^2,                     u ],
#!   [              [ "2a" ],        [ "3a", "3b" ],  [ "2a", "3a", "3b" ] ],
#!   [                   ---,                   ---,                   --- ],
#!   [                 [ 1 ],              [ 0, 1 ],          [ -2, 2, 1 ] ],
#!   [                 [ 1 ],              [ 1, 0 ],          [ -2, 1, 2 ] ] ]
#! @EndExample

#! @EndChunk



#! @BeginChunk  EMCVExample
#! @BeginExample
C := CharacterTable("A6");;
HeLP_WithGivenOrder(C, 6);                               
#! #I  Number of solutions for elements of order 6: 2; stored in HeLP_sol[6].
#! [ [ [ 1 ], [ 0, 1 ], [ -2, 2, 1 ] ], [ [ 1 ], [ 1, 0 ], [ -2, 1, 2 ] ] ]
chi := Irr(C)[2];;   # a character of degree 5
HeLP_MultiplicitiesOfEigenvalues(chi, 6, HeLP_sol[6][2]);
#! [ 1, 0, 1, 2, 1, 0 ]
HeLP_CharacterValue(chi, 6, HeLP_sol[6][2][3]);          
#! -2
HeLP_CharacterValue(chi, 6, [-2,1,2]);
#! -2
HeLP_CharacterValue(chi, 6, [-2,2,1]);
#! 1
#! @EndExample
#! These eigenvalues were computed manually by M. Hertweck and may be found in <Cite Key="HerA6"/>.
#! @EndChunk
