#############################################################################
##
##                                                               HeLP package
##
##                                 Andreas Bächle, Vrije Universiteit Brussel
##                                        Leo Margolis, Universität Stuttgart
##
#############################################################################

####################################
#
#! @Chapter Introduction
#
####################################

#! @InsertChunk Intro


####################################
#
#! @Chapter The main functions
#
####################################

#! @Section Zassenhaus Conjecture

#!  This function checks whether the Zassenhaus Conjecture ((ZC) for short, cf. Section
#!  <Ref Sect='Chapter_Background_Section_The_Zassenhaus_Conjecture_and_related_questions'/>) can be proved
#!  using the HeLP method with the data available in GAP.

#! @Description
#!  <K>HeLP_ZC</K> checks whether the Zassenhaus Conjecture can be solved for
#!  the given group using the HeLP method, the Wagner test and all character data available.
#!  The argument of the function can be either an ordinary character table
#!  or a group.  In the second case it will first calculate the corresponding 
#!  ordinary character table.
#!  If the group in question is nilpotent, the Zassenhaus Conjecture holds by a result of A. Weiss and the 
#!  function will return <K>true</K> without performing any calculations.<P/>
#!  If the group is not solvable, the function will check all orders
#!  which are divisors of the exponent of the group. If the group is solvable, it will only check the orders
#!  of group elements, as there can't be any torsion units of another order.
#!  The function will use the ordinary table 
#!  and, for the primes $p$ for which the group is not $p$-solvable, all $p$-Brauer tables which are available in GAP
#!  to produce as many constraints on the torsion units as possible.  Additionally, the Wagner test  
#!  is applied to the results, cf. Section <Ref Sect='Chapter_Background_Section_The_Wagner_test'/>.
#!  In case the information
#!  suffices to obtain a proof for the Zassenhaus Conjecture for this group
#!  the function will return <K>true</K> and <K>false</K> otherwise.
#!  The possible partial augmentations for elements of order <M>k</M> 
#!  and all its powers will also be stored in the list entry <K>HeLP_sol[k]</K>.<P/>
#!  The prior computed partial augmentations in <K>HeLP_sol</K> will not be used and will be overwritten.
#!  If you do not like the last fact, please use <Ref Func='HeLP_AllOrders'/>. 
#! @Arguments OrdinaryCharacterTable|Group
#! @Returns <K>true</K> if (ZC) can be solved using the given data, <K>false</K> otherwise
DeclareGlobalFunction( "HeLP_ZC" );
#! @InsertChunk ZCExample
#! @EndSection


#! @Section Prime Graph Question
#!  This function checks whether the Prime Graph Question ((PQ) for short, cf. Section 
#!  <Ref Sect='Chapter_Background_Section_The_Zassenhaus_Conjecture_and_related_questions'/>) can be verified
#!  using the HeLP method with the data available in GAP.

#! @Description
#!  <K>HeLP_PQ</K> checks whether an affirmative answer for the Prime Graph Question for
#!  the given group can be obtained using the HeLP method, the Wagner restrictions and the data available.
#!  The argument of the function can be either an ordinary character table
#!  or a group.  In the second case it will first calculate the corresponding 
#!  ordinary character table.
#!  If the group in question is solvable, the Prime Graph Question has an affirmative answer by a result of W. Kimmerle and the 
#!  function will return <K>true</K> without performing any calculations.<P/>
#!  If the group is non-solvable, the ordinary character table and all $p$-Brauer
#!  tables for primes $p$ for which the group is not $p$-solvable and which are available in GAP will be used to produce as many
#!  constraints on the torsion units as possible. Additionally, the Wagner test  
#!  is applied to the results, cf. Section <Ref Sect='Chapter_Background_Section_The_Wagner_test'/>.
#!  In case the information
#!  suffices to obtain an affirmative answer for the Prime Graph Question,
#!  the function will return <K>true</K> and it will return <K>false</K> otherwise.
#!  Let $p$ and $q$ be distinct primes such that there are elements of order $p$ and $q$ in $G$ but no elements of order $pq$.
#!  Then for $k$ being $p$, $q$ or $pq$ the function will save the possible partial augmentations for elements of order $k$
#!  and its (non-trivial) powers in <K>HeLP_sol[k]</K>. The function also does not use the previously computed partial augmentations
#!  for elements of these orders but will overwrite the content of <K>HeLP_sol</K>.
#!  If you do not like the last fact, please use <Ref Func='HeLP_AllOrdersPQ'/>. 
#! @Arguments OrdinaryCharacterTable|Group
#! @Returns <K>true</K> if (PQ) can be solved using the given data, <K>false</K> otherwise
DeclareGlobalFunction( "HeLP_PQ" );
#! @InsertChunk PQExample
#! @EndSection

#! @Section Spectrum Problem
#!  This function checks whether the Spectrum Problem ((SP) for short, cf. Section 
#!  <Ref Sect='Chapter_Background_Section_The_Zassenhaus_Conjecture_and_related_questions'/>) can be verified
#!  using the HeLP method with the data available in GAP.

#! @Description
#!  <K>HeLP_SP</K> checks whether an affirmative answer for the Spectrum Problem for
#!  the given group can be obtained using the HeLP method, the Wagner restrictions and the data available.
#!  The argument of the function can be either an ordinary character table
#!  or a group.  In the second case it will first calculate the corresponding 
#!  ordinary character table.
#!  If the group in question is solvable, the Spectrum Problem has an affirmative answer by a result of M. Hertweck and the 
#!  function will return <K>true</K> without performing any calculations.<P/>
#!  If the group is non-solvable, the ordinary character table and all $p$-Brauer
#!  tables for primes $p$ for which the group is not $p$-solvable and which are available in GAP will be used to produce as many
#!  constraints on the torsion units as possible. Additionally, the Wagner test  
#!  is applied to the results, cf. Section <Ref Sect='Chapter_Background_Section_The_Wagner_test'/>.
#!  In case the information
#!  suffices to obtain an affirmative answer for the Spectrum Problem,
#!  the function will return <K>true</K> and it will return <K>false</K> otherwise.
#!  The possible partial augmentations for elements of order <M>k</M> 
#!  and all its powers will also be stored in the list entry <K>HeLP_sol[k]</K>.<P/>
#!  The function also does not use the previously computed partial augmentations
#!  for elements of these orders but will overwrite the content of <K>HeLP_sol</K>.
#!  If you do not like the last fact, please use <Ref Func='HeLP_AllOrdersSP'/>. 
#! @Arguments OrdinaryCharacterTable|Group
#! @Returns <K>true</K> if (SP) can be solved using the given data, <K>false</K> otherwise
DeclareGlobalFunction( "HeLP_SP" );
#! @InsertChunk SPExample
#! @EndSection


#! @Section Kimmerle Problem
#!  This function checks whether the Kimmerle Problem ((KP) for short, cf. Section 
#!  <Ref Sect='Chapter_Background_Section_The_Zassenhaus_Conjecture_and_related_questions'/>) can be verified
#!  using the HeLP method with the data available in GAP.

#! @Description
#!  <K>HeLP_KP</K> checks whether an affirmative answer for the Kimmerle Problem for
#!  the given group can be obtained using the HeLP method, the Wagner restrictions and the data available.
#!  The argument of the function can be either an ordinary character table
#!  or a group.  In the second case it will first calculate the corresponding 
#!  ordinary character table.
#!  If the group in question is nilpotent, then even the Zassenahus Conjecture and so also the Kimmerle Problem has an affirmative answer by a result of Al Weiss and the 
#!  function will return <K>true</K> without performing any calculations.<P/>
#!  If the group is non-nilpotent, the ordinary character table and all $p$-Brauer
#!  tables for primes $p$ for which the group is not $p$-solvable and which are available in GAP will be used to produce as many
#!  constraints on the torsion units as possible. Additionally, the Wagner test  
#!  is applied to the results, cf. Section <Ref Sect='Chapter_Background_Section_The_Wagner_test'/>.
#!  In case the information
#!  suffices to obtain an affirmative answer for the Kimmerle Problem,
#!  the function will return <K>true</K> and it will return <K>false</K> otherwise.
#!  The possible partial augmentations for elements of order <M>k</M> 
#!  and all its powers will also be stored in the list entry <K>HeLP_sol[k]</K>.<P/>
#!  The function also does not use the previously computed partial augmentations
#!  for elements of these orders but will overwrite the content of <K>HeLP_sol</K>.
#!  If you do not like the last fact, please use <Ref Func='HeLP_AllOrdersKP'/>. 
#! @Arguments OrdinaryCharacterTable|Group
#! @Returns <K>true</K> if (KP) can be solved using the given data, <K>false</K> otherwise
DeclareGlobalFunction( "HeLP_KP" );
#! @InsertChunk KPExample
#! @EndSection



####################################
#
#! @Chapter Further functions
#
####################################

#! A short remark is probably in order on the three global variables the package is using: 
#! <K>HeLP_CT</K>, <K>HeLP_sol</K> and <K>HeLP_settings</K>. The first one stores the character table for
#! which the last calculations were performed, the second one containing at the <K>k</K>'s spot the already calculated 
#! admissible partial augmentations of elements of order $k$ (and its powers $u^d$ for $d \not= k$ a divisor of $k$).  If a function of the HeLP-package is called with 
#! a character table different from the one saved in <K>HeLP_CT</K> then the package tries to check if the character tables
#! belong to the same group. This can be done in particular for tables from the ATLAS. If this check is successful
#! the solutions already written in <K>HeLP_sol</K> are kept, otherwise this variable is reset.
#! For a more detailed account see Sections <Ref Sect='Chapter_Extended_examples_Section_The_behavior_of_the_variable_HeLP_sol'/>,
#! <Ref Sect='Chapter_Background_Section_Partial_augmentations_and_the_structure_of_HeLP_sol'/> and <Ref Func='HeLP_ChangeCharKeepSols'/>.
#! In most situations, the user does not have to worry about this, the program will 
#! take care of it as far as possible. <K>HeLP_settings</K> is a varaible which is used to store some settings on how linear inequalities are solved by the package.


#! @Section Checks for specific orders 

#! @Description
#!  Calculates the admissible partial augmentations for elements of 
#!  order <A>ord</A> using only the data given in the first argument.
#!  The first argument can be an ordinary 
#!  character table, a Brauer table, or a list of class functions, all having
#!  the same underlying character table.
#!  This function only uses the constraints of the HeLP method (from the class functions given), but does not apply
#!  the Wagner test <Ref Sect='Chapter_Background_Section_The_Wagner_test'/>.
#!  If the constraints allow only a finite number of solutions, these lists will
#!  be written in <K>HeLP_sol[ord]</K>.
#!  If for divisors <M>d</M> of <A>ord</A> solutions are already calculated and 
#!  stored in <K>HeLP_sol[d]</K>, these will be used, otherwise the function <K>HeLP_WithGivenOrder</K>
#!  will first be applied to this order and the data given in the first argument.
#! @Arguments CharacterTable|ListOfClassFunctions ord
#! @Returns List of admissible partial augmentations
DeclareGlobalFunction( "HeLP_WithGivenOrder" );

#! @InsertChunk GOExample


#! @Description
#!  Calculates the admissible partial augmentations for elements of 
#!  order <A>ord</A> using only the data given in the first argument.
#!  The first argument can be an ordinary 
#!  character table, a Brauer table, or a list of class functions, all having
#!  the same underlying character table.
#!  The function uses the partial augmentations for the powers <M>u^d</M> with <M>d</M>
#!  divisors of <M>k</M> different from <M>1</M> and <M>k</M> given in <A>partaugs</A>.
#!  Here, the <M>d</M>'s have to be in a descending order (i.e. the orders of the $u^d$'s
#!  are ascending).
#!  This function only uses the constraints of the HeLP method, but does not apply
#!  the Wagner test <Ref Sect='Chapter_Background_Section_The_Wagner_test'/>.
#!  Note that this function will not affect <K>HeLP_sol</K>.
#! @Arguments CharacterTable|ListOfClassFunctions ord partaugs
#! @Returns List of admissible partial augmentations
DeclareGlobalFunction( "HeLP_WithGivenOrderAndPA" );

#! @InsertChunk PAExample


#! @Description
#!  Calculates the admissible partial augmentations for elements of 
#!  order <A>ord</A> using the given character table <A>CharacterTable</A>
#!  and all Brauer tables that can be obtained from it. <A>CharacterTable</A> can be
#!  an ordinary or a Brauer table.  In any case, then given table will be used first to
#!  obtain a finite number of solutions (if the characteristic does not divide <A>ord</A>, 
#!  otherwise the ordinary table will be used), with the other tables only checks will be performed to
#!  restrict the number of possible partial augmentations as much as possible.  If certain Brauer
#!   tables are not avaialble, this will be printed if HeLP_Info is at least 1.
#!  This function only uses the constraints of the HeLP method, but does not apply
#!  the Wagner test <Ref Sect='Chapter_Background_Section_The_Wagner_test'/>.
#!  If the constraints allow only a finite number of solutions, these lists will
#!  be written in <K>HeLP_sol[ord]</K>.
#!  If for divisors <M>d</M> of <A>ord</A> solutions are already calculated and 
#!  stored in <K>HeLP_sol[d]</K>, these will be used, otherwise the function <K>HeLP_WithGivenOrder</K>
#!  will first be applied to this order and the data given in the first argument.
#! @Arguments CharacterTable ord
#! @Returns List of admissible partial augmentations
DeclareGlobalFunction( "HeLP_WithGivenOrderAllTables" );


#! @Description
#!  Calculates the admissible partial augmentations for elements of 
#!  order <A>ord</A> using the given character table <A>CharacterTable</A>
#!  and all other tables that can be obtained from it.  <A>CharacterTable</A> can be
#!  an ordinary or a Brauer table.  In any case, then given table will be used first to
#!  obtain a finite number of solutions (if the characteristic does not divide <A>ord</A>, 
#!  otherwise the ordinary table will be used), with the other tables only checks will be performed to
#!  restrict the number of possible partial augmentations as much as possible.  If certain Brauer
#!   tables are not avaialble, this will be printed if HeLP_Info is at least 1.
#!  The function uses the partial augmentations for the powers <M>u^d</M> with <M>d</M>
#!  divisors of <M>k</M> different from <M>1</M> and <M>k</M> given in <A>partaugs</A>.
#!  Here, the <M>d</M>'s have to be in a descending order (i.e. the orders of the $u^d$'s
#!  are ascending).
#!  This function only uses the constraints of the HeLP method, but does not apply
#!  the Wagner test <Ref Sect='Chapter_Background_Section_The_Wagner_test'/>.
#!  Note that this function will not affect <K>HeLP_sol</K>.
#! @Arguments CharacterTable ord partaugs
#! @Returns List of admissible partial augmentations
DeclareGlobalFunction( "HeLP_WithGivenOrderAndPAAllTables" );



#! @Description
#!  Calculates the admissible partial augmentations for elements of 
#!  order <A>ord</A> using only the data given in the first argument.
#!  The first argument is a list, which can contains as entries characters or pairs with first entry a character
#!  and second entrie an integer or a mixture of these.
#!  The first argument is understood as follows: If a character <M>\chi</M> is not given in a pair all
#!  inequalities obtainable by this character are used. If it is given in a pair with the integer <M>m</M>
#!  the inequalities obtainable from the multiplicity of <K>E(ord)</K> taken to the power <M>m</M>
#!  as an eigenvalue of a representation affording <M>\chi</M> are used.   
#!  The function uses the partial augmentations for the powers <M>u^d</M> with <M>d</M>
#!  divisors of <M>k</M> different from <M>1</M> and <M>k</M> given in <A>partaugs</A>.
#!  Here, the <M>d</M>'s have to be in a descending order (i.e. the orders of the $u^d$'s
#!  are ascending). 
#!  This function only uses the constraints of the HeLP method, but does not apply
#!  the Wagner test <Ref Sect='Chapter_Background_Section_The_Wagner_test'/>.
#!  Note that this function will not affect <K>HeLP_sol</K>.
#! @Arguments list ord partaugs [b]
#! @Returns List of admissible partial augmentations
DeclareGlobalFunction( "HeLP_WithGivenOrderAndPAAndSpecificSystem" );

#! @InsertChunk PASSExample


#! @Section Checks for specific orders with s-constant characters

#! When considering elements of order $st$ (in absence of elements of this order in the group
#! ; in particular when trying to prove (PQ)) and there are several conjugacy classes of 
#! elements of order $s$, it might be useful to consider $s$-constant characters 
#! (cf. Section <Ref Sect='Chapter_Background_Section_s-constant_characters'/>)
#! to reduce the computational complexity.


#! @Description
#!  Calculates the admissible partial augmentations for elements <M>u</M> of 
#!  order <M>s*t</M> using only the $s$-constant class functions that are contained in the first argument.
#!  The first argument can be an ordinary 
#!  character table, a Brauer table, or a list of class functions, all having
#!  the same underlying character table.
#!  <A>s</A> and <A>t</A> have to be different prime numbers, such that there are elements of order <A>s</A>
#!  and <A>t</A> in the group, but no elements of order <M>s*t</M>. <P/>
#!  The function filters which class functions given in the first argument are constant on all conjugacy classes
#!  of elements of order <A>s</A>.  For the element <M>u^s</M> of order <A>t</A> the partial augmentations
#!  given in <K>HeLP_sol[t]</K> are used.  If they are not yet calculated, the function calculates them first,
#!  using the data given in the first argument and stores them in <K>HeLP_sol[t]</K>.
#!  This function only uses the constraints of the HeLP method, but does not apply
#!  the Wagner test <Ref Sect='Chapter_Background_Section_The_Wagner_test'/>.
#!  If these calculations allow an infinite number of solutions of elements of order $st$ the function returns 
#!  <K>"infinite"</K>, otherwiese it returns the finite list of solutions for elements of order <M>s*t</M>.
#!  The first entry of every solution is a list of the partial augmentations of <M>u^s</M> and the second entry is a list of the 
#!  "partial augmentations" for <M>u</M>: the first entry of this list is the sum of the partial augmentations 
#!  on all classes of elements of order <A>s</A> and the other entries are the partial augmentations on the 
#!  classes of order <A>t</A>.
#!  Only in the case that the existence of units of order $s*t$ can be excluded by this function the variable 
#!  <K>HeLP_sol[s*t]</K> will be affected and <K>HeLP_sol[s*t]</K> will be set to <K>[  ]</K>.
#! @Arguments CharacterTable|ListOfClassFunctions s t
#! @Returns List of admissible "partial augmentations" or <K>"infinite"</K>
DeclareGlobalFunction( "HeLP_WithGivenOrderSConstant" );

#! @InsertChunk SCExample

#! @Description
#!  Given an ordinary character table <A>CT</A> the function calculates the orbits under the action of the Galois group
#!  and returns a list of characters containing the ones contained in <A>CT</A> and the ones obtained by summing
#!  up the Galois-orbits.
#! @Arguments CT
#! @Returns List of characters
DeclareGlobalFunction( "HeLP_AddGaloisCharacterSums" );


#! @Section Checks for all orders

#! @Description
#!  This function does almost the same as <Ref Func='HeLP_ZC'/>. It checks whether the Zassenhaus
#!  Conjecture can be verified for a group, but does not compute the partial augmentations
#!  of elements of order $k$, if <K>HeLP_sol[k]</K> already exists. It does however verify the solutions
#!  given in <K>HeLP_sol</K> using all available tables for the group, see <Ref Func='HeLP_VerifySolution'/>. 
#!  Thus some precalculations using e.g. <Ref Func='HeLP_WithGivenOrder'/> are respected.
#!  In contrast to <Ref Func='HeLP_ZC'/> 
#!  this function also does not check whether the group is nilpotent to use the Weiss-result to have an 
#!  immediate positive solution for (ZC). <P/>
#!  This function is interesting
#!  if one wants to save time or possesses some information, which was not obtained using this package and was
#!  entered manually into <K>HeLP_sol</K>.  
#! @Arguments CharacterTable|Group
#! @Returns <K>true</K> if (ZC) can be solved using the given data, <K>false</K> otherwise
DeclareGlobalFunction( "HeLP_AllOrders" );

#! @InsertChunk AllOrdersExample 

#! @Description
#!  This function does almost the same as <Ref Func='HeLP_PQ'/>. It checks whether the Prime Graph
#!  Question can be verified for a group, but does not compute the partial augmentations
#!  of elements of order $k$, if <K>HeLP_sol[k]</K> already exists. Thus some precalculations using 
#!  e.g. <Ref Func='HeLP_WithGivenOrder'/> are respected. In contrast to <Ref Func='HeLP_PQ'/> 
#!  this function also does not check whether the group is solvable to use the Kimmerle-result to have an 
#!  immediate positive solution for (PQ). <P/>
#!  This function is interesting if one wants to save time or possesses some information, which was 
#!  not obtained using this package and was entered manually into <K>HeLP_sol</K>.  
#! @Arguments CharacterTable|Group
#! @Returns <K>true</K> if (PQ) can be solved using the given data, <K>false</K> otherwise
DeclareGlobalFunction( "HeLP_AllOrdersPQ" );

#! @InsertChunk AllOrdersExamplePQ 

#! @Description
#!  This function does almost the same as <Ref Func='HeLP_SP'/>. It checks whether the Spectrum
#!  Problem can be verified for a group, but does not compute the partial augmentations
#!  of elements of order $k$, if <K>HeLP_sol[k]</K> already exists. Thus some precalculations using 
#!  e.g. <Ref Func='HeLP_WithGivenOrder'/> are respected. In contrast to <Ref Func='HeLP_SP'/> 
#!  this function also does not check whether the group is solvable to use the Hertweck-result to have an 
#!  immediate positive solution for (SP). <P/>
#!  This function is interesting if one wants to save time or possesses some information, which was 
#!  not obtained using this package and was entered manually into <K>HeLP_sol</K>.  
#! @Arguments CharacterTable|Group
#! @Returns <K>true</K> if (SP) can be solved using the given data, <K>false</K> otherwise
DeclareGlobalFunction( "HeLP_AllOrdersSP" );

#! @Description
#!  This function does almost the same as <Ref Func='HeLP_KP'/>. It checks whether the Kimmerle
#!  Problem can be verified for a group, but does not compute the partial augmentations
#!  of elements of order $k$, if <K>HeLP_sol[k]</K> already exists. Thus some precalculations using 
#!  e.g. <Ref Func='HeLP_WithGivenOrder'/> are respected. In contrast to <Ref Func='HeLP_KP'/> 
#!  this function also does not check whether the group is nilpotent to use the Weiss-result to have an 
#!  immediate positive solution for (KP). <P/>
#!  This function is interesting if one wants to save time or possesses some information, which was 
#!  not obtained using this package and was entered manually into <K>HeLP_sol</K>.  
#! @Arguments CharacterTable|Group
#! @Returns <K>true</K> if (KP) can be solved using the given data, <K>false</K> otherwise
DeclareGlobalFunction( "HeLP_AllOrdersKP" );





#! @Section Changing the used Character Table

#! @Description
#!  This function changes the used character table to the character table <A>CT</A> and keeps
#!  all the solutions calculated so far.  It is in this case the responsibility of the user that the 
#!  tables belong to the same group and the ordering of the conjugacy classes in <A>CT</A> is consistent with
#!  the one in the previously used table.  This function can be used to change from one table
#!  of the group to another, e.g. from a Brauer table to the ordinary table if the calculations will
#!  involve $p$-singular elements.
#!  (In case the involved character tables come from the ATLAS and their InfoText begins with "origin: ATLAS of finite groups",
#!  this is done automatically by the program.)
#!  A user may also use characters, which are normally not accessible in GAP.
#! @Arguments CT
#! @Returns nothing
DeclareGlobalFunction( "HeLP_ChangeCharKeepSols" );

#! @InsertChunk CCExample

#! @Description
#!  This function delets all the values calculated so far and resets the global variables <K>HeLP_CT</K> 
#!  and <K>HeLP_CT</K> to their initial value <K>[ [ [1] ] ]</K> and <K>CharacterTable(SmallGroup(1,1))</K>
#!  respectively.
#! @Arguments
#! @Returns nothing
DeclareGlobalFunction( "HeLP_Reset" );


#! @Section Influencing how the Systems of Inequalities are solved

#! HeLP uses currently three external programs (i.e. programs that are not part of the GAP-system):
#! zsolve from 4ti2 and/or normaliz to solve the systems of linear inequalities and redund from lrslib to simplify the  
#! inequlities before handing them over to the solver (HeLP can also be used without lrslib installed. In
#! general it is recommanded to have lrslib installed, if 4ti2 is used as the solver). The following functions can be used to influence
#! the behaviour of these external programms.


#! @Description
#! This function can be used to change the solver used for the HeLP-system between 4ti2 and normaliz. 
#! If the function is called without an argument it prints which solver is currently used.
#! If the argument it is called with is one of the stings "4ti2" or "normaliz", then the solver used
#! for future calculations is changed to the one given as argument in case this solver is found by the HeLP-package.
#! If both solvers are found when the package is loaded normaliz is taken as default.
#! @Arguments [string]
#! @Returns nothing
DeclareGlobalFunction("HeLP_Solver");

#! @Description
#!  This function determines whether HeLP uses 'redund' from the lrslib-package to remove redundant 
#!  equations from the HeLP system.  If <A>bool</A> is <K>true</K> 'redund' will be used in all calculation that follow,
#!  if it is <K>false</K>, 'redund' will not be used (which might take significantly longer).  
#!  If 'redund' was not found by GAP a warning will be 
#!  printed and the calculations will be performed without 'redund'.
#!  As default 'redund' will be used in all calculations, if 4ti2 is the chosen solver, and 'redund' will not be used, if normaliz is used.
#! @Arguments bool
#! @Returns nothing
DeclareGlobalFunction("HeLP_UseRedund");

#! @Description
#!  This function changes the maximum precision of the calculations of 4ti2 to solve the occurring systems of
#!  linear inequalities. The possible arguments are <K>"32"</K>, <K>"64"</K> and <K>"gmp"</K>.  After calling the function
#!  the new precision will be used until this function is used again. The default value is <K>"32"</K>. 
#!  A higher precision causes slower calculations. 
#!  But this function might be used to increase the precision of 4ti2, when one gets an error message like
#!  "Error, 4ti2 Error:
#!  Results were near maximum precision (32bit).
#!  Please restart with higher precision!"
#!  stating that the results were close to the maximum 4ti2-precision.
#!  normaliz does automatically change its precision, when it reaches an overflow.
#! @Arguments string
#! @Returns nothing
DeclareGlobalFunction("HeLP_Change4ti2Precision");

#! @InsertChunk PRExample

#! @Description
#! If normaliz is used as the solver of the HeLP-system this function influences, whether the "VerticesOfPolyhedron"
#! are computed by normaliz. By default these are only computed, if the system has a trivial solution.
#! The function takes "vertices", "novertices" and "default" as arguments. If you do not understand what this means, don't worry.
#! @Arguments string
#! @Returns nothing
DeclareGlobalFunction("HeLP_Vertices");


#! @Section Checking solutions, calculating and checking solutions

#! @Description
#!  This function checks which of the partial augmentations for elements of order <K>k</K> given
#!  in <K>HeLP_sol[k]</K> or the optional third argument <K>list_paraugs</K>
#!  fulfill the HeLP equations obtained from the characters in the first argument.
#!  This function does not solve any inequalities, but only checks, if the
#!  given partial augmentations fulfill them. It is for this reason often faster then
#!  e.g. <Ref Func='HeLP_WithGivenOrder'/>.<P/>
#!  If there is no third argument given, i.e. the augmentations from <K>HeLP_sol[k]</K> are used,
#!  the result overwrites <K>HeLP_sol[k]</K>.
#! @Arguments CharacterTable|ListOfClassFunctions k [list_paraugs]
#! @Returns List of admissible partial augmentations
DeclareGlobalFunction( "HeLP_VerifySolution" );

#! @InsertChunk CSExample


#! @Description
#!  This function provides the same functionality as <Ref Func='HeLP_WithGivenOrder'/> but
#!  instead of constructiong the corresponding system with all characters from the first argument
#!  <A>CharacterTable|ListOfClassFunctions</A> it does it consecutively with larger sets of characters
#!  from the argument until a finite list of solutions is found and then applies <Ref Func='HeLP_VerifySolution'/>
#!  to these solutions with the entirety of the class functions in the first argument.<P/>
#!  This function is sometimes faster than <Ref Func='HeLP_WithGivenOrder'/>, but the output is the same,
#!  thus the examples from <Ref Func='HeLP_WithGivenOrder'/> also apply here.
#! @Arguments CharacterTable|ListOfClassFunctions k
#! @Returns List of admissible partial augmentations or "infinite"
DeclareGlobalFunction( "HeLP_FindAndVerifySolution" );

#! @InsertChunk FCExample

#! @Description
#! This function provides the possible partial augmentations of the powers of units of a given order $n,$  
#! if the partial augmentations if units of order $n/p$ have been already computed for all primes $p$ 
#! dividing $n.$ The possibilities are sorted in the same way as, if the order $n$ is checked with any other 
#! function like e.g. <Ref Func='HeLP_WithGivenOrder'/> or <Ref Func='HeLP_ZC'/>. Thus, if the InfoLevel is 
#! high enough and one obtains that the computation of some possibility is taking too long, one can check it 
#! using <Ref Func='HeLP_WithGivenOrderAndPA'/>.  
#! @Arguments n
#! @Returns List of partial augmentations of powers.
DeclareGlobalFunction( "HeLP_PossiblePartialAugmentationsOfPowers" ); 

#! @InsertChunk PPExample

#! @Description
#! Given a character table <A>C</A> and an order <A>k</A>, the function calculates the partial augmentations 
#! of units of order $k$ that are rationally conjugate to group elements (note that they just coincide with the
#! partial augmentations of group elements) and stores them in <K>HeLP_sol[k]</K>. If solutions of order $k$ were
#! already calculated, they are overwritten by this function, so this function can be used in particular if elements
#! of order $k$ are known to be rationally conjugate to group elements by theoretical results.
#! @Arguments C k
#! @Returns Trivial solutions.
DeclareGlobalFunction( "HeLP_WriteTrivialSolution" ); 

# #! @InsertChunk TSExample

#! @Section The Wagner test

#! @Description
#!  This function applies the Wagner test (cf. Section <Ref Sect='Chapter_Background_Section_The_Wagner_test'/>) to the given data.
#!  If only the order <A>k</A> is given as argument,
#!  the Wagner test will be applied to the solutions stored in <K>HeLP_sol[k]</K>.
#!  If the arguments are the order <A>k</A>, a list of possible solutions <A>list_paraugs</A> and an ordinary character table 
#!  <A>OrdinaryCharacterTable</A> it applies the test to the solutions given in <A>list_paraugs</A> and using the number of conjugacy classes
#!  for elements a divisor of <A>k</A>, which will be extracted from the head of <A>OrdinaryCharacterTable</A>.
#! @Arguments k [list_paraugs, OrdinaryCharacterTable]
#! @Returns List of admissible partial augmentations
DeclareGlobalFunction( "HeLP_WagnerTest" );

#! @InsertChunk WTExample


#! @Section Action of the automorphism group

#! @Description
#!  For a list of possible partial augmentations, this function calculates representatives of each orbit of the action of the automorphism group of $G$
#!  on them.  The first two mandatory arguments are an ordinary character table <A>C</A> (with an underlying group) and the order <A>k</A> for which the partial augmentations
#!  should be filtered with respect to the action of the automorphism group of $G$.  If as third argument a list of partial augmentations is given,
#!  then these will be used, otherwise the partial augmentations that are stored in <K>HeLP_sol[k]</K> are used.
#! @Arguments C, k [, list_paraug]
#! @Returns List of admissible partial augmentations
DeclareGlobalFunction( "HeLP_AutomorphismOrbits" );

#! @InsertChunk AOExample


#! @Section Output 

#! @Description
#!  This function prints the possible solutions in a pretty way.  If a positive integer <A>k</A> as argument is given, then it prints the
#!  admissible partial augmentations of units of order <A>k</A>, if they are already calculated.  If no argument is given, the 
#!  function prints information on all orders for which there is already some information available.
#! @Arguments [k]
#! @Returns nothing
DeclareGlobalFunction( "HeLP_PrintSolution" );

#! @InsertChunk PSExample


#! @Section Eigenvalue multiplicities and character values 

#! @Description
#!  The returned list contains at the <M>l</M>-th spot the multiplicity of <K>E(k)^(l-1)</K> as eigenvalue of a unit <M>u</M>
#!  of order <A>k</A> under the representation corresponding to <A>chi</A> having the partial augmentations
#!  <A>paraugs</A> for the elements <M>u^d</M> for divisors <M>d</M> different from <A>k</A>.
#! @Arguments chi, k, paraugs
#! @Returns a list of multiplicities of eigenvalues
DeclareGlobalFunction( "HeLP_MultiplicitiesOfEigenvalues" );

#! @Description
#!  The function returns the character value <M>chi(u)</M> of an element <M>u</M> of order $k$ having the partial augmentations <A>paraug</A>.
#! @Arguments chi k paraug
#! @Returns the character value <M>chi(u)</M>
DeclareGlobalFunction( "HeLP_CharacterValue" );

#! @InsertChunk EMCVExample

#! @Section Check for triviality modulo normal subgroup

#! @Description 
#!  This function checks, if the image of a unit in $\mathrm{V}(\mathbb{Z}G)$ given by the partial augmentations of itself (not its powers)
#!  is trivial modulo a normal subgroup $N$, i.e.
#!  if it maps to the identity under the natural homomorphism $\mathbb{Z}G \rightarrow \mathbb{Z}(G/N)$.
#!  The input is a character table, the order of the unit, its partial augmentations, the group and the normal subgroup.
#! @Arguments UCT, k, pa, G, N  
#! @Returns <K>true</K> or <K>false</K>
DeclareGlobalFunction( "HeLP_IsOneModuloN" );
#! @InsertChunk IOMNExample

#! @Description

#! @Arguments C
#! @Returns Same character table as <K>C</K> but without underlying group
DeclareGlobalFunction( "HeLP_ForgetUnderlyingGroup" );
#! @InsertChunk FUGExample

#! @Section Check Kimmerle Problem for single units

#! @Description
#!  Decides if a unit described by the partial augmentations of its powers satisfies the Kimmerle Problem. Input is Ordinary character table, order of the unit
#!  and the partial augmentations.
#! @Arguments UCT, k, pa
#! @Returns <K>true</K> or <K>false</K>
DeclareGlobalFunction( "HeLP_UnitSatisfiesKP" );
#! @InsertChunk USKPExample

#! @Section Check whether Zassenhaus Conjecture is known from theoretical results

#! @Description
#!  For the given group <A>G</A> this function applies five checks, namely it checks
#!   * if $G$ is nilpotent
#!   * if $G$ has a normal Sylow subgroup with abelian complement,
#!   * if $G$ is cyclic-by-abelian
#!   * if it is of the form $X \rtimes A$, where $X$ and $A$ are abelian and $A$ is of prime order $p$ such that $p$ is smaller then any prime divisor of the order of $X$
#!   * or if the order of $G$ is smaller than $144$.
#!  <P/>
#!  
#!  In all these cases the Zassenhaus Conjecture is known.  See <Ref Sect='Chapter_Background_Section_Known_results_about_the_Zassenhaus_Conjecture_and_the_Prime_Graph_Question'/> for references.
#!   This function is designed for solvable groups.
#! @Arguments G
#! @Returns <K>true</K> if (ZC) can be derived from theoretical results, <K>false</K> otherwise
DeclareGlobalFunction( "HeLP_IsZCKnown" );



# The following functions are the only internal one, which are
# defined via DeclareGlobalFunction / InstallGlobalFunction 
# as it seems otherwise not possible define it recursively
DeclareGlobalFunction( "HeLP_INTERNAL_WithGivenOrder" );
DeclareGlobalFunction( "HeLP_INTERNAL_WithGivenOrderAllTables" );
DeclareGlobalFunction( "HeLP_v4_INTERNAL_WithGivenOrder");


#E
