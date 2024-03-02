#! @BeginChunk PRExample
#!  Sometimes it is desirable to perform calculations without redund (even if it is 
#!  installed and in many cases improves the performance of the package) or with a higher
#!  precision.
#!  For example, determining the partial augmentations for units of order $14$ for
#!  <K>SmallGroup(392, 30)</K> involves very long calculations (when called with redund and precision 32)
#!  or cause errors (when called without redund and precision 32).  However, the following works in a 
#!  reasonable time.
#! @BeginExample
C := CharacterTable(SmallGroup(392,30));
#! CharacterTable( <pc group of size 392 with 5 generators> )
HeLP_Solver("4ti2");
#! '4ti2' will be used from now on.
HeLP_UseRedund(false);
#! The calculations will be performed without using 'redund' from now on.
HeLP_ZC(C);
#! Error, 4ti2 Error:
#! Results were near maximum precision (32bit).
#! Please restart with higher precision!
#! If you continue, your results might be wrong called from
#! 4ti2Interface_zsolve_equalities_and_inequalities( 
#!  [ ListWithIdenticalEntries( Size( T[1] ), 1 ) ], [ 1 ], temp[1], - temp[2] 
#!  ) called from
#! HeLP_TestSystemINTERNAL( W[1], W[2], k, arg[3] ) called from
#! HeLP_WithGivenOrderAndPAINTERNAL( C, k, pa ) called from
#! HeLP_WithGivenOrderINTERNAL( Irr( T ), k ) called from
#! <function "HeLP_ZC">( <arguments> )
#!  called from read-eval loop at line 19 of *stdin*
#! you can 'quit;' to quit to outer loop, or
#! you can 'return;' to continue
brk> quit;
#! #I  Options stack has been reset
HeLP_Change4ti2Precision("64");
#! The calculations of 4ti2 will be performed with precision 64 from now on.
HeLP_ZC(C);
#! true
#! @EndExample
#! The reproducibility of the above example depends on the versions of the progrmas involved and probably also your machine.
#! @EndChunk

HeLP_UseRedund(true);
HeLP_Change4ti2Precision("32");

