## 4.2 (unreleased)

* The lrs package had major changes in version one, so that redund was producing errors when newer version of lrs were used. This is now fixed by changing the correponding lines in HeLP_UseRedund

## 4.1 (2026-01-09)

* When NormalizInterface or 4ti2 is not available, the functions which use them are now not loaded. This is achieved by restucturing the files in /lib and using Extensions in PackageInfo.g. In particular, solving a system is now divided into two functions, one for normaliz and one for 4ti2, each of which has its own file which is only loaded if the corresponding interface package can be loaded by GAP. Accordingly adjustements have been made to the needed package versions, the GAP versiona and in the manual.
* To resolve issues of failing tests and loading errors IO and CTblLib are now required
* Test files adjusted to new structure and extended. Also by requiring new version of CTblLib an error which came up during testing is resolved.
* Corrected mistake in HeLP_WithGivenOrderAllTables which appeared in version 4.0
* Changes by the GAP admins about GitHub packages, release options etc

## 4.0 (2024-02-29)

* Two new global functions: HeLP_SP and HeLP_KP to study the Spectrum Problem and the Kimmerle Problem and their companion functions HeLP_AllOrdersSP and HeLP_AllOrdersKP
* More theoretical results are taken into account when the system to solve is generated, namely more partial augmentations are assumed to be 0 a priori. This includes three results: Berman-Higman (in the general form that the coefficient at a central element is 0, if the unit is not that element), Hertweck's Proposition 2 from [Hertweck08] (if a unit u has smaller order modulo a normal p-subgroup, elements with non-zero partial augmentations must have same p-power order as u) and Hertweck's p-adic criterion (units which map to 1 modulo a normal p-subgroup are p-adically conjugate to elements in G and hence the p-part of any element with non-zero partial augmentation is unique up to conjugacy in G)
* Three new minor global functions: HeLP_UnitSatisfiesKP to check manually for KP and given partial augmentaion; HeLP_IsOneModuloN to see if a unit is mapped to the identity modulo a normal subgroup N; HeLP_ForgetUnderlyingGroup to obtain same table without group structure. This way the new theoretical restictions do not apply and one can obtain the results as in older versions

Minor chnages:
* Noted that mistankenly the manual was shortened in version 3.3, reversed
* Removed formal dependencies on other packages such as solvers
* HeLP_Solver changed to allow more flexibility for the user. 4ti2 or Normaliz is not longer necessary to set the option

## 3.4 (2018-11-20)

* Package migrated to GitHub

## 3.3 (2017-12-11)

* testfile in PackageInfo.g changed to "tst/testall.g"

## 3.2 (2017-11-20)

* Solely line 542 in "expamples/examples.g" corrected

## 3.1 (2017-01-12)

* New function HeLP_AutomorphismOrbits

* New function HeLP_IsZCKnown

* New functions HeLP_WithGivenOrderAllTables and HeLP_WithGivenOrderAndPAAllTables

* New functions HeLP_WriteTrivialSolution

## 3.0 (2016-03-03)

* Normaliz is included as a possible solver.

* Functions included to switch between the solvers. For normaliz a function is included, to influence whether normaliz computes vertices of polyhedra.

## 2.2 (2015-12-23)

* Package can be loaded even if zsolve was not found.  An error is indicated explaining the situation when a function is called that needs zsolve but zsolve was not found when loading the package.

* The functions HeLP_WithGivenOrderSConstant and HeLP_VerifySolution give more output in InfoLevel 1 or higher on the number of solutions

## 2.1 (2015-09-30)

* HeLP_WithGivenOrder always returns a value (in the case there are infinitely many solutions, the function returns "infinite", in case the function can't be applied -- the characteristic of the Brauer table divides the order or s-constant characters can't be used -- the function returns "non-admissible")

* Functionality of the Wagner-Test essentialy improved.

* New function "HeLP_WithGivenOrderAndPAAndSpecificSystem"

## 2.0 (2015-07-29)

* The package now provides an interface to redund from the lrslib software to remove redundant inequalities before solving the system obtained by the HeLP-method.

* There is a new global variable HeLP_settings determining whether 'redund' will be used or not to get rid of redundant equations before using 4ti2.  The variable has true as default value, whenever the executable redund is found by GAP, false otherwise (so that the package can still be used without lrslib being installed).  It also stores the precision with which 4ti2 will be called.  Here, the default value is "32".  The value of HeLP_settings can be influenced by the new functions HeLP_UseRedund and HeLP_Change4ti2Precision.

* HeLP_ZC and HeLP_PQ do not use solutions, which were previously calculated for the group or character table in question, but start all calculations from scratch.

* Bug in HeLP_VerifySolution and related functions fixed (when checking for elements of order k the global factor 1/k was missing)

* New function 'HeLP_PossiblePartialAugmentationsOfPowers' to calculate the possibel partial augmentations for the powers



TODO:
* (s,t)-constant characters.
