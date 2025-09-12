#############################################################################
##
##                                                               HeLP package
##
##                                 Andreas Bächle, Vrije Universiteit Brussel
##                                        Leo Margolis, Universität Stuttgart
##
#############################################################################

ReadPackage( "HeLP", "lib/HeLP_internal_functions_no_solving.gi" );
ReadPackage( "HeLP", "lib/HeLP_no_solving.gi" );

if not TestPackageAvailability("4ti2Interface") = true and not TestPackageAvailability("NormalizInterface") = true then
  Print("WARNING: Neither 4ti2Interface nor NormalizInterface could be loaded. No functions which need to solve a system of inquealities will work. Please install one of the solvers in case you want to use these.\n");
elif not TestPackageAvailability("4ti2Interface") = true and TestPackageAvailability("NormalizInterface") = true then
  Print("Only NormalizInterface could be loaded, so normaliz will be used as a solver. \n");
  ReadPackage( "HeLP", "lib/HeLP_internal_functions_solving_normaliz.gi");
  ReadPackage( "HeLP", "lib/HeLP_solving.gi");
elif TestPackageAvailability("4ti2Interface") = true and not TestPackageAvailability("NormalizInterface") = true then
  Print("Only 4ti2Interface could be loaded, so 4ti2 will be used as a solver. \n");
  ReadPackage( "HeLP", "lib/HeLP_internal_functions_solving_4ti2.gi");
  ReadPackage( "HeLP", "lib/HeLP_solving.gi");
elif TestPackageAvailability("4ti2Interface") = true and TestPackageAvailability("NormalizInterface") = true then
  ReadPackage( "HeLP", "lib/HeLP_internal_functions_solving_two_solvers.gi");
  ReadPackage( "HeLP", "lib/HeLP_solving.gi");
fi;

