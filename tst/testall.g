TestMyPackage := function( pkgname )
local pkgdir, testfiles, testresult, ff, fn;
LoadPackage( pkgname );
pkgdir := DirectoriesPackageLibrary( pkgname, "tst" );

testresult := true;
if LoadPackage("NormalizInterface") <> fail then
  Print("normaliz found\n");
  fn := Filename( pkgdir, "yes_normaliz.tst" );
  Print("#I  Testing ", fn, "\n");
  if not Test( fn, rec(compareFunction := "uptowhitespace") ) then
    testresult:=false;
  fi;
fi;

if Filename(DirectoriesSystemPrograms(), "zsolve") <> fail then
  Print("4ti2 found\n");
  fn := Filename( pkgdir, "yes_4ti2.tst" );
  Print("#I  Testing ", fn, "\n");
  if not Test( fn, rec(compareFunction := "uptowhitespace") ) then
    testresult := false;
  fi;
fi;

if Filename(DirectoriesSystemPrograms(), "zsolve") = fail and LoadPackage("NormalizInterface") = fail then
  fn := Filename( pkgdir, "no_solver.tst" );
  Print("No solver found\n");
  if not Test( fn, rec(compareFunction := "uptowhitespace") ) then
    testresult := false;
  fi;
fi;

if testresult then
  Print("#I  No errors detected while testing package ", pkgname, "\n");
  QUIT_GAP(0);
else
  Print("#I  Errors detected while testing package ", pkgname, "\n");
  QUIT_GAP(1);
fi;
end;

# Set the name of the package here
TestMyPackage( "help" );

FORCE_QUIT_GAP(1); # if we ever get here, there was an error

