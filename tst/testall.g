TestMyPackage := function( pkgname )
local pkgdir, testfiles, testresult, ff, fn;
LoadPackage( pkgname );
pkgdir := DirectoriesPackageLibrary( pkgname, "tst" );

testresult := true;
if TestPackageAvailability("NormalizInterface") = true then
  Print("normaliz found\n");
  if exec := IO_FindExecutable( "lrs" ) <> fail then
    Print("Also found lrs, HeLP_UseRedund will be tested also\n");
    fn := Filename( pkgdir, "yes_normaliz_lrs.tst" );
    Print("#I  Testing ", fn, "\n");
    if not Test( fn, rec(compareFunction := "uptowhitespace") ) then
      testresult:=false;
    fi;
  else
    fn := Filename( pkgdir, "yes_normaliz.tst" );
    Print("#I  Testing ", fn, "\n");
    if not Test( fn, rec(compareFunction := "uptowhitespace") ) then
      testresult:=false;
    fi;
  fi;     
fi;

if TestPackageAvailability("4ti2Interface") = true and Filename(DirectoriesSystemPrograms(), "zsolve") <> fail then
  Print("4ti2 found\n");
  if exec := IO_FindExecutable( "lrs" ) <> fail then
    Print("Also found lrs, HeLP_UseRedund will be tested also\n");
    fn := Filename( pkgdir, "yes_4ti2_lrs.tst" );
    Print("#I  Testing ", fn, "\n");
    if not Test( fn, rec(compareFunction := "uptowhitespace") ) then
      testresult:=false;
    fi;
  else
    fn := Filename( pkgdir, "yes_4ti2.tst" );
    Print("#I  Testing ", fn, "\n");
    if not Test( fn, rec(compareFunction := "uptowhitespace") ) then
      testresult := false;
    fi;
  fi;
fi;

# in any case test functions not requiring any solver 
fn := Filename( pkgdir, "no_solver.tst" );
Print("Testing functions whihc do not use solvers\n");
if not Test( fn, rec(compareFunction := "uptowhitespace") ) then
  testresult := false;
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

