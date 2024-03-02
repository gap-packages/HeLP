# run GAP from the folder where PackageInfo.g is contained
LoadPackage("AutoDoc");
AutoDoc(rec(autodoc := true, scaffold:=true));
QUIT;
