#############################################################################
##
##                                                               HeLP package
##
##                                 Andreas Bächle, Vrije Universiteit Brussel
##                                        Leo Margolis, Universidad de Murcia
##
#############################################################################

SetPackageInfo( rec(

PackageName    := "HeLP",
Subtitle       := Concatenation( [
                  "Hertweck-Luthar-Passi method." ] ),
Version        := "3.1",
Date           := "12/01/2017",

ArchiveURL :=  "http://homepages.vub.ac.be/abachle/help/help",

ArchiveFormats := ".tar.gz",


PackageWWWHome := "http://homepages.vub.ac.be/abachle/help/",

PackageDoc := rec(
  BookName  := ~.PackageName,
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := ~.Subtitle,
  Autoload  := false
),

Persons :=
 [
     rec(
       LastName      := "Bächle",
       FirstNames    := "Andreas",
       IsAuthor      := true,
       IsMaintainer  := true,
       Email         := "ABachle@vub.ac.be",
       WWWHome       := "http://homepages.vub.ac.be/~abachle/",
       PostalAddress := Concatenation( [
                        "Vrije Universiteit Brussel\n",
                        "Vakgroep Wiskunde\n",
                        "Pleinlaan 2\n", 
                        "1050 Brussels\n",
                        "Belgium" ] ),
       Place         := "Brussels",
       Institution   := "Vrije Universiteit Brussel"
     ),
rec(
       LastName      := "Margolis",
       FirstNames    := "Leo",
       IsAuthor      := true,
       IsMaintainer  := true,
       Email         := "Leo.Margolis@mathematik.uni-stuttgart.de",
       WWWHome       := "http://www.igt.uni-stuttgart.de/LstDiffgeo/Margolis/",
       PostalAddress := Concatenation( [
                        "Departamento de Matemáticas\n",
                        "Facultad de Matemáticas\n",
                        "Universidad de Murcia\n",
                        "Murcia 30100\n",
                        "Spain" ] ),
       Place         := "Murcia",
       Institution   := "Universidad de Murcia"
     )
],

Status := "deposited",

README_URL := 
  Concatenation( ~.PackageWWWHome, "README" ),
PackageInfoURL := 
  Concatenation( ~.PackageWWWHome, "PackageInfo.g" ),
  
AbstractHTML := "<span class=\"pkgname\">HeLP</span> is a package to compute constraints on partial augmentations of torsion units in integral group rings using a method developed by Luthar, Passi and Hertweck.  The package can be employed to verify the Zassenhaus Conjecture and the Prime Graph Question for finite groups, once characters are known. It uses an interface to the software package 4ti2 to solve integral linear inequalities.",
                  
Dependencies := rec(
  GAP                    := ">=4.8.2",
  NeededOtherPackages    := [ ["io", ">=4.2" ], ["4ti2Interface", ">= 2015.04.29"], ["CTblLib", ">= 1.2.2"], ["NormalizInterface", ">= 0.9.6"] ],
  SuggestedOtherPackages := [ ],
  ExternalConditions     := [[ "zsolve", "http://www.4ti2.de" ] ]
),

TestFile := "examples/examples.g",

AvailabilityTest := function()
    return true;
  end,

Keywords := ["HeLP method", "torsion units", 
             "rational conjugacy", "Zassenhaus Conjecture", "Prime Graph Question"],

AutoDoc := rec(
    TitlePage := rec(
        Copyright := Concatenation(
                    "&copyright; 2017 by Andreas Bächle and Leo Margolis<P/>\n\n",
                    "This package is free software and may be distributed under the terms and conditions of the\n",
                    "GNU Public License Version 2.\n"
                ),
        Acknowledgements := Concatenation(
                    "The authors are grateful to Sebastian Gutsche, Christof Söger and Max Horn for endowing GAP\n",
                    "with a 4ti2-Interface and a normlaiz-Interface.\n",
                    "We also would like to thank Gutsche and Söger for many very helpful discussions.\n",
                    "We also want to give credits to the developers of the softwares 4ti2 and normaliz.\n",
                    "Thanks go to David Avis for writing lrslib and answering our questions about it.\n",
                    "We moreover thank Wolfgang Kimmerle for introducing us to the beautiful world of group rings.\n",
                    "The development of this package was partially supported by the Research Foundation Flanders \n",
                    "(FWO - Vlaanderen) and the DFG priority program SPP 1489 Algorithmic and Experimental Methods in Algebra, Geometry, and Number Theory.\n"
                ),
    )
),

));

