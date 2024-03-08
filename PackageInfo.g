#############################################################################
##
##                                                               HeLP package
##
##                                 Andreas Bächle, Vrije Universiteit Brussel
##                               Leo Margolis, Universidad Autonoma de Madrid
##
#############################################################################

SetPackageInfo( rec(

PackageName    := "HeLP",
Subtitle       := Concatenation( [
                  "Hertweck-Luthar-Passi method." ] ),
Version        := "4.0",
Date           := "29/02/2024", # dd/mm/yyyy format
License        := "GPL-2.0-or-later",

SourceRepository := rec(
    Type := "git",
    URL := Concatenation( "https://github.com/gap-packages/", ~.PackageName ),
),
IssueTrackerURL := Concatenation( ~.SourceRepository.URL, "/issues" ),
PackageWWWHome  := Concatenation( "https://gap-packages.github.io/", ~.PackageName ),
README_URL      := Concatenation( ~.PackageWWWHome, "/README" ),
PackageInfoURL  := Concatenation( ~.PackageWWWHome, "/PackageInfo.g" ),
ArchiveURL      := Concatenation( ~.SourceRepository.URL,
                                 "/releases/download/v", ~.Version,
                                 "/", ~.PackageName, "-", ~.Version ),
                                 
ArchiveFormats := ".tar.gz",

PackageDoc := rec(
  BookName  := ~.PackageName,
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0_mj.html",
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
       Email         := "ABachle@gmx.net",
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
       Email         := "Leo.Margolis@uam.es",
       WWWHome       := "https://margollo.github.io",
       PostalAddress := Concatenation( [
                        "Universidad Autonoma de Madrid\n",
                        "Campus Cantoblanco\n",
                        "Facultad de Ciencias\n", 
                        "Departamento de Matematicas\n", 
                        "28049 Madrid\n",
                        "Spain" ] ),
       Place         := "Madrid",
       Institution   := "Universidad Autonoma de Madrid"
     )
],

Status := "deposited",

AbstractHTML := "<span class=\"pkgname\">HeLP</span> is a package to compute constraints on partial augmentations of torsion units in integral group rings using a method developed by Luthar, Passi and Hertweck.  The package can be employed to verify the Zassenhaus Conjecture, the Prime Graph Question, the Spectrum Problem and the Kimmerle Problem for finite groups, once characters are known. It uses an interface to the software package 4ti2 to solve integral linear inequalities or alternatively Normaliz.",
                  
Dependencies := rec(
  GAP                    := ">=4.8.2",
  NeededOtherPackages    := [
    ["io", ">=4.2" ],
    ["CTblLib", ">= 1.2.2"],
  ],
  SuggestedOtherPackages := [
    ["4ti2Interface", ">= 2015.04.29"],
    ["NormalizInterface", ">= 0.9.6"],
  ],
  ExternalConditions     := [[ "zsolve", "https://4ti2.github.io" ] ]
),

TestFile := "tst/testall.g",

AvailabilityTest := function()
    return true;
  end,

Keywords := ["HeLP method", "torsion units", 
             "rational conjugacy", "Zassenhaus Conjecture", "Prime Graph Question", "Spectrum Problem", "Kimmerle Problem"],

AutoDoc := rec(
    TitlePage := rec(
        Copyright := Concatenation(
                    "&copyright; 2017 by Andreas Bächle and Leo Margolis<P/>\n\n",
                    "This package is free software and may be distributed under the terms and conditions of the\n",
                    "GNU Public License Version 2.\n"
                ),
        Acknowledgements := Concatenation(
                    "The authors are grateful to Sebastian Gutsche, Christof Söger and Max Horn for endowing GAP\n",
                    "with a 4ti2-Interface and a normaliz-Interface.\n",
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

