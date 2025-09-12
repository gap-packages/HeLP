##
##                                                               HeLP package
##
##                                 Andreas Bächle, Vrije Universiteit Brussel
##                                        Leo Margolis, Universität Stuttgart
##
#############################################################################

## User oriented functions that do not rely on any solver, i.e. to not use 4ti2 or normaliz and are loaded in any case

########################################################################################################

InstallGlobalFunction(HeLP_AddGaloisCharacterSums, function(C)
## HeLP_AddGaloisCharacterSums(C)
##  C a character table of a group
##  returns a list of sums of Galois conjugate characters
local gm, galoisfams, i;
if not IsOrdinaryTable(C) then
  Error("The argument of HeLP_AddGaloisCharacterSums has to be an ordinary character table.");
fi;
gm := GaloisMat(Irr(C)).galoisfams;
galoisfams := [ ];
for i in [1..Size(gm)] do
  if gm[i] = 1 then
    Add(galoisfams, [i]);
  elif IsList(gm[i]) then
    Add(galoisfams, gm[i][1]);
  fi;
od;
return DuplicateFreeList(Concatenation(List(galoisfams, x -> Sum(Irr(C){x})), Irr(C)));
end);

########################################################################################################

InstallGlobalFunction(HeLP_ChangeCharKeepSols, function(CT)
# Arguments: a character table
# Output: nothing
# Changes the user character table to the one given as argument without doing any checks
Info(HeLP_Info, 5, "WARNING: Change used character table without checking if the character tables have the same underlying groups and the ordering of the conjugacy classes are the same!");
MakeReadWriteGlobal("HeLP_CT");  
UnbindGlobal("HeLP_CT");
BindGlobal("HeLP_CT", CT);
# HeLP_CT := CT;
end);

##############################################################################################################

InstallGlobalFunction(HeLP_Reset, function()
# Arguments: none
# output: none
# Delets all values calculated so far and rests the varaibles to the inital value
MakeReadWriteGlobal("HeLP_CT");  
UnbindGlobal("HeLP_CT");
BindGlobal("HeLP_CT", CharacterTable(SmallGroup(1,1)));
HeLP_sol := [[[[1]]]];     
end);

###########################################################################################################
InstallGlobalFunction(HeLP_PossiblePartialAugmentationsOfPowers, function(k)
local primediv, npa, p;
if not IsPosInt(k) then
  Error("The argument of HeLP_PossiblePartialAugmentationsOfPowers must be a positive integer.");
fi;
primediv := PrimeDivisors(k);	
npa := [];
for p in primediv do
  if not IsBound(HeLP_sol[k/p]) then
    Info( HeLP_Info, 1, "No partial augmentations for elements of order ", k/p, " known yet. Please compute them first.");
    return fail;
  fi;
  Add(npa, HeLP_sol[k/p]); 
od;
npa := Cartesian(npa);
npa := List(npa, x -> HeLP_INTERNAL_CompatiblePartialAugmentations(x,k));
npa := Filtered(npa, x -> not x = fail);
return npa;
end);

###########################################################################################################
InstallGlobalFunction(HeLP_WriteTrivialSolution, function(C, k)
## HeLP_WriteTrivialSolution(C, k)
## C an ordinary character table, k a positive integer
## calculates the partial augmentations of units of order k that are rationally conjugate to group elements and stores this in HeLP_sol[k]
local nontrivialdivisors, o, posconjclasses, d, solution, x, l, a, s, t, all_solutions;
if not IsOrdinaryTable(C) then
  Error("The first argument of HeLP_WriteTrivialSolution has to be a character table.");
fi;
if not IsPosInt(k) then
  Error("The second argument of HeLP_WriteTrivialSolution has to be a positive integer.");
fi;
o := OrdersClassRepresentatives(C);
if not k in o then
  HeLP_sol[k] := [ ];
  return HeLP_sol[k];
fi;
nontrivialdivisors := HeLP_INTERNAL_DivNotOne(k);
posconjclasses := [];
for d in nontrivialdivisors do
  posconjclasses[d] := Positions(o, d);
od;
all_solutions := [ ];
for x in posconjclasses[k] do
  solution := [ ];
  # a := [ ];
  for l in nontrivialdivisors do
    # create list with right length and 0 entries
    a := ListWithIdenticalEntries( Sum( List( HeLP_INTERNAL_DivNotOne(l), e -> Size(posconjclasses[e]) ) ), 0);
    # number of conjugacy classes of strictly smaller order
    s := Sum( List( HeLP_INTERNAL_ProperDiv(l), e -> Size(posconjclasses[e]) ) );
    # Position of the conjugacy class of u^(k/l) in the conjugacy classes of order l
    t := Position(posconjclasses[l], PowerMap(C, k/l)[x]);
    a[s + t] := 1;
    Add(solution, a);
  od;
  Add(all_solutions, solution);
od;
HeLP_sol[k] := all_solutions;
return HeLP_sol[k];
end);


##########################################################################################################

InstallGlobalFunction(HeLP_MultiplicitiesOfEigenvalues, function(chi, k, paraugs)
## HeLP_MultiplicitiesOfEigenvalues(chi, k, paraugs)
## chi a character, k the order of the unit u in question, paraugs a list of partial augmentations of u^d (d|k) in ascending order of the elements starting with the partial augmentation of u^k
## returns a list with the mutliplicities of the eigenvalues E(k)^l, l=0, 1, ..., k-1 starting with 
local pdivisors, d, e, T, a, o, posconk, poscondiv, poscondivd, mu, pas;
pdivisors := Filtered(DivisorsInt(k), n -> not (n=k));
o := OrdersClassRepresentatives(UnderlyingCharacterTable(chi));
poscondiv := [ ];
posconk := [ ];
if k = 1 then
  pas := paraugs;
else
  pas := Concatenation([[1]], paraugs);   # add partial augmentation of u^k = 1
fi;
for d in pdivisors do
    # determine on which conjugacy classes the unit and its powers might have non-trivial partial augmentations
    if d = 1 then
       Add(poscondiv, Positions(o, 1));
    else
      poscondivd := [ ];
      for e in Filtered(DivisorsInt(d), f -> not (f = 1)) do
         Append(poscondivd, Positions(o, e));
      od;
      Add(poscondiv, poscondivd);
      Append(posconk, Positions(o, d));
    fi;
od;
Append(posconk, Positions(o, k));
T := 1/k*HeLP_INTERNAL_MakeCoefficientMatrixChar(chi, k, posconk);
a := 1/k*HeLP_INTERNAL_MakeRightSideChar(chi, k, pdivisors, poscondiv, pas{[1..Size(pas)-1]});
mu := T*pas[Size(pas)] + a;
return T*pas[Size(pas)] + a;
end);

##############################################################################################################

InstallGlobalFunction(HeLP_CharacterValue, function(chi, k, paraug)
## HeLP_CharacterValue(chi, k, paraug)
## chi a character, k the order of the unit u in question, paraug a list of the partial augmentations of u
## returns a list with the mutliplicities of the eigenvalues E(k)^l, l=0, 1, ..., k-1 starting with 
local pdivisors, d, o, posconk;
o := OrdersClassRepresentatives(UnderlyingCharacterTable(chi));
if k = 1 then
  posconk := Positions(o, 1);
else
  pdivisors := Filtered(DivisorsInt(k), n -> not (n=1));
  posconk := [ ];
  for d in pdivisors do
    # determine on which conjugacy classes the unit and its powers might have non-trivial partial augmentations
      Append(posconk, Positions(o, d));
  od;
fi;
return chi{posconk}*paraug;
end);


##############################################################################################################

InstallGlobalFunction(HeLP_WagnerTest, function(arg)
## Arguments: order of unit [list of possible partial augmentations for units of this order, ordinary character table]
## Output: list of possible partial augmentations for units of this order after applying the Wagner test
local k, list_paraugs;
if Size(arg) = 1 and IsPosInt(arg[1]) then
  # one argument (the order of the units)
  k := arg[1];
  if IsBound(HeLP_sol[k]) then
    return Filtered(HeLP_sol[k], x -> HeLP_INTERNAL_WagnerTest(HeLP_CT, k, x));
  else 
    Error("The solutions for elements of order ", k, " are not yet calculated.");
  fi;
elif Size(arg) = 3 and (IsPosInt(arg[1]) and IsList(arg[2]) and IsOrdinaryTable(arg[3])) then
  # three arguments (the order of the units, the pa's to check and the ordinary character table -- only its head is used)
  k := arg[1];  
  list_paraugs := arg[2];
  return Filtered(list_paraugs, x -> HeLP_INTERNAL_WagnerTest(arg[3], k, x));
else
  Error("The arguments of HeLP_WagnerTest have to be either the order of the units in question or the order, the solutions to test and a character table.   ");
fi;
end);


##############################################################################################################

InstallGlobalFunction(HeLP_VerifySolution, function(arg)
# Arguemnts: character table or list of class functions, an order k [list of partial augmentations]
# returns a list of admissable pa's or nothing (if there can not be a unit of that order for theoretical reasons or the method can not be applied)
# checks which of the pa's in HeLP_sol[k] (if there are 2 arguments given) or the pa's in the third  argument fulfill the HeLP-constraints
# from the class functions in the first argument
local C, k, list_paraugs, chars, W, asol, UCT, mu, pa, NumArg;
C := arg[1];
k := arg[2];
NumArg := 3;
if Size(arg) = 3 then
  list_paraugs := arg[3];
elif Size(arg) = 2 and IsBound(HeLP_sol[k]) then
  list_paraugs := HeLP_sol[k];
  NumArg := 2;
else
  Error("HeLP_sol[", k,"] is not bound and there is no third argument given.");  
fi;
if IsCharacterTable(C) then
  chars := Irr(C);
elif IsList(C) then
  if C = [] then return "infinite"; fi;
  chars := C;
else
  Error("The first argument of HeLP_VerifySolution has to be a character table or a list of class functions.");
fi;
UCT := UnderlyingCharacterTable(chars[1]);
if IsBrauerTable(UCT) and Gcd(k, UnderlyingCharacteristic(UCT)) > 1 then
  Info( HeLP_Info, 1, "HeLP can't be applied in this case as the characteristic of the Brauer table divides the order of the unit in question.");
  return "non-admissible";
fi;
if not Lcm(OrdersClassRepresentatives(UCT)) mod k = 0 then
  Info( HeLP_Info, 1, "There is no unit of order ", k, " in ZG as it does not divide the exponent of the group G.");
  return [];
fi;
asol := [];    # stores the solutions which fulfill the conditions of the HeLP equations
for pa in list_paraugs do
  W := HeLP_INTERNAL_MakeSystem(chars, k, UCT, pa{[1..Size(pa)-1]});
  if W = "infinite" then
    return "infinite";
  fi;
  W := HeLP_INTERNAL_DuplicateFreeSystem(W[1], W[2]);
  mu := 1/k*(W[1]*pa[Size(pa)] + W[2]);
  if HeLP_INTERNAL_IsIntVect(mu) and not false in List(mu, x -> SignInt(x) > -1) then
    Add(asol, pa);
  fi;
od;
if NumArg = 2 then
  HeLP_sol[k] := asol;
  Info( HeLP_Info, 1, "Number of solutions for elements of order ", k, ": ", Size(asol), "; stored in HeLP_sol[", k, "].");
else
  Info( HeLP_Info, 1, "Number of solutions for elements of order ", k, ": ", Size(asol), ".");
fi;
return asol;
end);

##############################################################################################################
InstallGlobalFunction(HeLP_PrintSolution, function(arg)
# Arguments: empty or order k for which the pa's should be printed
# return: nothing
# prints the solutions in a 'prety' way
local k, posdiv, w1, w2, w3, d, orders_calculated;
if arg = [] then
  # if there are no arguments the function prints all the solutions which were calculated so far
  orders_calculated := Filtered([1..Length(HeLP_sol)], y -> IsBound(HeLP_sol[y]) and not y = 1);
  for k in orders_calculated do
    HeLP_PrintSolution(k);
  od;
elif IsInt(arg[1]) then
  # prints the pa's for elements of order k
  k := arg[1];
  if IsBound(HeLP_sol[k]) then
    if HeLP_sol[k] = [] then
      Info( HeLP_Info, 1, "There are no admissible partial augmentations for elements of order ", k, "." );
    else
      w1 := [ ];
      w2 := [ ];
      w3 := [ ];
      if k = 1 then
        Add(w1, k);
        Add(w2, ClassNames(HeLP_CT){Positions(OrdersClassRepresentatives(HeLP_CT), 1)});
        Add(w3, "---");
        PrintArray(Concatenation([w1], [w2], [w3], HeLP_sol[k]));       
      else
        posdiv := Filtered(DivisorsInt(k), e -> not e = 1);
        for d in posdiv do
          if not d = k then 
            Add(w1, Concatenation("u^", String(k/d)));
          else
            Add(w1, "u");
          fi;
          Add(w2, Concatenation(List(Filtered(DivisorsInt(d), e -> e <> 1),
                  f -> ClassNames(HeLP_CT){Positions(OrdersClassRepresentatives(HeLP_CT), f)})));
          Add(w3, "---");
        od;
        Info( HeLP_Info, 1, "Solutions for elements of order ", k, ":");
        PrintArray(Concatenation([w1], [w2], [w3], HeLP_sol[k]));
      fi;     
    fi;
  else
    Info( HeLP_Info, 1, "Solutions for order ", k, " are not yet calculated.");      
  fi;
fi;
end);


##############################################################################################################
InstallGlobalFunction(HeLP_Solver, function(arg)
# Arguments: none or a string
# return: nothing
# changes the value of HeLP_settings[1] to the value of the argument of the function
local h1, h2, h3, h4;
if arg = [] then
  Info( HeLP_Info, 1, HeLP_settings[1]);
elif Size(arg) = 1 and arg[1] in ["4ti2", "normaliz"] then
  h1 := HeLP_settings[1];
  h2 := HeLP_settings[2];
  h3 := HeLP_settings[3];
  h4 := HeLP_settings[4];
  MakeReadWriteGlobal("HeLP_settings");  
  UnbindGlobal("HeLP_settings");
  if arg[1] = "4ti2" then
    BindGlobal("HeLP_settings", ["4ti2", h2, h3, h4]);
    Info( HeLP_Info, 1, "'4ti2' will be used from now on.\n");
    if IO_FindExecutable( "zsolve" ) = fail then
      Info( HeLP_Info, 1, "Though note I could not find the function zsolve.\n");
    fi;
#  elif arg[1] = "4ti2" and IO_FindExecutable( "zsolve" ) = fail then
#    BindGlobal("HeLP_settings", [h1, h2, h3, h4]);
#    Info( HeLP_Info, 1, "The executable 'zsolve' (from 4ti2) was not found.\nPlease install 4ti2 in a directory contained in the PATH variable.\nThe calculations will be performed as before.");
  elif arg[1] = "normaliz" then
    BindGlobal("HeLP_settings", ["normaliz", h2, h3, h4]);
    Info( HeLP_Info, 1, "'normaliz' will be used from now on.");
    if LoadPackage("NormalizInterface") = fail then
      Info( HeLP_Info, 1, "Though note I could not load the NormalizInterface package .\n");
    fi;
#  elif arg[1] = "normaliz" and LoadPackage("NormalizInterface") = fail then
#    BindGlobal("HeLP_settings", [h1, h2, h3, h4]);
#    Info( HeLP_Info, 1, "The executable 'BNmzCone' (from normaliz) was not found.\nPlease install normaliz. See the manual of the package NormalizInterface.\nThe calculations will be performed as before.");
  fi;
else
  Info( HeLP_Info, 1, "Argument of 'HeLP_Solver' must be empty or \"4ti2\" or \"normaliz\"."); 
fi;
end);


#s#############################################################################################################
InstallGlobalFunction(HeLP_UseRedund, function(b)
# Arguments: boolean
# return: nothing
# changes the value of HeLP_settings[2] to the value of the argument of the function
local h1, h3, h4;
if IsBool(b) then
  h1 := HeLP_settings[1];
  h3 := HeLP_settings[3];
  h4 := HeLP_settings[4];
  MakeReadWriteGlobal("HeLP_settings");  
  UnbindGlobal("HeLP_settings");
  if b and IO_FindExecutable( "redund" ) <> fail then
    BindGlobal("HeLP_settings", [h1, b, h3, h4]);
    Info( HeLP_Info, 1, "'redund' will be used from now on.");
  elif b and IO_FindExecutable( "redund" ) = fail then
    BindGlobal("HeLP_settings", [h1, false, h3, h4]);
    Info( HeLP_Info, 1, "The executable 'redund' (from package the lrslib-package) was not found.\nPlease install 'redund' in a directory contained in the PATH variable.\nThe calculations will be performed without using 'redund'.");
  else
    BindGlobal("HeLP_settings", [h1, false, h3, h4]);
    Info( HeLP_Info, 1, "The calculations will be performed without using 'redund' from now on.");
  fi;
else
  Info( HeLP_Info, 1, "Argument of 'HeLP_UseRedund' must be a boolean.");
fi;
end);


##############################################################################################################
InstallGlobalFunction(HeLP_Change4ti2Precision, function(string)
# Arguments: string
# return: nothing
# changes the value of HeLP_settings[3] to the value of the argument
local h1, h2, h4;
if string in ["32", "64", "gmp"] then
  h1 := HeLP_settings[1];
  h2 := HeLP_settings[2];
  h4 := HeLP_settings[4];
  MakeReadWriteGlobal("HeLP_settings");
  UnbindGlobal("HeLP_settings");
  BindGlobal("HeLP_settings", [h1, h2, string, h4]);
  Info( HeLP_Info, 1, "The calculations of 4ti2 will be performed with precision ", string, " from now on.");
else
  Info( HeLP_Info, 1, "Only \"32\", \"64\" and \"gmp\" are allowed as argument of 'HeLP_Change4ti2Precision'.");
fi;
end);

##############################################################################################################
InstallGlobalFunction(HeLP_Vertices, function(string)
# Arguments: string
# return: nothing
# changes the value of HeLP_settings[4] to the value of the argument
local h1, h2, h3;
if string in ["vertices", "novertices", "default"] then
  h1 := HeLP_settings[1];
  h2 := HeLP_settings[2];
  h3 := HeLP_settings[3];
  MakeReadWriteGlobal("HeLP_settings");
  UnbindGlobal("HeLP_settings");
  BindGlobal("HeLP_settings", [h1, h2, h3, string]);
  if string = "vertices" then
    Info( HeLP_Info, 1, "The calculations of normaliz will always compute VerticesOfPolyhedron from now on.");
  elif string = "novertices" then
    Info( HeLP_Info, 1, "The calculations of normaliz will not compute VerticesOfPolyhedron from now on.");
  elif string = "default" then
    Info( HeLP_Info, 1, "The calculations of normaliz will compute VerticesOfPolyhedron, if there is a trivial solution, from now on.");
  fi;
else
  Info( HeLP_Info, 1, "Only \"vertices\", \"novertices\" and \"default\" are allowed as argument of 'HeLP_Change4ti2Precision'.");
fi;
end);


##############################################################################################################
InstallGlobalFunction(HeLP_AutomorphismOrbits,  function( arg )
# Arguments: charactertable, order [, partial augmentations]
# return: list
# returns a list of representatives of the orbits of the possible partial augmentations under the action of the automorphism group of G
local C, ord, sols, G, Conj, A, I, rep, classes, images, a, pos, ocr, classnames, orignames, orbit, solorbits, perms, s, pis, res;
C := arg[1];
ord := arg[2];
if not IsCharacterTable(C) or not IsInt(ord) then
  Error("The first argument has to be a character table, the second has to be a positive integer.");
fi;
if Size(arg) > 2 then
  sols := arg[3];
else
  if not IsBound(HeLP_sol[ord]) then
    HeLP_WithGivenOrder(C, ord);
  fi;
  sols := HeLP_sol[ord];
fi;
solorbits := [];
if "UnderlyingGroup" in KnownAttributesOfObject(C) then
  G := UnderlyingGroup(C);
elif "Identifier" in KnownAttributesOfObject(C) then
  G := AtlasGroup(Identifier(C));
  C := CharacterTableWithStoredGroup(G, C);
  if G = fail then
    Info( HeLP_Info, 1, "The underlying group of the character table could not be determined.");
    return fail;
  fi;
else
  Info( HeLP_Info, 1, "The underlying group of the character table could not be determined.");
  return fail;
fi;
Conj := ConjugacyClasses(C);;
ocr := OrdersClassRepresentatives(C);
A := AutomorphismGroup(G);
I := InnerAutomorphismsAutomorphismGroup(A);;
rep := RightTransversal(A, I);

pos := List(Difference(DivisorsInt(ord), [1]), 
       d -> Flat(List(Difference(DivisorsInt(d), [1]), e -> Positions(ocr, e))));

classes := List(pos, k -> List(Conj{k}, cl -> Representative(cl)));
images := [];
for a in rep do
  Add(images, List(classes, k -> List(k,  c -> PositionProperty(Conj, cl -> c^a in cl))));
od;
images := Set(images);
# if Size(arg) = 2 then
#  classnames := ClassNames(C);
#  orignames := List(pos, k -> classnames{k});
#  return List(images, k -> [orignames, "->", List(k, cl -> classnames{cl})]);
# fi;
#if Size(arg) = 3 then
  perms := List(images, k -> List([1..Size(k)], d -> PermListList(pos[d], k[d])));
  while sols <> []  do
    s := sols[1];
    Add(solorbits, s);
    orbit := [];
    for pis in perms do
      res := List([1..Size(s)], j -> Permuted(s[j], pis[j]));
      Add(orbit, res);
    od;
    sols := Difference(sols, orbit);
  od;   
  return solorbits;
# fi;
end);




##############################################################################################################
InstallGlobalFunction(HeLP_IsZCKnown, function(G)
local primes, p, P, NT, CNT, ANT, N;
if IsNilpotent(G) then
  Info(HeLP_Info, 1, "G is nilpotent, hence the Zassenhaus Conjecture holds by A. Weiss, 'Torsion units in integral group rings', J. Reine Angew. Math., 415, 175--187, 1991.");
  return true;
fi;
primes := PrimeDivisors(Order(G));
for p in primes do
  P := SylowSubgroup(G, p);
  if IsNormal(G, P) and IsAbelian(G/P) then
    Info(HeLP_Info, 1, "G has a normal Sylow ", p, "-subgroup with abelian complement, hence the Zassenhaus Conjecture holds by M. Hertweck, 'On the torsion units of some integral group rings', Algebra Colloq., 13(2), 329--348, 2006.");
    return true;
  fi;
od;
NT := NormalSubgroups(G);
CNT := Filtered(NT, N -> IsCyclic(N));
for N in CNT do
  if IsAbelian(G/N) then
    Info(HeLP_Info, 1, "G is cyclic-by-abelain, hence the Zassenhaus Conjecture holds by M. Caicedo, L. Margolis, A. del Rio,  'Zassenhaus conjecture for cyclic-by-abelian groups', J. Lond. Math. Soc. (2), 88(1), 65--78, 2013.");
    return true;
  fi;
od;
ANT := Filtered(NT, N -> IsCyclic(N));
for N in ANT do
  if IsPrime(Index(G, N)) then
    p :=Index(G, N);
    if Set(List(PrimeDivisors(Order(N)), p -> Index(G, N) < p)) = [true] then
      Info(HeLP_Info, 1, "G has a normal abelian subgroup A with complement of order a prime q such that each prime divisor of |A| is greater than q, hence the Zassenhaus Conjecture holds by Z. Marciniak, J. Ritter, S.K. Sehgal,  A. Weiss, 'Torsion units in integral group rings of some metabelian groups. II.',  J. Number Theory 25 (1987), no. 3, 340–352.");
      return true;     
    fi;
  fi;
od;
if Order(G) < 144 then
  Info(HeLP_Info, 1, "G has order smaller than 144, hence the Zassenhaus Conjecture holds by A. Bächle, A. Herman, A. Konovalov, L. Margolis, G. Singh, 'The status of the Zassenhaus conjecture for small groups', arXiv:1609.00042, 2016.");
  return true;
fi;
return false;
end);

##################
##3 Fucntions from version 4
################

###############################################################################################
# Arguments: Underlying character table, order of unit, p.a.'s of unit (not of powers), underlying group, normal subgroup N of underlying group, positions of conj classes of order dividing k in UCT
# Output: Whether the unit is 1 modulo the normal subgroup N
InstallGlobalFunction(HeLP_IsOneModuloN, function(UCT, k, pa, G, N)
local o, d, phi, Q, CCQ, pos1Q, paim, pau, i, rep, posQ, posconk;

o := OrdersClassRepresentatives(UCT);
posconk := [];
for d in DivisorsInt(k) do
  if d <> 1 then
    Append(posconk, Positions(o, d));
  fi;
od;

phi := NaturalHomomorphismByNormalSubgroup(G, N); 
Q := Image(phi);
CCQ := ConjugacyClasses(Q);
pos1Q := Position(CCQ, One(Q)^Q);
paim := ListWithIdenticalEntries(Size(CCQ), 0);
for i in [1..Size(pa)] do
  if pa[i] <> 0 then
    rep := Representative(ConjugacyClasses(UCT)[posconk[i]]); # image of element with non-trivial p.a.
    posQ := Position(CCQ, (rep^phi)^Q);
    paim[posQ] := paim[posQ] + pa[i]; # add p.a. at right spot in image
  fi;
od;
for i in [1..Size(CCQ)] do
  if i <> pos1Q then
    if paim[i] <> 0 then # only at identity can we have non-zero p.a. to get 1 as image
      return false;
    fi;
  fi;
od;
return true;
end);

##############################################################################################
# Arguments: Character table, order of unit, partial augmentations of unit and powers
# Output: Whether the unit satisfies KP
InstallGlobalFunction(HeLP_UnitSatisfiesKP, function(UCT, k, pa)
local o, divsnot1, i, d, papow, divsdnot1, count, dd, odd;
o := OrdersClassRepresentatives(UCT);
divsnot1 := Filtered(DivisorsInt(k), x -> x <> 1);
for i in [1..Size(divsnot1)] do
  d := divsnot1[i];
  papow := pa[i];
  divsdnot1 := Filtered(DivisorsInt(d), x -> x <> 1);
  count := 0;
  for dd in divsdnot1 do
    odd := Positions(o, dd);
    if dd < d then
      if Sum(papow{[count+1..count+Size(odd)]}) <> 0 then
        return false;
      fi;
    else 
      if Sum(papow{[count+1..count+Size(odd)]}) <> 1 then
        return false;
      fi;
    fi;
    count := count + Size(odd);
  od;
od;
return true;
end);
###########################################################################################
# Function to ignore additional info on vanishing partial augmentations included in version 4 and reproduce earlier results
# Arguments: Ordinary character table
# Output: Same table, but without underlying group
InstallGlobalFunction(HeLP_ForgetUnderlyingGroup, function(C)
local CCop;
CCop := rec(UnderlyingCharacteristic := 0, Size := Size(C), NrConjugacyClasses := NrConjugacyClasses(C), OrdersClassRepresentatives := OrdersClassRepresentatives(C), SizesCentralizers := SizesCentralizers(C), Irr := Irr(C) );
CCop := ConvertToCharacterTable(CCop);
return CCop;
end);


#E

