
##
##                                                               HeLP package
##
##                                 Andreas Bächle, Vrije Universiteit Brussel
##                                        Leo Margolis, Universität Stuttgart
##
#############################################################################


############################## the internal functions in case both packages only NormalizInterface could be loaded and 4ti2Interface could not

HeLP_settings[1] := "normaliz"; # th usually preferable solver


####################################################################
BindGlobal("HeLP_INTERNAL_TestSystem",  function(T,a,k,pa)
# Arguments: A matrix, a vector, the order of the unit, the partial augmentations of the proper powers of the unit
# Output: The possible partial augmentations given by the HeLP-method for this partial augmentations of the powers. 
# This function is internal.
# It relies on the 4ti2-interface and program. 
local solutions, v, mue, intersol, Tscaled, ascaled, HeLP_TestConeForIntegralSolutionsINTERNAL, temp, T_temp, D, t, s, solext, uneq, interintersol, temptemp, HB, DoVertices;


HeLP_TestConeForIntegralSolutionsINTERNAL := function(b, c, k, T, a)
# Arguments: set of basepoints, set of translations in non-negative direction,  order of the unit, matrix, vector of HeLP-system
# returns true if there is an integral solution in the "non-negative cone", false otherwise
local int, Tba, L, v, w;      
Tba := List(b, v -> T*v + Flat(a));
int := false;  # no integral solution found so far
if c = [ ] then 
  for v in Tba do
    int := int or HeLP_INTERNAL_IsIntVect(v);
    if int then break; fi;
  od;
else
  # Check if there is a combination y = x + sum l_j c_j (x in b, l_j in [0, 1, ..., k-1], c_j in c) such that Ty + a is integral
  L := List(c, v -> List([0..k-1], j -> j*T*v));
  for v in Tba do
    for w in IteratorOfCartesianProduct(L) do
      int := int or HeLP_INTERNAL_IsIntVect(v + Sum(w));
      if int then break; fi;
    od;
    if int then break; fi;
  od;
fi;
return int;
end;


if HeLP_settings[2] then        # if wished, use redund first to minimize the system
  temp := HeLP_INTERNAL_Redund(T,a);
  if temp = "nosystem" then
    return [ ];
  fi;
else                      # redund is not used
  temp := HeLP_INTERNAL_DuplicateFreeSystem(T, a);      # remove multiple times occuring inequalities
fi;

if HeLP_settings[1] = "normaliz" then
  T_temp := ListWithIdenticalEntries(Size(T[1]), 1);
  Add(T_temp,-1);
  temptemp := [ ];
  for t in [1..Size(temp[1])] do
    Add(temptemp, ShallowCopy(temp[1][t]));
    Add(temptemp[t], temp[2][t]);
  od;
  if HeLP_settings[4] = "vertices" then
    DoVertices := true;
  elif HeLP_settings[4] = "novertices" then
    DoVertices := false;
  else  
    DoVertices := false;  # Test, wheter there exists a trivial solution. In this case VerticesOfPolyhedron may be a life saver.
    s := Size(T_temp) - 1;
    for t in [1..s] do
      solext := true;
      intersol := ListWithIdenticalEntries(s, 0);
      intersol[t] := 1;
      Add(intersol, 1);
      for uneq in temptemp do
        if uneq*intersol < 0 then
          solext := false;
          break;
        fi;
      od;
      if solext then
        DoVertices := true; 
        break;
      fi;
    od;
  fi;

  D := NmzCone(["inhom_equations", [T_temp], "inhom_inequalities", temptemp]); #Solve the system
  if DoVertices then
    NmzCompute(D, ["DualMode", "HilbertBasis", "ModuleGenerators", "MaximalSubspace", "VerticesOfPolyhedron"]);
  else
    NmzCompute(D, ["DualMode", "HilbertBasis", "ModuleGenerators", "MaximalSubspace"]);
  fi;

  if NmzModuleGenerators(D) = [ ] then       # No solutions at all
    return [ ];
  elif NmzHilbertBasis(D) = [ ] and NmzMaximalSubspace(D) = [ ] then # finitely many solutions
    intersol := [ ];
    interintersol := NmzModuleGenerators(D);
    interintersol := List(interintersol, x -> x{[1..Size(x)-1]});
    Tscaled := 1/k*T;
    ascaled := 1/k*a;
    for v in interintersol do
      mue := Tscaled*v + ascaled;		# calculating the multiplicities of the eigenvalues
      if HeLP_INTERNAL_IsIntVect(mue) then	# checking if the other condition, i.e. the multiplicities are integers, of HeLP are satisfied
        Append(intersol, [Concatenation(pa, [v])]);	# why do we need to put '[v]' here and not just 'v'? v should already be a vector?
      fi;
    od;
    return intersol;
  else
    interintersol := NmzModuleGenerators(D);
    interintersol := List(interintersol, x -> x{[1..Size(x)-1]});
    if NmzHilbertBasis(D) <> [ ] then
      HB := List(NmzHilbertBasis(D), x -> x{[1..Size(x)-1]});
    else
      HB := [ ];
    fi;
    Tscaled := 1/k*T;
    ascaled := 1/k*a; 
    if HeLP_TestConeForIntegralSolutionsINTERNAL(interintersol, HB, k, Tscaled, ascaled) then # infinitely many integral solutions
      return "infinite";
    else    # infinitely many solutions for the system, but none of them integral
      return [ ]; 
    fi;
  fi;

elif HeLP_settings[1] = "4ti2" then
  Info( HeLP_Info, 1, "4ti2 has been spcified as solver, but 4ti2Interface could not be loaded with the package.");
else
  Info( HeLP_Info, 1, "No solver found or specified. Please install 4ti2 or normaliz so that GAP can use it.");
fi;
end);

######################################

########################################################################################################

BindGlobal("HeLP_INTERNAL_WithGivenOrderAndPA", function(arg)
# Same function as HeLP_WithGivenOrderAndPA, but the Character table is not rechecked. Meant mostly for internal use via HeLP_INTERNAL_WithGivenOrder
# arguments: a list of characters, order of the unit, acting partial augmentations
local C, k, divisors, W, UCT, intersol;
C := arg[1];
k := arg[2];
UCT := UnderlyingCharacterTable(C[1]);
divisors := DivisorsInt(k);
W := HeLP_INTERNAL_MakeSystem(C, k, UCT, arg[3]);
if W = "infinite" then
  return "infinite";
fi;
intersol := HeLP_INTERNAL_TestSystem(W[1], W[2], k, arg[3]);
return intersol;
end);

########################################################################################################
InstallGlobalFunction(HeLP_INTERNAL_WithGivenOrder, function(C, k)

# arguments: C is a list of class functions
# k is the order of the unit in question
# output: Result obtainable using the HeLP method for the characters given in arg[1] for units of order arg[2] or "infinite". The result is stored also in HeLP_sol[k]
local properdivisors, d, pa, npa, asol, intersol, presol, UCT, primediv, p, size_npa, j;

UCT := UnderlyingCharacterTable(C[1]);
if IsBrauerTable(UCT) and not Gcd(k, UnderlyingCharacteristic(UCT)) = 1 then
  return "non-admissible";
fi;
properdivisors := Filtered(DivisorsInt(k), d -> not (d = k));
for d in properdivisors do
  if not IsBound(HeLP_sol[d]) then
    Info(HeLP_Info, 4, "    Solutions for order ", d, " not yet calculated.  Restart for this order.");
    presol := HeLP_INTERNAL_WithGivenOrder(C, d);
    if presol = "infinite" then
      Info( HeLP_Info, 1, "There are infinitely many solutions for elements of order ", d, ", HeLP stopped.  Try with more characters.");
      return "infinite";
    else
      HeLP_sol[d] := presol;
    fi;
  fi;
  if HeLP_sol[d] = [ ] then # If there are no elements of order d, there are none of order k.    
    Info(HeLP_Info, 4, "There are no elements of order ", d, ", so there are none of order ", k, "."); 
    return [ ];
  fi;
od; 
asol := [ ];  # stores all solution of elements of order k found so far
primediv := PrimeDivisors(k);	
npa := [ ];
for p in primediv do
  Add(npa, HeLP_sol[k/p]); 
od;
npa := Cartesian(npa);
npa := List(npa, x -> HeLP_INTERNAL_CompatiblePartialAugmentations(x,k));
npa := Filtered(npa, x -> not x = fail); #The powers to be computed.
size_npa := Size(npa);
j := 1;
# looping over all possible partial augmentations for the powers of u
for pa in npa do
  if InfoLevel(HeLP_Info) >= 4 then
    Print("#I      Testing possibility ", j, "/", size_npa, " for elements of order ", k, ".\r");
  fi;
  intersol := HeLP_INTERNAL_WithGivenOrderAndPA(C, k, pa);
  if intersol = "infinite" then
      return "infinite";
  fi;
  Append(asol, intersol);
  j := j + 1;
od;
if InfoLevel(HeLP_Info) >= 4 then
  Print("                                                                              \r");
fi;
return DuplicateFreeList(asol);
end);

########################################################################################################

BindGlobal("HeLP_INTERNAL_WithGivenOrderAndPAAllTables", function(CAct, tables, ord, pas)
# arguments: CAct character table
# tables: list of other character tables (derived from CAct) that should be used
# ord is the order of the unit in question
# pas list of partial augmentations of the powers
local W, intersol, tab;
W := HeLP_INTERNAL_MakeSystem(Irr(CAct), ord, CAct, pas);
if W = "infinite" then
  # This case should never happen.
  return "infinite";
fi;
intersol := HeLP_INTERNAL_TestSystem(W[1], W[2], ord, pas);
if intersol = "infinite" then
  # This case should never occur.
  Info( HeLP_Info, 1, "The given data admit infinitely many solutions for elements of order ", ord, "."); 
  return "infinite";
else
  for tab in tables do
    if Gcd(UnderlyingCharacteristic(tab), ord) = 1 then
      intersol := HeLP_INTERNAL_VerifySolution(tab, ord, intersol);
    fi;
  od;
  return intersol;
fi;
end);


########################################################################################################


InstallGlobalFunction("HeLP_INTERNAL_WithGivenOrderAllTables", function(CAct, tables, ord)
# arguments: CAct character table
# tables: list of other character tables (derived from CAct) that should be used
# ord is the order of the unit in question
local properdivisors, d, presol, W, asol, primediv, p, npa, size_npa, j, pa, intersol, tab;


properdivisors := Filtered(DivisorsInt(ord), d -> not (d = ord));
for d in properdivisors do
  if not IsBound(HeLP_sol[d]) then
    Info(HeLP_Info, 4, "    Solutions for order ", d, " not yet calculated.  Restart for this order.");
    presol := HeLP_INTERNAL_WithGivenOrderAllTables(CAct, tables, d);
    if presol = "infinite" then
      # this should never happen
      Info( HeLP_Info, 1, "There are infinitely many solutions for elements of order ", d, ", HeLP stopped.");
      return "infinite";
    else
      HeLP_sol[d] := presol;
    fi;
  fi;
  if HeLP_sol[d] = [ ] then # If there are no elements of order d, there are none of order k.    
    Info(HeLP_Info, 4, "There are no elements of order ", d, ", so there are none of order ", ord, "."); 
    return [ ];
  fi;
od;
asol := [ ];  # stores all solution of elements of order k found so far
primediv := PrimeDivisors(ord);	
npa := [ ];
for p in primediv do
  Add(npa, HeLP_sol[ord/p]); 
od;
npa := Cartesian(npa);
npa := List(npa, x -> HeLP_INTERNAL_CompatiblePartialAugmentations(x,ord));
npa := Filtered(npa, x -> not x = fail); #The powers to be computed.
size_npa := Size(npa);
j := 1;
# looping over all possible partial augmentations for the powers of u
for pa in npa do
  if InfoLevel(HeLP_Info) >= 4 then
    Print("#I      Testing possibility ", j, "/", size_npa, " for elements of order ", ord, ".\r");
  fi;
  intersol := HeLP_INTERNAL_WithGivenOrderAndPAAllTables(CAct, tables, ord, pa);
  if intersol = "infinite" then
      return "infinite";
  fi;
  Append(asol, intersol);
  j := j + 1;
od;
if InfoLevel(HeLP_Info) >= 4 then
  Print("                                                                              \r");
fi;
if asol = "infinite" then
  # This case should never occur.
  Info( HeLP_Info, 1, "The given data admit infinitely many solutions for elements of order ", ord, "."); 
  return "infinite";
else
  asol := DuplicateFreeList(asol);
  for tab in tables do
    if Gcd(UnderlyingCharacteristic(tab), ord) = 1 then
      asol := HeLP_INTERNAL_VerifySolution(tab, ord, asol);
    fi;
  od;
  return asol;
fi;
end);

#############3
### Functions from version 4
################################

###########################3
BindGlobal("HeLP_v4_INTERNAL_WithGivenOrderAndPA", function(arg)
# Same function as HeLP_WithGivenOrderAndPA, but the Character table is not rechecked. Meant mostly for internal use via HeLP_INTERNAL_WithGivenOrder
# arguments: a list of characters, order of the unit, acting partial augmentations
local C, k, pa, UCT, nonintclas, o, posconk, d, posconkint, W, intersol, sol, pasol, v, i, j, done, solsol, interintersol, p, G, N;
C := arg[1];
k := arg[2];
pa := arg[3];
UCT := UnderlyingCharacterTable(C[1]);

nonintclas := HeLP_INTERNAL_DetermineInterestingClasses(UCT, k, pa); # this is new and so posconk which goes to MakeSystem changes
o := OrdersClassRepresentatives(UCT);
posconk := [];
for d in DivisorsInt(k) do
  if d <> 1 then
    Append(posconk, Positions(o, d));
  fi;
od;
posconkint := posconk{Difference([1..Size(posconk)], nonintclas) }; # determine classes which go into the inequalities
if posconkint = [ ] then # this can happen, e.g. in SG(144, 33) order 18
  return [ ];
fi;

W := HeLP_v4_INTERNAL_MakeSystem(C, k, UCT, pa, posconkint);
if W = "infinite" then
  return "infinite";
fi;
intersol := HeLP_INTERNAL_TestSystem(W[1], W[2], k, pa);

if intersol = [ ] or intersol = "infinite" then
  return intersol;
else ### Put inside the solutions the 0's at right spot
  interintersol := [ ];
  for i in [1..Size(intersol)] do
    sol := intersol[i];
    pasol := sol[Size(sol)]; # partial augmentations of unit which will be changed
    v := [ ];
    done := 0;
    for j in [1..Size(posconk)] do  # run over all classes which will have an entry
      if j in nonintclas then       # non-interesting classes just filled by 0's
        v[j] := 0;
      else                          # interesting classes filled by solution of system 
        done := done + 1;
        v[j] := pasol[done];
      fi; 
    od;
    solsol := ShallowCopy(sol);
    solsol[Size(solsol)] := v;
    Add(interintersol, solsol);
  od;
  intersol := [ ];
  if IsPrimePowerInt(k) then  # if u is of prime power order and it is 1 modulo O_p(G) we only take trivial solutions
    p := PrimeDivisors(k)[1];
    G := UnderlyingGroup(UCT);
    N := PCore(G, p);
    for sol in interintersol do
      if HeLP_INTERNAL_IsOneModuloN(UCT, p, sol[Size(sol)], G, N, posconk) then
        if HeLP_INTERNAL_IsTrivialSolution([sol], k, o) then
          Add(intersol, sol);
        fi;
      else
        Add(intersol, sol); 
      fi;
    od;
    return intersol;
  fi;
  return interintersol;  
fi;
end);

##############################################################################################
InstallGlobalFunction("HeLP_v4_INTERNAL_WithGivenOrder", function(C, k)
# arguments: C is a list of class functions
# k is the order of the unit in question
# output: Result obtainable using the HeLP method for the characters given in arg[1] for units of order arg[2] or "infinite". The result is stored also in HeLP_sol[k]
local properdivisors, d, pa, npa, asol, intersol, presol, UCT, primediv, p, size_npa, j;

UCT := UnderlyingCharacterTable(C[1]);
if IsBrauerTable(UCT) and not Gcd(k, UnderlyingCharacteristic(UCT)) = 1 then
  return "non-admissible";
fi;
properdivisors := Filtered(DivisorsInt(k), d -> not (d = k));
for d in properdivisors do
  if not IsBound(HeLP_sol[d]) then
    Info(HeLP_Info, 4, "    Solutions for order ", d, " not yet calculated.  Restart for this order.");
    presol := HeLP_v4_INTERNAL_WithGivenOrder(C, d);
    if presol = "infinite" then
      Info( HeLP_Info, 1, "There are infinitely many solutions for elements of order ", d, ", HeLP stopped.  Try with more characters.");
      return "infinite";
    else
      HeLP_sol[d] := presol;
    fi;
  fi;
  if HeLP_sol[d] = [ ] then # If there are no elements of order d, there are none of order k.    
    Info(HeLP_Info, 4, "There are no elements of order ", d, ", so there are none of order ", k, "."); 
    return [ ];
  fi;
od; 
asol := [ ];  # stores all solution of elements of order k found so far
primediv := PrimeDivisors(k);	
npa := [ ];
for p in primediv do
  Add(npa, HeLP_sol[k/p]); 
od;
npa := Cartesian(npa);
npa := List(npa, x -> HeLP_INTERNAL_CompatiblePartialAugmentations(x,k));
npa := Filtered(npa, x -> not x = fail); #The powers to be computed.
size_npa := Size(npa);
j := 1;
# looping over all possible partial augmentations for the powers of u
for pa in npa do
  if InfoLevel(HeLP_Info) >= 4 then
    Print("#I      Testing possibility ", j, "/", size_npa, " for elements of order ", k, ".\r");
  fi;
  intersol := HeLP_v4_INTERNAL_WithGivenOrderAndPA(C, k, pa);
  if intersol = "infinite" then
      return "infinite";
  fi;
  Append(asol, intersol);
  j := j + 1;
od;
if InfoLevel(HeLP_Info) >= 4 then
  Print("                                                                              \r");
fi;
return DuplicateFreeList(asol);
end);

################################################################################################################3
BindGlobal("HeLP_v4_INTERNAL_WithGivenOrderAndPAAllTables", function(CAct, tables, ord, pas)
# arguments: CAct character table
# tables: list of other character tables (derived from CAct) that should be used
# ord is the order of the unit in question
# pas list of partial augmentations of the powers
local W, intersol, tab;
W := HeLP_v4_INTERNAL_MakeSystem(Irr(CAct), ord, CAct, pas);
if W = "infinite" then
  # This case should never happen.
  return "infinite";
fi;
intersol := HeLP_INTERNAL_TestSystem(W[1], W[2], ord, pas);
if intersol = "infinite" then
  # This case should never occur.
  Info( HeLP_Info, 1, "The given data admit infinitely many solutions for elements of order ", ord, "."); 
  return "infinite";
else
  for tab in tables do
    if Gcd(UnderlyingCharacteristic(tab), ord) = 1 then
      intersol := HeLP_INTERNAL_VerifySolution(tab, ord, intersol);
    fi;
  od;
  return intersol;
fi;
end);

#E
