##
##                                                               HeLP package
##
##                                 Andreas Bächle, Vrije Universiteit Brussel
##                                        Leo Margolis, Universität Stuttgart
##
#############################################################################

#user oriented functions that need at least one of 4ti2 or Normaliz

########################################################################################################


InstallGlobalFunction(HeLP_ZC, function(GC)
# Argument: an ordinary character table or a group
# Output: true if ZC can be proved using the HeLP method and the data available in GAP or false otherwise. On higher info-levels it prints more information.
local C, o, op, posords, p, BT_not_available, j, k, T, intersol, interintersol, CharTabs, ord, issolvable, isnotpsolvable, result_orders, critical_orders;
if not IsOrdinaryTable(GC) then
  if not IsGroup(GC) then
    Error( "Function HeLP_ZC has to be called with an ordinary character table or a group.");
  else
    if IsNilpotent(GC) then # if the group is nilpotent then ZC is true by [Weiss91]
      Info( HeLP_Info, 1, "Since the given group is nilpotent the Zassenhaus Conjecture holds by a result of Al Weiss.");
      return true;
    else
      C := CharacterTable(GC);
      if C = fail then
        Error( "Calculation of the character table of the given group failed.");
      fi;
    fi;
  fi;
else
  if IsNilpotent(GC) then  # if the group belonging to the character table given is nilpotent then ZC is true by [Weiss91]
    Info( HeLP_Info, 1, "Since the given group is nilpotent the Zassenhaus Conjecture holds by a result of Al Weiss.");
    return true;
  else
     C := GC;
  fi;
fi;
ord := OrdersClassRepresentatives(C);
o := DuplicateFreeList(ord);
op := Filtered(o, m -> IsPrime(m));
issolvable := IsSolvable(C);
isnotpsolvable := Filtered(op, p -> not IsPSolvableCharacterTable(C, p));
# if the group is p-solvable the p-Brauer table will not provide any additional information (by Fong-Swan-Rukolaine [CR81, 22.1]), so it will not be used
if issolvable then
  posords := Filtered(o, d -> not d = 1);# for solvable groups its known that the orders of torsion units coincide with orders of group elements 
				#[Her08a, The orders of torsion units in integral group rings of finite solvable groups]
else
  posords := Filtered(DivisorsInt(Lcm(o)), k -> not k = 1); #All divisors of the exponent of the group, i.e. the orders to be checked in the case of non-solvable groups
fi;
posords := SortedList(posords);
BT_not_available := [];
CharTabs := [C];    # calculate all character tables of interest, which are availbale in GAP and sort them wrt the smallest character degree
for p in isnotpsolvable do
  T := C mod p;
  if not T = fail then
    Add(CharTabs, T);
  else
    Add(BT_not_available, p);
  fi;
od;
CharTabs := HeLP_INTERNAL_SortCharacterTablesByDegrees(CharTabs);
HeLP_INTERNAL_CheckChar(Irr(C));
for k in posords do
  Info( HeLP_Info, 2, "Checking order ", k, ".");
  j := 1;
    Info( HeLP_Info, 3, "  Calculating the solutions for elements of order ", k, ".");
    while not IsBound(intersol) and j <= Size(CharTabs) do
      T := CharTabs[j];
      HeLP_ChangeCharKeepSols(T);
      Info(HeLP_Info, 3, "  Using table ", T, "."); 
      if "UnderlyingGroup" in KnownAttributesOfObject(T) then  # only when we know the group we can use its quotients
        interintersol := HeLP_v4_INTERNAL_WithGivenOrder(Irr(T), k);
      else
        interintersol := HeLP_INTERNAL_WithGivenOrder(Irr(T), k);
      fi;
      if interintersol = "infinite" then
        Error("Unexpected theoretical error.  Please report this to the authors.");
      fi;
      if not interintersol = "non-admissible" then
        intersol := interintersol;
      fi;
      j := j + 1;
    od;
  while not HeLP_INTERNAL_IsTrivialSolution(intersol, k, ord) and j <= Size(CharTabs) do
    T := CharTabs[j];		# test with so far not used character tables
    HeLP_ChangeCharKeepSols(T);
    Info(HeLP_Info, 3, "  Using table ", T, "."); 
    interintersol := HeLP_INTERNAL_VerifySolution(T, k, intersol);
    if not interintersol = "non-admissible" then
      intersol := interintersol;
    fi;
    j := j + 1;
  od;
  HeLP_sol[k] := Filtered(intersol, x -> HeLP_INTERNAL_WagnerTest(C, k, x));  
  if not Size(HeLP_sol[k]) = Size(intersol) then
    Info(HeLP_Info, 4, "  Wagner test for order ", k, " eliminated ", Size(intersol) - Size(HeLP_sol[k]), " possible partial augmentations.");
  fi;
  Unbind(intersol);
od;
result_orders := List(posords, k -> [k, HeLP_INTERNAL_IsTrivialSolution(HeLP_sol[k], k, ord)]);
critical_orders := List(Filtered(result_orders, w -> w[2] = false), v -> v[1]);
if critical_orders <> [] and BT_not_available <> [] then  # not issolvable and 
  Info( HeLP_Info, 1, "The Brauer tables for the following primes are not available: ", Set(BT_not_available), ".");
fi;
if critical_orders <> [] then
  Info( HeLP_Info, 1, "(ZC) can't be solved, using the given data, for the orders: ", critical_orders, ".");
fi;
return critical_orders = [];
end);

 

########################################################################################################

InstallGlobalFunction(HeLP_PQ, function(GC)
# Argument: an ordinary character table or a group
# Output: true if PQ can be proved using the HeLP method and the data available in GAP or false otherwise. On higher info-levels it prints more information.
local C, o, op, crit, crit_p, k, j, isnotpsolvable, intersol, interintersol, CharTabs, p, BT_not_available, T, ord, result_orders, critical_orders;
if not IsOrdinaryTable(GC) then
  if not IsGroup(GC) then
    Error( "Function HeLP_PQ has to be called with an ordinary character table or a group.");
  else
    if IsSolvable(GC) then # if the group is solvable then PQ has an affirmative answer by [Kimmerle06]
      Info( HeLP_Info, 1, "Since the group is solvable, the Prime Graph Question has an affirmative answer for this group by a result of W. Kimmerle.");  
      return true;
    else
      C := CharacterTable(GC);
      if C = fail then
        Error( "Calculation of the character table of the given group failed.");
      fi;
    fi;
  fi;
else
  if IsSolvable(GC) then # if the group belnging to the character table given is solvable then PQ has an affirmative answer by [Kimmerle06]
    Info( HeLP_Info, 1, "Since the group is solvable, the Prime Graph Question has an affirmative answer for this group by a result of W. Kimmerle.");  
    return true;
  else
    C := GC;
  fi;
fi;
if IsSolvable(C) then # if the group is solvable then PQ has an affirmative answer by [Kimmerle06]
  Info( HeLP_Info, 1, "Since the group is solvable, the Prime Graph Question has an affirmative answer for this group by a result of W. Kimmerle.");  
  return true;
fi;
ord := OrdersClassRepresentatives(C);
o := DuplicateFreeList(ord);
op := Filtered(o, m -> IsPrime(m));
crit_p := [ ];
crit := [ ];
# check which edges are missing in the prime graph of G
for j in Combinations(op, 2) do
  if not Product(j) in o then
    Append(crit_p, j);
    Add(crit, Product(j));
  fi;
od;
crit_p := Set(crit_p);
crit := Set(crit);
BT_not_available := [];
# calculate all character tables that are available and of interest and sort them wrt the smallest character degree
CharTabs := [C];
isnotpsolvable := Filtered(op, p -> not IsPSolvableCharacterTable(C, p));
for p in isnotpsolvable do
  T := C mod p;
  if not T = fail then
    Add(CharTabs, T);
  else
    Add(BT_not_available, p);
  fi;
od;
CharTabs := HeLP_INTERNAL_SortCharacterTablesByDegrees(CharTabs);
HeLP_INTERNAL_CheckChar(Irr(C));
# calcuate the minimal possible solutions for elements of prime order involved in (PQ) and for order p*q
for k in Union(crit_p, crit) do
  Info( HeLP_Info, 2, "Checking order ", k, ".");
  j := 1;				# calculate a finite number of pa's
    Info( HeLP_Info, 3, "  Calculating the solutions for elements of order ", k, ".");
    while not IsBound(intersol) and j <= Size(CharTabs) do
      T := CharTabs[j];
      HeLP_ChangeCharKeepSols(T);
      Info(HeLP_Info, 3, "  Using table ", T, "."); 
      if "UnderlyingGroup" in KnownAttributesOfObject(T) then  # only when we know the group we can use its quotients
        interintersol := HeLP_v4_INTERNAL_WithGivenOrder(Irr(T), k);
      else
        interintersol := HeLP_INTERNAL_WithGivenOrder(Irr(T), k);
      fi;
      if interintersol = "infinite" then
        Error("Unexpected theoretical error.  Please report this to the authors.");
      fi;
      if not interintersol = "non-admissible" then
        intersol := interintersol;
      fi;
      j := j + 1;
    od;
  while not HeLP_INTERNAL_IsTrivialSolution(intersol, k, ord) and j <= Size(CharTabs) do
    T := CharTabs[j];			# test with so far not used character tables
    HeLP_ChangeCharKeepSols(T);
    Info(HeLP_Info, 3, "  Using table ", T, "."); 
    interintersol := HeLP_INTERNAL_VerifySolution(T, k, intersol);
    if not interintersol = "non-admissible" then
      intersol := interintersol;
    fi;
    j := j + 1;
  od;
  HeLP_sol[k] := Filtered(intersol, x -> HeLP_INTERNAL_WagnerTest(C, k, x)); 
  if not Size(HeLP_sol[k]) = Size(intersol) then
    Info(HeLP_Info, 4, "  Wagner test for order ", k, " eliminated ", Size(intersol) - Size(HeLP_sol[k]), " possible partial augmentations.");
  fi;
  Unbind(intersol);
od;
result_orders := List(crit, k -> [k, HeLP_INTERNAL_IsTrivialSolution(HeLP_sol[k], k, ord)]);
critical_orders := List(Filtered(result_orders, w -> w[2] = false), v -> v[1]);
if critical_orders <> [] and BT_not_available <> [] then
  Info( HeLP_Info, 1, "The Brauer tables for the following primes are not available: ", Set(BT_not_available), ".");
fi;
if critical_orders <> [] then
  Info( HeLP_Info, 1, "(PQ) can't be solved, using the given data, for the orders: ", critical_orders, ".");
fi;
return critical_orders = [];
end);



########################################################################################################
########################################################################################################

InstallGlobalFunction(HeLP_AllOrders, function(GC)
# Argument: an ordinary character table or a group
# Output: true if ZC can be proved using the HeLP method and the data available in GAP or false otherwise. On higher info-levels it prints more information.
local C, o, op, posords, p, BT_not_available, j, k, T, intersol, interintersol, CharTabs, ord, prob, issolvable, isnotpsolvable, result_orders, critical_orders;
if not IsOrdinaryTable(GC) then
  if not IsGroup(GC) then
    Error( "Function HeLP_ZC has to be called with an ordinary character table or a group.");
  else
    C := CharacterTable(GC);
    if C = fail then
     Error( "Calculation of the character table of the given group failed.");
    fi;
  fi;
else
   C := GC;
fi;
ord := OrdersClassRepresentatives(C);
o := DuplicateFreeList(ord);
op := Filtered(o, m -> IsPrime(m));
issolvable := IsSolvable(C);
isnotpsolvable := Filtered(op, p -> not IsPSolvableCharacterTable(C, p));
# if the group is p-solvable the p-Brauer table will not provide any additional information (by Fong-Swan-Rukolaine [CR81, 22.1]), so will not be used
if issolvable then
  posords := Filtered(o, d -> not d = 1);# for solvable groups its known that the orders of torsion units coincide with orders of group elements 
				#[Her08, The orders of torsion units in integral group rings of finite solvable groups]
else
  posords := Filtered(DivisorsInt(Lcm(o)), k -> not k = 1); #All divisors of the exponent of the group, i.e. the orders to be checked
fi;
posords := SortedList(posords);
BT_not_available := [];
CharTabs := [C]; # calculate all character tables that are available and sort them wrt the smallest character degree
for p in isnotpsolvable do
  T := C mod p;
  if not T = fail then
    Add(CharTabs, T);
  else
    Add(BT_not_available, p);
  fi;
od;
CharTabs := HeLP_INTERNAL_SortCharacterTablesByDegrees(CharTabs);
HeLP_INTERNAL_CheckChar(Irr(C));
for k in posords do
  Info( HeLP_Info, 2, "Checking order ", k, ".");
  j := 1;
  if IsBound(HeLP_sol[k]) then  # use the given pa's
    Info( HeLP_Info, 3, "  Using the known solutions for elements of order ", k, ".");
    intersol := HeLP_sol[k];
  else				# calculate a finite list of pa's
    Info( HeLP_Info, 3, "  Calculating the solutions for elements of order ", k, ".");
    while not IsBound(intersol) and j <= Size(CharTabs) do
      T := CharTabs[j];
      HeLP_ChangeCharKeepSols(T);
      Info(HeLP_Info, 3, "  Using table ", T, "."); 
      if "UnderlyingGroup" in KnownAttributesOfObject(T) then  # only when we know the group we can use its quotients
        interintersol := HeLP_v4_INTERNAL_WithGivenOrder(Irr(T), k);
      else
        interintersol := HeLP_INTERNAL_WithGivenOrder(Irr(T), k);
      fi;
      if interintersol = "infinite" then
        Error("Unexpected theoretical error.  Please report this to the authors.");
      fi;
      if not interintersol = "non-admissible" then
        intersol := interintersol;
      fi;
      j := j + 1;
    od;
  fi;
  while not HeLP_INTERNAL_IsTrivialSolution(intersol, k, ord) and j <= Size(CharTabs) do
    T := CharTabs[j];		# test with so far not used character tables
    HeLP_ChangeCharKeepSols(T);
    Info(HeLP_Info, 3, "  Using table ", T, "."); 
    interintersol := HeLP_INTERNAL_VerifySolution(T, k, intersol);
    if not interintersol = "non-admissible" then
      intersol := interintersol;
    fi;
    j := j + 1;
  od;
  HeLP_sol[k] := Filtered(intersol, x -> HeLP_INTERNAL_WagnerTest(C, k, x));
  #HeLP_sol[k] := HeLP_INTERNAL_WagnerTest(k, intersol, SortedList( Filtered(ord, d -> k mod d = 0 and (not d = 1)) ) );
  if not Size(HeLP_sol[k]) = Size(intersol) then
    Info(HeLP_Info, 4, "  Wagner test for order ", k, " eliminated ", Size(intersol) - Size(HeLP_sol[k]), " possible partial augmentations.");
  fi;
  Unbind(intersol);
od;
result_orders := List(posords, k -> [k, HeLP_INTERNAL_IsTrivialSolution(HeLP_sol[k], k, ord)]);
critical_orders := List(Filtered(result_orders, w -> w[2] = false), v -> v[1]);
if critical_orders <> [] and BT_not_available <> [] then
  Info( HeLP_Info, 1, "The Brauer tables for the following primes are not available: ", Set(BT_not_available), ".");
fi;
if critical_orders <> [] then
  Info( HeLP_Info, 1, "(ZC) can't be solved, using the given data, for the orders: ", critical_orders, ".");
fi;
return critical_orders = [];
end);

 

########################################################################################################

InstallGlobalFunction(HeLP_AllOrdersPQ, function(GC)
# Argument: an ordinary character table or a group
# Output: true if PQ can be proved using the HeLP method and the data available in GAP or false otherwise. On higher info-levels it prints more information.
# Brauer Tafeln nur für ATLAS einfügen
local C, o, op, crit, crit_p, k, j, isnotpsolvable, intersol, interintersol, CharTabs, p, BT_not_available, T, ord, result_orders, critical_orders;
if not IsOrdinaryTable(GC) then
  if not IsGroup(GC) then
    Error( "Function HeLP_PQ has to be called with an ordinary character table or a group.");
  else
    C := CharacterTable(GC);
    if C = fail then
      Error( "Calculation of the character table of the given group failed.");
    fi;
  fi;
else
    C := GC;
fi;
ord := OrdersClassRepresentatives(C);
o := DuplicateFreeList(ord);
op := Filtered(o, m -> IsPrime(m));
crit_p := [];
crit := [];
# check which edges are missing in the prime graph of G
for j in Combinations(op, 2) do
  if not Product(j) in o then
    Append(crit_p, j);
    Add(crit, Product(j));
  fi;
od;
crit_p := Set(crit_p);
crit := Set(crit);
BT_not_available := [];
# calculate all character tables that are available and sort them wrt the smallest character degree
CharTabs := [C];
isnotpsolvable := Filtered(op, p -> not IsPSolvableCharacterTable(C, p));
for p in isnotpsolvable do
  T := C mod p;
  if not T = fail then
    Add(CharTabs, T);
  else
    Add(BT_not_available, p);
  fi;
od;
CharTabs := HeLP_INTERNAL_SortCharacterTablesByDegrees(CharTabs);
HeLP_INTERNAL_CheckChar(Irr(C));
# calcuate the minimal possible solutions for elements of prime order involved in PQ
for k in Union(crit_p, crit) do
  Info( HeLP_Info, 2, "Checking order ", k, ".");
  j := 1;
  if IsBound(HeLP_sol[k]) then		# using the given pa's
    Info( HeLP_Info, 3, "  Using the known solutions for elements of order ", k, ".");
    intersol := HeLP_sol[k];
  else					# calculate a finite number of pa's
    Info( HeLP_Info, 3, "  Calculating the solutions for elements of order ", k, ".");
    while not IsBound(intersol) and j <= Size(CharTabs) do
      T := CharTabs[j];
      HeLP_ChangeCharKeepSols(T);
      Info(HeLP_Info, 3, "  Using table ", T, "."); 
      if "UnderlyingGroup" in KnownAttributesOfObject(T) then  # only when we know the group we can use its quotients
        interintersol := HeLP_v4_INTERNAL_WithGivenOrder(Irr(T), k);
      else
        interintersol := HeLP_INTERNAL_WithGivenOrder(Irr(T), k);
      fi;
      if interintersol = "infinite" then
        Error("Unexpected theoretical error.  Please report this to the authors.");
      fi;
      if not interintersol = "non-admissible" then
        intersol := interintersol;
      fi;
      j := j + 1;
    od;
  fi;
  while not HeLP_INTERNAL_IsTrivialSolution(intersol, k, ord) and j <= Size(CharTabs) do
    T := CharTabs[j];			# test with so far not used character tables
    HeLP_ChangeCharKeepSols(T);
    Info(HeLP_Info, 3, "  Using table ", T, "."); 
    interintersol := HeLP_INTERNAL_VerifySolution(T, k, intersol);
    if not interintersol = "non-admissible" then
      intersol := interintersol;
    fi;
    j := j + 1;
  od;
  HeLP_sol[k] := Filtered(intersol, x -> HeLP_INTERNAL_WagnerTest(C, k, x));
  #HeLP_sol[k] := HeLP_INTERNAL_WagnerTest(k, intersol, SortedList( Filtered(ord, d -> k mod d = 0 and (not d = 1)) ) );
  if not Size(HeLP_sol[k]) = Size(intersol) then
    Info(HeLP_Info, 4, "  Wagner test for order ", k, " eliminated ", Size(intersol) - Size(HeLP_sol[k]), " possible partial augmentations.");
  fi;
  Unbind(intersol);
od;
result_orders := List(crit, k -> [k, HeLP_INTERNAL_IsTrivialSolution(HeLP_sol[k], k, ord)]);
critical_orders := List(Filtered(result_orders, w -> w[2] = false), v -> v[1]);
if critical_orders <> [] and BT_not_available <> [] then
  Info( HeLP_Info, 1, "The Brauer tables for the following primes are not available: ", Set(BT_not_available), ".");
fi;
if critical_orders <> [] then
  Info( HeLP_Info, 1, "(PQ) can't be solved, using the given data, for the orders: ", critical_orders, ".");
fi;
return critical_orders = [];
end);



########################################################################################################
########################################################################################################

InstallGlobalFunction(HeLP_WithGivenOrder, function(arg)
# arguments: arg[1] is a character table or a list of class functions
# arg[2] is the order of the unit in question
# output: Result obtainable using the HeLP method for the characters given in arg[1] for units of order arg[2]. The result is stored also in HeLP_sol[k]
local C, k, UCT, intersol;
if IsCharacterTable(arg[1]) then
  C := Irr(arg[1]);
elif IsList(arg[1]) then
  if arg[1] = [] then return "infinite"; fi;
  C := arg[1];
else
  Error("The first argument of HeLP_WithGivenOrder has to be a character table or a list of class functions.");
fi;
k := arg[2];
UCT := UnderlyingCharacterTable(C[1]);
if IsBrauerTable(UCT) and not Gcd(k, UnderlyingCharacteristic(UCT)) = 1 then
  Info( HeLP_Info, 1, "HeLP can't be applied in this case as the characteristic of the Brauer table divides the order of the unit in question.");
  return "non-admissible";
fi;
if not Lcm(OrdersClassRepresentatives(UCT)) mod k = 0 then
  Info( HeLP_Info, 1, "There is no unit of order ", k, " in ZG as it does not divide the exponent of the group G.");
  return [ ];
fi;
HeLP_INTERNAL_CheckChar(C);
if "UnderlyingGroup" in KnownAttributesOfObject(C) then  # only when we know the group we can use its quotients
  intersol := HeLP_v4_INTERNAL_WithGivenOrder(C, k);
else
  intersol := HeLP_INTERNAL_WithGivenOrder(C, k);
fi;
if intersol = "infinite" then
  Info( HeLP_Info, 1, "The given data admit infinitely many solutions for elements of order ", k, ".");
  return "infinite";
elif intersol = "non-admissible" then
  Error("This should not happen! Call the authors.");
else
  HeLP_sol[k] := intersol;
  Info( HeLP_Info, 1, "Number of solutions for elements of order ", k, ": ", Size(HeLP_sol[k]), "; stored in HeLP_sol[", k, "].");
  return HeLP_sol[k];
fi;
end);

########################################################################################################

InstallGlobalFunction(HeLP_WithGivenOrderAndPA, function(arg)
# arguments: arg[1] is a character table or a list of class functions
# arg[2] is the order of the unit in question
# arg[3] partial augmentations of the powers
local C, k, W, UCT, intersol;
if IsCharacterTable(arg[1]) then
  C := Irr(arg[1]);
elif IsList(arg[1]) then
  if arg[1] = [] then return "infinite"; fi;
  C := arg[1];
else
  Error("The first argument of HeLP_WithGivenOrderAndPA has to be a character table or a list of characters.");
fi;
k := arg[2];
UCT := UnderlyingCharacterTable(C[1]);
if IsBrauerTable(UCT) and not Gcd(k, UnderlyingCharacteristic(UCT)) = 1 then
  Info( HeLP_Info, 1, "HeLP can't be applied in this case as the characteristic of the Brauer table divides the order of the unit in question.");
  return "infinite";
fi;
if not IsPosInt(Lcm(OrdersClassRepresentatives(UCT))/k) then
    Info( HeLP_Info, 1, "There is no unit of order ", k, " in ZG as it does not divide the exponent ", Lcm(OrdersClassRepresentatives(UCT)), " of the group G.");
    return [];
fi;
HeLP_INTERNAL_CheckChar(C);
W := HeLP_INTERNAL_MakeSystem(C, k, UCT, arg[3]);
if W = "infinite" then
  return "infinite";
fi;
intersol := HeLP_INTERNAL_TestSystem(W[1], W[2], k, arg[3]);
if intersol = "infinite" then
  Info( HeLP_Info, 1, "The given data admit infinitely many solutions for elements of order ", k, "."); 
  return "infinite";
else
  Info(HeLP_Info, 1,  "Number of solutions for elements of order ", k, " with these partial augmentations for the powers: ", Size(intersol), ".");
  return intersol;
fi;
end);


########################################################################################################

InstallGlobalFunction(HeLP_WithGivenOrderAndPAAllTables, function(CT, ord, pas)
# arguments: CT ordinay character table
# ord is the order of the unit in question
# pas list of partial augmentations of the powers
local C, relevant_primes, tables, B, p, BT_not_available, intersol, CAct;
if not IsCharacterTable(CT) then
  Error("The first argument of 'HeLP_WithGivenOrderAndPAAllTables' has to be a character table.");
fi;
if not IsPosInt(ord) then
  Error("Second argument of 'HeLP_WithGivenOrderAndPAAllTables' has to be positive integer.\n");
fi;
if IsOrdinaryTable(CT) then
  C := CT;
  CAct := CT;
else
  C := OrdinaryCharacterTable(CT);
  if Gcd(UnderlyingCharacteristic(CT), ord) > 1 then
    CAct := C;
  else
    CAct := CT;
  fi;
fi;
tables := [ ];
if not IsSolvable(C) then
  relevant_primes := Filtered( PrimeDivisors(Size(C)), p -> Gcd(p, ord) = 1);
  BT_not_available := [ ];
  for p in relevant_primes do
    B := C mod p;
    if B = fail then
      Add(BT_not_available, p);
    else
      Add(tables, B);
    fi;
  od;
fi;
if "UnderlyingGroup" in KnownAttributesOfObject(CAct) then  # only when we know the group we can use its quotients
  intersol := HeLP_v4_INTERNAL_WithGivenOrderAndPAAllTables(CAct, tables, ord, pas);
else
  intersol := HeLP_INTERNAL_WithGivenOrderAndPAAllTables(CAct, tables, ord, pas);
fi;
#intersol := HeLP_INTERNAL_WithGivenOrderAndPAAllTables(CAct, tables, ord, pas);
if intersol = "infinite" then
  # This case should never occur.
  Info( HeLP_Info, 1, "The given data admit infinitely many solutions for elements of order ", ord, "."); 
  return "infinite";
else
  if IsSolvable(C) then
     Info( HeLP_Info, 1, "The group is solvable, so only the given character table was used.");   
  fi;
  if BT_not_available <> [ ] then 
    Info( HeLP_Info, 1, "The Brauer tables for the following primes are not available: ", Set(BT_not_available), ".");
  fi;
  Info(HeLP_Info, 1,  "Number of solutions for elements of order ", ord, " with these partial augmentations for the powers: ", Size(intersol), ".");
  return intersol;
fi;
end);

########################################################################################################

InstallGlobalFunction(HeLP_WithGivenOrderAllTables, function(CT, ord)
local C, CAct, B, BT_not_available, relevant_primes, p, tables, properdivisors, intersol, d, presol;
if not IsCharacterTable(CT) then
  Error("First argument of 'HeLP_WithGivenOrderAllTables' has to be a character table.\n");
fi;
if not IsPosInt(ord) then
  Error("Second argument of 'HeLP_WithGivenOrderAllTables' has to be positive integer.\n");
fi;
if IsOrdinaryTable(CT) then
  C := CT;
  CAct := CT;
else
  C := OrdinaryCharacterTable(CT);
  if Gcd(UnderlyingCharacteristic(CT), ord) > 1 then
    CAct := C;
  else
    CAct := CT;
  fi;
fi;
tables := [ ];
if not IsSolvable(C) then
  if not IsPrimePowerInt(ord) then
    relevant_primes := PrimeDivisors(Size(C));
  else
    relevant_primes := Difference( PrimeDivisors(Size(C)), PrimeDivisors(ord) );
  fi;
  BT_not_available := [ ];
  for p in relevant_primes do
    B := C mod p;
    if B = fail then
      Add(BT_not_available, p);
    else
      Add(tables, B);
    fi;
  od;
fi;
HeLP_INTERNAL_CheckChar(Irr(C));
if "UnderlyingGroup" in KnownAttributesOfObject(CAct) then  # only when we know the group we can use its quotients
  intersol := HeLP_v4_INTERNAL_WithGivenOrderAndPAAllTables(CAct, tables, ord);
else
  intersol := HeLP_INTERNAL_WithGivenOrderAndPAAllTables(CAct, tables, ord);
fi;
#intersol := HeLP_INTERNAL_WithGivenOrderAllTables(CAct, tables, ord);
if intersol = "infinite" then
  # This case should never occur.
  Info( HeLP_Info, 1, "The given data admit infinitely many solutions for elements of order ", ord, "."); 
  return "infinite";
else
  if IsSolvable(C) then
     Info( HeLP_Info, 1, "The group is solvable, so only the given character table was used.");   
  fi;
  if BT_not_available <> [ ] then 
    Info( HeLP_Info, 1, "The Brauer tables for the following primes are not available: ", Set(BT_not_available), ".");
  fi;
  Info(HeLP_Info, 1,  "Number of solutions for elements of order ", ord, " with these partial augmentations for the powers: ", Size(intersol), ".");
  HeLP_sol[ord] := intersol;
  return HeLP_sol[ord];
fi;

end);


####################################################################################################

InstallGlobalFunction(HeLP_WithGivenOrderAndPAAndSpecificSystem, function(arg)
#Input: * arg[1]: list containing as entries characters or pairs consisting of characters and integers
# * arg[2]: order of the unit
# * arg[3]: list of partial agmentations of u^d for divisors d <> 1 of k
# * (if present) arg[4]: boolean determining whether the HeLP-constraints determined by arg[1] are returned
# Retrun: list of adminissible pa's for elements of order k (if arg[4] is not true) or
# list containing the admissible partial augmentations the coefficient matrix and the riht hand side f the equalities (if arg[4] = true)
local lst, UCT, k, pa, extended_pa, chi, o, properdivisors, poscondiv, poscondivd, posconk, lst1, p1, d, e, T, a, r, v, j, l, w, x;
if not Size(arg) in [3,4] then
  Error("HeLP_SpecificSystem needs three or four arguments");
fi;
lst := arg[1];
k := arg[2];
pa := arg[3];
if IsClassFunction(lst[1]) then
  UCT := UnderlyingCharacterTable(lst[1]);
elif IsList(lst[1]) then
  UCT := UnderlyingCharacterTable(lst[1][1]);
fi;
o := OrdersClassRepresentatives(UCT);
poscondiv := [ ];
posconk := [ ];
p1 := Positions(o,1);
properdivisors := Filtered(DivisorsInt(k), n -> not (n=k));
for d in properdivisors do
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
T := [ ];
a := [ ];
extended_pa := Concatenation([[1]], pa);	# add the partial augmentation of u^k = 1
lst1 := [ ];
for w in lst do
  if IsClassFunction(w) then	# if classfunc is given then add all possible l's
    Append(lst1, List([0..k-1], j -> [w, j]));
  elif IsList(w) and IsClassFunction(w[1]) and IsInt(w[2]) then
    Add(lst1, [w[1], w[2]]);
  fi;
od;

for w in [1..Size(lst1)] do
  T[w] := [ ];
  chi := lst1[w][1];
  l := lst1[w][2];
  for r in [1..Size(posconk)] do
    T[w][r] := Trace(CF(k), Rationals, chi[posconk[r]]*E(k)^(-l));
  od;
  v := 0;
  for j in [1..Size(properdivisors)] do	#  loop over (proper) divisors of k
      for r in [1..Size(poscondiv[j])] do
          v := v + extended_pa[j][r]*Trace(CF(properdivisors[j]), Rationals, chi[poscondiv[j][r]]*E(properdivisors[j])^(-l));
      od;
  od;        
  # Note that a contains k*the value of the HeLP-system (we do not divide by k here)
  a[w] := v;
od;
x := HeLP_INTERNAL_TestSystem(T, a, k, pa);
if Size(arg) = 4 and arg[4] then
  return [x, (1/k)*T, (1/k)*a];  
else
  return x;
fi;

end);


########################################################################################################

InstallGlobalFunction(HeLP_WithGivenOrderSConstant, function(arg)
## HeLP_WithGivenOrderSConstant(C, s, t)
## C a character table or a collection of class functions
## s, t rational primes
## tests what partial augmentations for elements of order s*t are admissable using those characters from C that are s-constant
local C, s, t, chars, W, UCT, paq, spq, o, tintersol, intersol;
if IsCharacterTable(arg[1]) then
  C := Irr(arg[1]);
elif IsList(arg[1]) then
  if arg[1] = [] then return "infinite"; fi;
  C := arg[1];
else
  Error("The first argument of HeLP_WithGivenOrderSConstant has to be a character table or a list of class functions.");
fi;
s := arg[2];
t := arg[3];
UCT := UnderlyingCharacterTable(C[1]);
HeLP_INTERNAL_CheckChar(C);
if not (IsPosInt(s) and IsPosInt(t) and IsPrime(s) and IsPrime(t)) or s = t then
  Error("HeLP_WithGivenOrderSConstant can only deal with arguments a list of characters and two different positive rational primes.\n");
fi;
o := OrdersClassRepresentatives(UCT);
if Size(Positions(o, s)) = 0 then
  Info( HeLP_Info, 1, "There are no elements of order ", s, " in G.");
  return "non-admissible";
fi;
if Size(Positions(o, t)) = 0 then
  Info( HeLP_Info, 1, "There are no elements of order ", t, " in G.");
  return "non-admissible";
fi;
if Size(Positions(o, s*t)) <> 0 then
  Info( HeLP_Info, 1, "There are elements of order ", s*t, " in G.");
  return "non-admissible";
fi;
if not IsBound(HeLP_sol[t]) then 
  Info( HeLP_Info, 2, "  Partial augmentations for elements of order ", t, " not yet calculated.  Restart for this order.");
  if "UnderlyingGroup" in KnownAttributesOfObject(C) then  # only when we know the group we can use its quotients
    tintersol := HeLP_v4_INTERNAL_WithGivenOrder(C, t);
  else
    tintersol := HeLP_INTERNAL_WithGivenOrder(C, t);
  fi;
#  tintersol := HeLP_INTERNAL_WithGivenOrder(C, t);
  if tintersol = "infinite" then
    Info( HeLP_Info, 1, "Solutions for elements of order ", t, " were not calculated.  When using the characters given in the first argument, there are infinitely many solutions for elements of order ", t, ".\n");
    Info( HeLP_Info, 1, "Calculate first a finite list for elements of order ", t, ".");
    return "non-admissible";   
  else 
    HeLP_sol[t] := tintersol;
  fi;
fi;
chars := HeLP_INTERNAL_SConstantCharacters(Filtered(C, c -> not Set(ValuesOfClassFunction(c)) = [1]),  s, UCT);
if chars = [] then
  Info( HeLP_Info, 1, "There are no non-trivial irreducible ", s, "-constant characters in the list given.");
  return "non-admissible";
fi;
Info( HeLP_Info, 3, "  Number of non-trivial ", s, "-constant characters in the list: ", Size(chars), ".");
spq := [];
# Info( HeLP_Info, 3, "  Number of non-trivial partial augmentations of elements of order: ", t, ": ", Size(HeLP_sol[t]), ".");
for paq in HeLP_sol[t] do
  if InfoLevel(HeLP_Info) >= 4 then
    Print("#I      Testing possibility ", Position(HeLP_sol[t], paq), "/", Size(HeLP_sol[t]), ".\r");
  fi;
  # Workaround to use carriage return in InfoLevel
  W := HeLP_INTERNAL_MakeSystemSConstant(chars, s, t, UCT, paq[1]);
  if W = "infinite" then
    return "infinite";
  fi;
  intersol := HeLP_INTERNAL_TestSystem(W[1], W[2], s*t, [[1], paq[1]]);
  if intersol = "infinite" then
    Info( HeLP_Info, 1, "The given data admits infinitely many solutions.");
    return "infinite";
  else 
    Append(spq, intersol);
  fi;
od;
if InfoLevel(HeLP_Info) >= 4 then
  Print("                                                                              \r");
fi;
if spq = [] then
  HeLP_sol[s*t] := [];			# if by using s-constant characters the existence of elements of order s*t can be excluded, this is stored in the global variable HeLP_sol
  Info( HeLP_Info, 1, "Number of solutions for elements of order ", s*t, ": ", Size(spq), "; stored in HeLP_sol[", s*t, "].");
else
  Info( HeLP_Info, 1, "Number of solutions for elements of order ", s*t, ": ", Size(spq), ".");
fi;
return List(spq, x -> x{[2,3]});	# {[2,3]} -> don't return the "fake" p.a. for elements of order s
end);

##############################################################################################################
InstallGlobalFunction(HeLP_FindAndVerifySolution, function(C, k)
# Arguemnts: character table or list of class functions and an order k
# returns a list of admissable pa's or "infinite"
# Does the same as HeLP_WithGivenOrder but does not build up the system with all information at once, but rather class function by class function.
# If one class function is not enough to obtain a finite number of solutions it tests with 2, 3, ... class functions.
# As soon as there is a finite number of solution the function uses HeLP_Verify solution to check which of the solutions fulfill the constraints of all class functions given
local chars, UCT, t, D, d, j, S, intersol;
if IsCharacterTable(C) then
  chars := Irr(C);
elif IsList(C) then
  if C = [] then return "infinite"; fi;
  chars := C;
else
  Error("The first argument of HeLP_FindAndVerifySolution has to be a character table or a list of class functions.");
fi;
UCT := UnderlyingCharacterTable(chars[1]);
if IsBrauerTable(UCT) and not Gcd(k, UnderlyingCharacteristic(UCT)) = 1 then
  Info( HeLP_Info, 1, "HeLP can't be applied in this case as the characteristic of the Brauer table divides the order of the unit in question.");
  return ;
fi;
if not Lcm(OrdersClassRepresentatives(UCT)) mod k = 0 then
  Info( HeLP_Info, 1, "There is no unit of order ", k, " in ZG as it does not divide the exponent of the group G.");
  return [];
fi;
HeLP_INTERNAL_CheckChar(chars);
D := DuplicateFreeList(Filtered(chars, c -> not Set(ValuesOfClassFunction(c)) = [1]));
for d in Filtered(DivisorsInt(k), e -> not (e = 1 or e = k)) do
  if not IsBound(HeLP_sol[d]) then
    if HeLP_FindAndVerifySolution(D, d) = "infinite" then
      Info( HeLP_Info, 1, "There are infinitely many solutions for elements of order ", d, ", HeLP stopped.  Try with more characters.");
      return "infinite";
    fi;
  fi; 
od;
for j in [1..Size(D)] do
  S := Combinations([1..Size(D)], j);
  for t in S do
    if InfoLevel(HeLP_Info) >= 4 then
      Print("#I  Checking order ", k, " with possibility [", j, ":", Position(S,t), "]             \n");
    fi;
    if "UnderlyingGroup" in KnownAttributesOfObject(C) then  # only when we know the group we can use its quotients
      intersol := HeLP_v4_INTERNAL_WithGivenOrder(D{t}, k);
    else
      intersol := HeLP_INTERNAL_WithGivenOrder(D{t}, k);
    fi;
    #intersol := HeLP_INTERNAL_WithGivenOrder(D{t}, k);
    if not intersol = "infinite" then 
      break;
    fi;
  od;
  if not intersol = "infinite" then
    break;
  fi;
od;
if intersol = "infinite" then
  return "infinite";
else
  if InfoLevel(HeLP_Info) >= 4 then
    Print("#I  Checking whether the solution for order fulfil the constraints from the other characters.      \r");
  fi;
  intersol := HeLP_VerifySolution(D, k, intersol);
  HeLP_sol[k] := intersol;
  Info( HeLP_Info, 1, "Number of solutions for elements of order ", k, ": ", Size(HeLP_sol[k]), "; stored in HeLP_sol[", k, "].");
  return intersol;
fi;
end);


##################
##3 Fucntions from version 4
################

# Function was written together with Andreas in Brussels at some point
InstallGlobalFunction(HeLP_SP, function(GC)
# Argument: an ordinary character table or a group
# Output: true if Spectrum Problem can be proved using the HeLP method and the data available in GAP or false otherwise. On higher info-levels it prints more information.
local C, o, op, posords, k, j, isnotpsolvable, intersol, interintersol, CharTabs, p, BT_not_available, T, ord, result_orders, critical_orders;
if not IsOrdinaryTable(GC) then
  if not IsGroup(GC) then
    Error( "Function HeLP_SP has to be called with an ordinary character table or a group.");
  else
    if IsSolvable(GC) then # if the group is solvable then SP has an affirmative answer by [Hertweck08]
      Info( HeLP_Info, 1, "Since the group is solvable, the Spectrum Problem has an affirmative answer for this group by a result of M. Hertweck.");  
      return true;
    else
      C := CharacterTable(GC);
      if C = fail then
        Error( "Calculation of the character table of the given group failed.");
      fi;
    fi;
  fi;
else
  if IsSolvable(GC) then # if the group belonging to the character table given is solvable then SP has an affirmative answer by [Hertweck08]
    Info( HeLP_Info, 1, "Since the group is solvable, the Spectrum Problem has an affirmative answer for this group by a result of M. Hertweck.");  
    return true;
  else
    C := GC;
  fi;
fi;
if IsSolvable(C) then # if the group is solvable then SP has an affirmative answer by [Hertweck08]
  Info( HeLP_Info, 1, "Since the group is solvable, the Spectrum Problem has an affirmative answer for this group by a result of M. Hertweck.");  
  return true;
fi;
ord := OrdersClassRepresentatives(C);
o := DuplicateFreeList(ord);
op := Filtered(o, m -> IsPrime(m));
# check which divisors of exp(G) don't have corresponding group elements
posords := Filtered(DivisorsInt(Lcm(o)), k -> not k = 1);
posords := Difference(posords, o);

BT_not_available := [];
# calculate all character tables that are available and of interest and sort them wrt the smallest character degree
CharTabs := [C];
isnotpsolvable := Filtered(op, p -> not IsPSolvableCharacterTable(C, p));
for p in isnotpsolvable do
  T := C mod p;
  if not T = fail then
    Add(CharTabs, T);
  else
    Add(BT_not_available, p);
  fi;
od;
CharTabs := HeLP_INTERNAL_SortCharacterTablesByDegrees(CharTabs);
HeLP_INTERNAL_CheckChar(Irr(C));
# calcuate the minimal possible solutions for elements of orders critical for SP
for k in posords do
  Info( HeLP_Info, 2, "Checking order ", k, ".");
  j := 1;
    Info( HeLP_Info, 3, "  Calculating the solutions for elements of order ", k, ".");
    while not IsBound(intersol) and j <= Size(CharTabs) do
      T := CharTabs[j];
      HeLP_ChangeCharKeepSols(T);
      Info(HeLP_Info, 3, "  Using table ", T, "."); 
      if "UnderlyingGroup" in KnownAttributesOfObject(T) then  # only when we know the group we can use its quotients
        interintersol := HeLP_v4_INTERNAL_WithGivenOrder(Irr(T), k);
      else
        interintersol := HeLP_INTERNAL_WithGivenOrder(Irr(T), k);
      fi;
      if interintersol = "infinite" then
        Error("Unexpected theoretical error.  Please report this to the authors.");
      fi;
      if not interintersol = "non-admissible" then
        intersol := interintersol;
      fi;
      j := j + 1;
    od;
  while not HeLP_INTERNAL_IsTrivialSolution(intersol, k, ord) and j <= Size(CharTabs) do
    T := CharTabs[j]; # test with so far not used character tables
    HeLP_ChangeCharKeepSols(T);
    Info(HeLP_Info, 3, "  Using table ", T, "."); 
    interintersol := HeLP_INTERNAL_VerifySolution(T, k, intersol);
    if not interintersol = "non-admissible" then
      intersol := interintersol;
    fi;
    j := j + 1;
  od;
  HeLP_sol[k] := Filtered(intersol, x -> HeLP_INTERNAL_WagnerTest(C, k, x));  
  if not Size(HeLP_sol[k]) = Size(intersol) then
    Info(HeLP_Info, 4, "  Wagner test for order ", k, " eliminated ", Size(intersol) - Size(HeLP_sol[k]), " possible partial augmentations.");
  fi;
  Unbind(intersol);
od;
result_orders := List(posords, k -> [k, HeLP_INTERNAL_IsTrivialSolution(HeLP_sol[k], k, ord)]);
critical_orders := List(Filtered(result_orders, w -> w[2] = false), v -> v[1]);
if critical_orders <> [] and BT_not_available <> [] then  # not issolvable and 
  Info( HeLP_Info, 1, "The Brauer tables for the following primes are not available: ", Set(BT_not_available), ".");
fi;
if critical_orders <> [] then
  Info( HeLP_Info, 1, "(SP) can't be solved, using the given data, for the orders: ", critical_orders, ".");
fi;
return critical_orders = [];
end);

#########################################################################################################
# Version of HeLP_SP which does not overwrite known HeLP_sol. Like HeLP_AllOrdersPQ for HeLP_PQ
InstallGlobalFunction(HeLP_AllOrdersSP, function(GC)
# Argument: an ordinary character table or a group
# Output: true if Spectrum Problem can be proved using the HeLP method and the data available in GAP or false otherwise. On higher info-levels it prints more information.
local C, o, op, posords, k, j, isnotpsolvable, intersol, interintersol, CharTabs, p, BT_not_available, T, ord, result_orders, critical_orders;
if not IsOrdinaryTable(GC) then
  if not IsGroup(GC) then
    Error( "Function HeLP_SP has to be called with an ordinary character table or a group.");
  else
    C := CharacterTable(GC);
    if C = fail then
      Error( "Calculation of the character table of the given group failed.");
    fi;
  fi;
else
  if IsSolvable(GC) then # if the group belonging to the character table given is solvable then SP has an affirmative answer by [Hertweck08]
    Info( HeLP_Info, 1, "Since the group is solvable, the Spectrum Problem has an affirmative answer for this group by a result of M. Hertweck.");  
    return true;
  else
    C := GC;
  fi;
fi;
if IsSolvable(C) then # if the group is solvable then SP has an affirmative answer by [Hertweck08]
  Info( HeLP_Info, 1, "Since the group is solvable, the Spectrum Problem has an affirmative answer for this group by a result of M. Hertweck.");  
  return true;
fi;
ord := OrdersClassRepresentatives(C);
o := DuplicateFreeList(ord);
op := Filtered(o, m -> IsPrime(m));
# check which divisors of exp(G) don't have corresponding group elements
posords := Filtered(DivisorsInt(Lcm(o)), k -> not k = 1);
posords := Difference(posords, o);

BT_not_available := [];
# calculate all character tables that are available and of interest and sort them wrt the smallest character degree
CharTabs := [C];
isnotpsolvable := Filtered(op, p -> not IsPSolvableCharacterTable(C, p));
for p in isnotpsolvable do
  T := C mod p;
  if not T = fail then
    Add(CharTabs, T);
  else
    Add(BT_not_available, p);
  fi;
od;
CharTabs := HeLP_INTERNAL_SortCharacterTablesByDegrees(CharTabs);
HeLP_INTERNAL_CheckChar(Irr(C));
# calcuate the minimal possible solutions for elements of orders critical for SP
for k in posords do
  Info( HeLP_Info, 2, "Checking order ", k, ".");
  j := 1;
  if IsBound(HeLP_sol[k]) then  # use the given pa's
    Info( HeLP_Info, 3, "  Using the known solutions for elements of order ", k, ".");
    intersol := HeLP_sol[k];
  else
    Info( HeLP_Info, 3, "  Calculating the solutions for elements of order ", k, ".");
    while not IsBound(intersol) and j <= Size(CharTabs) do
      T := CharTabs[j];
      HeLP_ChangeCharKeepSols(T);
      Info(HeLP_Info, 3, "  Using table ", T, "."); 
      if "UnderlyingGroup" in KnownAttributesOfObject(T) then  # only when we know the group we can use its quotients
        interintersol := HeLP_v4_INTERNAL_WithGivenOrder(Irr(T), k);
      else
        interintersol := HeLP_INTERNAL_WithGivenOrder(Irr(T), k);
      fi;
      if interintersol = "infinite" then
        Error("Unexpected theoretical error.  Please report this to the authors.");
      fi;
      if not interintersol = "non-admissible" then
        intersol := interintersol;
      fi;
      j := j + 1;
    od;
  fi;
  while not HeLP_INTERNAL_IsTrivialSolution(intersol, k, ord) and j <= Size(CharTabs) do
    T := CharTabs[j]; # test with so far not used character tables
    HeLP_ChangeCharKeepSols(T);
    Info(HeLP_Info, 3, "  Using table ", T, "."); 
    interintersol := HeLP_INTERNAL_VerifySolution(T, k, intersol);
    if not interintersol = "non-admissible" then
      intersol := interintersol;
    fi;
    j := j + 1;
  od;
  HeLP_sol[k] := Filtered(intersol, x -> HeLP_INTERNAL_WagnerTest(C, k, x));  
  if not Size(HeLP_sol[k]) = Size(intersol) then
    Info(HeLP_Info, 4, "  Wagner test for order ", k, " eliminated ", Size(intersol) - Size(HeLP_sol[k]), " possible partial augmentations.");
  fi;
  Unbind(intersol);
od;
result_orders := List(posords, k -> [k, HeLP_INTERNAL_IsTrivialSolution(HeLP_sol[k], k, ord)]);
critical_orders := List(Filtered(result_orders, w -> w[2] = false), v -> v[1]);
if critical_orders <> [] and BT_not_available <> [] then  # not issolvable and 
  Info( HeLP_Info, 1, "The Brauer tables for the following primes are not available: ", Set(BT_not_available), ".");
fi;
if critical_orders <> [] then
  Info( HeLP_Info, 1, "(SP) can't be solved, using the given data, for the orders: ", critical_orders, ".");
fi;
return critical_orders = [];
end);

#########################################################################################################
InstallGlobalFunction(HeLP_KP, function(GC)
# Argument: an ordinary character table or a group
# Output: true if KP can be proved using the HeLP method and the data available in GAP or false otherwise. On higher info-levels it prints more information.
local C, o, op, posords, p, BT_not_available, j, k, T, intersol, interintersol, CharTabs, ord, issolvable, isnotpsolvable, result_orders, critical_orders;
if not IsOrdinaryTable(GC) then
  if not IsGroup(GC) then
    Error( "Function HeLP_KP has to be called with an ordinary character table or a group.");
  else
    if IsNilpotent(GC) then # if the group is nilpotent then ZC is true by [Weiss91]
      Info( HeLP_Info, 1, "Since the given group is nilpotent the Zassenhaus Conjecture holds by a result of Al Weiss.");
      return true;
    else
      C := CharacterTable(GC);
      if C = fail then
        Error( "Calculation of the character table of the given group failed.");
      fi;
    fi;
  fi;
else
  if IsNilpotent(GC) then  # if the group belonging to the character table given is nilpotent then ZC is true by [Weiss91]
    Info( HeLP_Info, 1, "Since the given group is nilpotent the Zassenhaus Conjecture holds by a result of Al Weiss.");
    return true;
  else
     C := GC;
  fi;
fi;
ord := OrdersClassRepresentatives(C);
o := DuplicateFreeList(ord);
op := Filtered(o, m -> IsPrime(m));
issolvable := IsSolvable(C);
isnotpsolvable := Filtered(op, p -> not IsPSolvableCharacterTable(C, p));
# if the group is p-solvable the p-Brauer table will not provide any additional information (by Fong-Swan-Rukolaine [CR81, 22.1]), so it will not be used
if issolvable then
  posords := Filtered(o, d -> not d = 1);# for solvable groups its known that the orders of torsion units coincide with orders of group elements 
				#[Her08a, The orders of torsion units in integral group rings of finite solvable groups]
else
  posords := Filtered(DivisorsInt(Lcm(o)), k -> not k = 1); #All divisors of the exponent of the group, i.e. the orders to be checked in the case of non-solvable groups
fi;
posords := SortedList(posords);
posords := Filtered(posords, x -> not IsPrime(x)); # for units of prime order KP always holds
BT_not_available := [];
CharTabs := [C];    # calculate all character tables of interest, which are availbale in GAP and sort them wrt the smallest character degree
for p in isnotpsolvable do
  T := C mod p;
  if not T = fail then
    Add(CharTabs, T);
  else
    Add(BT_not_available, p);
  fi;
od;
CharTabs := HeLP_INTERNAL_SortCharacterTablesByDegrees(CharTabs);
HeLP_INTERNAL_CheckChar(Irr(C));
posords := Filtered(posords, x -> not IsPrimeInt(x));
for k in posords do
  Info( HeLP_Info, 2, "Checking order ", k, ".");
  j := 1;
    Info( HeLP_Info, 3, "  Calculating the solutions for elements of order ", k, ".");
    while not IsBound(intersol) and j <= Size(CharTabs) do
      T := CharTabs[j];
      HeLP_ChangeCharKeepSols(T);
      Info(HeLP_Info, 3, "  Using table ", T, "."); 
      if "UnderlyingGroup" in KnownAttributesOfObject(T) then  # only when we know the group we can use its quotients
        interintersol := HeLP_v4_INTERNAL_WithGivenOrder(Irr(T), k);
      else
        interintersol := HeLP_INTERNAL_WithGivenOrder(Irr(T), k);
      fi;
      if interintersol = "infinite" then
        Error("Unexpected theoretical error.  Please report this to the authors.");
      fi;
      if not interintersol = "non-admissible" then
        intersol := interintersol;
      fi;
      j := j + 1;
    od;
  while not HeLP_INTERNAL_IsTrivialSolution(intersol, k, ord) and j <= Size(CharTabs) do
    T := CharTabs[j];     # test with so far not used character tables
    HeLP_ChangeCharKeepSols(T);
    Info(HeLP_Info, 3, "  Using table ", T, "."); 
    interintersol := HeLP_INTERNAL_VerifySolution(T, k, intersol);
    if not interintersol = "non-admissible" then
      intersol := interintersol;
    fi;
    j := j + 1;
  od;
  HeLP_sol[k] := Filtered(intersol, x -> HeLP_INTERNAL_WagnerTest(C, k, x));  
  if not Size(HeLP_sol[k]) = Size(intersol) then
    Info(HeLP_Info, 4, "  Wagner test for order ", k, " eliminated ", Size(intersol) - Size(HeLP_sol[k]), " possible partial augmentations.");
  fi;
  Unbind(intersol);
od;
result_orders := List(posords, k -> [ k, HeLP_INTERNAL_UnitsSatisfyKP(C, k, HeLP_sol[k]) ]);
critical_orders := List(Filtered(result_orders, w -> w[2] = false), v -> v[1]);
if critical_orders <> [] and BT_not_available <> [] then  # not issolvable and 
  Info( HeLP_Info, 1, "The Brauer tables for the following primes are not available: ", Set(BT_not_available), ".");
fi;
if critical_orders <> [] then
  Info( HeLP_Info, 1, "(KP) can't be solved, using the given data, for the orders: ", critical_orders, ".");
fi;
return critical_orders = [];
end);

##############################################################################################
InstallGlobalFunction(HeLP_AllOrdersKP, function(GC)
# Argument: an ordinary character table or a group
# Output: true if KP can be proved using the HeLP method and the data available in GAP or false otherwise. On higher info-levels it prints more information.
local C, o, op, posords, p, BT_not_available, j, k, T, intersol, interintersol, CharTabs, ord, issolvable, isnotpsolvable, result_orders, critical_orders;
if not IsOrdinaryTable(GC) then
  if not IsGroup(GC) then
    Error( "Function HeLP_KP has to be called with an ordinary character table or a group.");
  else
    C := CharacterTable(GC);
    if C = fail then
      Error( "Calculation of the character table of the given group failed.");
    fi;
  fi;
else
  if IsNilpotent(GC) then  # if the group belonging to the character table given is nilpotent then ZC is true by [Weiss91]
    Info( HeLP_Info, 1, "Since the given group is nilpotent the Zassenhaus Conjecture holds by a result of Al Weiss.");
    return true;
  else
     C := GC;
  fi;
fi;
ord := OrdersClassRepresentatives(C);
o := DuplicateFreeList(ord);
op := Filtered(o, m -> IsPrime(m));
issolvable := IsSolvable(C);
isnotpsolvable := Filtered(op, p -> not IsPSolvableCharacterTable(C, p));
# if the group is p-solvable the p-Brauer table will not provide any additional information (by Fong-Swan-Rukolaine [CR81, 22.1]), so it will not be used
if issolvable then
  posords := Filtered(o, d -> not d = 1);# for solvable groups its known that the orders of torsion units coincide with orders of group elements 
				#[Her08a, The orders of torsion units in integral group rings of finite solvable groups]
else
  posords := Filtered(DivisorsInt(Lcm(o)), k -> not k = 1); #All divisors of the exponent of the group, i.e. the orders to be checked in the case of non-solvable groups
fi;
posords := SortedList(posords);
posords := Filtered(posords, x -> not IsPrime(x)); # for units of prime order KP always holds
BT_not_available := [];
CharTabs := [C];    # calculate all character tables of interest, which are availbale in GAP and sort them wrt the smallest character degree
for p in isnotpsolvable do
  T := C mod p;
  if not T = fail then
    Add(CharTabs, T);
  else
    Add(BT_not_available, p);
  fi;
od;
CharTabs := HeLP_INTERNAL_SortCharacterTablesByDegrees(CharTabs);
HeLP_INTERNAL_CheckChar(Irr(C));
posords := Filtered(posords, x -> not IsPrimeInt(x));
for k in posords do
  Info( HeLP_Info, 2, "Checking order ", k, ".");
  j := 1;
  if IsBound(HeLP_sol[k]) then  # use the given pa's
    Info( HeLP_Info, 3, "  Using the known solutions for elements of order ", k, ".");
    intersol := HeLP_sol[k];
  else
    Info( HeLP_Info, 3, "  Calculating the solutions for elements of order ", k, ".");
    while not IsBound(intersol) and j <= Size(CharTabs) do
      T := CharTabs[j];
      HeLP_ChangeCharKeepSols(T);
      Info(HeLP_Info, 3, "  Using table ", T, "."); 
      if "UnderlyingGroup" in KnownAttributesOfObject(T) then  # only when we know the group we can use its quotients
        interintersol := HeLP_v4_INTERNAL_WithGivenOrder(Irr(T), k);
      else
        interintersol := HeLP_INTERNAL_WithGivenOrder(Irr(T), k);
      fi;
      if interintersol = "infinite" then
        Error("Unexpected theoretical error.  Please report this to the authors.");
      fi;
      if not interintersol = "non-admissible" then
        intersol := interintersol;
      fi;
      j := j + 1;
    od;
  fi;
  while not HeLP_INTERNAL_IsTrivialSolution(intersol, k, ord) and j <= Size(CharTabs) do
    T := CharTabs[j];     # test with so far not used character tables
    HeLP_ChangeCharKeepSols(T);
    Info(HeLP_Info, 3, "  Using table ", T, "."); 
    interintersol := HeLP_INTERNAL_VerifySolution(T, k, intersol);
    if not interintersol = "non-admissible" then
      intersol := interintersol;
    fi;
    j := j + 1;
  od;
  HeLP_sol[k] := Filtered(intersol, x -> HeLP_INTERNAL_WagnerTest(C, k, x));  
  if not Size(HeLP_sol[k]) = Size(intersol) then
    Info(HeLP_Info, 4, "  Wagner test for order ", k, " eliminated ", Size(intersol) - Size(HeLP_sol[k]), " possible partial augmentations.");
  fi;
  Unbind(intersol);
od;
result_orders := List(posords, k -> [ k, HeLP_INTERNAL_UnitsSatisfyKP(C, k, HeLP_sol[k]) ]);
critical_orders := List(Filtered(result_orders, w -> w[2] = false), v -> v[1]);
if critical_orders <> [] and BT_not_available <> [] then  # not issolvable and 
  Info( HeLP_Info, 1, "The Brauer tables for the following primes are not available: ", Set(BT_not_available), ".");
fi;
if critical_orders <> [] then
  Info( HeLP_Info, 1, "(KP) can't be solved, using the given data, for the orders: ", critical_orders, ".");
fi;
return critical_orders = [];
end);

#E

