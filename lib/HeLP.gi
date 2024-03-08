##
##                                                               HeLP package
##
##                                 Andreas Bächle, Vrije Universiteit Brussel
##                                        Leo Margolis, Universität Stuttgart
##
#############################################################################


########################################################################################################
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

#########################################################3
#################################################################################################################
# New functions in version 4
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
