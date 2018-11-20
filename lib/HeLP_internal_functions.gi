BindGlobal("HeLP_CT", CharacterTable(SmallGroup(1,1)));
BindGlobal("HeLP_sol", [[[[1]]]]);          # sol[k] contains the possible solutions for elements of order k
MakeReadWriteGlobal("HeLP_sol");
# HeLP_sol will be set as global variable on loading of the package to have a warning in case it already has a value
# afterwards it is made RaedWrite so that the user can also change its value.

########################################################################################################
# InstallGlobalFunction(HeLP_IsIntVectINTERNAL, function(v)
BindGlobal("HeLP_IsIntVectINTERNAL", function(v)
# Arguments: a vector
# Output: true if the vector has only integral entries, false otherwise
local w, j;
w := IsVector(v);
for j in [1..Size(v)] do
  w := w and IsInt(v[j]);
od;
return w;
end);

########################################################################################################

BindGlobal("HeLP_MakeRightSideCharINTERNAL", function(chi, k, properdivisors, positions, pa)
# Arguments: A character, order of the unit, proper divisors of k, a list of positions of the conjugacy
# classes having potential non-trivial partial augmentation on every proper power of u in the list of conjugacy classes of chi, the partial augmentations of the proper powers of u
# Output: The vector used in HeLP_MakeSystemINTERNAL
# This is an internal function.
local Q, j, l, v, K, r, a;
Q := Rationals;
a := [];
for l in [0..(k-1)] do         # looping over the k-th roots of unity (i.e. entries of a)
    a[l+1] := [];
    v := 0;
    for j in [1..Size(properdivisors)] do	#  loop over (proper) the divisors of k
        K := CF(properdivisors[j]);
        for r in [1..Size(positions[j])] do
            v := v + pa[j][r]*Trace(K, Q, chi[positions[j][r]]*E(properdivisors[j])^(-l));
        od;
    od;        
    # Note that a contains k*the value of the HeLP-system (we do not divide by k here)
    a[l+1] := v;
od;
return a;
end);

########################################################################################################

BindGlobal("HeLP_DuplicateFreeSystemINTERNAL", function(T, a)
#Arguments: A matrix and a vector building a system of inequalities
#Output: Same system of inequalities, where lines appearing more then once are removed.-
local Tsimple, asimple, W, x, i;
W := [];
for i in [1..Size(T)] do
  W[i] := ShallowCopy(T[i]);
  Add(W[i], a[i]);
od;
W := DuplicateFreeList(W);
W := TransposedMatMutable(W);
asimple := W[Size(W)];
Remove(W, Size(W));
Tsimple := TransposedMat(W);
return [Tsimple,asimple];
end);


########################################################################################################

BindGlobal("HeLP_TestSimplifiedSystemINTERNAL",  function(T,a,k,pa)
# Arguments: A matrix, a vector, the order of the unit, the partial augmentations of the proper powers of the unit
# Output: The possible partial augmentations given by the HeLP-method for this partial augmentations of the powers. 
# This function is internal.
# It relies on the 4ti2-interface and program. 
local solutions, v, mue, intersol, Tscaled, ascaled, HeLP_TestConeForIntegralSolutionsINTERNAL;

HeLP_TestConeForIntegralSolutionsINTERNAL := function(b, c, k, T, a)
# Arguments: set of basepoints, set of translations in non-negative direction,  order of the unit, matrix, vector of HeLP-system
# returns true if there is an integral solution in the "non-negative cone", false otherwise
local int, Tba, L, v, w;      #, Tcp
Tba := List(b, v -> T*v + Flat(a));
int := false;  # no integral solution found so far
if c = [] then 
  for v in Tba do
    int := int or HeLP_IsIntVectINTERNAL(v);
    if int then break; fi;
  od;
else
  # Check if there is a combination y = x + sum l_j c_j (x in b, l_j in [0, 1, ..., k-1], c_j in c) such that Ty + a is integral
  L := List(c, v -> List([0..k-1], j -> j*T*v));
  for v in Tba do
    for w in IteratorOfCartesianProduct(L) do
      int := int or HeLP_IsIntVectINTERNAL(v + Sum(w));
      if int then break; fi;
    od;
    if int then break; fi;
  od;
fi;
return int;
end;

solutions := 4ti2Interface_zsolve_equalities_and_inequalities([ListWithIdenticalEntries(Size(T[1]), 1)], [1], T, -a);
# there are infinitely many solutions if there is a base-point, i.e. solutions[1] <> [], and there are translations,
# i.e. solutions[2] <> [] or solutions[3] <> [] s.t. T*x + a is integral with x = b + \sum l_c v_c 
# (b \in solutions[1], v_c \in \solutions[2] and l_c non-negative integers (w.l.o.g. l_c < k)).
if solutions[1] = [] then       # No solutions at all
  return [];
elif solutions[2] = [] and solutions[3] = [] then # finitely many solutions
  intersol := [];
  for v in solutions[1] do
    Tscaled := 1/k*T;
    ascaled := 1/k*a;
    mue := Tscaled*v + ascaled;		# calculating the multiplicities of the eigenvalues
    if HeLP_IsIntVectINTERNAL(mue) then	# checking if the other condition, i.e. the multiplicities are integers, of HeLP are satisfied
      Append(intersol, [Concatenation(pa, [v])]);	# why do we need to put '[v]' here and not just 'v'? v should already be a vector?
    fi;
  od;
  return intersol;
else 
  Tscaled := 1/k*T;
  ascaled := 1/k*a;
  if HeLP_TestConeForIntegralSolutionsINTERNAL(solutions[1], solutions[2], k, Tscaled, ascaled) then # infinitely many integral solutions
    return "infinite";
  else    # infinitely many solutions for the system, but none of them integral
    return []; 
  fi;
fi;
end);

########################################################################################################

BindGlobal("HeLP_MakeCoefficientMatrixCharINTERNAL", function(chi, k, poscon)
# Arguments: A character, order of the unit, positions of the conjugacy classes having potential non-trivial partial augmentation on u in the list of conjugacy classes of chi
# Output: The matrix used in HeLP_MakeSystemINTERNAL
# This is an internal function.
local K, l, r, T;
K := Field(Rationals, [E(k)]);
T := [];
for l in [0..(k-1)] do    # looping over the k-th roots of unity (i.e. rows of T)
    T[l+1] := [];
    for r in [1..Size(poscon)] do	    # looping over the columns of T    
        T[l+1][r] := Trace(K, Rationals, chi[poscon[r]]*E(k)^(-l));
        # Note that T contains k*the value of the HeLP-system (we do not divide by k here)
    od;    
od;
return T;
end);


########################################################################################################

BindGlobal("HeLP_MakeSystemINTERNAL", function(C, k, UCT, pa)
# Arguments: list of characters, order, underlying character table, partial augmentations of proper powers of u
# Output: matrix and vector of the system obtain by the HeLP constraints
# This is an internal function.
local properdivisors, chi, d, e, i, W, T, a, r, l, t, CS, o, posconk, poscondiv, poscondivd, paraug, extended_pa;
properdivisors := Filtered(DivisorsInt(k), n -> not (n=k));
o := OrdersClassRepresentatives(UCT);
poscondiv := [];
posconk := [];
paraug := [];
for d in properdivisors do
    # determine on which conjugacy classes the unit and its powers might have non-trivial partial augmentations
    if d = 1 then
       Add(poscondiv, Positions(o, 1));
    else
      poscondivd := [];
      for e in Filtered(DivisorsInt(d), f -> not (f = 1)) do
         Append(poscondivd, Positions(o, e));
      od;
      Add(poscondiv, poscondivd);
      Append(posconk, Positions(o, d));
    fi;
od;
Append(posconk, Positions(o, k));
T := [];
a := [];
extended_pa := Concatenation([[1]], pa);	# add the partial augmentation of u^k = 1
for chi in C do
    Append(T, HeLP_MakeCoefficientMatrixCharINTERNAL(chi, k, posconk));
    Append(a, HeLP_MakeRightSideCharINTERNAL(chi, k, properdivisors, poscondiv, extended_pa));
od;
return HeLP_DuplicateFreeSystemINTERNAL(T, a);
end);


########################################################################################################

BindGlobal("HeLP_MakeSystemSConstantINTERNAL", function(C, s, t, UCT, pa_t)
local chi, T, a, o, poscon;
o := OrdersClassRepresentatives(UCT);
poscon := [Position(o, s)];
Append(poscon, Positions(o, t));
T := [];
a := [];
for chi in C do
    Append(T, HeLP_MakeCoefficientMatrixCharINTERNAL(chi, s*t, poscon));
    Append(a, HeLP_MakeRightSideCharINTERNAL(chi, s*t, [1, s, t], [[Position(o, 1)], [Position(o, s)], Positions(o, t)], [[1], [1], pa_t] ) );
od;
return HeLP_DuplicateFreeSystemINTERNAL(T, a);
end);


########################################################################################################

BindGlobal("HeLP_SConstantCharactersINTERNAL", function(C, s, UCT)
# C a list of characters
# UCT the underlying character table
# s the prime with respect to which the characters should be constant
local L, j, k, o, op, w, chi;
o := OrdersClassRepresentatives(UCT);
op := Positions(o, s);
L := [];
for j in [1..Size(C)] do 	# looping over all characters in C
  chi := C[j];
  w := true;
  for k in [1..Size(op)] do
    w := w and (chi[op[1]] = chi[op[k]]);
  od;
  if w then Append(L, [chi]); fi;
od;
return L;
end);


#######################################################################################################
#######################################################################################################


BindGlobal("HeLP_CheckCharINTERNAL", function(t)
# Argument: list of characters
# Output: nothing
# Resets all global variables and sets HeLP_CT to the new character table, if the characters in the argument
# belong to another character table than the one used so far
# Tries to verify if the new character table belongs to the same group as the character table used so far.
local UCT, KP, same, x, CT, TPCT, pi, tau, i, n, e, o, oe, C1, C2, HeLP_IsPrefixINTERNAL;

# Local function to check if one string is a prefix of another.
HeLP_IsPrefixINTERNAL := function(s1,s2)
## Arguments: two strings
## Output: true if s1 is a prefix of s2, false otherwise
if not (IsString(s1) and  IsString(s2)) then 
  return false;
elif Size(s1) > Size(s2) then 
  return false;
else
  return s2{[1..Size(s1)]} = s1;
fi;
end;

if not DuplicateFreeList(List(t , chi -> IsClassFunction(chi))) = [true] then
  Error("The argument is not a list of class functions.");
fi;
UCT := UnderlyingCharacterTable(t[1]);
for i in [2..Size(t)] do
  if not UCT = UnderlyingCharacterTable(t[i]) then
    Error("The underlying character tables of class functions 1 and ", i, " do not coincide, calculation stoped.  Try again with all class functions belonging to the same character table.");
  fi;
od;
if IsIdenticalObj(HeLP_CT, UCT) then
  Info(HeLP_Info, 5 ,"Using same character table as until now; all known solutions kept.");
else 
  if IsBrauerTable(UCT) then
    C1 := OrdinaryCharacterTable(UCT);
  else
    C1 := UCT;
  fi;
  if IsBrauerTable(HeLP_CT) then
    C2 := OrdinaryCharacterTable(HeLP_CT);
  else
    C2 := HeLP_CT;
  fi;
  if  "InfoText" in KnownAttributesOfObject(C1) and "InfoText" in KnownAttributesOfObject(C2) and HeLP_IsPrefixINTERNAL(String("origin: ATLAS of finite groups"), InfoText(C1)) and HeLP_IsPrefixINTERNAL(String("origin: ATLAS of finite groups"), InfoText(C2)) then
    # both CT come from the ATLAS    
    # In particular the conjugacy classes are in the same order
    if Identifier(C1) = Identifier(C2) then
      Info(HeLP_Info, 5,"Using character table of the same group; all known solutions kept.");
      MakeReadWriteGlobal("HeLP_CT");  
      UnbindGlobal("HeLP_CT");
      BindGlobal("HeLP_CT", UCT);      
    else
      MakeReadWriteGlobal("HeLP_CT");
      UnbindGlobal("HeLP_CT");
      BindGlobal("HeLP_CT", UCT);         
      HeLP_sol := [[[[1]]]];
      Info(HeLP_Info, 5, "USED CHARACTER TABLE CHANGED TO ", HeLP_CT, ", ALL GLOBAL VARIABLES RESET.");
    fi;
  else
    MakeReadWriteGlobal("HeLP_CT");
    UnbindGlobal("HeLP_CT");
    BindGlobal("HeLP_CT", UCT);     
    HeLP_sol := [[[[1]]]];
    Info(HeLP_Info, 5, "USED CHARACTER TABLE CHANGED TO ", HeLP_CT, ", ALL GLOBAL VARIABLES RESET.");
  fi;
fi;    
end);


########################################################################################################
########################################################################################################

BindGlobal("HeLP_IsTrivialSolutionINTERNAL", function(l, k, o)
# Arguments: a list of partial augmentations of u and all its powers (<> 1), the order of u, the list of the orders of the class representatives
# Output: returns true if all partial augemntations in l are "trivial"
local s, w, j, n, properdiv, i, num, ncc;
if k = 1 then
  return l = [[[1]]];  
fi;
if l = [] then
  return true;
else
  s := true;
  properdiv := Filtered(DivisorsInt(k), e -> e <> 1);
  ncc := [];
  for j in properdiv do
    ncc[j] := Number(o, x-> x = j);
  od;
  for w in l do
    for j in properdiv do
      num := Sum(ncc{Filtered(DivisorsInt(j), e -> not e in [1, j])});
      for i in [1..num] do
        if w[Position(properdiv, j)][i] <> 0 then
          s := false;
          break;
        fi;
      od;
      for i in [num+1..Size(w[Position(properdiv,j)])] do
        if w[Position(properdiv, j)][i] < 0 then
          s := false;
          break;
        fi;
      od;       
      if s = false then
        return false;
      fi;
    od;
  od;
  return s;
fi;
end);

########################################################################################################

BindGlobal("HeLP_SortCharacterTablesByDegreesINTERNAL", function(CharTabs)
# Arguments: a list of character tables
# Output: reorded list of character tables
# tables having characters with smaller degree appear earlier in the list
local pi;
pi :=  SortingPerm(List(CharTabs, C -> CharacterDegrees(C)));
return Permuted(CharTabs, pi);
end);

##############################################################################################################
##############################################################################################################

BindGlobal("HeLP_WagnerTestINTERNAL", function(k, list_paraugs, o)
## Arguments: order of unit and list of possible partial augmentations for units of this order after applying HeLP, list of orders of class representatives dividing k
## Output: list of possible partial augmentations for units of this order after applying the Wagner test
local pd, fac, filtered_solutions, p, s, v, pexp, i, pos;
pd := PrimeDivisors(k);
fac := FactorsInt(k);
pexp:=[];
for p in pd do
  Add(pexp, Size(Positions(fac,p)));
od;
filtered_solutions := [];
if IsPrimePowerInt(k) then
  for v in list_paraugs do
    s := true;
    for i in [1..pexp[1]-1] do
      pos := Positions(o, p^i); 
      if not Sum(v[Size(v)]{pos}) mod p = 0 then
        s := false;
        break;  
       fi;
    od;
    if s then
    Add(filtered_solutions, v);
    fi;  
  od;
else
  for v in list_paraugs do
    s := true; 
    for p in pd do 
      for i in [1..pexp[Position(pd,p)]] do
        pos := Positions(o, p^i); 
        if not Sum(v[Size(v)]{pos}) mod p = 0 then
          s := false;
          break;  
        fi;
      od;
    od;
    if s then
      Add(filtered_solutions, v);
    fi; 
  od;
fi;
return filtered_solutions;
end);




########################################################################################################

BindGlobal("HeLP_WithGivenOrderAndPAINTERNAL", function(arg)
# Same function as HeLP_WithGivenOrderAndPA, but the Character table is not rechecked. Meant mostly for internal use via HeLP_WithGivenOrderINTERNAL
# arguments: a list of characters, order of the unit, acting partial augmentations
local C, k, divisors, W, UCT, intersol;
C := arg[1];
k := arg[2];
UCT := UnderlyingCharacterTable(C[1]);
divisors := DivisorsInt(k);
W := HeLP_MakeSystemINTERNAL(C, k, UCT, arg[3]);
intersol := HeLP_TestSimplifiedSystemINTERNAL(W[1], W[2], k, arg[3]);
return intersol;
end);


########################################################################################################
InstallGlobalFunction(HeLP_WithGivenOrderINTERNAL, function(C, k)

# arguments: C is a list of class functions
# k is the order of the unit in question
# output: Result obtainable using the HeLP method for the characters given in arg[1] for units of order arg[2] or "infinite". The result is stored also in HeLP_sol[k]
local properdivisors, d, pa, npa, asol, intersol, presol, UCT, primediv, p, act_pa, size_npa, j, HeLP_CompatiblePartialAugmentationsINTERNAL;

HeLP_CompatiblePartialAugmentationsINTERNAL := function(pa_powers, k)
# Arguments: list of partial augmentations for the smallest proper powers of u, i.e. for u^p for every prime p dividing the order of u
# tests if the partial augmentations are compatible, e.g. if (u^p)^q has the same p.A. as (u^q)^p
# Output: list of partial augmentations of u if compatible, fail otherwise
local primediv, pa, j, l, div1, properdivisors;
primediv := PrimeDivisors(k);
properdivisors := Filtered(DivisorsInt(k), d -> not d in [1, k]);
pa := ListWithIdenticalEntries(Size(properdivisors), []);
div1 := Filtered(DivisorsInt(k/primediv[1]), d -> not d = 1);
for j in div1 do
  pa[Position(properdivisors, j)] := pa_powers[1][Position(div1, j)];
od;
for l in [2..Size(pa_powers)] do
  div1 := Filtered(DivisorsInt(k/primediv[l]), d -> not d = 1);
  for j in div1 do
    if pa[Position(properdivisors, j)] = [] then
      pa[Position(properdivisors, j)] := pa_powers[l][Position(div1, j)];
    elif pa[Position(properdivisors, j)] <> pa_powers[l][Position(div1, j)] then
      # partial augmentations are not compatible
      return fail;
    fi;
  od; 
od; 
return pa;
end;

UCT := UnderlyingCharacterTable(C[1]);
if IsBrauerTable(UCT) and not Gcd(k, UnderlyingCharacteristic(UCT)) = 1 then
  return "non-admissible";
fi;
properdivisors := Filtered(DivisorsInt(k), d -> not (d = k));
for d in properdivisors do
  if not IsBound(HeLP_sol[d]) then
    Info(HeLP_Info, 4, "    Solutions for order ", d, " not yet calculated.  Restart for this order.");
    presol := HeLP_WithGivenOrderINTERNAL(C, d);
    if presol = "infinite" then
      Print("There are infinitely many solutions for elements of order ", d, ", HeLP stopped.  Try with more characters.\n");
      return "infinite";
    else
      HeLP_sol[d] := presol;
    fi;
  fi;
  if HeLP_sol[d] = [] then # If there are no elements of order d, there are none of order k.    
    Info(HeLP_Info, 4, "There are no elements of order ", d, ", so there are none of order ", k, "."); 
    return [ ];
  fi;
od; 
asol := [];  # stores all solution of elements of order k found so far
primediv := PrimeDivisors(k);	
npa := [];
for p in primediv do
  Add(npa, HeLP_sol[k/p]); 
od;
npa:=Cartesian(npa);
size_npa := Size(npa);
j := 1;
# looping over all possible partial augmentations for the powers of u
for pa in npa do
  if InfoLevel(HeLP_Info) >= 4 then
    Print("#I      Testing possibility ", j, "/", Size(npa), " for elements of order ", k, ".\r");
  fi;
  act_pa := HeLP_CompatiblePartialAugmentationsINTERNAL(pa, k);
  if not act_pa = fail then
    intersol := HeLP_WithGivenOrderAndPAINTERNAL(C, k, act_pa);
    if intersol = "infinite" then
      return "infinite";
    fi;
    Append(asol, intersol);
  fi;
  j := j + 1;
od;
if InfoLevel(HeLP_Info) >= 4 then
  Print("                                                                              \r");
fi;
return DuplicateFreeList(asol);
end);

##############################################################################################################

BindGlobal("HeLP_VerifySolutionINTERNAL", function(C, k, list_paraugs)
# Arguemnts: character table or list of class functions, an order k [list of partial augmentations]
# returns a list of admissable pa's or nothing (if there can not be a unit of that order for theoretical reasons or the method can not be applied)
# checks which of the pa's in HeLP_sol[k] (if there are 2 arguments given) or the pa's in the third  argument fulfill the HeLP-constraints
# from the class functions in the first argument
local chars, W, asol, mu, pa;
if IsBrauerTable(C) and Gcd(k, UnderlyingCharacteristic(C)) > 1 then
  return "non-admissible";
fi;
chars := Irr(C);
asol := [];    # stores the solutions which fulfill the conditions of the HeLP equations
for pa in list_paraugs do
  W := HeLP_MakeSystemINTERNAL(chars, k, C, pa{[1..Size(pa)-1]});
  mu := W[1]*pa[Size(pa)] + W[2];
  if HeLP_IsIntVectINTERNAL(mu) and not false in List(mu, x -> SignInt(x) > -1) then
    Add(asol, pa);
  fi;
od;
return asol;
end);

#E
