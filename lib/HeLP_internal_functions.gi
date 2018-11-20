BindGlobal("HeLP_CT", CharacterTable(SmallGroup(1,1)));
BindGlobal("HeLP_sol", [[[[1]]]]);          # sol[k] contains the possible solutions for elements of order k
MakeReadWriteGlobal("HeLP_sol");
# HeLP_sol will be set as global variable when loading the package to have a warning in case it is already ued as a variable
# afterwards it is made RaedWrite so that the user can also change its value.

# the global varaiable "HeLP_settings" contains as first entry a string telling which solver to use,
# as a second entry a boolean determining whether redund will be used, as a third entry the precision with which 4ti2 is called
# and finally as the fourth entry whether normaliz will compute VerticesOfPolyhedron

BindGlobal("HeLP_settings", ["normaliz", false, "32", "default"]); # Normally redund does not speed up normaliz-computations


########################################################################################################
# InstallGlobalFunction(HeLP_INTERNAL_IsIntVect, function(v)
BindGlobal("HeLP_INTERNAL_IsIntVect", function(v)
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

BindGlobal("HeLP_INTERNAL_DuplicateFreeSystem", function(T, a)
#Arguments: A matrix and a vector building a system of inequalities
#Output: Same system of inequalities, where lines appearing more then once are removed.
local Tsimple, asimple, W, i;
W := [ ];
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
BindGlobal("HeLP_INTERNAL_Redund", function(A, b)
# Arguments: a matrix and a vector for a system of lienar inequalities from which redund  should remove redundant lines
# Output: Same system of inequalities with redundnat lines removed or "nosystem" in case redund already finds that there are no solutions to the system
# parts of the code originally taken from Sebastian Gutsche's 4ti2-Interface, adapted to use redund
local filestream, i, j, dir, filename, filename2, exec, std_err_out, matrix, rhs, string, nr_rows, nr_columns, cd;
dir := DirectoryTemporary();
# dir := Directory(".");			# for tests write to files in the home-directory to see whether things work correctly
#transform the data to a format that redund can read
filename := IO_File(Filename( dir, "gap_redund.ine" ), "w");
i := Length( A );
if i = 0 then
  return A;
fi;
j := Length( A[ 1 ] );
if j = 0 then
  return A;
fi;
if not ForAll( A, k -> Length( k ) = j ) then
  Error( "Input is not a matrix" );
  return fail;
fi;
if not Length(A) = Length(b) then
  Error( "Input is not a matrix" );
  return fail;
fi;
IO_Write(filename, "H-representation\nbegin\n");
IO_Write(filename, Concatenation( String( i ), " ", String( j+1 ),  " integer\n"  ) );
for i in [1..Length(A)] do
  IO_Write( filename, Concatenation( String( b[i] ), " " ) );  
  for j in [1..Length(A[1])] do
    IO_Write( filename, Concatenation( String( A[i][j] ), " " ) );
  od;
  IO_Write( filename, "\n" );
od;
IO_Write(filename, "end");
# IO_Flush(filename);  # done by the call of IO_Close afterwards
IO_Close(filename);
###
exec := IO_FindExecutable( "redund" );
if exec = fail then
  Error("Executable redund (from package the lrslib-Package) not found.  Please check if it is installed in a directory contained in the PATH variable.");
fi;
filename2 := Filename( dir, "redund_out.ine" );   # write the output of redund to this file
filename := Filename( dir, "gap_redund.ine" );
filestream := IO_Popen3( exec, [ filename, filename2 ] );
# while IO_ReadLine( filestream.stdout ) <> "" do od;             # still busy - is this needed?
std_err_out := Concatenation( IO_ReadLines( filestream.stderr ) );
IO_Close( filestream.stdin );
IO_Close( filestream.stdout );
IO_Close( filestream.stderr );
if std_err_out <> "" then
  Error( Concatenation( "redund Error:\n", std_err_out, "If you continue, your results might be wrong" ) );
fi;

# convert data back to GAP-matrix form 
filestream := IO_File( filename2, "r" );
string := IO_ReadLine( filestream );
while string <> "begin\n" and string <> "" do
  string := IO_ReadLine( filestream );
od;
if string = "" then
  return "nosystem";
fi;
string := IO_ReadLine( filestream );
NormalizeWhitespace( string );
string := SplitString( string, " ", " \n" );
nr_rows := Int(string[1]);
nr_columns := Int(string[2]);
matrix := [ ];
rhs := [ ];
string := IO_ReadLine( filestream );
while string <> "end\n" and string <> "end" and string <> "" do
  NormalizeWhitespace( string );
  string := SplitString( string, " ", " \n" );
  string := List(string, Rat);
  cd := Lcm(List(string, DenominatorRat));
  if cd <> 1 then         # if the inequality returned by redund contains rational coefficients multiply by the common denominator
    string := cd*string;
  fi;
  Add( rhs, Int(string[1]) );
  string := [List( string{[2..Length(string)]}, Int )];
  matrix := Concatenation( matrix, string );
  string := IO_ReadLine( filestream );
od;
if nr_rows <> 0 and (Length( matrix ) <>  nr_rows or Length( matrix[1] ) <>  nr_columns - 1 or Length( matrix ) <> Length( rhs )) then
  Error( "Matrix returned by redund is corrupted." );
   return [ fail, [ nr_rows, nr_columns ], matrix ];
fi;
IO_Close( filestream );
return [matrix, rhs];
end);


########################################################################################################

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
  solutions := 4ti2Interface_zsolve_equalities_and_inequalities([ListWithIdenticalEntries(Size(T[1]), 1)], [1], temp[1], -temp[2] : precision := HeLP_settings[3]);
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
      if HeLP_INTERNAL_IsIntVect(mue) then	# checking if the other condition, i.e. the multiplicities are integers, of HeLP are satisfied
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
      return [ ]; 
    fi;
  fi;

else
  Print("No solver found or specified. Please install 4ti2 or normaliz such that GAP can use it.");

fi;
end);

########################################################################################################

BindGlobal("HeLP_INTERNAL_MakeRightSideChar", function(chi, k, properdivisors, positions, pa)
# Arguments: A character, order of the unit, proper divisors of k, a list of positions of the conjugacy
# classes having potential non-trivial partial augmentation on every proper power of u in the list of conjugacy classes of chi, the partial augmentations of the proper powers of u
# Output: The vector used in HeLP_INTERNAL_MakeSystem
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


BindGlobal("HeLP_INTERNAL_MakeCoefficientMatrixChar", function(chi, k, poscon)
# Arguments: A character, order of the unit, positions of the conjugacy classes having potential non-trivial partial augmentation on u in the list of conjugacy classes of chi
# Output: The matrix used in HeLP_INTERNAL_MakeSystem
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

BindGlobal("HeLP_INTERNAL_MakeSystem", function(C, k, UCT, pa)
# Arguments: list of characters, order, underlying character table, partial augmentations of proper powers of u
# Output: matrix and vector of the system obtain by the HeLP constraints
# This is an internal function.
local properdivisors, chi, d, e, T, a, o, posconk, poscondiv, poscondivd, extended_pa, D, p1;
properdivisors := Filtered(DivisorsInt(k), n -> not (n=k));
o := OrdersClassRepresentatives(UCT);
poscondiv := [];
posconk := [];
p1 := Positions(o,1);
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
T := [ ];
a := [ ];
extended_pa := Concatenation([[1]], pa);	# add the partial augmentation of u^k = 1
D := Filtered(C, x -> Size(DuplicateFreeList(ValuesOfClassFunction(x){Concatenation(p1, posconk)})) <> 1);
if D = [ ] then
  return "infinite";
fi;
for chi in D do
    Append(T, HeLP_INTERNAL_MakeCoefficientMatrixChar(chi, k, posconk));
    Append(a, HeLP_INTERNAL_MakeRightSideChar(chi, k, properdivisors, poscondiv, extended_pa));
od;
return [T, a];
end);


########################################################################################################

BindGlobal("HeLP_INTERNAL_MakeSystemSConstant", function(C, s, t, UCT, pa_t)
local chi, T, a, o, poscon, D, p1;
o := OrdersClassRepresentatives(UCT);
poscon := [Position(o, s)];
Append(poscon, Positions(o, t));
p1 := Positions(o,1);
T := [ ];
a := [ ];
D := Filtered(C, x -> Size(DuplicateFreeList(ValuesOfClassFunction(x){Concatenation(p1, poscon)})) <> 1);
if D = [ ] then
  return "infinite";
fi;
for chi in D do
    Append(T, HeLP_INTERNAL_MakeCoefficientMatrixChar(chi, s*t, poscon));
    Append(a, HeLP_INTERNAL_MakeRightSideChar(chi, s*t, [1, s, t], [[Position(o, 1)], [Position(o, s)], Positions(o, t)], [[1], [1], pa_t] ) );
od;
return [T,a];
end);


########################################################################################################

BindGlobal("HeLP_INTERNAL_SConstantCharacters", function(C, s, UCT)
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


BindGlobal("HeLP_INTERNAL_CheckChar", function(t)
# Argument: list of characters
# Output: nothing
# Resets all global variables and sets HeLP_CT to the new character table, if the characters in the argument
# belong to another character table than the one used so far
# Tries to verify if the new character table belongs to the same group as the character table used so far.
local UCT, i, C1, C2, HeLP_IsPrefixINTERNAL;

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

BindGlobal("HeLP_INTERNAL_IsTrivialSolution", function(l, k, o)
# Arguments: a list of partial augmentations of u and all its powers (<> 1), the order of u, the list of the orders of the class representatives
# Output: returns true if all partial augemntations in l are "trivial"
local s, w, j, properdiv, i, num, ncc;
if k = 1 then
  return l = [ [ [ 1 ] ] ];  
fi;
if l = [ ] then
  return true;
else
  s := true;
  properdiv := Filtered(DivisorsInt(k), e -> e <> 1);
  ncc := [ ];
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

BindGlobal("HeLP_INTERNAL_SortCharacterTablesByDegrees", function(CharTabs)
# Arguments: a list of character tables
# Output: reorded list of character tables
# tables having characters with smaller degree appear earlier in the list
local pi;
pi :=  SortingPerm(List(CharTabs, C -> CharacterDegrees(C)));
return Permuted(CharTabs, pi);
end);

##############################################################################################################
##############################################################################################################
BindGlobal("HeLP_INTERNAL_PositionsInCT", function(C, k)
local o, div, pos, res, w, d, f;
if not IsCharacterTable(C) or not IsPosInt(k) then
  Error("Arguments have to be a character table and a positive integer");
fi;
o :=  OrdersClassRepresentatives(C);
if k = 1 then
  return [Positions(o, 1)];
fi;
div := Filtered(DivisorsInt(k), e -> e <> 1);
pos := [];
res := [];
for d in div do
  pos[d] := Positions(o, d);
  w := [];
  for f in Filtered(DivisorsInt(d), e -> e <> 1) do
    Append(w, pos[f]);  
  od;
  Add(res, w);
od;
return res;
end);

######################################################################################################################

BindGlobal("HeLP_INTERNAL_PValuation", function(n, p)
local v;
if n = 0 then
  return "infinity";
else
  v := 0;
  while n mod p^(v + 1) = 0 do
    v := v + 1;
  od;
  return v;
fi;
end);

#####################################################################################################

BindGlobal("HeLP_INTERNAL_WagnerTest", function(C, k, sol)
local pd, pexp, p, div, positions_in_CT, pm, s, i, j, l, pa_u,
    position_of_relevant_list, cc_u_to_the_pj, nt_s, e_s,
    positions_summands, sum, pos, violation;
if not (IsCharacterTable(C) and IsPosInt(k) and IsList(sol)) then
  Error("Arguments have to be a character table, a positive integer and a list with the possible partial augmentations.");
fi;
if k = 1 then
  Error("k = 1");
fi;
div := Filtered(DivisorsInt(k), e -> e <> 1);
pd := PrimeDivisors(k);
pexp := List(pd, p -> HeLP_INTERNAL_PValuation(k, p));
#if Size(pd) = 1 then
#  return "Prime power";
#fi;
positions_in_CT := HeLP_INTERNAL_PositionsInCT(C, k);
pa_u := sol[Size(sol)]; 	# pa's of u

for i in [1..Size(pd)] do
  p := pd[i];
  for j in [1..pexp[i]] do		# looking at u^(p^j) of order k/(p^j)
    if p^j <> k then
      violation := false;
      position_of_relevant_list := Position(div, k/(p^j)); 	
      # stores at which spot of sol the partial augs of u^p are stored
      cc_u_to_the_pj := positions_in_CT[position_of_relevant_list]; 
      Add(cc_u_to_the_pj, 1); # Thus cc_u_to_the_pj are the classes of elements dividing the order of u^(p^j)
      pm := PowerMap(C, p^j);
      for s in cc_u_to_the_pj do 	# s can be in any conjclass of G whose order divides the order of u^(p^j)
        if s = 1 then 
          e_s := 0; # The Berman-Higman Theoerem, aka the classical Wagner-Test
        else
          nt_s := Position(cc_u_to_the_pj, s);
          e_s := sol[position_of_relevant_list][nt_s]; # The partial augmentation of u^(p^j) with respect to the s'th conjclass 
        fi;
        positions_summands := Positions(pm, s);
        if s = 1 then
          Remove(positions_summands,1); # The augmentation of the 1-element is not stored and we don't want to sum it
        fi;
        # conjugacy classes of elements having its p-th power conjugate to s
        sum := 0;
        for l in positions_summands do
          pos := Position(positions_in_CT[Size(positions_in_CT)], l);
          sum := sum + pa_u[pos];
        od;
        # sum := Sum(pa_u{List(Positions(pm,s),x -> positions_in_CT[Size(positions_in_CT)],x)});
        if sum mod p <> e_s mod p then
          violation := true;
          #Print(p, " ", j, " ", s, "\n"); 
          break;
        fi;
      od;
      if violation then
        return false;
        break;
      fi;
    fi;
  od;
od;
return true;
end);


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


###########################################################################################################
BindGlobal("HeLP_INTERNAL_CompatiblePartialAugmentations", function(pa_powers, k)
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
      Print("There are infinitely many solutions for elements of order ", d, ", HeLP stopped.  Try with more characters.\n");
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

##############################################################################################################

BindGlobal("HeLP_INTERNAL_VerifySolution", function(C, k, list_paraugs)
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
  W := HeLP_INTERNAL_MakeSystem(chars, k, C, pa{[1..Size(pa)-1]});
  if W = "infinite" then
    return "infinite";
  fi;
  W := HeLP_INTERNAL_DuplicateFreeSystem(W[1], W[2]);
  mu := 1/k*(W[1]*pa[Size(pa)] + W[2]);
  if HeLP_INTERNAL_IsIntVect(mu) and not false in List(mu, x -> SignInt(x) > -1) then
    Add(asol, pa);
  fi;
od;
return asol;
end);

#E
