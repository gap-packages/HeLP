HeLP_settings[1] := "normaliz"; #for the moment this file is loaded by the package

InstallGlobalFunction("HeLP_INTERNAL_TestSystem_Normaliz",  function(T,a,k,pa)
# Arguments: A matrix, a vector, the order of the unit, the partial augmentations of the proper powers of the unit
# Output: The possible partial augmentations given by the HeLP-method for this partial augmentations of the powers. 
# This function is internal.
# It relies on the NormalizInterface its functions. 
local solutions, v, mue, intersol, Tscaled, ascaled, temp, T_temp, D, t, s, solext, uneq, interintersol, temptemp, HB, DoVertices;

if HeLP_settings[2] then        # if wished, use redund first to minimize the system
  temp := HeLP_INTERNAL_Redund(T,a);
  if temp = "nosystem" then
    return [ ];
  fi;
else                      # redund is not used
  temp := HeLP_INTERNAL_DuplicateFreeSystem(T, a);      # remove multiple times occuring inequalities
fi;

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
end);
