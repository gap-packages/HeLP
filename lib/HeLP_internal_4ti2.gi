# only function which uses 4ti2 directly. This file is only loaded when the 4ti2Interface package is loaded
HeLP_settings[1] := "4ti2"; #for the moment this file is loaded by the package

InstallGlobalFunction("HeLP_INTERNAL_TestSystem_4ti2",  function(T,a,k,pa)
# Arguments: A matrix, a vector, the order of the unit, the partial augmentations of the proper powers of the unit
# Output: The possible partial augmentations given by the HeLP-method for this partial augmentations of the powers. 
# This function is internal.
# It relies on the 4ti2-interface and program. 
local solutions, v, mue, intersol, Tscaled, ascaled, temp, T_temp, D, t, s, solext, uneq, interintersol, temptemp, HB, DoVertices;

if HeLP_settings[2] then        # if wished, use redund first to minimize the system
  temp := HeLP_INTERNAL_Redund(T,a);
  if temp = "nosystem" then
    return [ ];
  fi;
else                      # redund is not used
  temp := HeLP_INTERNAL_DuplicateFreeSystem(T, a);      # remove multiple times occuring inequalities
fi;

solutions := 4ti2Interface_zsolve_equalities_and_inequalities([ListWithIdenticalEntries(Size(T[1]), 1)], [1], temp[1], -temp[2] : precision := HeLP_settings[3]);
# there are infinitely many solutions if there is a base-point, i.e. solutions[1] <> [], and there are translations,
# i.e. solutions[2] <> [] or solutions[3] <> [] s.t. T*x + a is integral with x = b + \sum l_c v_c 
# (b \in solutions[1], v_c \in \solutions[2] and l_c non-negative integers (w.l.o.g. l_c < k)).
if solutions[1] = [ ] then       # No solutions at all
  return [ ];
elif solutions[2] = [ ] and solutions[3] = [ ] then # finitely many solutions
  intersol := [ ];
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
end);
