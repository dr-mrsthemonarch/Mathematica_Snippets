(*Credit Required*)

multiTaylor[f_, {vars_?VectorQ, pt_?VectorQ, n_Integer?NonNegative}] :=
  Sum[Nest[(vars - pt) . # &, (D[f, {vars, \[FormalK]}] /. 
      Thread[vars -> pt]), \[FormalK]]/\[FormalK]!, {\[FormalK], 0, 
   n}, Method -> "Procedural"]

f[\[Phi], \[Phi]', \[Phi]''] := 7/3 m l^2 \[Phi]'' + 5/4 d l^2 Cos[\[Phi]]^2 \[Phi]' + 5/4 c l^2 Sin[\[Phi]] Cos[\[Phi]] - 5/8 c l^2 Cos[\[Phi]]

multiTaylor[f[\[Phi], \[Phi]', \[Phi]''], {{\[Phi], \[Phi]', \[Phi]''}, {\[Pi]/6,0, 0}, 1}]

x[\[Phi]1, \[Phi]1'', \[Phi]2] := 3 m l^2 \[Phi]1'' + 3 m g l^2 Sin[\[Phi]1] + l^2 c1 Sin[\[Phi]1] Cos[\[Phi]1] - l^2 c2 (Sin[\[Phi]2] - Sin[\[Phi]1]) Cos[\[Phi]1]

r[\[Phi]2, \[Phi]2'', \[Phi]1] := 2 m l^2 \[Phi]2'' + 2 m g l^2 Sin[\[Phi]2] + l^2 c3 Sin[\[Phi]2] Cos[\[Phi]2] - l^2 c2 (Sin[\[Phi]2] - Sin[\[Phi]1]) Cos[\[Phi]2]

multiTaylor[x[\[Phi]1, \[Phi]1'', \[Phi]2], {{\[Phi]1, \[Phi]1'', \[Phi]2}, {0, 0, 0}, 1}]

Together[multiTaylor[r[\[Phi]2, \[Phi]2'', \[Phi]1], {{\[Phi]2, \[Phi]2'', \[Phi]1}, {0, 0, 0}, 1}]]
