(*The white noise function(s)*)

whitenoise[\[Delta]_, t_] := \[Delta] RandomReal[{-1, 1}]/Sqrt[t]
morenoise[\[Sigma]_, \[Delta]_, 
  t_] := \[Sigma]^2 \[Delta] RandomReal[{-1, 1}]/Sqrt[t]
(*Change noise values yourself!*)
Manipulate[
 Plot[{Sin[t 5]/2.5 + morenoise[b, b, 0.1], 
   Sin[t 5]/5 + whitenoise[b, 0.1]}, {t, 0, \[Pi]/4}, 
  ImageSize -> Large], {b, 0.001, 1}]
(*Generate a gif to see the growing noise*)

tmax = 0.1;
imgtable = 
   Table[
    Plot[{Sin[t 5]/5, Sin[t 5]/5 + whitenoise[b, 1]}, {t, 0, \[Pi]/4},
      ImageSize -> Large], {b, 0.001, tmax, .001}]; ~Monitor~b

Export["test.gif", imgtable, "AnimationDuration" -> 15]; (*Export to your home folder*)

(*Something fun*)

f[k1_, k2_] := Exp[I 0.75 (k1 + 0.3 k2)] + 1.5 Exp[I (0.2 k1 + 0.3 k2)] + 
   Exp[I (0.2 k1 + 0.4 k2)];

data1 = Table[{Re[f[i, i + 0.1]], Im[f[i, i + 0.1]]}, {i, 0, 12000}];
data2 = Table[{Re[f[i, i + 0.1]] + whitenoise[0.01, 0.1], Im[f[i, i + 0.1]] + whitenoise[0.01, 0.1]}, {i, 0, 12000}];

ListPlot[{data1, data2}, ImageSize -> Large]
