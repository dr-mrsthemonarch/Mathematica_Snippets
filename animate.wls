(*Different ways to animate a dot following a curve*)

Dynamic@Plot[Sin[t] + 1/6 Sin[3 t], {t, 0, 2 \[Pi]}, ImageSize -> "Large", Mesh -> {{Clock[{0, 2 \[Pi]}, 5, 2]}}, MeshStyle -> Directive[PointSize[Large], Red]]

s[t_] := Sin[t] + 1/6 Sin[3 t];

Animate[Show[Plot[s[t], {t, 0, 2 \[Pi]}, PlotRange -> {{0, 2 \[Pi]}, {-1, 1}}, ImageSize -> Large, PlotTheme -> "Business"], Graphics[{PointSize[0.02], Red, Point[{u, s[u]}]}]], {u, 0, 2 \[Pi],0.1}]

Animate[Graphics[Arrow[{{0, 0}, {0.9 Cos[t], 0.9 Sin[t]}}], Axes -> True, PlotRange -> {{-1, 1}, {-1, 1}}], {t, 0, 3}]
