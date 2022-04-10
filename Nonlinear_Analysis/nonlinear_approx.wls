dir = NotebookDirectory[];
SetDirectory[dir];

swingphi = 
  Flatten@
   Import["/Users/morbo/stuffs/mathematica_analysis/swingphi.mat", 
    "Data"];
data = Take[swingphi, {155, Length@swingphi}];
sdata = MapAt[(#) + 0.062 &, data, All];
fdata = Table[{(i - 1) 20 10^-3, sdata[[i]]}, {i, 1, Length@data}];
ListLinePlot[fdata, ImageSize -> Large]
\[CapitalDelta]t = fdata[[2, 1]] - fdata[[1, 1]];
timestart = fdata[[1, 1]];
timestop = fdata[[Length[data], 1]];


corrdata = Table[fdata[[i, 2]], {i, 1, Length[fdata]}];
corr = ListCorrelate[Take[corrdata, Quotient[Length[corrdata], 2]], 
   corrdata];
ListPlot[corr, ImageSize -> Large]

gaps = Length /@ 
   Rest@Most@Split[UnitStep@Differences[corr], #1 <= #2 &];
per = Mean[N[gaps]] \[CapitalDelta]t;
params = {mb -> (1.617 - 0.124), mw -> 0.124, l -> 0.097, g -> 9.81`, 
   k -> 0.106, jw -> 0.00028331372879321354`};
inertia = \[ScriptJ] /. 
   First[
    Solve[{((mb l + mw k) g )/(\[ScriptJ] + 
         jw) == (2 \[Pi] 1/per)^2}, \[ScriptJ]] /. params];
Solve[Sqrt[(g ( mb l + mw k ))/(\[ScriptJ] + jw)] == 
   2 \[Pi]/per, \[ScriptJ]] /. params

fa = 2/Sqrt[Length@fdata[[All, 2]]] Abs@Fourier[fdata[[All, 2]]];
peaksize = Last[TakeLargest[fa, 2]];
peaks = Flatten[Position[fa, x_ /; x >= peaksize]];
pos = First[peaks];
n = Length@fdata[[All, 2]];

ListPlot[fa, Filling -> Axis, 
 DataRange -> {0, (n - 1)/Last@fdata[[All, 1]]}, 
 PlotRange -> {{-0.2, 4}, All}, 
 PlotStyle -> {{Red, PointSize[0.011]}}, ImageSize -> Large, 
 PlotLabel -> "Spectral Analysis", FrameLabel -> {"hz", "A"}, 
 Frame -> True, FrameStyle -> Black]

fr = Abs[
   Fourier[fdata Exp[2 Pi I (pos - 2) N[Range[0, n - 1]]/n], 
    FourierParameters -> {0, 2/n}]];
frpos = Position[fr, Max[fr]][[1, 1]];
period = N[n/(pos - 2 + 2 (frpos - 1)/n)] \[CapitalDelta]t;
Solve[Sqrt[(g ( mb l + mw k ))/(\[ScriptJ] + jw)] == 2 \[Pi]/T, 
  l] /. {T -> period, mb -> (1.617 - 0.124), mw -> 0.124, g -> 9.81`, 
  k -> 0.106, 
  jw -> 0.00028331372879321354`, \[ScriptJ] -> 0.019952423487442736`}

physpendulum = 
  ParametricNDSolveValue[{(\[CapitalTheta] + jw) x''[
         t] + (mb l + mw k) g  Sin[
         x[t]] == -\[Mu] (Tanh[x'[t]/0.1]) - rb x'[t] /. params, 
    x[0] == fdata[[1, 2]], x'[0] == 0 }, 
   x, {t, timestop + 1}, {\[Mu], \[CapitalTheta], rb}];

Manipulate[
 Plot[#[t] & @ physpendulum[\[Mu], \[CapitalTheta], rb] , {t, 0, 
   timestop + 1}, Epilog -> {Red, Point[fdata]}, 
  ImageSize -> {GoldenRatio 600, 600}, PlotPoints -> 50, 
  PlotRange -> {{0, timestop + 1}, {-0.11, 0.11}}, PlotPoints -> 200, 
  Frame -> True, FrameStyle -> Black], {{\[Mu], 0.0001}, 1 10^-5, 
  1 10^-3, Appearance -> "Labeled"}, {{\[CapitalTheta], inertia}, 
  inertia - inertia/2, 5*(inertia), 
  Appearance -> "Labeled"}, {{rb, 0.00804}, 1 10^-5, 1 10^-1, 
  Appearance -> "Labeled"}]

nmodel[\[ScriptJ]_?NumberQ, \[Mu]_?NumberQ, 
  rb_?NumberQ] :=  (nmodel[\[ScriptJ], \[Mu], rb] = 
   Module[{x, t}, 
    First[
     x /. 
      NDSolve[{(\[ScriptJ] + jw) x''[t] + (mb l + mw k) g  Sin[
             x[t]] == -\[Mu] (ArcTan[x'[t]/0.1]) - rb x'[t] /. {mb -> 
           1.617 - 0.124, mw -> 0.124, g -> 9.81`, k -> 0.106, 
          l -> l -> 0.09927955263574209`, 
          jw -> 0.00028331372879321354`}, x[0] == fdata[[1, 2]], 
        x'[0] == 0 }, x, {t, 0, Last@fdata[[All, 1]]}]]])

nfit = NonlinearModelFit[fdata, 
  nmodel[\[ScriptJ], \[Mu], rb][
   t], {{\[ScriptJ], 0.019}, {\[Mu], 0.0001}, {rb, 0.00080497}}, t, 
  Method -> "Gradient"]
nfit["BestFitParameters"]

Plot[{nfit[t]}, {t, 0, Last@fdata[[All, 1]]}, 
 Epilog -> {Black, Point[fdata]}, PlotPoints -> 50, 
 PlotRange -> {{0, timestop + 1}, {-0.12, 0.12}}, PlotPoints -> 200, 
 Frame -> True, FrameStyle -> Black, 
 PlotLabel -> "Nonlinear Model Fit", 
 FrameLabel -> {"t [s]", "\[Alpha] [rad]"}, ImageSize -> Large]
