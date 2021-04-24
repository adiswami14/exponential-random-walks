(* ::Package:: *)

(* function definitions *)

sumIntDigits[n_, b_]:=Total[IntegerDigits[n, b]]
twistExpSumList[n_, b_, p_, q_] := Prepend[Accumulate[(Exp[2.0*Pi*I*(sumIntDigits[#, b]/p + (#/q))]&/@Range[n])], 0]
twistVisualizer[n_, b_, p_, q_]:=ListLinePlot[ReIm /@ twistExpSumList[n,b, p, q], PlotStyle->Thick]
expSumList[n_ ,b_, p_]:=Prepend[Accumulate[Exp[2.0*Pi*I*sumIntDigits[#, b]/p]&/@Range[n]], 0]
visualizer[n_, b_, p_]:=ListLinePlot[ReIm /@ expSumList[n,b, p], PlotStyle->Thick]
chunkVisualizer[n1_, n2_, b_, p_, c_]:=ListLinePlot[ReIm /@ Take[expSumList[n2,b, p], {n1, n2+1}], PlotStyle->{Thick, c}]
pointVisualizer[n_, b_, p_]:=ListPlot[ReIm /@ expSumList[n,b, p], PlotStyle->{PointSize[Large], Red}]


(* Master manipulate code *)

mastermanip[] :=
Manipulate[
SeedRandom[seed];
kmax=N[Log[2,maxSteps]];
If[k>kmax,k=kmax];(*out of range error trick*)
kdelta=0.1;
colorlist=Table[Hue[0.25+i/maxSteps],{i,1,maxSteps}];
currentsteps=Floor[2^k];
irr={Pi,E,GoldenRatio->"\[CurlyPhi]",Pi^2,Log[2]->"ln(2)",Sqrt[2],Sqrt[3]};
points=ReIm/@twistExpSumList[maxSteps,b,p,q];
currentpoints=Take[points,maxSteps-currentsteps+1];
maxRange=If[scale,1.1*Max[Abs[points]],1.1*Max[Abs[currentpoints]]];
bgcolor=If[color,Black,White];
labelcolor = If[color, Yellow, Black];
plotlabel = Row[{"Exponential Sum Walk with b = ", b, ", p = ", p, ", q = ", q}];

Graphics[
{Thick,If[color,Line[currentpoints,VertexColors->colorlist],
Line[currentpoints]]},
Axes->True,
ImageSize->Large,
Background->bgcolor,
PlotRange->{{-maxRange,maxRange},{-maxRange,maxRange}},
PlotLabel->Style[plotlabel,15,labelcolor]
],

{{maxSteps,1500},{100,1500,3000,5000},SetterBar},
Control@{{k,1,"animate"},kmax,1,-kdelta,Trigger},
{{color,True},{True,False},Checkbox},
{{scale,False,"fixed scale"},{True,False},Checkbox},

Delimiter,
Style["base", 13],
{{b,2,""},2,12,1,SetterBar},

Delimiter,
Style["control p", 13],
(* Controls for p *)
{{p,3, ""},2,12,1,SetterBar},
{{p, Pi, ""},irr,SetterBar},
Button[Style["Randomize p",FontColor->Black],seed++;p=RandomReal[{3,10}],ImageSize->200],

Delimiter,
Style["control q", 13],
(* Controls for q *)
Control@{{q,3, ""},2,12,1,SetterBar},
{{q,Pi, ""},irr,SetterBar},
Button[Style["Randomize q",FontColor->Black],seed++;q=RandomReal[{3,10}],ImageSize->200],
{seed,1,1000,1,ControlType->None},
ControlPlacement->Left
];


mastermanip[]
