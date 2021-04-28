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
Manipulate[SeedRandom[seed];

(* prevents out-of-range errors for currentSteps *)
If[currentSteps > maxSteps, currentSteps = maxSteps];



(* setup code and definitions *)
colorList=Table[Hue[0.25+i/maxSteps],{i,1,maxSteps}];

irrationals = {Pi, E, GoldenRatio -> "\[CurlyPhi]", Pi^2, Log[2] -> "ln(2)", Sqrt[2], Sqrt[3]};

allPoints=ReIm /@ twistExpSumList[maxSteps,b,p,q];

currentPoints = Take[allPoints, maxSteps - currentSteps + 1];

plotlabel=Row[{"Sum-of-Digit Fractal with b = ",b,", p = ",p,", q = ",q}];

(* plot range scales with animation or stays fixed, depending on user choice *)
maxRange=If[scaleBoolean,
1.1*Max[Abs[allPoints]],
1.1*Max[Abs[currentPoints]]
];

backgroundColor=If[colorBoolean,Black,White];



(*graphics options*)
Graphics[{Thick,
If[colorBoolean, 
Line[currentPoints,VertexColors->colorList], 
Line[currentPoints]
]},

Axes->axesBoolean,

ImageSize->Large,

Background->backgroundColor,

PlotRange->{{-maxRange,maxRange},{-maxRange,maxRange}},

PlotLabel-> Style[plotlabel, 15, If[colorBoolean, Yellow, Black]]
],



(* user interface options *)
{{maxSteps, 1000, "steps"}, {100, 1000, 3000, 10000}, SetterBar},

{{colorBoolean, True, "color"}, {True, False}, Checkbox},

{{axesBoolean, True, "show axes"}, {True, False}, Checkbox},

{{scaleBoolean, False, "fixed scale"}, {True, False}, Checkbox},

Control@{{currentSteps,1,"animate"},maxSteps,1,-1,Trigger},


Delimiter,
(* control for b *)
Style["base", Bold],
{{b, 3, ""},2,12,1,SetterBar},


Delimiter,
Style["parameter p", Bold],
(* controls for p *)
{{p, 5, ""},2,12,1,SetterBar},
{{p, Pi, ""}, irrationals, SetterBar},
Row[{Spacer[55], Button[Style["Randomize", FontColor -> Black], seed++;p=RandomReal[{3, 10}],ImageSize -> 100]}],


Delimiter,
Style["parameter q", Bold],
(* controls for q *)
{{q, 3, ""}, 1,12,1,SetterBar},
{{q, Pi, ""}, irrationals, SetterBar},
Row[{Spacer[55], Button[Style["Randomize", FontColor -> Black], seed++;q=RandomReal[{3, 10}],ImageSize -> 100]}],

{seed,1,1000,1,ControlType -> None}
];


mastermanip[]
