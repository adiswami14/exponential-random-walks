(* ::Package:: *)

(* irrational presets for p and q *)
irrationals = {Pi, E, GoldenRatio -> "\[Phi]", Pi^2, Log[2] -> "ln 2", Log[3]-> "ln 3", Sqrt[2], Sqrt[3]};


(* sum of digit function *)
s[n_, b_] := Total[IntegerDigits[n, b]];


(* plot points for fractal curve *)
expSumList[n_, b_, p_, q_] := Accumulate[Exp[2.0*Pi*I*(s[#, b]/p + #/q)]&/@Range[n]];


(* Master manipulate code *)

mastermanip[] :=


Manipulate[SeedRandom[seed];

(* prevents out-of-range errors for currentSteps *)
If[currentSteps > maxSteps, currentSteps = maxSteps];

(* generate complete list of plot points up to maxSteps *)
allPoints=ReIm/@expSumList[maxSteps,b,p,q];


(* for Trigger animation (runs in reverse) *)
currentPoints = Take[allPoints, maxSteps - currentSteps + 1];


colorList=Table[Hue[0.25+i/maxSteps],{i,1,maxSteps}];

backgroundColor=If[colorBoolean,Black,White];


plotlabel=Row[{"base b = ",b,", p = ",p,", q = ",q}];

(* plot range scales with animation or stays fixed, depending on user choice *)
maxRange=If[scaleBoolean,
1.1*Max[Abs[allPoints]],
1.1*Max[Abs[currentPoints]]
];


(***********************  fractal curve graphics ****************************)
Graphics[{Thick,
If[colorBoolean, 
Line[currentPoints,VertexColors->colorList], 
Line[currentPoints]
]},

Axes->axesBoolean,
ImageSize->350, (* adjust to fit *)
ImagePadding->5,
Background->backgroundColor,
PlotRange->{{-maxRange,maxRange},{-maxRange,maxRange}},
PlotLabel-> Style[plotlabel, 15, If[colorBoolean, Yellow, Black]]
],



(**********************   Controls **********************)
{{maxSteps, 1000, "steps"}, {100,300, 1000, 3000, 10000}, SetterBar},
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
{{p, 5, ""},1,11,1,SetterBar},
{{p, Pi, ""}, irrationals, SetterBar},
Row[{Spacer[55], Button[Style["Randomize", FontColor -> Black], seed++;p=RandomReal[{3, 10}],ImageSize -> 100]}],


Delimiter,
Style["parameter q", Bold],
(* controls for q *)
{{q, 3, ""}, 1,11,1,SetterBar},
{{q, Pi, ""}, irrationals, SetterBar},
Row[{Spacer[55], Button[Style["Randomize", FontColor -> Black], seed++;q=RandomReal[{3, 10}],ImageSize -> 100]}],
(* dummy seed variable, for the Randomize buttons *)
{seed,1,1000,1,ControlType -> None},

(************* Manipulate options ************************************)

(* required because of initialization code *)
SaveDefinitions -> True,
(* specifies which controls are run with Autorun *)
AutorunSequencing->{1,2,5,8}

]


mastermanip[]
