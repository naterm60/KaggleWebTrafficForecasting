(* ::Package:: *)

(* ::Section:: *)
(*Cross validation *)


crossValidate[xIn_, yIn_, fitPars_, indicatorFunc_, fitterFunc_, scoreFunc_, numFolds_] := Block[
	{
		x,y,numDatasets,numDatapoints,partBoundaries,partRanges,i,scores,all,
		randomizer,derandomizer,
		predictions,test,testPart,statusCell,
		train,trainPart,xTrain,xTest,yTrain,yTest,predictor,pred,score,
		log,stream,logFile
	},

	numDatapoints = Length[xIn];
	x = indicatorFunc[xIn, fitPars];
	y = yIn;

	(* Randomize the order of datapoints, in case the dataset is in some order *)
	randomizer = RandomSample[Range[numDatapoints], numDatapoints];
	derandomizer = InversePermutation[randomizer];
	x = x[[randomizer]];
	y = y[[randomizer]];

	(* Get the part boundaries that will be used to take different parts of the dataset *)
	partBoundaries = Round[Range[1., numDatapoints, (numDatapoints-1)/numFolds]];

	partRanges = Table[
		partBoundaries[[{i,i+1}]] + If[i==1, {0,0}, {1,0}]
	,
		{i,1,Length[partBoundaries]-1}
	];

	scores = {};
	all = Range[numDatapoints];
	predictions = {};
	cvStatusInternal = "";
	statusCell = PrintTemporary[Dynamic[cvStatusInternal]];
	For[test=1, test<=numFolds, test++,
		cvStatusInternal = StringJoin["Cross validation part ", ToString[test], " of ", ToString[numFolds]];

		(* Get the part range for testing *)
		testPart = Take[all, partRanges[[test]]];

		(* Get the part range for training*)
		train = Drop[Range[numFolds], {test}];
		trainPart = Flatten[Map[Take[all, #]&, partRanges[[train]]]];

		(* Get those parts of the dataset *)
		xTrain = x[[trainPart]];
		xTest = x[[testPart]];
		yTrain = y[[trainPart]];
		yTest = y[[testPart]];

		(* Fit, predict, get score *)
		predictor = fitterFunc[xTrain, yTrain];
		pred = predictor[xTest];
		score = scoreFunc[pred, yTest];

		(* Store the predictions and the score *)
		predictions = Join[predictions, pred];
		AppendTo[scores, score];
	];

	NotebookDelete[statusCell];

	(* Log result *)
	log = Join[
		<|
			"dateTime" -> DateString[], 
			"scores" -> scores, 
			"numSamples" -> Length[xIn],
			"numFolds" -> numFolds
		|>
	, 
		fitPars
	];
	logFile = DATADIR <> "CVlog.txt";
	stream = OpenAppend[logFile];
	Write[stream, log];
	Close[logFile];

	{scores, predictions[[derandomizer]]}
];


(* ::Section:: *)
(*Grid search*)


gridSearch[objectiveFunction_, varNamesRanges_] := Block[
	{varNames,varRanges,rangeLengths,tuples,out,t,replacements,val,pos,format,grid,columnHeader,rowHeader},

	{varNames, varRanges} = Transpose[varNamesRanges];
	rangeLengths = Map[Length, varRanges];
	tuples = Tuples[Map[Range, rangeLengths]];
	out = ConstantArray[10^15., rangeLengths];

	Table[
		replacements = Table[varNames[[i]]->varRanges[[i, t[[i]]]], {i,1,Length[varRanges]}];
		val = ReleaseHold[ReplaceAll[objectiveFunction, replacements]];
		Print[replacements, "   result: ", val];
		pos = Apply[Sequence, t];
		out[[pos]] = val;
	,
		{t, tuples}
	];

	format[var_] := Item[Style["Variable " <> ToString[var], Underlined, Bold], Alignment->{Center,Center}];
	
	Switch[Length[varNames]
		,1,
		
			grid = Map[Item[#, Frame->True]&, out];
			grid = Grid[{
				{format[varNames[[1]]], SpanFromLeft}, 
				varRanges[[1]],
				grid
			}, 
				ItemSize->All, Alignment->Center
			];
		
		,2,
		
			grid = Map[Item[#, Frame->True]&, out, {2}];
		
			columnHeader = {
				PadRight[{format[varNames[[2]]]}, Length[varRanges[[2]]], SpanFromLeft],
				varRanges[[2]]
			};
			grid = Join[columnHeader, grid];

			rowHeader = {
				PadRight[{"", "", format[varNames[[1]]]}, Length[varRanges[[1]]]+2, SpanFromAbove],
				Join[{"",""}, varRanges[[1]]]		
			};
			grid = Transpose[Join[rowHeader, Transpose[grid]]];
		
			grid = Grid[grid, ItemSize->All, Alignment->Center];
		
		,_,
			grid = out;
	];

	grid
];


(*ofunc = "x"^2
gridSearch[ofunc, {{"x", {1,2,3}}}]*)


(*ofunc = ("x"+"y")^2
gridSearch[ofunc, {{"x", {1,2,3}}, {"y", {3,4,5}}}]*)


(* ::Section:: *)
(*Line search*)


Needs["ErrorBarPlots`"];

lineSearch[name_, values_] := Block[{fp,i,r,errorBars},

	fp = fitPars;
	res = {};
	For[i=1, i<=Length[values], i++,
		fp = Join[fp, <| name -> values[[i]]|>];
		r = crossValidate[from, to, fp, getIndicators, modelFit[#1, #2, fp]&, smape, 10][[1]];
		AppendTo[res, {Mean[r], StandardDeviation[r]}];
		Print[i, "/", Length[values], "    ", name, "=", values[[i]], "    ", res[[-1]]];
	];
	
	Beep[];
	
	ListPlot[Transpose[{values, res[[All,1]]}], Frame->True, FrameLabel->{name, "score"}]

];
