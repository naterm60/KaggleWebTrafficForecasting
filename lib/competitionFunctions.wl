(* ::Package:: *)

(* ::Section:: *)
(*Constants*)


DATADIR = ParentDirectory[NotebookDirectory[]] <> "\\data\\";
NULLVALUE = -128.;


(* ::Section:: *)
(*Import data*)


parseLine[in_] := Block[{stringEnd,page,numbers},

	(* Get the end position of the page string *)	
	stringEnd = StringPosition[in, "\","];
	If[Length[stringEnd]>1, 
		Print["Error in parseLine: >1 string ends of form \", were found"];
		Abort[];
	,
		stringEnd = stringEnd[[1,1]]-1;
	];

	(* Separate page and numbers *)
	page = StringTake[in, {2, stringEnd}];
	numbers = StringTake[in, {stringEnd+2, -1}];

	(* Make explicit nulls *)
	numbers = StringReplace[numbers, ",," -> ",Null,"];
	If[StringTake[numbers, -1]==",", numbers = numbers <> "Null"];

	(* Split on commas *)
	numbers = StringSplit[numbers, ","];

	(* Convert C scientific notation to a format Mathematica will understand *)
	numbers = Map[StringReplace[#, {"e+" :> "*^", "e-" :> "*^-"}]&, numbers];

	(* Convert to list of numbers *)
	numbers = Map[ToExpression, numbers];

	(* Null values will be denoted by -128 *)
	(* That should not collide with data, since page hits are \[GreaterEqual]0 and logp1 page hits are \[GreaterEqual] Log 1 *)
	numbers = ReplaceAll[numbers, Null->NULLVALUE];
	
	{page, numbers}
];


importTraffic[file_] := Block[{stream,colnames,dates,dat},

	stream = OpenRead[file];

	colnames = StringSplit[ReadLine[stream], ","];
	dates = colnames[[2;;-1]];

	dat = ReadList[stream, Record];

	dat = Map[parseLine, dat];

	Close[stream];

	{dat[[All,1]], dates, N[dat[[All,2]]]}

];


importKeys[file_] := Block[
	{stream,colNames,keys},

	(* Open file and get column names *)
	stream = OpenRead[file];
	colNames = ReadLine[stream];
	
	(* Get remaining lines *)
	keys = ReadList[stream, Record];
	
	(* Drop first quotation mark *)
	keys = Map[StringTake[#, {2,-1}]&, keys];
	
	(* Split on second quotation mark and comma *)
	keys = Map[StringSplit[#, "\","]&, keys];
	
	(* Make association *)
	keys = Map[#[[1]]->#[[2]]&, keys];
	keys = Apply[Association, keys];
	
	(* Finish *)
	Close[stream];
	keys
];


exportSubmission[testPredictions_, pages_, keys_, trainEndDate_, offset_, end_] := Block[
	{x,testDates,stream,i,j,page,date,key,id,visits},

	(* Get dates for test date range *)
	x = DatePlus[DateObject[trainEndDate], offset-1];
	testDates = Table[x = DatePlus[x,1]; x, {i,1,end-offset+1}];
	testDates = Map[DateString[#, {"Year", "-", "Month", "-", "Day"}]&, testDates];
	Print["Dates from " <> testDates[[1]] <> " to " <> testDates[[-1]]];
	
	(* Open stream *)
	stream = OpenWrite[NotebookDirectory[] <> "submission " 
		<> DateString[Riffle[{"Year","Month","Day","Hour","Minute","Second"}, "-"]] 
		<>  ".csv"
	];

	(* Write column label line *)
	WriteLine[stream, "Id,Visits"];

	(* Export page views *)
	For[i=1, i<=Length[testPredictions], i++,
		For[j=1, j<=Length[testDates], j++,
			(* Get Id *)
			page = pages[[i]];
			date = testDates[[j]];
			key = page <> "_" <> date;
			id = keys[[key]];
			
			(* Get visits *)
			visits = testPredictions[[i,j]];
			visits = Round[visits, 10.^-3];
			(* Convert to string without scientific notation *)
			visits = ToString[NumberForm[visits, Infinity, ExponentFunction->(Null&)]];
		
			(* Write line *)
			WriteLine[stream, id <> "," <> visits];
		];
	];
	
	(* Close stream *)
	Close[stream];

];


(* ::Section:: *)
(*Sample history (get dataset with random stop days)*)


sampleHistory[views_, fitPars_] := Block[
	{dims,stopDays,from,length,i,to},
	
	dims = Dimensions[views];
	
	stopDays = RandomInteger[{480, dims[[2]]-fitPars[["end"]]-1}, dims[[1]]];

	from = Reap[
		For[i=1, i<=dims[[1]], i++,
			Sow[views[[i, 1;;stopDays[[i]]]]];
		];
	];
	from = from[[2,1]];
	
	(* Make a regular array *)
	length = Max[Map[Length, from]];
	from = Map[PadLeft[#, length, NULLVALUE]&, from];

	to = Reap[
		For[i=1, i<=dims[[1]], i++,
			Sow[views[[i, stopDays[[i]]+fitPars[["offset"]] ;; stopDays[[i]]+fitPars[["end"]]]]];
		];
	];
	to = to[[2,1]];

	{from, to}
];


(* ::Section:: *)
(*Objective functions*)


smape = Compile[{{predicted, _Real, 2}, {actual, _Real, 2}}, Module[
	{
		out, i, j, p, a
	},

	out = 0.;
	For[i=1, i<=Length[predicted], i++,
		For[j=1, j<=Length[predicted[[1]]], j++,
			p = predicted[[i,j]];
			a = actual[[i,j]];
			
			If[p>=0 && a>=0 && (Abs[p]+Abs[a])>10.^-5,
				out += Abs[p-a]/(Abs[p]+Abs[a]);		
			];
		];
	];

	out * 200 / Length[predicted] / Length[predicted[[1]]]

], CompilationTarget->"C"];


smapeRows = Compile[{{predicted, _Real, 2}, {actual, _Real, 2}}, Module[
	{
		out, x, i, j, p, a
	},

	out = Table[0., {Length[predicted]}];

	For[i=1, i<=Length[predicted], i++,
		x = 0.;
		For[j=1, j<=Length[predicted[[1]]], j++,
			p = predicted[[i,j]];
			a = actual[[i,j]];

			If[p>=0 && a>=0 && (Abs[p]+Abs[a])>10.^-5,
				x += Abs[p-a]/(Abs[p]+Abs[a]);		
			];
		];
		out[[i]] = x;
	];

	out * 200 / Length[predicted[[1]]]

], CompilationTarget->"C"];


(* ::Section:: *)
(*Fast median regression*)


polyBasis[order_, len_] := Block[{basis},

	(* Get basis functions *)
	(* Constant *)
	basis = {Table[1., {len}]};  
	(* First-order *)
	If[order>0, AppendTo[basis, Range[len]-1-(len-1)/2]];
	(* Second and higher order *)
	If[order>1,
		basis = Join[basis, Table[basis[[-1]]^o, {o, 2, order}]]
	];

	(* Orthonormalize basis functions*)
	basis = Orthogonalize[basis]
];


residualToAdjustedSign = Compile[{{residual, _Real, 1}}, Module[
	{sign,all,above,below,justAbove,justBelow,diff},

	(* Get the sign of the residuals*)
	sign = Sign[residual]+0.;

	(* For the datapoints immediately above and below the current approximation,
		replace the sign with magnitudes that help the procedure converge
		to the mean of the nearest data points. *)
	all = Range[Length[residual]];
	above = Select[all, residual[[#]]>0&];
	below = Select[all, residual[[#]]<0&];
	If[above!={} && below!={},
		justAbove = above[[Ordering[residual[[above]],1]]];
		justBelow = below[[Ordering[residual[[below]],-1]]];
		diff = residual[[justAbove]]-residual[[justBelow]];
		If[diff[[1]]>10.^-8,
			sign[[justAbove]] = residual[[justAbove]] / diff;
			sign[[justBelow]] = residual[[justBelow]] / diff;
		];
	];

	sign

], CompilationTarget->"C"];


fastMedianRegression = Compile[{{data, _Real, 1}, {basis, _Real, 2}, {basisInverse, _Real, 2}, {convergenceTolerance, _Real}}, Module[
	{
		coefficients,portionNew,lastUpdate,i,approximation,
		residual,sign,data2,coefficientsNew,thisUpdate
	},

	(* Model: coefficients.basis = data *)
	
	(* Estimate the coefficients *)
	coefficients = data.basisInverse;

	(* Initialize variables for tracking convergence *)
	portionNew = 0.3;
	lastUpdate = Table[0., {Length[coefficients]}];
	
	For[i=1, i<=500, i++,

		approximation = coefficients.basis;
		
		(* Get the residual
			Note that the data doesn't change.  
			This is where the data influences the estimate. *)
		residual = data-approximation;
		
		(* Get the sign of the residuals *)
		sign = residualToAdjustedSign[residual];
		sign *= StandardDeviation[residual];  (* Converges faster *)
					
		(* Add sign to the approximation *)
		data2 = approximation + sign;

		(* Estimate the coefficients again.  
			Since we replaced errors with signs, the new approximation 
			will tend to distribute points evenly above and below 
			the approximation *)
		coefficientsNew = data2.basisInverse;

		(* Change the solution gradually, which helps it converge *)
		coefficientsNew = (1-portionNew) coefficients + portionNew coefficientsNew;

		(* Get the direction of change in coefficients *)
		thisUpdate = coefficientsNew - coefficients;

		(* Break if update is small enough *)
		If[Norm[thisUpdate]<convergenceTolerance, Break[]];
		
		(* Set coefficients after Break[], so that algorithm is capable 
			of returning ordinary least squares estimates *)
		coefficients = coefficientsNew;
		
		(* If update has reversed direction, reduce the descent rate *)
		If[Flatten[thisUpdate].Flatten[lastUpdate]<0,
			portionNew /= 2;
		];

		(* Get ready for next iteration *)
		lastUpdate = thisUpdate;
		
	];

	coefficients

], 
	CompilationTarget->"C", 
	RuntimeAttributes->{Listable}, Parallelization->True, 
	CompilationOptions->{"InlineExternalDefinitions" -> True}
];


(* ::Section:: *)
(*Comparison plot*)


comparisonPlot[from_, toPred_, toActual_] := Block[{},
	
	ListPlot[
		Log[{
			from, 
			Join[Table[Null, {Length[from]}], toActual],
			Join[Table[Null, {Length[from]}], toPred]
		}+1]
	, 
		Joined->True, PlotRange->All
	]

];


(* ::Section:: *)
(*Ensemble plot*)


ensemblePlot[from_, toPred_] := Block[{},
	
	ListPlot[
		Log[
			Join[{from}, 
				Map[Join[Table[Null, {Length[from]}], #]&, toPred]
			]
		+1]
	, 
		Joined->True, PlotRange->All, ImageSize->Full
	]

];
