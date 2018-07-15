(* ::Package:: *)

(* ::Section:: *)
(*Tell Mathematica what C compiler and options to use*)


(* ::Subsection:: *)
(*Installation*)


(* ::Text:: *)
(*Search online for the MinGW 64 compiler download (it's currently on SourceForge), and then install it.*)
(*In the installer, select x86-64 compiler, win32 threads.*)
(*In C:\Program Files\mingw-w64\, find the mingw64 folder that contains bin, lib etc.*)
(*	If you've already installed mingw64 on another computer, then you can get it from there.*)
(*Copy that folder to C:\ and rename it to MinGW-64*)
(*	Note: something doesn't like the space in "Program Files".*)
(*Add C:\MinGW-64\bin\ to the PATH system variable*)
(*Create a new system variable named LIBRARY_PATH with value C:\MinGW-64\lib\*)
(*For Mathematica 11, copy C:\MinGW-64\bin\lib*.dll to *)
(*	C:\Program Files\Wolfram Research\Mathematica\11.1\SystemFiles\FrontEnd\Binaries\Windows-x86-64*)
(*	(or it's equivalent on your system)*)
(*For more info, see the "Specific Compilers" tutorial, section "MinGW for 64-Bit Targets".*)
(*To make compiled functions with the C compiler, use the option CompilationTarget->"C"*)


(* ::Subsection:: *)
(*Notes*)


(* ::Text:: *)
(*You can only use numeric functions and datatypes in your compiled functions, so no plots, associations, etc.*)
(*	That's not much of a limitation for most things that need a speedup.*)
(*Remember that your MMA code is compiling to early-bound code behind the scenes, and program with that in mind.*)
(*	Don't use a dynamic typing style of coding, where you reassign different types to the same variable.*)
(*	Instead, stick to the first type that was assigned to the variable.*)
(*	The same goes for array depths.  Lists must stay as lists, matrices as matrices, etc., though you can append or delete elements.*)
(*	If you don't follow this, you'll get errors when compiling.*)
(*Ideally, your code will compile to C with no calls back to Mathematica.*)
(*	Some Mathematica functions, however, have to be executed on a MMA kernel.  *)
(*	Avoid these functions if you can, since the overhead of calling MMA will slow your code way down.*)
(*	These functions tend to be more complex numeric ones.  There's a way to find them in your compiled function.  I forgot how, search for it.*)
(*Debugging: sometimes MMA will still get confused by your code, refuse to compile it, and not give helpful messages.*)
(*	Then you'll have to debug by:*)
(*		Commenting out sections of your code and bringing them back until the problem returns*)
(*		Searching online for the problem*)
(*		Setting "ShellOutputFunction" -> Print*)
(*Miscellanea*)
(*	Module[] works better than Block[] when reading/assigning global variables.*)
(*	If Mathematica's compiler doesn't read global variable tensors as tensors, pass them to the function as arguments.*)


(* ::Subsection:: *)
(*Initialization*)


Needs["CCompilerDriver`"]
Needs["CCompilerDriver`GenericCCompiler`"]
$CCompiler = {
	"Compiler" -> GenericCCompiler, 
	"CompileOptions" -> "-O3 -ftree-vectorize -msse2 -ffast-math", 
	"CompilerInstallation" -> "C:\\MinGW-64\\", 
	"CompilerName" -> "x86_64-w64-mingw32-gcc.exe"
};


(* ::Subsection:: *)
(*Test it (from MMA documentation)*)


(*greeter = CreateExecutable[StringJoin[
      "#include <stdio.h>\n",
      "int main(){\n",
      "  printf(\"Hello MinGW-w64 world.\\n\");\n",
      "}\n"],
    "hiworld", "ShellOutputFunction" -> Print]

Import["!\"" <> greeter <> "\"", "Text"]*)


(* ::Subsection:: *)
(*A function compiled with and without CompilationTarget->"C". (from MMA documentation)*)


(*(* With CompilationTarget\[Rule]"C" *)
c=Compile[ {{x,_Real},{n,_Integer}},
	Module[ {sum, inc},sum=1.0;inc=1.0;Do[inc=inc*x/i;sum=sum+inc,{i,n}];sum],CompilationTarget->"C"];
c[1.5, 10000000]//AbsoluteTiming*)


(*(* Without CompilationTarget\[Rule]"C" *)
c=Compile[ {{x,_Real},{n,_Integer}},
	Module[ {sum, inc},sum=1.0;inc=1.0;Do[inc=inc*x/i;sum=sum+inc,{i,n}];sum]];
c[1.5, 10000000]//AbsoluteTiming*)
