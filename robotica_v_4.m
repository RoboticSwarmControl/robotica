(* ::Package:: *)


BeginPackage["robotica`"]
Off[Replace::rep]


DH::usage = ""
DH1::usage = ""
\[Mu]v::usage =""
\[Mu]w::usage =""
Q::usage = ""
r::usage = ""
a::usage = ""
d::usage = ""
\[Alpha]::usage = ""
\[Theta]::usage = ""
Type::usage = ""
Joint::usage = ""
jointtype::usage = ""
alpha::usage = ""
theta::usage = ""
thetac::usage = ""
alphac::usage = ""
dof::usage = ""
Td::usage = ""
x5::usage = ""
x4::usage= ""
cc::usage = ""
zz::usage = ""
xx::usage = ""
x3::usage = ""
ze::usage = ""
b::usage = ""
z::usage = ""
k::usage = ""
l::usage = ""
v::usage = ""
A::usage = ""
T::usage = ""
J::usage = ""
Jvw::usage = ""
z::usage = ""
o::usage = ""
M::usage = ""
MU::usage = ""
gravity::usage = ""
mass::usage = ""
com::usage = ""
inertia::usage = ""
Jc::usage = ""
Jvc::usage = ""
Jwc::usage = ""
c::usage = ""
TellFunctions::usage = ""

SetRanges::usage = "SetRanges[xrange, yrange, zrange] sets a consistent
range for viewlimits used during animations."

TPrint::usage = "TPrint[name_String:''] prints all T matrices to the
file 'name', or to screen if no name is given."

APrint::usage = "APrint[name_String:''] prints all A matrices to the
file 'name', or to screen if no name is given."

MPrint::usage = "MPrint[M_List, text_String, name_String:''] prints the
matrix/vector M in standard form with 'text' as a label.  Saved in
file 'name' if specified."

EPrint::usage = "EPrint[M_List, text_String, name_String:''] prints the
elements of M one per line, each with label 'text'.  Saved in file 'name' if
specified."


DataFile::usage = "DataFile[name_String:''] reads in a DH input file
from 'name' if given.  Otherwise prompt for a file and read it."

dhInput::usage = "dhInput[] lets the user enter the DH parameters, in a list of {joint_type,r,alpha,d,theta}."

createdh::usage = "create DH parameter by Given DOF"
createdh2::usage " Create DH Parameter Table by given DH Matrix"
PrintInputData::usage = "PrintInputData[] prints the current robot
data set in tabular form to the screen."

FKin::usage = "FKin[] generates the A and T matrices, as well as the Jacobian
for the current input data set."

drawZArrow::usage = ""
drawCoordAxes::usage = ""
drawJoint::usage = ""
drawShaft::usage = ""
drawGripper::usage = ""
dhTransform::usage = ""
drawRobot::usage = "drawRobot[] displays a manipulate window and the robot so users can adjust on joint parameters.
Optional parameters:
showArrows displays the coordinate axes,
showH writes the homogenous transform,
showManipEllipse-> False,
showPlanes displays a controller to show the xy plane at each axis (useful for inverse kinematics)
"


SimplifyTrigNotation::usage =
"SimplifyTrigNotation[] changes the notation of Cosine and Sine functions.
Cos[q1] --> C1,
Sin[q1] --> S1,
Cos[q1+q2] --> C12,
Sin[q1+q2] --> S12,
Cos[q1-q2] --> C1-2,
Sin[q1-q2] --> S1-2,
Cos[q1+q2+q3] --> C123,
Sin[q1+q2+q3] --> S123,
Cos[q1+q2-q3] --> C12-3,
Sin[q1+q2-q3] --> S12-3,
Sin[q1+q2+q3+q4] --> C1234,
Cos[q1+q2+q3+q4] --> C1234."


Begin["`Private`"]

 (*
   Here are some variables which define the overall state of the Robotica
   session.  They should be available to all the functions in the package.
 *)

$DATAFILE$ = "NO";
$dhInput$ = "NO";
$createdh$= "NO";
$FKINRUN$ = "NO";
$DYNRUN$ = "NO";
$LINK$ = Null;
$ENDEFF$=Null;
$XRANGE$ = {-10,10};
$YRANGE$ = {-10,10};
$ZRANGE$ = {-10,10};
$RANGES$ = {$XRANGE$, $YRANGE$, $ZRANGE$};
$FILE$="NOT_SET";
$VERSION$ = "4.01";
(*todo: make this draw robots again*)
(*Version 3.62 fixes TrigSimplify so it works with more than 2 arguments (up to 4).  Also formats the output of FKin*)
(* Version 3.61 is the same as 3.60, with a few small errors fixed
to squelch warnings which appear on newer versions of Mathematica. *)
(* Version 3.62 is the same as 3.61, with SimplifyTrigNotation[] modified to use subscripts. *)
(* Version 3.63 is the same as 3.62, with simplifications to formatting and a few extra *)
(*Version 4.00 by adding dhInput[], drawRobot[] and fixing the output for DataFile[]*)
$ROBGUY$ = "atbecker@uh.edu mmsultan@uh.edu";

Print["Robotica version ", $VERSION$, "."];


SimplifyTrigNotation[]:=
	Do[
	Unprotect[Cos];

	Format[Cos[x_]] := Subscript["c",StringJoin[Drop[Characters[ToString[x]],1]]]
           /; (Head[x] == Symbol) && (StringLength[ToString[x]] > 1);

	Format[Cos[x_ + y_]] :=
               Subscript["c",StringJoin[Drop[Characters[ToString[x]],1],
                              Drop[Characters[ToString[y]],1]]]
           /; (Head[x] == Symbol) && (StringLength[ToString[x]] > 1) &&
              (Head[y] == Symbol) && (StringLength[ToString[y]] > 1);

	Format[Cos[x_ - y_]] :=
               Subscript["c",StringJoin[Drop[Characters[ToString[x]],1],"-",
                              Drop[Characters[ToString[y]],1]]]
           /; (Head[x] == Symbol) && (StringLength[ToString[x]] > 1) &&
              (Head[y] == Symbol) && (StringLength[ToString[y]] > 1);

	Format[Cos[x_ + y_ + z_]] :=
               Subscript["c",StringJoin[Drop[Characters[ToString[x]],1],
							Drop[Characters[ToString[y]],1],
                              Drop[Characters[ToString[z]],1]]]
           /; (Head[x] == Symbol) && (StringLength[ToString[x]] > 1) &&
              (Head[y] == Symbol) && (StringLength[ToString[y]] > 1) &&
              (Head[z] == Symbol) && (StringLength[ToString[z]] > 1);

	Format[Cos[x_ + y_ - z_]] :=
               Subscript["c",StringJoin[Drop[Characters[ToString[x]],1],
                              Drop[Characters[ToString[y]],1],"-",
                              Drop[Characters[ToString[z]],1]]]
           /; (Head[x] == Symbol) && (StringLength[ToString[x]] > 1) &&
              (Head[y] == Symbol) && (StringLength[ToString[y]] > 1) &&
              (Head[z] == Symbol) && (StringLength[ToString[z]] > 1);


	Format[Cos[w_ + x_ + y_ + z_]] :=
               Subscript["c",StringJoin[Drop[Characters[ToString[w]],1],
							Drop[Characters[ToString[x]],1],
							Drop[Characters[ToString[y]],1],
							Drop[Characters[ToString[z]],1]]]
           /; (Head[w] == Symbol) && (StringLength[ToString[w]] > 1) &&
              (Head[x] == Symbol) && (StringLength[ToString[x]] > 1) &&
              (Head[y] == Symbol) && (StringLength[ToString[y]] > 1) &&
              (Head[z] == Symbol) && (StringLength[ToString[z]] > 1);

	Protect[Cos];

	Unprotect[Sin];
	Format[Sin[x_]] := Subscript["s",StringJoin[Drop[Characters[ToString[x]],1]]]
           /; (Head[x] == Symbol) && (StringLength[ToString[x]] > 1);

	Format[Sin[x_ + y_]] :=
               Subscript["s",StringJoin[Drop[Characters[ToString[x]],1],
                              Drop[Characters[ToString[y]],1]]]
           /; (Head[x] == Symbol) && (StringLength[ToString[x]] > 1) &&
              (Head[y] == Symbol) && (StringLength[ToString[y]] > 1);

	Format[Sin[x_ - y_]] :=
               Subscript["s",StringJoin[Drop[Characters[ToString[x]],1],"-",
                              Drop[Characters[ToString[y]],1]]]
           /; (Head[x] == Symbol) && (StringLength[ToString[x]] > 1) &&
              (Head[y] == Symbol) && (StringLength[ToString[y]] > 1);

	Format[Sin[x_ + y_ + z_]] :=
               Subscript["s",StringJoin[Drop[Characters[ToString[x]],1],
                              Drop[Characters[ToString[y]],1],
                              Drop[Characters[ToString[z]],1]]]
           /; (Head[x] == Symbol) && (StringLength[ToString[x]] > 1) &&
              (Head[y] == Symbol) && (StringLength[ToString[y]] > 1) &&
              (Head[z] == Symbol) && (StringLength[ToString[z]] > 1);

	Format[Sin[x_ + y_ - z_]] :=
               Subscript["s",StringJoin[Drop[Characters[ToString[x]],1],
                              Drop[Characters[ToString[y]],1],"-",
                              Drop[Characters[ToString[z]],1]]]
           /; (Head[x] == Symbol) && (StringLength[ToString[x]] > 1) &&
              (Head[y] == Symbol) && (StringLength[ToString[y]] > 1) &&
              (Head[z] == Symbol) && (StringLength[ToString[z]] > 1);

  Format[Sin[w_ + x_ + y_ + z_]] :=
               Subscript["s",StringJoin[Drop[Characters[ToString[w]],1],
              Drop[Characters[ToString[x]],1],
              Drop[Characters[ToString[y]],1],
              Drop[Characters[ToString[z]],1]]]
           /; (Head[w] == Symbol) && (StringLength[ToString[w]] > 1) &&
              (Head[x] == Symbol) && (StringLength[ToString[x]] > 1) &&
              (Head[y] == Symbol) && (StringLength[ToString[y]] > 1) &&
              (Head[z] == Symbol) && (StringLength[ToString[z]] > 1);

	Protect[Sin];
	]


`TrigCanonicalRel =
{
    Sin[x_]^n_. Cos[x_]^m_.	:> Tan[x]^n	/; n+m == 0,
    Sin[- a_. x_]       :>   - Sin[a x] ,
    Sin[r_Rational Pi]	:>   Sin[(r - 2 Floor[r/2]) Pi] /; r > 2,
    Cos[r_Rational Pi]	:>   Cos[(r - 2 Floor[r/2]) Pi] /; r > 2,
    Tan[r_Rational Pi]	:>   Tan[(r - 2 Floor[r/2]) Pi] /; r > 2,
    Sin[r_Rational Pi]	:> - Sin[(r - 1) Pi] /; r > 1,
    Cos[r_Rational Pi]	:> - Cos[(r - 1) Pi] /; r > 1,
    Tan[r_Rational Pi]	:>   Tan[(r - 1) Pi] /; r > 1,
    Sin[r_Rational Pi]	:>   Sin[(1 - r) Pi] /; r > 1/2,
    Cos[r_Rational Pi]	:> - Cos[(1 - r) Pi] /; r > 1/2,
    Tan[r_Rational Pi]	:> - Tan[(1 - r) Pi] /; r > 1/2,
    Sin[r_Rational Pi]	:>   Cos[(1/2 - r) Pi] /; r > 1/4,
    Cos[r_Rational Pi]	:>   Sin[(1/2 - r) Pi] /; r > 1/4

}


TrigCanonical[e_] := e //. TrigCanonicalRel

`TrigFactorRel = {
    a_. Sin[x_] + a_. Sin[y_] :> 2 a Sin[x/2+y/2] Cos[x/2-y/2],
    a_. Sin[x_] - a_. Sin[y_] :> 2 a Sin[x/2-y/2] Cos[x/2+y/2],
    a_. Cos[x_] + a_. Cos[y_] :> 2 a Cos[x/2+y/2] Cos[x/2-y/2],
    a_. Cos[x_] - a_. Cos[y_] :> 2 a Sin[x/2+y/2] Sin[y/2-x/2],
    a_. Tan[x_] + a_. Tan[y_] :> a Sin[x+y]/(Cos[x] Cos[y]),
    a_. Tan[x_] - a_. Tan[y_] :> a Sin[x-y]/(Cos[x] Cos[y]),

    a_. Sin[x_] Cos[y_] + a_. Sin[y_] Cos[x_] :> a Sin[x + y],
    a_. Cos[x_] Cos[y_] - a_. Sin[x_] Sin[y_] :> a Cos[x + y],
    a_. Cos[x_] Cos[y_] + a_. Sin[x_] Sin[y_] :> a Cos[x - y],
    a_. Cos[x_] Cos[y_ + z_] + a_. Sin[x_] Sin[y_ +z_] :> a Cos[x - y -z],
    a_. Sin[x_] Cos[y_] - a_. Sin[y_] Cos[x_] :> a Sin[x - y],
    (* Added by Fathi Ghorbel *)
    a_. Sin[x_]^2 + a_. Cos[x_]^2	      :> a

}

(*
   TPrint prints all the T Matrices to a file or to the screen.
   If the filename is $, the default file name is used
   TODO: display all the T matrices, not print them
*)
TPrint[name_String:""] :=
   Block[{i,j,temp,file},
     file=name;
     If[file =!= "",
       If[file == "$", file = ToString[$FILE$], $FILE$=file]
       ];
     If[file == "NOT_SET",
      Print["No default filename yet..."];
      Return[]];
     For[i=0, i<dof, i++,
      For[j=1, j<=dof, j++,
       If[j>i,
       temp=
        MPrint[T[i,j], StringJoin["T[", ToString[i], ",", ToString[j], "]= "]];

     If [file != "",
       PutAppend[OutputForm[temp], file];
       PutAppend[OutputForm[""], file],

       Print[temp];
       ]
       ]
      ]
     ]
    ]

(*
   APrint prints all the A matrices to the screen or to a file.
  If the filename is $, the default file name is used
  TODO: display all the A matrices, not print them
*)
APrint[name_String:""] :=
   Block[{j,temp, file},
     file=name;
     If[file != "",
     If[file == "$", file = ToString[$FILE$], $FILE$=file]];
     If[file == "NOT_SET",
      Print["No default filename yet..."];
      Return[]];
      For[j=1, j<=dof, j++,
       temp=MPrint[A[j], StringJoin["A[", ToString[j], "]= "]];
       If [file != "",
         PutAppend[OutputForm[temp], file];
         PutAppend[OutputForm[""], file],

         Print[temp];
         ]
      ]
     ]

(*
   MPrint prints any matrix with a label to the screen or to a file.
   If the filename is $, the default file name is used
*)
MPrint[M_List, text_String, name_String:""] :=(
  StringForm["````",text, MatrixForm[M]]
  );
	(*
MPrint[M_List, text_String, name_String:""] :=
    Block[{i, ro, co, c1, c2, c3, file},
         c1=c2=c3={};

(* find out how many rows and columns *)

         If[VectorQ[M], ro=Length[M],
            If[MatrixQ[M], {ro, co} = Dimensions[M], Return[]]];

(* put the label half way down the matrix *)

         For[i=0, i<(ro-1)/2, i++, c1=Append[c1," "]];
         c1=Append[c1,text];
         c1=ColumnForm[c1];

         For[i=0, i<ro, i++, c2=Append[c2, " | "]];
         c2=ColumnForm[c2];

         If[VectorQ[M], c3=ColumnForm[M], c3="";
           For[i=1, i<=co, i++, c3=SequenceForm[c3, ColumnForm[Transpose[M][[i]]]];
           c3=SequenceForm[c3, ColumnForm[{"  "}]]]];

         If[name != "",
          file=name;
          If[file == "$", file = ToString[$FILE$], $FILE$=file];
          If[file == "NOT_SET",
           Print["No default filename yet..."];
           Return[]];
           PutAppend[OutputForm[SequenceForm[c1,c2,c3,c2]], file];
           PutAppend[OutputForm[""], file],

          SequenceForm[c1,c2,c3,c2]
          ]
	]*)

(* EPrint prints all the elements of a matrix one per line *)
EPrint[M_List, text_String, name_String:""] :=
    Block[{i,j, temp, file, ro, co},
        If[VectorQ[M], ro=Length[M],
          If[MatrixQ[M], {ro, co} = Dimensions[M], Return[]]];
	Print[" "];
	Print[" "];
        If [name == "",

         If[MatrixQ[M],
           Do[
             Do[
		Print[text,"[",i,",",j,"] = ",M[[i,j]]];
                Print[""],
	        {i,ro}],
              {j,co}],

           Do[
              Print[text,"[",i,"] = ",M[[i]]];
              Print[""],
              {i,ro}]];

	Print[" "]];

        If[name != "",
          file=name;
          If[file == "$", file = ToString[$FILE$], $FILE$=file];
          If[file == "NOT_SET",
            Print["No default filename yet..."];
            Return[]];

          If[MatrixQ[M],
            Do[
              Do[
           temp=StringForm["``````````````",text,"[",i,",",j,"] = ",M[[i,j]]];
                 PutAppend[OutputForm[temp],file];
                 PutAppend[OutputForm[""],file],
                 {i,ro}],
              {j,co}],

            Do[
           temp=StringForm["``````````",text,"[",i,"] = ",M[[i]]];
               PutAppend[OutputForm[temp],file];
               PutAppend[OutputForm[""],file],
               {i,ro}]];

           PutAppend[OutputForm[""],file];
          ]
     ]


(*
  DataFile reads in a table of DH parameters, and checks to see if any
  Dynamics info is in the file as well.  DataFile sets some internal
  variables to indicate a successful read, and presence of dynamics
  info.

*)
DataFile[name_String:""] :=

  Block[{file, f, skip, temp, i, read, error} ,

    If [name == "",
      file = InputString["Enter data file name :"],
      file = name];

(* try to open file *)

    f = OpenRead[file];
    If [f == Null || f == $Failed, Print["Couldn't open your file."];
                                   Return[]];

(* clear old variable definitions *)
    ResetState[];

(* find the DOF line *)
    skip=True;
    While[skip,
      read = Read[f, String];
      If [read==EndOfFile, Print["Bad data file format."];
                           Close[f];
                           Return[]];

      If[StringLength[read] >4,
        If[StringTake[read,3] == "DOF", skip=False]]];

    temp = Flatten[StringPosition[read, "="]];
    If [temp == {},
      Print["Couldn't find the dof equals sign."];
      Close[f];
      Return[],
      dof = ToExpression[StringTake[read, {temp[[1]]+1, StringLength[read]}]]];

    If [Head[dof] =!= Integer,
      Print["Can't read the number of dof."];
      ResetState[];
      Close[f];
      Return[]];

    Read[f, String];
    error = False;

(* tq is a list of replacements for calculating time response *)
    tq = {};
    Do[
      read=Read[f, String]; (* The first line in the joint definition *)
      If [read==EndOfFile, Print["Bad data file format."];
                      error=True;
                      Break[]];

      temp = Flatten[StringPosition[read, "="]];

      If [temp == {},
        Print["Couldn't find the jointtype:", i];
          error=True;
          Break[],

(* the jointype string is everything after the equal sign *)
        jointtype[i] = ToString[ToExpression[
          StringTake[read, {temp[[1]]+1, StringLength[read]}]]]];

      If [jointtype[i] =!= "revolute" && jointtype[i] =!= "prismatic",
        Print["Bad joint type for link ", i];
        error=True;
        Break[]
		];

(* repeat a bunch of code to read a, alpha, d, theta *)
      read = Read[f, String]; (* The second line in the joint definition *)

      If [read==EndOfFile, Print["Bad data file format."];
                      error=True;
                      Break[]];

      temp = Flatten[StringPosition[read, "="]];

      If [temp == {},
        Print["Couldn't find the a parameter:", i];
          error=True;
          Break[],
        a[i] = ToExpression[StringTake[read,
               {temp[[1]]+1, StringLength[read]}]]];

      read = Read[f, String];

      If [read==EndOfFile, Print["Bad data file format."];
                      error=True;
                      Break[]];

      temp = Flatten[StringPosition[read, "="]];

      If [temp == {},
        Print["Couldn't find the alpha parameter:", i];
          error=True;
          Break[],
        alpha[i] = ToExpression[StringTake[read,
                   {temp[[1]]+1, StringLength[read]}]]];

      read = Read[f, String];

      If [read==EndOfFile, Print["Bad data file format."];
                      error=True;
                      Break[]];

      temp = Flatten[StringPosition[read, "="]];

      If [temp == {},
        Print["Couldn't find the d parameter:", i];
          error=True;
          Break[],
        d[i] = ToExpression[StringTake[read,
                {temp[[1]]+1, StringLength[read]}]]];

      read = Read[f, String];

      If [read==EndOfFile, Print["Bad data file format."];
                      error=True;
                      Break[]];

      temp = Flatten[StringPosition[read, "="]];

      If [temp == {},
        Print["Couldn't find the theta parameter:", i];
          error=True;
          Break[],
        theta[i] = ToExpression[StringTake[read,
                   {temp[[1]]+1, StringLength[read]}]];

        ];
      If [jointtype[i] == "prismatic", q[i] = d[i];
                                 AppendTo[tq, d[i]->d[i][Global`t]],
                                 q[i] = theta[i];
                                 AppendTo[tq,  theta[i]->theta[i][Global`t]]],
      {i, 1, dof}];

(*
   create a list of replacement rules to not change derivatives, but
   replace qx with qx[t] for NDSolve routines
*)
    For[i=1, i<=dof, i++,
     PrependTo[tq, q[i]'[Global`t] -> q[i]'[Global`t]];
     PrependTo[tq, q[i][Global`t] -> q[i][Global`t]];
       ];

    If [error, ResetState[]; Close[f]; Return[]];

(* lets see if we can find the string DYNAMICS to indicate dynamics info *)
    skip=True;
    While[skip,
      read=Read[f, String];
      If [read==EndOfFile, Print["No dynamics data found."];
                           $DATAFILE$ = "YES";
                           $DYNRUN$ = "CANT";
     ze=ConstantArray[0,{dof,5}];

k={Type,r, \[Alpha],d,\[Theta]};
b=Join[{k},ze];
cc=Transpose[b];
k=Join[{Joint},Array[#&,dof]];
l = Join[{k},cc];
DH1=Transpose[l];


     For[j=1,j<=dof,j++,DH1[[j+1,1]]= j;];
     For[j=1,j<=dof,j++,DH1[[j+1,2]]= jointtype[j];];
     For[j=1,j<=dof,j++,DH1[[j+1,3]]= a[j];];
     For[j=1,j<=dof,j++,DH1[[j+1,4]]= alpha[j];];
     For[j=1,j<=dof,j++,DH1[[j+1,5]]= d[j];];
     For[j=1,j<=dof,j++,DH1[[j+1,6]]= theta[j];];

     Print[Grid[DH1,Frame->All]];
                           Close[f];
                           Return[]];

      If[StringLength[read] >7,
        If[StringTake[read,8] == "DYNAMICS", skip=False]]];

    Read[f, String];

    read = Read[f, String];

    If [read==EndOfFile, Print["Bad data file format."];
                    error=True;
                    Break[]];

    temp = Flatten[StringPosition[read, "="]];

    If [temp == {},
      Print["Couldn't find the gravity vector:"];
        error=True;
        Break[],
      gravity[1] = ToExpression[StringTake[read,
                   {temp[[1]]+1, StringLength[read]}]]];

    For[i=2, i<=dof, i++, gravity[i] = gravity[1]];

    Do[
      read=Read[f, String];

      If [read==EndOfFile, Print["Bad data file format."];
                      error=True;
                      Break[]];

      temp = Flatten[StringPosition[read, "="]];

      If [temp == {},
        Print["Couldn't find the mass parameter:", i];
          error=True;
          Break[],
        mass[i] = ToExpression[StringTake[read,
                  {temp[[1]]+1, StringLength[read]}]]];

      read = Read[f, String];

      If [read==EndOfFile, Print["Bad data file format."];
                      error=True;
                      Break[]];

      temp = Flatten[StringPosition[read, "="]];

      If [temp == {},
        Print["Couldn't find the com vector parameter:", i];
          error=True;
          Break[],
        com[i] = ToExpression[StringTake[read,
                 {temp[[1]]+1, StringLength[read]}]]];

      read = Read[f, String];

      If [read==EndOfFile, Print["Bad data file format."];
                      error=True;
                      Break[]];

      temp = Flatten[StringPosition[read, "="]];

      If [temp == {},
        Print["Couldn't find the inertia matrix:", i];
          error=True;
          Break[],
        temp = ToExpression[StringTake[read,
               {temp[[1]]+1, StringLength[read]}]]];

        If[Length[temp] =!= 6,
          Print["Syntax error in data file at or before inertia tensor ", i];
          error=True;
          Break[];
          ];

        inertia[i] = { {temp[[1]], temp[[2]], temp[[3]]},
                       {temp[[2]], temp[[4]], temp[[5]]},
                       {temp[[3]], temp[[5]], temp[[6]]} },

      {i,1,dof}];

      Close[f];

      If[error, ResetState[]; Return[];];

      OKDynamics = True;
      $DATAFILE$ = "YES";

  PrintInputData[];
];

(* DH Input Functon *)

dhInput[x2_]:=
  Do[
    x3 = Dimensions[x2];
    If[ NumberQ[x2] && x2>0,
      dof = x2;
      createdh[],

      If[ Length[x2]==5 && Length[x3]==2 ,
        dof= x3[[2]];
        DH = x2;
        createdh2[],

        dof= Input["You entered an incorrect Matrix. How many joints (DOF) does your robot have?"];
        createdh[]
      ]
    ];
  $dhInput$ = "YES";
  ];

(*
Create DH function: parses input and generates the DH table
*)
(*'*)
createdh[]:=
  Do[
    If[ IntegerQ[dof] && dof>0,

      DH= Input[ "Fill out the DH parameters:
        Note: \[Alpha] and \[Theta] should be in radians.",
        ze=ConstantArray[{"r",0,0,0,0},{dof}];
        k={Type,r, \[Alpha],d,\[Theta]};
        b=Join[{k},ze];
        cc=Transpose[b];
        k=Join[{Joint},Array[#&,dof]];
        l = Join[{k},cc];
        Q=Transpose[l];
        Grid[ Q ,Frame->All, Alignment->Center,Background->{{Gray},{Gray},Automatic},ItemStyle->{{Directive[White,Bold,12]},{Directive [ White,Bold,12] }} ]
      ],

      Print["DOF should be a positive Integer"];
      Return[]

    ];


    If[ Dimensions[DH]!= {6} || DH == k ,
      Print["Cancelled"];
      Return[]
    ];

    For[ i=1,i<=dof,i++,
      zz=ToString[DH[[1,i+1,2]] ];
      If[ !MemberQ[{"Prismatic","prismatic","P","p","Revolute","revolute","R","r"},zz],
        Print[" Type column, should include only:
          Revolute, revolute , R, r, Prismatic, prismatic, P or p"];
        Return[]
      ]
    ];
    For[ i=1,i<=dof,i++,
      thetac[i]=DH[[1,i+1,6]];
      zz=ToString[DH[[1,i+1,2]] ];
      If[MemberQ[{"Prismatic","prismatic","P","p"},zz],
        DH[[1,i+1,5]] = Subsuperscript["d",i,"*"];
        DH[[1,i+1,2]]="prismatic",

        If[ NumberQ[DH[[1,i+1,5]]] || NumericQ[DH[[1,i+1,5]]],
          DH[[1,i+1,5]],

          DH[[1,i+1,5]]=Subscript["d",i]
        ]
      ];


      If[ MemberQ[{"Revolute","revolute","R","r"},zz],
        DH[[1,i+1,6]] = Subsuperscript["\[Theta]",i,"*"];
        DH[[1,i+1,2]]="revolute",

        DH[[1,i+1,6]]
      ];

      If[ NumberQ[DH[[1,i+1,3]]] || NumericQ[DH[[1,i+1,3]]],
        DH[[1,i+1,3]],

        DH[[1,i+1,3]]=Subscript["r",i]
      ];

      a[i]=DH[[1,i+1,3]];
      alpha[i]=DH[[1,i+1,4]];
      d[i] =DH[[1,i+1,5]];
      theta[i]=DH[[1,i+1,6]];
      jointtype[i] = ToString[DH[[1,1+i,2]]];
    ];

    $DATAFILE$="NO";
    $dhInput$ = "YES";
    DH1=ConstantArray[0,{dof+1,6}];
    For[ i=1,i<=dof+1,i++,
      For[j=1,j<=6,j++,
        DH1[[i,j]]=DH[[1,i,j]];
      ]
    ];

    Print[Grid[DH1,Frame->All]]

    For[ i=1,i<=dof,i++,
      theta[i]=thetac[i];
    ]
  ];

(*Create DH 2*)

createdh2[]:=
  Do[
    For[ i=1,i<=dof,i++,
      zz=ToString[DH[[1,i]] ];
      If[ !MemberQ[{"Prismatic","prismatic","P","p","Revolute","revolute","R","r"},zz],
        Print[" Type column, should include only:
          Revolute, revolute, R, r, Prismatic, prismatic, P, or p"];
        Return[]
      ]
    ];
    For[ i=1,i<=dof,i++,
      alphac[i]=DH[[3,i]];
      thetac[i]=DH[[5,i]];
      zz=ToString[ DH[[1,i]] ];
      If[ MemberQ[{"Prismatic","prismatic","P","p"},zz],
        DH[[4,i]] = Subsuperscript["d",i,"*"];
        DH[[1,i]]="prismatic",

        If[ NumberQ[DH[[4,i]]] ||  NumericQ[DH[[4,i]]],
          DH[[4,i]],

          DH[[4,i]]=Subscript["d",i]
        ]

      ];

      If[ MemberQ[{"Revolute","revolute","R","r"},zz],
        DH[[5,i]] = Subsuperscript["\[Theta]",i,"*"];
        DH[[1,i]]="revolute"
      ];

      If[NumberQ[DH[[2,i]]] || NumericQ[DH[[2,i]]],
        DH[[2,i]],
        DH[[2,i]]=Subscript["r",i]
      ];
      a[i]=DH[[2,i]];
      alpha[i]=DH[[3,i]];
      d[i] =DH[[4,i]];
      theta[i]=DH[[5,i]];
      jointtype[i] = ToString[ DH[[1,i]] ];
    ];
    $DATAFILE$="NO";
    $dhInput$ = "YES";
    DH1=Transpose[DH];

    k2={Type,r, \[Alpha],d,\[Theta]};
    b2=Join[{k2},DH1];
    b3=Transpose[b2];
    k3=Join[{Joint},Array[#&,dof]];
    l2 = Join[{k3},b3];
    L2=Transpose[l2];

    Print[Grid[L2,Frame->All]]

    For[ i=1,i<=dof,i++,
      theta[i]=thetac[i];
      alpha[i]=alphac[i];
    ];
  ];

(*
  reset variables'
*)

ResetState[] := Block[{},
   $DATAFILE$ = "NO";
   $dhInput$ = "NO";
   $FKINRUN$ = "NO";
   $DYNRUN$ = "NO";
   $XRANGE$ = {-10, 10};
   $YRANGE$ = {-10, 10};
   $ZRANGE$ = {-10, 10};
   $RANGES$ = {$XRANGE$, $YRANGE$, $ZRANGE$};

(* created by datafile for kinematics info *)
   Clear[dof, jointtype, a, alpha, d, theta, q];

(* created by FKIN to generate forward kinematics *)
   Clear[A, J, T, z, o];

(* created by datafile for dynamics info *)
   Clear[mass, com, gravity, inertia, OKDynamics];

(* created by ELDynamics[] to generate dynamics info *)
   Clear[Jc, Jvc, Jwc, MU, M, c];
   ];

(*
  display a table of the various parameters read in
*)
PrintInputData[]:=
	Block[{i,j,k,jointVector,jointtypeVector,aColumn,alphaColumn,
               dColumn, thetaColumn, linkVector, massColumn, temp2,
               lcColumn},

        If [$DATAFILE$ == "NO" && $dhInput$ == "NO",
          Print["You must first load or a data file or input DH parameters."];
          Return[];
          ];
	Print[Style["Kinematics Input Data:",Italic]];
	jointVector=ColumnForm[
	   Prepend[Table[j,{j,dof}],Style["Joint",Bold]]
	  ];
	jointtypeVector=ColumnForm[
	   Prepend[Table[jointtype[j],{j,dof}],Style["Type",Bold]]
	  ];
	aColumn=ColumnForm[
	   Prepend[Table[a[j],{j,dof}],Style["r",Bold]]
	   ];
	alphaColumn=ColumnForm[
	Prepend[Table[alpha[j],{j,dof}],Style["\[Alpha]",Bold]]
	      ];
	dColumn=ColumnForm[
	   Prepend[Table[d[j],{j,dof}],Style["d",Bold]]
	  ];
	thetaColumn=ColumnForm[
	Prepend[Table[theta[j],{j,dof}],Style["\[Theta]",Bold]]
			      ];

	Print[SequenceForm[jointVector,
	 	     ColumnForm[{"   "}],
		     jointtypeVector,
	 	     ColumnForm[{"   "}],
		     aColumn,
	 	     ColumnForm[{"   "}],
		     alphaColumn,
		     ColumnForm[{"   "}],
		     dColumn,
		     ColumnForm[{"   "}],
		     thetaColumn]];

	If[OKDynamics === True,
		Print["Dynamics Input Data"];
		Print["-------------------"];
                Print["Gravity vector: [",gravity[1][[1]], ", ",
                                          gravity[1][[2]], ", ",
                                          gravity[1][[3]], "]" ];

		linkVector=ColumnForm[Prepend[
		   Prepend[Table[j,{j,dof}]," "],"Link"]
		  ];
		massColumn=ColumnForm[Prepend[
		   Prepend[Table[FortranForm[mass[j]],{j,dof}]," "],"mass"]
		  ];
		lcColumn=ColumnForm[Prepend[
		   Prepend[Table[SequenceForm[ "[", CForm[com[j][[1]]], ", ",
                                     CForm[com[j][[2]]], ", ",
                                     CForm[com[j][[3]]],  "]"],
                                  {j,dof}]," "],"com vector"]
			  ];
		Print[SequenceForm[
		     linkVector,
	 	     ColumnForm[{"   "}],
		     massColumn,
	 	     ColumnForm[{"   "}],
		     lcColumn
		     ]];
		Print[" "];
		Do[
			temp2=StringJoin["Inertia[",ToString[i],"] = "];
			Print[MPrint[inertia[i],temp2]];
                        Print[""]
			,{i,1,dof}]

  	  ]
        ];

(*
  Run the functions that generate the forward kinematics
*)
FKin[]:=
	Do[
    If[$dhInput$ == "YES", (*DH Parameter entered from dhInput[]*)
      Print[""],
      If[$DATAFILE$ == "YES",(*DH Parameter entered from DataFile[]*)
        Print[""],
	      If[ $DATAFILE$ == "NO" && $dhInput$ == "NO",
          dhInput[]
        ]
      ]
    ];

	  FormAllAs[];
	  FormAllTs[];
	  FormTheJacobianJ[];
      $FKINRUN$ = "YES";
  ]

(*
  The A matrices are just a fill-in-the-blank procedure
*)
FormAllAs[]:=
	Block[
    {i},
    st = "A Matrices Formed:";
    Do[
      st = StringJoin[
        st,
        If[i>1,", "," "],
        ToString[StringForm["A[``]",i]]
      ];

        A[i]=FormA[
          a[i],
          alpha[i],
          d[i],
          theta[i]
        ],
      {i,1,dof}
     ]
	]

FormA[a_,alpha_,d_,theta_] :=

Chop[{ {Cos[theta], -Sin[theta] Cos[alpha], Sin[theta] Sin[alpha], a Cos[theta]},
  {Sin[theta], Cos[theta] Cos[alpha], -Cos[theta] Sin[alpha], a Sin[theta]},
  {0,          Sin[alpha],            Cos[alpha],             d},
  {0,          0,                     0,                      1} }]


FormAllTs[]:=
	Block[{i,j},

  T[0,0]=IdentityMatrix[4];
  st = "T Matrices Formed: T[0,0]";
	For[ i=0,i < dof, i++,
	  For[j=1,j<=dof , j++ ,
      If[ j>i ,
        st = StringJoin[st, ToString[
          StringForm[", T[``,``]",i,j]]
        ];
		    T[i,j]=Chop[TrigFactor[FormTij[i,j]]]
			]
		]
	]

  ]
(*
  Recursively form the T matrix T[i,j]
*)
FormTij[k_,l_]:=
  If[ (l-k)==1,
    A[l],

    A[k+1].FormTij[k+1, l]
  ];

FormTheJacobianJ[]:=
  Block[ {i,j,v,w,Jvw},
    Do[
      z[j] = {T[0,j][[1,3]], T[0,j][[2,3]], T[0,j][[3,3]]};
      o[j] = {T[0,j][[1,4]], T[0,j][[2,4]], T[0,j][[3,4]]},

      {j,0,dof}
    ];
    For[ j=1, j<=dof, j++,
      If[ jointtype[j]=="revolute",
        v= Cross3[z[j-1],o[dof]-o[j-1]];
        w=z[j-1];
        Jvw[j]=Join[v,w]
      ];
      If[ jointtype[j]=="prismatic",
        v=z[j-1];
        w={0,0,0};
        Jvw[j]=Join[v,w]
      ]
    ];
    J = Transpose[ Table[Jvw[j], {j, 1, dof} ] ];
	  (*Print["Jacobian Formed:  J","(6","x",dof,")"];*)
	]

(*
  A simple cross product calculator
*)
Cross3[x_,y_]:=
	Return[{ x[[2]]y[[3]]-x[[3]]y[[2]],
           x[[3]]y[[1]]-x[[1]]y[[3]],
           x[[1]]y[[2]]-x[[2]]y[[1]] }]


(*
  Form the nxnxn table of ChristoffelSymbols
*)
FormChristoffelSymbols[x_]:=
	Block[{i,j,k},
		c=Table[0,{dof},{dof},{dof}];
		Do[c[[i,j,k]]=TrigFactor[1/2 ( D[x[[k,j]],q[i]]+
				    D[x[[k,i]],q[j]]-
				    D[x[[i,j]],q[k]])]
		,{i,1,dof},{j,1,dof},{k,1,dof}];
	Print[" "];
	]







(*
  The length of a vector
*)
Long[{x1_, y1_, z1_:0}] := Sqrt[x1 x1 + y1 y1 + z1 z1];

(*
  Return either Graphics or Graphics3d
*)
Gr[x_, dim_] :=
 Block[{},
  If[Length[x] >1,
  If[Head[x[[2]]] === Graphics3D,
   Return[x[[2]] ]]];

  If[dim==7,
   Return[Graphics3D[{AbsoluteThickness[1], x}]],
   Return[Graphics[{AbsoluteThickness[1], x}]]];
];


(*
  Set the x, y, and z plot ranges for animations
*)
SetRanges[x_List, y_List, z_List] :=
  Block[{},
   If [Length[x] !=2 || Length[y]!= 2 || Length[z]!=2,
    Print["Each list should contain a start and end value only."];
    Return[]];

   If[!NumberQ[Plus @@ N[x]], Print["X Ranges must be numbers."];
    Return[]];
   If[!NumberQ[Plus @@ N[y]], Print["Y Ranges must be numbers."];
    Return[]];
   If[!NumberQ[Plus @@ N[z]], Print["Z Ranges must be numbers."];
    Return[]];

  $XRANGE$ = x;
  $YRANGE$ = y;
  $ZRANGE$ = z;
];


(*
  Show the user the input vector
*)
(*drawing functions*)
drawZArrow[jr_]:=Line[{{{0,0,0},{0,0,2jr}},
{{0,0,2jr},{1/32,0,3/2jr}},
{{0,0,2jr},{-1/32,0,3/2jr}},
{{0,0,2jr},{0,1/32,3/2jr}},
{{0,0,2jr},{0,-1/32,3/2jr}}}];
drawCoordAxes[jr_]:= {Thick,{Red,drawZArrow[jr]},
{Blue,Rotate[drawZArrow[jr],\[Pi]/2,{0,1,0}]},
{Green,Rotate[drawZArrow[jr],-\[Pi]/2,{1,0,0}]}}


drawJoint[ j_,d_,r_,\[Theta]_,showArrow_:True]:=Module[{jr = 1/5,ar = 1/20,pr=1/7,vr=1/6},{
If[showArrow,(*draw coordinate axis*) drawCoordAxes[jr]],
Opacity[1],
(*draw z-axis*)
{Opacity[0.5],Gray,If[j == "prismatic",Cuboid[{-ar,-ar,-1+d-jr-.01},{ar,ar,d+.01}],Cylinder[{{0,0,Min[-ar,d-jr]-.01},{0,0,Max[ar,d]+.01}},ar]]},
(*draw joint*)
{LightBlue,If[j == "prismatic",
{Cuboid[{-jr,-jr,-jr},{jr,jr,+jr-.1}],
Cuboid[{-jr,-jr,+jr},{jr,jr,+jr+.05}]
},
{Cylinder[{{0,0,-jr-.1},{0,0,+jr+.1}},.9*jr]
}]},
(*draw x(i+1) *)
Rotate[{Opacity[0.5],Gray,Cuboid[{-ar,-ar,d-ar},{r,ar,d+ar}]},\[Theta],{0,0,1}]
}];


drawShaft[ j_,d_,r_,\[Theta]_]:=Module[{jr = 1/5,ar = 1/20},{
Opacity[1],
(*draw z-axis*)
{Opacity[0.5],Gray,
  If[j == "prismatic",Cuboid[{-ar,-ar,-1+d-jr-.01},{ar,ar,d+.01}],
                      Cylinder[{{0,0,Min[-ar,d-jr]-.01},{0,0,Max[ar,d]+.01}},ar]]}
}];

drawGripper[g_,r_,showArrow_:True]:=Module[{jr = 1/5,ar = 1/20},{
Opacity[1],
  If[showArrow,(*draw coordinate axis*) drawCoordAxes[jr]],
If[r!= 0,
{Gray,
Cuboid[{-2ar,-ar,-4ar},{0,ar,4ar}],
Cuboid[{0ar,-ar,g 2ar},{4ar,ar,2(1+g)ar}],
Cuboid[{0ar,-ar,-g 2ar},{4ar,ar,-2(1+g)ar}]},
(*looks better if hand points along d*)
{Gray,
Cuboid[{-4ar,-ar,-2ar},{4ar,ar,0}],
Cuboid[{g 2ar,-ar,0ar},{2(1+g)ar,ar,4ar}],
Cuboid[{-g 2ar,-ar,0ar},{-2(1+g)ar,ar,4ar}]}]
}];
(*math function to create a T matrix*)
dhTransform[d_,r_,\[Theta]_,\[Alpha]_]:=RotationTransform[\[Theta],{0,0,1}].TranslationTransform[{0,0,d}].TranslationTransform[{r,0,0}].RotationTransform[\[Alpha],{1,0,0}];

Options[drawRobot] = {showArrows -> True, showH -> True, showManipEllipse-> False, showPlanes->False};

drawRobot[OptionsPattern[]]:=
Manipulate[
Chop[%,10^-10];
Module[{jr = 1/10,ar = 1/40,Ad,Td,Ts,j,i,ii,jj,Tv},

Ad =Table[
If[jointtype[i]=="prismatic",
dhTransform[params[[i]],a[i],theta[i],alpha[i]],dhTransform[d[i],a[i],params[[i]],alpha[i]]],{i,1,dof}];

For[j=1,j<=dof,j++,
Tv=dhTransform[0,0,0,0];
Ts=dhTransform[0,0,0,0];         (*  For Loop to create Td[i]loop *)
For[i=1,i<=j,i++,Ts=Ts.Ad[[i]]];
For[ii=1,ii<=4,ii++,
For[jj=1,jj<=4,jj++,
Tv[[1,ii,jj]]=Chop[Ts[[1,ii,jj]]];
];
];
Td[j]=Tv;
];

Graphics3D[{
(*ground*)
{LightBrown,Cylinder[{{0,0,-2/5},{0,0,-1/5-1/20}},2.2]},
If[jointtype[1]== "revolute",drawJoint[jointtype[1],d[1],a[1],params[[1]],OptionValue[showArrows]],drawJoint[jointtype[1],params[[1]],a[1],theta[1]],OptionValue[showArrows]],

If[dof==1,GeometricTransformation[drawGripper[g,0,OptionValue[showArrows]],Chop[Td[dof]]],
If[showRobot,
 Table[
  If[jointtype[i]=="revolute",GeometricTransformation[drawJoint[jointtype[i],d[i],a[i],params[[i]],OptionValue[showArrows]],Td[i-1]],
                              GeometricTransformation[drawJoint[jointtype[i],params[[i]],a[i],theta[i],OptionValue[showArrows]],Td[i-1]]],
      {i,2,dof}]](*,
If[jointtype[dof]== "revolute",GeometricTransformation[drawShaft[jointtype[dof],d[dof], a[dof],params[[dof]]],Td[dof-1]],
                               GeometricTransformation[drawShaft[jointtype[dof],params[[dof]], a[dof],theta[dof]],Chop[Td[dof-1]]]]*)
],
GeometricTransformation[drawGripper[g,0,OptionValue[showArrows]],Chop[Td[dof]]],

If[OptionValue[showPlanes], (*show x_i, y_i plane for figuring out inverse kinematics*)
GeometricTransformation[{  Thick,
{Blue,Rotate[drawZArrow[1/2],\[Pi]/2,{0,1,0}], Text[Subscript["x",planei],{.9,.2,0}]},
{Green,Rotate[drawZArrow[1/2],-\[Pi]/2,{1,0,0}], Text[Subscript["y",planei],{.2,.9,0}]},
  Blue, Opacity[0.2],Polygon[{{-1,-1,0},{-1,1,0},{1,1,0},{1,-1,0}}]}
  ,If[planei>0,Td[planei],dhTransform[0,0,0,0]]]],

If[OptionValue[showH],
Text[StringForm["\!\(\*
StyleBox[\"H\",\nFontSlant->\"Italic\"]\)=``",MatrixForm[N[Chop[Td[dof]],2]]],{0,0,-3.2}]]
},SphericalRegion->True,ImageSize->425,Boxed->False]],
{{params,ConstantArray[0,dof]},ControlType->None},
Dynamic[Grid[Table[With[{i=i},
If[jointtype[i]=="prismatic",
{Subscript["d",i],Slider[Dynamic[params[[i]]],{0,1,1/20},ImageSize->Small],Dynamic[params[[i]]]},
{Subscript["\[Theta]",i],Slider[Dynamic[params[[i]]],{-\[Pi],\[Pi],\[Pi]/32},ImageSize->Small],Dynamic[params[[i]]]}
]],{i,dof}](*Table*)]],(*Dynamic*)
Delimiter,
{{g,1,"grip"},0,1,0.01,ImageSize->Small,Appearance->"Labeled"},
{{showRobot,True,"show robot"},{True,False}},
{{planei,0,"xy Plane"},0,dof,1,ImageSize->Small,Appearance->"Labeled",ControlType->If[OptionValue[showPlanes],Slider,None]},
ControlPlacement->Left,
SaveDefinitions->True
];




End[]
EndPackage[]
