(* ::Package:: *)

(*
         Robotica

A Mathematica package for the analysis and design of robots.
Author: John Nethery, nethery@robot1.ge.uiuc.edu  (email no longer works)
Copyright 1993 Board of Trustees, University of Illinois
All rights reserved.

Updated by Aaron T. Becker (atbecker@uh.edu) & Mohammad Sultan in 2017

*)

BeginPackage["robotica`"]
Off[Replace::rep]

(*
   Define the help strings for the functions that will be available to
   the user.  Define also the variables that we are going to export.
   We should eventually add help strings for the variables as wel...
*)
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


RElp::usage = "RElp[vars_List, options_List:{}] plots manipulability or force
ellipsoids over a range of variables, subject to the options listed."

SetRanges::usage = "SetRanges[xrange, yrange, zrange] sets a consistent
range for viewlimits used during animations."

SimplifyExpression::usage = "SimplifyExpression[x_] tries to reduce the
expression 'x' to its most reduced form."



DisplayTau::usage = "DisplayTau[] shows the elements of the Response
tau vector."

GetInputTau::usage = "GetInputTau[file_String] loads a definition file
for the tau vector used in Response for solving the dynamics equations."

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

CPrint::usage = "CPrint[text_String, name_String:''] prints the Christoffel
symbols stored in 'c' one per line each with a label 'text'.  Saved in
file 'name' if specified."

DataFile::usage = "DataFile[name_String:''] reads in a DH input file
from 'name' if given.  Otherwise prompt for a file and read it."

dhInput::usage = "dhInput[] lets the user enter the DH parameters, in a list of {joint_type,r,alpha,d,theta}."

createdh::usage = "create DH parameter by Given DOF"
createdh2::usage " Create DH Parameter Table by given DH Matrix"
PrintInputData::usage = "PrintInputData[] prints the current robot
data set in tabular form to the screen."

FKin::usage = "FKin[] generates the A and T matrices, as well as the Jacobian
for the current input data set."

ELDynamics::usage =
"ELDynamics[] generates Euler Lagrange dynamics equations provided
that the A, T, and Jacobian matrices are generated."

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
drawRobot3::usage= "drawRobot3[] displays a manipulate window and the robot with the manipulability ellipses"
myArrowArc::usage=""
myArrowArc2::usage=""
makeHand::usage=""
JvRRRanthro::usage=""
splineCircle::usage=""


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
$TAU$ = Null;
$DYNRUN$ = "NO";
$REVJOINT$ = Null;
$PRISJOINT$ = Null;
$LINK$ = Null;
$NDS$ = Null;
$ENDEFF$=Null;
$MOVIE$ = Null;
$XRANGE$ = {-10,10};
$YRANGE$ = {-10,10};
$ZRANGE$ = {-10,10};
$RANGES$ = {$XRANGE$, $YRANGE$, $ZRANGE$};
$FILE$="NOT_SET";
$SIMVARLIST$ = {};
$SIMVALUES$ = Null;
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
(*Print["Copyright 1993 Board of Trustees, University of Illinois"];
Print["All rights reserved."];
Print["Email questions, comments, or concerns to ", $ROBGUY$, "."];*)

(*
  SimplifyTrigNotation replaces Sin[] and Cos[] with S and C, and the
  parameter with the same parameter with its first character removed.
  i.e. q1 -> 1, q10 -> 10, d3 ->3, qtest -> test
  Don't change compound expressions: Cos[4 r] does not change to Cr,
  Also, preserve one character variables:  Cos[w] = Cos[w]

TODO: fix this to use 3 parameters
*)


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
 (* Tan[r_Rational Pi]	:> 1/Tan[(1/2 - r) Pi] /; r > 1/4 *)

}
(*Protect[TrigCanonicalRel] XYZZY *)

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

(* CPrint prints the Christoffel symbols to screen or file *)
CPrint[text_String, name_String:""] :=
  Block[{i,j,k},
    If[$DYNRUN$ == "NO", Print["You must run ELDynamics[] first..."];
                         Return[]];
    If[$DYNRUN$ == "CANT", Print["You must run ELDynamics[] first..."];
                         Return[]];

    Print[" "];

    If [name == "",
      For [i=1, i<=dof, i++,
        For [j=1, j<=dof, j++,
          For [k=1, k<=dof, k++,
            Print[text, "[[", i, ",", j, ",", k, "]] = ", c[[i,j,k]]];
            Print[""];
          ];
        ];
      ];
     Print[" "];
    ];

   If[name != "",
     file=name;
     If[file == "$", file = ToString[$FILE$], $FILE$=file];
     If[file == "NOT_SET",
       Print["No default filename yet..."];
       Return[]];

      Do[
        Do[
          Do[
             temp=StringForm["``````````````````",
                  text,"[[",i,",",j,",",k,"]] = " ,c[[i,j,k]]];
             PutAppend[OutputForm[temp],file];
             PutAppend[OutputForm[""],file],
                 {k,dof}],
              {j,dof}],
           {i, dof}];
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
  reset variables
*)
ResetState[] := Block[{},
   $DATAFILE$ = "NO";
   $dhInput$ = "NO";
   $FKINRUN$ = "NO";
   $TAU$ = Null;
   $DYNRUN$ = "NO";
   $SIMVARLIST$ = {};
   $SIMVALUES$ = Null;
   $NDS$ = Null;
   $XRANGE$ = {-10, 10};
   $YRANGE$ = {-10, 10};
   $ZRANGE$ = {-10, 10};
   $MOVIE$ = Null;
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
  Calculate the Euler-Lagrange dynamics
*)
ELDynamics[]:=
	Do[
        If[ $FKINRUN$ == "NO", Print["You must run FKin[] first..."];
                               Return[]];
        If[ $DYNRUN$ == "CANT",
               	 Print["There was no dynamics data in the file."];
                 Return[]];

	FormInertiaMatrix[];

	FormChristoffelSymbols[MU];
	Print[" "];
	Print["Christoffel Symbols Formed. "];

(*
	SimplifyDerivativeNotation[];
*)
        $DYNRUN$ = "YES";
	]


(*
  The inertia matrix MU, unsimplified, is created
*)
FormInertiaMatrix[]:=
	Block[{i},
	FormDynamicsJacobians[];
	MU=
	Sum[
            mass[i] Transpose[Jvc[i]].Jvc[i]+
            Transpose[Jwc[i]].T[0,i][[ Range[1,3],Range[1,3] ]].
            inertia[i].Transpose[T[0,i][[ Range[1,3],Range[1,3] ]]]. Jwc[i]
	,{i,1,dof}];

	Print[" "];
	Print[" "];
	Print["Mass Matrix MU(",dof," x ",dof,") Formed."
               " No Trigonometric Simplification."]

];

(*
  Form dynamics jacobians sets up the calculation of the Jc matrices.
  Note that the center of mass vector is first moved into the appropriate
  coordinate frame.
*)
FormDynamicsJacobians[] := Block[{i,j,lp,ap,cmv,col},

        For[j=1, j<=dof, j++,
         Jc[j] = {};

         cmv = o[j] + T[0,j][[{1,2,3}, {1,2,3}]] . com[j];

         For[i=1, i<=dof, i++,
          col = {0,0,0,0,0,0};
          If[i<=j,
           If[jointtype[i] == "prismatic",
              col = Join[z[i-1], {0,0,0}]];

           If[jointtype[i] == "revolute",
              ap=z[i-1] ;
              lp = Cross3[z[i-1], cmv - o[i-1]];
              col = Join[lp, ap] ];
             ];
           AppendTo[Jc[j], col ]];

         Jc[j] = Transpose[Jc[j]];
         Jvc[j] = Jc[j][[{1,2,3}, Range[1,dof]]];
         Jwc[j] = Jc[j][[{4,5,6}, Range[1,dof]]];
         ]
       ];

(*
  Simplify the inertia matrix with SimplifyExpression, M is created.
*)
SimplifyInertiaMatrix[x_]:=
	Do[
	M=SimplifyExpression[x];
	Print[" "];
	Print["Mass Matrix M(",dof," x ",dof,")",
               " with Trigonometric Simplification Formed "];
	Print[" "];
	];

(*
  Simplify is just this
*)
SimplifyExpression[x_]:=
	Simplify[TrigFactor[Simplify[Expand[x]]]]

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
   RElp calculates the manipulability ellipsoids
*)
RElp[vars_List, options_List:{}] :=
    Block[{i, j, k, num, evallist, axis,
           dim, maxaxis, anim, opts, axeslabel,tj, manout,
           forceout, temp, mj, endlocation, XtoZ, len, file,
           fval, frameopt },

  If[$FKINRUN$ == "NO",
    Print["You must run FKin first."];
    Return[]];

  opts = options;
  num = Null;
  file = Null;

  If [MemberQ[opts, Global`frame], fval = True, fval = False];
Needs["Graphics`Animation`"];
  If [MemberQ[$ContextPath, "Graphics`Animation`"],Null,

   If [MemberQ[opts, Global`animate],
     Print["Animation functions haven't been loaded...\n"];
     opts = Delete[opts, Position[opts, Global`animate]]]];

(* find the numerical step parameter in the var list *)

  If[Length[vars]>0, num = vars[[-1]]];

  If[NumberQ[N[num]], i = Length[vars] -1, i = Length[vars]; num = 5];
  For[j=1, j<=i, j++,
    If[ ( (Length[vars[[j]]]<3 && !MemberQ[opts,Global`single]) ||
         (Length[vars[[j]]]<2 && MemberQ[opts,Global`single]) ||
         !NumberQ[N[vars[[j,2]]]] ||
         !NumberQ[N[vars[[j,3]]]]),
       Print["Bad parameter list at ", j];
       Return[]
      ]];

  For[j=1, j<=Length[opts], j++,
    If[Head[opts[[j]]] == String &&
      (MemberQ[opts,Global`monly] || MemberQ[opts,Global`measures]) ,
       file=opts[[j]];
          If[file == "$", file = ToString[$FILE$], $FILE$=file];
          If[file == "NOT_SET",
            Print["No default filename yet..."];
            Return[]];
    Break[]]];

  If [file =!= Null,
    Print["Saving measures in file:", file]];

  If[MemberQ[opts, Global`single], num=0];

  If[!MatrixQ[J],
    Print["Bad Jacobian - Did you FKinematics?"];
    Return[]];

  dim=Planar[];
  Print["Planar number: ", dim];
  Print[" "];

(* set the frame option for two or three dimention graphics *)
  If [dim == 7,
    frameopt = Boxed->fval,
    frameopt = Frame->fval];

  centers[0]={0,0,0};
  maxaxis=-5;

(* find out which dimention we are working in, and take the appropriate
   elements of the Jacobian
*)
  If[dim==3,
    axeslabel={"x", "y"};
    mj = {J[[1]], J[[2]]};
    $RANGES$ = {$XRANGE$, $YRANGE$};
    ];

  If[dim==4,
    axeslabel={"x", "z"};
    mj = {J[[1]]};
    $RANGES$ = {$XRANGE$, $ZRANGE$};
    ];

  If[dim==5,
    axeslabel={"x", "z"};
    mj = {J[[1]], J[[3]]};
    $RANGES$ = {$XRANGE$, $ZRANGE$};
    ];

  If[dim==6,
    axeslabel={"y", "z"};
    mj = {J[[2]], J[[3]]};
    $RANGES$ = {$YRANGE$, $ZRANGE$};
    ];

  If[dim==7,
    axeslabel = {"x", "y", "z"};
    mj = {J[[1]], J[[2]], J[[3]]};
    $RANGES$ = {$XRANGE$, $YRANGE$, $ZRANGE$};
    ];

  forceout = {};
  manout = {};
  anim = {};

  For[j=1, j<=num+1, j++,
    evallist = {};

(* create a list of eval rules that step through param values *)

    For[k=1, k<=i, k++,
      If[num>0,
        evallist=Append[evallist, vars[[k,1]] -> vars[[k,2]] +
                    (j-1) (vars[[k,3]] - vars[[k,2]])/num] ,

        evallist=Append[evallist, vars[[k,1]] -> vars[[k,2]]]]];

    tj = { J[[1]], J[[2]], J[[3]] } /. evallist;
    If[!NumberQ[Plus @@ Flatten[N[tj]]],
      Print["There are undefined symbols in the Jacobian..."];
      Return[]];

(* set the frame to frame translation and rotation matrices *)

    For[k=1, k<=dof, k++,
      centers[k] = { T[0,k][[1,4]], T[0,k][[2,4]], T[0,k][[3,4]] } /. evallist;
       ];

    endlocation = centers[dof];

    For[k=1, k<=dof, k++,
      If[!NumberQ[Plus @@ N[centers[k]]],
        Print["Can't get numbers - are all variables set?"];
        Print[centers[k]];
        Return[]];
       ];

    Print["Working... ", j-1];
    If [file === Null,
      Print[evallist],
      PutAppend[OutputForm[evallist], file]];

    Print[" "];
    {u, md, v} = SingularValueDecomposition[N[tj]];

(* remember the maxaxis size for scaling *)

   If[md[[1]] > maxaxis, maxaxis=md[[1]]];

   axis[1] = {0,0,0};
   axis[2] = {0,0,0};
   axis[3] = {0,0,0};

   For[k=1, k<=Length[md], k++,
      axis[k] = Chop[N[md[[k]] u[[k]]]]];

(* store the axes for a call to wire ellipsoid *)

   axisset[j]={axis[1], axis[2], axis[3], endlocation};

(* calculate measures *)
   If [MemberQ[opts, Global`measures] || MemberQ[opts, Global`monly],
     If [file === Null,
       If [Length[md] == 1,
         Print["The ellipse has become a line at this point."]];

       temp = Chop[Sqrt[N[Det[N[(mj /. evallist)] .
                    Transpose[N[(mj /. evallist)]]]]]];
       Print["axis 1:", axis[1]];
       Print["axis 2:", axis[2]];
       Print["axis 3:", axis[3]];
       Print["Volume of ellipsoid: ", N[temp]];
       Print["Eccentricity:", N[md[[-1]] / md[[1]] ]];
       Print["Minimum Radius:", N[md[[-1]]]];
       Print["Geometric Mean:", N[temp ^ (1/Length[md])]];
       Print[""],

       If [Length[md] == 1,
          PutAppend[OutputForm["The ellipse has become a line at this point."],
          file]];

       temp = Chop[Sqrt[N[Det[N[(mj /. evallist)] .
                               Transpose[N[(mj /. evallist)]]]]]];
       PutAppend[OutputForm[
                 StringForm["````", "axis 1:", axis[1]]], file];

       PutAppend[OutputForm[StringForm["````", "axis 2:", axis[2]]], file];

       PutAppend[OutputForm[StringForm["````", "axis 3:", axis[3]]], file];

       PutAppend[OutputForm[
                 StringForm["````", "Volume of ellipsoid: ", N[temp]]], file];

       PutAppend[OutputForm[StringForm["````",
           "Eccentricity:", N[md[[-1]] / md[[1]] ]]], file];

       PutAppend[OutputForm[
                 StringForm["````", "Minimum Radius:", N[md[[-1]]]]], file];

       PutAppend[OutputForm[StringForm["````",
           "Geometric Mean:", N[temp ^ (1/Length[md])]]], file];

       PutAppend[OutputForm[" "], file];
      ];
    ];

   If [!MemberQ[opts, Global`monly],
     AppendTo[manout, DrawRobot[evallist, dim, 0]];

(* when force ellipsoids are implemented, this will create a plot *)
    If[MemberQ[opts, Global`forces],
       AppendTo[forceout, DrawRobot[evallist, dim]]];

      ]
  ];  (* calculate over all steps *)

  If [MemberQ[opts,Global`monly],
    Return[]];

(* scale ellipsoids *)
  If [MemberQ[opts,Global`scale], norm=maxaxis/2 , norm=1];

(* call WireE to generate a rotated ellipse, it is returned in ellipse.
   forceell is there to hold a force ellipsoid, but is not implemented
*)
  For[j=1, j<=num+1, j++,
    {ellipse, forceell}=
       WireE[axisset[j][[-1]], axisset[j][[1]]/norm,
             axisset[j][[2]]/norm, axisset[j][[3]]/norm];

    If [MemberQ[opts, Global`animate],
      AppendTo[anim,  Append[Gr[{manout[[j]], Planarize[ellipse,dim]}, dim],
          List[
          Rule[ Axes, True],
          Rule[ AspectRatio, Automatic],
          Rule[ PlotRange, $RANGES$],
          frameopt,
          Rule[ PlotLabel, "Manipulating Ellipsoids"],
          Rule[ AxesLabel, axeslabel]]]]];

       AppendTo[manout, Planarize[ellipse, dim]];
       AppendTo[forceout, Planarize[forceell,dim]]];

  manplot = Gr[manout, dim];

  If [Length[anim] != 0 && MemberQ[opts, Global`animate],
    $MOVIE$ = anim;];

  If [!MemberQ[opts, Global`animate],
  manplot = Show[manplot,
               PlotRange->All,
               DisplayFunction->$DisplayFunction,
               Axes->True,
               frameopt,
               AspectRatio->Automatic,
               PlotLabel->"Manipulating Ellipsoid",
               AxesLabel->axeslabel];

  If[MemberQ[opts,Global`forces],
  Print["Force ellipsoids not implemented."];
(*
  Show[forceout,
                 PlotRange->All,
                 DisplayFunction->$DisplayFunction,
                 AspectRatio->Automatic,
                 Axes->True,
                 PlotLabel->"Force Ellipsoid",
                 AxesLabel->axeslabel]
*)
   ];

  If[MemberQ[opts, Global`print],
    If [Context[LaserPrint] == "System`" ,
      Print["Using LaserPrint to print graphics..."];
      LaserPrint[manplot];
      Print["Done..."],
    Print["There is no internal LaserPrint command..."]]];

  If[MemberQ[opts,Global`xprint],
      Print["Adjust windows, then press return. "];
      Print["When the cursor changes to a cross, click on the graphics"];
      Print["window you want to store as 'xrelp.out'."];
      InputString["Waiting..."];

      Run["xwd >relp.t"];
      Run["xpr -device ps -gray 3 -portrait relp.t >xrelp.out"];
      Run["rm relp.t"];
      Print["File 'xrelp.out' created."]];

  If[MemberQ[opts,Global`mprint],
      Display["mrelp.out", manplot];
      Print["mrelp.out created."]];
  ];

  If [Length[anim] != 0 && MemberQ[opts, Global`animate],
    Print["Invoking ShowAnimation; this may take a while."];
    Graphics`Animation`ShowAnimation[anim];
   ];
];





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
  This functions writes a file containing all of the functions available
  to the user for the front end to read
*)
TellFunctions[fname_String]:= Block[{ttt},

  ttt = OpenWrite[fname, PageWidth -> Infinity];


  Write[ttt, OutputForm["APrint:OStringFilename:END;"]];

  Write[ttt, OutputForm["ClearLinkShape:END;"]];

  Write[ttt, OutputForm["ClearPrisJoint:END;"]];

  Write[ttt, OutputForm["ClearRevJoint:END;"]];

  Write[ttt, OutputForm["CPrint:RStringLabel:OStringFilename:END;"]];

  Write[ttt, OutputForm["DataFile:RStringFilename:END;"]];

  Write[ttt, OutputForm["DisplayTau::END;"]];

  Write[ttt, OutputForm["ELDynamics:END;"]];

  Write[ttt, OutputForm["EPrint:RSymbolMatrix:RStringLabel:OStringFilename:END;"]];

  Write[ttt, OutputForm["FKin:END;"]];

  Write[ttt, OutputForm["GetInputTau:RStringFilename:END;"]];

  Write[ttt, OutputForm["LinkShape:RStringFilename:END;"]];

  Write[ttt, OutputForm["LoadAnim:RStringFilename:END;"]];

  Write[ttt, OutputForm["MPrint:RSymbolMatrix:RStringLabel:OStringFilename:END;"]];

  Write[ttt, OutputForm["Planar:END;"]];

  Write[ttt, OutputForm["PrintInputData:END;"]];


  Write[ttt, OutputForm["RElp:RListParameter List:OList,frame,animate,single,measures,monly,scale,print,xprint,mprint,file,:END;"]];

  Write[ttt, OutputForm["Response:RListTime range:RListInitial Conditions:END;"]];

  Write[ttt, OutputForm["SaveResponse:RStringFilename:RListTime Range:OSymbolTime step:END;"]];

  Write[ttt, OutputForm["SetRanges:RListX Range:RListY Range:RListZ Range:END;"]];

  Write[ttt, OutputForm["SimplifyExpression:RSymbolExpression:END;"]];

  Write[ttt, OutputForm["SimplifyTrigNotation::END;"]];


  Write[ttt, OutputForm["TPrint:OStringFilename:END;"]];

  Close[ttt];

  Print["THESTART"];
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
  Read the input file defining the tau vector for response function
*)

GetInputTau[file_String:""] :=
  Block[{list, f},

  If[ $DATAFILE$ == "NO", Print["You must load a data file first..."];
                          Return[]];

  If[file=="",
   $TAU$ = Table[0, {dof}];
   Print["Tau set to 0."];
    Return[]];

  f = OpenRead[file];
  If[f == Null || f == $Failed,
    Print["Couldn't find file..."];
    $TAU$ = Null;
    Return[]];

  list = ReadList[f];
  Close[f];

  If[MemberQ[list,  $Failed],
     Print["Input file error at expression position ", Position[list, $Failed]];
     $TAU$ = Null;
     Return[]];

  $TAU$ = {};

  For[i=1, i<=dof, i++,
   AppendTo[$TAU$, N[Global`tau[i] /. tq]]];

  For[i=1, i<=dof, i++,
   If[Head[$TAU$[[i]]] == tau,
     Print["Undefined tau entry at position ",i];
     $TAU$ = Null;
     Return[]]];

 Print["Tau vector loaded."];
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
Manipulate[Chop[%,10^-10];
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

drawRobot3[]:=

Manipulate[Module[{jr=1/10,ar=1/40,Ad,Td,Ts,j,i,Jval,U,\[CapitalSigma]full,V,\[CapitalSigma],o0,on,Jvalw,Uw,\[CapitalSigma]w,vb,Vw},
Ad=Table[If[jointtype[i]=="prismatic",dhTransform[params[[i]],a[i],theta[i],alpha[i]],dhTransform[d[i],a[i],params[[i]],alpha[i]]],{i,1,dof}];

For[j=1,j<=dof,j++,Ts=dhTransform[0,0,0,0];(*For Loop to create Td[i]loop*)For[i=1,i<=j,i++,Ts=Ts.Ad[[i]]];
Td[j]=Ts;];

o0 = {0,0,0};
on=(First@Td[dof])[[1;;3,4]];
If[showJacobian,
Module[{zim1,oim1},
Jval = N[Transpose[Table[
zim1 = If[i==1,{0,0,1},(First@Td[i-1])[[1;;3,3]]];
If[jointtype[i]=="prismatic",zim1,
oim1 = If[i==1,{0,0,0},(First@Td[i-1])[[1;;3,4]]];
Cross[zim1,(on-oim1)]
],{i,1,dof}
]]]];

\[Mu]v=  Det[Jval.Transpose[Jval]];

(*Augment the matrix if less than 3 columns*)
If[Last@Dimensions[Jval]==1, Jval =Join[Jval,Jval,Jval,2]];
If[Last@Dimensions[Jval]==2, Jval =Join[Jval,Jval,2]];
{U,\[CapitalSigma],V} =SingularValueDecomposition[ N[Jval]]
(*\[CapitalSigma] =\[CapitalSigma]/\[CapitalSigma][[1,1]];*)
];

If[showJacobianw,
Module[{zim1},
Jvalw = N[Transpose[Table[
zim1 = If[i==1,{0,0,1},(First@Td[i-1])[[1;;3,3]]];
If[jointtype[i]=="prismatic",{0,0,0},zim1], {i,1,dof}
]]]];
(*Augment the matrix if less than 3 columns*)
If[Last@Dimensions[Jvalw]==1, Jvalw =Join[Jvalw,Jvalw,Jvalw,2]];
If[Last@Dimensions[Jvalw]==2, Jvalw =Join[Jvalw,Jvalw,2]];
{Uw,\[CapitalSigma]w,Vw} =SingularValueDecomposition[ N[Jvalw]]
(*\[CapitalSigma] =\[CapitalSigma]/\[CapitalSigma][[1,1]];*)
];
Column[{
If[showJacobian,
Text[Style[StringForm["\!\(\*
StyleBox[\"\!\(\*SubscriptBox[\(\[Mu]\), \(v\)]\)\",\nFontSlant->\"Italic\"]\)=``",Det[Jval.Transpose[Jval]]],TextAlignment->Right]]],

If[showJacobianw,
Text[Style[StringForm["\!\(\*
StyleBox[\"\!\(\*SubscriptBox[\(\[Mu]\), \(w\)]\)\",\nFontSlant->\"Italic\"]\)=``",Det[Jvalw.Transpose[Jvalw]]],TextAlignment->Right]]],

If[showRobot,
Text[Style[StringForm["\!\(\*
StyleBox[\"H\",\nFontSlant->\"Italic\"]\)=``",MatrixForm[N[Td[dof],2]]],TextAlignment->Right]],Text[Style[StringForm["\!\(\*
StyleBox[\"H\",\nFontSlant->\"Italic\"]\)=``",MatrixForm[N[Td[dof],2]]],TextAlignment->Right]]],



Graphics3D[{
If[showJacobian,(*draw manipulability ellipses*)
GeometricTransformation[
{Arrowheads[.02],
If[\[CapitalSigma][[1,1]]>0.1,{Blue,Arrow[{o0 ,\[CapitalSigma][[1,1;;3]]}],Arrow[{o0,-\[CapitalSigma][[1,1;;3]]}],
Blue,GeometricTransformation[splineCircle[{0,0,0},1,{0,2\[Pi]}],
DiagonalMatrix[{\[CapitalSigma][[1,1]],\[CapitalSigma][[2,2]],1}]]
}]
,If[\[CapitalSigma][[2,2]]>0.1,{Red,Arrow[{o0,\[CapitalSigma][[2,1;;3]]}],Arrow[{o0,-\[CapitalSigma][[2,1;;3]]}],
Red,GeometricTransformation[splineCircle[{0,0,0},1,{0,2\[Pi]}],DiagonalMatrix[{1,\[CapitalSigma][[2,2]],\[CapitalSigma][[3,3]]}].RotationMatrix[\[Pi]/2,{0,1,0}]]}],
If[\[CapitalSigma][[3,3]]>0.1,{Green,Arrow[{o0,\[CapitalSigma][[3,1;;3]]}],Arrow[{o0,-\[CapitalSigma][[3,1;;3]]}],
Darker[Green],GeometricTransformation[splineCircle[{0,0,0},1,{0,2\[Pi]}],DiagonalMatrix[{\[CapitalSigma][[1,1]],1,\[CapitalSigma][[3,3]]}].RotationMatrix[\[Pi]/2,{1,0,0}]]
}],
If[MatrixRank[\[CapitalSigma]]>2,{Opacity[0.5],LightBlue,Ellipsoid[{0,0,0},\[CapitalSigma][[1;;3,1;;3]]^2]}]}
,{U,on}]],
If[showJacobianw,(*draw manipulability ellipses*)
GeometricTransformation[
{Arrowheads[.02],
If[\[CapitalSigma]w[[1,1]]>0.1,{Blue,Arrow[{o0 ,\[CapitalSigma]w[[1,1;;3]]}],Arrow[{o0,-\[CapitalSigma]w[[1,1;;3]]}],
Blue,GeometricTransformation[splineCircle[{0,0,0},1,{0,2\[Pi]}],
DiagonalMatrix[{\[CapitalSigma]w[[1,1]],\[CapitalSigma]w[[2,2]],1}]]
}]
,If[\[CapitalSigma]w[[2,2]]>0.1,{Red,Arrow[{o0,\[CapitalSigma]w[[2,1;;3]]}],Arrow[{o0,-\[CapitalSigma]w[[2,1;;3]]}],
Red,GeometricTransformation[splineCircle[{0,0,0},1,{0,2\[Pi]}],DiagonalMatrix[{1,\[CapitalSigma]w[[2,2]],\[CapitalSigma]w[[3,3]]}].RotationMatrix[\[Pi]/2,{0,1,0}]]}],
If[\[CapitalSigma]w[[3,3]]>0.1,{Green,Arrow[{o0,\[CapitalSigma]w[[3,1;;3]]}],Arrow[{o0,-\[CapitalSigma]w[[3,1;;3]]}],
Darker[Green],GeometricTransformation[splineCircle[{0,0,0},1,{0,2\[Pi]}],DiagonalMatrix[{\[CapitalSigma]w[[1,1]],1,\[CapitalSigma]w[[3,3]]}].RotationMatrix[\[Pi]/2,{1,0,0}]]
}],
If[MatrixRank[\[CapitalSigma]w]>2,{Opacity[0.5],LightBlue,Ellipsoid[{0,0,0},\[CapitalSigma]w[[1;;3,1;;3]]^2]}]}
,{Uw,on}]],
(*ground*){LightBrown,Cylinder[{{0,0,-2/5},{0,0,-1/5-1/20}},2.2]},If[jointtype[1]=="revolute",
drawJoint[jointtype[1],d[1],a[1],params[[1]]],drawJoint[jointtype[1],params[[1]],a[1],theta[1]]],
If[showRobot,Table[If[jointtype[i]=="revolute",GeometricTransformation[drawJoint[jointtype[i],d[i],a[i],params[[i]]],Td[i-1]],
GeometricTransformation[drawJoint[jointtype[i],params[[i]],a[i],theta[i]],Td[i-1]]],{i,2,dof}]],
If[jointtype[dof]=="revolute",GeometricTransformation[drawShaft[jointtype[dof],d[dof],a[dof],params[[dof]]],Td[dof-1]],
GeometricTransformation[drawShaft[jointtype[dof],params[[dof]],a[dof],theta[dof]],Td[dof-1]]],
GeometricTransformation[drawGripper[g,0],Td[dof]]



(*If[showJacobian & showJacobianw,vb=1;
Text[StringForm["\!\(\*
StyleBox[\"\[Mu]w\",\nFontSlant->\"Italic\"]\)=``   ,   \!\(\*
StyleBox[\"H\",\nFontSlant->\"Italic\"]\)=``   ,   \!\(\*
StyleBox[\"\[Mu]v\",\nFontSlant->\"Italic\"]\)=``" ,Det[Jvalw.Transpose[Jvalw]], MatrixForm[N[Td[dof],2]],Det[Jval.Transpose[Jval]]]]],

If[showJacobian & vb\[NotEqual] 1,
Text[StringForm["\!\(\*
StyleBox[\"H\",\nFontSlant->\"Italic\"]\)=``   ,   \!\(\*
StyleBox[\"\[Mu]v\",\nFontSlant->\"Italic\"]\)=``" , MatrixForm[N[Td[dof],2]],Det[Jval.Transpose[Jval]]]]],

If[showJacobianw& vb \[NotEqual]1 ,
Text[StringForm["\!\(\*
StyleBox[\"\[Mu]w\",\nFontSlant->\"Italic\"]\)=``   ,   \!\(\*
StyleBox[\"H\",\nFontSlant->\"Italic\"]\)=``" ,Det[Jvalw.Transpose[Jvalw]], MatrixForm[N[Td[dof],2]]]]
]*)
},SphericalRegion->True,ImageSize->425,Boxed->False]},Center]],

{{params,ConstantArray[0,dof]},ControlType->None},Dynamic[Grid[Table[With[{i=i},If[jointtype[i]=="prismatic",{Subscript["d",i],
Slider[Dynamic[params[[i]]],{0,1,1/20},ImageSize->Small],Dynamic[params[[i]]]},{Subscript["\[Theta]",i],
Slider[Dynamic[params[[i]]],{-\[Pi],\[Pi],\[Pi]/32},ImageSize->Small],Dynamic[params[[i]]]}]],{i,dof}](*Table*)]],
(*Dynamic*)
Delimiter,
{{g,1,"grip"},0,1,0.01,ImageSize->Small,Appearance->"Labeled",Alignment->Left},
{{showRobot,True,"show robot"},{True,False}},
Delimiter,
Style["show Manipulability ellipses:",12],
{{showJacobian,True,"\[Sum]v"},{True,False}},
{{showJacobianw,True,"\[Sum]w"},{True,False}},
ControlPlacement->Left,SaveDefinitions->True];


(*draw circular arcs with arrowheads*)
myArrowArc[c_,r_,{start_,end_}]:=(*problem -- initial point too far out*)
If[start==end,Point[c+r{Cos[start],Sin[start]}],
Arrow[BSplineCurve[Table[c+r{Cos[t],Sin[t]},{t,start,end,(end-start)/11}]]]]
myArrowArc2[c_,r_,{start_,end_}]:=(*problem -- initial point too far out*)
If[start===end,Point[c+r{Cos[start],Sin[start]}],
Arrow[BSplineCurve[Table[c+r{Cos[t],Sin[t]},{t,start,end,(end-start)/11}]]]]
makeHand[fl_, bl_, fw_, bw_] :=
 Polygon[{{ -bl,-bw}, { -bl,bw}, { fl,fw}, { fl + 8 fw,0}, { fl,-fw}}/10];
makeDialControl[Dynamic[pt_],cent_,color_]:=Module[{ang},
LocatorPane[Dynamic[pt, (pt =  Nearest[Table[{Cos@a,Sin@a},{a,-\[Pi],\[Pi],\[Pi]/32}],#][[1]]) &],
(*LocatorPane[Dynamic[pt,(pt = Normalize[#])&],*)
ang=Dynamic[ArcTan[pt[[1]],pt[[2]]]];(*If[ang <0,ang=ang+2\[Pi]];*)
Graphics[
{
Gray,Circle[cent],
Gray,Table[Line[{cent+.94{Cos@a,Sin@a},cent+{Cos@a,Sin@a}}],{a,0,2\[Pi],\[Pi]/8}],
Table[Line[{cent+.98{Cos@a,Sin@a},cent+{Cos@a,Sin@a}}],{a,\[Pi]/16,2\[Pi],\[Pi]/8}],
{FontSize->10,Table[Text[\[Theta],cent+.7{Cos@\[Theta],Sin@\[Theta]}],{\[Theta], Rest@Range[- Pi,Pi, Pi/4]}]},
{color, EdgeForm[Black],Rotate[Translate[makeHand[7, 7/3, .1, .3],cent],ang,cent]},
Black,Arrowheads[.05],
color,myArrowArc2[cent,0.3,{0,ang}]
}]]];
(*Linear Velocity Jacobian of a 3-Link Elbow Manipulator with joint angles \[Theta]1, \[Theta]2, and \[Theta]3 and link lengths r1, r2, and r3*)
JvRRRanthro[\[Theta]1_,\[Theta]2_,\[Theta]3_,r2_,r3_] :=\!\(\*
TagBox[
RowBox[{"(", GridBox[{
{
RowBox[{
RowBox[{"-",
RowBox[{"(",
RowBox[{
RowBox[{"r2", " ",
RowBox[{"Cos", "[", "\[Theta]2", "]"}]}], "+",
RowBox[{"r3", " ",
RowBox[{"Cos", "[",
RowBox[{"\[Theta]2", "+", "\[Theta]3"}], "]"}]}]}], ")"}]}], " ",
RowBox[{"Sin", "[", "\[Theta]1", "]"}]}],
RowBox[{
RowBox[{
RowBox[{"-", "r2"}], " ",
RowBox[{"Sin", "[", "\[Theta]2", "]"}],
RowBox[{"Cos", "[", "\[Theta]1", "]"}]}], "-",
RowBox[{"r3", " ",
RowBox[{"Cos", "[", "\[Theta]1", "]"}],
RowBox[{"Sin", "[",
RowBox[{"\[Theta]2", "+", "\[Theta]3"}], "]"}]}]}],
RowBox[{
RowBox[{"-", "r3"}], " ",
RowBox[{"Cos", "[", "\[Theta]1", "]"}],
RowBox[{"Sin", "[",
RowBox[{"\[Theta]2", "+", "\[Theta]3"}], "]"}]}]},
{
RowBox[{
RowBox[{"Cos", "[", "\[Theta]1", "]"}], " ",
RowBox[{"(",
RowBox[{
RowBox[{"r2", " ",
RowBox[{"Cos", "[", "\[Theta]2", "]"}]}], "+",
RowBox[{"r3", " ",
RowBox[{"Cos", "[",
RowBox[{"\[Theta]2", "+", "\[Theta]3"}], "]"}]}]}], ")"}]}],
RowBox[{
RowBox[{
RowBox[{"-", "r2"}], " ",
RowBox[{"Sin", "[", "\[Theta]1", "]"}],
RowBox[{"Sin", "[", "\[Theta]2", "]"}]}], "-",
RowBox[{"r3", " ",
RowBox[{"Sin", "[", "\[Theta]1", "]"}],
RowBox[{"Sin", "[",
RowBox[{"\[Theta]2", "+", "\[Theta]3"}], "]"}]}]}],
RowBox[{
RowBox[{"-", "r3"}], " ",
RowBox[{"Sin", "[", "\[Theta]1", "]"}],
RowBox[{"Sin", "[",
RowBox[{"\[Theta]2", "+", "\[Theta]3"}], "]"}]}]},
{"0",
RowBox[{
RowBox[{"r2", " ",
RowBox[{"Cos", "[", "\[Theta]2", "]"}]}], "+",
RowBox[{"r3", " ",
RowBox[{"Cos", "[",
RowBox[{"\[Theta]2", "+", "\[Theta]3"}], "]"}]}]}],
RowBox[{"r3", " ",
RowBox[{"Cos", "[",
RowBox[{"\[Theta]2", "+", "\[Theta]3"}], "]"}]}]}
},
GridBoxAlignment->{"Columns" -> {{Center}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.7]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}}], ")"}],
Function[BoxForm`e$, MatrixForm[BoxForm`e$]]]\);
splineCircle[m_List,r_,angles_List: {0,2 \[Pi]}]:=
(*Efficient 3D circle, thanks http://mathematica.stackexchange.com/questions/10957/an-efficient-circular-arc-primitive-for-graphics3d *)
Module[{seg,\[Phi],start,end,pts,w,k},{start,end}=Mod[angles//N,2 \[Pi]];
If[end<=start,end+=2 \[Pi]];
seg=Quotient[end-start//N,\[Pi]/2];
\[Phi]=Mod[end-start//N,\[Pi]/2];
If[seg==4,seg=3;\[Phi]=\[Pi]/2];
pts=r RotationMatrix[start].#&/@Join[Take[{{1,0},{1,1},{0,1},{-1,1},{-1,0},{-1,-1},{0,-1}},2 seg+1],RotationMatrix[seg \[Pi]/2].#&/@{{1,Tan[\[Phi]/2]},{Cos[\[Phi]],Sin[\[Phi]]}}];
If[Length[m]==2,pts=m+#&/@pts,pts=m+#&/@Transpose[Append[Transpose[pts],ConstantArray[0,Length[pts]]]]];
w=Join[Take[{1,1/Sqrt[2],1,1/Sqrt[2],1,1/Sqrt[2],1},2 seg+1],{Cos[\[Phi]/2],1}];
k=Join[{0,0,0},Riffle[#,#]&@Range[seg+1],{seg+1}];
BSplineCurve[pts,SplineDegree->2,SplineKnots->k,SplineWeights->w]]/;Length[m]==2||Length[m]==3





DisplayTau[]:=
 Block[{i},

  If[$TAU$==Null,
    Print["The vector is undefined."];
    Return[]];

  EPrint[$TAU$, "tau"];

];
End[]
EndPackage[]
