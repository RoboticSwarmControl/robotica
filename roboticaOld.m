(*
         Robotica

A Mathematica package for the analysis and design of robots.
Author: John Nethery, nethery@robot1.ge.uiuc.edu

Copyright 1993 Board of Trustees, University of Illinois
All rights reserved.

*)

BeginPackage["robotica`"]
Off[Replace::rep]

(*
   Define the help strings for the functions that will be available to
   the user.  Define also the variables that we are going to export.
   We should eventually add help strings for the variables as wel...
*)
a::usage = ""
d::usage = ""
alpha::usage = ""
theta::usage = ""
A::usage = ""
T::usage = ""
J::usage = ""
Jvw::usage = ""
z::usage = ""
o::usage = ""
M::usage = ""
MU::usage = ""
CM::usage = ""
G::usage = ""
gravity::usage = ""
mass::usage = ""
com::usage = ""
inertia::usage = ""
Jc::usage = ""
Jvc::usage = ""
Jwc::usage = ""
c::usage = ""
simplot::usage =""
manplot::usage =""
robotplot::usage = ""
driveplot::usage = ""

TellFunctions::usage = ""

RElp::usage = "RElp[vars_List, options_List:{}] plots manipulability or force
ellipsoids over a range of variables, subject to the options listed."

SetRanges::usage = "SetRanges[xrange, yrange, zrange] sets a consistent
range for viewlimits used during animations."

SimplifyExpression::usage = "SimplifyExpression[x_] tries to reduce the
expression 'x' to its most reduced form."

Response::usage = "Response[intime_List, incond_List] solves
the dynamics equations for a numerical response over a period of time with
certain initial conditions."

SaveResponse::usage = "SaveResponse[file_String, time_List, inc_:.1] saves
numerical data on the dynamics response to a file over a period of time.
The response is first calculated with Response."

DisplayTau::usage = "DisplayTau[] shows the elements of the Response
tau vector."

GetInputTau::usage = "GetInputTau[file_String] loads a definition file
for the tau vector used in Response for solving the dynamics equations."

LoadAnim::usage = "LoadAnim[file_String] loads an animation previously saved
with SaveAnim from 'file'."

SaveAnim::usage = "SaveAnim[file_String] saves an animation genrated with
RElp[], SimDrive[], etc. to 'file'."

ShowAnim::usage = "ShowAnim[] shows the current animation that was either
loaded with LoadAnim[], or generated with RElp[], SimDrive[], etc."

ShowRobot::usage = "ShowRobot[vars_List, options_List:{}] displays a drawing
or animation of the robot loaded with DataFile[] over a range of
joint parameters."

SeqShowRobot::usage = "SeqShowRobot[vars_List, options_List:{}] displays a
 drawing or animation of the robot loaded with DataFile[] over a range
 of joint parameters, taking each joint parameter sequentially."

Planar::usage = "Return an integer describing the dimensions of the space
in which the robot can move."

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

PrintInputData::usage = "PrintInputData[] prints the current robot
data set in tabular form to the screen."

FKin::usage = "FKin[] generates the A and T matrices, as well as the Jacobian
for the current input data set."

ELDynamics::usage =
"ELDynamics[] generates Euler Lagrange dynamics equations provided
that the A, T, and Jacobian matrices are generated."

SDynamics::usage =
"SDynamics[] simplifies the dynamics equations generated with ELDynamics."

RevJoint::usage = "RevJoint[file_String] loads a revolute joint shape from
file if specified, otherwise resets to standard cylinder shape."

PrisJoint::usage = "PrisJoint[file_String] loads a prismatic joint shape from
file if specified, otherwise resets to standard block shape."

LinkShape::usage = "LinkShape[file_String] loads a interjoint shape from
file if specified, otherwise resets to standard block shape."

(*
WireE::usage = "WireE[{xc_, yc_, zc_:0}, {x1_, y1_, z1_},
{x2_, y2_, z2_}, {x3_, y3_, z3_}, fine_:0.1] returns a set
of lines approximating an ellipsoid at xc, yc, zc, with axes x1...z3.  A
smaller value for fine gives a smoother ellipse."
*)

ClearPrisJoint::usage = "ClearPrisJoint[] clears the current definition for
the prismatic joint shape."

ClearRevJoint::usage = "ClearRevJoint[] clears the current definition for the
revolute joint shape."

ClearLinkShape::usage = "ClearLinkShape[] clears the current definition for
the interjoint shape."

SimDrive::usage = "SimDrive[file_String, options_List:{}] takes a set
of simnon input data from 'file', and use it to update arm parameters,
showing the results graphically."

SimPlot::usage = "SimPlot[x_Symbol, ys_List, options_List:{}] uses the
simnon input data set to plot x vs the list of variables in ys."

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
Sin[q1+q2-q3] --> S12-3."

(*
SimplifyDerivativeNotation::usage =
"SimplifyDerivativeNotation[] simplifies the notation of the derivative
of the position vector {q1,q2,..} with respect to time t,
D[qi,t,NonConstants{q1,q2,...}] --> Dqi."
*)

(*
Adopted From Trigonometry.m
TrigFactor::usage = "TrigFactor[expr] writes sums of trigonometric
	functions as products."
*)



Begin["`Private`"]

 (*
   Here are some variables which define the overall state of the Robotica
   session.  They should be available to all the functions in the package.
 *)

$DATAFILE$ = "NO";
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
$VERSION$ = "3.62";
(* Version 3.61 is the same as 3.60, with a few small errors fixed
to squelch warnings which appear on newer versions of Mathematica. *)
(* Version 3.62 is the same as 3.61, with SimplifyTrigNotation[] modified to use subscripts. *)
$ROBGUY$ = "m-spong@uiuc.edu";

Print["Robotica version ", $VERSION$, "."];
Print["Copyright 1993 Board of Trustees, University of Illinois"];
Print["All rights reserved."];
Print["Email questions, comments, or concerns to ", $ROBGUY$, "."];

(*
  SimplifyTrigNotation replaces Sin[] and Cos[] with S and C, and the
  parameter with the same parameter with its first character removed.
  i.e. q1 -> 1, q10 -> 10, d3 ->3, qtest -> test
  Don't change compound expressions: Cos[4 r] does not change to Cr,
  Also, preserve one character variables:  Cos[w] = Cos[w]
*)

SimplifyTrigNotation[]:=
	Do[
	Unprotect[Cos];
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

	Format[Cos[x_]] := Subscript["c",StringJoin[Drop[Characters[ToString[x]],1]]]
           /; (Head[x] == Symbol) && (StringLength[ToString[x]] > 1);

	Protect[Cos];

	Unprotect[Sin];
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

	Format[Sin[x_]] := Subscript["s",StringJoin[Drop[Characters[ToString[x]],1]]]
           /; (Head[x] == Symbol) && (StringLength[ToString[x]] > 1);
	Protect[Sin];
	]


(*
SimplifyDerivativeNotation[]:=
	Block[{i},
	Unprotect[D];
        For[i=1, i<=dof, i++,

        Format[D[theta[i],t,NonConstants->joints]]:=
               StringJoin["D",ToString[theta[i]]];
        ];

	Protect[D]
     ]
*)
(*
`TrigCanonicalRel = Integrator`TrigCanonicalRel ~Join~
*)

(*
  Define the set of trig simplification rules to use for Trig reduction
*)
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
(* Protect[TrigFactorRel] XYZZY *)
(* TrigFactor[e_] := FixedPoint[(# /. TrigCanonicalRel /. TrigFactorRel)&, e] *)

(*
   TPrint prints all the T Matrices to a file or to the screen.
   If the filename is $, the default file name is used
*)
TPrint[name_String:""] :=
   Block[{i,j,temp,file},
     file=name;
     If[file =!= "",
       If[file == "$", file = ToString[$FILE$], $FILE$=file]];
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

       Print[OutputForm[temp]];
       Print[""]]
       ]
      ]
     ]
    ]

(*
   APrint prints all the A matrices to the screen or to a file.
  If the filename is $, the default file name is used
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

       Print[OutputForm[temp]];
       Print[""]]
      ]
     ]

(*
   MPrint prints any matrix with a label to the screen or to a file.
   If the filename is $, the default file name is used
*)
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
	]

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
                           PrintInputData[];
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

(*
  reset variables
*)
ResetState[] := Block[{},
   $DATAFILE$ = "NO";
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
   Clear[Jc, Jvc, Jwc, MU, M, G, CM, c];

   ];

(*
  display a table of the various parameters read in
*)
PrintInputData[]:=
	Block[{i,j,k,jointVector,jointtypeVector,aColumn,alphaColumn,
               dColumn, thetaColumn, linkVector, massColumn, temp2,
               lcColumn},

        If [$DATAFILE$ == "NO",
          Print["You must first load a data file."];
          Return[];
          ];
	Print[" "];
	Print["Kinematics Input Data"];
	Print["---------------------"];
	Print[" "];
	jointVector=ColumnForm[Prepend[
	   Prepend[Table[j,{j,dof}]," "],"Joint"]
	  ];
	jointtypeVector=ColumnForm[Prepend[
	   Prepend[Table[jointtype[j],{j,dof}]," "],"Type"]
	  ];
	aColumn=ColumnForm[Prepend[
	   Prepend[Table[FortranForm[a[j]],{j,dof}]," "],"a"]
	  ];
	alphaColumn=ColumnForm[ Prepend[
		Prepend[Table[FortranForm[alpha[j]],{j,dof}]," "],"alpha"]
	      ];
	dColumn=ColumnForm[Prepend[
	   Prepend[Table[FortranForm[d[j]],{j,dof}]," "],"d"]
	  ];
	thetaColumn=ColumnForm[ Prepend[
		Prepend[Table[FortranForm[theta[j]],{j,dof}]," "],"theta"]
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
		Print[" "];
		Print[" "];
		Print["Dynamics Input Data"];
		Print["-------------------"];
		Print[" "];
                Print["Gravity vector: [",gravity[1][[1]], ", ",
                                          gravity[1][[2]], ", ",
                                          gravity[1][[3]], "]" ];
		Print[" "];


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
        If[ $DATAFILE$ == "NO", Print["You must load a data file first..."];
                                Return[]];

	FormAllAs[];
	FormAllTs[];
	FormTheJacobianJ[];
        $FKINRUN$ = "YES";
        ]

(*
  The A matrices are just a fill in the blank procedure
*)
FormAllAs[]:=
	Block[{i},
	Print[" "];
	Print[" "];
	Print["A Matrices Formed :"];
	Print[" "];
	Do[Print["A[",i,"]"];
	   A[i]=FormA[a[i],alpha[i],d[i],theta[i]],{i,1,dof}]
	]

FormA[a_,alpha_,d_,theta_] :=

{ {Cos[theta], -Sin[theta] Cos[alpha], Sin[theta] Sin[alpha], a Cos[theta]},
  {Sin[theta], Cos[theta] Cos[alpha], -Cos[theta] Sin[alpha], a Sin[theta]},
  {0,          Sin[alpha],            Cos[alpha],             d},
  {0,          0,                     0,                      1} }


FormAllTs[]:=
	Block[{i,j},
	Print[" "];
	Print[" "];
	Print["T Matrices Formed :"];
	Print[" "];
	T[0,0]=IdentityMatrix[4];
	Print["T[0,0]"];
	For[i=0,i < dof, i++,
	For[j=1,j<=dof , j++ ,
                If[j>i,Print["T[",i,",",j,"]"];
		T[i,j]=TrigFactor[FormTij[i,j]]]
			]
		]
	]

(*
  Recursively form the T matrix T[i,j]
*)
FormTij[k_,l_]:= If[(l-k)==1, A[l], A[k+1] . FormTij[k+1, l] ];

FormTheJacobianJ[]:= Block[{i,j,v,w,Jvw},

        Do[z[j] = {T[0,j][[1,3]], T[0,j][[2,3]], T[0,j][[3,3]]};
           o[j] = {T[0,j][[1,4]], T[0,j][[2,4]], T[0,j][[3,4]]},
           {j,0,dof}];

        For[j=1, j<=dof, j++,
          If[jointtype[j]=="revolute",
           v=Cross3[z[j-1],o[dof]-o[j-1]];
           w=z[j-1];
           Jvw[j]=Join[v,w]
            ];

          If[jointtype[j]=="prismatic",
           v=z[j-1];
           w={0,0,0};
           Jvw[j]=Join[v,w]
            ]];

        J = Transpose[Table[Jvw[j], {j, 1, dof}]];
	Print[" "];
	Print[" "];
	Print["Jacobian Formed : "];
	Print[" "];
	Print["Jacobian  J","(6","x",dof,")"];
	Print[" "]
	]

(*
  A simple cross product calculator
*)
Cross3[x_,y_]:=
	Return[{x[[2]]y[[3]]-x[[3]]y[[2]],
                x[[3]]y[[1]]-x[[1]]y[[3]],
                x[[1]]y[[2]]-x[[2]]y[[1]]}]

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

	FormCMatrix[];
	Print[" "];
	Print["C Matrix CM(",dof," x ",dof,") Formed. "];

	FormGravityVector[];
	Print[" "];
	Print["Gravity Vector G(",dof," x 1) Formed. "];

(*
	SimplifyDerivativeNotation[];
*)
        $DYNRUN$ = "YES";
	]

(*
  Run simplify on the dynamics info
*)
SDynamics[] :=
      Do[
        If[ $DYNRUN$ == "NO", Print["You must run ELDynamics[] first..."];
                               Return[]];
        If[ $DYNRUN$ == "CANT", Print["You must run ELDynamics[] first..."];
                               Return[]];
        Print["Working on the intertia matrix..."];

(* M is created by SimplifyInertiaMatrix *)
	SimplifyInertiaMatrix[MU];

        Print["Working on the Christoffel symbols..."];
	FormChristoffelSymbols[M];
        c=SimplifyExpression[c];

        Print["Working on the C matrix..."];
	FormCMatrix[];
        CM=SimplifyExpression[CM];

        Print["Working on the gravity vector..."];
	FormGravityVector[];
        G=SimplifyExpression[G];
       ];

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
  Make the C Matrix, force joint variables to be funtions of time
*)
FormCMatrix[]:=
	Block[{i,j,k},
	CM=Table[0,{dof},{dof}];

	Do[
	CM[[k,j]]=Sum[c[[i,j,k]] *
                D[q[i][Global`t], Global`t]
(*
		D[q[i],t,NonConstants -> joints]
*)
		,{i,1,dof}]
	,{k,1,dof},{j,1,dof}];
	Print[" "];
	]

(*
  Calculate the gravity vector
*)
FormGravityVector[]:=
	Block[{i,j},
	G=Table[0,{dof}];
	Do[
	G[[i]] = TrigFactor[Sum[
			mass[j] gravity[j].
				Jvc[j][[Range[1,3],Range[i,i]]]
				,{j,1,dof}]]
	,{i,1,dof}];
	G=Flatten[G] ;
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
    {u, md, v} = SingularValues[N[tj]];

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
  Determine which dimensions the robot can move in by
  examining x, y, z, components of Jacobian
*)
Planar[] :=
         Block[{x,y,z},

     If [$FKINRUN$ == "NO",
       Print["You must run FKin[] first."];
       Return[]];

         If[Length[Select[J[[1]], # =!= 0&]] != 0, x=1, x=0];
         If[Length[Select[J[[2]], # =!= 0&]] != 0, y=2, y=0];
         If[Length[Select[J[[3]], # =!= 0&]] != 0, z=4, z=0];
         Return[x+y+z];
];

(*
  This function multiplies the x,y,z coordinates in a shape by {a,b,c}
  respectively.
*)
Affine3DShape[shape_, vec_List] :=
  Block[{tvec = N[vec]},
   shape /. { Point[pt_]  :>  Point[pt * vec],
              Line[ln_]    :> Line[ln * Table[vec, {Length[ln]}]],
              Polygon[pg_] :> Polygon[pg * Table[vec, {Length[pg]}]]}
       ];

(*
  This function adds the x,y,z coordinates in a shape to {a,b,c}
  respectively.
*)
Translate3DShape[shape_, vec_List] :=
  Block[{tvec = N[vec]},
   shape /. { Point[pt_]  :>  Point[pt + vec],
              Line[ln_]    :> Line[ln + Table[vec, {Length[ln]}]],
              Polygon[pg_] :> Polygon[pg + Table[vec, {Length[pg]}]]}
       ];

(*
  This function rotates a shape by a rotation matrix.
  This function is a slightly modified form of RotateShape
  in Shapes.m in the standard Mathematica packages.
*)
Rotate3DShape[shape_, vec_List] :=
  Block[{tvec = N[vec]},
   shape /. { poly:Polygon[_] :> Map[(tvec . #)&, poly, {2}],
              line:Line[_]    :> Map[(tvec . #)&, line, {2}],
              point:Point[_]  :> Map[(tvec . #)&, point, {1}]}
       ];


(*
  Drop coordinates from a shape depending on the Planar number
  e.g., for {x,y,z}, drop z if x-y planar.
*)
Planarize[shape_, d_Integer] :=
 Block[{},
  (* the x-y plane, x axis, y axis *)
  If[d == 3 || d == 1 || d == 2,
  Return[ shape /. {poly:Polygon[_] :> Map[(Take[#, {1,2}])&, poly, {2}],
                    line:Line[_]    :> Map[(Take[#, {1,2}])&, line, {2}],
                    point:Point[_]  :> Map[(Take[#, {1,2}])&, point, {1}]}]];



  (* the x-z plane, z axis *)
  If[d == 5 || d == 4,
  Return[ shape /. {poly:Polygon[_] :> Map[(Drop[#, {2}])&, poly, {2}],
                    line:Line[_]    :> Map[(Drop[#, {2}])&, line, {2}],
                    point:Point[_]  :> Map[(Drop[#, {2}])&, point, {1}]} ]];

  (* the y-z plane *)
  If[d == 6,
  Return[ shape /. {poly:Polygon[_] :> Map[(Drop[#, {1}])&, poly, {2}],
                    line:Line[_]    :> Map[(Drop[#, {1}])&, line, {2}],
                    point:Point[_]  :> Map[(Drop[#, {1}])&, point, {1}]} ]];

  (* the x-y-z space, return it all *)
  If[d == 7,
  Return[shape]];

  Print["Not supported..."];
  Return[{{0,0}}];
];

(*
  Drop coordinates from a list
*)
PlanarizeList[list_, d_] :=
 Block[{},
  If[d==5 || d==4,
  Return[Flatten[Map[(Drop[#, {2,2}])&, {list}]]]];

  If[d==3 || d==1 || d==2,
  Return[Flatten[Map[(Take[#, {1,2}])&, {list}]]]];

  If[d==6,
  Return[Flatten[Map[(Take[#, {2,3}])&, {list}]]]];

  If[d==7,
  Return[list]];
];

(*
  Draw a rotated ellipse with a series of line segments, given the
  axes
*)
WireE[{xc_, yc_, zc_:0}, {x1_, y1_, z1_},
      {x2_, y2_, z2_}, {x3_, y3_, z3_}, fine_:0.1] :=
 Block[{t,i, bandx, bandy, bandz, normx, normy, normz, a, b, c, tcs },
   a = (x1 x1 + y1 y1 + z1 z1)^.5;
   b = (x2 x2 + y2 y2 + z2 z2)^.5;
   c = (x3 x3 + y3 y3 + z3 z3)^.5;
   tcs = Table[{Sin[t], Cos[t]},  {t, 0, 2*Pi, fine}];
   AppendTo[tcs, {0,1}]; (* Sin, Cos of 0, to close the loop *)

   bandx = Map[{0, b #[[1]], c #[[2]]}&, tcs];
   bandy = Map[{a #[[2]], 0, c #[[1]]}&, tcs];
   bandz = Map[{a #[[2]], b #[[1]], 0}&, tcs];

   If[(x3==0) && (y3==0) && (z3==0),
    normz = {0,0,0},
    normz = {x3,y3,z3}/Long[{x3,y3,z3}]];

   If[(x1==0) && (y1==0) && (z1==0),
    normx = {0,0,0},
    normx = {x1,y1,z1}/Long[{x1,y1,z1}]];

   If[(x2==0) && (y2==0) && (z2==0),
    normy = {0,0,0},
    normy = {x2,y2,z2}/Long[{x2,y2,z2}]];

   bandz = {Line[bandz], Line[{{a + a/7, 0,0}, {-a - a/7,0,0},
                               {0,0,0}, {0,b + b/7,0}, {0,-b - b/7,0}}]};
   bandx = {Line[bandx], Line[{{0, 0,c + c/7}, {0,0,-c - c/7},
                               {0,0,0}, {0,b + b/7,0}, {0,-b - b/7,0}}]};
   bandy = {Line[bandy], Line[{{a + a/7, 0,0}, {-a - a/7,0,0},
                               {0,0,0}, {0,0,c + c/7}, {0,0,-c - c/7}}]};
   bandx = Rotate3DShape[bandx, Transpose[{normx, normy, normz}]];
   bandy = Rotate3DShape[bandy, Transpose[{normx, normy, normz}]];
   bandz = Rotate3DShape[bandz, Transpose[{normx, normy, normz}]];

   bandx = Translate3DShape[bandx, {xc, yc, zc}];
   bandy = Translate3DShape[bandy, {xc, yc, zc}];
   bandz = Translate3DShape[bandz, {xc, yc, zc}];
   If[(x3==0) && (y3==0) && (z3==0),
    Return[{bandz, Null}],
    Return[{{bandx, bandy, bandz}, Null}]];
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
  Load a revolute joint shape
*)
RevJoint[file_String:""] :=
  Block[{f},
   If [file != "",
     f = OpenRead[file];
     If[f == Null || f == $Failed,
        Print["Can't find your file..."];
        Return[]];
     $REVJOINT$ = ReadList[f, Expression];
     Close[f];
     Print["Revolute joint shape loaded..."]];

   If [file == "",
     $REVJOINT$ = StandardRev[];
     $REVJOINT$ = Affine3DShape[$REVJOINT$, {.5, .5, 1}];
     Print["Standard revolute joint shape loaded."]];
    ];

(*
  Load a prismatic joint shape
*)
PrisJoint[file_String:""] :=
  Block[{f},
   If [file != "",
     f = OpenRead[file];
     If[f == Null || f == $Failed,
        Print["Can't find your file..."];
        Return[]];
     $PRISJOINT$ = ReadList[f, Expression];
     Close[f];
     Print["Prismatic joint shape loaded..."];];

   If [file == "",
    $PRISJOINT$ = StandardPris[];
    Print["Standard prismatic joint shape loaded."]];
  ];

(*
  Load an interjoint shape
*)
LinkShape[file_String:""] :=
  Block[{f},
   If [file != "",
     f = OpenRead[file];
     If[f == Null || f == $Failed,
        Print["Can't find your file..."];
        Return[]];
     $LINK$ = ReadList[f, Expression];
     Close[f];
     Print["Interjoint shape loaded..."];];

   If [file == "",
    $LINK$ = 1;
    Print["Standard interjoint shape loaded."]];
  ];

(*
EndEff[file_String] :=
  Block[{f},
   f = OpenRead[file];
   If[f == Null || f == $Failed,
      Print["Can't find your file..."];
      Return[]];
   $ENDEFF$ = ReadList[f, Expression];
   Close[file];
   Print["End effector shape loaded..."];
  ];
*)

(*
  The standard Revolute joint is a cylinder shape, generated with Cylinder
  from shapes.m
*)
StandardRev[] := Block[{},
{{AbsoluteThickness[1], RGBColor[0, 0, 0],
    Line[{{0.951056516295153, 0.3090169943749474, 1},
      {0.951056516295153, 0.3090169943749474, 0},
      {0.809016994374947, 0.5877852522924732, 0},
      {0.809016994374947, 0.5877852522924732, 1},
      {0.951056516295153, 0.3090169943749474, 1}}]},
   {AbsoluteThickness[1], RGBColor[0, 0, 0],
    Line[{{0.809016994374947, 0.5877852522924732, 1},
      {0.809016994374947, 0.5877852522924732, 0},
      {0.5877852522924732, 0.809016994374947, 0},
      {0.5877852522924732, 0.809016994374947, 1},
      {0.809016994374947, 0.5877852522924732, 1}}]},
   {AbsoluteThickness[1], RGBColor[0, 0, 0],
    Line[{{0.5877852522924732, 0.809016994374947, 1},
      {0.5877852522924732, 0.809016994374947, 0},
      {0.3090169943749474, 0.951056516295153, 0},
      {0.3090169943749474, 0.951056516295153, 1},
      {0.5877852522924732, 0.809016994374947, 1}}]},
   {AbsoluteThickness[1], RGBColor[0, 0, 0],
    Line[{{0.3090169943749474, 0.951056516295153, 1},
      {0.3090169943749474, 0.951056516295153, 0}, {0, 1., 0}, {0, 1., 1},
      {0.3090169943749474, 0.951056516295153, 1}}]},
   {AbsoluteThickness[1], RGBColor[0, 0, 0],
    Line[{{0, 1., 1}, {0, 1., 0},
      {-0.3090169943749474, 0.951056516295154, 0},
      {-0.3090169943749474, 0.951056516295154, 1}, {0, 1., 1}}]},
   {AbsoluteThickness[1], RGBColor[0, 0, 0],
    Line[{{-0.3090169943749474, 0.951056516295154, 1},
      {-0.3090169943749474, 0.951056516295154, 0},
      {-0.587785252292473, 0.809016994374947, 0},
      {-0.587785252292473, 0.809016994374947, 1},
      {-0.3090169943749474, 0.951056516295154, 1}}]},
   {AbsoluteThickness[1], RGBColor[0, 0, 0],
    Line[{{-0.587785252292473, 0.809016994374947, 1},
      {-0.587785252292473, 0.809016994374947, 0},
      {-0.809016994374947, 0.5877852522924733, 0},
      {-0.809016994374947, 0.5877852522924733, 1},
      {-0.587785252292473, 0.809016994374947, 1}}]},
   {AbsoluteThickness[1], RGBColor[0, 0, 0],
    Line[{{-0.809016994374947, 0.5877852522924733, 1},
      {-0.809016994374947, 0.5877852522924733, 0},
      {-0.951056516295153, 0.3090169943749475, 0},
      {-0.951056516295153, 0.3090169943749475, 1},
      {-0.809016994374947, 0.5877852522924733, 1}}]},
   {AbsoluteThickness[1], RGBColor[0, 0, 0],
    Line[{{-0.951056516295153, 0.3090169943749475, 1},
      {-0.951056516295153, 0.3090169943749475, 0}, {-1., 0, 0}, {-1., 0, 1},
      {-0.951056516295153, 0.3090169943749475, 1}}]},
   {AbsoluteThickness[1], RGBColor[0, 0, 0],
    Line[{{-1., 0, 1}, {-1., 0, 0},
      {-0.951056516295153, -0.3090169943749477, 0},
      {-0.951056516295153, -0.3090169943749477, 1}, {-1., 0, 1}}]},
   {AbsoluteThickness[1], RGBColor[0, 0, 0],
    Line[{{-0.951056516295153, -0.3090169943749477, 1},
      {-0.951056516295153, -0.3090169943749477, 0},
      {-0.809016994374947, -0.587785252292473, 0},
      {-0.809016994374947, -0.587785252292473, 1},
      {-0.951056516295153, -0.3090169943749477, 1}}]},
   {AbsoluteThickness[1], RGBColor[0, 0, 0],
    Line[{{-0.809016994374947, -0.587785252292473, 1},
      {-0.809016994374947, -0.587785252292473, 0},
      {-0.5877852522924733, -0.809016994374947, 0},
      {-0.5877852522924733, -0.809016994374947, 1},
      {-0.809016994374947, -0.587785252292473, 1}}]},
   {AbsoluteThickness[1], RGBColor[0, 0, 0],
    Line[{{-0.5877852522924733, -0.809016994374947, 1},
      {-0.5877852522924733, -0.809016994374947, 0},
      {-0.3090169943749475, -0.951056516295153, 0},
      {-0.3090169943749475, -0.951056516295153, 1},
      {-0.5877852522924733, -0.809016994374947, 1}}]},
   {AbsoluteThickness[1], RGBColor[0, 0, 0],
    Line[{{-0.3090169943749475, -0.951056516295153, 1},
      {-0.3090169943749475, -0.951056516295153, 0}, {0, -1., 0},
      {0, -1., 1}, {-0.3090169943749475, -0.951056516295153, 1}}]},
   {AbsoluteThickness[1], RGBColor[0, 0, 0],
    Line[{{0, -1., 1}, {0, -1., 0},
      {0.3090169943749472, -0.951056516295154, 0},
      {0.3090169943749472, -0.951056516295154, 1}, {0, -1., 1}}]},
   {AbsoluteThickness[1], RGBColor[0, 0, 0],
    Line[{{0.3090169943749472, -0.951056516295154, 1},
      {0.3090169943749472, -0.951056516295154, 0},
      {0.587785252292473, -0.809016994374948, 0},
      {0.587785252292473, -0.809016994374948, 1},
      {0.3090169943749472, -0.951056516295154, 1}}]},
   {AbsoluteThickness[1], RGBColor[0, 0, 0],
    Line[{{0.587785252292473, -0.809016994374948, 1},
      {0.587785252292473, -0.809016994374948, 0},
      {0.809016994374947, -0.5877852522924734, 0},
      {0.809016994374947, -0.5877852522924734, 1},
      {0.587785252292473, -0.809016994374948, 1}}]},
   {AbsoluteThickness[1], RGBColor[0, 0, 0],
    Line[{{0.809016994374947, -0.5877852522924734, 1},
      {0.809016994374947, -0.5877852522924734, 0},
      {0.951056516295153, -0.3090169943749476, 0},
      {0.951056516295153, -0.3090169943749476, 1},
      {0.809016994374947, -0.5877852522924734, 1}}]},
   {AbsoluteThickness[1], RGBColor[0, 0, 0],
    Line[{{0.951056516295153, -0.3090169943749476, 1},
      {0.951056516295153, -0.3090169943749476, 0}, {1., 0, 0}, {1., 0, 1},
      {0.951056516295153, -0.3090169943749476, 1}}]},
   {AbsoluteThickness[1], RGBColor[0, 0, 0],
    Line[{{1., 0, 1}, {1., 0, 0}, {0.951056516295153, 0.3090169943749474, 0},
      {0.951056516295153, 0.3090169943749474, 1}, {1., 0, 1}}]}}
];

(*
  The standard prismatic joint is a block
*)
StandardPris[] := Block[{},

 {AbsoluteThickness[1], RGBColor[0, 0, 0],
    Line[{{-.25,-.25, 0}, {-.25,.25,0}, {.25,.25,0},
          {.25,-.25,0}, {-.25,-.25,0}}],
    Line[{{-.25,-.25, .5}, {-.25,.25,.5}, {.25,.25,.5},
          {.25,-.25,.5}, {-.25,-.25,.5}}],
    Line[{{.25,.25,0}, {.25,.25,.5}}],

    Line[{{-.25,.25,0}, {-.25,.25,.5}}],
    Line[{{-.25,-.25,0}, {-.25,-.25,.5}}],
    Line[{{.25,-.25,0}, {.25,-.25,.5}}],
    Line[{{.25, 0, 0}, {.25, 0, .5}}],
    Line[{{-.25, 0, 0}, {-.25, 0, .5}}],
    Line[{{0, .25, 0}, {0, .25, .5}}],
    Line[{{0, -.25, 0}, {0, -.25, .5}}],
    Line[{{0, .25, .5}, {0, -.25, .5}}],
    Line[{{0, .25, 0}, {0, -.25, 0}}],
    Line[{{.25, 0, .5}, { -.25, 0, .5}}],
    Line[{{.25, 0, 0}, { -.25, 0, 0}}]
    }

];

ClearPrisJoint[] := Block[{},
 $PRISJOINT$ = Null];

ClearRevJoint[] := Block[{},
 $REVJOINT$ = Null];

ClearLinkShape[] := Block[{},
 $LINK$ = Null];

(*
  Read in general simulation files from simnon, etc.
  The format is line 1 contains a quoation mark and a list of
  variables, the first of which must be t (time) separated
  by spaces (not tabs).  After that,
  there can be any number of comment lines that start with a
  quotation mark.  Then, the data must come in columns in the order
  given in the first line.
  Example:
  " t q1 q2 d3
  " time, joint1, joint2, pris3
  0 1 1 1
  1 2 2 2
 ...
*)
SimDrive[file_String, options_List:{}] :=
 Block[{f, i, j, line, opts, temp, alist, anim, dim, current, currenttime,
        old, graph, last, stat, delta, len, lasttime,
        tempstream, fval, frameopt},

  opts = options;
  If[StringMatchQ[file, "$"], old = True,
    f = OpenRead[file]; old = False;
    If[f == Null || f == $Failed,
      Print["Couldn't find file..."];
      Return[]];
   ];

  If[old && Length[$SIMVARLIST$] == 0,
   Print["No simulation data file loaded yet..."];
   Return[]];

  graph = {};
  stat = 0;
  delta = .2;

  If [MemberQ[opts, Global`frame], fval = True, fval = False];

  If[Length[opts] > 0,
   If[NumberQ[N[opts[[-1]]]],
    delta = N[opts[[-1]]]]];

(* Look for a quotation mark, a space, and variables *)
 If[!old,
  line = Read[f, String];
  If[StringLength[line] < 4, Print["Bad input file format."];
    Close[f]; Return[]];

(*
   Get the names of the variables, skip over t, which is always assumed
   to be first.
*)

  i = StringLength[line];
  line = Take[Characters[line], {4, i}];

  temp="";
  simset={"t"};
  For[j=1, j<=Length[line], j++,
   If[ !StringMatchQ[line[[j]], " "],
    temp = StringJoin[temp, line[[j]]],
    If[!StringMatchQ[temp, ""],
     AppendTo[simset, temp]; temp=""] ]];

  If[!StringMatchQ[temp, ""], AppendTo[simset, temp]];

  Print["Attempting to read values for these parameters:"];
  For[j=1, j<=Length[simset], j++, Print[simset[[j]]];];
  Print["Working..."];

(* Get rid of " empty comment line(s) *)
  line = "\"";
  While[StringMatchQ[line, "\"*"],
   line = Read[f, String]];

  $SIMVALUES$ = values = ReadList[f, Table[Number, {Length[simset]}]];

(* prepend the first line that wasn't a comment line to values *)
  tempstream = StringToStream[line];
  $SIMVALUES$ = values = Prepend[values, Read[tempstream,
                                 Table[Number, {Length[simset]}]]];
  Close[tempstream];

  ];  (* end of get new set of values *)

  If[!old,
   $SIMVARLIST$ = {};
   For[i=1, i<=Length[simset], i++,
    simset[[i]] = ToExpression[simset[[i]]];
    AppendTo[$SIMVARLIST$, simset[[i]]]];
   ];

  If[old, simset = $SIMVARLIST$];

  If[!MemberQ[opts, Global`trace] && !MemberQ[opts, Global`arm],
    Print["Data file loaded..."];
    If[!old,
     Close[file]];
    Return[]];

  If[$FKINRUN$ == "NO",
    Print["You must run FKin[] first."];
    Return[]];

  dim=Planar[];

  If [dim == 7,
    frameopt = Boxed->fval,
    frameopt = Frame->fval];

(* 9/15/93 get the time to use as update parameter *)
(* JFN *)
  last = {-1000, -1000, -1000};
  lasttime=values[[1]][[1]];
  centers[0]={0,0,0};

  If [MemberQ[$ContextPath, "Graphics`Animation`"],Null,

   If [MemberQ[opts, Global`animate],
     Print["Animation functions haven't been loaded...\n"];
     opts = Delete[opts, Position[opts, Global`animate]]]];

  anim = {};
  alist = {};

  If[dim==3,
     axeslabel={"x", "y"};
     $RANGES$ = {$XRANGE$, $YRANGE$}];

  If[dim==4,
     axeslabel={"x", "z"};
     $RANGES$ = {$XRANGE$, $ZRANGE$}];

  If[dim==5,
     axeslabel={"x", "z"};
     $RANGES$ = {$XRANGE$, $ZRANGE$}];

  If[dim==6,
     axeslabel={"y", "z"};
     $RANGES$ = {$YRANGE$, $ZRANGE$}];

  If[dim==7,
     axeslabel={"x", "y", "z"};
     $RANGES$ = {$XRANGE$, $YRANGE$, $ZRANGE$}];


  For[j=1, j<=Length[values], j++,
    evallist={};
(* 9/15/93 update the current time -JFN*)
    currenttime = values[[j]][[1]];
    For[i=1, i<=Length[simset], i++,
      AppendTo[evallist, simset[[i]] -> values[[j]][[i]]]];

  If[N[100j/Length[values]] - stat > 10,
    Print[Round[N[100j/Length[values]]],"%"];
    stat = N[100j/Length[values]]];

    For[i=1, i<=dof, i++,
     centers[i] = { T[0,i][[1,4]], T[0,i][[2,4]], T[0,i][[3,4]] } /. evallist];

  For[i=1, i<=dof, i++,
   If[!NumberQ[Plus @@ N[centers[j]]],
      Print["Can't get numbers - are all variables set?"];
      Print[centers[j]];
      Close[f];
      Return[]];
     ];

(* 9/15/93 make time the determiner of when to update rather than distance *)
(* JFN *)
  If[ currenttime - lasttime >delta,

   If[MemberQ[opts, Global`trace],
     If[last[[1]] != -1000,
      AppendTo[graph, Gr[{AbsoluteThickness[1],
                         {RGBColor[0,0,0], Planarize[
                          Line[{last, centers[dof]}], dim]}}, dim]];

      AppendTo[alist, Planarize[Line[{last, centers[dof]}], dim]];

    If [MemberQ[opts, Global`animate] && !MemberQ[opts, Global`arm],
      AppendTo[anim,  Append[Gr[alist, dim],
          List[
          Rule[ Axes, True],
          Rule[ AspectRatio, Automatic],
          Rule[ PlotRange, $RANGES$],
          frameopt,
          Rule[ PlotLabel, "Sim results"],
          Rule[ AxesLabel, axeslabel]]]]];

    ]]; (* != -1000 *)


  If[MemberQ[opts, Global`arm],
    AppendTo[graph, DrawRobot[evallist, dim]];

    If [MemberQ[opts, Global`animate] && !MemberQ[opts, Global`trace],
   AppendTo[anim,  Append[Gr[DrawRobot[evallist, dim, 0], dim],
          List[
          Rule[ Axes, True],
          Rule[ AspectRatio, Automatic],
          Rule[ PlotRange, $RANGES$],
          frameopt,
          Rule[ PlotLabel, "Sim results"],
          Rule[ AxesLabel, axeslabel]]]]];

    If [MemberQ[opts, Global`animate] && MemberQ[opts, Global`trace],
   AppendTo[anim,  Append[Gr[{DrawRobot[evallist, dim, 0], alist}, dim],
          List[
          Rule[ Axes, True],
          Rule[ AspectRatio, Automatic],
          frameopt,
          Rule[ PlotRange, $RANGES$],
          Rule[ PlotLabel, "Sim results"],
          Rule[ AxesLabel, axeslabel]]]]]];

     last = centers[dof];
     lasttime = currenttime];

  ];  (* For all times= Length[values] *)

  If [Length[anim] != 0 && MemberQ[opts, Global`animate],
    $MOVIE$ = anim;];

  If [!MemberQ[opts, Global`animate],
  If[Length[graph] != 0,
  driveplot = temp = Show[graph,
          PlotRange->All,
          DisplayFunction->$DisplayFunction,
          Axes->True,
          AspectRatio->Automatic,
          PlotLabel->"Sim results",
          AxesLabel->axeslabel]];

  If[MemberQ[opts, Global`print],
    If[Context[LaserPrint] == "System`" ,
      Print["Using LaserPrint to print graphics..."];
      LaserPrint[temp];
      Print["Done..."],
    Print["There is no internal LaserPrint command..."]]];

  If[MemberQ[opts,Global`xprint],
      Print["Adjust windows, then press return. "];
      Print["When the cursor changes to a cross, click on the graphics"];
      Print["window you want to store as 'xsim.out'."];
      InputString["Waiting..."];

      Run["xwd >sim.t"];
      Run["xpr -device ps -gray 3 -portrait sim.t >xsim.out"];
      Run["rm sim.t"];
      Print["File 'xsim.out' created."]];

  If[MemberQ[opts,Global`mprint],
      Display["msim.out", temp];
      Print["msim.out created."]];
 ];

  If [Length[anim] != 0 && MemberQ[opts, Global`animate],
    Print["Invoking ShowAnimation; this may take a while."];
    Graphics`Animation`ShowAnimation[anim];
   ];

 If[!old,
  Close[file]];
] (* Block *)

(*
  Create two dimensional plots of data read in from simnon
*)
SimPlot[x_Symbol, ys_List, options_List:{}] :=
  Block[{j, k, ar, xloc, yloc, new, axeslabel, temp, frameopt,totalplot},
   If[!MemberQ[$SIMVARLIST$, x],
    Print["I don't have data for ", x];
    Return[]];

  ar = Automatic;

  If [MemberQ[options, Global`frame], frameopt = Frame-> True,
                               frameopt = Frame->False];

  If[Length[options] > 0,
   If[NumberQ[N[options[[-1]]]],
    ar = N[options[[-1]]]]];

  totalplot = Plot[0h, {h, 0,.001}, DisplayFunction->Identity];
  axeslabel = {ToString[x] , ""};
  For[j=1, j<=Length[ys], j++,
   new={};
   xloc = Flatten[Position[$SIMVARLIST$, x]][[1]];
   yloc = Position[$SIMVARLIST$, ys[[j]]];
   If[Length[yloc] == 0,
    Print["I don't know about ", ys[[j]]];
    Return[],
    axeslabel[[2]] = StringJoin[axeslabel[[2]], ToString[ys[[j]]]];
    axeslabel[[2]] = StringJoin[axeslabel[[2]], " "];
    yloc = Flatten[yloc][[1]]];

   For[k=1, k<=Length[$SIMVALUES$], k++,
    AppendTo[new, {$SIMVALUES$[[k]][[xloc]], $SIMVALUES$[[k]][[yloc]] }]];

   totalplot=Show[totalplot, ListPlot[new,
                               PlotJoined -> True,
                               DisplayFunction->Identity ]
                 ]
      ];

   temp = simplot = Show[totalplot,
          DisplayFunction->$DisplayFunction,
          AspectRatio->ar,
          frameopt,
          Prolog-> AbsoluteThickness[1],
          Axes->True,
          AxesLabel->axeslabel];

  If[MemberQ[options, Global`print],
    If [Context[LaserPrint] == "System`" ,
      Print["Using LaserPrint to print graphics..."];
      LaserPrint[temp];
      Print["Done..."],
    Print["There is no internal LaserPrint command..."]]];

  If[MemberQ[options,Global`xprint],
      Print["Adjust windows, then press return. "];
      Print["When the cursor changes to a cross, click on the graphics"];
      Print["window you want to store as 'xsimplot.out'."];
      InputString["Waiting..."];

      Run["xwd >simplot.t"];
      Run["xpr -device ps -gray 3 -portrait simplot.t >xsimplot.out"];
      Run["rm simplot.t"];
      Print["File 'xsimplot.out' created."]];

  If[MemberQ[options,Global`mprint],
      Display["msimplot.out", temp];
      Print["msimplot.out created."]];
];

(*
  Show a sequence of frames of the robot in various positions
*)
ShowRobot[vars_List, options_List:{}] :=
 Block[{dim, opts, anim, fval, frameopt, axeslabel, temp,
        num, file, i, j, k, evallist, robot},

  If [$FKINRUN$ == "NO",
    Print["You must run FKin[] first."];
    Return[]];

  opts = options;
  num = Null;
  robot = {};
  anim = {};

  If [MemberQ[opts, Global`frame], fval = True, fval = False];

  If [MemberQ[$ContextPath, "Graphics`Animation`"],Null,

   If [MemberQ[opts, Global`animate],
     Print["Animation functions haven't been loaded...\n"];
     opts = Delete[opts, Position[opts, Global`animate]]]];

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

  If[MemberQ[opts, Global`single], num=0];
  dim = Planar[];

  If [dim == 7,
    frameopt = Boxed->fval,
    frameopt = Frame->fval];

  If[dim==3,
     axeslabel={"x", "y"};
     $RANGES$ = {$XRANGE$, $YRANGE$}];

  If[dim==4,
     axeslabel={"x", "z"};
     $RANGES$ = {$XRANGE$, $ZRANGE$}];

  If[dim==5,
     axeslabel={"x", "z"};
     $RANGES$ = {$XRANGE$, $ZRANGE$}];

  If[dim==6,
     axeslabel={"y", "z"};
     $RANGES$ = {$YRANGE$, $ZRANGE$}];

  If[dim==7,
     axeslabel={"x", "y", "z"};
     $RANGES$ = {$XRANGE$, $YRANGE$, $ZRANGE$}];

  For[j=1, j<=num+1, j++,
    evallist = {};

(* set up a list of evaluations for joint values *)
    For[k=1, k<=i, k++,
      If[num>0,
        evallist=Append[evallist, vars[[k,1]] -> vars[[k,2]] +
                    (j-1) (vars[[k,3]] - vars[[k,2]])/num] ,

        evallist=Append[evallist, vars[[k,1]] -> vars[[k,2]]]]];

  Print[evallist];
  temp = DrawRobot[evallist, dim];
  If [temp =!= Null, AppendTo[robot, temp], Return[]];

  If [MemberQ[opts, Global`animate], AppendTo[anim, Append[temp,
          List[
          Rule[ Axes, True],
          Rule[ AspectRatio, Automatic],
          Rule[ PlotRange, $RANGES$],
          frameopt,
          Rule[ PlotLabel, ToString[vars]],
          Rule[ AxesLabel, axeslabel]]]]];

    ];

  If [Length[anim] != 0 && MemberQ[opts, Global`animate],
    $MOVIE$ = anim;];

  If [!MemberQ[opts, Global`animate] && Length[robot] >0,
  robotplot = Show[robot,
               PlotRange->All,
               DisplayFunction->$DisplayFunction,
               Axes->True,
               AspectRatio->Automatic,
               frameopt,
               PlotLabel->ToString[vars],
               AxesLabel->axeslabel];

  If[MemberQ[opts, Global`print],
    If [Context[LaserPrint] == "System`" ,
      Print["Using LaserPrint to print graphics..."];
      LaserPrint[robotplot];
      Print["Done..."],
    Print["There is no internal LaserPrint command..."]]];

  If[MemberQ[opts,Global`xprint],
      Print["Adjust windows, then press return. "];
      Print["When the cursor changes to a cross, click on the graphics"];
      Print["window you want to store as 'xrobotplot.out'."];
      InputString["Waiting..."];

      Run["xwd >robotplot.t"];
      Run["xpr -device ps -gray 3 -portrait robotplot.t >xrobotplot.out"];
      Run["rm robotplot.t"];
      Print["File 'xrobotplot.out' created."]];

  If[MemberQ[opts,Global`mprint],
      Display["mrobotplot.out", robotplot];
      Print["mrobotplot.out created."]];
  ];

  If [Length[anim] != 0 && MemberQ[opts, Global`animate],
    Print["Invoking ShowAnimation; this may take a while."];
    Graphics`Animation`ShowAnimation[anim];
   ];
]

(*
  Do the same thing as ShowRobot, but one joint at a time
*)
SeqShowRobot[vars_List, options_List:{}] :=
 Block[{dim, opts, anim, fval, frameopt, axeslabel, temp,
        num, file, i, j, k, evallist, robot,
        param, evallistbase, addedlist},

  If [$FKINRUN$ == "NO",
    Print["You must run FKin[] first."];
    Return[]];

  opts = options;
  num = Null;
  robot = {};
  anim = {};

  If [MemberQ[opts, Global`frame], fval = True, fval = False];

  If [MemberQ[$ContextPath, "Graphics`Animation`"],Null,

   If [MemberQ[opts, Global`animate],
     Print["Animation functions haven't been loaded...\n"];
     opts = Delete[opts, Position[opts, Global`animate]]]];

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

  If[MemberQ[opts, Global`single], num=0];
  dim = Planar[];

  If [dim == 7,
    frameopt = Boxed->fval,
    frameopt = Frame->fval];

  If[dim==3,
     axeslabel={"x", "y"};
     $RANGES$ = {$XRANGE$, $YRANGE$}];

  If[dim==4,
     axeslabel={"x", "z"};
     $RANGES$ = {$XRANGE$, $ZRANGE$}];

  If[dim==5,
     axeslabel={"x", "z"};
     $RANGES$ = {$XRANGE$, $ZRANGE$}];

  If[dim==6,
     axeslabel={"y", "z"};
     $RANGES$ = {$YRANGE$, $ZRANGE$}];

  If[dim==7,
     axeslabel={"x", "y", "z"};
     $RANGES$ = {$XRANGE$, $YRANGE$, $ZRANGE$}];

 For[param =1, param<=i, param++,
   evallistbase = {};
   addedlist={};

   For [j=param-1, j>0, j--,
     If [!MemberQ[addedlist, vars[[j,1]]] && vars[[param,1]] =!= vars[[j,1]],
       AppendTo[addedlist, vars[[j,1]]];
       AppendTo[evallistbase, vars[[j,1]] -> vars[[j,3]]]]];

   For [j=param+1, j<=i, j++,
     If [!MemberQ[addedlist, vars[[j,1]]] && vars[[param,1]] =!= vars[[j,1]],
       AppendTo[addedlist, vars[[j,1]]];
       AppendTo[evallistbase, vars[[j,1]] -> vars[[j,2]]]]];

  For[j=1, j<=num+1, j++,
    evallist = evallistbase;

    If[num>0,
      AppendTo[evallist, vars[[param,1]] -> vars[[param,2]] +
                  (j-1) (vars[[param,3]] - vars[[param,2]])/num] ,

      AppendTo[evallist, vars[[param,1]] -> vars[[param,2]]]];

  Print[evallist];

  temp = DrawRobot[evallist, dim];
  If [temp =!= Null, AppendTo[robot, temp], Return[]];

  If [MemberQ[opts, Global`animate], AppendTo[anim, Append[temp,
          List[
          Rule[ Axes, True],
          Rule[ AspectRatio, Automatic],
          Rule[ PlotRange, $RANGES$],
          frameopt,
          Rule[ PlotLabel, ToString[vars]],
          Rule[ AxesLabel, axeslabel]]]]];

    ]];

  If [Length[anim] != 0 && MemberQ[opts, Global`animate],
    $MOVIE$ = anim;];

  If [!MemberQ[opts, Global`animate] && Length[robot] >0,
  robotplot = Show[robot,
               PlotRange->All,
               DisplayFunction->$DisplayFunction,
               Axes->True,
               AspectRatio->Automatic,
               frameopt,
               PlotLabel->ToString[vars],
               AxesLabel->axeslabel];

  If[MemberQ[opts, Global`print],
    If [Context[LaserPrint] == "System`" ,
      Print["Using LaserPrint to print graphics..."];
      LaserPrint[robotplot];
      Print["Done..."],
    Print["There is no internal LaserPrint command..."]]];

  If[MemberQ[opts,Global`xprint],
      Print["Adjust windows, then press return. "];
      Print["When the cursor changes to a cross, click on the graphics"];
      Print["window you want to store as 'xrobotplot.out'."];
      InputString["Waiting..."];

      Run["xwd >robotplot.t"];
      Run["xpr -device ps -gray 3 -portrait robotplot.t >xrobotplot.out"];
      Run["rm robotplot.t"];
      Print["File 'xrobotplot.out' created."]];

  If[MemberQ[opts,Global`mprint],
      Display["mrobotplot.out", robotplot];
      Print["mrobotplot.out created."]];
  ];

  If [Length[anim] != 0 && MemberQ[opts, Global`animate],
    Print["Invoking ShowAnimation; this may take a while."];
    Graphics`Animation`ShowAnimation[anim];
   ];
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

  Write[ttt, OutputForm["PrisJoint:RStringFilename:END;"]];

  Write[ttt, OutputForm["RElp:RListParameter List:OList,frame,animate,single,measures,monly,scale,print,xprint,mprint,file,:END;"]];

  Write[ttt, OutputForm["Response:RListTime range:RListInitial Conditions:END;"]];

  Write[ttt, OutputForm["RevJoint:RStringFilename:END;"]];

  Write[ttt, OutputForm["SDynamics:END;"]];

  Write[ttt, OutputForm["SaveAnim:RStringFilename:END;"]];

  Write[ttt, OutputForm["SaveResponse:RStringFilename:RListTime Range:OSymbolTime step:END;"]];

  Write[ttt, OutputForm["SeqShowRobot:OListParameter list:OList,frame,animate,single,print,xprint,mprint,:END;"]];

  Write[ttt, OutputForm["SetRanges:RListX Range:RListY Range:RListZ Range:END;"]];

  Write[ttt, OutputForm["ShowAnim:END;"]];

  Write[ttt, OutputForm["ShowRobot:OListParameter list:OList,frame,animate,single,print,xprint,mprint,:END;"]];

  Write[ttt, OutputForm["SimDrive:RStringData Source:OList,frame,trace,arm,animate,print,xprint,mprint,num,:END;"]];

  Write[ttt, OutputForm["SimPlot:RSymbolIndependent variable:RListDependent variables:OList,frame,print,xprint,mprint,num,:END;"]];

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
  Save an animation in a file
*)

SaveAnim[file_String] :=
 Block[{st},

  If[$MOVIE$ == Null,
   Print["No animation has been loaded or created."];
   Return[]];

  If[file == "", Print["You must specify a filename."]; Return[]];

  st = OpenWrite[file];
  If [st == Null || st == $Failed, Print["Couldn't open your file."];
                                   Return[]];
  Put[$MOVIE$ , st];
  Close[file];
];

(*
  Load an animation
*)
LoadAnim[file_String] :=
 Block[{st},

  If[file == "", Print["You must specify a filename."]; Return[]];

  st = OpenRead[file];
  If [st == Null || st == $Failed, Print["Couldn't open your file."];
                                   Return[]];
 Close[st];

  Print["Reading..."];
  $MOVIE$ = Get[file];

  If[Length[$MOVIE$] != 0 && Head[$MOVIE$ == List] &&
    (Length[Select[$MOVIE$, (Head[#] == Graphics)&]] == Length[$MOVIE$] ||
     Length[Select[$MOVIE$, (Head[#] == Graphics3D)&]] == Length[$MOVIE$]),

    Print["Animation loaded."],
    Print["No frames loaded."];
    $MOVIE$ = Null;];
];

(*
  Show an animation
*)
ShowAnim[] := Block[{},

  If [MemberQ[$ContextPath, "Graphics`Animation`"],Null,
     Print["Animation functions haven't been loaded...\n"];
     Return[]];

  If [$MOVIE$ == Null,
    Print["No animation has been loaded or created."];
    Return[]];

    Print["Invoking ShowAnimation; this may take a while."];
    Graphics`Animation`ShowAnimation[$MOVIE$];
];

(*
  Produce a picture of the robot at a certain configuration
*)
DrawRobot[config_List, dim_, opt_Integer:1] :=
 Block[{robot, i,j, temp, Zrot, inter, itemp},

     If [$FKINRUN$ == "NO",
       Print["You must run FKin[] first."];
       Return[]];

     robot={};

     For [k=0, k<=dof, k++,

       tr[k] = { Flatten[T[0,k]][[4]],
                 Flatten[T[0,k]][[8]],
                 Flatten[T[0,k]][[12]] } /. config;

       rot[k] = { Take[Flatten[T[0,k]], {1,3}],
                  Take[Flatten[T[0,k]], {5,7}],
                  Take[Flatten[T[0,k]], {9,11}]} /. config;

          ];


     For[k=1, k<=dof, k++,
       If[!NumberQ[Plus @@ N[Flatten[T[0,k]] /. config]],
         Print["Can't resolve to values - are all variables set?"];
         Print[""];
         Print[OutputForm[MPrint[T[0,k], ""]]];
         Return[]];
        ];

     For [k=1, k<=dof, k++,

     Zrot = { {Cos[q[k]], -Sin[q[k]], 0},
              {Sin[q[k]], Cos[q[k]], 0},
              {0,0,1}};

(* add the interlink shape *)
      If[dim =!= 7 && $LINK$ =!= Null,
       If[a[k] =!= 0,
        If[$LINK$ === 1,
          inter = Affine3DShape[StandardPris[], {2,1,1}];
          itemp = Translate3DShape[inter, {.5,0,0}],
          itemp = $LINK$];
        itemp = Affine3DShape[itemp, {a[k], 1, 1} /.config];
        If[jointtype[k] == "revolute",
          itemp = Rotate3DShape[itemp, Zrot /. config]];
        itemp = Rotate3DShape[itemp, rot[k-1]]];

       If[d[k] =!= 0,
        If[$LINK$ === 1,
          inter = Affine3DShape[StandardPris[], {1,1,2}];
          itemp = Translate3DShape[inter, {0,0,0}],
          itemp = $LINK$];
        itemp = Affine3DShape[itemp, {1, 1, d[k]} /.config];
        If[jointtype[k] == "revolute",
          itemp = Rotate3DShape[itemp, Zrot /. config]];
        itemp = Rotate3DShape[itemp, rot[k-1]]];

        itemp = Translate3DShape[itemp, tr[k-1]];
        itemp = Planarize[itemp, dim];
        AppendTo[robot, {RGBColor[0,0,0], itemp}];
       ];

       If[ ($PRISJOINT$ == Null && jointtype[k] == "prismatic") ||
           ($REVJOINT$  == Null && jointtype[k] == "revolute"),

         AppendTo[robot, {RGBColor[0,0,0], Planarize[Line[{tr[k-1],
                                              tr[k]}],dim]}];
         Continue[]];

(* add the joint shapes *)
      If[jointtype[k] == "revolute",
        temp = $REVJOINT$;
        temp = Rotate3DShape[temp, Zrot /. config],
        temp = $PRISJOINT$];

        temp = Rotate3DShape[temp, rot[k-1]];
        temp = Translate3DShape[temp, tr[k-1]];
        temp = Planarize[temp, dim];


      AppendTo[robot, {RGBColor[0,0,0], temp}];
(*
   the shape between joints
*)
      AppendTo[robot, {AbsoluteThickness[1], Planarize[Line[{tr[k-1],
                                            tr[k]}],dim]}]];

If [opt == 1, Gr[robot, dim], robot]
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
  Solve the dynamics equations numerically
*)
Response[intime_List, incond_List] :=
  Block[{f, i, dlist, ddlist, jp, time, steps, eqs, list},

  If[$DYNRUN$ == "NO", Print["You must run ELDynamics[] first..."];
                       Return[]];

  If[$DYNRUN$ == "CANT", Print["You must run ELDynamics[] first..."];
                       Return[]];

  If[$TAU$ == Null, Print["The input tau is not defined."];
                    Return[]];
time = intime;
steps = 500;
If[Length[intime] == 3,
  time = {intime[[1]], intime[[2]]};
  steps = intime[[3]]
 ];

If[!IntegerQ[steps] || ! NumberQ[steps],
  Print["The step parameter must be an integer."];
  Return[]];

jp = {};

If[Length[time] =!= 2 || !NumberQ[Plus @@ N[time]],
   Print["The time vector should be a list containing a start and stop time."];
   Return[]];

dlist = Table[D[q[i][Global`t], Global`t], {i,1,dof}];
ddlist = D[dlist, Global`t];

eqs = (M /. tq) . ddlist + (CM /. tq) . dlist + (G /. tq);

For[i=1, i<=dof, i++,
 eqs[[i]] = (eqs[[i]] == $TAU$[[i]]);
 AppendTo[jp, q[i]];

(*
 Print["eq", i, "= ", eqs[[i]]]
*)
];

Print["Solving equations..."];
$NDS$ = NDSolve[Join[eqs, incond], jp,
                Flatten[{Global`t, time}], MaxSteps->steps]
];

(*
  Save the response in a file
*)
SaveResponse[file_String, time_List, inc_:.1] :=
   Block[{i, temp, vars, stringvar},

 If [$NDS$ == Null,
   Print["You must run Response first."];
   Return[]];

 If[Length[time] =!= 2 || !NumberQ[Plus @@ N[time]],
   Print["The time vector should be a list containing a start and stop time."];
   Return[]];

 stringvar = " ";
 vars = {Global`t};
 For[i=1, i<=dof, i++,
  AppendTo[vars, q[i][Global`t]];
  stringvar = stringvar <> ToString[q[i]] <> " ";
    ];

 If[Length[Flatten[$NDS$]] =!= dof,
   Print["There seems to be some mismatch between the solution and the "];
   Print["equations."];
   Return[]];

 temp = Table[Flatten[vars /. $NDS$], {Global`t, time[[1]], time[[2]], inc}];

 Write[file, OutputForm["\""], OutputForm[stringvar]];
 Write[file, OutputForm["\" Numerical solution generated by Response"]];
 Write[file, OutputForm[TableForm[temp]]];
 Close[file];
];

(*
  Show the user the input vector
*)
DisplayTau[]:=
 Block[{i},

  If[$TAU$==Null,
    Print["The vector is undefined."];
    Return[]];

  EPrint[$TAU$, "tau"];

];
End[]
EndPackage[]
