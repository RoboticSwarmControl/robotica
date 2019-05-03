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
x3::usage = ""
ze::usage = ""
b::usage = ""
z::usage = ""
k::usage = ""
l::usage = ""
v::usage = ""
A::usage = ""
T::usage = ""
z::usage = ""
M::usage = ""
c::usage = ""

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


dhInput::usage = "dhInput[] lets the user enter the DH parameters, in a list of {joint_type,r,alpha,d,theta}."

readJointTable::usage = "create DH parameter by Given DOF"
loadRobot::usage " Create DH Parameter Table by given DH Matrix"

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

$dhInput$ = "NO";
$XRANGE$ = {-10,10};
$YRANGE$ = {-10,10};
$ZRANGE$ = {-10,10};
$RANGES$ = {$XRANGE$, $YRANGE$, $ZRANGE$};
$FILE$="NOT_SET";
$VERSION$ = "4.01";

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




(* DH Input Functon *)

dhInput[jt_List]:=
  Do[
		If[checkJointTable[jt],
			loadRobot[jt]
		];
		$dhInput$ = "YES";
	]

(*
Create DH function: parses input and generates the DH table
*)
(**)
readJointTable[]:=
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


checkJointTable(jt_List):=
	Do[
	x3 = Dimensions[jt];
	dof= x3[[2]];
	If[ !NumberQ[x2] || x2<=0,
		Return[False]
	]
	If[ Length[x2]!=5 || Length[x3]!=2 ,
		Return[False]
	]

	For[ i=1,i<=dof,i++,
		If[ !MemberQ[{"Prismatic","prismatic","P","p","Revolute","revolute","R","r"},ToString[jt[[1,i]] ]],
			Print[" Type column, should include only:
				Revolute, revolute, R, r, Prismatic, prismatic, P, or p"];
			Return[False]
		]
	];


(*function isPrismatic
	isRevolutionary
	*)

(*print Joint table, recap, for what I understood, as debug



	Print[
		Grid[
			Transpose[
				Join[
					{
						Join[
							{Joint},
							Array[#&,dof]
						]
					},
					Transpose[
						Join[
							{{Type,r, \[Alpha],d,\[Theta]}},
							Transpose[jt]
						]
					]
				]
			],
			Frame->All
		]
	]




	*)



(*Create DH 2*)
(*assuming jt to be a valid matrix describing joints*)
loadRobot[jt]:=
  Do[ (*use block instead*)

    For[ i=1,i<=dof,i++,
      alpha[i]=jt[[3,i]];
			a[i]=jt[[2,i]];
			If[ MemberQ[{"Prismatic","prismatic","P","p"},ToString[ jt[[1,i]] ]],
				theta[i] = 0;
				d[i] = _,

				theta[i] = _;
				d[i] = 0;
			];
      jointtype[i] = ToString[ jt[[1,i]] ];
    ];

    $dhInput$ = "YES";
  ];


(*
  Run the functions that generate the forward kinematics
*)
FKin[]:=
	Do[
    If[$dhInput$ == "YES",
      Print[""],

	    dhInput[]
    ];

	  FormAllAs[];
	  FormAllTs[];

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
drawZArrow[jr_]:=
  Line[
    {
      {{0,0,0},{0,0,2jr}},
      {{0,0,2jr},{1/32,0,3/2jr}},
      {{0,0,2jr},{-1/32,0,3/2jr}},
      {{0,0,2jr},{0,1/32,3/2jr}},
      {{0,0,2jr},{0,-1/32,3/2jr}}
    }
  ];


drawCoordAxes[jr_]:=
  {
    Thick,
    {Red,drawZArrow[jr]},
    {Blue,Rotate[drawZArrow[jr],\[Pi]/2,{0,1,0}]},
    {Green,Rotate[drawZArrow[jr],-\[Pi]/2,{1,0,0}]}
  }


drawJoint[ j_,d_,r_,\[Theta]_,showArrow_:True]:=
  Module[
    {jr = 1/5,ar = 1/20,pr=1/7,vr=1/6},
    {
      If[ showArrow, drawCoordAxes[jr] ],
      Opacity[1],
      {
        Opacity[0.5],
        Gray,
        If[ j == "prismatic",
          Cuboid[{-ar,-ar,-1+d-jr-.01},{ar,ar,d+.01}],

          Cylinder[{{0,0,Min[-ar,d-jr]-.01},{0,0,Max[ar,d]+.01}},ar]
        ]
      },

      {
        LightBlue,
        If[ j == "prismatic",
          {
            Cuboid[{-jr,-jr,-jr},{jr,jr,+jr-.1}],
            Cuboid[{-jr,-jr,+jr},{jr,jr,+jr+.05}]
          },

          {
            Cylinder[{{0,0,-jr-.1},{0,0,+jr+.1}},.9*jr]
          }
        ]
      },

      Rotate[{Opacity[0.5],Gray,Cuboid[{-ar,-ar,d-ar},{r,ar,d+ar}]},\[Theta],{0,0,1}]
    }
  ];


drawShaft[ j_,d_,r_,\[Theta]_]:=
  Module[
    {jr = 1/5,ar = 1/20},
    {
      Opacity[1],
      {
        Opacity[0.5],
        Gray,
        If[ j == "prismatic",
          Cuboid[{-ar,-ar,-1+d-jr-.01},{ar,ar,d+.01}],

          Cylinder[{{0,0,Min[-ar,d-jr]-.01},{0,0,Max[ar,d]+.01}},ar]
        ]
      }
    }
  ];

drawGripper[g_,r_,showArrow_:True]:=
  Module[
    {jr = 1/5,ar = 1/20},
    {
      Opacity[1],
      If[ showArrow,drawCoordAxes[jr]],
      If[ r!= 0,
        {
          Gray,
          Cuboid[{-2ar,-ar,-4ar},{0,ar,4ar}],
          Cuboid[{0ar,-ar,g 2ar},{4ar,ar,2(1+g)ar}],
          Cuboid[{0ar,-ar,-g 2ar},{4ar,ar,-2(1+g)ar}]
        },

        {
          Gray,
          Cuboid[{-4ar,-ar,-2ar},{4ar,ar,0}],
          Cuboid[{g 2ar,-ar,0ar},{2(1+g)ar,ar,4ar}],
          Cuboid[{-g 2ar,-ar,0ar},{-2(1+g)ar,ar,4ar}]
        }
      ]
    }
  ];


dhTransform[d_,r_,\[Theta]_,\[Alpha]_]:=
  RotationTransform[\[Theta],{0,0,1}].TranslationTransform[{0,0,d}].TranslationTransform[{r,0,0}].RotationTransform[\[Alpha],{1,0,0}];

Options[drawRobot] = {showArrows -> True, showH -> True, showManipEllipse-> False, showPlanes->False};

drawRobot[OptionsPattern[]]:=
  Manipulate[
    Chop[%,10^-10];
    Module[
      {jr = 1/10,ar = 1/40,Ad,Td,Ts,j,i,ii,jj,Tv},
      Ad =Table[
        If[ jointtype[i]=="prismatic",
          dhTransform[params[[i]],a[i],theta[i],alpha[i]],

          dhTransform[d[i],a[i],params[[i]],alpha[i]]
        ],
        {i,1,dof}
      ];

      For[ j=1,j<=dof,j++,
        Tv=dhTransform[0,0,0,0];
        Ts=dhTransform[0,0,0,0];
        For[ i=1,i<=j,i++,Ts=Ts.Ad[[i]]];

				For[ ii=1,ii<=4,ii++,
          For[ jj=1,jj<=4,jj++,
            Tv[[1,ii,jj]]=Chop[Ts[[1,ii,jj]]];
          ];
        ];

        Td[j]=Tv;
      ];

      Graphics3D[
        {
          {
            LightBrown,
            Cylinder[{{0,0,-2/5},{0,0,-1/5-1/20}},2.2]
          },
          If[ jointtype[1]== "revolute",
            drawJoint[jointtype[1],d[1],a[1],params[[1]],OptionValue[showArrows]],

            drawJoint[jointtype[1],params[[1]],a[1],theta[1]],

            OptionValue[showArrows]
          ],

          If[ dof==1,
            GeometricTransformation[drawGripper[g,0,OptionValue[showArrows]],Chop[Td[dof]]],

            If[ showRobot,
              Table[
                If[ jointtype[i]=="revolute",
                  GeometricTransformation[
                    drawJoint[jointtype[i],d[i],a[i],params[[i]],OptionValue[showArrows]],
                    Td[i-1]
                  ],

                  GeometricTransformation[
                    drawJoint[jointtype[i],params[[i]],a[i],theta[i],OptionValue[showArrows]],
                    Td[i-1]
                  ]
                ],
                {i,2,dof}
              ]
            ]
          ],
          GeometricTransformation[drawGripper[g,0,OptionValue[showArrows]],Chop[Td[dof]]],

          If[ OptionValue[showPlanes],
            GeometricTransformation[
              {
                Thick,
                {Blue,Rotate[drawZArrow[1/2],\[Pi]/2,{0,1,0}], Text[Subscript["x",planei],{.9,.2,0}]},
                {Green,Rotate[drawZArrow[1/2],-\[Pi]/2,{1,0,0}], Text[Subscript["y",planei],{.2,.9,0}]},
                Blue,
                Opacity[0.2],
                Polygon[{{-1,-1,0},{-1,1,0},{1,1,0},{1,-1,0}}]
              },

              If[planei>0,
                Td[planei],

                dhTransform[0,0,0,0]
              ]
            ]
          ],

          If[ OptionValue[showH],
            Text[StringForm["\!\(\*StyleBox[\"H\",\nFontSlant->\"Italic\"]\)=``",MatrixForm[N[Chop[Td[dof]],2]]],{0,0,-3.2}]
          ]
        },

        SphericalRegion->True,
        ImageSize->425,
        Boxed->False
      ]
    ],
    {
      {params,ConstantArray[0,dof]},
      ControlType->None
    },
    Dynamic[
      Grid[
        Table[
          With[ {i=i},
            If[ jointtype[i]=="prismatic",
              {Subscript["d",i],Slider[Dynamic[params[[i]]],{0,1,1/20},ImageSize->Small],Dynamic[params[[i]]]},
              {Subscript["\[Theta]",i],Slider[Dynamic[params[[i]]],{-\[Pi],\[Pi],\[Pi]/32},ImageSize->Small],Dynamic[params[[i]]]}
              ]
          ],
          {i,dof}
        ]
      ]
    ],
    Delimiter,
    {{g,1,"grip"},0,1,0.01,ImageSize->Small,Appearance->"Labeled"},
    {{showRobot,True,"show robot"},{True,False}},
    {{planei,0,"xy Plane"},0,dof,1,ImageSize->Small,Appearance->"Labeled",ControlType->If[OptionValue[showPlanes],Slider,None]},
    ControlPlacement->Left,
    SaveDefinitions->True
  ];

End[]
EndPackage[]
