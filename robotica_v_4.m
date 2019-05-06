(* ::Package:: *)


BeginPackage["robotica`"]
Off[Replace::rep]


DH::usage = ""
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
dof::usage = ""
Td::usage = ""
cc::usage = ""
zz::usage = ""
x3::usage = ""
ze::usage = ""
b::usage = ""
z::usage = ""
k::usage = ""
l::usage = ""
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

dhInput::usage = "dhInput[] lets the user enter the DH parameters, in a list of {joint_type,r,alpha,d,theta}."

readJointTable::usage = "create DH parameter by Given DOF"
loadRobot::usage = " Create DH Parameter Table by given DH Matrix"
checkJointTable::usage = "do things" 


FKin::usage = "FKin[d_List,Theta_List] calculates the end position based on the R_List and Alpha_List preloaded and the d_List and Theta_List given by parameter"

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


Begin["`Private`"]

$VERSION$ = "4.01";

Print["Robotica version ", $VERSION$, "."];

TPrint[name_String:""] :=
	Block[
		{i,j},

    For[i=0, i<dof, i++,
    	For[j=1, j<=dof, j++,
        If[j>i,
       		Print[MPrint[T[i,j], StringJoin["T[", ToString[i], ",", ToString[j], "]= "]]];
       	]
      ]
    ]
  ]

APrint[name_String:""] :=
	Block[
		{j},
    For[j=1, j<=dof, j++,
    	Print[MPrint[A[j], StringJoin["A[", ToString[j], "]= "]]];
    ]
  ]



(*
show to the user a table to fill with the values
*)
readJointTable[]:=
  Do[
		(*ask the user for
				joint number
				joints Type
				joint connections (a,alpha)
				(a is called r, because)

				d and theta, will be deduced by load robot

				store everything in a valid joint table


		*)
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

		(*call loadRobot*)
  ];


checkJointTable[jt_List]:=
	Do[
		x3 = Dimensions[jt];
		dof= x3[[2]];
		If[ Length[jt]!=5 || Length[x3]!=2 ,
		    Print["jt malformed"];
			Return[False];
		]

		For[ i=1,i<=dof,i++,
			If[ !isPrismatic[ jt[[1,i]] ] && !isRevolutionary[ jt[[1,i]] ],
				Print[" Type column, should include only: Revolute, revolute, R, r, Prismatic, prismatic, P, or p"];
				Return[False];
				
			]
		];
		Print["it's oke'"];
		Return[True];
		
	];


isPrismatic[jtype_String]:=MemberQ[{"Prismatic","prismatic","P","p"},jtype];


isRevolutionary[jtype_String]:=MemberQ[{"Revolute","revolute","R","r"},jtype];


(*print Joint table, recap, for what I understood, as debug *)

JTRecapPrint[]:=Print[
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
							Transpose[JTRecap]
						]
					]
				]
			],
			Frame->All
		]
	]


(*assuming jt to be a valid matrix describing joints*)
loadRobot[jt_List]:=
    Do[	(*load JTRecap*)
	Print["calling loadRobot, inside"];
    For[ i=1,i<=dof,i++,
      alpha[i]=jt[[3,i]];
			a[i]=jt[[2,i]];
			If[ MemberQ[{"Prismatic","prismatic","P","p"},ToString[ jt[[1,i]] ]],
				theta[i] = 0,
				(*d[i] = _,

				theta[i] = _;*)
				d[i] = 0;
			];
      jointtype[i] = ToString[ jt[[1,i]] ];
    ];

	FormAllAs[];
	FormAllTs[];

  ];


(*
  Run the functions that calculates the forward kinematics
*)
FKin[parD_,parTheta_,i_,j_]:=
	Block[{},
		(*calculates T[parD,parTheta] somehow*)
    d=parD;
    theta=parTheta;
    Print[d];
    Print[theta];
    T[i,j];
	];

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
	Block[
		{i,j},
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

FormTij[k_,l_]:=
  If[ (l-k)==1,
    A[l],

    A[k+1].FormTij[k+1, l]
  ];


(* DH Input Functon *)

dhInput[jt_List]:=
  Block[{},
		If[ checkJointTable[jt],
		    Print["calling loadRobot"];
			loadRobot[jt];
		];
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
        If[ isPrismatic[j],
          Cuboid[{-ar,-ar,-1+d-jr-.01},{ar,ar,d+.01}],

          Cylinder[{{0,0,Min[-ar,d-jr]-.01},{0,0,Max[ar,d]+.01}},ar]
        ]
      },

      {
        LightBlue,
        If[ isPrismatic[j],
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
        If[ isPrismatic[ jointtype[i] ],
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
          If[ isRevolutionary[ jointtype[1] ],
            drawJoint[jointtype[1],d[1],a[1],params[[1]],OptionValue[showArrows]],

            drawJoint[jointtype[1],params[[1]],a[1],theta[1]],

            OptionValue[showArrows]
          ],

          If[ dof==1,
            GeometricTransformation[drawGripper[g,0,OptionValue[showArrows]],Chop[Td[dof]]],

            If[ showRobot,
              Table[
                If[ isRevolutionary[ jointtype[i]],
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
            If[ isPrismatic[jointtype[i]],
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
