Robotica is a collection of useful robotics problem solving functions enscapsulated in a Mathematica package,

authored by John Nethery and M.W.Spong

New Functions:

dhInput[ex] Function:

    Now accepts DH parameters as a matrix as well as old method using .txt file.
  
    Alternately, user can enter DH parameters using a dialogue box.
  
    Both new ways work inside Mathematica.
    
    Scenario 1:
    ex = {{r, R, p, Revolute, revolute, r}, {0, 0, 0, 0, 0, 0}, {-Pi/2, 
    Pi/2, 0, -Pi/2, Pi/2, 0}, {1, 1, q3, 1/2, 0, 1/2}, {q1, q2, 0, q4,
     q5, q6}}; 
     
    dhInput[ex];
    
     
    
    

drawRobot[] Function:

    After setting DH parameters using dhInput[ex] functon...
  
    ...call drawRobot[] function to draw your own robot!

