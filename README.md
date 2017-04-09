Robotica is a collection of useful robotics problem solving functions enscapsulated in a Mathematica package,

authored by John Nethery and M.W.Spong

New Functions:

dhInput[ex] Function:

    Now accepts DH parameters as a matrix as well as old method using .txt file.
  
    Alternately, user can enter DH parameters using a dialogue box.
  
    Both new ways work inside Mathematica.
    
    Method 1:
    
    
    ex = {{r, R, p, Revolute, revolute, r}, {0, 0, 0, 0, 0, 0}, {-Pi/2, 
    Pi/2, 0, -Pi/2, Pi/2, 0}, {1, 1, q3, 1/2, 0, 1/2}, {q1, q2, 0, q4,
     q5, q6}}; 
     
    dhInput[ex];
    
    Method 2:
    
    Also using dhInput[ex] with ex = dof;   
    
    ex = 6;
    
    dhInput[ex];
    
    showing the following dialogue box to fill:
![dialogue](https://cloud.githubusercontent.com/assets/25996170/24841733/35e7b270-1d50-11e7-938c-9d52dbf48f57.JPG)    
    
    The new dhInput[ex] methods all give the same result:
 ![dhinput](https://cloud.githubusercontent.com/assets/25996170/24841516/25c48dd6-1d4c-11e7-94be-e2e69a122337.JPG)

drawRobot[] Function:

    After setting DH parameters using dhInput[ex] functon...
  
    ...call drawRobot[] function to draw your own robot!

