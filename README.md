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

    this function shows your robot initialized with joints set to zero. 
    You can manipulate each joint using the sliders.
    
    Example:
    
    After setting DH parameters using dhInput[ex] function as from previous example...
  
    ...call drawRobot[] function to draw your own robot!

   ![draw1](https://cloud.githubusercontent.com/assets/25996170/24841896/79f4f604-1d54-11e7-8ddb-e1da27fc5f66.JPG)
     
   ![draw2](https://cloud.githubusercontent.com/assets/25996170/24841927/0773788e-1d55-11e7-947b-91f02e18c004.JPG)

    The following are different robots with a range of DOF:
    
   ![draw3](https://cloud.githubusercontent.com/assets/25996170/24842020/945a2e90-1d56-11e7-9c04-669742d608d9.JPG)
   
   ![draw4](https://cloud.githubusercontent.com/assets/25996170/24842047/01f2d1a0-1d57-11e7-97e6-b0a1c0c8702b.JPG)
