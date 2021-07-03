# CLEAN DATA

This directory contains the data cleaned and standardised. There two types of files: 

1. community matrix files (`CM_`, wide format) with a set of id columns (`Name`, `source`, `country`, `site`, `strata`) and the labels, each in one column. A `source` variable is added to store the origin of the data: Robot, Human or Visual

## Community matrix files

- `CM_robot.csv`: Annotations from the robot classifier over a grid of 100 points. Used to compare the performance against the Human labelling of the photquadrats and the Visual (field) labelling of the quadrats.  
- `CM_human.csv`: Annotations from the human over a grid of 100 points. Used to compare the performance against the Robot labelling of the photo quadrats 
- `CM_visual.csv`: Cover of the quadrat estimated in the field. Used to compare the performance against the Robot labelling of the photo quadrats 
