# CLEAN DATA

This directory contains the data cleaned and standardised. There community matrix files (`CM_`, wide format) with a set of id columns (`Name`, `source`, `country`, `site`, `strata`) and the labels, each in one column. A `source` variable is added to store the origin of the data: Robot, Human or Visual


- `CM_robot.csv`: Annotations from the robot classifier over a grid of 100 points. Used to compare the performance against the Human labelling of the photo quadrats and the Visual (field) labelling of the quadrats.  
- `CM_human.csv`: Annotations from the human over a grid of 100 points. Used to compare the performance against the Robot labelling of the photo quadrats 
- `CM_visual.csv`: Cover of the quadrat estimated in the field. Used to compare the performance against the Robot labelling of the photo quadrats 
- `CM_HumanRobot`: bind of Human and Robot community matrices only with common labels. `NA` has been replace by zero.
- `CM_VisualRobot`: bind of Visual (field) and Robot community matrices only with common labels. `NA` has been replace by zero. 

- `DF_HumanRobot`: long_format version of the combined community matrices of Human and Robot. Labels are under `Label` and cover estimates under `Cover`
- `DF_VisualRobot`: long_format version of the combined community matrices of Visual and Robot. Labels are under `Label` and cover estimates under `Cover`

- `CM_FG_Robot`: Community matrix returned by Robot, recoded into functional groups
- `CM_FG_Human`: Community matrix labelled by human from photo quadrats, recoded into functional groups
- `CM_FG_Visual`: Community matrix labelled in the field by human from real quadrats, recoded into functional groups

