# Reinforcement-learning-in-financial-forecasts

Repository contains two different algorithm codes: Q_learning_3 refers to the Q-Learning algorithm (both training set and test set) and Q_Learning_3_SARSA refers to SARSA algoritmh (training and test set). 
Fondi_Esterni.xlsx is the data input for the code: here there are ten insurance funds which 604 nominal share's value, from 02/01/2019 to 18/05/2021.

The main parameters of the code are the following:
| ----- | ----- | ----- | ----- |
| #| Parameter | Symbol |Value|
|1	|Initial cash	|initial.cash	|5000 or a value 25% greater than di maximum shares’ nominal value|
|2	|Initial share	|initial.shares	|0|
|3	|Transaction costs	|costs.perc	|0.019|
|4	|State’s level of discretization	|tick	|10|
|5	|Time window|	|win	|20|
|6	|How often the agent trades	|dt	|5 (days)|
|7	|Type of reward	|modified.reward	|T/F*|
|8	|Episode number	|episodes 	|20000|
|9	|Number of episodes of strengthening of the Q-matrix**|	refining.episodes	|1000|
|10	|Learning rate	|lr	|0.9|
|11	|Learning rate with linear decrease	|lr.lin.decay|	F|
|12	|Epsilon rate	|eps 	|0.5|
|13	|Epsilon rate with linear decrease|	eps.lin.decay	|T|
|14	|Discount factor	|df 	|0.9|
|15	|Discount factor with linear decrease|	df.lin.decay|	F|
|16	|Share of dataset intended for training	|training.perc	|0.75|
|17	|Random seed	seed	|42|
|18	|How many episodes the graphs are displayed|	visualize.every	|1000|
