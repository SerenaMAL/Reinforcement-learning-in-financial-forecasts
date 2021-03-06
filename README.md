# Reinforcement-learning-in-financial-forecasts

Repository contains two different algorithm codes: Q_learning_3 refers to the Q-Learning algorithm (both training set and test set) and Q_Learning_3_SARSA refers to SARSA algoritmh (training and test set). 
Fondi_Esterni.xlsx is the data input for the code: here there are ten insurance funds which 604 nominal share's value, from 02/01/2019 to 18/05/2021.

The main parameters of the code are the following; they are organized in:
| # > Parameter > Symbol > Value

1 > Initial cash	> initial.cash	> 5000 or a value 25% greater than di maximum shares’ nominal value

2	> Initial share	> initial.shares	> 0

3	> Transaction costs	> costs.perc > 0.019

4	> State’s level of discretization	> tick > 10

5	> Time window >	win	> 20

6	> How often the agent trades	> dt > 5 (days)

7	> Type of reward > modified.reward > T/F*

8	> Episode number > episodes > 20000

9	> Number of episodes of strengthening of the Q-matrix** >	refining.episodes	> 1000

10 > Learning rate > lr > 0.9

11 > Learning rate with linear decrease > lr.lin.decay > F

12 > Epsilon rate > eps > 0.5

13 > Epsilon rate with linear decrease > eps.lin.decay > T

14 > Discount factor > df > 0.9

15 > Discount factor with linear decrease > df.lin.decay > F

16 > Share of dataset intended for training > training.perc > 0.75

17 > Random seed	seed > 42

18 > How many episodes the graphs are displayed > visualize.every > 1000


The seventh parameter (“modified reward”) refers to the type of reward: if the parameter is equal to “T” (true), then the reward is instant, so the agent gains a positive reward when he sells the shares which an import that is equal to the difference between the cash gained in a certain step and the initial cash; if the “Modified reward” is set to “F” (false), the agent considers an episodic reward which is positive only at the end of the period.
The ninth parameter refers to the number of episodes of strengthening of the Q-matrix: this represents the number of episodes launched with the epsilon rate equal to zero, which involves a change on the e-greedy policy; if epsilon is equal to one, the agent acts randomly while if epsilon is equal to zero the agent takes the current greedy action. During this number of episodes, the agent strengthens the Q-matrix using the policy learning computing the code with epsilon equal to 0.5.
The involved rates are
1.	Learning rate: represents how the agent learns during the algorithm; it takes value between (0;1) and if the learning rate is equal to zero, then the value q of the Q-matrix isn’t update and the agent is learning nothing.
2.	Epsilon rate: represents the exploration parameter, determining the e-greedy policy; it is used to sample a new experience basing on the existing policy.
3.	Discount rate: determines the future rewards’ importance, so if this parameter is equal to zero then the agent is “myopic” because he only considers the current reward, while if the discount rate is equal to one then the agent is interested to receive a big reward over a long-term period.

