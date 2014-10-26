
%---------------------------------------------------------------
a_star(Pos, Goals, Plan, BestGoal, Cost) :-
	actual_direction(Dir),
	build_frontier(Pos, Goals, Dir, [Pos], Frontier),
	a_star_aux(Pos, Dir, Goals, Visited, Frontier, Plan, BestGoal, Cost).
%---------------------------------------------------------------
a_star_aux(Pos, Dir, Goals, Visited, Frontier, Plan, BestGoal, Cost) :-
	member(Pos, Goals),
	Cost is 0,
	BestGoal = Pos.

a_star_aux(Pos, Dir, Goals, Visited, Frontier, Plan, BestGoal, Cost) :-
	Frontier = [[NextCost, NextPos] | _],
	
	NewVisited = [Pos | Visited],
	pos_substraction(NextPos, Pos, NewDir),
	build_frontier(NextPos, Goals, NewDir, NewVisited, NewFrontier),
	a_star_aux(NextPos, NewDir, Goals, NewVisited, NewFrontier, NewPlan, BestGoal, NewCost),

	new_plan(Pos, NextPos, Dir, NewPlan, Plan),
	cost(Pos, NextPos, Dir, AddedCost),
	Cost is AddedCost + NewCost.

%% TODO: new_plan

%---------------------------------------------------------------
manhattan_distance([X1 | [Y1]], [X2 | [Y2]], Dist) :-
	DifX is X1 - X2,
	DifY is Y1 - Y2,
	Dist is abs(DifX) + abs(DifY).
%---------------------------------------------------------------
pos_substraction([X1 | [Y1]], [X2 | [Y2]], Result) :-
	X is X1 - X2,
	Y is Y1 - Y2,
	Result = [X , Y].
%---------------------------------------------------------------
cost(Pos1, Pos2, Dir, Result) :-
	pos_substraction(Pos2, Pos1, Dir),
	Result es land_cost.

cost(Pos1, Pos2, Dir, Result) :-
	Result is land_cost + 1.
%---------------------------------------------------------------	
a_star_function(Pos, Pos2, Dir, Goals, Result) :-
	findall(
		FuncRes,
		(
			member(Goal, Goals),
			manhattan_distance(Pos2, Goal, MDist),
			cost(Pos, Pos2, Dir, Cost),
			FuncRes is MDist + Cost
		),
		FunctionResults
	),
	sort(FunctionResults, [Result | _]).
	
%---------------------------------------------------------------
build_frontier(Pos, Goals, Dir, Visited, Frontier) :-
	possible_directions(Pos, Directions),
	findall(
		[Cost, NeighbourPos],
		(
			member([Direction, NeighbourPos], Directions),
			not(member(NeighbourPos, Visited)),
			a_star_function(Pos, NeighbourPos, Direction, Goals, Cost)
		),
		FrontierUnsorted
	),
	sort(FrontierUnsorted, Frontier).

%---------------------------------------------------------------
possible_directions(Pos, PossibleDirs) :-
	Directions = [[0, 1], [1, 0], [-1, 0], [0, -1]],

	findall(
		[Dir, AdjPosition],
		(
			member(Dir, Directions),
			pos_substraction(AdjPosition, Pos, Dir),
			can_walk(AdjPosition)
		),
		PossibleDirs
	).