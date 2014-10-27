%---------------------------------------------------------------

%---------------------------------------------------------------
a_star(Pos, Goals, Plan, BestGoal, Cost) :-
	actual_direction(Cardinal),
	map_dir_to_cardinal(Dir, Cardinal),
	build_frontier(Pos, Goals, Dir, [Pos],[], Frontier),
	print_frontier(Frontier),
	a_star_aux(Pos, Dir, Goals, [], Frontier, Plan, BestGoal, Cost).
%---------------------------------------------------------------
a_star_aux(Pos, Dir, Goals, Visited, Frontier, Plan, BestGoal, Cost) :-
	member(Pos, Goals),
	Cost is 0,
	BestGoal = Pos,
	Plan = [].

a_star_aux(Pos, Dir, Goals, Visited, Frontier, Plan, BestGoal, Cost) :-
	% Select new node
	Frontier = [[NextCost, NextPos] | RestFrontier],
	% visit the node
	NewVisited = [Pos | Visited],
	get_direction(NextPos, Pos, NewDir),
	% Add the Neighbours to the Frontier
	build_frontier(NextPos, Goals, NewDir, NewVisited,RestFrontier, NewFrontier),
	print_frontier(NewFrontier),
	% Keep going
	a_star_aux(NextPos, NewDir, Goals, NewVisited, NewFrontier, NewPlan, BestGoal, NewCost),
	% 
	new_plan(Dir, NewDir, NewPlan, Plan),
	cost(Pos, NextPos, Dir, AddedCost),
	Cost is AddedCost + NewCost.

%---------------------------------------------------------------

new_plan(Dir, NewDir, Plan, NewPlan):-
	Dir = NewDir,
	NewPlan = [move_fwd|Plan].

new_plan(Dir, NewDir, Plan, NewPlan):-
	Dir \= NewDir,
	map_dir_to_cardinal(NewDir,Cardinal),
	NewPlan = [turn(Cardinal)| [move_fwd|Plan]].

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
build_frontier(Pos, Goals, Dir, Visited,OldFrontier ,NewFrontier) :-
	write('Posicion actual: '), writeln(Pos),
	possible_directions(Pos, Directions),
	% print_possible_directions(Directions),
	findall(
		[Cost, NeighbourPos],
		(
			member([Direction, NeighbourPos], Directions),
			not(member(NeighbourPos, Visited)),
			a_star_function(Pos, NeighbourPos, Dir, Goals, Cost)
		),
		FrontierUnsorted
	),

	append(OldFrontier,FrontierUnsorted,Frontier),
	sort(Frontier, NewFrontier).

%---------------------------------------------------------------
possible_directions(Pos, PossibleDirs) :-
	Directions = [[0, 1], [1, 0], [-1, 0], [0, -1]],

	findall(
		[Dir, AdjPosition],
		(
			member(Dir, Directions),
			get_position_in_direction(Pos, Dir, AdjPosition),
			can_walk(AdjPosition),
			map_dir_to_cardinal(Dir, Cardinal),
			write('\t'), write(AdjPosition), write(' at '), write(Cardinal)
		),
		PossibleDirs
	), nl.

%---------------------------------------------------------------
map_dir_to_cardinal(Dir, Cardinal):-
	Dir = [-1, 0], Cardinal = n.

map_dir_to_cardinal(Dir, Cardinal):-
	Dir = [0, 1], Cardinal = e.

map_dir_to_cardinal(Dir, Cardinal):-
	Dir = [0, -1], Cardinal = w.

map_dir_to_cardinal(Dir, Cardinal):-
	Dir = [1, 0], Cardinal = s.

%---------------------------------------------------------------
land_cost(Pos,Land_cost):-
	land(Pos,plain),
	Land_cost = 1.

land_cost(Pos,Land_cost):-
	land(Pos,mountain),
	Land_cost = 2.

%---------------------------------------------------------------
manhattan_distance([X1 | [Y1]], [X2 | [Y2]], Dist) :-
	DifX is X1 - X2,
	DifY is Y1 - Y2,
	%((X1 = X2; Y1 = Y2) -> PlusOne = 0; PlusOne = 1),
	Dist is abs(DifX) + abs(DifY). % + PlusOne.
%---------------------------------------------------------------
get_direction([X1 | [Y1]], [X2 | [Y2]], Result) :-
	X is X1 - X2,
	Y is Y1 - Y2,
	Result = [X , Y].

%---------------------------------------------------------------
get_position_in_direction(Pos, Dir, PosInDir):-
	Pos = [X1, Y1], Dir = [X2, Y2],
	DifX is X1 + X2,
	DifY is Y1 + Y2,
	PosInDir = [DifX, DifY].

%---------------------------------------------------------------
cost(Pos1, Pos2, Dir, Result) :-
	get_direction(Pos2, Pos1, Dir),
	land_cost(Pos2, Land_cost),
	Result is Land_cost.

cost(Pos1, Pos2, Dir, Result) :-
	land_cost(Pos2, Land_cost),
	Result is Land_cost + 1.

% UNUSED
print_possible_directions(Directions) :-
	write('Possible directions are: '),
	forall(
		member([Dir, _], Directions),
		(
			map_dir_to_cardinal(Dir, Cardinal),
			write(Cardinal), write(',')
		)
	),
	nl.

print_frontier(Frontier) :-
	writeln('Frontier: '),
	forall(
		member([Cost, Pos], Frontier),
		(
			write('\tCost: '), write(Cost), write(' Pos: '), writeln(Pos)
		)
	).