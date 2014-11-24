%==============================================================================
%	Algorithms module
%==============================================================================

%==============================================================================
%	Find the nearest goal
%	Finds the nearest goal from Goals, being in position Pos.
%==============================================================================
a_star(Pos, Goals, Plan, BestGoal, Cost) :-
	actual_direction(Cardinal),
	map_dir_to_cardinal(Dir, Cardinal),
	
	list_to_heap([], EmptyHeap),	
	add_to_heap(EmptyHeap, 0, [0, Pos, [], Dir], Frontier),
	
	a_star_aux(Dir, Goals, [], Frontier, Plan, BestGoal, Cost, _).

%==============================================================================
%	Auxiliar for A*
%	Based on Dir builds the frontier as a heap, chooses the least cost
%	each time, repeats until goal is reached and then backtracks to build
%	the plan.
%==============================================================================
a_star_aux(_, Goals, _, Frontier, Plan, OutPos, MyCost, [OutPos, OutParent]) :-
	min_of_heap(Frontier, _, Node),
	Node = [ MyCost, OutPos, OutParent, _],
	member(OutPos, Goals),
	get_from_heap(Frontier, _, Node, _),
	Plan = [],
	!.	

a_star_aux(Dir, Goals, Visited, Frontier, Plan, BestGoal, Cost, [OutPos, OutParent]) :-
	% Select least cost node from Frontier
	get_from_heap(Frontier, _, Node, RestFrontier),
	Node = [MyCost, Pos, Parent, ParentDir],
		
	% Special case when the node is the starting point and has no parent
	if_then_else(Parent \= [], get_direction(Pos, Parent, NewDir), NewDir = Dir),
	
	% Visit the node
	NewVisited = [[ Pos, NewDir, MyCost] | Visited],
	
	% Prints frontier step
	print_frontier_step([Pos, NewDir]),
	
	%cost(Parent, Pos, NewDir, StepCost),
	
	%ToFrontCost is StepCost + MyCost,
	
	% Add the Neighbours to the Frontier
	build_frontier(Pos, MyCost, Goals, NewDir, NewVisited, RestFrontier, NewFrontier),
		
	% Prints the frontier if it's enabled
	print_frontier(NewFrontier),
	
	% Keep going
	a_star_aux(NewDir, Goals, NewVisited, NewFrontier, NewPlan, BestGoal, NewCost, [NewOutPos, NewOutParent]),
	
	if_then_else(
		% If this Node belongs to the least cost path	
		Pos = NewOutParent,
		% then inserts actions nedded to the plan 
		(	
			get_direction(NewOutPos, NewOutParent, OutDir),
			OutPos = Pos, OutParent = Parent,
			
			new_plan(ParentDir, OutDir, NewPlan, Plan),
			
			Cost = NewCost
		),
		% else updates output to continue backtracking
		(
			OutPos = NewOutPos, OutParent = NewOutParent,
			Plan = NewPlan,	Cost = NewCost
		)
	), !.
	
%==============================================================================
%	Build the frontier
%	Finds the possible directions from Pos and for each one, calculates
%	A* function to every goal, gets the minimum and adds it to the heap
%==============================================================================
build_frontier(Pos, Cost, Goals, Dir, Visited, OldFrontier, NewFrontier) :-
	possible_directions(Pos, Directions),
	
	build_frontier_aux(Pos, Cost, Goals, Dir, Visited, Directions, OldFrontier, NewFrontier).
	
	
build_frontier_aux(_, _, _, _, _, [], OldFrontier, OldFrontier) :- !.
	
build_frontier_aux(Pos, Cost, Goals, Dir, Visited, [[Direction, NeighbourPos] | RestDirections], OldFrontier, NewFrontier) :-
	
	cost(Pos, NeighbourPos, Dir, StepCost),
	NeighCost is Cost + StepCost,
		
	VisitedNode = [NeighbourPos,_, VCost],
	member(VisitedNode, Visited),
	VCost > NeighCost,
	
	subtract(Visited, [VisitedNode], NewVisited),
	
	a_star_function(NeighbourPos, Goals, NeighCost, StarFuncResult),
	
	add_to_heap(OldFrontier, StarFuncResult, [NeighCost, NeighbourPos, Pos, Direction], TempFrontier),
	build_frontier_aux(Pos, Cost, Goals, Dir, NewVisited, RestDirections, TempFrontier, NewFrontier), !. 
	
build_frontier_aux(Pos, Cost, Goals, Dir, Visited, [[Direction, NeighbourPos] | RestDirections], OldFrontier, NewFrontier) :-
	
	VisitedNode = [NeighbourPos,_, _],
	not(member(VisitedNode, Visited)),
	
	cost(Pos, NeighbourPos, Dir, StepCost),
	NeighCost is Cost + StepCost,
	
	a_star_function(NeighbourPos, Goals, NeighCost, StarFuncResult),
	
	add_to_heap(OldFrontier, StarFuncResult, [NeighCost, NeighbourPos, Pos, Direction], TempFrontier),
	build_frontier_aux(Pos, Cost, Goals, Dir, Visited, RestDirections, TempFrontier, NewFrontier), !. 

build_frontier_aux(Pos, Cost, Goals, Dir, Visited, [_ | RestDirections], OldFrontier, NewFrontier) :-
	build_frontier_aux(Pos, Cost, Goals, Dir, Visited, RestDirections, OldFrontier, NewFrontier), !.



%==============================================================================
%	Builds new plan
%	Calculates the moves from Dir to NewDir and adds it to Plan
%==============================================================================
new_plan(Dir, NewDir, Plan, NewPlan):-
	Dir = NewDir,
	NewPlan = [move_fwd|Plan].

new_plan(Dir, NewDir, Plan, NewPlan):-
	Dir \= NewDir,
	map_dir_to_cardinal(NewDir,Cardinal),
	NewPlan = [turn(Cardinal)| [move_fwd|Plan]].
	
%==============================================================================
%	Calculates A* function
%	Given two adjacent positions returns the nearest Goal based on the sum
%	of the cost from one position to the other and the Manhattan distance
%	to the goal.
%==============================================================================
a_star_function(Pos2, Goals, Cost, Result) :-
	findall(
		FuncRes,
		(
			member(Goal, Goals),
			manhattan_distance(Pos2, Goal, MDist),
			FuncRes is MDist + Cost
		),
		FunctionResults
	),
	sort(FunctionResults, [Result | _]).

%==============================================================================
%	Possible directions
%	Calculates the directions of neighbours that can be visited.
%==============================================================================
possible_directions(Pos, PossibleDirs) :-
	Directions = [[0, 1], [1, 0], [-1, 0], [0, -1]],

	findall(
		[NeighDir, NeighPos],
		(
			member(NeighDir, Directions),
			
			% Get neighbour
			get_position_in_direction(Pos, NeighDir, NeighPos),
			
			% The neighbour is in the map
			contained_in_map(NeighPos),	
			
			can_walk(NeighPos)
		),
		PossibleDirs
	),
			
	% Print neighbours
	print_neighbours(PossibleDirs).
	

%==============================================================================
check_hostel_needed(BestGoal,Cost, GoalPlan, NewPlan) :-
	if_then_else(
		need_go_to_hostel(BestGoal,Cost,HostelPlan,BestHostelGoal,HostelCost),
		(
			NewPlan = HostelPlan,			
			write('- I will go to Hostel: '),write(BestHostelGoal),write('. Cost: '), writeln(HostelCost),
			write('The new Plan is: '), writeln(HostelPlan),
			update_current_goal(BestHostelGoal)
		),
		(	
			NewPlan = GoalPlan,
			write('- New Goal is '), write(BestGoal), write(' with cost '), write(Cost), 
			writeln('.'),write('The new plan is: '), writeln(NewPlan),
			update_current_goal(BestGoal)
		)
	).


%==============================================================================
need_go_to_hostel(GoalPos,Cost,HostelPlan,BestHostelGoal,BestHostelCost):-
	format('- I will check if my stamina is enough to go to goal ~w an then to a hostel.~n', [GoalPos]),
	
	findall(
		Pos,
		(
			hostel(Hostel,Pos),
			not_forbbiden_entry(Hostel)
		),
		Hostels
	),
	
	if_then(
		member(hostel(_, GoalPos), Hostels),
		(writeln('\t- Goal is in a hostel!'), fail)
	),
	
	if_then(Hostels = [],
		(
			writeln('\t- No hostels available.'),
			fail
		)
	),
	if_then_else(
		a_star(GoalPos,Hostels,_,BestHostel,HostelCost),
		format('\t- Nearest hostel is ~w with cost ~w~n', [BestHostel, HostelCost]),
		(format('\t- I couldn\'t reach any hostel.~n'), fail)
	),
	
	actual_position(ActualPos),
	if_then(
		ActualPos = BestHostel,
		(
			writeln('\t- I\'m already in a hostel.'),
			fail
		)
	),
	
	actual_stamina(Stamina),
	margin(Margin),
	TotalCost is HostelCost + Cost + Margin,
	if_then_else(
		Stamina < TotalCost,
		(
			writeln('\t- Hostel needed!'),
			a_star(ActualPos,Hostels,HostelPlan,BestHostelGoal,BestHostelCost)
		),
		(
			writeln('\t- Hostel not needed!.'),
			fail
		)		
	).

%==============================================================================
% Search for objects to pickup
%==============================================================================
find_objects_graves(Goals):-
	findall(
		Pos,
		(
			at(Entity, Pos),
			can_walk(Pos),
			(
				pickable_object(Entity);
				openable_grave(Entity)	
			)		
		),
		Entities
	),
	if_then_else(
		Entities = [],
		(writeln('\t- No objects or graves to go.'), fail),
		Goals = Entities
	).
	
	