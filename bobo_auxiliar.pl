%---------------------------------------------------------------

%---------------------------------------------------------------
a_star(Pos, Goals, Plan, BestGoal, Cost) :-
	actual_direction(Cardinal),
	map_dir_to_cardinal(Dir, Cardinal),

	a_star_aux(Dir, Goals, [], [ [0, Pos, [], Dir] ], Plan, BestGoal, Cost, _).
%---------------------------------------------------------------
a_star_aux(Dir, Goals, Visited, Frontier, Plan, OutPos, Cost, [OutPos, OutParent]) :-
	Frontier = [ Node | RestFrontier ],
	Node = [ Cost, OutPos, OutParent, NewDir],
	member(OutPos, Goals),
	Plan = [],
	writeln('I virtually reached the goal'),
	!.

a_star_aux(Dir, Goals, Visited, Frontier, Plan, BestGoal, Cost, [OutPos, OutParent]) :-
	% Select new node
	Frontier = [ Node | RestFrontier],
	Node = [MyCost, Pos, Parent, ParentDir],
	
	( Parent \= [] -> get_direction(Pos, Parent, NewDir); NewDir = Dir ),
	
	% Visit the node
	NewVisited = [Pos | Visited],	
	
	% Add the Neighbours to the Frontier
	build_frontier(Pos, MyCost, Goals, NewDir, NewVisited, RestFrontier, NewFrontier),
	%print_frontier(NewFrontier),
	
	% Keep going
	a_star_aux(NewDir, Goals, NewVisited, NewFrontier, NewPlan, BestGoal, NewCost, [NewOutPos, NewOutParent]),
	
	( 
		Pos = NewOutParent -> 
		(	
			get_direction(NewOutPos, NewOutParent, OutDir),
			OutPos = Pos, OutParent = Parent,
			
			new_plan(ParentDir, OutDir, NewPlan, Plan),
			
			cost(Pos, NewOutPos, NewDir, AddedCost),
			Cost is AddedCost + NewCost
		);
		(
			OutPos = NewOutPos, OutParent = NewOutParent,
			Plan = NewPlan,	Cost = NewCost
		)
	).
	
%---------------------------------------------------------------
build_frontier(Pos, MyCost, Goals, Dir, Visited,OldFrontier ,NewFrontier) :-
	possible_directions(Pos, Directions),
	
	findall(
		[Cost, NeighbourPos, Pos, Direction],
		(
			member([Direction, NeighbourPos], Directions),
			not(member(NeighbourPos, Visited)),
			a_star_function(Pos, NeighbourPos, Dir, Goals, LocalCost),
			Cost is LocalCost + MyCost
		),
		FrontierUnsorted
	),

	append(OldFrontier,FrontierUnsorted,Frontier),
	sort(Frontier, NewFrontier).

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
possible_directions(Pos, PossibleDirs) :-
	Directions = [[0, 1], [1, 0], [-1, 0], [0, -1]],

	findall(
		[Dir, AdjPosition],
		(
			member(Dir, Directions),
			get_position_in_direction(Pos, Dir, AdjPosition),
			contained_in_map(AdjPosition),
			map_dir_to_cardinal(Dir, Cardinal)
			%print_neighbour(AdjPosition, Cardinal)
		),
		PossibleDirs
	).

%---------------------------------------------------------------
contained_in_map([X | [Y]]) :-
	mapWidth(W),
	mapHeight(H),
	X > 0, X =< W,	
	Y > 0, Y =< H.
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

land_cost(Pos,Land_cost):-
	not(land(Pos, forest)),
	not(land(Pos, water)),
	Land_cost = 5.
	
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

	
%---------------------------------------------------------------

% Search a Hostel if needed	
%---------------------------------------------------------------
find_goals(Goals):-
	need_stamina,
	writeln('I need stamina?'),
	% If there are known hostels
	hostel(_,_),	
	% Find Manhattan distance to every one and sort them
	actual_position(MyPos),
	findall(
		[Dist,Name,Pos],
		(
			hostel(Name,Pos),
			manhattan_distance(MyPos,Pos,Dist)
		),
		Hostels		
	),
	sort(Hostels,SortedHostels),	
	%SortedHostels = [[DistNext,_,PosNext]|_],
	SortedHostels = [[_,_,PosNext]|_],
	% If my stamina is low enough to go to hostel add it to goals
	%actual_stamina(Stamina),
	% If the level of stamina is low
	%margin(Margin),
	%DistNext >= Stamina - Margin,
	writeln('I need a Hostel!'),
	Goals = [PosNext],
	retractall(need_stamina),
	!.
	
% Search for objets to pickup
find_goals(Goals):-
	writeln('Search for objets'),
	pickable_object(Entity),
	findall(
		Pos,
		(
			at(Object, Pos),
			pickable_object(Object)
		),
		Objects
	),
	(Objects = [] ->
		(writeln('No objects'),fail);
		(Goals = Objects)
	),
	!.


	
% Explore map
find_goals(Goals):-
	findall(
		Pos,
		unexplored(Pos),
		Goals	
	).

%---------------------------------------------------------------

%---------------------------------------------------------------
generate_unexplored:-
	mapWidth(W),
	mapHeight(H),	
	forall(
		(between(0,W, X),between(0,H,Y)),
		assert(unexplored([X,Y]))
	).

%---------------------------------------------------------------
%Return if an Entity can be pickup
pickable_object(Entity):-
	(
		Entity = [treasure, _];
		Entity = [sleep_potion,_];
		Entity = [opening_potion,_]
	).
