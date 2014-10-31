%---------------------------------------------------------------

%---------------------------------------------------------------
a_star(Pos, Goals, Plan, BestGoal, Cost) :-
	actual_direction(Cardinal),
	map_dir_to_cardinal(Dir, Cardinal),

	a_star_aux(Dir, Goals, [], [ [0, Pos, [], Dir] ], Plan, BestGoal, Cost, _),
	!.
%---------------------------------------------------------------
a_star_aux(Dir, Goals, Visited, Frontier, Plan, OutPos, Cost, [OutPos, OutParent]) :-
	Frontier = [ Node | RestFrontier ],
	Node = [ Cost, OutPos, OutParent, NewDir],
	member(OutPos, Goals),
	Plan = [],
	format('I virtually reached the goal ~w.', [OutPos]),
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
	), !.
	
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
	sort(Frontier, NewFrontier),
	!.

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


object_or_grave(Entity):-
	Entity = [grave,_],
	has(Entity,_).
	

object_or_grave(Entity):-
	pickable_object(Entity).

%---------------------------------------------------------------
% Auxiliaries functions to decide Action do
%
%---------------------------------------------------------------

% Pick up an object if it's in the same position
inmediate_action(pickup(Object)):-
	%Check preconditions for the current action
	preconditions(pickup(Object)),
	show_action_taken(pickup(Object)),		
	% Delete entity from location data
	retractall(seen_entity(Object,_,_)),
	retractall(at(Object,_)),
	!.

inmediate_action(take_from(Object,Grave)):-
	preconditions(take_from(Object,Grave)),
	show_action_taken(take_from(Object,Grave)),
	% Delete entity from the records
	retractall(seen_entity(Object,_,_)),
	retractall(has(Entity,Object)),
	!.

inmediate_action(cast_spell(sleep(Agent))):-
	preconditions(cast_spell(sleep(Agent))),
	show_action_taken(cast_spell(sleep(Agent))),
	!.


inmediate_action(cast_spell(open(Grave))):-
	preconditions(cast_spell(open(Grave))),
	show_action_taken(cast_spell(open(Grave))),
	!.

charge_stamina(null_action):-
	actual_position(Pos),
	hostel(Name,Pos),
	not_forbbiden_entry(Name),
	actual_stamina(Stamina),
	actual_max_stamina(MaxStamina),
	Stamina < MaxStamina.


/*
need_stamina(Cost,GoalPos,HostelPos):-
	writeln('I need stamina?'),
	not(im_going_to_hostel),	
	hostel(_,_),
	% If there are known hostels
	writeln('Before findall'),
	findall(
		[Dist,Name,Pos],
		(
			hostel(Name,Pos),
			manhattan_distance(GoalPos,Pos,Dist)
		),
		Hostels		
	),
	writeln('Finish the findall'),
	sort(Hostels,SortedHostels),	
	SortedHostels = [[DistNext,_,HostelPos]|_],
	writeln('sort'),
	% If my stamina is low enough to go to hostel add it to goals
	actual_stamina(Stamina),
	% If the level of stamina is low
	margin(Margin),
	AproximateTotalCost is Cost + DistNext,
	AproximateTotalCost >= Stamina - Margin,
	writeln('I need a Hostel!'),
	writeln('Before assert'),
	assert(im_going_to_hostel),
	writeln('After assert'),
	!.*/
search_objects_and_graves(Action):-
	(
		not(plan(_));
		plan(Plan), Plan = []
	),
	i_have_opening_potion,
	writeln('I don\'t have plan. I will create a plan to find objects and graves.'),
	find_objects_graves(Goals),
	display_goals(Goals),
	actual_position(Pos),
	a_star(Pos, Goals, GoalPlan, BestGoal, Cost),
	format('(graves) goal plan: ~w, best goal: ~w, cost: ~w~n', [GoalPlan, BestGoal, Cost]),
	(
	 need_go_to_hostel(BestGoal,Cost,HostelPlan,BestHostelGoal,HostelCost) ->
		(
			NewPlan = HostelPlan,			
			writeln('I need to go to a Hostel'),
			write('I go to Hostel: '),write(BestHostelGoal),write('. Cost'), writeln(HostelCost),
			write('The new Plan is: '), writeln(HostelPlan)
		);
		(	
			NewPlan = GoalPlan,
			writeln('Hostel not need!.'),
			write('New Goal: '), write(BestGoal), write('. Cost: '), write(Cost), 
			write('. The new plan is: '), writeln(NewPlan)
		)
	),
	NewPlan = [ Action | RemainingPlan],
	update_plan(RemainingPlan),
	!.


search_object(Action):-
	(
		not(plan(_));
		plan(Plan), Plan = []
	),
	writeln('I don\'t have plan. I will create a plan to find Objects.'),
	
	find_objects(Goals),
	display_goals(Goals),
	actual_position(Pos),
	a_star(Pos, Goals, GoalPlan, BestGoal, Cost),
	format('(objects) goal plan: ~w, best goal: ~w, cost: ~w~n', [GoalPlan, BestGoal, Cost]),
	
	(
	 need_go_to_hostel(BestGoal,Cost,HostelPlan,BestHostelGoal,HostelCost) ->
		(
			NewPlan = HostelPlan,			
			writeln('I need to go to a Hostel'),
			write('I go to Hostel: '),write(BestHostelGoal),write('. Cost'), writeln(HostelCost),
			write('The new Plan is: '), writeln(HostelPlan)
		);
		(	
			NewPlan = GoalPlan,
			writeln('Hostel not need!.'),
			write('New Goal: '), write(BestGoal), write('. Cost: '), write(Cost), 
			write('. The new plan is: '), writeln(NewPlan)
		)
	),
	NewPlan = [ Action | RemainingPlan],
	update_plan(RemainingPlan),
	!.




explore(Action):-
	(
		not(plan(_));
		plan(Plan), Plan = []
	),
	writeln('I don\'t have plan. I will create a plan to explore.'),
	
	find_unexplore_goals(Goals),
	display_goals(Goals),
	actual_position(Pos),
	a_star(Pos, Goals, NewPlan, BestGoal, Cost),
	
	write('New Goal: '), write(BestGoal), write('. Cost: '), write(Cost), 
	write('. The new plan is: '), writeln(NewPlan),
	NewPlan = [ Action | RemainingPlan],
	update_plan(RemainingPlan),
	!.


follow_plan(Action):-
	plan(Plan), Plan \= [],
	write('I have a plan. '),
	
	Plan = [ Action | RemainingPlan],
	preconditions(Action),
	writeln('I follow my plan.'),
	write('Next action: '), writeln(Action),
	write('Remaining Plan: '), writeln(RemainingPlan),
	update_plan(RemainingPlan),
	!.

follow_plan(Action):-
	plan(Plan), Plan \= [],
	Plan = [ Action | _],
	not(preconditions(Action)),
	retractall(plan(_)),
	!.
/*
follow_plan(Action):-
	write('Action: '), write(Action), write(' can\'t be performed. '), writeln('I abort my plan.'),
	retractall(plan(_)),
	writeln('I don\'t have plan. I will create it.'),
	
	find_goals(Goals),
	display_goals(Goals),
	actual_position(Pos),
	a_star(Pos, Goals, NewPlan, BestGoal, Cost),
	
	write('New Goal: '), write(BestGoal), write('. Cost: '), write(Cost), 
	write('. The new plan is: '), writeln(NewPlan),
	NewPlan = [ Action | RemainingPlan],
	update_plan(RemainingPlan),
	!.
*/

%---------------------------------------------------------------
% Auxiliars functions
%
%---------------------------------------------------------------

update_plan(Plan) :-
  retractall(plan(_)),
  assert(plan(Plan)).

need_go_to_hostel(GoalPos,Cost,HostelPlan,BestHostelGoal,BestHostelCost):-
	writeln('Check if i can go to the target an then to the hostel'),
	hostel(_,_),
	findall(
		Pos,
		(
			hostel(Hostel,Pos),
			not_forbbiden_entry(Hostel)

		),
		Hostels
	),
	write('Hostels: '),writeln(Hostels),
	a_star(GoalPos,Hostels,_,BestHostel,HostelCost),
	actual_position(ActualPos),
	ActualPos \= BestHostel,
	write('From goal '), write(GoalPos), write(' to hostel: '),writeln(BestHostel),
	actual_stamina(Stamina),
	margin(Margin),
	TotalCost is HostelCost + Cost + Margin,
	Stamina < TotalCost,
	actual_position(ActualPos),
	a_star(ActualPos,Hostels,HostelPlan,BestHostelGoal,BestHostelCost),
	!.

  % Search for objets to pickup
find_objects(Goals):-
	writeln('Search for objets'),
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

find_objects_graves(Goals):-
	writeln('Search for objects and graves'),
	findall(
		Pos,
		(
			at(Entity, Pos),
			object_or_grave(Entity)
			
		),
		Entities
	),
	(Entities = [] ->
		(writeln('No objects or graves'),fail);
		(Goals = Entities)
	),
	!.

% Explore map
find_unexplore_goals(Goals):-
	findall(
		Pos,
		unexplored(Pos),
		Goals	
	).

/*
find_goals(Goals):-
	find_objects

*/
	