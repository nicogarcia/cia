%==============================================================================
%	Display module
%==============================================================================

:- dynamic perception_analysis_output/1.

perception_analysis_output([]).

display_agent_status:-
	turn(Turn),
	ag_name(AgName),
	property([agent, me], stamina, AgStamina),
	property([agent, me], max_stamina, AgMaxStamina),
	at([agent, me], MyPos),
	property([agent, me], dir, MyDir),
		
	format(
		' Turn ~w~tAgent: ~w~tPos: ~w~tDir: ~w~tStamina: ~w / ~w~104|~n', 
		[Turn, AgName, MyPos, MyDir, AgStamina, AgMaxStamina]
	),
	
	show_turn_decoration,
	write('My posesions:\n\t'),
	forall(has([agent, me], Entity), (write(Entity), write(', '))), nl,
	print_separator.

display_perceptions :-
		
	display_fixed_entities,
	
	print_separator,
	
	writeln('I remember: '),
	% Print all entities I see its location
	forall(
		(
			at(Entity, _Pos),
			not(Entity = [agent, me])
		), 
		show_entity(Entity)
	),
	% Print all entities I see another entity has
	forall(
		(
			has(Owner, Entity),
			not(Owner = [agent, me])		
		),
		show_entity_with_owner(Entity)
	), 
	print_separator,
	
	% Previously seen entities	
	writeln('Previously seen entities: '),
	forall(seen_entity(SeenEntity, Pos, SeenTurn), show_entity(SeenEntity, Pos, SeenTurn)),
	
	print_separator,
	
	writeln('Perception analysis:'),
	
	display_perceptions_analysis,
	
	print_separator.
	
	
show_entity(Entity):-
	at(Entity, Pos),
	entity_descr(Entity, Descr),
	format(' ~w~t~30|at ~w~40| Description: ~w~n', [Entity, Pos, Descr]),
	
	% Print entity's objects
	if_then(
		has(Entity, _),
		(
			write('\t\tHas:\t'),
			forall(has(Entity, Entity2), (write(Entity2), write(', '))),
			nl
		)
	).

show_entity_with_owner(Entity):-
	has(Owner, Entity),
	entity_descr(Entity, Descr),
	format(' ~w~t~30|has ~w~t~10|~tDescription: ~w~103|', [Owner, Entity, Descr]).
	
show_entity(Entity, Pos, Turn):-
	format(' ~w~t at ~w~30|  in turn ~w~n', [Entity, Pos, Turn]).
	
display_fixed_entities :-
	writeln('Fixed location entities: '),
	forall(hostel(Entity, Pos), (write('\tHostel: '), write(Entity), write(', '), writeln(Pos))),
	forall(grave(Entity, Pos), (write('\tGrave: '), write(Entity), write(', '), writeln(Pos))).
	
display_goals(Goals):-
	write('\tGoals: '),
	forall(member(Goal, Goals), (write(Goal), write(' '))),
	nl.
	
print_frontier_step([Pos, NewDir]) :-
	if_then(
		print_frontier_enabled,
		(
			map_dir_to_cardinal(NewDir, Cardinal),
			format('-> Current step: staying at ~w heading ~w~n', [Pos, Cardinal])
		)
	).
	 

print_frontier(HeapFrontier) :-
	if_then(print_frontier_enabled,
		(
			heap_to_list(HeapFrontier, Frontier),
			writeln('\tFrontier: '),
			forall(
				member(LeastCost - [_, NeighPos, Parent, _], Frontier),
				format('\t\tPos: ~w~t~40|Cost + Heur: ~w~t~60|Coming from: ~w~t~80|~n', [NeighPos, LeastCost, Parent])
			)
		)
	).
	
print_neighbours(Neighbours):-
	if_then(
		print_frontier_enabled,
		(
			format('\t* Current neighbours:   '),			
			forall(
				member([NeighDir, NeighPos], Neighbours),
				(
					% Get the cardinal point of neighbours dir
					map_dir_to_cardinal(NeighDir, Cardinal),
					
					format('~w at ~w   ', [NeighPos, Cardinal])
				)
			),
			nl
		)
	).

%==========================================================
%              Screen Information Display
%==========================================================

print_separator :-
	format('~`-t~104|~n').
	
show_turn_decoration :-
	format('~`=t~104|~n').

%--------------------- Perception Messages ------------------------------%

show_message(hostel_found) :-
	format(atom(Output), '- I see a hostel! I will save it!', []),
	perception_analysis_output(Old),
	retractall(perception_analysis_output(_)),
	assert(perception_analysis_output([ Output | Old])).

show_message(grave_found) :-
	format(atom(Output), '- I see a grave! I will save it!', []),
	perception_analysis_output(Old),
	retractall(perception_analysis_output(_)),
	assert(perception_analysis_output([ Output | Old])).
	
display_perceptions_analysis :-
	perception_analysis_output(Output),
	forall(
		member(Message, Output),
		writeln(Message)
	),
	retractall(perception_analysis_output(_)),
	assert(perception_analysis_output([])).

show_action_taken(Action) :-
	print_separator,
	
	if_then_else(
		current_goal(Goal),
		format('Action taken --> ~w~tCurrent goal --> ~w~t~90|~n', [Action, Goal]),
		format('Action taken --> ~w~tCurrent goal --> None~t~90|~n', [Action])
	).
	
