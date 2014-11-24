%=============================================================
%	Decision module
%=============================================================

:- dynamic plan/1,unexplored/1,goals/1, current_goal/1.

decide_action(Action):-
	writeln('Decision analysis:'),
	fail.

decide_action(Action):-
	inmediate_action(Action),
	writeln('- Execute inmediate action'),
	!.

decide_action(Action):-
  	charge_stamina(Action),
  	writeln('- Execute charge stamina action'),
  	!.

decide_action(Action):-
	follow_plan(Action),
	!.
	
decide_action(Action):-
	search_objects_and_graves(Action),
	!.

decide_action(Action):-
	explore(Action),
	!.

decide_action(Action):-
	random_position(Action).

update_plan(Plan) :-
	retractall(plan(_)),
	assert(plan(Plan)).
	
update_current_goal(Goal):-
	retractall(current_goal(_)),
	assert(current_goal(Goal)).

%---------------------------------------------------------------
% Auxiliaries functions to decide Action do
%
%---------------------------------------------------------------

% Pick up an object if it's in the same position
inmediate_action(pickup(Object)):-
	%Check preconditions for the current action
	preconditions(pickup(Object)),		
	% Delete entity from location data
	retractall(seen_entity(Object,_,_)),
	retractall(at(Object,_)),
	!.

inmediate_action(take_from(Object,Grave)):-
	preconditions(take_from(Object,Grave)),
	% Delete entity from the records
	retractall(seen_entity(Object,_,_)),
	retractall(has(_,Object)),
	!.

inmediate_action(cast_spell(sleep(Agent))):-
	preconditions(cast_spell(sleep(Agent))),
	!.

inmediate_action(attack(Victim)) :-
	preconditions(attack(Victim)),
	!.

inmediate_action(cast_spell(open(Grave))):-
	preconditions(cast_spell(open(Grave))),
	!.

charge_stamina(null_action):-
	actual_position(Pos),
	hostel(Name,Pos),
	not_forbbiden_entry(Name),
	actual_stamina(Stamina),
	actual_max_stamina(MaxStamina),
	Stamina < MaxStamina.
	
search_objects_and_graves(Action):-
	(
		not(plan(_));
		plan(Plan), Plan = []
	),
	write('- ******   I don\'t have a plan! ******'),
	writeln('I will create a plan to find objects.'),
	
	find_objects_graves(Goals),
	display_goals(Goals),
	actual_position(Pos),
	if_then_else(
		a_star(Pos, Goals, GoalPlan, BestGoal, Cost),
		format('\t- Nearest goal is ~w with cost ~w~n', [BestGoal, Cost]),
		(format('\t- Goals can\'t be reached~n'), fail)
	),	
	
	check_hostel_needed(BestGoal,Cost, GoalPlan, NewPlan),
	
	NewPlan = [ Action | RemainingPlan],
	update_plan(RemainingPlan),
	!.
	

explore(Action):-
	(
		not(plan(_));
		plan(Plan), Plan = []
	),
	writeln('- I don\'t have goals, I will explore the map.'),
	
	borders(Goals),
	display_goals(Goals),
	actual_position(Pos),
	
	assert(exploring),
	a_star(Pos, Goals, NewPlan, BestGoal, Cost),
	retractall(exploring),
	
	write('\tBest Goal: '), write(BestGoal), write('. Cost: '), write(Cost), 
	writeln('. '), write('- The new plan is: '), writeln(NewPlan),
	NewPlan = [ Action | RemainingPlan],
	update_plan(RemainingPlan),
	update_current_goal(BestGoal),
	!.
	

follow_plan(Action):-
	plan(Plan), Plan \= [],
	write('- I have a plan. '),
	
	Plan = [ Action | RemainingPlan],
	preconditions(Action),
	writeln('I will follow my plan.'),
	write('\tNext action: '), writeln(Action),
	write('\tRemaining Plan: '), writeln(RemainingPlan),
	update_plan(RemainingPlan),
	!.

follow_plan(Action):-
	plan(Plan), Plan \= [],
	Plan = [ Action | _],
	not(preconditions(Action)),
	retractall(plan(_)),
	writeln('- I had a plan, but I abort it because it\'s impossible.'),
	
	retractall(current_goal(_)),
	
	fail,	
	!.
	
random_position(Action):-
	writeln('- I\'ve explored all the map. I will create a plan to move randomly.'),
	get_random_pos(RPos),
	Goals = [RPos],
	display_goals(Goals),
	actual_position(Pos),
	a_star(Pos, Goals, GoalPlan, BestGoal, Cost),
	
	check_hostel_needed(BestGoal,Cost, GoalPlan, NewPlan),
	
	NewPlan = [ Action | RemainingPlan],
	update_plan(RemainingPlan).
	
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliar for preconditions
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

actual_position(Pos):-
  property([agent,me],pos,Pos).

actual_direction(Dir):-
  property([agent,me],dir,Dir).

is_unconscious(Agent):-
  property(Agent,unconscious,true).

can_walk(Pos):-
  land(Pos,plain), !.

can_walk(Pos):-
  land(Pos,mountain), !.
  
can_walk(Pos):-
  not(land(Pos,_)),
  exploring.	
  
actual_stamina(Stamina):-
	property([agent,me],stamina, Stamina).

actual_max_stamina(MaxStamina):-
  property([agent,me],max_stamina,MaxStamina).

i_have_opening_potion:-
	has([agent,me],[opening_potion,_]),
  !.

not_forbbiden_entry(Hostel):-
  entity_descr([hostel,Hostel],[[forbidden_entry,ForbbidenList]]),
  ag_name(AgInstanceName),
  (
    \+ member([[agent,AgInstanceName],_],ForbbidenList) ;
      member([[agent,AgInstanceName],Turn],ForbbidenList),
      turn(ActualTurn),
      Turn < ActualTurn
  ).

is_open(Grave):-
	property(Grave, open, true).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Actions' preconditions
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

preconditions(move_fwd):-
	actual_position(Pos),
	actual_direction(Dir),
	ady_at_cardinal(Pos,Dir,Next_pos),
	can_walk(Next_pos),
	!.

preconditions(turn(Cardinal)) :-
	actual_position(MyPos),
	map_dir_to_cardinal(Dir, Cardinal),
	get_position_in_direction(MyPos, Dir, Pos),
	can_walk(Pos),
	!.
	
preconditions(pickup(Object)):-
	Object = [Type, _],
	% Define types that can be picked up
	(Type = treasure; Type = sleep_potion; Type = opening_potion),	
	% Check if agent is in the same position as the object
	at([agent,me],Pos), at(Object,Pos),
	!.

preconditions(drop(Object)):-
  has([agent,me],Object),
  !.

preconditions(take_from(Object,Grave)):-
  Grave = [grave,_],
  actual_position(Pos),
  at(Grave,Pos),
  is_open(Grave),
  has(Grave,Object),
  !.

preconditions(attack(Agent)):-
	Agent \= [agent, me],
  actual_position(Pos),
  actual_direction(Dir),
  not(is_unconscious(Agent)),
  at(Agent,AgentPos),
  findall(Position,pos_in_attack_range(Pos,Dir,Position),Positions),
  member(AgentPos,Positions),
  !.

preconditions(cast_spell(open(Grave))):-
  i_have_opening_potion,
  actual_position(Pos),
  Grave = [grave,_],
  at(Grave,Pos),
  not(is_open(Grave)),
  !.

preconditions(cast_spell(sleep(Agent))):-
  has([agent,me],[sleep_potion,_]),
  actual_position(Pos),
  at(Agent,Pos),
  Agent = [Type,_],
  Agent \= [agent,me],
  (Type = agent ; Type = dragon),
  not(is_unconscious(Agent)),
  !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliars
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
explore_cell(Pos):-
 	retractall(unexplored(Pos)).
 
 	
 	