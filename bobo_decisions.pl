%=============================================================
%	Decision module
%	Contains the methods to check actions' preconditions
%=============================================================

:- dynamic plan/1,unexplored/1,goals/1.

% Object pickup action
decide_action(pickup(Object)):-
	%Check preconditions for the current action
	preconditions_pickup(Object),
	    
	show_action_taken(pickup(Object)),
		
	% Delete entity from location data
	retractall(seen_entity(Object,_,_)),
	retractall(at(Object,_)).
	
decide_action(Action) :-
	print_separator,
	writeln('Plan analysis:'),
	fail.

decide_action(Action):-
	need_stamina(),
	find_goals(Goals),
	display_goals(Goals),
	actual_position(Pos),
	a_star(Pos, Goals, NewPlan, BestGoal, Cost),
	
	write('New Goal: '), write(BestGoal), write('. Cost: '), write(Cost), 
	write('. The new plan is: '), writeln(NewPlan),
	NewPlan = [ Action | RemainingPlan],
	update_plan(RemainingPlan),
	!.

decide_action(Action) :-
	  plan(Plan), Plan \= [],
	  
	  write('I have a plan. '),
	
	  Plan = [ Action | RemainingPlan],
	    
	  (
	  	preconditions(Action) ->
	  	(
	  		writeln('I follow my plan.'),
	  		write('Next action: '), writeln(Action),
	  		write('Remaining Plan: '), writeln(RemainingPlan),
		  	update_plan(RemainingPlan)
	  	);
	  	(
	  		write('Action: '), write(Action), write(' can\'t be performed. '), writeln('I abort my plan.'),
	  		retractall(plan(_)),
	  		fail
	  	)
	  ),
	  !.


decide_action(Action) :-
	(
		not(plan(_));
		plan(Plan), Plan = []
	),
	writeln('I don\'t have plan. I will create it.'),
	
	find_goals(Goals),
	display_goals(Goals),
	actual_position(Pos),
	a_star(Pos, Goals, NewPlan, BestGoal, Cost),
	
	write('New Goal: '), write(BestGoal), write('. Cost: '), write(Cost), 
	write('. The new plan is: '), writeln(NewPlan),
	NewPlan = [ Action | RemainingPlan],
	write('Test1'),
	update_plan(RemainingPlan),
	write('Test2'),
	!.
	
decide_action(move_fwd).

update_plan(Plan) :-
  retractall(plan(_)),
  assert(plan(Plan)).

%-------------------------------------------------------------
%	Actions' preconditions
%-------------------------------------------------------------

preconditions_pickup(Object):- 
	Object = [Type, _],
	% Define types that can be picked up
	(Type = treasure; Type = sleep_potion; Type = opening_potion),
	
	% Check if agent is in the same position as the object
	at([agent,me],Pos), at(Object,Pos).


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
  propert(Agent,unconscious,true).

can_walk(Pos):-
  land(Pos,plain).

can_walk(Pos):-
  land(Pos,mountain).
  
actual_stamina(Stamina):-
	property([agent,me],stamina, Stamina).





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

preconditions(turn(_)).
	
preconditions(pickup(Object)):-
  actual_position(Pos),
  at(Object,Pos),
  !.

preconditions(drop(Object)):-
  has([agent,me],Object),
  !.

preconditions(take_from(Object,Grave)):-
  is_open(Grave),
  has(Grave,Object),
  actual_position(Pos),
  at(Grave,Pos),
  !.

preconditions(attack(Agent)):-
  actual_position(Pos),
  actual_direction(Dir),
  findall(Position,pos_in_attack_range(Pos,Dir,Position),Positions),
  at(Agent,AgentPos),
  member(AgentPos,Positions),
  not(is_unconscious(Agent)),
  !.

preconditions(cast_spell(open(Grave))):-
  not(is_open(Grave)),
  actual_position(Pos),
  at(Grave,Pos),
  has([agent,me],[opening_potion,_]),
  !.

preconditions(cast_spell(sleep(Agent))):-
  has([agent,me],[sleep_potion,_]),
  actual_position(Pos),
  at(Agent,Pos),
  not(is_unconscious(Agent)),
  !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliars
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
explore(Pos):-
 	retractall(unexplored(Pos)).
 
 need_stamina():-
 	actual_stamina(Stamina),
 	150 > Stamina,
 	assert(need_stamina).
 	