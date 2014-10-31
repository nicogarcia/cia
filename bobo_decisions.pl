
%=============================================================
%	Decision module
%	Contains the methods to check actions' preconditions
%=============================================================

:- dynamic plan/1,unexplored/1,goals/1.

% Object pickup action
decide_action(Action):-
	inmediate_action(Action),
	writeln('inmediate_action'),
	!.

decide_action(Action):-
  charge_stamina(Action),
  !.

decide_action(Action):-
		follow_plan(Action),
		writeln('folow_plan'),
		!.
decide_action(Action):-
	search_objects_and_graves(Action),
	writeln('Search objects and graves'),
	!.

decide_action(Action) :-
	  search_object(Action),
	  writeln('search_object'),
	  !.

decide_action(Action):-
		explore(Action),
		writeln('explore'),
		!.

decide_action(Action):-
  need_stamina(Action),
  writeln('need_stamina').

decide_action(move_fwd).


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
  land(Pos,plain).

can_walk(Pos):-
  land(Pos,mountain).
  
actual_stamina(Stamina):-
	property([agent,me],stamina, Stamina1),
  % To debug
	Stamina is Stamina1.

actual_max_stamina(MaxStamina):-
  property([agent,me],maxstamina,MaxStamina1),
  % To debug
  MaxStamina is MaxStamina1.

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
  writeln('checking precond'),
  Grave = [grave,_],
  actual_position(Pos),
  at(Grave,Pos),
  format('im in the same position as the grave ~w', [Grave]),
  is_open(Grave),
  writeln('grave open'),
  format('object: ~w, grave: ~w', [Object, Grave]),
  forall(
    has(Grave, Obj),
    writeln(Obj)
  ),
  has(Grave,Object),
  writeln('grave has object i think'),
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
  Agent = [Type,Name],
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

is_open(Grave):-
	property(Grave, open, true).
 	
 	