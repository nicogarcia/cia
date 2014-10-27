%=============================================================
%	Decision module
%	Contains the methods to check actions' preconditions
%=============================================================

:- dynamic plan/1.

decide_action(Action) :-
  (
    not(plan(_));
    plan(Plan), Plan = []
  ),
  writeln('No hay ningun plan. Voy a crear uno.'),

  Goals = [[6, 9]],
  actual_position(Pos),
  a_star(Pos, Goals, NewPlan, BestGoal, Cost),

  writeln('El nuevo plan es: '), writeln(NewPlan),
  NewPlan = [ Action | RemainingPlan],
  update_plan(RemainingPlan), !.

decide_action(Action) :-
  plan(Plan), Plan \= [],
  writeln('Continúo con mi plan.'),

  Plan = [ Action | RemainingPlan],
  write('Proxima acción: '), writeln(Action),
  update_plan(RemainingPlan), !.

% Object pickup action
decide_action(pickup(Object)):-
	% Check preconditions for the current action
    preconditions_pickup(Object),
    
	show_action_taken(pickup(Object)),
	
	% Delete entity from location data
    retractall(seen_entity(Object,_,_)),
    retractall(at(Object,_)).

decide_action(turn(Dir)):-
	Random is random(20),
	0 is mod(Random, 5),
	Random2 is Random / 5,
	nth0(Random2, [n, w, e, s], Dir).	

% Move forward decision
decide_action(move_fwd):-
	show_action_taken(move_fwd).

update_plan(Plan) :-
  retractall(plan(_)),
  assert(plan(Plan)).

there_is_no_plan(Plan) :-
  (
    not(plan(Plan));
    plan(Plan), Plan = []
  ).

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

preconditions(turn).
	
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
