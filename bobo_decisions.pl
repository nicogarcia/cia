%=============================================================
%	Decision module
%	Contains the methods to check actions' preconditions
%=============================================================

decide_action(Action):-	
	% Get agent position and direction
    at([agent, me], Pos),
    property([agent, me], dir, Dir),

	% Get the position in fron of me in my direction
    ady_at_cardinal(Pos, Dir, PosInFront),
    
    % Get the terrain type of the position in front
    land(PosInFront, Land),      
      
    (
       Land = water
           ;
       Land = forest
           ;
       at([EntityType, EntityName], PosInFront),
       is_a(EntityType, agent),
       write('aaaahhhhh, no me ataques '), write(EntityName), write('!!!'),nl %si es un agente, y entonces lo esquiva!!!
    ),
    next_90_clockwise(Dir, DesiredDir),
    Action = turn(DesiredDir),
    show_action_taken(Action).

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
