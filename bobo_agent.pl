%==============================================================================
% Agent Bobo main module
%==============================================================================

:- dynamic turn/1, has/2, at/2, entity_descr/2, land/2.

:- dynamic turn/1, seen_entity/3, hostel/2, grave/2.

:- dynamic im_going_to_hostel/0.

:- 	consult(ag_primitives),
	consult(extras_for_agents),
	consult(bobo_display),
	consult(bobo_decisions),
	consult(bobo_auxiliar).

% Configuration
mapWidth(25).
mapHeight(30).
margin(20).

print_frontier_enabled(false).
print_neighbour_enabled(false).


% Game loop
run:-
	%remove all past fact about what i have
	retract_my_own_perceptions,
	% Get perceptions
    get_percept(Perc),

	% Update agent's internal state
    update_state(Perc),
    
	% Display agent internal state
    display_agent_status(Perc),

    % To debug
    display_agent_simulated_stamina,
    	
	% Display current state
	display_fixed_entities,
	    
    % Choose action to take
    decide_action(Action),

	% Actually take the action
    do_action(Action),
    
    show_turn_decoration,    

	% Loop
    run.

%==============================================================================
%  Internal State Update
%==============================================================================

update_state(Perceptions):-
	% Remove last turn fact	
	retractall(turn(_)),
		
	% Store the perceptions
	save_perceptions(Perceptions).
	
%==============================================================================
%	Save perceptions
%	Store new perceptions and remove false beliefs.
%==============================================================================
  
save_perceptions(Perceptions):-
	
	show_perception_analysis_title,
	
	% Analyse and store enviroment information
	analyse_perception(Perceptions),

	% Delete not existing objects from beliefs
	delete_missing_seen_entities(Perceptions).

%==============================================================================
%	Delete Missing Seen Entities
%	Removes beliefs that are not true anymorer due to environment changes.
%==============================================================================

delete_missing_seen_entities(Perceptions):-	
	% Forget seen objects really not existing in viewing position 		
	forall(
       (
           member(land(IterationPos,_), Perceptions),
           seen_entity(EntitySeen, IterationPos, _),
           not(member(at(EntitySeen, IterationPos),Perceptions))
        ),
        (
        	show_missing_entity(EntitySeen),
        	retractall(seen_entity(EntitySeen, IterationPos, _)),
        	retractall(at(EntitySeen,IterationPos))
        )
	).
	
%==============================================================================
%	Perception Set Analysis
%	Matches beliefs with new facts to have a consistent view of the world
%==============================================================================

analyse_perception([]).

% Hostels will be stored apart to know where they are in every moment
analyse_perception([Perception|ReminderPerc]):- 
	Perception = at([hostel,Name],Pos),
	
	show_message(hostel_found),
	
	% Update hostel location fact
	retractall(hostel(Name,Pos)),
	assert(hostel(Name,Pos)),
	
	% Update hostel location belief
	retractall(Perception),
	assert(Perception), !,
	
	% Call recursively with the tail of the list
	analyse_perception(ReminderPerc).
	
% Graves will be stored apart to know where they are in every moment
analyse_perception([Perception|ReminderPerc]):- 
	Perception = at([grave,Name],Pos),
	
	show_message(grave_found),
	
	% Update grave location fact
	retractall(grave(Name,Pos)),
	assert(grave(Name,Pos)),
	
	% Update grave location belief
	retractall(Perception),
	assert(Perception), !,
	
	% Call recursively with the tail of the list
	analyse_perception(ReminderPerc).
	
% Store 'at' belief for every entity, and store it as seen in this turn
analyse_perception([Perception|ReminderPerc]):-
	Perception = at(Entity, Pos),
	turn(T),
	
	% Remove fact of having saw the entity in another place
	retractall(seen_entity(Entity, _, _)),
	retractall(at(Entity,_)),
	retractall(has(_,Entity)),
	
	% Update location and turn of just entity discovery
	assert(seen_entity(Entity, Pos, T)),
	assert(Perception), !,
	
	% Call recursively with the tail of the list
	analyse_perception(ReminderPerc).
	
% Update state of an object being held by an Entity
analyse_perception([Perception|ReminderPerc]):-
	Perception = has(Entity,Entity2),
	% It was seen in another place
	at(Entity2,_),
	
	% Remove the fact that it was seen 
	retractall(at(Entity2,_)),
	retractall(seen_entity(Entity2,_,_)),
	has(Entity3,Entity2),
	Entity3 \= Entity,
	% Remove the fact that its being held by another Entity
	retractall(has(Entity3,Entity2)),
	
	% Update the fact that an Entity has the Entity2
	assert(Perception), !,
		
	% Call recursively with the tail of the list
	analyse_perception(ReminderPerc).

% Update state of an object being held by another Entity
analyse_perception([Perception|ReminderPerc]):-
	Perception = has(Entity,Entity2),
	
	% Other Entity has been the owner
	has(Entity3,Entity2),
	
	% Remove the fact that it was seen 
	retractall(has(Entity3,Entity2)),
	retractall(seen_entity(Entity2,_,_)),
	
	% Remove the fact that its in another position
	retractall(at(Entity2,_)),
	
	% Update the fact that the Entity has the Entity2
	assert(Perception), !,
		
	% Call recursively with the tail of the list
	analyse_perception(ReminderPerc).

		
analyse_perception([Perception|ReminderPerc]):-
	Perception = entity_descr(Entity, Description),
	
	% Update old facts with newer for the remaining entities
	retractall(entity_descr(Entity, _)),
	assert(entity_descr(Entity, Description)), !,
	
	% Call recursively with the tail of the list
	analyse_perception(ReminderPerc).

analyse_perception([Perception|ReminderPerc]):-
	Perception = land(Pos,_),
	
	% Not seen before
	not(land(Pos,_)),
	assert(land(Pos,_)),
	explore_cell(Pos),
	
	% Call recursively with the tail of the list
	analyse_perception(ReminderPerc).

analyse_perception([Perception|ReminderPerc]):-
	% Update old facts with newer for the remaining entities
	retractall(Perception),
	assert(Perception),
	
	% Call recursively with the tail of the list
	analyse_perception(ReminderPerc).
% -----------------------------------------------------------------
% Remove all belief i have about me
retract_my_own_perceptions:-
	retractall(has([agent,me],_)),
	retractall(entity_descr([agent,me], _)).
	

% -----------------------------------------------------------------

:- dynamic ag_name/1.

start_ag:- 
	% Choose random name to be able 
	% to run more than one instance
	Number is random(1000),
	atom_number(Atom, Number), 
	atom_concat(bobo, Atom, AgName),
	
	generate_unexplored,

	AgID = [agent, AgName],
   	register_me(AgID, Status),
   	!,
	write('REGISTRATION STATUS: '),
	write(Status), nl, nl,
	Status = connected,
	assert(ag_name(AgName)),
	run.

s:- start_ag.

start_ag_instance(InstanceID):-
	ag_name(AgName),
    AgClassName = AgName,
    AgInstanceName =.. [AgClassName, InstanceID],
    register_me([agent, AgInstanceName], Status),
    !,
    write('REGISTRATION STATUS: '),
    write(Status), nl, nl,
    Status = connected,
    assert(ag_name(AgInstanceName)),
    run.

si(InstanceID):- start_ag_instance(InstanceID).


% To debug
display_agent_simulated_stamina:-
	write('Simulated Stamina: '), 
	actual_stamina(Stamina),
	writeln(Stamina).