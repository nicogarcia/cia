

display_agent_status(Perception):-
	print_separator,
	nl,
	ag_name(AgName),
	write('Agent: '), write(AgName), write('	  '),

	turn(Turn),
	write('turn: '), write(Turn), write('	       '),

    property([agent, me], stamina, AgStamina),
	property([agent, me], max_stamina, AgMaxStamina),
    write('stamina: '), write(AgStamina), write(' / '), 
    write(AgMaxStamina), write('	       '),

	at([agent, me], MyPos),
	write('Pos: '), write(MyPos), write('	       '),

	property([agent, me], dir, MyDir),
	write('Dir: '), writeln(MyDir),
	
	write('\t'),
	forall(has([agent, me], Entity), (write(Entity), write(', '))),
	nl,

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
		show_entity(Entity)
	), 

	print_separator,
	% seen entities	
	writeln('Previously seen entities: '),
	forall(seen_entity(SeenEntity, Pos, SeenTurn), show_entity(SeenEntity, Pos, SeenTurn)).
	
	
show_entity(Entity):-
	at(Entity, Pos),
	write(' '), write(Entity), write('\t\tat\t'), write(Pos), write('.'),
	entity_descr(Entity, Descr),
	write('\tDescr:\t'), write(Descr), write('.'),
	implies(has(Entity, _), (nl, write('\tHas:\t'))),
	forall(has(Entity, Entity2),
	       (write(Entity2), write(', '))),
	nl.

show_entity(Entity):-
	has(_, Entity),
	entity_descr(Entity, Descr),
	write(' '), write(Entity), write('\t\tDescr:\t'), writeln(Descr).
	
show_entity(Entity, Pos, Turn):-
	write(' '), write(Entity), write('\t\tat\t'), write(Pos), write('\tin turn\t'), writeln(Turn).
	
display_fixed_entities :-
	print_separator,
	writeln('Fixed location entities: '),
	forall(hostel(Entity, Pos), (write(' '), write(Entity), write(', '), writeln(Pos))),
	forall(grave(Entity, Pos), (write(' '), write(Entity), write(', '), writeln(Pos))).
	
	
%==========================================================
%              Screen Information Display
%==========================================================

show_perception_analysis_title :-
	writeln('Perception analysis:').
	
print_separator :-
	nl, write('............................................................'),
	writeln('...........................................').

show_message(hostel_found) :-
	writeln('- I see a hostel! I will save it!').
	
show_message(grave_found) :-
	writeln('- I see a grave! I will save it!').
	
show_turn_decoration :-
	write('===================================================='),
	writeln('===================================================').
	
show_missing_entity(Entity) :-
	write('- '), write(Entity), writeln(' isn\'t in the position I saw it anymore.').
 
show_dragon_got_an_entity(Entity) :-
	write('- Dragon got '), write(Entity), write(' from the floor.').
	
show_action_taken(Action) :-
	print_separator,
	write('Action taken: '), writeln(Action), nl.
	 
