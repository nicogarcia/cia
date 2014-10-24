% Predicados auxiliares

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% display_ag
%
% Muestra por consola cierta información básica relevante del agente en
% el turno corriente.

display_ag:-
	nl,
	writeln('-------------------------------------------------------------'),
	nl,
	ag_name(AgName),
	write('Agent: '), write(AgName), write('	  '),

	turn(Turn),
	write('turn: '), write(Turn), write('	       '),

        property([agent, me], stamina, AgStamina),
	property([agent, me], max_stamina, AgMaxStamina),
        write('stamina: '), write(AgStamina), write(' / '), write(AgMaxStamina), write('	       '),

	at([agent, me], MyPos),
	write('Pos: '), write(MyPos), write('	       '),

	property([agent, me], dir, MyDir),
	write('Dir: '), write(MyDir), nl, nl,

        writeln('I remember: '),
	forall(at(Entity, _Pos), display_entity(Entity)),
	forall(has(_, Entity), display_entity(Entity)),
	writeln('en display_ag')
	.

display_entity(Entity):-
	implies(at(Entity, Pos), (write(' '), write(Entity), write(' at '), write(Pos), write('.'))),
	implies(entity_descr(Entity, Descr), (write(' Descr: '), write(Descr), write('.'))),
	implies(has(Entity, _), write(' Has: ')),
	forall(has(Entity, Entity2),
	       (write(Entity2), write(', '))),
	nl.

display_entity(Entity):-
	has(_, Entity),
	entity_descr(Entity, Descr),
	write(' '), write(Entity), write(' descr: '), writeln(Descr).



objects_at_sight(Vision, ObjectsAtSight):-
                         findall([Pos, Obj], (member([Pos, _Land, Objects], Vision), member(Obj, Objects)), ObjectsAtSight).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% pos_in_attack_range(+AgPos, +AgDir, -PosInAttackRange)
%
% Retorna una posición PosInAttackRange dentro del rango de ataque de
% un agente que se encuentra en la posición AgPos, mirando en
% dirección AgDir.

pos_in_attack_range(MyPos, _MyDir, MyPos).

pos_in_attack_range(MyPos, MyDir, FrontPos):-
	ady_at_cardinal(MyPos, MyDir, FrontPos).

pos_in_attack_range(MyPos, MyDir, FrontRightPos):-
	ady_at_cardinal(MyPos, MyDir, FrontPos),
        next_90_clockwise(MyDir, NextDir),
        ady_at_cardinal(FrontPos, NextDir, FrontRightPos).

pos_in_attack_range(MyPos, MyDir, FrontLeftPos):-
      ady_at_cardinal(MyPos, MyDir, FrontPos),
      next_90_clockwise(PrevDir, MyDir),
      ady_at_cardinal(FrontPos, PrevDir, FrontLeftPos).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% next_90_clockwise(?Dir, ?Dir90)
%
% Tiene éxito si Dir90 es la dirección (n, s, e, u o) que resulta de
% girar 90 grados en sentido horario a partir de la dirección Dir.


next_90_clockwise(n, e).
next_90_clockwise(e, s).
next_90_clockwise(s, w).
next_90_clockwise(w, n).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ady_at_cardinal(?Pos, ?Dir, ?PosAtDir)
%
% Tiene éxito si PosAtDir es la posición [X, Y] adyacente a Por en
% la dirección Dir.

ady_at_cardinal([F,C], n, [PredF, C]):- next(PredF, F).
ady_at_cardinal([F,C], e, [F, SuccC]):- next(C, SuccC).
ady_at_cardinal([F,C], s, [SuccF, C]):- next(F, SuccF).
ady_at_cardinal([F,C], w, [F, PredC]):- next(PredC, C).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ady(?Pos, ?AdyPos)
%
% Tiene éxito si AdyPos es una posición [X, Y] adyacente a Pos.

ady([F,C], [F1, C1]):-
	ady_at_cardinal([F,C], _, [F1, C1]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% is_a_direct(?SubType, ?SuperType)
%
% Type inheritance hierarchy
% Establece relación directa de subtipo (mismo significado que
% "extends" de JAVA)
%%  Tal vez debería buscar una manera de traer este código del server, o
%%  tenerlo en común, para no tenerlo duplicado


is_a_direct(dragon, agent).

is_a_direct(hostel, building).

is_a_direct(grave, building).

is_a_direct(treasure, object).

is_a_direct(potion, object).

is_a_direct(opening_potion, potion).

is_a_direct(sleep_potion, potion).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% is_a(?SubType, ?SuperType)
%
% Relación de subtipo (clausura transitiva de is_a_direct).

is_a(Type, Type).

is_a(Type, AncestorType):- is_a_direct(Type, ParentType), is_a(ParentType, AncestorType).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% property(+Entity, +Prop, -Value)
%
% Retorna el valor Value de una propiedad de nombre Prop de una
% entidad Entity.


property(Thing, Prop, Value):-
	entity_descr(Thing, Descr),
	member([Prop, Value], Descr).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% update_prop(+Entity, +Prop, -CurrValue, +NewValue, +Where)
%
% Actualiza la propiedad Prop de la entidad Entity. CurrValue se liga
% con el valor actual de la propiedad, NewValue con el nuevo valor que
% se desea que tenga, y Where (de uso opcional) establece la relación
% entre el valor actual, CurrValue, y el nuevo valor, NewValue.
%
% ACLARACIÓN: la meta Where no debe fallar.
%
% ej:
%update(Ag, stamina, CurrValue, NewValue, NewValue is CurrValue + 1)
%update(Ag, dir, CurrValue, NewValue, next_90_clockwise(CurrValue, NewValue))
%update(Ag, pos, _CurrValue, [1,1], true)


update_prop(Thing, Prop, CurrValue, NewValue, Where):-
		entity_descr(Thing, Descr),
	        replace([Prop, CurrValue], [Prop, NewValue], Descr, NewDescr),
		call(Where), % Cuidado!!! Debería asegurarme que el where no falle!
                retract(entity_descr(Thing, Descr)),
	        assert(entity_descr(Thing, NewDescr)).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% if_fails_do(+Goal, +ExceptionHandler)
%

if_fails_do(Goal, _ExceptionHandler):- call(Goal), !.

if_fails_do(_Goal, ExceptionHandler):- call(ExceptionHandler), fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% implies(+Ant, +Cons)
%
% Tiene éxito si no se satisface Ant, o se satisfacen tanto Ant como
% Cons.


implies(Ant, Cons):- call(Ant), !,
                     call(Cons).

implies(_Ant, _Cons).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% random_member(-Elem, +List)
%
% Versión random del predicado member/2. Dada una lista List
% retorna un elemento Elem de List aleatoriamente. Frente al pedido
% de soluciones alternativas va retornando uno a uno los elementos
% de List en forma aleatoria, y sin repetirlos.

random_member(X, L):-
	random_select(Y, L, LsinY),
	(   X = Y
	;   random_member(X, LsinY)
	).

random_select(X, L, LsinX):-
	length(L, NOfElements),
	NOfElements > 0,
	Random is random(NOfElements) + 1,
	remove_nth(Random, L, X, LsinX).



% remove_nth(N, L, X, LsinX).

remove_nth(1, [X|Xs], X, Xs).

remove_nth(N, [Y|Ys], X, [Y|Xs]):-
	N>1,
	N1 is N-1,
	remove_nth(N1, Ys, X, Xs).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% next(?N, ?Nn)
%
% Tiene éxito si Nn es el sucesor de N. Admite que o bien N o Nn
% vengan sin instanciar (pero no ambos).


next(N,Nn):- not(var(N)), !,
             Nn is N+1.

next(N,Nn):- not(var(Nn)), !,
             N is Nn-1.



/*----------------------------------------------*/
nn_pred(1, 2).

nn_pred(PredM, M):- M > 2, PredM is M - 1.


nnleq(N, M):- nn_pred(PredM, M), nnleq(N, PredM).

nnleq(M, M).

/*----------------------------------------------*/


% replace(+X, +Y, +Set, -NewSet)

replace(X, Y, [X|Xs], [Y|Xs]).

replace(X, Y, [Z|Xs], [Z|NewXs]):-
	X \= Z,
	replace(X, Y, Xs, NewXs).


















