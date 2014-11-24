%==============================================================================
%	Auxiliar functions module
%==============================================================================

:- dynamic borders/1, exploring/0.

borders([]).
side_sight(2).
forward_sight(3).

%==============================================================================
%	Updates unexplored borders
%	
%	Calculates the border of the explored map to reduce the exploration
%	goals, finding all the unexplored positions of the map that are 
%	currently in the border or belong to the border of the currently
%	visible positions of the agent.
%==============================================================================
update_borders([X, Y], Dir) :-
	side_sight(SideSight),
	forward_sight(ForwardSight),
	borders(CurrentBorders),

	map_from_game([X, Y], [XX, YY]),

	Left is -SideSight - 1,
	Right is SideSight + 1,
	Top is ForwardSight + 1,
	Bottom is -1,
	
	/*forall(
		(
			LeftP1 is Left + 1, RightM1 is Right - 1,
			BottomP1 is Bottom + 1, TopM1 is Top - 1,  
			(between(LeftP1, RightM1, X1), between(BottomP1, TopM1, Y1)),
			rotate(Dir, [X1, Y1], [RotX, RotY]),
			FinalX is XX + RotX,
			FinalY is YY + RotY,
	
			map_to_game([FinalX, FinalY], [FinalXG, FinalYG])
		),
		explore_cell([FinalXG, FinalYG])
	),*/
	
	% Find all positions that...
	findall(
		% ...position Pos
		[FinalXG, FinalYG],
		(
			% it's unexplored, valid and...
			unexplored([FinalXG, FinalYG]),
			contained_in_map([FinalXG, FinalYG]),
			(
				% it's currently a border
				member([FinalXG, FinalYG], CurrentBorders);
				(
				% Or it's in the new border
					(			
						(between(Left, Right, X1), Y1 = Top);
						(between(Left, Right, X1), Y1 = Bottom);
						(between(Bottom, Top, Y1), X1 = Right);
						(between(Bottom, Top, Y1), X1 = Left)
					),
					rotate(Dir, [X1, Y1], [RotX, RotY]),
					FinalX is XX + RotX,
					FinalY is YY + RotY,
		
					map_to_game([FinalX, FinalY], [FinalXG, FinalYG])
				)
			)
		),
		Borders
	),
	sort(Borders, SortedBorders),
	retractall(borders(_)),
	assert(borders(SortedBorders)).
	

% Rotates a pos to a given direction

rotate(s, [X, Y], [X, Y]).

rotate(w, [X, Y], [Y, MenosX]):-
  	MenosX is -X.
	
rotate(e, [X, Y], [MenosY, X]):-
	MenosY is -Y.
	
rotate(n, [X, Y], [MenosX, MenosY]):-
	MenosY is -Y,
	MenosX is -X.
	
%------------------------------------------------------------------------------
% Transforms the coordinates between the game and universal coordinates
% that start from [0, 0] and X grows to the east and Y to the north.
%------------------------------------------------------------------------------
map_from_game([X, Y], [X1, Y1]) :-
	X1 is -(Y - 1),
	Y1 is X - 1.

map_to_game([X, Y], [X1, Y1]):-
	X1 is Y + 1,
	Y1 is -X + 1.
	
%------------------------------------------------------------------------------
% Tests if a position belongs to the map
%------------------------------------------------------------------------------
contained_in_map([X | [Y]]) :-
	mapWidth(W),
	mapHeight(H),
	X > 0, X =< W,	
	Y > 0, Y =< H.
%------------------------------------------------------------------------------
% Maps a direction translation transformation to a cardinal point
%------------------------------------------------------------------------------
map_dir_to_cardinal(Dir, Cardinal):-
	Dir = [-1, 0], Cardinal = n.

map_dir_to_cardinal(Dir, Cardinal):-
	Dir = [0, 1], Cardinal = e.

map_dir_to_cardinal(Dir, Cardinal):-
	Dir = [0, -1], Cardinal = w.

map_dir_to_cardinal(Dir, Cardinal):-
	Dir = [1, 0], Cardinal = s.
	
%------------------------------------------------------------------------------
% Gets the cost of going from one position to one of his neighbours
%------------------------------------------------------------------------------
cost(Pos1, Pos2, Dir, Result) :-
	get_direction(Pos2, Pos1, Dir),
	land_cost(Pos2, Land_cost),
	Result is Land_cost, !.

cost(_, Pos2, _, Result) :-
	land_cost(Pos2, Land_cost),
	Result is Land_cost + 1.

%------------------------------------------------------------------------------
% Gets the cost of a position
%------------------------------------------------------------------------------
land_cost(Pos,Land_cost):-
	land(Pos,plain),
	Land_cost = 1, !.

land_cost(Pos,Land_cost):-
	land(Pos,mountain),
	Land_cost = 2, !.
	
land_cost(Pos, Land_cost):-
	exploring,
	Land_cost = 4.

%------------------------------------------------------------------------------
% Calculates Manhattan's distance
%------------------------------------------------------------------------------
manhattan_distance([X1 | [Y1]], [X2 | [Y2]], Dist) :-
	DifX is X1 - X2,
	DifY is Y1 - Y2,
	Dist is abs(DifX) + abs(DifY).
	
%------------------------------------------------------------------------------
% Gets the direction translation transformation from two adjacent positions
%------------------------------------------------------------------------------
get_direction([X1 | [Y1]], [X2 | [Y2]], Result) :-
	X is X1 - X2,
	Y is Y1 - Y2,
	Result = [X , Y].
	
%------------------------------------------------------------------------------
% Gets the position in direction (translation transformation)
%------------------------------------------------------------------------------
get_position_in_direction(Pos, Dir, PosInDir):-
	Pos = [X1, Y1], Dir = [X2, Y2],
	DifX is X1 + X2,
	DifY is Y1 + Y2,
	PosInDir = [DifX, DifY].
	
%------------------------------------------------------------------------------
% Generates all [X, Y] positions of the map and sets them as unexplored
%------------------------------------------------------------------------------
generate_unexplored:-
	mapWidth(W),
	mapHeight(H),
	forall(
		(between(1,W, X), between(1,H,Y)),
		assert(unexplored([X,Y]))
	).

%------------------------------------------------------------------------------
% Pretty use of if then else clauses
%------------------------------------------------------------------------------
if_then(Cond, Then) :-
	Cond -> call(Then); true.

if_then_else(Cond, Then, Else) :-
	Cond -> call(Then); call(Else).
	
%---------------------------------------------------------------
% Return if an Entity can be pickup
pickable_object(Entity):-
	(
		Entity = [treasure, _];
		Entity = [sleep_potion,_];
		Entity = [opening_potion,_]
	).

openable_grave(Entity):-
	Entity = [grave,_],
	has(Entity,_),
	i_have_opening_potion.
	
get_random_pos(Pos):-	
	mapWidth(W),
	mapHeight(H),
	RW is random(W),
	RH is random(H),
	TempPos = [RW,RH],
	if_then_else(
		can_walk(TempPos),
		Pos = TempPos,
		get_random_pos(Pos)
	).
	


