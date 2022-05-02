% name surname
% studentid
% compiling: no
% complete: no
:- ['cmpecraft.pro'].

:- init_from_map.

% 10 points
% manhattan_distance(+A, +B, -Distance) :- .
manhattan_distance([X1, Y1], [X2, Y2], D) :- D is abs(X1-X2) + abs(Y1-Y2).

% 10 points
% minimum_of_list(+List, -Minimum) :- .
minimum_of_list([H | T], Min) :- minimum_of_list(T, H, Min).
minimum_of_list([], Min, Min).
minimum_of_list([H | T], Min0, Min) :-
    Min1 is min(H, Min0),
    minimum_of_list(T, Min1, Min).

% 10 points
% find_nearest_type(+State, +ObjectType, -ObjKey, -Object, -Distance) :- .
iterate_list_and_calculate_distances([], ObjectType, AgentDict, []).

iterate_list_and_calculate_distances([object{hp:_, type:ObjectType, x:X, y:Y} | Tail], ObjectType, AgentDict, Distances) :-
    iterate_list_and_calculate_distances(Tail, ObjectType, AgentDict, List),
    manhattan_distance([X, Y], [AgentDict.x, AgentDict.y], NewDistance),
    append([NewDistance], List, Distances).


iterate_list_and_calculate_distances([object{hp:_, type:Type, x:X, y:Y} | Tail], ObjectType, AgentDict, Distances) :-
    not(Type = ObjectType),
    iterate_list_and_calculate_distances(Tail, ObjectType, AgentDict, Distances).    

find_nearest_type([AgentDict, ObjectDict, Time], ObjectType, ObjKey, Object, Distance) :-
    % manhattan_distance([AgentDict.x, AgentDict.y], [ObjectDict.0.x, ObjectDict.0.y], Distance),
    dict_pairs(ObjectDict, _, Pairs),
    pairs_values(Pairs, Values), % list of object dict
    iterate_list_and_calculate_distances(Values, ObjectType, AgentDict, Distances),
    minimum_of_list(Distances, Distance).



% 10 points
% navigate_to(+State, +X, +Y, -ActionList, +DepthLimit) :- .
% 10 points
% chop_nearest_tree(+State, -ActionList) :- .
% 10 points
% mine_nearest_stone(+State, -ActionList) :- .
% 10 points
% gather_nearest_food(+State, -ActionList) :- .
% 10 points
% collect_requirements(+State, +ItemType, -ActionList) :- .
% 5 points
% find_castle_location(+State, -XMin, -YMin, -XMax, -YMax) :- .
% 15 points
% make_castle(+State, -ActionList) :- .
