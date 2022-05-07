% name atik
% 2020400261
% compiling: yes
% complete: yes
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

check_dists(NewDist, OldDist, NewKey, _, NewObj, _, NewDist, NewKey, NewObj) :-
    NewDist =< OldDist.

check_dists(NewDist, OldDist, _, OldKey, _, OldObj, OldDist, OldKey, OldObj) :-
    NewDist > OldDist.
   

iterate_list_and_calculate_distances([H], ObjectDict, AgentDict, H, D, ObjectDict.H) :-
    manhattan_distance([ObjectDict.H.x, ObjectDict.H.y], [AgentDict.x, AgentDict.y], D).

iterate_list_and_calculate_distances([Head | Tail], ObjectDict, AgentDict, MinKey, MinDist, MinObj) :-
    % minimum_of_list(Distances, Min),
    
    iterate_list_and_calculate_distances(Tail, ObjectDict, AgentDict, OldMinKey, OldMinDist, OldMinObject),
    manhattan_distance([ObjectDict.Head.x, ObjectDict.Head.y], [AgentDict.x, AgentDict.y], NewDistance),
    check_dists(NewDistance, OldMinDist, Head, OldMinKey, ObjectDict.Head, OldMinObject, MinDist, MinKey, MinObj).
    


find_nearest_type([AgentDict, ObjectDict, _], ObjectType, ObjKey, Object, Distance) :-
    findall(X, ObjectDict.X.type=ObjectType, Keys),
    iterate_list_and_calculate_distances(Keys, ObjectDict, AgentDict, ObjKey, Distance, Object).
    % write_ln(Distances),
    % minimum_of_list(Distances, Distance).
    
    % dict_pairs(ObjectDict, _, Pairs),
    % pairs_values(Pairs, Values), % list of object dict
    % iterate_list_and_calculate_distances(Values, ObjectType, AgentDict, Distances),
    % minimum_of_list(Distances, Distance).


% 10 points
% navigate_to(+State, +X, +Y, -ActionList, +DepthLimit) :- .
append_n(List1, List2, List2,0).
append_n(List1, List2, ListOut,1) :-
    append(List1, List2, ListOut), !.
append_n(List1, List2, ListOut, N) :-
    N > 1,
    X is N-1,
    append_n(List1, List2, NewListOut, X),
    append(List1, NewListOut, ListOut).

% navigate_to_y([AgentDict, _, _], _, Dy, ActionList, DepthLimit) :-
%     DepthLimit > 0,
%     AgentDict.y > Dy,
%     N is AgentDict.y - Dy,
%     NewDepthLimit is DepthLimit - (AgentDict.y - Dy),
%     NewDepthLimit >= 0,
%     append_n([go_up], [], ActionList, N).


% navigate_to_y([AgentDict, _, _], _, Dy, ActionList, DepthLimit) :-
%     DepthLimit > 0,
%     AgentDict.y < Dy,
%     N is Dy - AgentDict.y,
%     NewDepthLimit is DepthLimit - (Dy - AgentDict.y),
%     NewDepthLimit >= 0,
%     append_n([go_down], [], ActionList, N).



% navigate_to([AgentDict, ObjectDict, _], Dx, Dy, ActionList, DepthLimit) :-
%     DepthLimit > 0,
%     AgentDict.x > Dx,
%     N is AgentDict.x - Dx,
%     NewDepthLimit is DepthLimit - (AgentDict.x - Dx),
%     navigate_to_y([AgentDict, ObjectDict, _], Dx, Dy, List, NewDepthLimit),
%     append_n([go_left], List, ActionList, N).


% navigate_to([AgentDict, ObjectDict, _], Dx, Dy, ActionList, DepthLimit) :-
%     DepthLimit > 0,
%     Dx > AgentDict.x,
%     N is Dx - AgentDict.x,
%     NewDepthLimit is DepthLimit - (Dx - AgentDict.x),
%     navigate_to_y([AgentDict, ObjectDict, _], Dx, Dy, List, NewDepthLimit),
%     append_n([go_right], List, ActionList, N).

navigate_to([AgentDict, ObjectDict, T], Dx, Dy, ActionList, DepthLimit) :-
    Ax is AgentDict.x - Dx,
    abs(Ax, Nx),
    Nx =< DepthLimit,
    (
        (Ax < 0) -> (
            append_n([go_right], [], List, Nx)
        );
        (append_n([go_left], [], List, Nx))
    ),
    Ay is AgentDict.y - Dy,
    abs(Ay, Ny),
    Total is Ny + Nx,
    Total =< DepthLimit,
    (
        (Ay < 0) -> (
        append_n([go_down], List, ActionList, Ny)
        );
        (append_n([go_up], List, ActionList, Ny))
    ).

% 10 points
% chop_nearest_tree(+State, -ActionList) :- .
insert_at_end(X,Y,Z) :- append(Y,X,Z).

append_end_n(List1, List2, ListOut,1) :-
    !,
    insert_at_end(List1, List2, ListOut).
append_end_n(List1, List2, ListOut, N) :-
    N > 1,
    X is N-1,
    append_end_n(List1, List2, NewListOut, X),
    insert_at_end(List1, NewListOut, ListOut).

chop_nearest_tree([AgentDict, ObjectDict, T], ActionList) :-
    find_nearest_type([AgentDict, ObjectDict, T], tree, _, Obj, Dist),
    navigate_to([AgentDict, ObjectDict, T], Obj.x, Obj.y, MoveList, Dist),
    append_end_n([left_click_c], MoveList, ActionList, 4).
    % get_dict(tree, AgentDict.inventory, CurNoTree),
    % NewNoTree is CurNoTree + 1,
    % put_dict(bag{tree:NewNoTree}, AgentDict.inventory, NewD).
    

% 10 points
% mine_nearest_stone(+State, -ActionList) :- .

mine_nearest_stone([AgentDict, ObjectDict, T], ActionList) :-
    notrace,
    find_nearest_type([AgentDict, ObjectDict, T], stone, _, Obj, Dist),
    navigate_to([AgentDict, ObjectDict, T], Obj.x, Obj.y, MoveList, Dist),
    append_end_n([left_click_c], MoveList, ActionList, 4).


% 10 points
% gather_nearest_food(+State, -ActionList) :- .

gather_nearest_food([AgentDict, ObjectDict, T], ActionList) :-
    find_nearest_type([AgentDict, ObjectDict, _], food, _, Obj, Dist),
    navigate_to([AgentDict, ObjectDict, T], Obj.x, Obj.y, MoveList, Dist),
    append_end_n([left_click_c], MoveList, ActionList, 1).

% 10 points
% collect_requirements(+State, +ItemType, -ActionList) :- .

% collect_for_item([log, Amount], ActionList) :-
%     %  chop nearest tree Amount times.

% collect_for_item(AgentDict, [stone, Amount], ActionList) :-
    
% collect_for_item(AgentDict, [stick, Amount], ActionList) :-


% collect_for_list(AgentDict, [], []).
% collect_for_list(AgentDict, [H | T], ActionList, NewAgentDict) :-
%     collect_for_list(AgentDict, T, List, Mid),
%     collect_for_item(AgentDict, H, ItemList),
%     append(ItemList, List, ActionList),
%     make_moves(Mid2, ItemList, NewAgentDict).

count_type_in_map(ObjectDict, Type, Count) :-
    findall(X, ObjectDict.X.type=Type, Keys),
    length(Keys, Count).

has_enough(ObjectDict, []).
has_enough(ObjectDict, [[Type, Count] | T]) :-
    count_type_in_map(ObjectDict, Type, TypeCount),
    Count =< TypeCount,
    has_enough(ObjectDict, T).

agent_will_need(AgentDict, [], []).
agent_will_need(AgentDict, [[Type, Count] | Tail], OutReqList) :-
    agent_will_need(AgentDict, Tail, List),
    (
        (has(Type, 1, AgentDict.inventory)) -> (
            (Count - AgentDict.inventory.Type < 0) -> (
                NeedCount is 0
            );
            (NeedCount is Count - AgentDict.inventory.Type)
        );
        (NeedCount is Count)
    ),
    (
        (NeedCount > 0) -> (
            append([[Type, NeedCount]], List, OutReqList)
        );
        (append([], List, OutReqList))
    ).


is_empty([]).


collect_requirements([AgentDict, ObjectDict, _], stick, ActionList) :-
        chop_nearest_tree([AgentDict, ObjectDict, _], ActionList).

% TODO: mine stone only checks for stone, might need single cobble so check for that too.
collect_requirements([AgentDict, ObjectDict, T], Type, ActionList) :-
    (not(has(Type, 1, AgentDict.inventory))) -> (
        item_info(Type, R, _),
        findall([X, Y], R.X=Y, List),
        agent_will_need(AgentDict, List, Reqs),
        (
            (not(is_empty(Reqs))) -> (
                (
                    (member([log, _], Reqs)) -> (
                        chop_nearest_tree([AgentDict, ObjectDict, T], LogActions)
                    );
                    (LogActions=[])
                ),
                (
                    (not(is_empty(LogActions))) -> (
                        execute_actions([AgentDict, ObjectDict, T], LogActions, AfterLogActionsState)
                    );
                    (AfterLogActionsState = [AgentDict, ObjectDict, T])
                ),
                (
                    
                    (member([cobblestone, _], Reqs)) -> (
                        mine_nearest_stone(AfterLogActionsState, StoneActions)    
                    );
                    (StoneActions=[])
                ),
                (
                    (not(is_empty(StoneActions))) -> (
                        execute_actions(AfterLogActionsState, StoneActions, AfterStoneActionsState)
                    );
                    (AfterStoneActionsState = AfterLogActionsState, write_ln(AfterStoneActionsState))    
                ),
                (
                    
                    (member([stick, _], Reqs)) -> (
                        collect_requirements(AfterStoneActionsState, stick, MidStickActions),
                        append_end_n([craft_stick], MidStickActions, StickActions, 1)
                        % (N is 2) -> (
                        %     execute_actions(AfterStoneActionsState, FirstStickActions, AfterFirstStickState),
                        %     collect_requirements(AfterFirstStickState, stick, SecondStickActions),
                        %     append(FirstStickActions, SecondStickActions, StickActions)
                        % );
                        % (StickActions=FirstStickActions)
                    );
                    (StickActions=[])   
                ),
                append(StickActions, [], M),
                append(StoneActions, M, M2),
                append(LogActions, M2, ActionList)
            );
            (ActionList=[])
        )
    );
    (ActionList=[]).


% 5 points
% find_castle_location(+State, -XMin, -YMin, -XMax, -YMax) :- .

my_tile_occupied(X, Y, State) :-
    State = [_, StateDict, _],
    get_dict(_, StateDict, Object),
    get_dict(x, Object, Ox),
    get_dict(y, Object, Oy),
    X = Ox, Y = Oy.

find_castle_location([AgentDict, ObjectDict, T], Xmin, Ymin, Xmax, Ymax) :-
    width(A), height(B),
    Width is A - 2, Height is B -2,
    S = [AgentDict, ObjectDict, T],
    findall(
        [X , Y],
        (
            between(1, Width, X),
            between(1, Height, Y),
            X + 2 =< Width,
            Y + 2 =< Height,

            not(my_tile_occupied(X, Y, S)), 
            X1 is X +1, X2 is X +2, Y1 is Y+1, Y2 is Y+2,
            
                                            not(my_tile_occupied(X1, Y, S)), not(my_tile_occupied(X2, Y, S)),
            not(my_tile_occupied(X, Y1, S)), not(my_tile_occupied(X1, Y1, S)), not(my_tile_occupied(X2, Y1, S)),
            not(my_tile_occupied(X, Y2, S)), not(my_tile_occupied(X1, Y2, S)), not(my_tile_occupied(X2, Y2, S))
        ),
        List
    ),
    write_ln(List),
    List = [H | _],
    H = [Xmin, Ymin],
    Xmax is Xmin + 2,
    Ymax is Ymin + 2,

    Xmax =< Width,
    Ymax =< Height.

% 15 points
% make_castle(+State, -ActionList) :- 

make_castle([AgentDict, ObjectDict, T], ActionList) :-
    find_castle_location([AgentDict, ObjectDict, T], Xmin, Ymin, Xmax, Ymax),
    % Requirements = [[cobblestone, 9]],
    % agent_will_need(AgentDict, Requirements, AgentNeeds),
    % write_ln(AgentNeeds), % [[cobblestone, N]]
    % AgentNeeds = [[cobblestone, NeededCobbles]],
    % findall(
    %     X,
    %     ObjectDict.X.type = stone,
    %     StoneList
    % ),
    % findall(
    %     Y,
    %     ObjectDict.Y.type = cobblestone,
    %     CobbleList    
    % ),
    % length(StoneList, StonesInMap),
    % length(CobbleList, CobblesInMap),
    mine_nearest_stone([AgentDict, ObjectDict, T], FirstStoneActions),
    execute_actions([AgentDict, ObjectDict, T], FirstStoneActions, AfterFirstStoneState),

    mine_nearest_stone(AfterFirstStoneState, SecondStoneActions),
    execute_actions(AfterFirstStoneState, SecondStoneActions, AfterSecondStoneState),

    mine_nearest_stone(AfterSecondStoneState, ThirdStoneActions),
    execute_actions(AfterSecondStoneState, ThirdStoneActions, AfterThirdStoneState),
    write_ln(AfterThirdStoneState),
    [A, _, _] = AfterThirdStoneState,
    
    Ox is Xmin + 1,
    Oy is Ymin + 1,

    manhattan_distance([A.x, A.y], [Ox, Oy], DepthLimit),

    navigate_to(AfterThirdStoneState, Ox, Oy, GoToCastleStartActions, DepthLimit),

    append([place_c, place_n, place_ne, place_e, place_se, place_s, place_sw, place_w, place_nw], [], M),
    append(GoToCastleStartActions, M, M2),

    append(ThirdStoneActions, M2, Mid),
    append(SecondStoneActions, Mid, Mid2),
    append(FirstStoneActions, Mid2, ActionList).




