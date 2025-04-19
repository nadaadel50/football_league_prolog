 %%%%%%%%%%%%%%%%%%%%%%%%%%%% Teams Info %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Team(Name, Country, Num_of_winning_times)

team(barcelona, spain, 5).
team(real_madrid, spain, 6).
team(manchester_united, england, 3).
team(liverpool, england, 4).
team(juventus, italy, 7).
team(bayern_munich, germany, 8).
team(psg, france, 3).
team(inter_milan, italy, 3).
team(ajax, netherlands, 4).
team(porto, portugal, 7).

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Players Info %%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Player(Name, Team, Position)

player(messi, barcelona, forward).
player(ronaldo, real_madrid, forward).
player(pogba, manchester_united, midfielder).
player(salah, liverpool, forward).
player(modric, real_madrid, midfielder).
player(alisson, liverpool, goalkeeper).
player(ter_stegen, barcelona, goalkeeper).
player(varane, manchester_united, defender).
player(dybala, juventus, forward).
player(lewandowski, bayern_munich, forward).
player(neymar, psg, forward).
player(van_dijk, liverpool, defender).
player(ben_yedder, monaco, forward).
player(ronaldo_silva, manchester_city, midfielder).
player(de_jong, barcelona, midfielder).
player(ruben_dias, manchester_city, defender).

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Matches Info %%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Match(Team1, Team2, Team1Goals, Team2Goals)

match(barcelona, real_madrid, 3, 2).
match(manchester_united, liverpool, 1, 1).
match(real_madrid, manchester_united, 2, 0).
match(liverpool, barcelona, 2, 2).
match(juventus, inter_milan, 1, 2).
match(bayern_munich, psg, 3, 1).
match(ajax, porto, 2, 1).
match(manchester_city, juventus, 2, 2).
match(liverpool, bayern_munich, 0, 3).
match(real_madrid, ajax, 4, 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Goals Info %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Goals(Player, Num_of_scored_goals)

goals(messi, 10).
goals(ronaldo, 12).
goals(pogba, 5).
goals(salah, 11).
goals(modric, 4).
goals(alisson, 0).
goals(dybala, 8).
goals(lewandowski, 9).
goals(neymar, 7).
goals(van_dijk, 2).
goals(ben_yedder, 6).
goals(ronaldo_silva, 3).
goals(de_jong, 4).
goals(ruben_dias, 1).
goals(ter_stegen, 0).


%Task 1

%Base Case
member_custom(X, [X|_]).

member_custom(X, [_|Tail]):-
member_custom(X, Tail).

%Base Case
append_custom([], L1, L1).

append_custom([H|T], L2, [H|NT]):-
append_custom(T, L2, NT).



players_in_team(Team, Players):-
    all_unique_players(All),
    filter_players(Team, All, Players),
    !.


all_unique_players(Res_List):-
    get_Players([], Res_List).

%Base Case
get_Players(Acc,Acc):-
    \+ (player(P, _, _),\+ member_custom(P, Acc)).

get_Players(Acc,Res_List):-
    player(P, _, _),
    \+ member_custom(P, Acc),
    append_custom(Acc,[P], NewAcc),
    get_Players(NewAcc, Res_List).


%Base Case
filter_players(_, [], []).

filter_players(Team, [P | Rest], [P | Players]):-
    player(P, Team, _),
    filter_players(Team, Rest, Players).

filter_players(Team, [_ | Rest], Players):-
    filter_players(Team, Rest, Players).



% TASK 2

team_count_by_country(Country, Count) :-
    count_teams_country(Country,Count,0,[]),
    !.

count_teams_country(Country,Count,Counter,VisitedTeams):-
    team(Team, Country, _),
    not_in_visited(Team, VisitedTeams),
    New_counter is Counter + 1,
    count_teams_country(Country, Count, New_counter, [Team|VisitedTeams]).
count_teams_country(_, Count, Count, _).

not_in_visited(_, []).
not_in_visited(X, [Y|T]):-
    X \= Y,
    not_in_visited(X,T).



%Task 3

most_successful_team(Team) :-
    team(FirstTeamInData, _, FirstWinningTimes),
    get_most_successful_team(FirstTeamInData, FirstWinningTimes, Team),
    !.

get_most_successful_team(CurrentBestTeam, CurrentBestWinningTimes, TheBest):-
    team(NextTeam, _, NextWinningTimes),
    NextTeam \= CurrentBestTeam,
    NextWinningTimes > CurrentBestWinningTimes,
    get_most_successful_team(NextTeam, NextWinningTimes, TheBest).

%Base Case
get_most_successful_team(TheBest, _, TheBest).



% TASK 4

matches_of_team(Team, L) :-
    match_team(Team, [], L),
    !.

match_team(Team, ListAdd, L) :-
    match(Team1, Team2, Goals1, Goals2),
   (Team1 = Team; Team2 = Team),
    not_in_visited((Team1, Team2, Goals1, Goals2), ListAdd),
    match_team(Team,[(Team1, Team2, Goals1, Goals2)|ListAdd],L).
match_team(_,L,L).



%Task 5

num_matches_of_team(Team, C) :-
    all_matches(AllMatches),
    count_matches(Team, AllMatches, C),
    !.

all_matches(Matches):-
    collect_all_matches([], Matches).


collect_all_matches(Acc, Matches):-
    match(Team1, Team2, Team1Goals, Team2Goals),
    \+ member_custom([Team1, Team2, Team1Goals, Team2Goals], Acc),
    collect_all_matches([[Team1, Team2, Team1Goals, Team2Goals]| Acc], Matches).

%Base Case
collect_all_matches(Matches, Matches).

%Base Case
count_matches(_, [], 0).

count_matches(Team, [[Team1, Team2, _, _] | Rest], C):-
    (Team = Team1 ; Team = Team2),
    count_matches(Team, Rest, C2),
    C is C2 + 1.

count_matches(Team, [_ | Rest], C):-
    count_matches(Team, Rest, C).



% TASK 6

top_scorer(P) :-
    goals(Player, NumGoals),
    max_goals(Player, NumGoals, P),
    !.

max_goals(_, MaxGoals, P):-
    goals(Player, NumGoals),
    NumGoals > MaxGoals,
    max_goals(Player, NumGoals, P).
max_goals(P, _, P).



%Task 7


most_common_position_in_team(Team, MostCommonPos) :-
    get_players_in_team(Team, Players),
    get_positions(Team, Players, Positions),
    remove_duplicates(Positions, UniquePositions),
    count_pos_for_each_position(Players, UniquePositions, CountedPositions),
    get_common_position(CountedPositions, none, 0, MostCommonPos),
    !.


get_players_in_team(Team, Players) :-
    collect_all_team_players(Team, [], Players).

collect_all_team_players(Team, Seen, [[P, Team, Pos] | Rest]) :-
    player(P, Team, Pos),
    \+ member_custom([P, Team, Pos], Seen),
    collect_all_team_players(Team, [[P, Team, Pos] | Seen], Rest).

%Base Case
collect_all_team_players(_, _, []).

%Base Case
get_positions(_, [], []).

get_positions(Team, [[_, Team, Pos] | Rest], [Pos | Positions]) :-
    get_positions(Team, Rest, Positions).
get_positions(Team, [[_, OtherTeam, _] | Rest], Positions) :-
    Team \= OtherTeam,
    get_positions(Team, Rest, Positions).




%Base Case
remove_duplicates([], []).

remove_duplicates([Pos | Positions], Removed) :-
     member_custom(Pos, Positions),
    remove_duplicates(Positions, Removed).

remove_duplicates([Pos | Positions], [Pos | Removed]) :-
     \+ member_custom(Pos, Positions),
    remove_duplicates(Positions, Removed).


%Base Case
count_pos_for_each_position(_, [], []).
count_pos_for_each_position(Players, [Pos | Rest], [[Pos, Counter] | Counters]) :-
    count_position(Pos, Players, Counter),
    count_pos_for_each_position(Players, Rest, Counters).


%Base Case
count_position(_, [], 0).

count_position(Pos, [[_, _, Pos] | Rest], Counter) :-
    count_position(Pos, Rest, C),
    Counter is C + 1.

count_position(Pos, [[_, _, AnotherPos] | Rest], Counter) :-
    Pos \= AnotherPos,
    count_position(Pos, Rest, Counter).




%Base Case
get_common_position([], BestPosition, _, BestPosition).

get_common_position([[Pos, Counter] | Rest], _, BestCounter, BestPosition) :-
    Counter > BestCounter,
    get_common_position(Rest, Pos, Counter, BestPosition).

get_common_position([[_, Counter] | Rest], CurrentBest, BestCounter, BestPosition) :-
    Counter =< BestCounter,
    get_common_position(Rest, CurrentBest, BestCounter, BestPosition).

















