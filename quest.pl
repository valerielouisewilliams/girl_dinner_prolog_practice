/**********************************************************************
    ✨ GIRL DINNER QUEST — A Prolog Adventure ✨  
    Author: Valerie Williams
    Purpose: Learn Prolog fundamentals using a silly, chaotic world  
***********************************************************************

   You live in the land of SNACKORIA.  
   A giant cheese cube oracle demands the PERFECT GIRL DINNER.
   If you fail to supply it... the world collapses into BOY DINNER  
   (raw protein powder + sadness).  

   Your quest:
   Implement all the missing Prolog rules below following the TODOs.
   This project intentionally practices:

       • Facts  
       • Rules  
       • Goals  
       • Unification  
       • Backtracking  
       • Recursion  
       • List Processing  
       • Arithmetic (is, =:=, =\=, =<, etc.)  
       • Conjunctions / disjunctions  
       • Using rule order to prevent chaos (and infinite loops)

***********************************************************************/


/**********************************************************************
*  SECTION 1 — FACTS: The World of Snackoria
*  These are complete. Do not modify unless extending the world.
***********************************************************************/

% ingredient(Name, Vibe, ChaosScore)
ingredient(strawberries,           aesthetic,     3).
ingredient(cheez_its,              feral,         8).
ingredient(popcorn,                cozy,          5).
ingredient(yogurt,                 clean_girl,    2).
ingredient(protein_bar,            gym_girl,      4).
ingredient(olives,                 european,      6).
ingredient(bread_with_nothing_on_it, depressive,  10).
ingredient(pickles,                feral,         7).
ingredient(goat_cheese,            aesthetic,     3).
ingredient(cold_pasta_from_last_night, divorce_core, 5).

% forbidden combinations
forbidden_combo(cheez_its, yogurt).
forbidden_combo(pickles, strawberries).
forbidden_combo(bread_with_nothing_on_it, joy).   % joy is imaginary; lore purposes

% creatures: creature(Name, Personality, Quirk)
creature(aika,         judgmental, loves_cheese).
creature(frog_king,    wise,       hates_feral_vibes).
creature(gossip_fairy, chaotic,    loves_gossip).
creature(chad_knight,  oblivious,  eats_protein_powder_plain).

% NOTES:
% 1. Base case: pred([], Base).

% 2. Recursive case:
%    pred([H|T], Result) :-
%        pred(T, R1),
%        Result is f(H, R1).

/**********************************************************************
*  SECTION 2 — LIST UTILITY PREDICATES
*  You must implement these using recursion + unification.
***********************************************************************/
% List to use to test functions
dinner_example([popcorn, yogurt, olives]).

%% my_member(Item, List) succeeds if Item is inside List.
%%
%% Base case: Item is the head
%% Recursive case: skip the head, search the tail
my_member(Item, [Item | _]). % Base case

my_member(Item, [_ | Tail]) :- % Recursive case
    my_member(Item, Tail).

%% my_length(List, N)
%% Counts elements in a list.
%% Base case: empty list has length 0
%% Recursive case: increment count
my_length([], 0). % list has 0 ([]) so ensure that N is 0 by unifying it
my_length([_ | Tail], N) :-
    my_length(Tail, N1),
    N is N1 + 1.

%% dinner_vibes(DinnerList, VibesList)
%% Transform a list of ingredient names → their vibe list.
%% Use ingredient(Name, Vibe, _) fact.
%%
%% Example:
%%      ?- dinner_vibes([popcorn, yogurt], V).
%%      V = [cozy, clean_girl].

dinner_vibes([], []). % base case if list is empty
dinner_vibes([Item | Tail], [Vibe | Vibes]) :-
    ingredient(Item, Vibe, _),
    dinner_vibes(Tail, Vibes).

%% chaos_total(DinnerList, Total)
%%
%% Add up all chaos scores.
%% Use ingredient(Item, _, ChaosScore).
%% Must use recursion + `is`.
chaos_total([], 0).
chaos_total([Item | Tail], N) :-
    ingredient(Item, _, C),
    chaos_total(Tail, N1),
    N is N1+ C.

/**********************************************************************
*  SECTION 3 — CORE PROJECT RULES
***********************************************************************/

%% TODO ❗ enough_vibes(Dinner)
%% True if the dinner contains AT LEAST 3 ingredients.
%% Use my_length/2.
%%
%% Hint: 
%%    my_length(Dinner, L), L >= 3.
enough_vibes(Dinner) :-
    my_length(Dinner,N),
    N >=3.

%% cursed(Dinner)
%% True if ANY forbidden combination appears in the dinner.
%% Must use your my_member/2, not builtin member/2.
%%
%% Plan:
%%    Pick A from dinner
%%    Pick B from dinner
%%    Check forbidden_combo(A, B)

cursed(Dinner) :- 
forbidden_combo(A,B),
my_member(A, Dinner),
my_member(B, Dinner).


%% TODO ❗ contains_feral(Dinner)
%% True if any ingredient in the dinner has vibe = feral.
%%
%% Use ingredient(Item, feral, _).
%% Use recursion to scan list.

% contains_feral(Dinner) :-
%     ...
contains_feral([Item | _]):-
    ingredient(Item, feral, _). % base case (first ingredient is feral)
contains_feral([_ | Tail]) :- % Recursive case (move on to the rest of the list)
    contains_feral(Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% APPROVAL RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Oracle rules:
%% The Oracle approves dinners that:
%%    1. Have enough vibes
%%    2. Are NOT cursed
%%    3. Have chaos_total <= 15

oracle_approves(Dinner) :-
    enough_vibes(Dinner), 
    \+ cursed(Dinner),
    chaos_total(Dinner, N),
    N =< 15. 

%% Frog King:
%% Approves only dinners with NO feral ingredients.

frog_king_approves(Dinner) :-
    \+ contains_feral(Dinner).

%% Aika:
%% Approves any dinner containing goat cheese

aika_approves(Dinner) :-
    my_member(goat_cheese, Dinner).


%% Gossip Fairy:
%% Approves if dinner includes ANY divorce_core ingredient

gossip_fairy_approves(Dinner) :-
    dinner_vibes(Dinner, V),
    my_member(divorce_core, V).


/**********************************************************************
* SECTION 4 — FINAL BOSS LOGIC
***********************************************************************/

%% save_snackoria(Dinner)
%%
%% Snackoria is saved if:
%%   - The Oracle approves  (FIRST RULE)
%%   - OR Aika AND Frog King approve together (SECOND RULE)
%%
%% NOTE: This teaches disjunction via multiple clauses.

save_snackoria(Dinner) :-
    oracle_approves(Dinner).

save_snackoria(Dinner) :-
    aika_approves(Dinner),
    frog_king_approves(Dinner).



/**********************************************************************
*  SECTION 5 — SAMPLE QUERIES YOU SHOULD RUN (NOT CODE)
*
*  Run these in your interpreter:
*
*   ?- chaos_total([strawberries, popcorn, goat_cheese], Score).
*   ?- cursed([cheez_its, yogurt]).
*   ?- oracle_approves([popcorn, strawberries, goat_cheese]).
*   ?- frog_king_approves([olives, strawberries]).
*   ?- save_snackoria([popcorn, yogurt, goat_cheese]).
*   ?- oracle_approves(Dinner).     % Prolog will GENERATE dinners
*
***********************************************************************/

% Extra Practice

% Count how many ingredients have vibe Feral
count_feral([], 0).

count_feral([_ | T], N):-
    count_feral(T, N2)
    N is N2 + 1

% Convert ingredients into chaos score list
chaos_list([], []).

chaos_list([H | T], [C | R]):- 
    ingredient(H, _, C), % extract the chaos score
    chaos_list(T, R). % recurse on the tails
