% Código visto em https://stackoverflow.com/questions/41025065/prolog-strips-planner-never-completes
% Based on
%
% Exercise 17.5 on page 429 of "Prolog Programming for Artificial Intelligence"
% by Ivan Bratko, 3rd edition
%
% The text says:
%
% "This planner searches through the state space in iterative-deepening style."
%
% See also:
%
% https://en.wikipedia.org/wiki/Iterative_deepening_depth-first_search
% https://en.wikipedia.org/wiki/Blocks_world
%
% The "iterative deepening" trick is all in the "Plan" list structure.
% If you remove it, the search becomes depth-first and no longer terminates!


% ----------
% Encapsulator to be called by user from the toplevel
% ----------

run :- 
    % Setting up
    start_state(State),
    final_state(Goals),
    % TODO: Build predicates that verify that State and Goal are actually validly constructed
    % Or choose better representations
    nb_setval(glob_plancalls,0), % global variable for counting calls (non-backtrackable)
    b_setval(glob_depth,0), % global variable for counting depth (backtrable)
    % plan/3 is backtrackable and generates different/successively longer plans on backtrack
    % it may however generate the same plan several times
    plan(State, Goals, Plan), 
    dump_plan(Plan,1).
 
 % ----------
 % Writing out a solution found
 % ----------
 
 dump_plan([P|R],N) :-
    % TODO: Verify that the plan indeed works!
    format('Plan step ~w: ~w~n',[N,P]),
    NN is N+1,
    dump_plan(R,NN).
 
 dump_plan([],_).
 
 % The representation of the blocks world (see below) is a bit unfortunate as places and blocks
 % have to be declared separately and relationships between places and blocks, as well
 % as among blocks themselves have to declared explicitely and consistently. 
 % Additionally we have to specify which elements have a view of the sky (i.e. are clear/1)
 % On the other hand, the final state and end state need not be specified fully, which is
 % interesting (not sure what that means exactly regarding solution finding)
 % The atoms used in describing places and blocks must be distinct due to program construction!
 
 start_state([on(a,1), on(b,2), on(c,3), clear(a), clear(b), clear(c), clear(4)]).
 final_state([on(a,1), on(b,2), on(c,a), clear(c), clear(2), clear(3), clear(4)]).

%  start_state([on(b,1), on(d,2), clear(b), clear(d), clear(3), clear(4)]).
%  final_state([on(d,2), on(b,d), clear(b), clear(1), clear(3), clear(4)]).

% INICIAL    FINAL
% d(^)        d(^)
% c           c
% b           b
% a           a
% 1 2 3 4 | 1 2 3 4

 % ----------
 % Representation of the blocks world
 % ----------
 
 % We have BLOCKs identified by atoms a,b,c, ...
 % Each of those is identified by block/1 attribute.
 % A block/1 is clear/1 if there is nothing on top of it.
 % A block/1 is on(Shape, Object) where Object is a block/1 or place/1.
 
 block(a).
%  block(b).
%  block(c).
 triangle(b).
 ball(c).
 
 % We have PLACEs (i.e. columns of blocks) onto which to stack blocks.
 % Each of these is identified by place/1 attribute.
 % A place/1 can be clear/1 if there is nothing on top of it.
 % (In fact these are like special immutable blocks and should be modeled as such)
 
 place(1).
 place(2).
 place(3).
 place(4).
 
 % OBJECTs are place/1 or block/1.
 
 shape(X) :- block(X) ; triangle(X) ; ball(X).
 object(X) :- place(X) ; shape(X).

 % Checks if a shape can be stacked
 stackable(Shape, To) :- 
  shape(Shape), 
  (place(To) ; block(To)). % Shapes can only be stacked or blocks

 % Balls can only be stacked in place
 stackable(Shape, To) :-
  ball(Shape),
  place(To).
 
 % ACTIONs are terms "move( Shape, From, To)".
 % "Shape" must be block/1.
 % "From" must be object/1 (i.e. block/1 or place/1).
 % "To" must be object/1 (i.e. block/1 or place/1).
 % Evidently constraints exist for a move/3 to be possible from or to any given state.
 
 % STATEs are sets (implemented by lists) of "goal" terms.
 % A goal term is "on( X, Y)" or "clear(Y)" where Y is object/1 and X is block/1.
 
 % ----------
 % plan( +State, +Goals, -Plan)
 % Build a "Plan" get from "State" to "Goals".
 % "State" and "Goals" are sets (implemented as lists) of goal terms.
 % "Plan" is a list of action terms.
 % The implementation works "backwards" from the "Goals" goal term list towards the "State" goal term list.
 % ----------
 
 % ___ Satisfaction branch ____ 
 
 % This can only succeed if we are at the "end" of a Plan (the Plan must match '[]') and State matches Goal.
 
 plan( State, Goals, []) :-
 
   % Debugging output
   nb_getval(glob_plancalls,P), 
   b_getval(glob_depth,D), 
   NP is P+1, 
   ND is D+1, 
   nb_setval(glob_plancalls,NP), 
   b_setval(glob_depth,ND),
   statistics(stack,STACK),
   format('plan/3 call ~w at depth ~d (stack ~d)~n',[NP,ND,STACK]),
 
   % If the Goals statisfy State, print and succeed, otherwise print and fail
   ( satisfied( State, Goals) -> 
      (sort(Goals,Goals_s),
       sort(State,State_s),
       format('   Goals: ~w~n', [Goals_s]),
       format('   State: ~w~n', [State_s]),
       format('   *** SATISFIED ***~n'))
      ;
       format('   --- NOT SATISFIED ---~n'),
       fail).
 
 % ____ Search branch ____
 %
 % Magic which generates the breath-first iterative deepening search:
 %
 % In the top node of the call tree (the node directly underneath "run"), "Plan" is unbound.
 %
 %    At point "XXX" "Plan" is set to a list of as-yet-unbound actions of a given length.
 %    At each backtrack that reaches up to "XXX", "Plan" is bound to list longer by 1.
 %
 % In any other node of the call tree than the top node, "Plan" is bound to a list of fixed length
 % becoming shorter by 1 on each recursive call.
 %
 % The length of that list determines how deep the search through the state space *must* go because 
 % satisfaction can only be happen if the "Plan" list is equal to [] and State matches Goal.
 %
 % So: 
 % On first activation of the top, build plans of length 0 (only possible if Goals passes satisfied/2 directly)
 % On second activation of the top, build plans of length 1 (and backtrack over all possibilities of length 1)
 % ...
 % On Nth activation of the top, build plans of length N-1 (and backtrack over all possibilities of length N-1)
 %
 % A slight improvement is to fail the search branch immediately if Plan is a nonvar and is equal to []
 % because append( PrePlan, [Action], Plan) will fail...
 
 plan( State, Goals, Plan)  :-
 
   % The line below can be commented out w/o ill effects, it is just there to fail early
   ((nonvar(Plan), Plan == []) -> fail ; true ),
 
   % Debugging output
   nb_getval(glob_plancalls,P), 
   b_getval(glob_depth,D), 
   NP is P+1, 
   ND is D+1, 
   nb_setval(glob_plancalls,NP), 
   b_setval(glob_depth,ND),
   statistics(stack,STACK),
   format('plan/3 call ~w at depth ~d (stack ~d)~n',[NP,ND,STACK]),
   format('       goals ~w~n',[Goals]),
 
   % Even more debugging output
   ( var(Plan) -> format('  Top node of plan/3 call~n') ; true ), 
   ( nonvar(Plan) -> (length(Plan,LP), format('  Low node of plan/3 call, plan length to complete: ~w~n',[LP])) ; true ),
 
   % prevent runaway behaviour
   % assertion(NP < 1000000),
 
   % XXX
   % append/3 is backtrackable.
   % For the top node, it will generate longer completely uninstantiated PrePlans on backtracking:
   % PrePlan = [], Plan = [Action] ;
   % PrePlan = [_G981], Plan = [_G981, Action] ;
   % PrePlan = [_G981, _G987], Plan = [_G981, _G987, Action] ;
   % PrePlan = [_G981, _G987, _G993], Plan = [_G981, _G987, _G993, Action] ;
   % For lower nodes, Plan is instantiated to a list of length N already, and PrePlan will therefore necessarily
   % be the prefix list of length N-1
   % XXX
 
   append( PrePlan, [Action], Plan),
 
   % Backtrackably select some concrete Goal from Goals
   select_goal( Goals, Goal), % FIX: In the original this seems to depend on State, but it really doesn't
     assert_goal(Goal),
     format( '    Depth ~d, selected Goal: ~w~n',[ND,Goal]),
   % Check whether Action achieves the Goal. 
   % As Action is free, what we actually do is instantiate Action backtrackably with something that achieves Goal
   achieves( Action, Goal),
     format( '    Depth ~d, selected Action: ~w~n', [ND,Action]),
   % Fully instantiate Action backtrackably
   % FIX: Passed "conditions", the precondition for a move, which is unused at this point: broken up into two calls
   instantiate_action( Action),
     format( '    Depth ~d, action instantiated to: ~w~n', [ND,Action]),
     assertion(ground(Action)),
   % Check that the Action does not clobber any of the Goals
   preserves( Action, Goals),
   % We now have a ground Action that "achieves" some goals in Goals while "preserving" all of them
   % Work backwards from Goals to a "prior goals". regress/3 may fail to build a consistent GoalsPrior!
   regress( Goals, Action, GoalsPrior),
   plan( State, GoalsPrior, PrePlan).
 
 % ----------
 % Check
 % ----------
 
 assert_goal(X) :-
    assertion(ground(X)),
    assertion((X = on(A,B), shape(A), object(B) ; X = clear(C), object(C))).
 
 % ----------
 % A State (a list) is satisfied by Goals (a list) if all the terms in Goals can also be found in State
 % ----------
 
 satisfied( State, Goals)  :-
   subtract( Goals, State, []). % Set difference yields empty list: [] = Goals - State
 
 % ----------
 % Backtrackably select a single Goal term from a set of Goals
 % ----------
 
 select_goal( Goals, Goal)  :-
   member( Goal, Goals).
 
 % ----------
 % When does an Action (move/2) achieve a Goal (clear/1, on/2)?
 % This is called with instantiated Goal and free Action, so this actually instantiates Action
 % with something (partially specified) that achieves Goal.
 % ----------
 
 achieves( Action, Goal) :-
   assertion(var(Action)),
   assertion(ground(Goal)),
   would_add( Action, GoalsAdded),
   member( Goal, GoalsAdded).
 
 % ----------
 % Given a ground Action and ground Goals, will Action from a State leading to Goals preserve Goals?
 % ----------
 
 preserves( Action, Goals)  :-
   assertion(ground(Action)),
   assertion(ground(Goals)),
   would_del( Action, GoalsDeleted),
   intersection( Goals, GoalsDeleted, []). % "would delete none of the Goals"
 
 % ----------
 % Given existing Goals and an (instantiated) Action, compute the previous Goals
 % that, when Action is applied, yield Goals. This may actually fail if no
 % consistent GoalsPrior can be built!
 % ** It is actually not at all self-evident that this is right and that we get a valid
 %    "GoalsPrior" via this method! ** (prove it!)
 % FIX: "Condition" replaced by "Preconditions" which is what this is about.
 % ----------
 
 regress( Goals, Action, GoalsPrior) :-
   assertion(ground(Action)),
   assertion(ground(Goals)),
   would_add( Action, GoalsAdded),
   subtract( Goals, GoalsAdded, GoalsPriorPass), % from the "lists" library
   preconditions( Action, Preconditions),
   % All the Preconds must be fulfilled in Goals2, so try adding them
   % Adding them may not succeed if inconsistencies appear in the resulting set of goals, in which case we fail
   add_preconditions( Preconditions, GoalsPriorPass, GoalsPrior).
 
 % ----------
 % Adding preconditions to existing set of goals and checking for inconsistencies as we go
 % Previously named addnew/3
 % New we use union/3 from the "lists" library and the modified "consistent"
 % ----------
 
 add_preconditions( Preconditions, GoalsPriorIn, GoalsPriorOut) :-
   add_preconditions_recur( Preconditions, GoalsPriorIn, GoalsPriorIn, GoalsPriorOut).
 
 add_preconditions_recur( [], _, GoalsPrior, GoalsPrior).
 
 add_preconditions_recur( [G|R], Goals, GoalsPriorAcc, GoalsPriorOut) :-
   consistent( G, Goals),
   union( [G], GoalsPriorAcc, GoalsPriorAccNext),
   add_preconditions_recur( R, Goals, GoalsPriorAccNext, GoalsPriorOut).
 
 % ----------
 % Check whether a given Goal is consistent with the set of Goals to which it will be added
 % Previously named "impossible/2".
 % Now named "consistent/2" and we use negation as failure
 % ----------
 
 consistent( on(X,Y), Goals ) :-
   \+ on(X,Y) = on(A,A),            % this cannot ever happen, actually
   \+ member( clear(Y), Goals ),    % if X is on Y then Y cannot be clear
   \+ ( member( on(X,Y1), Goals ), Y1 \== Y ), % Shape cannot be in two places
   \+ ( member( on(X1,Y), Goals),  X1 \== X ). % Two blocks cannot be in same place
 
 consistent( clear(X), Goals ) :-
   \+ member( on(_,X), Goals).      % if something is on X, X cannot be clear
 
 % ----------
 % Backtrackably instantiate a partially instantiated Action
 % Previously named "can/2" and it also instantiated the "Condition", creating confusion
 % ----------
 
 instantiate_action(Action) :-
   assertion(Action = move( Shape, From, To)),
   Action = move( Shape, From, To),
  %  shape(Shape),
   stackable(Shape, To), % unify "To" with a concrete object (shape or place) and Shape with shape
   % object(To),   % will unify "To" with a concrete object (shape or place)
   To \== Shape, % equivalent to \+ == (but = would do here); this demands that blocks and places have disjoint sets of atoms
   object(From), % will unify "From" with a concrete object (shape or place)
   From \== To,
   Shape \== From.
 
 % ----------
 % Find preconditions (a list of Goals) of a fully instantiated Action
 % ----------
 
 preconditions(Action, Preconditions) :-
   assertion(ground(Action)),
   Action = move( Shape, From, To),
   Preconditions = [clear(Shape), clear(To), on(Shape, From)].
 
 % ----------
 % would_del( Move, DelGoals )
 % would_add( Move, AddGoals )
 % If we run Move (assuming it is possible), what goals do we have to add/remove from an existing Goals
 % ----------
 
 would_del( move( Shape, From, To), [on(Shape,From), clear(To)] ).
 would_add( move( Shape, From, To), [on(Shape,To), clear(From)] ).