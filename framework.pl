%% Preliminary Declarations
:- op( 950, xfx, ==>  ).        % Define a special operator to make the rules more readable.
:- dynamic fact/1, new_fact/1.  % fact/1 predicate can be altered while program is running.

:- retractall( fact(_) ). % Clear previously stored facts.

:- discontiguous fact/1.
:- discontiguous rule/2.
:- style_check(-singleton).

% DOMAIN: ANIMALS
% Facts about animals
fact([rabbit, isa, prey]).              % Rabbit is classified as prey
fact([zebra, isa, prey]).               % Zebra is classified as prey
fact([deer, isa, prey]).                % Deer is classified as prey
fact([sparrow, isa, prey]).             % Sparrow is classified as prey
fact([snake, isa, prey]).               % Snake is classified as prey

fact([tiger, isa, predator]).           % Tiger is classified as a predator
fact([lion, isa, predator]).            % Lion is classified as a predator
fact([cheetah, isa, predator]).         % Cheetah is classified as a predator

fact([tiger, preys_on, rabbit]).        % Tiger preys on rabbit
fact([lion, preys_on, rabbit]).         % Lion preys on rabbit
fact([cheetah, preys_on, zebra]).       % Cheetah preys on zebra

fact([rabbit, lives_in, zoo1]).         % Rabbit lives in zoo1
fact([tiger, lives_in, zoo2]).          % Tiger lives in zoo2
fact([lion, lives_in, zoo2]).           % Lion lives in zoo2
fact([cheetah, lives_in, zoo2]).        % Cheetah lives in zoo2

fact([lion, aggression_level, 3]).
fact([tiger, aggression_level, 4]).
fact([cheetah, aggression_level, 5]).

fact([tiger, must_be_relocated_to, zoo1]). % Tiger must be relocated to zoo1
fact(-[zebra, isa, predator]).          % Zebra is not a predator
fact(-[rabbit, is_companion_of, tiger]).% Rabbit is not a companion of tiger
fact(-[cheetah, is_companion_of, zebra]). % Cheetah is not a companion of zebra
fact(-[zebra, preys_on, rabbit]).       % Zebra does not prey on rabbit
fact(-[lion, isa, prey]).               % Lion is not prey
fact(-[deer, preys_on, snake]).         % Deer does not prey on snake

% DOMAIN: HABITATS
% % Facts about habitat and zoos
fact([zoo1, isa, zoo]).                 % Zoo1 is a zoo
fact([zoo2, isa, zoo]).                 % Zoo2 is a zoo

fact([zoo1, houses, rabbit]).           % Zoo1 houses rabbit
fact([zoo1, houses, zebra]).            % Zoo1 houses zebra
fact([zoo2, houses, tiger]).            % Zoo2 houses tiger
fact([zoo2, houses, lion]).             % Zoo2 houses lion
fact([zoo2, houses, cheetah]).          % Zoo2 houses cheetah

fact([zoo1, cooperates_with, zoo2]).    % Zoo1 cooperates with zoo2
fact([zoo2, cooperates_with, zoo3]).    % Zoo2 cooperates with zoo3

fact(-[cheetah, lives_in, zoo1]).       % Cheetah does not live in zoo1
fact(-[rabbit, lives_in, zoo2]).        % Rabbit does not live in zoo2

% DOMAIN: PEOPLE
% Facts about people and roles
fact([bob, isa, zookeeper]).            % Bob is a zookeeper
fact([alice, isa, inspector]).          % Alice is an inspector
fact([alex, isa, inspector]).          % Alex is an inspector
fact([alex, isa, vip_inspector]).       % Alex is a VIP inspector
fact([vip_inspector, isa, inspector]).  % VIP inspector is a type of inspector
fact([anne, isa, vip]).                 % Alex is classified as a VIP

fact([bob, manages, zoo1]).             % Bob manages zoo1
fact([bob, indirectly_cares_for, rabbit]). % Bob indirectly cares for rabbit
fact([bob, must_care_for, rabbit]).     % Bob must care for rabbit

fact([alex, is_assigned_to, zoo1]).     % Alex is assigned to zoo1
fact([alice, is_assigned_to, zoo1]).    % Alice is assigned to zoo1

fact([rex, is_companion_of, alice]).    % Rex is a companion of Alice
fact([rex, is_companion_of, bob]).      % Rex is a companion of Bob
fact([champ, is_companion_of, bob]).    % Champ is a companion of Bob
fact([tiger, is_companion_of, zookeeper]). % Tiger is a companion of the zookeeper
fact([lion, is_companion_of, vip_visitor]). % Lion is a companion of a VIP visitor

fact(-[lion, is_companion_of, alice]).  % Lion is not a companion of Alice
fact(-[bob, isa, predator]).            % Bob is not a predator
fact(-[bob, indirectly_cares_for, tiger]). % Bob does not indirectly care for tiger

% DOMAIN: GOVERNANCE AND CONSERVATION
% Facts about governance and conservation
fact([national_government, isa, government]). % National government is a government entity
fact([government, isa, government]).         % Generic government entity
fact([national_wildlife_authority, isa, wildlife_authority]). % National wildlife authority is a wildlife authority
fact([wildlife_conservation_authority, isa, conservation_authority]). % Wildlife conservation authority is a conservation authority
fact([conservation_authority, isa, conservationauthority]). % General conservation authority

fact([channel1, isa, globalmedia]).         % Channel1 is a global media outlet

% Rules
% Depth 1
% A zookeeper issues an alert for a prey they indirectly care for.
rule(zookeeper_alert, [[Zookeeper, indirectly_cares_for, Prey]] ==> [[Zookeeper, issues_alert_for, Prey]]).
% A predator is a threat to a prey if it preys on it and the prey is identified as a prey.
rule(predator_alert, [[Predator, preys_on, Prey], [Prey, isa, prey]] ==> [[Predator, is_a_threat_to, Prey]]).
% A zookeeper must be alert for a predator if the predator preys on a prey the zookeeper indirectly cares for.
rule(extended_threat_chain, [[Predator, preys_on, Prey], [Prey, isa, prey], [Zookeeper, indirectly_cares_for, Prey]] ==> [[Zookeeper, must_be_alert_for, Predator]]).
% A zookeeper must alert about a predator if it preys on a prey the zookeeper indirectly cares for.
rule(prey_alert_chain, [[Zookeeper, indirectly_cares_for, Prey], [Predator, preys_on, Prey]] ==> [[Zookeeper, must_alert_about, Predator]]).
% A predator indirectly preys on a prey if it preys on another prey that is identified as prey.
rule(indirect_prey_relationship, [[Predator, preys_on, MidPrey], [MidPrey, isa, prey]] ==> [[Predator, indirectly_preys_on, MidPrey]]).
% A zoo declares an emergency for a predator if the predator preys on a prey housed in that zoo.
rule(chain_reaction_threat, [[Zoo, houses, Prey], [Predator, preys_on, Prey]] ==> [[Zoo, declares_emergency_for, Predator]]).
% Calculate and assign the number of companions for a given animal.
rule(assign_companion_count, 
    [[Animal, is_companion_of, _]] ==> 
    [[Animal, companion_count, Count]]) :-
    % Find unique companions for the given animal.
    setof(Companion, fact([Animal, is_companion_of, Companion]), Companions),
    length(Companions, Count).

% Predicate: Identifies the animal with the highest value for a given property.
% This predicate compares the values of the given property for all animals and asserts
% a new fact indicating which animal has the top value for that property.
top_animal_by_property(Property) :-
    fact([Animal, Property, Value]),  % Find a fact about an animal's property value.
    \+ (
        fact([Other, Property, Higher]),  % Check if any other animal has a higher value.
        Higher > Value,
        Other \= Animal
    ),
    write('Top animal with '), write(Property), write(' is: '), write(Animal), nl,
    assertz(fact([Animal, top_animal_with, Property])).  % Assert the new fact for the top animal.

% A zoo prepares a special enclosure for the most aggressive animal living in it.
% This rule uses the new fact created by `top_animal_by_property` to take appropriate actions.
rule(handle_top_aggressive_animal,
    [[Animal, top_animal_with, aggression_level], [Animal, lives_in, Zoo]] ==>
    [[Zoo, prepares_special_enclosure_for, Animal]]).
?- top_animal_by_property(aggression_level).

%NEGATION RULES
% If an animal is a prey and does not live in a zoo, it is considered at risk.
rule(at_risk_prey,
    [[Animal, isa, prey], -[Animal, lives_in, _]] ==>
    [[Animal, is_at_risk]]
).

% If a predator does not live in the same zoo as its prey, it is not a threat to that prey.
rule(no_threat,
    [
        [Predator, isa, predator],
        [Prey, isa, prey],
        [Prey, lives_in, Zoo],
        \+ fact([Predator, lives_in, Zoo])
    ] ==>
    [[Predator, is_not_a_threat_to, Prey]]
).

% If a zookeeper is not a predator, they can be assigned to care for prey.
rule(zookeeper_assignment,
    [[Person, isa, zookeeper], -[Person, isa, predator]] ==>
    [[Person, can_care_for, prey]]
).

% If a predator does not live in the same zoo as its prey, it must be relocated to that zoo.
rule(predator_relocation,
    [
        [Predator, isa, predator],
        [Prey, isa, prey],
        [Prey, lives_in, Zoo],
        \+ fact([Predator, lives_in, Zoo])
    ] ==>
    [[Predator, must_be_relocated_to, Zoo]]
).

% The zoo must allocate resources for animals under care.
rule(zoo_responsibility_for_risk,
    [
        [Zookeeper, must_care_for, Animal],
        [Animal, lives_in, Zoo]
    ] ==>
    [[Zoo, must_allocate_resources_for, Animal]]
).

% After a predator is relocated, prey safety is evaluated.
rule(prey_safety_check,
    [[Predator, must_be_relocated_to, Zoo], [Prey, isa, prey], [Prey, lives_in, Zoo]] ==>
    [[Zoo, evaluates_safety_for, Prey]]
).

% If a prey is not threatened by any predator, it is categorized as safe.
rule(prey_safety_categorization,
    [[Animal, isa, prey], -[Predator, preys_on, Animal]] ==>
    [[Animal, is_safe]]
).

% Depth 2
% Extends the alert chain: if a zookeeper must alert about a predator, they must also alert about its prey.
rule(alert_chain_extension, [[Zookeeper, must_alert_about, Predator], [Predator, preys_on, Prey]] ==> [[Zookeeper, must_alert_about, Prey]]).

% Zoo declares an alert for a prey if a zookeeper issues an alert and the prey lives in the zoo.
rule(zookeeper_zoo_alert, 
    [[Zookeeper, issues_alert_for, Prey], [Prey, isa, prey], [Prey, lives_in, Zoo]] 
    ==> 
    [[Zoo, declares_alert_for, Prey]]).

% Zookeeper must report to a VIP if they must alert about a predator housed in the zoo.
rule(zookeeper_hierarchy_escalation, [[Zookeeper, must_alert_about, Predator], [Predator, isa, predator], [Zoo, houses, Predator]] ==> [[Zookeeper, must_report_to_vip, Zoo]]).

% Zoo implements lockdown if it declares an emergency for a predator.
rule(emergency_security_measures, [[Zoo, declares_emergency_for, Predator], [Predator, isa, predator]] ==> [[Zoo, implements_lockdown, Predator]]).

% Zoo enhances security for an animal if it prepares a special enclosure for it.
rule(enclosure_security,
    [[Zoo, prepares_special_enclosure_for, Animal], [Zoo, isa, zoo]] ==>
    [[Zoo, enhances_security_for, Animal]]).

% A predator not a threat to prey does not require immediate relocation.
rule(no_relocation_needed,
    [[Predator, is_not_a_threat_to, Prey], [Predator, isa, predator]] ==>
    [[Predator, requires_no_immediate_action]]
).

%NEGATION RULES
% If a prey is at risk and not in a zoo, a conservation plan must be initiated.
rule(conservation_plan,
    [[Animal, is_at_risk], -[Animal, lives_in, Zoo]] ==>
    [[wildlife_conservation_authority, initiates_plan_for, Animal]]
).

% A zookeeper must assign additional monitoring to prey at risk.
rule(prey_monitoring,
    [[Zookeeper, must_care_for, Animal], [Animal, is_at_risk]] ==>
    [[Zookeeper, must_assign_monitoring_to, Animal]]
).

% Safe prey does not require zookeeper monitoring.
rule(no_monitoring_needed,
    [[Animal, is_safe], [Zookeeper, must_care_for, Animal]] ==>
    [[Zookeeper, stops_monitoring, Animal]]
).

% If prey is evaluated as safe, its monitoring is gradually reduced.
rule(gradual_monitoring_reduction,
    [[Zoo, evaluates_safety_for, Prey], [Prey, is_safe]] ==>
    [[Zoo, begins_gradual_monitoring_reduction_for, Prey]]
).

% If prey is no longer at risk, a zoo updates its policies.
rule(update_policies_post_safety,
    [[Zoo, evaluates_safety_for, Prey], [Prey, is_safe]] ==>
    [[Zoo, updates_policies_based_on_safety, Prey]]
).

% Policy evaluations lead to wildlife management reforms.
rule(wildlife_management_reform,
    [[Government, evaluates_policy_for, Predator]] ==>
    [[Government, initiates_wildlife_management_reform]]
).

% Depth 3
% Zoo secures against a predator if it declares an alert for a prey targeted by that predator.
rule(zoo_predator_action, [[Zoo, declares_alert_for, Prey], [Predator, preys_on, Prey]] ==> [[Zoo, secures_against, Predator]]).

% A VIP must inspect security at a zoo if the zoo enhances security for an animal and is classified as a zoo.
rule(vip_notification,
    [[Zoo, enhances_security_for, Animal], [Zoo, isa, zoo], [Vip, isa, vip]] ==>
    [[Vip, must_inspect_security_at, Zoo]]).

% Zoo reviews its security policy after enhancing security for a prey.
rule(policy_update_due_to_security,
    [[Zoo, enhances_security_for, Prey]] ==>
    [[Zoo, reviews_security_policy]]
).

% If no immediate action is required, allocate resources elsewhere.
rule(redirect_resources,
    [[Predator, requires_no_immediate_action], [Zoo, houses, Predator]] ==>
    [[Zoo, reallocates_resources_from, Predator]]
).

% Prey under conservation plans require funding.
rule(funding_request,
    [[ConservationAuthority, initiates_plan_for, Animal]] ==>
    [[ConservationAuthority, requests_funding_for, Animal]]
).

% If monitoring is assigned, special reporting is required.
rule(special_reporting,
    [[Zookeeper, must_assign_monitoring_to, Animal]] ==>
    [[Zookeeper, submits_special_report_for, Animal]]
).

% Reduced monitoring requires a final zookeeper assessment.
rule(final_zookeeper_assessment,
    [[Zoo, begins_gradual_monitoring_reduction_for, Prey], [Zookeeper, isa, zookeeper], [Zookeeper, manages, Zoo]] ==>
    [[Zookeeper, conducts_final_assessment_for, Prey]]
).

% Funding requests initiate government reviews.
rule(government_review_for_funding,
    [[NewZoo, requests_additional_funding_for, Predator], [Government, isa, government]] ==>
    [[Government, reviews_funding_request_for, NewZoo]]
).

% Conservation plans involve creating protective environments.
rule(create_protective_environment,
    [[ConservationAuthority, initiates_plan_for, Prey]] ==>
    [[ConservationAuthority, designs_protective_environment_for, Prey]]
).

% Policy updates trigger public awareness campaigns.
rule(public_awareness_campaign,
    [[Zoo, updates_policies_based_on_safety, Prey]] ==>
    [[Zoo, launches_public_awareness_campaign]]
).

% Reforms influence funding allocations.
rule(funding_reallocations_due_to_reform,
    [[Government, initiates_wildlife_management_reform]] ==>
    [[Government, reallocates_funding_to_conservation]]
).

% Depth 4
% A VIP assigned to a zoo must inspect it if the zoo secures against a predator.
rule(vip_escalation, 
    [[Zoo, secures_against, Predator], [Predator, isa, predator], [Vip, is_assigned_to, Zoo]] 
    ==> 
    [[Vip, must_inspect, Zoo]]).

% A VIP inspector must inspect a zoo if it secures against a predator.
rule(vip_involvement_escalation, 
    [[Zoo, secures_against, Predator], [Predator, isa, predator], [Vip, isa, inspector]] 
    ==> 
    [[Vip, must_inspect_zoo, Zoo]]).

% A zoo deploys a specialist for security if a VIP must inspect its security.
rule(deploy_specialist,
    [[Vip, must_inspect_security_at, Zoo], [Zoo, isa, zoo]] ==>
    [[Zoo, deploys_specialist_for_security]]).

% Designing protective environments requires expert consultation.
rule(expert_consultation,
    [[ConservationAuthority, designs_protective_environment_for, Prey]] ==>
    [[ConservationAuthority, consults_experts_for, Prey]]
).

% Awareness campaigns require media partnerships.
rule(media_partnership,
    [[Zoo, launches_public_awareness_campaign]] ==>
    [[Zoo, partners_with_media_outlets]]
).

%Depth 5
% A VIP orders containment measures for a predator if they must inspect the zoo and the zoo declares an emergency for the predator.
rule(expanded_vip_actions, [[Vip, must_inspect_zoo, Zoo], [Zoo, declares_emergency_for, Predator]] ==> [[Vip, orders_containment_measures, Predator]]).

% A zoo activates its emergency plan when it deploys a specialist for security.
rule(emergency_plan_activation,
    [[Zoo, deploys_specialist_for_security], [Zoo, isa, zoo]] ==>
    [[Zoo, activates_emergency_plan]]).


% Expert consultations result in habitat recommendations.
rule(habitat_recommendations,
    [[ConservationAuthority, consults_experts_for, Prey]] ==>
    [[ConservationAuthority, makes_habitat_recommendations_for, Prey]]
).

% Depth 6
% A specialist is deployed for a predator when a VIP orders containment measures, and the predator lives in the zoo.
rule(specialist_deployment, 
    [[Vip, orders_containment_measures, Predator], [Predator, isa, predator], [Predator, lives_in, Zoo]] 
    ==> 
    [[Zoo, deploys_specialist_for, Predator]]).

% A zoo informs the public about an emergency when it activates its emergency plan.
rule(public_notification,
    [[Zoo, activates_emergency_plan], [Zoo, isa, zoo]] ==>
    [[Zoo, informs_public_about_emergency]]).

% Depth 7
% The government receives an emergency alert when a zoo informs the public about an emergency and is classified as a zoo.
rule(government_alert,
    [[Zoo, informs_public_about_emergency], [Zoo, isa, zoo], [Government, isa, government]] ==>
    [[Government, receives_emergency_alert, Zoo]]).

% A zoo initiates a citizen support campaign after informing the public about an emergency.
rule(citizen_support_campaign,
    [[Zoo, informs_public_about_emergency]] ==>
    [[Zoo, initiates_citizen_support_campaign]]
).

% A zoo starts training programs for animals after deploying a specialist for them.
rule(training_programs_post_specialist_deployment,
    [[Zoo, deploys_specialist_for, Animal]] ==>
    [[Zoo, starts_training_program_for, Animal]]
).


% Depth 8
% The wildlife authority intervenes in a zoo when the government receives an emergency alert for that zoo and the authority is classified as a wildlife authority.
rule(wildlife_authority_involvement,
    [[Government, receives_emergency_alert, Zoo], [WildlifeAuthority, isa, wildlife_authority]] ==>
    [[WildlifeAuthority, intervenes_in, Zoo]]).

% Government emergency alerts lead to wildlife conservation funding.
rule(government_funding_allocation,
    [[Government, receives_emergency_alert, Zoo]] ==>
    [[Government, allocates_funding_for, Zoo]]
).

% Depth 9
% Combined Rule: Trigger fundraising and policy review in one step
rule(handle_emergency_broadcast_phase1, [[WildlifeAuthority, intervenes_in, Zoo], [GlobalMedia, isa, globalmedia]] ==> [[GlobalMedia, broadcasts_emergency_at, Zoo]]).
rule(handle_emergency_broadcast_phase2, [[WildlifeAuthority, intervenes_in, Zoo], [GlobalMedia, isa, globalmedia]] ==> [[Zoo, starts_fundraising_campaign]]).
rule(handle_emergency_broadcast_phase3, [[WildlifeAuthority, intervenes_in, Zoo], [GlobalMedia, isa, globalmedia]] ==> [[Zoo, initiates_policy_review]]).

% Emergency funding leads to new conservation projects.
rule(new_conservation_projects,
    [[Government, allocates_funding_for, Zoo]] ==>
    [[Zoo, starts_conservation_project]]
).

% Wildlife authority interventions lead to stricter regulations.
rule(stricter_regulations,
    [[WildlifeAuthority, intervenes_in, Zoo]] ==>
    [[WildlifeAuthority, enforces_stricter_regulations_for, Zoo]]
).

%Depth 10
% Stricter regulations lead to changes in zoo management policies.
rule(management_policy_changes,
    [[WildlifeAuthority, enforces_stricter_regulations_for, Zoo]] ==>
    [[Zoo, updates_management_policies]]
).

%-------------------
% Combined rules
% Depth 1 & 2
% When a predator is relocated, the new zoo upgrades its security.
rule(security_upgrade_post_relocation,
    [[Predator, must_be_relocated_to, NewZoo]] ==>
    [[NewZoo, upgrades_security_for, Predator]]
).

% The government evaluates policies for relocated predators.
rule(government_policy_evaluation,
    [[Predator, must_be_relocated_to, Zoo], [Government, isa, government]] ==>
    [[Government, evaluates_policy_for, Predator]]
).

% Depth 2 & 3
% Security upgrades require additional funding.
rule(funding_for_security_upgrade,
    [[NewZoo, upgrades_security_for, Predator]] ==>
    [[NewZoo, requests_additional_funding_for, Predator]]
).

%% SPECIFY INFERENCE RULES

ruleset([_]). % This will allow any rule to work. Comment our this line if using specific set.
% Restricting to only use rules with certain labels can be useful for debugging.
% ruleset([logic,taxonomy]). % Example rule restriction to rules with logic or taxonomy label.

%% The form of a rule specification
%% rule( label, [ condition1, ... ] ==> [ conclusion1, ...] ).  
%% The meaning is that if facts matching the conditions are found, then facts matching the
%% conclusions can be added.

%% General logical and set-theoretic rules
:- discontiguous rule/2.

rule( logic1, [[C1, is_subclass_of, C2], [C2, is_subclass_of, C3]]  ==>  [[C1, is_subclass_of, C3]] ).
rule( logic2, [[X, isa, C1], [C1, is_subclass_of, C2]]  ==>  [[X, isa, C2]] ).

%% Taxonomic relationships regarding the concept vocabulary
rule( taxonomy, [[X, is_parent_of, Y], [X, is, male]]   ==> [[X, is_father_of, Y]] ).
rule( taxonomy, [[X, is_parent_of, Y], [X, is, female]] ==> [[X, is_mother_of, Y]] ).
rule( taxonomy, [[X, isa, dog], [X, is_age, A], test(A < 2) ]   ==> [[X, isa, puppy]] ).

rule( taxonomy, [[X, isa, dog]] ==> [-[X,isa,cat]] ).
rule( taxonomy, [[X, isa, cat]] ==> [-[X,isa,dog]] ).

rule( inverse_relation, [[X, is_parent_of, Y]] ==> [[Y, is_child_of, X]] ).


%%1
rule( happiness, [[X, isa, dog], [X, is, happy]] ==> [[X, wags, tail]] ).
%%2
rule( happiness, [[X, isa, puppy]]  ==> [[X, is, happy]] ).

rule( cuteness, [[X, isa, puppy], [X, wags, tail]] ==> [[X, is, cute]] ).

rule( genetics, [[X, isa, dog], [X, is_parent_of, Y]] ==> [[Y,isa,dog]] ).
rule( genetics, [[X, isa, dog], [X, is_child_of, Y]] ==>  [[Y,isa,dog]] ).

%% Default rules make inferences on condition that something is not provable.
%% Such rules should go after the positive inference rules.

rule( default,  [[X, isa, bird], \+(-[X, flies]) ]  ==>  [[X, flies]] ).

rule( exception, [[X, isa, penguin]] ==> [-[X, flies]] ).

%% Define how to interpret 'test' conditions in rule preconditions:
test( X < Y ) :- X < Y.
test( X =< Y ) :- X =< Y.
%% You could add extra test conditions.

%% SUGGESTIONS FOR NEW RULES TO ADD
%% a) Add a rule which enforces the condition: "Cats hate all dogs except puppies".
rule( hatred, [[X, isa, cat], [Y, isa, dog], \+([Y, isa, puppy])] ==> [[X, hates, Y]] ).

%% b) Define a compositional inference rule which would enable deduction of
%%    the implied fact: [grandfather,rex,champ]
rule( grandfather, [[X, is_parent_of, Y], [Y, is_parent_of, Z], [X, is, male]] ==> [[X, is_grandfather_of, Z]] ).

%% c) Uncomment the following "genesis" rule and explain what happens when you
%%    then run the inference mechanism.
%%rule( genesis, [[X, isa, dog]] ==> [[fatherof(X), isa, dog]] ).

%% d) Using the weak negation operator \+ checks whether a 
%% fact or condition is not provable in the knowledge base.
%%rule(example1, [[X, isa, bird], \+([X, isa, penguin])] ==> [[X, can_fly]]).
rule(fly1, [[X, isa, bird], \+([X, isa, penguin])] ==> [[X, can_fly]]).
rule(fly2, [[X, isa, penguin]] ==> [[X, cannot_fly]]).


%% THE INFERENCE MECHANISM

%% applyrule: check if premisses are satisfiable. If so assert the conclusion.

applyrule( T, [] ==> Conc ) :- % If no conditions, assert conclusion (unless already known).
    add_conclusions( T, Conc ).

applyrule( T, [-(P) | Rest] ==> Conc ) :-   % Check strong negated premiss is a negative fact
    fact( -(P) ),                           % Check that the negative fact is known
    applyrule( T, Rest ==> Conc ).

applyrule( T,  [\+(P) | Rest] ==> Conc ) :-   % Check weak negated premiss is not a fact
    \+(fact( P )),                            % Check the fact has not been asserted.
    applyrule( T, Rest ==> Conc ).

applyrule( T, [test(Condition) | Rest] ==> Conc ) :-  % Evaluate a test condition.
     ground(Condition),                    % A test condition may not contain variables
     test( Condition ),                    % Evaluate the condition with (user defined) test
     applyrule( T, Rest ==> Conc ).

applyrule( T,[Prem | Rest] ==> Conc ) :-  % Look for fact matching first premiss
     fact( Prem ),                        % Is it a fact (either in original set or derived)
     applyrule( T, Rest ==> Conc ).       % See if the other conditions can be satisfied.

%% This will add the conclusion if it is new and may print the new fact.
add_conclusion(T, Conc ) :-
    \+( fact( Conc ) ; new_fact( Conc ) ), % Check fact not already known or derived
    assert( new_fact( Conc ) ),
    (show_inferences -> show_inference(Conc, T) ; true).

add_conclusions(_, []).
add_conclusions(T, [C1 | Rest] ) :-
    add_conclusion(T, C1),
    add_conclusions(T, Rest).

check_consistency :- fact(F), fact(-F), !,
      write( '!! WARNING: inconsistent facts:'), nl,
      write(F), nl, write(-F), nl.
check_consistency. % 

%% infer applies all rules to all currently stored facts.
infer :- ruleset(Rules),
         check_consistency,
         findall(R, ( rule(Type, R), member(Type, Rules), applyrule(Type, R)), Infs ),
         length( Infs, Len ),
         write('* Number of inferences carried out: '), write( Len ), nl, nl,
         Len > 0,  % fail if no inferences found.
         %% Assert new facts to the main set of facts:
		 findall( _, (new_fact(F), assert(fact(F))), _),
		 retractall(new_fact(_)).

%% infer/1 repeatedly calls infer up to a given inference depth limit.
infer( Limit ) :- infer( 1, Limit ).
infer( Depth, Limit ) :- Depth>Limit, !, write( '* Max inference depth reached' ), nl, nl.
infer( Depth, Limit ) :- write( '* Inference depth: ' ), write( Depth ), 
                              write( ' ... ' ), nl,
                              infer, !, 
                              Next is Depth + 1, infer( Next, Limit ).
infer( _, _ ) :- write( '* No more inferences found' ), nl, nl.          


%% Useful Display Predicates
%% Show all facts 
allfacts :-  write('=== ALL KNOWN FACTS ==='), nl, (fact(F), write(F), nl, fail) ; true.
%% Show all facts involving terms in list L
describe(L) :- L=[] ; (L = [H|T], describe(H), describe(T)).
describe(X) :- write('=== Facts involving: '), write( X ), write(' ==='), nl, 
              ( (fact(F), (member(X,F);(F= -(G), member(X,G))), write(F), nl, fail) ; true ).

show_inference( Conc, Type ) :-
    format( 'Infer: ~p ~40+  (~p)~n', [Conc, Type] ).

show_inferences :- true. %% Change this to false to hide inference output.

:- style_check(-singleton).
?- infer(15). % Increase depth to ensure all possible inferences are made.
%?- allfacts. % Check the entire knowledge base.
