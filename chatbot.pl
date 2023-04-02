:-dynamic refer_medical/1, additional_q_db/2, additional_a_db/2, additional_answers/2, has_symptom/2, patient_symptoms/1, asked_symptom/1.

chat:-
    welcome,
    converse.

welcome:-
    format('Hello there! I am MedBot. I\'m here to help you with any health concerns you may have.'), nl.

converse:-
    ask(Patient),
    ask_age(Patient, Age),
    ask_sex(Patient, Sex),
    ask_weight(Patient, Weight),
    ask_height(Patient, Height),
    compute_BMI(Weight, Height, BMI),
    ask_symptoms,
    diagnosis(Patient, Age, BMI, Sex).

diagnosis(Patient, Age, BMI, Sex) :-
    (   diagnose(Patient, Disease)
    ->  (   (   additional_q_db(Disease, Questions),
            additional_a_db(Disease, AnswerLabels),
            length(Questions, NumQuests),
             NumQuests > 0 
           -> additional_q(Patient, Questions, AnswerLabels)
        ;    true),

        probability(Patient, Disease),
        (higher_likelihood(Disease, Patient, Age, BMI, Sex) ->
            format('Based on the risk factors (age, BMI, Sex) of ~w, it appears that they are at a higher likelihood of currently having this condition.~n', [Patient])
         ; true),
            higher_likelihood(Patient, Disease, Questions, AnswerLabels),
            refer_medical(Disease)
         
        ;    true)

    ;  format('Unfortunately, we cannot diagnose ~w with any disease right now based on your symptoms. ~n However, ~w can always go to the local clinic and ask for a check-up.~n', [Patient, Patient])
    ).

additional_q(Patient, [Q|ListOfQuestions], [A|AnswerLabels]):-
        format('~w~nAnswer "yes" or "no": ', [Q]),
        nl,
        read(Response),
        length(ListOfQuestions, Length),
        (Response = yes ; Response = no),
        (   Response == yes ->
            assert(additional_answers(Patient, A)),
            ( Length > 0 ->  
            	additional_q(Patient, ListOfQuestions, AnswerLabels)
            ;   true
            )
         ; Response == no ->
        	( Length > 0 ->  
        	additional_q(Patient, ListOfQuestions, AnswerLabels)
            ;   true)
        ).


higher_likelihood(Patient, Disease, Questions, AnswerLabels):-
    count_likelihood_symptoms(Patient, AnswerLabels, Count),
    length(Questions, LengthQ),
    (
        Count > LengthQ * 0.25 ->
            (format('Also, the patient\'s reported symptoms and medical history show that the probability of having ~w is higher.~n', [Disease]))
    );
    true,
    !.

% di ko pa natetest to hehe
probability(Patient, Disease):-
    findall(Symptom, patient_symptoms(Symptom), PatientSymptoms),
    count_symptoms(Disease, PatientSymptoms, Count),
    disease(Disease, AllSymptoms),
    length(AllSymptoms, NumSymptoms),
    (   (Count >= NumSymptoms*0.25, Count < NumSymptoms*0.5) ->
    	format('The patient may have ~w disease. ~n', [Disease]) 
    ; (   Count >= NumSymptoms*0.5, Count < NumSymptoms*0.75) -> 
    	format('The patient likely have ~w disease. ~n', [Disease]) 
    ; (   Count >= NumSymptoms*0.75) -> 
    format('The patient most likely have ~w disease. ~n', [Disease])
    ;    true).


refer_medical(uti):-
	format('Refer to a medical facility for Urinalysis and/or Urine Culture for an accurate diagnosis.').
refer_medical(psoriasis):- 
    format('Refer to a medical facility for Skin biopsy, Blood test, and Joint Imaging for an accurate diagnosis.').
refer_medical(diarrhea):-
    format('Refer to a medical facility for Stool test, Blood test, and Endoscopy for an accurate diagnosis.').
refer_medical(tuberculosis):-
    format('Refer to a medical facility for Tuberculin skin test, TB blood tests, Chest X-ray, and Sputum test for an accurate diagnosis.').
refer_medical(chickenpox):-
	format('Refer to a medical facility for Viral culture, PCR test, and Blood test for an accurate diagnosis.').
refer_medical(influenza):-
    format('Refer to a medical facility for Viral culture, Rapid Influenza diagnostic test, and RT-PCR test for an accurate diagnosis.').
refer_medical(Disease):- !.

ask(Patient):-   
    format('What\'s the patient\'s name? Type name in lowercase letters.~n'),
    read(Patient).

ask_age(Patient, Age):-   
    repeat, format('What\'s the ~w\'s age?~n', [Patient]),   
    read(Age),
    number(Age), Age >= 1, Age =< 999.  
                                      
ask_sex(Patient, Sex):-   
    repeat, format('What\'s the patient\'s sex? m/f~n'),
    read(Sex),  
    valid_sex(Sex).  

ask_weight(Patient, Weight):-
	repeat, format('What\'s ~w\'s weight in kg? ~n', [Patient]),    
    read(Weight),  
    number(Weight), Weight >= 1, Weight =< 999. 

ask_height(Patient, Height):-
    repeat, format('What\'s ~w\'s height in cm?', [Patient]), nl,   
    read(Height),  
    number(Height), Height >= 1, Height =< 999.

compute_BMI(Weight, Height, BMI):-
    BMI is Weight / ((Height / 100) * (Height / 100)).
    
ask_symptoms:-
    is_main_symptom(MainSymptoms),
    is_supporting_symptom(OtherSymptoms),
    patient_has(MainSymptoms),
   	findall(Symptom, patient_symptoms(Symptom), Symptoms), %check if no main symptoms
    Symptoms == [] ->  patient_has(OtherSymptoms); true.

patient_has([]).
patient_has([Symptom | OtherSymptoms]) :-
    (
        asked_symptom(Symptom) ->
        patient_has(OtherSymptoms)
        ;
        format('Do you have or experience ~w?~nAnswer "yes" or "no": ', [Symptom]),
        nl,
        read(Response),
        (Response = yes ; Response = no),
        (Response == yes ->
            assert(has_symptom(Patient, Symptom)),
            assert(patient_symptoms(Symptom)),
            assert(asked_symptom(Symptom)),
            findall(ExistingSymp, patient_symptoms(ExistingSymp), PatientSymptomsList),
            find_symptoms(PatientSymptomsList, NewSymptoms),
            write(NewSymptoms),
            patient_has(NewSymptoms)
            ;
            Response == no ->
        	
        	assert(asked_symptom(Symptom)),
            patient_has(OtherSymptoms)
        )
    ).

find_symptoms(FilterList, Symptoms) :-
    print([FilterList]), nl,
    findall(S, (disease(_, S), subset(FilterList, S)), SymptomLists),
    flatten(SymptomLists, Symptoms).

count_symptoms(Disease, PatientSymptoms, Count):-
    disease(Disease, Symptoms),
    findall(SymptomElem, (member(SymptomElem, Symptoms), member(SymptomElem, PatientSymptoms)), MatchingSymptoms),
    length(MatchingSymptoms, Count).
    
/*rules and knowledge database*/ 

disease(hypertension, [headache, nosebleed, breath_shortness, loud_heartbeat, high_bp]).
disease(scabies, [itching, skin_rash, itchy_rash, burrows]).
disease(uti, [urination_pain, frequent_urination, pelvic_pressure, chills, lower_back_pain, nausea, fever]).
disease(dengue, [fever, headache, nausea, vomiting, skin_rash, body_aches, fatigue]).
disease(diabetes, [extreme_thirst, extreme_hunger, frequent_urination, weight_loss, fatigue, blurry_vision, slow_healing_wounds, itching, numbness]). 
disease(psoriasis, [red_patches, white_scales, cracked_skin, thickened_nails, itching, joint_stiffness]).
disease(diarrhea, [loose_stools, nausea, bloating, thirst, abdominal_pain, change_in_bowel_habits]).
disease(tuberculosis, [cough, bloody_cough, fatigue, weight_loss, night_sweats, chest_pain, fever]).
disease(chickenpox, [fever, skin_rash, blisters, scabs, headache, fatigue, appetite_loss, body_aches, itching]).
disease(influenza, [fever, cough, sore_throat, runny_nose, body_aches, headache, chills, fatigue]).

is_main_symptom([fever, cough, sore_throat, high_bp, burrows, abnormal_chest_sounds, bloody_cough, loose_stools, frequent_urination, extreme_thirst]).
is_supporting_symptom([headache, nosebleed, breath_shortness, loud_heartbeat, itching, skin_rash, itchy_rash, chills, 
                       chest_pain, nausea, breath_shortness, chest_congestion, wheezing,  bloating, thirst, abdominal_pain, 
                       change_in_bowel_habits, fatigue, weight_loss, night_sweats, body_aches, joint_pain, red_throat, yellow_discoloration,
                       runny_nose, blisters, scabs, appetite_loss, red_patches, white_scales, cracked_skin, 
                       vomiting, thickened_nails, joint_stiffness, urination_pain, pelvic_pressure, lower_back_pain,
                       extreme_hunger, blurry_vision, slow_healing_wounds, numbness]).

count_likelihood_symptoms(Patient, AnswerLabels, Count):-
    findall(Answer, (member(Answer, AnswerLabels), additional_answers(Patient, Answer)), CountedLikelihoodSymptoms),
    length(CountedLikelihoodSymptoms, Count).

higher_likelihood(hypertension, Patient, Age, BMI, Gender):-
    (Gender == f, Age > 65) ; 
     Age >= 40; 
     Gender == m;
     BMI > 24.9.

higher_likelihood(uti, Patient, Age, BMI, Gender):-
    Gender == f; 
    Age < 5;
    Age >= 65;
    BMI < 18.5; 
    BMI > 24.9.

higher_likelihood(diabetes, Patient, Age, BMI, Gender):-
    BMI > 24.9;
    Age > 45.

higher_likelihood(psoriasis, Patient, Age, BMI, Gender):-
    (Age >= 20, Age =< 30);
    (Age >= 50, Age =< 60);
    BMI > 18.5.

higher_likelihood(diarrhea, Patient, Age, BMI, Gender):-
    Age < 5;
    BMI > 24.9.

higher_likelihood(tuberculosis, Patient, Age, BMI, Gender):-
    Age >= 65;
    BMI < 18.5.

higher_likelihood(chickenpox, Patient, Age, BMI, Gender):-
    Age < 10.

higher_likelihood(influenza, Patient, Age, BMI, Gender):-
    Age < 5;
    Age > 65;
    BMI >= 30.

diagnose(Patient, scabies):-
    has_symptom(Patient, burrows),
    (has_symptom(Patient, itching);
    has_symptom(Patient, skin_rash);
    has_symptom(Patient, itchy_rash)).

diagnose(Patient, hypertension):-
    has_symptom(Patient, high_bp),
    (has_symptom(Patient, headache);
    has_symptom(Patient, nosebleed);
    has_symptom(Patient, breath_shortness);
    has_symptom(Patient, loud_heartbeat)).

diagnose(Patient, uti):-
    has_symptom(Patient, frequent_urination),
    (has_symptom(Patient, urination_pain);
    has_symptom(Patient, pelvic_pressure);
    has_symptom(Patient, chills);
    has_symptom(Patient, lower_back_pain);
    has_symptom(Patient, nausea);
    has_symptom(Patient, fever)).

diagnose(Patient, influenza):-
    has_symptom(Patient, fever),
   	(has_symptom(Patient, cough);
    has_symptom(Patient, sore_throat)),
    (has_symptom(Patient, runny_nose);
    has_symptom(Patient, body_aches);
    has_symptom(Patient, headache);
    has_symptom(Patient, chills);
    has_symptom(Patient, fatigue)).

diagnose(Patient, dengue):-
    has_symptom(Patient, fever),
    (has_symptom(Patient, headache);
    has_symptom(Patient, nausea);
    has_symptom(Patient, vomiting);
    has_symptom(Patient, skin_rash);
    has_symptom(Patient, body_aches);
    has_symptom(Patient, fatigue)).

diagnose(Patient, diabetes):-
    has_symptom(Patient, extreme_hunger), 
    has_symptom(Patient, extreme_thirst),
    has_symptom(Patient, weight_loss),
    (has_symptom(Patient, frequent_urination);
    has_symptom(Patient, fatigue);
    has_symptom(Patient, blurry_vision);
    has_symptom(Patient, slow_healing_wounds);
    has_symptom(Patient, itching);
    has_symptom(Patient, numbness)).

diagnose(Patient, psoriasis):-
    has_symptom(Patient, red_patches),
    has_symptom(Patient, white_scales),
    (has_symptom(Patient, cracked_skin);
    has_symptom(Patient, thickened_nails);
    has_symptom(Patient, itching);
    has_symptom(Patient, joint_stiffness)).

diagnose(Patient, diarrhea):-
    has_symptom(Patient, loose_stools),
    has_symptom(Patient, abdominal_pain);
    (has_symptom(Patient, nausea);
    has_symptom(Patient, bloating);
    has_symptom(Patient, thirst);
    has_symptom(Patient, abdominal_pain);
    has_symptom(Patient, change_in_bowel_habits)).

diagnose(Patient, tuberculosis):-
    has_symptom(Patient, cough),
    has_symptom(Patient, bloody_cough),
    (has_symptom(Patient, fatigue);
    has_symptom(Patient, weight_loss);
    has_symptom(Patient, night_sweats);
    has_symptom(Patient, chest_pain);
    has_symptom(Patient, fever)).

diagnose(Patient, chickenpox):-
    has_symptom(Patient, fever),
    (has_symptom(Patient, skin_rash);
    has_symptom(Patient, blisters);
    has_symptom(Patient, scabs)),
    (has_symptom(Patient, headache);
    has_symptom(Patient, fatigue);
    has_symptom(Patient, appetite_loss);
    has_symptom(Patient, body_aches);
    has_symptom(Patient, itching)).




additional_q_db(uti, [
    'Did you previously have UTI?',
    'Have you recently done any sexual activity?',
    'Are you currently in menopause or have used spermicides?',
    'Are you pregnant?',
    'Do you consider yourself to have poor hygiene?'
]).

additional_q_db(diabetes,[
    'Do you have a family history for diabetes?'
]).

additional_q_db(psoriasis, [
    'Do you smoke?',
    'Do you drink alcohol?',
    'Do you have a family history for psoriasis?'
]).

additional_q_db(diarrhea, [
    'Do you have a history of irritable bowel syndrome (IBS)?',
    'Do you have a history of inflammatory bowel disease (IBD)?'
]).

additional_q_db(tuberculosis, [
    'Do you have HIV?',
    'Do you have a history of tuberculosis?',
    'Have you recently been exposed to someone with tuberculosis?'
]).

additional_q_db(chickenpox, [
    'Are you pregnant?',
    'Have you not been vaccinated against chickenpox?',
    'Have you recently been exposed to someone who has chickenpox?'
]).

additional_q_db(influenza, [
    'Are you pregnant?',
    'Have you recently been exposed to someone who is sick?'
]).

additional_q_db(Disease, []).

additional_a_db(uti, [
    'previous_uti',
    'sexual_activity',
    'meno_sperm',
    'pregnant',
    'poor_hygiene'
]).

additional_a_db(diabetes, [
    'family_history_has_diabetes'
]).

additional_a_db(psoriasis, [
    'smoking',
    'alcohol',
    'family_history_has_psoriasis'
]).

additional_a_db(diarrhea,[
    'history_of_IBS',
    'history_of_IBD'
]).

additional_a_db(tuberculosis, [
    'hiv_infected',
    'history_of_tb',
    'exposed_to_someone_with_tb'
]).

additional_a_db(chickenpox, [
    'pregnant',
    'chickenpox_vaccination',
    'exposed_to_someone_who_has_chickenpox'                    
]).

additional_a_db(influenza, [
    'pregnant',
    'exposed_to_someone_who_is_sick'
]).


additional_a_db(Disease, []).
valid_sex(Sex):- 
    member(Sex, [m, f]).
