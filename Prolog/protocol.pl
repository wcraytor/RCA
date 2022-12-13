% Title:        Workflow for Residual Constraint Approach (RCA)
% Author:       William B Craytor
% Date:         12/12/2022
% Language:     SWI-Prolog,  Version 9.0.2
% Description:  This is a draft template for a Prolog program that dictates the protocol to be followed by an automaton to execute the RCA Appraisal Protocol
% Note 1:       Actual times are minutes to be filled in by some mechanism.  Same for Estimated Value and NumberOfSalesComparables
% Note 2:       Execute from SWI-Prolog by entering the command "execute_protocol."
% Note 3:       Of course this can be expanded as necessary.  Also it is intended primarily as an exercise in diligence to focus on the workflow - until such time
%               automatons to do the work come into existence.


% Phase I Breakdown
contract_and_scope_of_work_setup(OldTime, NewTime) :- NewTime is OldTime + 30, write('Contract and Scope of Work Completed.'),nl, true.
obtain_mls_comps(NumberOfComps, OldTime, NewTime)  :- NumberOfComps is 310, NewTime is OldTime + 20, write('MLS Comparables have been downloaded.'),nl, true.
build_mars_model_for_market_area(OldTime, NewTime) :- NewTime is OldTime + 90, write('The MARS price model for the market area has been  completed.'),nl,true.
select_comparables_to_photo(OldTime, NewTime)      :- NewTime is OldTime + 15, write('The sales comparables to be photographed have been selected.'),nl,true.

 
% Phase II Breakdown
travel_office_to_subject(OldTime,NewTime)             :- NewTime is OldTime + 35, write('Travel to subject property completed.'), nl,true.
draw_hand_sketch(OldTime, NewTime)                    :- NewTime is OldTime + 5,  write('Hand sketch of property has been completed.'), nl,true.
gps_coordinates_all_corners(OldTime, NewTime)         :- NewTime is OldTime + 10, write('GPS coordinates of property corners obtained.'),nl, true.
exterior_measurements_add_to_sketch(OldTime, NewTime) :- NewTime is OldTime + 15, write('Exterior measurements have been completed.'),nl, true.
aerial_photography(OldTime, NewTime)                  :- NewTime is OldTime + 15, write('Aerial photography completed.'),nl, true.
exterior_photographs(OldTime, NewTime)                :- NewTime is OldTime + 10, write('Exterior photos of property completed.'),nl, true.
interior_photographs(OldTime, NewTime)                :- NewTime is OldTime + 30, write('Interior photos of house completed.'),nl, true.
interior_measurements_add_to_sketch(OldTime, NewTime) :- NewTime is OldTime + 15, write('Interior measurements of property have been completed.'),nl,true.
photograph_comparables(OldTime, NewTime)              :- NewTime is OldTime + 40, write('Comparables photographed.'), nl, true.
return_to_office(OldTime, NewTime)                    :- NewTime is OldTime + 35, write('Returned to office.'), nl, true.

% Phase III Breakdown
upload_digital_media(OldTime, NewTime)                    :- NewTime is OldTime + 10, write('Digital media uploaded.'),nl, true.
process_with_lightroom(OldTime, NewTime)                  :- NewTime is OldTime + 60, write('Lightroom processing completed.'),nl, true.
create_cad_floorplan(OldTime, NewTime)                    :- NewTime is OldTime + 60, write('CAD Floorplan is done.'),nl, true.
residual_ranking(OldTime, NewTime)                        :- NewTime is OldTime + 30, write('Residual ranking is completed.'),nl, true.
subject_ranking(OldTime, NewTime)                         :- NewTime is OldTime + 30, write('Subject ranking is completed.'),nl,true.
subject_score(OldTime, NewTime)                           :- NewTime is OldTime + 30, write('Subject has received Residual Score.'),nl,true.
subject_residual(OldTime, NewTime)                        :- NewTime is OldTime + 30, write('Subject residual has been determined.'),nl, true. 
comparable_final_selection(OldTime, NewTime)              :- NewTime is OldTime + 15, write('Selection of final comparables has been completd.'),nl, true.
calculate_adjustments_and_input_to_forms(OldTime,NewTime) :- NewTime is OldTime + 30, write('Adjustments have been calculated and exported as spreadsheet.'),nl,true.
calculate_value_estimate(OldTime, NewTime, Value)         :- Value = '$550,000', 
                                                             NewTime is OldTime + 15, 
                                                             write('A value estimate for the subject property has been completed)'),
															 nl,
															 true.
complete_report(OldTime,NewTime)                          :- NewTime is OldTime + 45, 
                                                             write('The report has been completed.'), 
															 nl,  
															 true.


% Phase I
phase_I(OldTime, NewTime, NumberOfComps) :-
            nl, write('====== PHASE I ======'),nl,
			contract_and_scope_of_work_setup(OldTime, Time1),
            obtain_mls_comps(NumberOfComps, Time1, Time2),
            build_mars_model_for_market_area(Time2,Time3),
			select_comparables_to_photo(Time3,NewTime), 
            write('Phase I/Preparatory is completed: '), 
			Hrs is NewTime div 60,
			Mins is NewTime mod 60,
			write(Hrs),
			write(' hours, '),
			write(Mins),
			write(' minutes.'), nl,nl,
			true.

% Phase II - Visit Property and Comps

phase_II(OldTime, NewTime) :-  
            nl, write('====== PHASE II ======'),nl,
            travel_office_to_subject(OldTime,Time1),
            draw_hand_sketch(Time1, Time2),
            gps_coordinates_all_corners( Time2, Time3),
			exterior_photographs( Time3,  Time4),
			aerial_photography(Time4,Time5),
            exterior_measurements_add_to_sketch( Time5,  Time6),
			interior_photographs( Time6,  Time7),
			interior_measurements_add_to_sketch( Time7, Time8),
			photograph_comparables(Time8,Time9),
			return_to_office(Time9,NewTime),
			write('Phase II/Inspection is completed: '), 
			T is NewTime - OldTime,
			Hrs is T div 60,
			Mins is T mod 60,
			write(Hrs),
			write(' hours, '),
			write(Mins),
			write(' minutes.'), nl, nl,
			true.

% Phase III - Final Steps
phase_III(OldTime,NewTime,Value) :-
            nl, write('====== PHASE III ======'),nl,
            upload_digital_media(OldTime,Time1),
			process_with_lightroom(Time1,Time2),
            create_cad_floorplan(Time2,Time3 ),
			residual_ranking(Time3, Time4),
			subject_ranking(Time4,Time5),
			subject_score(Time5,Time6),
			subject_residual(Time6,Time7),
			comparable_final_selection(Time7,Time8),
			calculate_value_estimate(Time8,Time9,Value),
			complete_report(Time9,NewTime),
			write('Phase III/Post Production is completed: '), 
			T is NewTime - OldTime,
			Hrs is T div 60,
			Mins is T mod 60,
			write(Hrs),
			write(' hours, '),
			write(Mins),
			write(' minutes.'), nl, nl,
			true.
			
% Main Workflow
execute_protocol() :- 
            phase_I(0,T1,NumberOfComps),
            phase_II(T1,T2),
			phase_III(T2,TotalTime,Value),
			Hrs is TotalTime/60,
			TotalHours is float_integer_part(Hrs) + float_fractional_part(Hrs),
			nl,
		    write('Residual Constraint Approach Protocol Completed.'), 
			nl,
			write('Value is: '),
			write(Value),
			write("."),
			nl, 
			write('Total Time: '),
			write(TotalHours),
			write(' hours.'),
			true.
		 
			
            