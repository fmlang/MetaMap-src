% File:     mmi.pl
% Module:   MetaMap Indexing (MMI)
% Author:   Lan
% Purpose:  Ranks concepts found by MetaMap in biomedical text (MEDLINE
%           citations).


:- module(mmi,[
	do_MMI_processing/5,
	initialize_mmi/0
    ]).

:- use_module(lexicon(lexical),[
	concatenate_strings/3
    ]).

:- use_module(metamap(metamap_evaluation),[
	extract_components/3,
	merge_contiguous_components/2
    ]).

:- use_module(metamap(metamap_tokenization),[
	parse_phrase_word_info/9,
	tokenize_text/2,
	tokenize_text_more/2,
	tokenize_text_utterly/2
    ]).

:- use_module(mmi(mmi_med_cit),[
	construct_citation_from_utterances/3
    ]).

:- use_module(mmi(mmi_med_cit),[
	citation_field/3,
	compute_field_value/3
    ]).

:- use_module(mmi(mmi_mmo),[
	mmo_opt_mode/1,
	phrase_info/3
    ]).

:- use_module(skr(skr_utilities),[
	expand_all_phrase_texts/3,
	get_token_text/2
    ]).

% also for use with mmopt
:- use_module(skr_db(db_access),[
	get_db_access_year/1,
	initialize_db_access/3,
	db_get_concept_cui/2,
	db_get_concept_sts/2,
	db_get_meta_mesh_tc/3,
	db_get_meta_mesh/2,
	db_get_mesh_tc_strict/2,
	db_get_mesh_tc_relaxed/2,
	db_get_mesh_mh/2
    ]).

:- use_module(skr_lib(semtype_translation09), [
	expand_semtypes/2
    ]).

:- use_module(skr(skr_utilities), [
	compute_original_phrase/7,
	compute_original_phrase_1/7,
	skr_begin_write/1,
	skr_begin_write2/1,
	skr_end_write/1
    ]).

:- use_module(skr_lib(ctypes),[
	is_alnum/1,
	is_white/1
    ]).

:- use_module(skr_lib(nls_strings),[
	atom_codes_list/2,
	split_string/4,
	split_string_completely/3,
	string_prefix_n/4,
	trim_and_compress_internal_whitespace/2
    ]).

:- use_module(skr_lib(nls_system), [
	control_option/1,
	control_value/2
    ]).

:- use_module(skr_lib(sicstus_utils),[
	concat_atoms/2,
	concat_atoms_with_separator/3,
	index/3,
	lower/2,
	substring/4,
	ttyflush/0,
	upper/2
   ]).

:- use_module(text(text_objects),[
	extract_token_strings/2
   ]).		     

:- use_module(library(aggregate),[
	aggregate/3
    ]).

:- use_module(library(avl),[
	avl_member/3
    ]).

:- use_module(library(lists),[
	append/2,
	last/2,
	rev/2,
	subseq0/2
    ]).

:- dynamic processing_parameters/2.
:- dynamic max_freq/1.
:- dynamic prev_n/1.

do_MMI_processing(OrigUtterances, BracketedOutput,
		  Sentences, CoordSentences, DisambMMOutput) :-
	% Do MMI processing, if requested
	( control_value('XML', _) ->
	  true
	; control_option(machine_output) ->
	  true
	; control_option(mmi_output) ->
	  ( BracketedOutput =:= 1 ->
	    skr_begin_write2('MMI')
	  ; true
	  ),
          construct_citation_from_utterances(OrigUtterances, CitationRecord, UIAtom),
          current_output(Stream),
          process_citation(CitationRecord, Sentences, CoordSentences, DisambMMOutput, UIAtom,
	    		     Stream, none, none, none),
          ( BracketedOutput =:= 1 ->
	    skr_end_write('MMI')
	  ; true
	  )
        ; control_option(fielded_mmi_output) ->
          ( BracketedOutput =:= 1 ->
            skr_begin_write('MMI')
	  ; true
	  ),
          construct_citation_from_utterances(OrigUtterances, CitationRecord, UIAtom),
          current_output(Stream),
          process_citation(CitationRecord, Sentences, CoordSentences, DisambMMOutput, UIAtom,
	    		     none, none, none, Stream),
          ( BracketedOutput =:= 1 ->
	    skr_end_write('MMI')
	  ; true
	  )
        ; true
	).

initialize_mmi :-
    retractall(prev_n(_)),
    assert(prev_n(0)),
    set_default_processing_parameters,
    set_default_max_freq,
    (control_value(mm_data_version,Version) ->
	true
    ;   Version=normal
    ),
    get_db_access_year(Year),
    (control_option(strict_model) ->
	initialize_db_access(Version,strict,Year),
	(control_option(relaxed_model) ->
	    warn_about_model('relaxed model')
	;   true
	)
    ;   control_option(relaxed_model) ->
	initialize_db_access(Version,relaxed,Year)
    ;   initialize_db_access(Version,strict,Year)
    ).

warn_about_model(Model) :-
    format('WARNING: The additional request for the ~a has been ignored.~n',
	   [Model]).

% 2000 parameters
set_default_processing_parameters :-
%    \+control_option(use_dice_coefficient),
    retractall(processing_parameters(_,_)),
    assert(processing_parameters(wmm,1)),   % MetaMap weight
    assert(processing_parameters(wm,14)),   % MeSH tree depth weight
    assert(processing_parameters(ww,0)),    % word count weight
    assert(processing_parameters(wc,0)),    % character count weight
    assert(processing_parameters(wd,1)),    % default tree depth
    assert(processing_parameters(nmm,-10)), % MetaMap normalization index
    assert(processing_parameters(nm,0)),    % MeSH normalization index
    assert(processing_parameters(nw,0)),    % word normalization index
    assert(processing_parameters(nc,0)),    % character normalization index
    assert(processing_parameters(nf,-5)),   % frequency normalization index
    assert(processing_parameters(nz,0)),    % final normalization index
    !.
% 1996 parameters
%set_default_processing_parameters :-
%    \+control_option(use_dice_coefficient),
%    retractall(processing_parameters(_,_)),
%    assert(processing_parameters(wmm,1)),   % MetaMap weight
%    assert(processing_parameters(wm,12)),   % MeSH tree depth weight
%    assert(processing_parameters(ww,2)),    % word count weight
%    assert(processing_parameters(wc,1)),    % character count weight
%    assert(processing_parameters(wd,1)),    % default tree depth
%    assert(processing_parameters(nmm,-30)), % MetaMap normalization index
%    assert(processing_parameters(nm,0)),    % MeSH normalization index
%    assert(processing_parameters(nw,-10)),  % word normalization index
%    assert(processing_parameters(nc,-10)),  % character normalization index
%    assert(processing_parameters(nf,5)),    % frequency normalization index
%    assert(processing_parameters(nz,0)),    % final normalization index
%    !.
% use_dice_coefficient is obsolete
%set_default_processing_parameters :-
%    control_option(use_dice_coefficient),
%    retractall(processing_parameters(_,_)),
%    assert(processing_parameters(wmm,2)),
%    assert(processing_parameters(wm,12)),
%    assert(processing_parameters(ww,2)),
%    assert(processing_parameters(wc,1)),
%    assert(processing_parameters(wd,1)),
%    assert(processing_parameters(nmm,-35)),
%    assert(processing_parameters(nm,0)),
%    assert(processing_parameters(nw,-10)),
%    assert(processing_parameters(nc,-10)),
%    assert(processing_parameters(nf,10)),
%    assert(processing_parameters(nz,0)),
%    !.

set_default_max_freq :-
    retractall(max_freq(_)),
    assert(max_freq(13)),
    !.


/* process_citation(+CitationRecord, +Sentences, +CoordSentences, +MMOutput, +UIAtom,
                    +ReportStream, +SummaryStream, +FieldedStream, +FieldedStream2)

process_citation/7
process_citation/6
xxx
*/

process_citation(CitationRecord, Sentences, CoordSentences, MMOutput, UIAtom,
		 ReportStream, SummaryStream, FieldedStream, FieldedStream2) :-
	MMOutput = [FirstMMOTerm|_RestMMOTerms],
	FirstMMOTerm = mm_output(_ExpandedUtterance,CitationTextAtom,_ModifiedText,_Tagging,
				 _AAs,_Syntax,_DisambiguatedMMOPhrases,_ExtractedPhrases),
	% temp
	% format(ReportStream,'~nUIAtom: "~p"~nMMOutput: ~p~n',[UIAtom,MMOutput]),
	% format('~nUIAtom: "~p"~nMMOutput: ~p~n',[UIAtom,MMOutput]),
	%    exp_compute_mesh_in_text(ReportStream,MMOutput,TFInfo),
	exp_compute_mesh_in_text(MMOutput, Sentences, CoordSentences, TFInfo),
	% temp
	% wl(ReportStream,TFInfo,'TFInfo:'),
	% wl(TFInfo,'TFInfo:'),
	preprocess_tf(TFInfo, ATFInfo),
	% temp
	% wl(ReportStream,ATFInfo,'ATFInfo:'),
	rank_tf(ATFInfo, AATFInfo, _RankingMethod),
	% temp
	% wl(ReportStream,AATFInfo,'AATFInfo:'),
	compute_mesh_in_mesh(CitationRecord, _ParsedMHField, MHInfo),
	% temp
	%wl(ReportStream,MHInfo,'MHInfo:'),
	extract_mhs(MHInfo, MHs),
	% temp
	%wl(ReportStream,MHs,'MHs:'),
	mark_tf(AATFInfo, MHs, MTFInfo),
	% temp
	%wl(ReportStream,MTFInfo,'MTFInfo:'),
	length(MHs, TotRel),
	compute_rp_pairs(MTFInfo, TotRel, RPPairs0),
	% temp
	%wl(ReportStream,RPPairs0,'Raw RP Pairs'),
	conflate_rp_pairs(RPPairs0, RPPairs),
	% temp
	%wl(ReportStream,RPPairs,'Conflated RP Pairs'),
	( control_option(use_dice_coefficient) ->
	  compute_max_dice(RPPairs, MaxDice),
          ( ReportStream==none ->
            true
	  ; format(ReportStream,'~nMMI concepts',[]),
	    ( UIAtom=='' ->
              true
            ; format(ReportStream,' for ~a',[UIAtom])
	    ),
	    ( MaxDice > 0.0 ->
	      format(ReportStream,' (MaxDice=~f):~n',[MaxDice])
	    ; format(ReportStream,':~n',[])
	    ),
	    dump_aatf_info(AATFInfo, MHInfo, MTFInfo, UIAtom, ReportStream)
	  ),
	  ( FieldedStream == none ->
	    true
          ; dump_aatf_info_fielded(AATFInfo, MHInfo, MTFInfo, UIAtom, MMOutput, FieldedStream)
	  ),
	  ( FieldedStream2 == none ->
	    true
	  ; dump_aatf_info_fielded2(AATFInfo, CitationTextAtom, MHInfo,
				    MTFInfo, UIAtom, MMOutput, FieldedStream2)
	  ),
          ( SummaryStream == none ->
	    true
	  ; format(SummaryStream,'~a|~f~n',[UIAtom,MaxDice])
	  )
	  ; standardize_rp_pairs(RPPairs,SRPPairs,_StandardizationMethod),
	    % temp
	    % concatenate_items_to_atom(['Standardized RP Pairs (',
	    %                              StandardizationMethod,')'],TTitle),
	    % wl(ReportStream,SRPPairs,TTitle),
	    compute_average_precision(SRPPairs,AP,_AveragingMethod),
	    % temp
	    % format(ReportStream,'~nAP (~a): ~p~n',[AveragingMethod,AP]),
	    ( ReportStream==none ->
	      true
	    ; format(ReportStream,'~nMMI concepts',[]),
	      ( UIAtom=='' ->
	        true
	      ; format(ReportStream,' for ~a',[UIAtom])
	      ),
	      ( AP > 0.0 ->
	        format(ReportStream,' (3-pt AP=~f):~n',[AP])
	      ; format(ReportStream,':~n',[])
	      ),
	      dump_aatf_info(AATFInfo, MHInfo, MTFInfo, UIAtom, ReportStream)
	    ),
	    ( FieldedStream == none ->
	      true
	    ; dump_aatf_info_fielded(AATFInfo, MHInfo, MTFInfo, UIAtom, MMOutput, FieldedStream)
	    ),
	    ( FieldedStream2 == none ->
	      true
            ; dump_aatf_info_fielded2(AATFInfo, CitationTextAtom, MHInfo,
				      MTFInfo, UIAtom, MMOutput, FieldedStream2)
	    ),
            ( SummaryStream == none ->
	      true
	    ; format(SummaryStream,'~a|~f~n',[UIAtom,AP])
	    )
	),
	!.
process_citation(_CitationRecord, _Sentences, _CoordSentences, _MMOutput, UIAtom,
                 _ReportStream, _SummaryStream, _FieldedStream, _FieldedStream2) :-
    format('ERROR: process_citation/6 failed for UI ~a.~n',[UIAtom]),
    fail.


% temp
wl(List,Heading) :-
    current_output(Stream),
    wl(Stream,List,Heading),
    flush_output(Stream).

wl(Stream,List,Heading) :-
    format(Stream,'~n~a~n',[Heading]),
    wl_aux(Stream,List).

wl_aux(_,[]).
wl_aux(Stream,[First|Rest]) :-
    format(Stream,'    ~p~n',[First]),
    wl_aux(Stream,Rest).


/* compute_mesh_in_mesh(+CitationRecord, -ParsedMHField, -MHInfo)
   extract_headings_adding_tree_codes(+ParsedMHField, -MHInfo)

compute_mesh_in_mesh/3 computes both a ParsedMHField and MHInfo from the
MH field of CitationRecord.  ParsedMHField is of the form
     [heading(H,MNM),sub(SH,MNM),sub(SH,MNM),...]
e.g., 
     for Mesh heading = "*Laser"
         Mesh term = [heading("Laser",main)]

 and for Mesh heading = "Choroid/*RADIATION EFFECTS/ULTRASTRUCTURE"
         Mesh term = [heading("Choroid",main),
                      sub("RADIATION EFFECTS",main),
                      sub("ULTRASTRUCTURE",nm)]
MHInfo is a list of terms of the form
     mh(Heading,MNM,TreeCodes)
e.g.,
     mh("Laser",main,["H1.671.606.552.548","H1.671.768.638.548"]).
extract_headings_adding_tree_codes/2 is an auxiliary predicate.
*/

compute_mesh_in_mesh(CitationRecord,ParsedMHField,MHInfo) :-
    citation_field(mh,CitationRecord,MHField),
    compute_field_value(mh,MHField,ParsedMHField),
    extract_headings_adding_tree_codes(ParsedMHField,MHInfo),
    !.

extract_headings_adding_tree_codes([],[]).
extract_headings_adding_tree_codes([[heading(H,MNM0)|_Subheadings]|Rest],
                                  [mh(H,MNM0,TreeCodes)|RestMHInfo]) :-
    (control_option(strict_mesh_tc) ->
        db_get_mesh_tc_strict(H,TreeCodes)
    ;   db_get_mesh_tc_relaxed(H,TreeCodes)
    ),
    extract_headings_adding_tree_codes(Rest,RestMHInfo).


% WARNING: This is actually a misnomer; the real goal here is the treecodes
%          but the concept can be non-MeSH unless --restrict_to_mesh is on
%          This operation will eventually be generalized to use other
%          hierarchies.
% for use with debug version (arity 3)
%exp_compute_mesh_in_text(MMOutput,TFInfo) :-
%    exp_compute_mesh_in_text(user_output,MMOutput,TFInfo).

%exp_compute_mesh_in_text(ReportStream,MMOutput,TFInfo) :-
exp_compute_mesh_in_text(MMOutput, Sentences, CoordSentences, TFInfo) :-
	( control_value(debug, DebugFlags),
	  memberchk(0, DebugFlags) ->
	  get_pre_tf_in_utterances(MMOutput, Sentences, CoordSentences, _MarkedText, PreTFInfo0)
        ; exp_get_pre_tf_in_utterances(MMOutput, Sentences, CoordSentences, PreTFInfo0)
        ),
	% temp
	% wl(ReportStream,PreTFInfo0,'PreTFInfo0:'),
	% wl(PreTFInfo0,'PreTFInfo0:'),
	exp_augment_pre_tf(PreTFInfo0, PreTFInfo1),
	sort(PreTFInfo1, PreTFInfo),
	% temp
	% wl(ReportStream,PreTFInfo,'PreTFInfo:'),
	exp_compute_mesh_in_textfields(PreTFInfo, TFInfo0),
	% temp
	% wl(ReportStream,TFInfo0,'TFInfo0:'),
	% wl(TFInfo0,'TFInfo0:'),
	exp_augment_tf(TFInfo0, TFInfo1),
	sort(TFInfo1, TFInfo2),
	exp_collapse_tf(TFInfo2, TFInfo),
	!.

get_pre_tf_in_utterances([], _Sentences, _CoordSentences, [], []).
get_pre_tf_in_utterances([Utterance|Rest], Sentences, CoordSentences, MarkedText, PreTFInfo) :-
	get_pre_tf_in_utterance(Utterance,
				Sentences, CoordSentences, CoordSentencesOut,
				UtteranceMarkedText, UtterancePreTFInfo),
	append(UtteranceMarkedText, RestMarkedText, MarkedText),
	append(UtterancePreTFInfo, RestPreTFInfo, PreTFInfo),
	get_pre_tf_in_utterances(Rest, Sentences, CoordSentencesOut, RestMarkedText, RestPreTFInfo).

exp_get_pre_tf_in_utterances([], _Sentences, _CoordSentences, []).
exp_get_pre_tf_in_utterances([Utterance|Rest], Sentences, CoordSentencesIn, PreTFInfo) :-
	exp_get_pre_tf_in_utterance(Utterance,
				    Sentences, CoordSentencesIn, CoordSentencesOut,
				    UtterancePreTFInfo),
	append(UtterancePreTFInfo, RestPreTFInfo, PreTFInfo),
	exp_get_pre_tf_in_utterances(Rest, Sentences, CoordSentencesOut, RestPreTFInfo).

get_pre_tf_in_utterance(Utterance,
			Sentences, CoordSentencesIn, CoordSentencesOut,
			MarkedText, PreTFInfo) :-
	Utterance = mm_output(utterance(Label,_Text,_PosInfo,_ReplPos),
			      _Citation,_ModifiedText,_AAs,_Tagging,
			      _Syntax,Phrases,_ExtractedPhrases),
	determine_field_nsent(Label, Field, NSent),
	( control_option(titles_only),
	  Field \== ti ->
	  MarkedText = [],
	  PreTFInfo = []
        ; control_option(abstracts_only),
	  Field \== ab ->
	  MarkedText = [],
	  PreTFInfo = []
        ; get_pre_tf_in_phrases(Phrases,
			        Sentences, CoordSentencesIn, CoordSentencesOut,
				Field, NSent, MarkedText, PreTFInfo)
        ).

exp_get_pre_tf_in_utterance(Utterance,
			    Sentences, CoordSentencesIn, CoordSentencesOut,
			    PreTFInfo) :-
	Utterance = mm_output(utterance(Label,_Text,_PosInfo,_ReplPos),
			      _Citation,_ModifiedText,_Tagging,AAs,
			      _Syntax,Phrases,_ExtractedPhrases),
	determine_field_nsent(Label, Field, NSent),
	( control_option(titles_only),
	  Field \== ti ->
	  PreTFInfo=[]
        ; control_option(abstracts_only),
	  Field \== ab ->
	  PreTFInfo=[]
	; exp_get_pre_tf_in_phrases(Phrases, AAs,
				    Sentences, CoordSentencesIn, CoordSentencesOut,
				    Field, NSent, PreTFInfo)
        ).

exp_augment_pre_tf(PreTFInfoIn, PreTFInfoOut) :-
	exp_augment_pre_tf(PreTFInfoIn, 1, PreTFInfoOut).

exp_augment_pre_tf([], _, []).
exp_augment_pre_tf([tf0(Concept,STs,Term,Value,TextAndField)|Rest],
		   N,
                   [tf0(Concept,STs,Term,Value,TextAndField,N)|AugmentedRest]) :-
	M is N + 1,
	exp_augment_pre_tf(Rest, M, AugmentedRest).

determine_field_nsent(Label, Field, NSent) :-
	atom_codes(Label, LabelString),
	append([_X,":",_Y,":",FieldString,":",NString],LabelString),
	!,
	atom_codes(Field, FieldString),
	( number_codes(NSent,NString) ->
	  true
	; NSent = 0
	).
determine_field_nsent(Label, Field, NSent) :-
	atom_codes(Label, LabelString),
	% We want the last two period-delimited components of LabelString
	split_string_completely(LabelString, ".", SplitLabelString),
	append(_, [FieldString,NString], SplitLabelString),
	% append([_X,".",FieldString,".",NString],LabelString),
	!,
	atom_codes(Field, FieldString),
	( number_codes(NSent, NString) ->
	  true
	; NSent = 0
	).

get_pre_tf_in_phrases([], _Sentences, CoordSentences, CoordSentences, _Field, _NSent, [], []).
get_pre_tf_in_phrases([Phrase|Rest], Sentences, CoordSentencesIn, CoordSentencesOut,
		      Field, NSent, [MarkedText|RestMarkedText],
                      PreTFInfo) :-
	get_pre_tf_in_phrase(Phrase, Sentences, CoordSentencesIn, CoordSentencesNext,
			     Field, NSent, MarkedText, PhrasePreTFInfo),
	append(PhrasePreTFInfo, RestPreTFInfo, PreTFInfo),
	get_pre_tf_in_phrases(Rest, Sentences, CoordSentencesNext, CoordSentencesOut,
			      Field, NSent, RestMarkedText, RestPreTFInfo).

exp_get_pre_tf_in_phrases([], _AAs, _Sentences, CoordSentences, CoordSentences, _Field, _NSent, []).
exp_get_pre_tf_in_phrases([Phrase|Rest], AAs, Sentences, CoordSentencesIn, CoordSentencesOut,
			  Field, NSent, PreTFInfo) :-
	exp_get_pre_tf_in_phrase(Phrase, AAs,
				 Sentences, CoordSentencesIn, CoordSentencesNext,
				 Field, NSent, PhrasePreTFInfo),
	append(PhrasePreTFInfo, RestPreTFInfo, PreTFInfo),
	exp_get_pre_tf_in_phrases(Rest, AAs,
				  Sentences, CoordSentencesNext, CoordSentencesOut,
				  Field, NSent, RestPreTFInfo).

get_pre_tf_in_phrase(Phrase,
		     _Sentences, _CoordSentencesIn, _CoordSentencesOut,
		     Field, NSent, Field-MarkedText, PreTFInfo) :-
	phrase_info(mappings, Phrase, mappings(Mappings)),
	phrase_info(phrase, Phrase, phrase(PTextAtom,Syntax,_PosInfo,_ReplacementPos)),
	parse_phrase_word_info(Syntax, filtered, 1, [], PhraseWords0,
			       [], _PhraseHeadWords, [], _PhraseMap0),
	( Mappings == [] ->
	  Mapping = []
        ; control_option(use_only_first_mapping) ->
          Mappings = [map(_,Mapping)|_]  % take the first mapping
        ; form_super_mapping(Mappings,SuperMapping)
	),
	atom_codes(PTextAtom, PText),
	( SuperMapping == [] ->
	  MarkedText = PText,
	  PreTFInfo = []
	; atom_codes_list( PhraseWords0, PhraseWords),
	  % temp
	  % rev(PhraseMap0,PhraseMap),
	  % format('~nPhrase: ~p~nPhrase words: ~p~nPhrase map: ~p~nPText: ~p~n',
	  %       [Syntax,PhraseWords,PhraseMap,PText]),
	  %( PText==" of the F-phospholipids" ->
	  %    trace
	  % ;   true
	  %),
          annotate_text(PhraseWords, PText, AnnotatedPText),
	  % temp
	  % wl(AnnotatedPText,'Annotated PText'),
          get_pre_tf_in_mapping(SuperMapping, AnnotatedPText, Field, NSent,
                                PhraseComponents, PreTFInfo),
	  % temp
	  % format('Phrase components: ~p~n',[PhraseComponents]),
          mark_text(AnnotatedPText, PhraseComponents, MarkedText)
	).

% Phrase is a term of the following form, created by skr_phrase:
% phrase(phrase(PhraseTextAtom0,Phrase,StartPos/Length,ReplacementPos),
%        candidates(Evaluations),
%        mappings(Mappings),
%        pwi(PhraseWordInfo),
%        gvcs(GVCs),
%        ev0(Evaluations0),
%        aphrases(APhrases))

exp_get_pre_tf_in_phrase(Phrase, AAs,
			 Sentences, CoordSentencesIn, CoordSentencesOut,
			 Field, NSent, PreTFInfo) :-
	% extract the mappings component, which is a list of terms of the form
	% map(-888,[
	%  ev(-694,'C0025545','Metallothionein','Metallothionein',
	%      [metallothionein],[aapp,bacs],[[[1,1],[1,1],0]],no,no,Srcs,PI)])
	phrase_info(mappings, Phrase, mappings(AllMappings)),
	phrase_info(phrase, Phrase, phrase(PTextAtom,Syntax,_PosInfo,_ReplacementPos)),
	parse_phrase_word_info(Syntax, filtered, 1, [], PhraseWords0,
			       [], _PhraseHeadWords, [], _PhraseMap0),

	% This just keeps tabs on the original tokens,
	% consuming them as necessary to keep pace with the phrases
	compute_original_phrase(PTextAtom, AAs,
				Sentences, CoordSentencesIn, CoordSentencesOut,
				CoordPhraseTokens, PhraseTextAtom),
	( AllMappings == [] ->
	  Mapping = []
        ; control_option(use_only_first_mapping) ->
	  AllMappings = [map(_,Mapping)|_]  % take the first mapping only
        ; form_super_mapping(AllMappings, Mapping)
	),
	% temp
	% format('***~nPhrase: ~p~nAllMappings: ~p~nMapping: ~p~n',
	%       [Phrase,AllMappings,Mapping]),
	( Mapping == [] ->
	  PreTFInfo = []
	  % extract the phrase component; see above for form of term
	; % format(user_output, 'PHRASE: ~w~n', [PTextAtom]),
	  % PTextAtom = 'Metallothionein-III expression.'
	  atom_codes(PTextAtom, PText),
	  % format('PText: ~p~n',[PText]),
	  % extract the tokens from the Syntax tree

	  % WHY do we try to match up PTextAtom and the tokens?

	  atom_codes_list(PhraseWords0, PhraseWords),
	  % format('PhraseWords: ~p~n',[PhraseWords]),
	  expand_ptext(PText, AAs, ExpandedPText),
	  annotate_text(PhraseWords, ExpandedPText, AnnotatedPText),
	  % format('AnnotatedPText: ~p~n',[AnnotatedPText]),
	  atom_codes(PhraseTextAtom, PhraseTextString), 
	  exp_get_pre_tf_in_mapping(Mapping, AAs, PhraseTextString,
	  			    Sentences, CoordPhraseTokens,
	  			    AnnotatedPText, Field, NSent, PreTFInfo)
	).

expand_ptext(PhraseText0, AAs, ExpandedPhraseText) :-
	trim_and_compress_internal_whitespace(PhraseText0, PhraseText1),	
	tokenize_text_utterly(PhraseText1, TokenizedPhraseText0),
	expand_all_phrase_texts(TokenizedPhraseText0, AAs, TokenizedPhraseText1),
	append(TokenizedPhraseText1, ExpandedPhraseTextWords),
	append(ExpandedPhraseTextWords, ExpandedPhraseText).

% Collect the ev(_) terms from each mapping with the same NegScore as the first map(_) term
form_super_mapping([], []).
form_super_mapping([map(BestScore,EVList)|RestMaps], SuperMapping) :-
	glean_evs([map(BestScore,EVList)|RestMaps], BestScore, Evs),
	append(Evs, TempMappings),
	sort(TempMappings, SuperMapping).

glean_evs([], _, []).
glean_evs([map(ThisScore,Mapping)|Rest], BestScore, EVs) :-
	( ThisScore =:= BestScore ->
	  EVs = [Mapping|GleanedRest],
	  glean_evs(Rest,BestScore,GleanedRest)
	; EVs = []
	).

% PhraseWords is a list of the tokens from the Syntax tree
% Text is the text from the phrase(_) term
% annotate_text/3 identifies the position within PhraseWords of each word in Text
annotate_text(PhraseWords, Text, AnnotatedText) :-
	lower(Text, LCText),
	annotate_text_1(PhraseWords, 1, LCText, LCAnnotatedText),
	unlowercase_annotated_text(LCAnnotatedText, Text, AnnotatedText).

annotate_text_1([], _N, LCText, AnnotatedText) :-
	!,
	( LCText == "" ->
	  AnnotatedText = []
        ; AnnotatedText=[at(0,LCText)]
        ).
annotate_text_1([Word|Rest], N, LCText, AnnotatedText) :-
	( split_string(LCText, Word, Left, NewLCText) ->
	  ( Left == "" ->
	    AnnotatedText = [at(N,Word)|RestAnnotatedText]
	  ; AnnotatedText = [at(0,Left),at(N,Word)|RestAnnotatedText]
          )
        ; NewLCText = LCText,
          AnnotatedText = [at(N,"")|RestAnnotatedText]
        ),
	!,
	M is N + 1,
	annotate_text_1(Rest, M, NewLCText, RestAnnotatedText).
annotate_text_1(PhraseWords, _, LCText, _) :-
	format('ERROR: annotate_text/5 failed to annotate ~p~nPhrase words: ~p~n',
	       [LCText,PhraseWords]),
	fail.

unlowercase_annotated_text([], _, []).
unlowercase_annotated_text([at(N,LCText)|Rest], SourceText,
                           [at(N,Text)|RestUMT]) :-
	length(LCText, Length),
	string_prefix_n(Length, SourceText, Text, NewSourceText),
	unlowercase_annotated_text(Rest, NewSourceText, RestUMT).

get_pre_tf_in_mapping([], _AnnotatedPText, _Field, _NSent, [], []).
get_pre_tf_in_mapping([Candidate|Rest], AnnotatedPText, Field, NSent,
                      [PhraseComponents|RestPhraseComponents],
                      [tf0(ConceptString,STs,TermString,Value,Text-Field-NSent)
		        |RestPreTFInfo]) :-
	Candidate = ev(NegValue, _CUI, MetaTerm, MetaConcept, _MetaWords, STs,
		       MatchMap,_InvolvesHead,_IsOvermatch,_SourceInfo,_PosInfo),
	( mmo_opt_mode(mo) ->
	  Concept=MetaTerm
        ; Concept=MetaConcept
    	),
	Value is -NegValue,
	atom_codes(Concept,ConceptString),
	atom_codes(MetaTerm,TermString),
	extract_components(MatchMap, PhraseComponents0, _MetaComponents),
	merge_contiguous_components(PhraseComponents0, PhraseComponents),
	extract_text_for_phrase(PhraseComponents, AnnotatedPText, _, Text),
	!,
	get_pre_tf_in_mapping(Rest, AnnotatedPText, Field, NSent,
			      RestPhraseComponents, RestPreTFInfo).
get_pre_tf_in_mapping([Candidate|_], AnnotatedPText, _, _, _, _) :-
	Candidate = ev(_NegValue,_CUI,MetaTerm,MetaConcept,_MetaWords,_SemTypes,
		       _MatchMap,_InvolvesHead,_IsOvermatch,_SourceInfo,_PosInfo),
	( mmo_opt_mode(mo) ->
	  Concept=MetaTerm
        ; Concept=MetaConcept
	),
	format('ERROR: get_pre_tf_in_mapping/6 failed for ~p~n', [Concept]),
	wl(AnnotatedPText, 'Annotated phrase text:'),
	fail.

% assemble the tf0(_) term
exp_get_pre_tf_in_mapping([], _AAs,_PhraseTextString,_Sentences, _CoordPhraseTokens,
			  _AnnotatedPText, _Field, _NSent, []).
exp_get_pre_tf_in_mapping([Candidate|Rest], AAs, PhraseTextString,
			  Sentences, CoordPhraseTokens,
			  AnnotatedPText, Field, NSent,
                          [tf0(ConceptString,STs,TermString,Value,
			       Text-Field-NSent)|RestPreTFInfo]) :-
	Candidate = ev(NegValue,_CUI,MetaTerm,MetaConcept,_MetaWords,STs,
		       MatchMap,_InvolvesHead,_IsOvermatch,_SourceInfo,_PosInfo),
	( mmo_opt_mode(mo) ->
	  Concept=MetaTerm
        ; Concept=MetaConcept
        ),
	Value is -NegValue,
	atom_codes(Concept, ConceptString),
	atom_codes(MetaTerm, TermString),
	% Extract the matching text for this candidate
	extract_components(MatchMap, TempPhraseComponents, _MetaComponents),
	sort(TempPhraseComponents, PhraseComponents0),
	% temp
	% format('MatchMap: ~p~n',[MatchMap]),
	% format('PhraseComponents0: ~p~n',[PhraseComponents0]),
	merge_contiguous_components(PhraseComponents0, PhraseComponents),
	% format('PhraseComponents: ~p~n',[PhraseComponents]),
	% extract from AnnotatedPText the words corresponding to the MatchMap indexes
	extract_text_for_phrase(PhraseComponents, AnnotatedPText, _, Text),
	% generate_original_text_if_necessary(TempText, AAs, Sentences, CoordPhraseTokens, Text),
	% format('Text: ~p~n...~n',[Text]),
	!,
	exp_get_pre_tf_in_mapping(Rest, AAs, PhraseTextString, Sentences, CoordPhraseTokens,
				  AnnotatedPText, Field, NSent, RestPreTFInfo).
exp_get_pre_tf_in_mapping([Candidate|_], _AAs, _PhraseTextString,
			  _Sentences, _CoordPhraseTokens, _, _, _, _) :-
	Candidate = ev(_NegValue,_CUI,MetaTerm,MetaConcept,_MetaWords,_SemTypes,
		       _MatchMap,_InvolvesHead,_IsOvermatch,_SourceInfo,_PosInfo),
	( mmo_opt_mode(mo) ->
	  Concept=MetaTerm
        ; Concept=MetaConcept
	),
	format('ERROR: exp_get_pre_tf_in_mapping/5 failed for ~p~n', [Concept]),
	fail.

generate_original_text_if_necessary(TempText, AAs, Sentences, CoordPhraseTokens, OriginalText) :-
	( control_option(display_original_phrases) ->
	  atom_codes(TempTextAtom, TempText),
	  compute_original_phrase_1(TempTextAtom, AAs,
	  			    Sentences, CoordPhraseTokens, _CoordSentencesOut,
				    _TempTextTokens, OriginalTextAtom),
	  atom_codes(OriginalTextAtom, OriginalText)
	; OriginalText = TempText
	).

extract_text_for_phrase([], _, _, "").
extract_text_for_phrase([First|Rest], ATIn, ATOut, Text) :-
	extract_boundaries(First, Left, Right),
	extract_text_for_phrase_1(Left, Right, ATIn, ATInOut, FirstExtractedText),
	!,
	( Rest == [] ->
	  Text = FirstExtractedText
        ; append(FirstExtractedText, " ", S),
          append(S, RestText, Text),
	  extract_text_for_phrase(Rest, ATInOut, ATOut, RestText)
	).
extract_text_for_phrase([First|_], ATIn, _, _) :-
	format('ERROR: extract_text_for_phrase failed at ~p~nfor ~p~n',
		[First,ATIn]),
	fail.

extract_text_for_phrase_1(Left, Right, [at(Left,LeftText)|RestAT], ATOut, Text) :-
	!,
	( Right == Left ->
	  ATOut = RestAT,
	  Text = LeftText
        ; append(LeftText, RestText, Text),
          extract_rest_text_for_phrase(Right, RestAT, ATOut, RestText)
        ).
extract_text_for_phrase_1(Left, Right, [_|RestAT], ATOut, Text) :-
	extract_text_for_phrase_1(Left, Right, RestAT, ATOut, Text).

extract_rest_text_for_phrase(Right, [at(Right,RightText)|RestAT], RestAT, RightText) :- !.
extract_rest_text_for_phrase(Right, [at(_,MidText)|RestAT], ATOut, Text) :-
	append(MidText, RestText, Text),
	extract_rest_text_for_phrase(Right, RestAT, ATOut, RestText).

mark_text(AnnotatedText,PhraseComponents,MarkedText) :-
    compute_phrase_markers(PhraseComponents,PhraseMarkers0),
    sort(PhraseMarkers0,PhraseMarkers),
    insert_markers(AnnotatedText,PhraseMarkers,MarkedText).

compute_phrase_markers([],[]).
compute_phrase_markers([PhraseComponent|Rest],PhraseMarkers) :-
    compute_markers(PhraseComponent,FirstPhraseMarkers),
    append(FirstPhraseMarkers,RestPhraseMarkers,PhraseMarkers),
    compute_phrase_markers(Rest,RestPhraseMarkers).

compute_markers([],[]).
compute_markers([Singleton],
                [pm(Left,left,"\fB{"),pm(Right,right,"}\fP")]) :-
    !,
    extract_boundaries(Singleton,Left,Right).
compute_markers([First|Rest],
                 [pm(Left,left,"\fB{"),pm(Right,right,"]\fP")|RestMarkers]) :-
    extract_boundaries(First,Left,Right),
    compute_rest_markers(Rest,RestMarkers).

compute_rest_markers([],[]).
compute_rest_markers([Singleton],
                     [pm(Left,left,"\fB["),pm(Right,right,"}\fP")]) :-
    !,
    extract_boundaries(Singleton,Left,Right).
compute_rest_markers([First|Rest],
                     [pm(Left,left,"\fB["),pm(Right,right,"]\fP")
                     |RestMarkers]) :-
    extract_boundaries(First,Left,Right),
    compute_rest_markers(Rest,RestMarkers).

extract_boundaries([Left,Right], Left, Right) :- !.
extract_boundaries([94,Left0,94,Right0], Left, Right) :-
	% This clause is for the abomination in which [9,9]
	% somehow becomes "^I^I" (a list of four! characters)
	!,
	Left is Left0 - 64,
	Right is Right0 - 64.
extract_boundaries(Interval, _, _) :-
	format('ERROR: extract_boundaries/3 failed for ~p~n', [Interval]),
	fail.
    
insert_markers([],[],"").
insert_markers([],[pm(_,_,PMText)|RestPM],Text) :-
    !,
    append(PMText,RestText,Text),
    insert_markers([],RestPM,RestText).
insert_markers([at(N,ATText)|RestAT],[pm(N,left,PMText)|RestPM],Text) :-
    !,
    append(PMText,RestText,Text),
    insert_markers([at(N,ATText)|RestAT],RestPM,RestText).
insert_markers([at(N,ATText)|RestAT],[pm(N,right,PMText)|RestPM],Text) :-
    !,
    append([ATText,PMText,RestText],Text),
    insert_markers(RestAT,RestPM,RestText).
insert_markers([at(0,ATText)|RestAT],PM,Text) :-
    !,
    append(ATText,RestText,Text),
    insert_markers(RestAT,PM,RestText).
insert_markers([at(_N,ATText)|RestAT],PM,Text) :-
    append(ATText,RestText,Text),
    insert_markers(RestAT,PM,RestText).

compute_mesh_and_tcs(MetaConcept,Concept,TreeCodes) :-
    (control_option(strict_mesh_tc) ->
        db_get_meta_mesh_tc(MetaConcept,Concept0,TreeCodes),
        (Concept0=="" ->
            Concept=MetaConcept
        ;   Concept=Concept0
        )
    ;   (db_get_meta_mesh(MetaConcept,Concept) ->
            db_get_mesh_tc_relaxed(Concept,TreeCodes)
        ;   Concept0=MetaConcept,
            (db_get_mesh_mh(Concept0,Concept) ->
                true
            ;   Concept=Concept0
            ),
            db_get_mesh_tc_relaxed(Concept,TreeCodes)
        )
    ).

exp_compute_mesh_in_textfields([],[]).
exp_compute_mesh_in_textfields([tf0(MetaConcept,STs,Term1,Value1,
				    Text1-Field1-NSent1,_),
                                tf0(MetaConcept,STs,Term2,Value2,
				    Text2-Field2-NSent2,_)
			       |Rest],
                               [tf(Concept,STs,Terms,TitleFlag,FrequencyCount,
				   Texts,Value,TreeCodes)|ComputedRest]) :-
    !,
    exp_accumulate_rest_fields(MetaConcept,Rest,NewRest,RestTerms,RestValues,
			       RestTexts,RestFields),
    Terms0=[Term1-Field1-NSent1-Text1,Term2-Field2-NSent2-Text2|RestTerms],
    sort(Terms0,Terms),
    Values=[Value1,Value2|RestValues],
    compute_average(Values,Value),
    Texts0=[Text1,Text2|RestTexts],
    sort(Texts0,Texts),
    Fields=[Field1,Field2|RestFields],
    length(Fields,FrequencyCount),
    max_freq(MF0),
    (FrequencyCount > MF0 ->
% Don't display
%        format(user_output,'Resetting maximum frequency to ~d for ~p~n',
%               [FrequencyCount,MetaConcept]),
        retract(max_freq(_)),
        assert(max_freq(FrequencyCount))
    ;   true
    ),
    (member(ti,Fields) ->
        TitleFlag=yes
    ;   TitleFlag=no
    ),
    compute_mesh_and_tcs(MetaConcept,MeshConcept,TreeCodes),
    (control_option(restrict_to_mesh) ->
        Concept=MeshConcept
    ;   Concept=MetaConcept
    ),
    exp_compute_mesh_in_textfields(NewRest,ComputedRest).
exp_compute_mesh_in_textfields([tf0(MetaConcept,STs,Term,Value,
				    Text-Field-NSent,_)|Rest],
                               [tf(Concept,STs,[Term-Field-NSent-Text],
				   TitleFlag,1,[Text],Value,TreeCodes)|
			       ComputedRest]) :-
    (Field==ti ->
        TitleFlag=yes
    ;   TitleFlag=no
    ),
    compute_mesh_and_tcs(MetaConcept,MeshConcept,TreeCodes),
    (control_option(restrict_to_mesh) ->
        Concept=MeshConcept
    ;   Concept=MetaConcept
    ),
    exp_compute_mesh_in_textfields(Rest,ComputedRest).

compute_average(Values,Value) :-
    aggregate(sum(Val),member(Val,Values),Sum),
    length(Values,N),
    Value is Sum / N.

exp_accumulate_rest_fields(_MetaConcept,[],[],[],[],[],[]).
exp_accumulate_rest_fields(MetaConcept,
                           [tf0(MetaConcept,_STs,Term,Value,Text-Field-NSent,_)|
			   Rest],
                           NewRest,[Term-Field-NSent-Text|RestTerms],
			   [Value|RestValues],
			   [Text|RestTexts],[Field|RestFields]) :-
    !,
    exp_accumulate_rest_fields(MetaConcept,Rest,NewRest,RestTerms,RestValues,
			       RestTexts,RestFields).
exp_accumulate_rest_fields(_MetaConcept,Rest,Rest,[],[],[],[]).

exp_augment_tf(TFInfoIn,TFInfoOut) :-
    exp_augment_tf(TFInfoIn,1,TFInfoOut).

exp_augment_tf([],_,[]).
exp_augment_tf([tf(Concept,STs,Terms,TitleFlag,FrequencyCount,Texts,Value,
		   TreeCodes)|Rest],N,
               [tf(Concept,STs,Terms,TitleFlag,FrequencyCount,Texts,Value,
		   TreeCodes,N)
               |AugmentedRest]) :-
    M is N + 1,
    exp_augment_tf(Rest,M,AugmentedRest).

exp_collapse_tf([],[]).
exp_collapse_tf([tf(Concept,STs,Terms1,TF1,Freq1,Texts1,Value1,TC1,_),
                 tf(Concept,STs,Terms2,TF2,Freq2,Texts2,Value2,_TC2,_)|Rest],
                [tf(Concept,STs,Terms,TitleFlag,FrequencyCount,Texts,Value,TC1)
                |ComputedRest]) :-
    !,
    exp_accumulate_rest_tfvs(Concept,Rest,NewRest,RestTerms,RestTFs,
			     RestFreqs,RestTexts,RestValues),
    Terms0=[Terms1,Terms2|RestTerms],
    append(Terms0,Terms),
    TFs=[TF1,TF2|RestTFs],
    (member(yes,TFs) ->
        TitleFlag=yes
    ;   TitleFlag=no
    ),
    Freqs=[Freq1,Freq2|RestFreqs],
    Texts0=[Texts1,Texts2|RestTexts],
    append(Texts0,Texts),
    Values=[Value1,Value2|RestValues],
    compute_weighted_value(Freqs,Values,Value),
% temp
%format('~p ** ~p --> ~p~n',[Freqs,Values,Value]),
    aggregate(sum(Freq),member(Freq,Freqs),FrequencyCount),
    exp_collapse_tf(NewRest,ComputedRest).
exp_collapse_tf([tf(Concept,STs,Terms,TF,Freq,Texts,Value,TC,_)|Rest],
                [tf(Concept,STs,Terms,TF,Freq,Texts,Value,TC)|ComputedRest]) :-
    exp_collapse_tf(Rest,ComputedRest).

exp_accumulate_rest_tfvs(_Concept,[],[],[],[],[],[],[]).
exp_accumulate_rest_tfvs(Concept,
			 [tf(Concept,_STs,Terms,TF,Freq,Texts,Value,_TC)|Rest],
                         NewRest,[Terms|RestTerms],[TF|RestTFs], % watch out for STs!
			 [Freq|RestFreqs],[Texts|RestTexts],
			 [Value|RestValues]) :-
    !,
    exp_accumulate_rest_tfvs(Concept,Rest,NewRest,RestTerms,RestTFs,RestFreqs,
			     RestTexts,RestValues).
exp_accumulate_rest_tfvs(_Concept,Rest,Rest,[],[],[],[],[]).

compute_weighted_value(Freqs,Values,Value) :-
    compute_prods(Freqs,Values,FVs),
    aggregate(sum(FV),member(FV,FVs),Sum),
    aggregate(sum(Freq),member(Freq,Freqs),N),
    Value is Sum/N.

compute_prods([],[],[]).
compute_prods([FirstL|RestL],[FirstR|RestR],[Prod|RestProds]) :-
    Prod is FirstL * FirstR,
    compute_prods(RestL,RestR,RestProds).


preprocess_tf(TFInfo,ATFInfo) :-
    processing_parameters(nf,NF),
    processing_parameters(nmm,NMM),
    processing_parameters(nm,NM),
    processing_parameters(nw,NW),
    processing_parameters(nc,NC),
    processing_parameters(wmm,WMM),
    processing_parameters(wm,WM),
    processing_parameters(ww,WW),
    processing_parameters(wc,WC),
    processing_parameters(wd,WD),
    preprocess_tf(TFInfo,NF,NMM,NM,NW,NC,WMM,WM,WW,WC,WD,ATFInfo).

preprocess_tf([],_,_,_,_,_,_,_,_,_,_,[]).
preprocess_tf([First|Rest],NF,NMM,NM,NW,NC,WMM,WM,WW,WC,WD,
              [atf(NFreq,Spec,First)|PreprocessedRest]) :-
    First=tf(Concept,_STs,_Terms,_TitleFlag,FrequencyCount,_Texts,MMValue,
	     TreeCodes),
    (max_freq(MF) ->
        Freq is FrequencyCount / MF
    ;   Freq is FrequencyCount / 10
    ),
    normalize_value(NF,Freq,NFreq),
    compute_specificities(Concept,MMValue,TreeCodes,WD,NMM,NM,NW,NC,
                          NMMSpec,NMSpec,NWSpec,NCSpec),
    compute_weighted_value([WMM,WM,WW,WC],[NMMSpec,NMSpec,NWSpec,NCSpec],Spec),
%format('~p ~p ~p~n',[NFreq,Spec,First]),
    preprocess_tf(Rest,NF,NMM,NM,NW,NC,WMM,WM,WW,WC,WD,PreprocessedRest).

normalize_value(0, Value, NValue) :-
	!,
	( Value > 1 ->
	  NValue = 1
	; Value < 0 ->
	  NValue = 0
	; NValue = Value
	).
normalize_value(N, Value, NValue) :-
	N > 0,
	!,
	( Value > 1 ->
	  Value1 = 1
	; Value < 0 ->
	  Value1 = 0
	; Value1 = Value
	),
	EN is exp(N),
	A is EN + 1,
	B is EN - 1,
	C is -N * Value1,
	EC is exp(C),
	NValue is (A/B) * (1 - EC)/(1 + EC).
normalize_value(N, Value, NValue) :-
	N < 0,
	!,
	( Value > 1 ->
	  Value1 = 1
	; Value < 0 ->
	  Value1 =0
	; Value1 = Value
	),
	M is -N,
	EM is exp(M),
	A is EM + 1,
	B is EM - 1,
	C is (A + B*Value1) / (A - B*Value1),
	LC is log(C),
	NValue is LC / M.

compute_specificities(Concept, MMValue, TreeCodes,
		      WD, NMM, NM, NW, NC,
                      NMMSpec, NMSpec, NWSpec, NCSpec) :-
	% MetaMap specificity
	MMSpec is MMValue / 1000,
	normalize_value(NMM, MMSpec, NMMSpec),
	% MeSH tree depth specificity
	compute_tree_depth_specificity(Concept, TreeCodes, WD, MValue),
	MSpec is MValue / 9,
	normalize_value(NM, MSpec, NMSpec),
	% Word specificity
	tokenize_text_more(Concept, ConceptWords),
	length(ConceptWords, WValue),
	WSpec is WValue / 26,
	normalize_value(NW, WSpec, NWSpec),
	% Character specificity
	length(Concept, CValue),
	CSpec is CValue / 102,
	normalize_value(NC, CSpec, NCSpec).

compute_tree_depth_specificity(_Concept,TreeCodes,WD,MValue) :-
    (TreeCodes==[] ->
	MValue=WD
    ;   compute_tree_depths(TreeCodes,TreeDepths),
%% try both average and max--max is better (see runs a, b, and c)
%%        compute_average(TreeDepths,MValue)
        aggregate(max(TD),member(TD,TreeDepths),MValue)
    ).

compute_tree_depths([],[]).
compute_tree_depths([First|Rest],[FirstDepth|RestDepths]) :-
    split_string_completely(First,".",SplitFirst),
    length(SplitFirst,FirstDepth),
    compute_tree_depths(Rest,RestDepths).

% Non-determinate predicate
% Product method
rank_tf(ATFInfo,AATFInfo,product) :-
    processing_parameters(nz,NZ),
    compute_product_rankings(ATFInfo,NZ,AATFInfo0),
    sort(AATFInfo0,AATFInfo).
% Percentage method
%%rank_tf(ATFInfo,AATFInfo,percentage) :-
%%    processing_parameters(nz,NZ),
%%    sort(AATFInfo0,AATFInfo).

compute_product_rankings([],_,[]).
compute_product_rankings([atf(NFreq,Spec,TF)|Rest],NZ,
                         [aatf(NegNRank,NFreq,Spec,TF)|ComputedRest]) :-
    TF=tf(_,_,_,TitleFlag,_,_,_,_),
    (TitleFlag==yes ->
        Rank is Spec
    ;   Rank is NFreq * Spec
    ),
    normalize_value(NZ,Rank,NRank),
    NegNRank is -NRank,
% temp
%format('~p ~p ~p ~p~n',[NegNRank,NFreq,Spec,TF]),
    compute_product_rankings(Rest,NZ,ComputedRest).

dump_aatf_info(_, _, _, _, none) :- !.
dump_aatf_info([], _, _, _, Stream) :-
	!,
	format(Stream, '<none>~n', []).
dump_aatf_info(AATFInfo, MHInfo, MTFInfo, UIAtom, Stream) :-
	dump_aatf_info_aux(AATFInfo, MHInfo, MTFInfo, UIAtom, Stream).

dump_aatf_info_aux([], _, _, _, _).
dump_aatf_info_aux([aatf(NegRank,_,_,tf(Concept,_,_,_,_,_,_,TreeCodes))|Rest],
		   MHInfo, MTFInfo, UIAtom, Stream) :-
    compute_relevance_flag(MTFInfo,Concept,RelFlag),
    compute_main_flag(MHInfo,Concept,MainFlag),
    ScaledRank is -100.0 * NegRank,
    format(Stream,'  ~s~t~1f~8| ~s~s',[RelFlag,ScaledRank,MainFlag,Concept]),
    ( \+ control_option(hide_semantic_types) ->
      db_get_concept_sts(Concept,STs0),
      atom_codes_list(STs1,STs0),
      expand_semtypes(STs1,STs),
      format(Stream,'  ~p',[STs])
    ; true
    ),
    (control_option(include_treecodes) ->
	format(Stream,'  ',[]),
        dump_treecodes(TreeCodes,Stream)
    ;   true
    ),
    format(Stream,'~n',[]),
    dump_aatf_info_aux(Rest,MHInfo,MTFInfo,UIAtom,Stream).

compute_relevance_flag([],_MH," ").
compute_relevance_flag([mtf(Relevance,_,MH)|_Rest],MH,Rel) :-
    !,
    (Relevance==yes ->
        Rel="!"
    ;   Rel=" "
    ).
compute_relevance_flag([_First|Rest],MH,Rel) :-
    compute_relevance_flag(Rest,MH,Rel).

compute_main_flag([],_MH," ").
compute_main_flag([mh(MH,MNM,_)|_Rest],MH,Main) :-
    !,
    (MNM==main ->
        Main="*"
    ;   Main=" "
    ).
compute_main_flag([_First|Rest],MH,Main) :-
    compute_main_flag(Rest,MH,Main).


dump_treecodes([],_) :-
    !.
dump_treecodes([Singleton],Stream) :-
    format(Stream,'~s',[Singleton]),
    !.
dump_treecodes([First|Rest],Stream) :-
    format(Stream,'~s,',[First]),
    dump_treecodes(Rest,Stream).


% dump_aatf_info_fielded(_,_,_,_,none) :-
%     !.
dump_aatf_info_fielded(AATFInfo, MHInfo, MTFInfo, UIAtom, MMOutput, Stream) :-
    retract(prev_n(PrevN)),
    N is PrevN + 1,
    assert(prev_n(N)),
    dump_aatf_info_fielded_aux(AATFInfo, MHInfo, MTFInfo, UIAtom, N, MMOutput, Stream).

% temp clause
%dump_aatf_info_fielded([First|_],_MHInfo,_MTFInfo,UIAtom,N,Stream) :-
%    format(Stream,'~nDump of ~d: ~p~nAATF: ~p~n',[N,UIAtom,First]),
%    fail.
dump_aatf_info_fielded_aux([], _MHInfo, _MTFInfo, UIAtom, N, _MMOutput, Stream) :-
    (control_option(include_treecodes) ->
        format(Stream,'~a|~d|c|0.000000|EOC|~n',[UIAtom,N])
    ;   format(Stream,'~a|~d|c|0.000000|EOC~n',[UIAtom,N])
    ).
dump_aatf_info_fielded_aux([aatf(NegRank,_,_,
			     tf(Concept,_STs,Terms,TitleFlag,_,_,_,TreeCodes))
		       |Rest], _MHInfo, _MTFInfo, UIAtom, N, MMOutput, Stream) :-
    Rank is -1.0 * NegRank,
    (TitleFlag==yes ->
	TIorAB='TI'
    ;   TIorAB='AB'
    ),
    (control_option(include_treecodes) ->
        format(Stream,'~a|~d|c|~f|~s|~p|~a|',[UIAtom,N,Rank,Concept,Terms,
					      TIorAB]),
        dump_treecodes(TreeCodes,Stream),
        format(Stream,'~n',[])
    ;   format(Stream,'~a|~d|c|~f|~s|~p|~a~n',[UIAtom,N,Rank,Concept,Terms,
					       TIorAB])
    ),
    dump_aatf_info_fielded_aux(Rest, _MHInfo, _MTFInfo, UIAtom, N, MMOutput, Stream).


% dump_aatf_info_fielded2(_,_,_,_,none) :-
%     !.
dump_aatf_info_fielded2([], _CitationTextAtom, _MHInfo, _MTFInfo, _UIAtom, _MMOutput,_Stream) :- !.
% temp
%dump_aatf_info_fielded2([First|_],MHInfo,MTFInfo,UIAtom,Stream) :-
%    format(Stream,'~ndaif2~nAATF: ~p~nMHInfo: ~p~nMTFInfo: ~p~nUIAtom: ~p~n',
%	   [First,MHInfo,MTFInfo,UIAtom]),
%    fail.
dump_aatf_info_fielded2([aatf(NegRank,_,_,
			      tf(Concept,STs,Terms,_TitleFlag,_,_,_,_))|Rest],
			CitationTextAtom, _MHInfo, _MTFInfo, UIAtom, MMOutput, Stream) :-
	ScaledRank is -1000.0 * NegRank,
	db_get_concept_cui(Concept,CUI),
	% Compute TIorAB from the fields in Terms
	%    (TitleFlag==yes ->
	%	TIorAB='TI'
	%    ;   TIorAB='AB'
	%    ),
	( extract_fields(Terms,Fields0),
	  sort(Fields0,Fields),
	  form_fields_atom(Fields,FieldsAtom) ->
	  true
	; FieldsAtom='unknown'
	),
	% format(Stream,'Terms: ~p~nTitleflag: ~p~n',[Terms,TitleFlag]),
	( \+ control_option(hide_semantic_types) ->
	  format(Stream, '~a|MM|~0f|~s|~s|~p|~p|~a',
	                 [UIAtom,ScaledRank,Concept,CUI,STs,Terms,FieldsAtom])
	; format(Stream, '~a|MM|~0f|~s|~s|~p|~a',
		 	 [UIAtom,ScaledRank,Concept,CUI,Terms,FieldsAtom])
	),
	% generate the positional information for all terms in the list that looks like
	% ["Sepsis"-ab-1-"sepsis",
    	%  "Sepsis"-ab-2-"sepsis",
    	%  "Sepsis"-ti-1-"sepsis"]
	get_mmi_pos_info(Terms, CitationTextAtom, Stream, UIAtom, MMOutput, PosInfo),
	% and print out the positional information
	print_mmi_pos_info(PosInfo, Stream),
	dump_aatf_info_fielded2(Rest, CitationTextAtom, _MHInfo, _MTFInfo, UIAtom, MMOutput, Stream).

print_mmi_pos_info([], Stream) :- nl(Stream).
print_mmi_pos_info([FirstPosInfo|RestPosInfo], Stream) :-
	( FirstPosInfo == [] ->
	  true
	; FirstPosInfo = [StartPos/Length|RestPairs],
	  format(Stream, '|~d:~d', [StartPos, Length]),
	  print_rest_mmi_info_list(RestPairs, Stream)
	),
	print_mmi_pos_info(RestPosInfo, Stream).

print_rest_mmi_info_list([], _Stream).
print_rest_mmi_info_list([StartPos/Length|T], Stream) :-
	print_rest_mmi_info_list_aux(T, StartPos, Length, Stream).


print_rest_mmi_info_list_aux([], StartPos, Length, Stream) :-
	format(Stream, ',~d:~d', [StartPos, Length]).
print_rest_mmi_info_list_aux([NextStartPos/NextLength|RestPosInfo], ThisStartPos, ThisLength, Stream) :-
	format(Stream, ',~d:~d', [ThisStartPos,ThisLength]),
	print_rest_mmi_info_list_aux(RestPosInfo, NextStartPos, NextLength, Stream).

% Each element of the list in the first argument will be a term of the form
% ConceptName-FieldType-UtteranceNum-TextString
% e.g., "Sepsis"-ab-2-"sepsis"
% Each A-B-C-D term will have the same "A", i.e., the same concept.
% Get the positional information for all these terms, or for the concept
% that is the first component of each of the A-B-C-D terms in the list.
get_mmi_pos_info([], _CitationTextAtom, _Stream, _UIAtom, _MMOutput, []).
get_mmi_pos_info([H|T], CitationTextAtom, Stream, UIAtom, MMOutput, [PosInfoH|PosInfoT]) :-
	% H = Concept-TiOrAb-UttNum-Text,
	% format(user_output, '~n### ~s-~w-~w-~s~n', [Concept,TiOrAb,UttNum,Text]),
	% ttyflush,
	get_one_mmi_pos_info(H, CitationTextAtom, Stream, UIAtom, MMOutput, TempPosInfoH),
	% format(user_output, '~N### ~w~n', [TempPosInfoH]),
	% ttyflush,
	sort(TempPosInfoH, PosInfoH),
	get_mmi_pos_info(T, CitationTextAtom, Stream, UIAtom, MMOutput, PosInfoT).

% Get the positional information for one term of the form
% "Sepsis"-ab-2-"sepsis", i.e., for the concept "Sepsis" in utterance ab.2.
get_one_mmi_pos_info(ConceptString-FieldType-UtteranceNum-TextString,
		     CitationTextAtom,
		     _Stream, UIAtom, MMOutput, PosInfo) :-
	% First, create an atom of the form PMID.FieldType.UtteranceNum,
	% e.g., 7583965.ab.3
	concat_atoms([UIAtom,'.',FieldType,'.',UtteranceNum], UtteranceID),
	% MMOutput is a list of terms (one per utterance) of the form
	% mm_output(UtteranceID, CitationTextAtom, AAs, ModifiedText, Tagging,
        %   	    Syntax, DisambiguatedMMOPhrases, ExtractedPhrases)
	% in which UtteranceID is the atom created immediately above,
	% so calling memberchk/2 will retrieve the mm_output for the utterance
	% that was constructed by the call to concat_atom/2 above.
	memberchk(mm_output(utterance(UtteranceID,_UtteranceText,_PosInfo,_ReplPos),
			    _CitationTextAtom,_ModifiedText,_Tagging,AAs,_Syntax,
			    Phrases,_ExtractedPhrases),
		  MMOutput),
	atom_codes(ConceptAtom, ConceptString),
	atom_codes(TextAtom, TextString),
	% Now find the pos info for every occurrence of ConceptAtom in Phrases.
	get_concept_pos_info(Phrases, TextAtom, CitationTextAtom, AAs, ConceptAtom, PosInfo, []).

% Each element in the list in the first argument will be a term of the form
% phrase(phrase(PhraseTextAtom0,Phrase,StartPos/Length,ReplacementPos),
% 	 candidates(Evaluations),
%        mappings(Mappings),
%        pwi(PhraseWordInfo),
%        gvcs(GVCs),
%        ev0(Evaluations3),
%        aphrases(APhrases))
% We are interested in the Evaluations term, which is
% the first arg of the second arg of the phrase/7 term.
get_concept_pos_info([], _TextAtom, _CitationTextAtom, _AAs, _Concept, PosInfo, PosInfo).
get_concept_pos_info([FirstPhrase|RestPhrases],
		     TextAtom, CitationTextAtom, AAs,
		     Concept, PosInfoIn, PosInfoOut) :-
	arg(2, FirstPhrase, candidates(Evaluations)),
	get_concept_candidates_pos_info(Evaluations, TextAtom, CitationTextAtom,
					AAs, Concept, PosInfoIn, PosInfoNext),
	get_concept_pos_info(RestPhrases, TextAtom, CitationTextAtom,
			     AAs, Concept, PosInfoNext, PosInfoOut).

% Each element in the list in the first argument will be an Eval term of the form
% ev(_NegVal,_CUI,Concept,_,_,_,_,_,_,SourceInfo,PosInfoList)
% We have the concept name from the ConceptName-FieldType-UtteranceNum-TextString
% term several layers above. Now we need to find all the ev/10 terms
% whose third arg matches the concept name.
get_concept_candidates_pos_info([], _TextAtom, _CitationTextAtom, _AAs, _Concept, PosInfo, PosInfo).
get_concept_candidates_pos_info([FirstCandidate|RestCandidates], TextAtom,
				CitationTextAtom, AAs, Concept, PosInfoIn, PosInfoOut) :-
	get_concept_pos_info_from_one_candidate(FirstCandidate, TextAtom, CitationTextAtom,
						AAs, Concept, PosInfoIn, PosInfoNext),
	get_concept_candidates_pos_info(RestCandidates, TextAtom, CitationTextAtom,
					AAs, Concept, PosInfoNext, PosInfoOut).

% transform Atom by removing all non-alphanumeric chars
replace_non_alnum_with_ws(Atom, AlnumAtom) :-
	atom_codes(Atom, Chars),
	replace_non_alnum_with_ws_1(Chars, AlnumChars),
	atom_codes(AlnumAtom, AlnumChars).

replace_non_alnum_with_ws_1([], []).
replace_non_alnum_with_ws_1([H|T], AlnumChars) :-
	( is_alnum(H) ->
	  AlnumChars = [H|AlnumCharsT]
	; is_white(H) ->
	  AlnumChars = [H|AlnumCharsT]
	; AlnumChars = [32|AlnumCharsT]
	),
	replace_non_alnum_with_ws_1(T, AlnumCharsT).

matching_text(EvTextAtom0, TextAtom0, AAs) :-
	replace_non_alnum_with_ws(EvTextAtom0, EvTextAtom),
	replace_non_alnum_with_ws(TextAtom0, TextAtom),
	% Is TextAtom a prefix of EvTextAtom?
	( index(TextAtom, EvTextAtom, 0) ->
	  true
	; avl_member(AA, AAs, [Expansion]),
	  get_token_text(AA, AAText),
	  AAText == EvTextAtom,
	  get_token_text(Expansion, ExpansionText0),
	  replace_non_alnum_with_ws(ExpansionText0, ExpansionText),
	  % Can't require that TextAtom == ExpansionText, because of cases such as
	  % "The MacGill Pain Questionnaire (MPQ). The MPQ.",
	  % in which "questionnaire" is the TextAtom.
	  substring(ExpansionText, TextAtom, _, _) ->
	  true
	; tokenize_text(TextAtom, TokenizedTextAtom),
	  tokenize_text(EvTextAtom, EvTokenizedTextAtom),
	  expand_text(EvTokenizedTextAtom, AAs, ExpandedEvTextAtom),
	  append(ExpandedEvTextAtom, WordList),
	  subseq0(WordList, TokenizedTextAtom) ->
	  true
	).


expand_text([], _AAs, []).
expand_text([FirstAtom|RestAtoms], AAs, [FirstExpanded|RestExpanded]) :-
	expand_one_atom(FirstAtom, AAs, FirstExpanded),
	expand_text(RestAtoms, AAs, RestExpanded).

expand_one_atom(Atom, AAs, ExpandedAtom) :-
	( avl_member(AA, AAs, [Expansion]),
	  get_token_text(AA, AAText),
	  AAText == Atom ->
	  extract_token_strings(Expansion, Strings),
	  atom_codes_list(ExpandedAtom, Strings)
	; ExpandedAtom = [Atom]
	).

% This particular ev/10 term has the concept that we're looking for,
% so add the StartPos/Length for that concept to the PosInfo list.
get_concept_pos_info_from_one_candidate(FirstCandidate, TextAtom, CitationTextAtom,
					AAs, Concept, [CandidatePosInfo|Rest], Rest) :-
	
	FirstCandidate = ev(_NegVal,_CUI,EVConcept,_,_,_,_,_,_,_SourceInfo,PosInfoList),
	Concept == EVConcept,
	get_ev_text(PosInfoList, CitationTextAtom, EvTextAtom),
	matching_text(EvTextAtom, TextAtom, AAs),
	!,
	sort(PosInfoList, SortedPosInfoList),
	collapse_candidate_pos_info(SortedPosInfoList, CandidatePosInfo).
	% length(PosInfoList, PosInfoListLength),
	% ( PosInfoListLength > 1 ->
	%   format(user_output, '#### FirstCandidate: ~w~n', [FirstCandidate]),
	%   format(user_output, '#### Concept: ~w~n', [Concept]),
	%   format(user_output, '#### PosInfoList: ~w~n', [PosInfoList])
	% ; true
	% ).
% This particular ev/10 term does NOT have the concept that we're looking for,
% so do NOT add the StartPos/Length for that concept to the PosInfo list.
get_concept_pos_info_from_one_candidate(_FirstCandidate, _TextAtom, _CitationTextAtom,
					_AAs, _Concept, PosInfo, PosInfo).

get_ev_text(PosInfoList, CitationTextAtom, EvTextAtom) :-
	sort(PosInfoList, SortedPosInfoList),
	get_ev_atoms(SortedPosInfoList, CitationTextAtom, EvAtoms),
	concat_atoms_with_separator(EvAtoms, ' ', EvTextAtom).

get_ev_atoms([], _CitationTextAtom, []).
get_ev_atoms([Pos1/Length1|RestPosInfo], CitationTextAtom, [Atom|RestAtoms]) :-
	substring(CitationTextAtom, Atom, Pos1, Length1),
	get_ev_atoms(RestPosInfo, CitationTextAtom, RestAtoms).


% The ev/10 term's positional information will contain multiple StartPos/Length terms
% if the MatchMap term contains multiple lists. If there are multiple StartPos/Length terms,
% we need to collapse them into one StartPos/Length term.
% e.g., [12/4,17/6]    should be collapsed to 12/11;
%       [490/11,515/4] should be collapsed to 490/29. In general,
% [S1/L1, S2/L2, ... Sn/Ln] should be collapsed to S1/Sn-S1+Ln

collapse_candidate_pos_info(PosInfoList, CandidatePosInfo) :-
	PosInfoList = [FirstStartPos/_FirstLength|_],
	% reversed order of args from QP library version!
	last(PosInfoList, LastStartPos/LastLength),
	CandidateLength is LastStartPos - FirstStartPos + LastLength,
	CandidatePosInfo = FirstStartPos/CandidateLength.

extract_fields([],[]).
extract_fields([_-Field-_-_|Rest],[Field|ExtractedRest]) :-
    extract_fields(Rest,ExtractedRest).

form_fields_atom(Fields0,FieldsAtom) :-
    augment_fields(Fields0,AugFields0),
    sort(AugFields0,AugFields1),
    deaugment_fields(AugFields1,Fields),
    form_fields_atom_aux(Fields,FieldsAtom).

form_fields_atom_aux(Fields,FieldsAtom) :-
    atom_codes_list(Fields,FieldsStrings0),
    concatenate_strings(FieldsStrings0,";",FieldsString),
    atom_codes(FieldsAtom,FieldsString).

augment_fields([],[]) :-
    !.
augment_fields([Field|Rest],[N-UCField|AugmentedRest]) :-
    fieldn(Field,UCField,N),
    !,
    augment_fields(Rest,AugmentedRest).

fieldn('ti','TI',1).
fieldn('ab','AB',2).
fieldn(Field,UCField,3) :-
    upper(Field,UCField).

deaugment_fields([],[]) :-
    !.
deaugment_fields([_-Field|Rest],[Field|DeaugmentedRest]) :-
    deaugment_fields(Rest,DeaugmentedRest).


extract_mhs([],[]).
extract_mhs([mh(MH,_,_)|Rest],[MH|ExtractedRest]) :-
    extract_mhs(Rest,ExtractedRest).

mark_tf([],_,[]).
mark_tf([aatf(NegRank,_NFreq,_Spec,tf(MH,_,_,_,_,_,_,_))|Rest],MHs,
        [mtf(Relevance,Rank,MH)|MarkedRest]) :-
    (memberchk(MH,MHs) ->
        Relevance=yes
    ;   Relevance=no
    ),
    Rank is -NegRank,
    mark_tf(Rest,MHs,MarkedRest).

compute_rp_pairs(MTFInfo,TotRel,RPPairs) :-
%    compute_total_relevant(MTFInfo,TotRel),
    (TotRel > 0 ->
        compute_rp_pairs(MTFInfo,0,0,TotRel,RPPairs)
    ;   RPPairs=[rp(0,0)]
    ).

% This is wrong; total relevant is just the number of MHs
%compute_total_relevant(MTFInfo,TotRel) :-
%    compute_total_relevant(MTFInfo,0,TotRel).
%
%compute_total_relevant([],RelIn,RelIn).
%compute_total_relevant([mtf(Relevance,_,_)|Rest],RelIn,RelOut) :-
%    (Relevance==yes ->
%        RelInOut is RelIn + 1
%    ;   RelInOut=RelIn
%    ),
%    compute_total_relevant(Rest,RelInOut,RelOut).

compute_rp_pairs([],_,_,_,[]).
compute_rp_pairs([mtf(Relevance,_,_)|Rest],N0,NRel0,TotRel,
                 [rp(Recall,Precision)|RestPairs]) :-
    N is N0 + 1,
    (Relevance==yes ->
        NRel is NRel0 + 1
    ;   NRel=NRel0
    ),
    Recall is NRel / TotRel,
    Precision is NRel / N,
    compute_rp_pairs(Rest,N,NRel,TotRel,RestPairs).

conflate_rp_pairs([],[]) :-
    !.
conflate_rp_pairs([rp(Recall,Precision1),rp(Recall,Precision2)|Rest],
                  [rp(Recall,Precision)|ConflatedRest]) :-
    !,
    accumulate_rest_rp(Recall,Rest,NewRest,RestPrecision),
    Precisions=[Precision1,Precision2|RestPrecision],
    compute_average(Precisions,Precision),
    conflate_rp_pairs(NewRest,ConflatedRest).
conflate_rp_pairs([First|Rest],[First|ConflatedRest]) :-
    conflate_rp_pairs(Rest,ConflatedRest).

accumulate_rest_rp(_Recall,[],[],[]) :-
    !.
accumulate_rest_rp(Recall,[rp(Recall,Precision)|Rest],
                   NewRest,[Precision|RestPrecision]) :-
    !,
    accumulate_rest_rp(Recall,Rest,NewRest,RestPrecision).
accumulate_rest_rp(_Recall,Rest,Rest,[]).

% Non-determinate predicate
% Interpolated method
standardize_rp_pairs(RPPairs0,SRPPairs,interpolated) :-
    add_rp_endpoints(RPPairs0,1.0,RPPairs),
    compute_interpolated_rp_pairs(RPPairs,0,SRPPairs).
% Pessimistic method
%%standardize_rp_pairs(RPPairs0,SRPPairs,pessimistic) :-
%%    RPPairs0=[rp(_R,P)|_],
%%    add_rp_endpoints(RPPairs0,P,RPPairs),

add_rp_endpoints(RPPairs0,Pfor0,RPPairs) :-
    % add left endpoint, if necessary
    ((RPPairs0=[rp(RLeft,_)|_], RLeft =< 0) ->
        RPPairs1=RPPairs0
    ;   RPPairs1=[rp(0,Pfor0)|RPPairs0]
    ),
    % add right endpoint, if necessary
    rev(RPPairs1,RevRPPairs1),
    ((RevRPPairs1=[rp(RRight,_)|_], RRight >= 1) ->
        RPPairs=RPPairs1
    ;   RevRPPairs=[rp(1,0)|RevRPPairs1],
        rev(RevRPPairs,RPPairs)
    ),
    !.

compute_interpolated_rp_pairs(_,11,[]) :-
    !.
compute_interpolated_rp_pairs(RPPairs,I,[rp(R,P)|RestSRPPairs]) :-
    R is I / 10,
    find_surrounding_rp_pair(RPPairs,R,rp(R1,P1),rp(R2,P2)),
    (   R =:= R1 ->
        P=P1
    ;   R =:= R2 ->
        P=P2
    ;   P is P1 + ((R - R1)/(R2 - R1))*(P2 - P1)
    ),
    J is I + 1,
    compute_interpolated_rp_pairs(RPPairs,J,RestSRPPairs).

find_surrounding_rp_pair([rp(R1,P1),rp(R2,P2)|_Rest],R,rp(R1,P1),rp(R2,P2)) :-
    R1 =< R,
    R =< R2,
    !.
find_surrounding_rp_pair([_First|Rest],R,RP1,RP2) :-
    find_surrounding_rp_pair(Rest,R,RP1,RP2).

% Non-determinate predicate
compute_average_precision(SRPPairs,AP,'3-pt') :-
    extract_precisions(SRPPairs,Precisions0),
    Precisions0=[_,_,P2,_,_,P5,_,_,P8|_],
    Precisions=[P2,P5,P8],
    compute_average(Precisions,AP).
%%compute_average_precision(SRPPairs,AP,'11-pt') :-
%%    extract_precisions(SRPPairs,Precisions),
%%    compute_average(Precisions,AP).

extract_precisions([],[]).
extract_precisions([rp(_R,P)|Rest],[P|ExtractedRest]) :-
    extract_precisions(Rest,ExtractedRest).

compute_max_dice(RPPairs,MaxDice) :-
    compute_each_dice(RPPairs,EachDice),
    aggregate(max(D),member(D,EachDice),MaxDice).

compute_each_dice([],[]).
compute_each_dice([rp(R,P)|Rest],[Dice|RestDice]) :-
    ((R=:=0; P=:=0) ->
        Dice=0.0
    ;   Dice is 2.0/(1.0/R + 1.0/P)
    ),
    !,
    compute_each_dice(Rest,RestDice).

% :- use_module(library(addportray)).
% portray_mm_output(mm_output(_ExpandedUtterance,_CitationTextAtom,_ModifiedText,_Tagging,
% 			      _AAs,_Syntax,_DisambiguatedMMOPhrases,_ExtractedPhrases)) :-
%  	write('MM_OUTPUT').
% :- add_portray(portray_mm_output).
