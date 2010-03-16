
/****************************************************************************
*
*                          PUBLIC DOMAIN NOTICE                         
*         Lister Hill National Center for Biomedical Communications
*                      National Library of Medicine
*                      National Institues of Health
*           United States Department of Health and Human Services
*                                                                         
*  This software is a United States Government Work under the terms of the
*  United States Copyright Act. It was written as part of the authors'
*  official duties as United States Government employees and contractors
*  and thus cannot be copyrighted. This software is freely available
*  to the public for use. The National Library of Medicine and the
*  United States Government have not placed any restriction on its
*  use or reproduction.
*                                                                        
*  Although all reasonable efforts have been taken to ensure the accuracy 
*  and reliability of the software and data, the National Library of Medicine
*  and the United States Government do not and cannot warrant the performance
*  or results that may be obtained by using this software or data.
*  The National Library of Medicine and the U.S. Government disclaim all
*  warranties, expressed or implied, including warranties of performance,
*  merchantability or fitness for any particular purpose.
*                                                                         
*  For full details, please see the MetaMap Terms & Conditions, available at
*  http://metamap.nlm.nih.gov/MMTnCs.shtml.
*
***************************************************************************/


:- use_module(skr_lib(nls_io), [
	fget_non_ws_only_line/2
   ]).


:- use_module(skr_lib(nls_strings), [
	atom_codes_list/2,
	split_string_completely/3
   ]).

:- use_module(skr_lib(sicstus_utils), [
	concat_atom/2
   ]).

:- use_module(library(bdb), [
	db_close/1,
	db_current/5,
     	db_current_iterator/3,
     	db_enumerate/3,
	db_export/2,
     	db_fetch/3,
	db_findall/5,
        db_import/2,
	db_iterator_done/1,
	db_iterator_next/3,
	db_make_iterator/2,
     	db_make_iterator/3,
	db_open/4,
     	db_open/5,
	db_store/3
   ]).

:- use_module(library(file_systems), [
	delete_directory/2,
      	directory_exists/1,
	file_exists/1,
	file_members_of_directory/3,
	make_directory/1
   ]).

:- use_module(library(system), [
	datime/1,
	environ/2,
	now/1
   ]).


% properties([environment(none),database(SICStus_BDB/syns),speclist([syns(+,-,-,-)]),version(1)])

% sample call:
% convert_DB_dir(normal, '09', strict).

convert_DB_dir(Version, Year, Model) :-
	% First identify all the *.txt files in the specified BDB directory
	environ('MODEL_LOCATION_BASE_DIR', Base),
	concat_atom([Base, '/DB.', Version, '.', Year, '.', Model], Dir),
	file_members_of_directory(Dir, '*.txt', Files),
	convert_all_txt_files(Files, Dir, Version, Year, Model).

% sample call:
% do_strict('09').

do_strict(Year)  :-
	datime(Begin),
	format(user_output, 'BEGIN: ~w~n', [Begin]),
	convert_DB_dir(normal, Year, strict),
	datime(End),
	format(user_output, 'BEGIN: ~w~n', [Begin]),
	format(user_output, 'END  : ~w~n', [End]).

do_relaxed(Year) :-
	datime(Begin),
	format(user_output, 'BEGIN: ~w~n', [Begin]),
	convert_DB_dir(normal, Year, relaxed),
	datime(End),
	format(user_output, 'BEGIN: ~w~n', [Begin]),
	format(user_output, 'END  : ~w~n', [End]).

convert_all_txt_files([], Dir, _Version, _Year, _Model) :-
	( \+ directory_exists(Dir) ->
	  format(user_output, '~nERROR: Directory ~w does not exist!~n', [Dir])
	; format(user_output, '~nWARNING: Directory ~w contained no files!~n', [Dir])
	).
convert_all_txt_files([H|T], _Dir, Version, Year, Model) :-
	convert_all_txt_files_1([H|T], Version, Year, Model).


convert_all_txt_files_1([], _Version, _Year, _Model).
convert_all_txt_files_1([RelativePath-AbsolutePath|T], Version, Year, Model) :-
	convert_one_txt_file(RelativePath, AbsolutePath, Version, Year, Model),
	convert_all_txt_files_1(T, Version, Year, Model).

convert_one_txt_file(FileName, AbsolutePath, Version, Year, Model) :-
	% Open the txt file for reading
	open(AbsolutePath, read, InputStream),
	atom_codes(FileName, FileNameChars),
	% strip off the ".txt" suffix to get the basename
	append(BaseNameChars, ".txt", FileNameChars),
	!,
	atom_codes(BaseName, BaseNameChars),
	( bdb_file_spec(BaseName, DBArgSpecs) ->
	  make_BDB_path(BaseName, Version, Year, Model, BDBPath),
	  DBSpec =.. [BaseName|DBArgSpecs],
	  format(user_output, 'Creating directory ~w~n', [BDBPath]),
	  db_open(BDBPath, update, [DBSpec], [cache_size(4000000)], DBRef),
	  I is 0,
	  now(StartTime),
	  convert_one_txt_file_aux(InputStream, StartTime, DBRef, BaseName, I)
	; format(user_output, 'No bdb_file_spec/2 clause for table ~w.~n', [BaseName])
	).

convert_one_txt_file_aux(InputStream, StartTime, DBRef, FileName, I) :-
	now(Now),
	TimeDiff is Now - StartTime,
	( fget_non_ws_only_line(InputStream, Line) ->
	  split_string_completely(Line, "|", FieldStrings),
	  atom_codes_list(FieldAtoms, FieldStrings),
	  Term =.. [FileName|FieldAtoms],
	  db_store(DBRef, Term, _),
	  I1 is I + 1,
    	  conditionally_portray_clause(Term, TimeDiff, I),
	  convert_one_txt_file_aux(InputStream, StartTime, DBRef, FileName, I1)
	; close(InputStream),
	  db_close(DBRef),
	  format(user_output,
		 '~n~d records processed in ~d seconds for database ~w.~n~n',
		 [I, TimeDiff, FileName])
	).
	    
% For table foo, convert rows of the form a|b|c|d
% to Prolog terms of the form foo(a,b,c,d) for use by db_import/3
reformat_one_txt_file(Version, Year, ModelName, FileName) :-
	environ('MODEL_LOCATION_BASE_DIR', BDBDir),
	concat_atom([BDBDir, '/DB.', Version, '.',
		      Year, '.', ModelName,
		      '/', FileName, '.txt'], InputFileFullPath),
	concat_atom([BDBDir, '/DB.', Version, '.',
		      Year, '.', ModelName,
		      '/', FileName, '.pl'], OutputFileFullPath),
	% Open the txt file for reading
	open(InputFileFullPath,  read,  InputStream),
	open(OutputFileFullPath, write, OutputStream),
	I is 0,
	now(StartTime),
	reformat_one_txt_file_aux(FileName, StartTime, InputStream, OutputStream, I).
		     
reformat_one_txt_file_aux(FileName, StartTime, InputStream, OutputStream, I) :-
	now(Now),
	TimeDiff is Now - StartTime,
	( fget_non_ws_only_line(InputStream, Line) ->
	  split_string_completely(Line, "|", FieldStrings),
	  atom_codes_list(FieldAtoms, FieldStrings),
	  Term =.. [FileName|FieldAtoms],
    	  portray_clause(OutputStream, term(Term)),
	  I1 is I + 1,
	  conditionally_portray_clause(Term, TimeDiff, I),
	  reformat_one_txt_file_aux(FileName, StartTime, InputStream, OutputStream, I1)
	; close(InputStream),
	  close(OutputStream),
	  format(user_output, '~n~d records reformatted for database ~w.~n', [I, FileName])
	).

make_BDB_path(FileName, Version, Year, ModelName, BDBPath) :-
	sicstus_bdb_dir(SICStusDir),
	make_directory_if_necessary(SICStusDir),
	concat_atom([SICStusDir, '/', 'DB.', Version, '.', Year, '.', ModelName], BDBDir),
	% If necessary, create the directory e.g., SICStus_BDB/DB.normal.09.strict
	make_directory_if_necessary(BDBDir),
	% set the path to e.g., SICStus_BDB/DB.normal.09.strict/all_words
	concat_atom([BDBDir, '/', FileName], BDBPath),
	% delete any existing directory of that name
	delete_directory_if_necessary(BDBPath).

delete_directory_if_necessary(BDBPath) :-
        ( directory_exists(BDBPath) ->
          format(user_output, 'Deleting directory ~w~n', [BDBPath]),
          delete_directory(BDBPath, [if_nonempty(delete)])
        ; true
        ).

make_directory_if_necessary(BDBDir) :-
	( directory_exists(BDBDir) ->
	  true
	; format(user_output, 'Creating directory ~w~n', [BDBDir]),
	  make_directory(BDBDir)
	).

% Display every 1,000th term
conditionally_portray_clause(Term, TimeDiff, N) :-
	( 0 is N mod 1000 ->
	  format(user_output, '~N~d...~d...~q~n', [N, TimeDiff, Term]),
	  garbage_collect,
	  garbage_collect_atoms
	; true
	).

sicstus_bdb_dir('SICStus_BDB').

% All indexes are the first argument only, so the data below are needlessly redundant.
% E.g., we could do instead simply
% bdb_file_spec(all_words,      	3).
% bdb_file_spec(all_words_counts,      	2).
% bdb_file_spec(concept_cui,		2).
% and have the calling code automatically construct the DBSpec as [+,-,-,...,-],
% but the data below are more flexible and will easily allow creating indexes
% on fields other than the first.

% Wide version
bdb_file_spec(all_words,		[+,-,-,-]).
% Narrow version
% bdb_file_spec(all_words,      	[+,-,-]).
bdb_file_spec(all_words_counts,      	[+,-]).
bdb_file_spec(concept_cui,		[+,-]).
bdb_file_spec(concept_st,		[+,-]).
bdb_file_spec(cui_concept,       	[+,-]).
bdb_file_spec(cui_sourceinfo,		[+,-,-,-,-,-]).
bdb_file_spec(cui_src,			[+,-]).
bdb_file_spec(cui_st,			[+,-]).
% Wide version
bdb_file_spec(first_words,		[+,-,-,-]).
% Narrow version
% bdb_file_spec(first_words,		[+,-,-]).
bdb_file_spec(first_words_counts, 	[+,-]).
% Wide version
bdb_file_spec(first_words_of_one,	[+,-,-,-]).
% Narrow version
% bdb_file_spec(first_words_of_one,	[+,-,-]).
% Wide version
bdb_file_spec(first_words_of_two,	[+,-,-,-]).
% Narrow version
% bdb_file_spec(first_words_of_two,	[+,-,-]).
% Wide version
bdb_file_spec(first_wordsb,		[+,-,-,-]).
% Narrow version
% bdb_file_spec(first_wordsb,		[+,-,-]).
bdb_file_spec(first_wordsb_counts,	[+,-]).
bdb_file_spec(mesh_mh_opt,		[+,-]).
bdb_file_spec(mesh_tc_relaxed,		[+,-]).
bdb_file_spec(mesh_tc_strict,		[+,-]).
bdb_file_spec(meta_mesh_opt,		[+,-]).
bdb_file_spec(meta_mesh_tc_opt,		[+,-,-]).
bdb_file_spec(nls_aa,			[+,-,-,-]).
bdb_file_spec(nls_aau,			[+,-,-,-]).
bdb_file_spec(sui_cui,			[+,-]).
bdb_file_spec(sui_nmstr_str,		[+,-,-]).
bdb_file_spec(syns,           		[+,-,-,-]).
bdb_file_spec(vars,			[+,+,-,-,-,-,-]).
bdb_file_spec(varsan,			[+,+,-,-,-,-,-]).
bdb_file_spec(varsanu,			[+,+,-,-,-,-,-]).
bdb_file_spec(varsu,			[+,+,-,-,-,-,-]).
