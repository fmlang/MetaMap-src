
:- use_module(skr_lib(nls_io), [
	fget_non_ws_only_line/2
   ]).


:- use_module(skr_lib(nls_strings), [
	atom_codes_list/2,
	split_string_completely/3
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
	directory_members_of_directory/2,
	directory_members_of_directory/3,
	file_members_of_directory/3,
	make_directory/1
   ]).

:- use_module(library(system), [
	datime/1,
	environ/2
   ]).


% properties([environment(none),database(SICStus_BDB/syns),speclist([syns(+,-,-,-)]),version(1)])

convert_DB_dir(Version, Year, Model) :-
	% First identify all the *.txt files in the specified BDB directory
	environ('MODEL_LOCATION_BASE_DIR', Base),
	concat_atoms([Base, '/DB.', Version, '.', Year, '.', Model], Dir),
	file_members_of_directory(Dir, '*.txt', Files),
	convert_all_txt_files(Files, Version, Year, Model).

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

convert_all_txt_files([], _Version, _Year, _Model).
convert_all_txt_files([RelativePath-AbsolutePath|T], Version, Year, Model) :-
	convert_one_txt_file(RelativePath, AbsolutePath, Version, Year, Model),
	convert_all_txt_files(T, Version, Year, Model).

convert_one_txt_file(FileName, AbsolutePath, Version, Year, Model) :-
	% Open the txt file for reading
	open(AbsolutePath, read, InputStream),
	atom_codes(FileName, FileNameChars),
	append(BaseNameChars, ".txt", FileNameChars),
	atom_codes(BaseName, BaseNameChars),
	bdb_file_spec(BaseName, DBArgSpecs),
	make_BDB_path(BaseName, Version, Year, Model, BDBPath),
	DBSpec =.. [BaseName|DBArgSpecs],
	format(user_output, 'Creating directory ~w~n', [BDBPath]),
	db_open(BDBPath, update, [DBSpec], [cache_size(4000000)], DBRef),
	I is 0,
	convert_one_txt_file_aux(InputStream, DBRef, BaseName, I).

convert_one_txt_file_aux(InputStream, DBRef, FileName, I) :-
	( fget_non_ws_only_line(InputStream, Line) ->
	  split_string_completely(Line, "|", FieldStrings),
	  atom_codes_list(FieldAtoms, FieldStrings),
	  Term =.. [FileName|FieldAtoms],
	  db_store(DBRef, Term, _),
	  I1 is I + 1,
    	  conditionally_portray_clause(I1, Term),
	  convert_one_txt_file_aux(InputStream, DBRef, FileName, I1)
	; close(InputStream),
	  db_close(DBRef),
	  format(user_output, '~n~d records processed for database ~w.~n', [I, FileName])
	).
	    
% simply convert a|b|c|d to foo(a,b,c,d) potentially for use by db_import/3
reformat_one_txt_file(Version, Year, ModelName, FileName) :-
	environ('MODEL_LOCATION_BASE_DIR', BDBDir),
	concat_atoms([BDBDir, '/DB.', Version, '.',
		      Year, '.', ModelName,
		      '/', FileName, '.txt'], InputFileFullPath),
	concat_atoms([BDBDir, '/DB.', Version, '.',
		      Year, '.', ModelName,
		      '/', FileName, '.pl'], OutputFileFullPath),
	% Open the txt file for reading
	open(InputFileFullPath,  read,  InputStream),
	open(OutputFileFullPath, write, OutputStream),
	I is 0,
	reformat_one_txt_file_aux(FileName, InputStream, OutputStream, I).
		     
reformat_one_txt_file_aux(FileName, InputStream, OutputStream, I) :-
	( fget_non_ws_only_line(InputStream, Line) ->
	  split_string_completely(Line, "|", FieldStrings),
	  atom_codes_list(FieldAtoms, FieldStrings),
	  Term =.. [FileName|FieldAtoms],
    	  portray_clause(OutputStream, term(Term)),
	  I1 is I + 1,
	  conditionally_announce_count(I1),
	  reformat_one_txt_file_aux(FileName, InputStream, OutputStream, I1)
	; close(InputStream),
	  close(OutputStream),
	  format(user_output, '~n~d records reformatted for database ~w.~n', [I, FileName])
	).

make_BDB_path(FileName, Version, Year, ModelName, BDBPath) :-
	sicstus_bdb_dir(SICStusDir),
	make_directory_if_necessary(SICStusDir),
	concat_atoms([SICStusDir, '/', 'DB.', Version, '.', Year, '.', ModelName], BDBDir),
	% If necessary, create the directory e.g., SICStus_BDB/DB.normal.09.strict
	make_directory_if_necessary(BDBDir),
	% set the path to e.g., SICStus_BDB/DB.normal.09.strict/all_words
	concat_atoms([BDBDir, '/', FileName], BDBPath),
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


% Display every 10,000th term
conditionally_portray_clause(I, Term) :-
	( 0 is I mod 1000 ->
	  format(user_output, '~N~d...~q.~n', [I, Term])
	; true
	).

% Display every 10,000th count
conditionally_announce_count(I) :-
	( 0 is I mod 1000 ->
	  format(user_output, '~N~d...~n', [I])
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
bdb_file_spec(all_words,      		[+,-,-]).
bdb_file_spec(all_words_counts,      	[+,-]).
bdb_file_spec(concept_cui,		[+,-]).
bdb_file_spec(concept_st,		[+,-]).
bdb_file_spec(cui_concept,       	[+,-]).
bdb_file_spec(cui_sourceinfo,		[+,-,-,-,-,-]).
bdb_file_spec(cui_src,			[+,-]).
bdb_file_spec(cui_st,			[+,-]).
bdb_file_spec(first_words,		[+,-,-]).
bdb_file_spec(first_words_counts, 	[+,-]).
bdb_file_spec(first_words_of_one,	[+,-,-]).
bdb_file_spec(first_words_of_two,	[+,-,-]).
bdb_file_spec(first_wordsb,		[+,-,-]).
bdb_file_spec(first_wordsb_counts,	[+,-]).
bdb_file_spec(mesh_mh_opt,		[+,-]).
bdb_file_spec(mesh_tc_relaxed,		[+,-]).
bdb_file_spec(mesh_tc_strict,		[+,-]).
bdb_file_spec(mesh_tc_strict, 		[+,-]).
bdb_file_spec(meta_mesh_opt,		[+,-]).
bdb_file_spec(meta_mesh_tc_opt,		[+,-,-]).
bdb_file_spec(nls_aa,			[+,-,-,-]).
bdb_file_spec(nls_aau,			[+,-,-,-]).
bdb_file_spec(sui_cui,			[+,-]).
bdb_file_spec(sui_nmstr_str,		[+,-,-]).
bdb_file_spec(syns,			[+,-,-,-]).
bdb_file_spec(syns,           		[+,-,-,-]).
bdb_file_spec(vars,			[+,+,-,-,-,-,-]).
bdb_file_spec(varsan,			[+,+,-,-,-,-,-]).
bdb_file_spec(varsanu,			[+,+,-,-,-,-,-]).
bdb_file_spec(varsu,			[+,+,-,-,-,-,-]).
