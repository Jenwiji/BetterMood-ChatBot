%===================================%
%		start better mood bot		%
%===================================%


bettermood:-
	out("Starting up our better mood bot!\n"),
	init,
	out("Nice to meet you!, I am better mood bot... \nHow are you feeling today? \nWe can talk about many things!  :)"),
	%placeholder [begin] prevents infinite loop
	bettermood([begin]).

bettermood([quit|_]):-!.
bettermood(_):-
	in(Line),
	simplify(Line, Words),
	findReply(Words,Reply),
	writeWords(Reply),nl,
	bettermood(Words).

%=======================================================%
%        the input and output for bettermood bot        %
%=======================================================%


%	display the output
out(Text):-
	write(Text).


%	get the user input first and read it in readLine function
in(Text):-
	nl,
	write("Your Input ----->  "),
	readLine(Text),nl.
%	read the sentence
readLine(Text):-
	%	receive input until user press enter
	get_char(Char),
	toLowerCase(Char,LChar),
	readLine2(LChar,Text).
%	detect newline
readLine2('\n',[]):-!.
readLine2(LChar,[LChar|T]):-
	readLine(T).


%=======================================%
%				methods					%
%=======================================%


%		constant predicates		%

%	punctuations
charType('!', punctuation).
charType('?', punctuation).
charType('.', punctuation).
charType(',', punctuation).
charType('\'', punctuation).

%	whitespaces
charType(' ', whitespace).
charType('\t', whitespace).


%	toLowerCase(+Char, -LChar):- 
%		convert input to lowercase with ASCII
toLowerCase(Char, LChar):-
	%	[built in SWI] char_code get the character code by taking in Char (input) return the Code of that character
	%	if between A - Z which is upper character, then add 32 to obtain lower character
	char_code(Char, Code),
	Code >= "A",
	Code =< "Z",
	NewCode is Code + 32,
	%	convert it back to a character and cut. 
	char_code(LChar, NewCode), !.
toLowerCase(Char, Char).


%	toUpperCase(+Char, -UChar):- 
%		convert input to uppercase with ASCII
%		works similarly to converting to lower case
toUpperCase(Char, UChar):-
	char_code(Char, Code),
	Code >= "a",
	Code =< "z",
	NewCode is Code - 32,
	char_code(UChar, NewCode), !.
toUpperCase(Char, Char).


%	deleteChars(+Line, -Type, -Res):- 
%		delete a character type from the sentence input 
deleteChars([Char|Rest],Type,Out):-
	charType(Char, Type),
	deleteChars(Rest,Type,Out),!.

deleteChars([Char|Rest],Type,[Char|Out]):-
	deleteChars(Rest,Type,Out),!.

deleteChars([],_,[]).


%	toWords(+Line, -Words):- 
%		create a list of words from readLine input
toWords([],[]):-!.

toWords(Line, [Word|ResWords]):-
	%	breakdown and observe word components in readWord()
	readWord(Line, Word, ResLine),
	toWords(ResLine, ResWords).


%	readWord(+Line, -Word, -ResLine) :- 
%		reads a single word from the line input
% 		the rest of line is returned in ResLine
readWord([], '', []).

readWord([Char|Res], '', Res) :-
	%	check for a whitespace
	charType(Char, whitespace),!.

readWord([Char|ResLine], Word, Res) :- 
	readWord(ResLine, ResWord, Res),
	%	[built in SWI] append ResWord at the end of char to obtain Word
	atom_concat(Char, ResWord, Word).


%===================================================%
%			bettermood chatbot's functions			%
%===================================================%


:- dynamic resID/2.
resID(_,0).

%	init:- 
%		initializes environment for simplification rules
%		consult the other files
init:-
	consult("simplification.rules"),
	consult("reply.rules").	


%	simplify(+In,-Out):- 
%		removes unnecessary characters which are the punctuations according to
%		the provided predicates example: "," and "." 
% 		while simplifying words
simplify(In, Out):-
	%	remove
	deleteChars(In, punctuation, Out1),
	%	breakdown the necessary words (those which are kept)
	toWords(Out1,Out2),
	findSynonyms(Out2,Out3),
	Out = Out3.


%	findSynonyms(+Words, -Synonyms) :- 
%		identify the synonyms with
% 		simplification rules 
findSynonyms(Words, Syn) :-
	%	cut when found in the sr
	sr(Words, Syn, RestWords, ResOutput),!,
	findSynonyms(RestWords, ResOutput).

findSynonyms([Word| ResWords], [Word| ResSyn]):-
	findSynonyms(ResWords, ResSyn),!.

findSynonyms([], []).


%	findReply(+Words, -Reply) :- 
%		finds the best reply through identifying the highest rank
findReply(Words, Reply) :-
	findReply2(Words, -2, 0, [], ID, Reply),
	ID \= 0,
	updateResID(ID).


%	findReply2(+Words, +ActScore, +ActRuleID, +ActRes, -RuleID, -Res):- 
%		finds reply with two accumulators
findReply2([H|T], ActScore, _, _, ID, Res):-
	findall(Score,rules(_, Score,[H|T],_),Rules),
	Rules \= [], 
	max_list(Rules,NewScore),
	ActScore < NewScore,
	rules(NewID, NewScore,[H|T],Replyes),
	resID(NewID,ResID),
	nth0(ResID,Replyes,NewReply),
	findReply2(T, NewScore, NewID, NewReply, ID, Res),!.

findReply2([_|T], ActScore, ActID, ActRes, ID, Res):-
	findReply2(T, ActScore, ActID, ActRes, ID, Res).
findReply2([], _, ID, Res, ID, Res).


%	updateResID(+ID):- 
%		moves to next reply for rule
updateResID(ID):-
	resID(ID,RID),
	once(rules(ID,_,_,Replyes)),
	length(Replyes, Len),
	NRID is (RID + 1) mod Len,
	retract((resID(ID,RID):-!)),
	asserta(resID(ID,NRID):-!),!.

updateResID(ID):-
	resID(ID,RID),
	once(rules(ID,_,_,Replyes)),
	length(Replyes, Len),
	NRID is (RID + 1) mod Len,
	asserta(resID(ID,NRID):-!).


%===================================================%
%			writing replies to the user  			%
%===================================================%


%	writeWords(+Words) :- 
%		Makes the first letter uppercase and reply to output
writeWords([Word|Res]):-
	string_chars(Word,[Char|RChar]),
	toUpperCase(Char,UChar),
	readWord([UChar|RChar],Out,_),
	out(Out),
	writeWords2(Res).


%	writes the contnet
writeWords2([Word|Res]):-
	%	[built in SWI] is_list checks if the list is a proper list
	% 	if it is a proper list, it should continue
	is_list(Word),
	writeWords2(Word),
	writeWords2(Res),!.


%	writes necessary punctuation
writeWords2([Word|Res]):-
	charType(Word,punctuation),
	out(Word),
	writeWords2(Res),!.


%	writes standard char with whitespace separating the words
writeWords2([Word|Res]):-
	out(" "),
	out(Word),
	writeWords2(Res),!.
writeWords2([]).
%	writes out all words until list is empty