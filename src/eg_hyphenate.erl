%%==========================================================================
%% Copyright (C) 2003 Joe Armstrong
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to permit
%% persons to whom the Software is furnished to do so, subject to the
%% following conditions:
%% 
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%% USE OR OTHER DEALINGS IN THE SOFTWARE.
%%
%% Author: Joe Armstrong <joe@sics.se>
%% Purpose: Hyphenation program - adapted from TeX
%%==========================================================================

-module(eg_hyphenate).

-export([partitions/2, 
	 word/2, 
	 test/1
	]).

-define(TO_SHORT, 4).


test(1) -> word("hyphenation", eg_hyphen_rules_en_GB);
test(2) -> partitions("supercalifragilisticexpialidocious", 
		      eg_hyphen_rules_en_GB);
test(3) -> test_text(eg_hyphen_rules, text_en()); %% using old .tex based rules 
test(4) -> test_text(eg_hyphen_rules_en_GB, text_en());


test_text(Rules, Text) -> 
    io:format("====================================~n",[]),
    lists:foreach(
      fun(W) -> io:format("~s ~s~n", [word(W, Rules), W]) 
      end,
      get_words(Text)).


get_words(Text) ->
    [Str || Str <- string:tokens(Text, " ,."), length(Str) > ?TO_SHORT].


%% ============================================================================

%% text from: BBC (2009-08-05) 
%%            http://news.bbc.co.uk/2/hi/middle_east/8184240.stm
text_en() ->
    "Mahmoud Ahmadinejad has been sworn in for a second term as Iran's "
	"president, after weeks of post-election unrest. "
	"In an address after the ceremony, he criticised foreign powers "
	"who have cast doubt on the validity of the election, saying Iran "
	"would resist them. "
	"Opposition supporters protesting outside parliament were met by "
	"hundreds of riot police. "
	"Germany, France, Britain and the US all said they would not be "
	"sending letters of congratulation to Mr Ahmadinejad. "
	"At least 30 people died during the street protests which followed "
	"the 12 June poll.".

%% ============================================================================

partitions(W, Rules) -> partitions(word(W, Rules), [], [], Rules).

partitions([$-|T],  B, L, Rules) -> partitions(T, B, [{lists:reverse(B), 
						       remove_hyphens(T)}|L], 
					       Rules);
partitions([H|T], B, L, Rules)   -> partitions(T, [H|B], L, Rules);
partitions([], _, L, _)          -> lists:reverse(L).


remove_hyphens([$-|T]) -> remove_hyphens(T);
remove_hyphens([H|T])  -> [H|remove_hyphens(T)];
remove_hyphens([])     -> [].

word(L, Rules) when length(L) > ?TO_SHORT ->
    case Rules:exception(L) of
	no ->
	    L1 = word([$.|L] ++ ".", 0, [], Rules),
	    L2 = lists:sort(keep_odd(L1)),
	    %% io:format("L2=~p~n",[L2]),
	    W1 = make_word(L2, 1, L),
	    remove_singleton(W1);
	Str ->
	    Str
    end;
word(X, _) ->
    X.


%% -AAA... => AAA..
%% X-AAA.. => AAA..
%% ...-B   => ...B
%% ...B-   => ...B

remove_singleton([H,$-|T]) -> [H|remove_singleton1(T)];
remove_singleton([$-|T])   -> remove_singleton1(T);
remove_singleton(X)        -> remove_singleton1(X).

remove_singleton1([$-])   -> [];
remove_singleton1([$-,H]) -> [H];
remove_singleton1([H|T])  -> [H|remove_singleton1(T)];
remove_singleton1([])     -> [].

make_word([{Pos, _C}|T], Pos, L) ->
    [$-|make_word(T, Pos, L)];
make_word(S=[{Pos,_}|_], Pos1, [H|T]) when Pos1 < Pos ->
    [H|make_word(S, Pos1+1, T)];
make_word([], _, L) ->
    L.

word([], _, L, _) ->
    L;
word(T, N, L, Rules) ->
    case Rules:hyphens(T) of
	[] ->
	    word(tl(T), N+1, L, Rules);
	M ->
	    M1 = lists:map(fun({Pos,Val}) -> {Pos+N,Val} end, M),
	    %% io:format("~s ~p => ~p~n", [T, M, M1]),
	    L1 = merge(M1, L),
	    word(tl(T), N+1, L1, Rules)
    end.


merge([], L)          -> L;
merge([{Pos,C}|T], L) -> merge(T, merge1(Pos, C, L)).

merge1(Pos, C, [])                       -> [{Pos,C}];
merge1(Pos, C, [{Pos,C1}|T]) when C > C1 -> [{Pos,C}|T];
merge1(Pos, _C, [{Pos,C1}|T])             -> [{Pos,C1}|T];
merge1(Pos, C, [H|T])                    -> [H|merge1(Pos, C, T)].

keep_odd(L) ->    
    lists:filter(fun({_Pos, Count}) -> odd(Count) end, L).

odd(X) ->
    (X rem 2) == 1.

    
