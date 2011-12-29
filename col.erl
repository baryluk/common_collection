-module(col).
-author('baryluk@smp.if.uj.edu.pl').
-vsn('0.3').

-export([new/1, store/3, lookup/2, fetch/2, erase/2, delete/2, balance/1,
	is_key/2, fold/3, keys/1, map/2, to_list/1, size/1, from_orddict/2,
	update/3, update_val/3, update_counter/3, update_full/4]).

% Simple wrapper for key-value datastructures, with one common API (very
% similar to dict, but few functions are added).

% opaque types
% dict() = {dict, integer(), integer(), integer(), integer(), integer(), integer(), tuple(), tuple()}
% gb_tree() = {integer(), _}

-define(W_DICT(X), {dict,X}).
-define(W_GB_TREES(X), {gb_trees,X}).
%-define(W_GB_TREES(X), {gb_trees_col_wrap,X}).
-define(W_ORDDICT(X), {orddict,X}).

-define(W_PROCDICT(X), {procdict_col_wrap,X}).
-define(W_ETS(X), {ets_col_wrap,X}).
-define(W_DETS(X), {dets_col_wrap,X}).
%-define(W_MNESIA(X), {mnesia_col_wrap,X}).

% rbdict (from usercontrib, identical API like dict, but red-black tree, not hash table)
-define(W_RBDICT(X), {rbdict,X}).
% https://github.com/baryluk/bbtree
-define(W_BBTREE(X), {bbtree,X}).



new(gb_trees) ->
	?W_GB_TREES(.gb_trees:empty());
new(bbtree) ->
	?W_BBTREE(.bbtree:empty());
new(M) ->
	{M, M:new()}.

store(Key, Val, ?W_GB_TREES(Tree)) ->
	?W_GB_TREES(.gb_trees:enter(Key, Val, Tree)); % :insert assumes that Key is not present in Tree
store(Key, Val, {M, Dict}) ->
	{M, M:store(Key, Val, Dict)}.

lookup(Key, ?W_GB_TREES(Tree)) ->
	case .gb_trees:lookup(Key, Tree) of
		{value, Val} ->
			{ok, Val};
		none ->
			error
	end;
lookup(Key, {M, Dict}) ->
	case M:find(Key, Dict) of
		{ok, Val} ->
			{ok, Val};
		error ->
			error
	end.

% crash if no such key
fetch(Key, ?W_GB_TREES(Tree)) ->
	.gb_trees:get(Key, Tree);
fetch(Key, {M, Dict}) ->
	{M, M:fetch(Key, Dict)}.


% dict:find(Key, Dict) -> {ok, Val} | error

% doesn't crash
erase(Key, ?W_GB_TREES(Tree)) ->
	?W_GB_TREES(.gb_trees:delete_any(Key, Tree));
erase(Key, {M, Dict}) ->
	{M, M:erase(Key, Dict)}.

% exception if not in tree
delete(Key, ?W_GB_TREES(Tree)) ->
	?W_GB_TREES(.gb_trees:delete(Key, Tree));
delete(Key, {M, Dict}) ->
	case M:is_key(Dict) of
		true ->
			{M, M:erase(Key, Dict)};
		false ->
			exit(no_such_key)
	end.

balance(?W_GB_TREES(Tree)) ->
	?W_GB_TREES(.gb_trees:balance(Tree));
balance(D) ->
	D.

is_key(Key, ?W_GB_TREES(Tree)) ->
	.gb_trees:is_defined(Key, Tree);
is_key(Key, {M, Dict}) ->
	M:is_key(Key, Dict).

fold(Fun, Init, ?W_GB_TREES(Tree)) ->
	Iter = .gb_trees:iterator(Tree),
	gf(Fun, Init, Iter);
fold(Fun, Init, {M, Dict}) ->
	M:fold(Fun, Init, Dict).

gf(Fun, AccIn, Iter) ->
	case .gb_trees:next(Iter) of
		{Key, Val, Iter2} ->
			AccOut = Fun(Key, Val, AccIn),
			gf(Fun, AccOut, Iter2);
		none ->
			AccIn
	end.

keys(?W_GB_TREES(Tree)) ->
	.gb_trees:keys(Tree);
keys({M, Dict}) ->
	M:fetch_keys(Dict).

map(Fun, ?W_GB_TREES(Tree)) ->
	?W_GB_TREES(.gb_trees:map(Fun, Tree));
map(Fun, {M, Dict}) ->
	{M, .dict:map(Fun, Dict)}.

to_list(?W_GB_TREES(Tree)) ->
	.gb_trees:to_list(Tree);
to_list({M, Dict}) ->
	M:to_list(Dict).

size(?W_GB_TREES(Tree)) ->
	.gb_trees:size(Tree);
size({M, Dict}) ->
	M:size(Dict).

from_orddict(gb_trees, List) ->
	?W_GB_TREES(.gb_trees:from_orddict(List)); % must not contain duplicate keys
from_orddict(M, List) ->
	{M, M:from_list(List)}.

% assumes Key in Tree
update(Key, Fun, ?W_GB_TREES(_Tree) = T) when is_function(Fun, 1) ->
	OldVal = fetch(Key, T),
	NewVal = Fun(OldVal),
	update_val(Key, NewVal, T);
% exception if no Key in Dict
update(Key, Fun, {M, Dict}) ->
	{M, M:update(Key, Fun, Dict)}.

update_val(Key, Val, ?W_GB_TREES(Tree)) ->
	?W_GB_TREES(.gb_trees:update(Key, Val, Tree));
update_val(Key, Val, D) ->
	update(Key, fun(_Old) -> Val end, D).

update_counter(Key, Increment, ?W_GB_TREES(_Tree) = T) ->
	update(Key, fun(Old) -> Old+Increment end, T);
update_counter(Key, Increment, {M, Dict}) ->
	{M, M:update_counter(Key, Increment, Dict)}.

% similar to dict:update/4, but instead to Initial, we have FunInit,
% which is called/computed  only we actually need Initial. Can save use
% some time, if calculation of Initial  is costly.
update_full(Key, Fun, FunInit, ?W_GB_TREES(Tree) = T) when is_function(Fun, 1), is_function(FunInit, 0) ->
	case .gb_trees:is_defined(Key, Tree) of
		true ->
			update(Key, Fun, T);
		false ->
			Val = FunInit(),
			?W_GB_TREES(.gb_trees:insert(Key, Val, Tree))
	end;
update_full(Key, Fun, FunInit, {M, Dict}) when is_function(Fun, 1), is_function(FunInit, 0) ->
	case M:is_key(Key, Dict) of
		true ->
			{M, M:update(Key, Fun, Dict)};
		false ->
			Val = FunInit(),
			{M, M:store(Key, Val, Dict)}
	end.
