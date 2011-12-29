-module(col).
-author('baryluk@smp.if.uj.edu.pl').
-vsn('0.2').

-export([new/1, store/3, lookup/2, fetch/2, erase/2, delete/2, balance/1,
	is_key/2, fold/3, keys/1, map/2, to_list/1, size/1, from_orddict/2,
	update/3, update_val/3, update_counter/3, update_full/4]).

% Simple wrapper for key-value datastructures, with one common API (very
% similar to dict, but few functions are added).

% opaque types
% dict() = {dict, integer(), integer(), integer(), integer(), integer(), integer(), tuple(), tuple()}
% gb_tree() = {integer(), _}

-define(W_DICT(X), {col_dict,X}).
-define(W_GB_TREE(X), {col_gb_tree,X}).
-define(W_ORDDICT(X), {col_orddict,X}).

-define(W_PROCDICT(X), {col_procdict,X}).
-define(W_ETS(X), {col_ets,X}).
-define(W_DETS(X), {col_dets,X}).
%-define(W_MNESIA(X), {col_mnesia,X}).

% rbdict (from usercontrib, identical API like dict, but red-black tree, not hash table)
-define(W_RBDICT(X), {col_rbdict,X}).
% https://github.com/baryluk/bbtree
-define(W_BB_TREE(X), {col_bb_tree,X}).



new(dict) ->
	?W_DICT(.dict:new());
new(gb_trees) ->
	?W_GB_TREE(.gb_trees:empty());
new(rbdict) ->
	?W_RBDICT(.rbdict:new());
new(orddict) ->
	?W_ORDDICT(.orddict:new()).
%new(procdict) ->
%	?W_PROCDICT([]);
%new(bbtree) ->
%	?W_BB_TREE(.bbtree:empty()).

store(Key, Val, ?W_GB_TREE(Tree)) ->
	?W_GB_TREE(.gb_trees:enter(Key, Val, Tree)); % :insert assumes that Key is not present in Tree
store(Key, Val, ?W_DICT(Dict)) ->
	?W_DICT(.dict:store(Key, Val, Dict));
store(Key, Val, ?W_ORDDICT(Orddict)) ->
	?W_ORDDICT(.orddict:store(Key, Val, Orddict));
store(Key, Val, ?W_RBDICT(Dict)) ->
	?W_RBDICT(.rbdict:store(Key, Val, Dict)).

lookup(Key, ?W_GB_TREE(Tree)) ->
	case .gb_trees:lookup(Key, Tree) of
		{value, Val} ->
			{ok, Val};
		none ->
			error
	end;
lookup(Key, ?W_DICT(Dict)) ->
	case .dict:find(Key, Dict) of
		{ok, Val} ->
			{ok, Val};
		error ->
			error
	end;
lookup(Key, ?W_ORDDICT(Orddict)) ->
	case .orddict:find(Key, Orddict) of
		{ok, Val} ->
			{ok, Val};
		error ->
			error
	end;
lookup(Key, ?W_RBDICT(Dict)) ->
	case .rbdict:find(Key, Dict) of
		{ok, Val} ->
			{ok, Val};
		error ->
			error
	end.

% crash if no such key
fetch(Key, ?W_GB_TREE(Tree)) ->
	.gb_trees:get(Key, Tree);
fetch(Key, ?W_DICT(Dict)) ->
	.dict:fetch(Key, Dict);
fetch(Key, ?W_RBDICT(Dict)) ->
	.rbdict:fetch(Key, Dict).


% dict:find(Key, Dict) -> {ok, Val} | error

% doesn't crash
erase(Key, ?W_GB_TREE(Tree)) ->
	?W_GB_TREE(.gb_trees:delete_any(Key, Tree));
erase(Key, ?W_DICT(Dict)) ->
	?W_DICT(.dict:erase(Key, Dict));
erase(Key, ?W_ORDDICT(Orddict)) ->
	?W_ORDDICT(.orddict:erase(Key, Orddict));
erase(Key, ?W_RBDICT(Dict)) ->
	?W_RBDICT(.rbdict:erase(Key, Dict)).

% exception if not in tree
delete(Key, ?W_GB_TREE(Tree)) ->
	?W_GB_TREE(.gb_trees:delete(Key, Tree));
delete(Key, ?W_DICT(Dict)) ->
	case .dict:is_key(Dict) of
		true ->
			?W_DICT(.dict:erase(Key, Dict));
		false ->
			exit(no_such_key)
	end;
delete(Key, ?W_ORDDICT(Orddict)) ->
	case .orddict:is_key(Orddict) of
		true ->
			?W_ORDDICT(.orddict:erase(Key, Orddict));
		false ->
			exit(no_such_key)
	end;
delete(Key, ?W_RBDICT(Dict)) ->
	case .rbdict:is_key(Dict) of
		true ->
			?W_RBDICT(.rbdict:erase(Key, Dict));
		false ->
			exit(no_such_key)
	end.

balance(?W_GB_TREE(Tree)) ->
	?W_GB_TREE(.gb_trees:balance(Tree));
balance(?W_RBDICT(_Dict) = D) ->
	D;
balance(D) ->
	D.

is_key(Key, ?W_GB_TREE(Tree)) ->
	.gb_trees:is_defined(Key, Tree);
is_key(Key, ?W_DICT(Dict)) ->
	.dict:is_key(Key, Dict);
is_key(Key, ?W_ORDDICT(Orddict)) ->
	.orddict:is_key(Key, Orddict);
is_key(Key, ?W_RBDICT(Dict)) ->
	.rbdict:is_key(Key, Dict).

fold(Fun, Init, ?W_GB_TREE(Tree)) ->
	Iter = .gb_trees:iterator(Tree),
	gf(Fun, Init, Iter);
fold(Fun, Init, ?W_DICT(Dict)) ->
	.dict:fold(Fun, Init, Dict);
fold(Fun, Init, ?W_ORDDICT(Orddict)) ->
	.orddict:fold(Fun, Init, Orddict);
fold(Fun, Init, ?W_RBDICT(Dict)) ->
	.rbdict:fold(Fun, Init, Dict).

gf(Fun, AccIn, Iter) ->
	case .gb_trees:next(Iter) of
		{Key, Val, Iter2} ->
			AccOut = Fun(Key, Val, AccIn),
			gf(Fun, AccOut, Iter2);
		none ->
			AccIn
	end.

keys(?W_GB_TREE(Tree)) ->
	.gb_trees:keys(Tree);
keys(?W_DICT(Dict)) ->
	.dict:fetch_keys(Dict);
keys(?W_ORDDICT(Orddict)) ->
	.orddict:fetch_keys(Orddict).

map(Fun, ?W_GB_TREE(Tree)) ->
	?W_GB_TREE(.gb_trees:map(Fun, Tree));
map(Fun, ?W_DICT(Dict)) ->
	?W_DICT(.dict:map(Fun, Dict));
map(Fun, ?W_ORDDICT(Dict)) ->
	?W_ORDDICT(.orddict:map(Fun, Dict));
map(Fun, ?W_RBDICT(Dict)) ->
	?W_RBDICT(.rbdict:map(Fun, Dict)).

to_list(?W_GB_TREE(Tree)) ->
	.gb_trees:to_list(Tree);
to_list(?W_DICT(Dict)) ->
	.dict:to_list(Dict);
to_list(?W_ORDDICT(Orddict)) ->
	.orddict:to_list(Orddict);
to_list(?W_RBDICT(Dict)) ->
	.rbdict:to_list(Dict).

size(?W_GB_TREE(Tree)) ->
	.gb_trees:size(Tree);
size(?W_DICT(Dict)) ->
	.dict:size(Dict);
size(?W_ORDDICT(Orddict)) ->
	.orddict:size(Orddict).

from_orddict(gb_trees, List) ->
	?W_GB_TREE(.gb_trees:from_orddict(List)); % must not contain duplicate keys
from_orddict(dict, List) ->
	?W_DICT(.dict:from_list(List));
from_orddict(orddict, List) ->
	?W_ORDDICT(.orddict:from_list(List)).

% assumes Key in Tree
update(Key, Fun, ?W_GB_TREE(_Tree) = T) when is_function(Fun, 1) ->
	OldVal = fetch(Key, T),
	NewVal = Fun(OldVal),
	update_val(Key, NewVal, T);
% exception if no Key in Dict
update(Key, Fun, ?W_DICT(Dict)) ->
	?W_DICT(.dict:update(Key, Fun, Dict));
update(Key, Fun, ?W_ORDDICT(Orddict)) ->
	?W_ORDDICT(.orddict:update(Key, Fun, Orddict)).

update_val(Key, Val, ?W_GB_TREE(Tree)) ->
	?W_GB_TREE(.gb_trees:update(Key, Val, Tree));
update_val(Key, Val, ?W_DICT(_Dict) = D) ->
	update(Key, fun(_Old) -> Val end, D);
update_val(Key, Val, ?W_ORDDICT(_Dict) = D) ->
	update(Key, fun(_Old) -> Val end, D).

update_counter(Key, Increment, ?W_GB_TREE(_Tree) = T) ->
	update(Key, fun(Old) -> Old+Increment end, T);
update_counter(Key, Increment, ?W_DICT(Dict)) ->
	?W_DICT(.dict:update_counter(Key, Increment, Dict));
update_counter(Key, Increment, ?W_ORDDICT(Orddict)) ->
	?W_ORDDICT(.orddict:update_counter(Key, Increment, Orddict)).

% similar to dict:update/4, but instead to Initial, we have FunInit,
% which is called/computed  only we actually need Initial. Can save use
% some time, if calculation of Initial  is costly.
update_full(Key, Fun, FunInit, ?W_GB_TREE(Tree) = T) when is_function(Fun, 1), is_function(FunInit, 0) ->
	case .gb_trees:is_defined(Key, Tree) of
		true ->
			update(Key, Fun, T);
		false ->
			Val = FunInit(),
			?W_GB_TREE(.gb_trees:insert(Key, Val, Tree))
	end;
update_full(Key, Fun, FunInit, ?W_DICT(Dict)) when is_function(Fun, 1), is_function(FunInit, 0) ->
	case .dict:is_key(Key, Dict) of
		true ->
			?W_DICT(.dict:update(Key, Fun, Dict));
		false ->
			Val = FunInit(),
			?W_DICT(.dict:store(Key, Val, Dict))
	end;
update_full(Key, Fun, FunInit, ?W_ORDDICT(Orddict)) when is_function(Fun, 1), is_function(FunInit, 0) ->
	case .orddict:is_key(Key, Orddict) of
		true ->
			?W_ORDDICT(.orddict:update(Key, Fun, Orddict));
		false ->
			Val = FunInit(),
			?W_ORDDICT(.orddict:store(Key, Val, Orddict))
	end.
