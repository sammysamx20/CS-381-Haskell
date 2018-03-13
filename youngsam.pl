% Sam Young - youngsam
% Bradford Wong - wongbra
% Benjamin Richards - richaben
% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!

female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).



%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.
child(X, Y) :-  parent(Y,X).


% 2. Define two predicates `isMother/1` and `isFather/1`.
isMother(X) :- parent(X,_), female(X).
isFather(X) :- parent(X,_), male(X).

% 3. Define a predicate `grandparent/2`.
grandparent(X,Y) :- parent(X,Z),parent(Z,Y).
% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
sibling(X,Y) :- child(X,Z),child(Y,Z),X \= Y.

% 5. Define two predicates `brother/2` and `sister/2`.
brother(X,Y) :- sibling(X,Y) , male(X).

sister(X,Y) :- sibling(X,Y), female(X).



% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.
siblingInLaw(X,Y) :-  married(X,Z),sibling(Z,Y).
siblingInLaw(X,Y) :-  married(Y,Z),sibling(Z,X).

% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.
aunt(X,Y) :- female(X),sibling(X,Z), married(Z,A), parent(A,Y).
aunt(X,Y) :- female(X),sibling(X,Z), parent(Z,Y).
aunt(X,Y) :- female(X), siblingInLaw(X,Z), parent(Z,Y).

uncle(X,Y) :- male(X),sibling(X,Z), married(Z,A), parent(A,Y).
uncle(X,Y) :- male(X), sibling(X,Z), parent(Z,Y).
uncle(X,Y) :- male(X), siblingInLaw(X,Z), parent(Z,Y).

% 8. Define the predicate `cousin/2`.
cousin(X,Y) :- parent(A,X), parent(B,Y), sibling(A,B), X\=Y.


% 9. Define the predicate `ancestor/2`.
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(Z,Y), ancestor(X,Z).

% Define the predicate `descendent/2`.
descendent(X,Y) :- child(X,Y).
descendent(X,Y) :- child(Z,Y), descendent(X,Z).

% Extra credit: Define the predicate `related/2`.
related(X,Y) :- ancestor(X,Y).
related(X,Y) :- ancestor(Y,X).
related(X,Y) :- aunt(X,Y).
related(X,Y) :- aunt(Y,X).
related(X,Y) :- uncle(X,Y).
related(X,Y) :- uncle(Y,X).
related(X,Y) :- cousin(X,Y).
related(X,Y) :- siblingInLaw(X,Y).
related(X,Y) :- sibling(X,Y).
related(X,Y) :- married(X,Y).


%%
% Part 2. Language implementation
%%


% 1. Define the predicate `cmd/3`, which describes the effect of executing a
%    command on the stack.
cmd(add,[X1,X2|X3],Y) :- Y = [X1X2|X3], X1X2 is X1 + X2.
cmd(lte,[X1,X2|X3],Y) :- Y = [t|X3], X1 =< X2.
cmd(lte,[X1,X2|X3],Y) :- Y = [f|X3], X1 > X2.
cmd(if(P1,_),[X1|XT],Y) :- X1 = t, prog(P1,XT,Y).
cmd(if(_,P2),[X1|XT],Y) :- X1 = f, prog(P2,XT,Y).
cmd(X,Y,Z) :- number(X), Z = [X|Y].
cmd(X,Y,Z) :- X = t, Z = [X|Y].
cmd(X,Y,Z) :- X = f, Z = [X|Y].
cmd(X,Y,Z) :- string(X), Z = [X|Y].

% 2. Define the predicate `prog/3`, which describes the effect of executing a
%    program on the stack.

prog([X1|X2],Y,Z2) :- cmd(X1,Y,Z1), prog(X2,Z1,Z2).
prog(X,Y,Z) :- cmd(X,Y,Z).
