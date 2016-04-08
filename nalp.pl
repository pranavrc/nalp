/ ** A Prolog natural language interface parser.
  *
  * Pranav Ravichandran <me@onloop.net>
  **/

s --> s(_,_,_,_,_,_,_).
s(noinv, WH, Gap, Vform, Agr, s(Subcat,NP,VP)) --> 
    np(Case,WH,Agr,_,NP), 
    {member(Vform,[present,past,future])},
    vp(_, _, Subcat, Agr, Gap, Vform, VP).

s(inv, WH, Gap, Vform, Agr, s(Subcat,Aux,NP,VP)) -->
    aux(_,Agr,Vform,Compform,Root,Aux),
    np(Case,WH,Agr,nogap,NP),
    vp(_,_,Subcat,Agr,Gap,Compform,VP).

s(inv, WH, Gap, Vform, Agr, s(Subcat,NP,S)) -->
    np(Case,q,Agr,nogap,NP),
    s(inv,WH,np,Vform,Agr,S).

s(inv, WH, Gap, Vform, Agr, s(Subcat,PP,S)) -->
    pp(q,Pform,nogap,PP),
    s(inv,WH,pp,Vform,Agr,S).

stype(imperative),[you] --> [].
stype(declarative) --> [].

np(_,_,Agr,_,np(Agr,Art,N)) --> art(Agr,Art), n(Agr,N).
np(_,_,Agr,_,np(Agr,N)) --> n(Agr,N).
np(Case,WH,Agr,_,np(Case,WH,Agr,Pro)) --> pro(Case,WH,Agr,Pro).
np(subjective,WH,Agr,_,np(Det,N)) --> det(WH,Agr,Det), cnp(_,_,Agr,N).
np(_,WH,Agr,_,np(Agr,Art,Adjp,N)) --> art(Agr,Art), adjp(_,Adjp), n(Agr,N).

cnp(_,_,Agr,cnp(Agr,N)) --> n(Agr,N).
cnp(_,_,Agr,cnp(Agr,Adjp,N)) --> adjp(_,Adjp), n(Agr,N).

det(_,_,det(Det)) --> art(Agr,Det).
det(WH,_,det(Det)) --> np(possessive,WH,_,_,Det).
det(WH,_,det(Det)) --> qdet(WH,Agr,Det).

pp(WH,Pform,Gap,pp(PP,NP)) --> p(PP), np(_,WH,_,_,NP).
pp(WH,Pform,Gap,pp(PP)) --> pp-word(WH, Pform, PP).

vp(PG,Cat,Subcat,Agr,Gap,Compform,vp_modal(aux(Modal,Aux),VP)) --> 
	aux(Modal,Agr,_,Compform,Root,Aux),
	vp(PG,Cat,Subcat,Agr,Gap,Compform,VP).
vp(PG,Cat,Subcat,Agr,Gap,Vform,vp3(aux(be,Aux),VP)) -->
	aux(be,Agr,Vform,Compform,be,Aux),
	vp(PG,main,Subcat,Agr,Gap,ing,VP).
vp(PG,Cat,Subcat,Agr,Gap,Vform,vp4(aux(be,Aux),VP)) -->
	aux(be,Agr,Vform,Compform,be,Aux),
	vp(PG,passive,Cat,Subcat,Agr,Gap,ing,VP).
vp(PG,passive,Cat,Subcat,Agr,Gap,Vform,vp5(aux(be,Aux),VP)) -->
	aux(be,Agr,Vform,Compform,be,Aux),
	vp('+passgap',passive,main,Subcat,Agr,Gap,pastprt,VP).
vp('-passgap',main,'_none',Agr,Gap,Vform,vp_intr(V)) --> v(main,'_none',Agr,Vform,V).
vp('-passgap',main,'_np',Agr,Gap,Vform,vp_tran(V,NP)) --> v(main,'_np',Agr,Vform,V), np(objective,_,_,Gap,NP).
vp('+passgap',main,'_np',Agr,Gap,Vform,vp_pass(V)) --> v(main,'_np',Agr,Vform,V).

vp(PG, active, main, '_none', Agr, Gap, Vform, vp(V)) --> v(main, '_none', Agr, Vform, V).
vp(PG, active, main, '_np', Agr, Gap, Vform, vp(V,NP))--> v(main, '_np', Agr, Vform, V), np(objective, _, _, Gap, NP).
vp(PG, active, main, '_vp:inf', Agr, Gap, Vform, vp(V,VP))--> v(main, '_vp:inf', Agr, Vform, V), vp(active, 'inf', Agr, Gap, Vform, VP).
vp(PG, active, main, 'inf', Agr, Gap, _, vp(V,VP))-->to(V), vp(main, '_none', Agr, Gap, 'present', VP).
vp(PG, active, main, 'inf', Agr, Gap, _, vp(V,VP))-->to(V), vp(main, '_none', Agr, Gap, base, VP).
vp(PG, active, main, '_np_vp:inf', Agr, Gap, Vform, vp(V,NP,VP))--> v(main, '_np_vp:inf', Agr, Vform, V), np(objective, _, _, Gap, NP), vp(active, 'inf', Agr, nogap, Vform, VP).
vp(PG, active, main, '_np_vp:inf', Agr, Gap, Vform, vp(V,NP,VP))--> v(main, '_np_vp:inf', Agr, Vform, V), np(objective, _, _, nogap, NP), vp(active, 'inf', Agr, Gap, Vform, VP).
vp(PG, active, main, '_np_pp:loc', Agr, Gap, Vform, vp(V,NP,VP))--> v(main, '_np_pp:loc', Agr, Vform, V), np(objective, _, _, Gap, NP), pp(_, 'loc', nogap, VP).
vp(PG, active, main, '_np_pp:loc', Agr, Gap, Vform, vp(V,NP,VP))--> v(main, '_np_pp:loc', Agr, Vform, V), np(objective, _, _, nogap, NP), pp(_, 'loc', Gap, VP).
vp(PG, active, main, '_adjp', Agr, Gap, Vform, vp(V,VP))--> v(main, '_adjp', Agr, Vform, V), adjp(_, VP).

adjp(_, adjp(A))-->adj(_, A).
adjp('_vp:inf', adjp(A,VP))-->adj('_vp:inf', A), vp(PG, active, 'inf', _, _, base, VP).



% LEXICON

aux(Modal,Agr,Vform,Compform,Root,aux(Aux)) --> [Aux], 
   {is_aux(Modal,Agr,Vform,Compform,Root,Aux)}.
is_aux(modal,_,present,base,can,can).
is_aux(modal,_,Vform,base,could,could):-member(Vform,['present','past']).
is_aux(modal,Agr,present,base,do,do):-member(Agr,['1s','2s','1p','2p','3p']).
is_aux(modal,_,Vform,base,did,did):-member(Vform,['present','past']).
is_aux(modal,_,future,base,will,will).
is_aux(be,_,_,base,be,be).
is_aux(be,_,past,base,be,was).
is_aux(be,_,present,base,be,is).
is_aux(be,_,_,base,be,being).
is_aux(have,_,base,have,have).
is_aux(have,_,base,have,had).
is_aux(have,_,base,have,has).
is_aux(have,_,base,have,having).

art(Agr,art(D)) --> [D],{is_art(D,Agr)}.
is_art(the,Agr):-member(Agr,['3s','3p']).
is_art(a,'3s').

adj(Subcat,adj(D)) --> [D],{is_adj(D,Subcat)}.
is_adj(happy,Subcat):-member(Subcat,['_vp:inf']).

pro(Case,WH,Agr,pro(Pro)) --> [Pro],{is_pro(Case,WH,Agr,Pro)}.
is_pro(subjective,_,'3s',he).
is_pro(subjective,_,'1s',i).
is_pro(subjective,q,Agr,what) :- member(Agr,['3s','3p']).
is_pro(subjective,WH,Agr,who) :- member(Agr,['3s','3p']), member(WH,['q','r']).
is_pro(subjective,r,Agr,which) :- member(Agr,['3s','3p']).
is_pro(possessive,WH,Agr,whose) :- member(Agr,['3s','3p']), member(WH,['q','r']).

pp-word(WH,Pform,pp-word(PPW)) --> [PPW],{is_ppwrd(WH,Pform,PPW)}.
is_ppwrd(WH,time,when) :- member(WH,['q','r']).
is_ppwrd(WH,Pform,where) :- member(WH,['q','r']), member(Pform, ['loc','mot']).

p(p(P)) --> [P],{is_p(P)}.
is_p(in).
is_p(at).

qdet(WH,Agr,qdet(QDET)) --> [QDET],{is_qdet(WH,Agr,QDET)}.
is_qdet(q,Agr,what) :- member(Agr, ['3s','3p']).
is_qdet(q,Agr,which) :- member(Agr, ['3s','3p']).

to(to(to)) --> [to].

n(Agr,n(Ntype,N)) --> [N],{is_name(N,Ntype,Agr);is_noun(N,Ntype,Agr)}.
is_name(jack,proper,'3s').
is_name(sue,proper,'3s').
is_noun(saw,count,'3s').
is_noun(men,count,'3p').
is_noun(man,count,'3s').
is_noun(dog,count,'3s').
is_noun(fish,mass,Agr):-member(Agr,['3s','3p']),irreg-pl(fish).
is_noun(seed,count,'3s').
is_noun(house,count,'3s').

v(Cat,Subcat,Agr,Vform,v(V)) --> [V], {is_verb(Vform, main, Subcat, V, Agr)}.
is_verb(present,main,Subcat,be,Agr):-member(Agr,['1s','2s','1p','2p','3s','3p']),member(Subcat,['_none','_adjp','_np']),irreg-pres(be),irreg-past(be).
is_verb(present,main,Subcat,saw,Agr):-member(Agr,['1s','2s','1p','2p','3p']),member(Subcat,['_none','_np']).
is_verb(past,main,Subcat,saw,_):-member(Subcat,['_none','_np']).
is_verb(present,main,Subcat,cry,Agr):-member(Agr,['1s','2s','1p','2p','3p']),member(Subcat,['_none']).
is_verb(present,main,Subcat,see,Agr):-member(Agr,['1s','2s','1p','2p','3p']),member(Subcat,['_none','_np']),irreg-past(see),en-pastprt(see).
is_verb(present,main,Subcat,eat,Agr):-member(Agr,['1s','2s','1p','2p','3p']),member(Subcat,['_none','_np']).
is_verb(present,main,Subcat,want,Agr):-member(Agr,['1s','2s','1p','2p','3p']),member(Subcat,['_none','_np','_vp:inf','_np_vp:inf']).
is_verb(base,main,Subcat,cry,Agr):-member(Agr,['1s','2s','1p','2p','3p']),member(Subcat,['_none']).
is_verb(base,main,Subcat,see,Agr):-member(Agr,['1s','2s','1p','2p','3p']),member(Subcat,['_none','_np']),irreg-past(see),en-pastprt(see).
is_verb(base,main,Subcat,want,Agr):-member(Agr,['1s','2s','1p','2p','3p']),member(Subcat,['_none','_np','_vp:inf','_np_vp:inf']).
is_verb(present,main,Subcat,is,'3s'):-member(Subcat,['_none','_np']).
is_verb(past,main,Subcat,was,Agr):-member(Agr,['1s','3s']),member(Subcat,['_none','_adjp','_np']).
is_verb(past,main,Subcat,were,Agr):-member(Agr,['2s','1p','2p','3p']),member(Subcat,['_none','_adjp','_np']).
is_verb(ing,main,Subcat,watching,_).
is_verb(present,main,Subcat,eat,'3p'):- member(Subcat,['_none','_np']).
is_verb(present,main,Subcat,eats,'3s'):- member(Subcat,['_none','_np']).
is_verb(past,main,Subcat,ate,_):- member(Subcat,['_none','_np']).
is_verb(base,main,Subcat,see,_):-member(Subcat,['_none','_np']). 
is_verb(base,main,Subcat,eat,_):-member(Subcat,['_none','_np']).
is_verb(ing,main,Subcat,eating,_):-member(Subcat,['_none','_np']).
is_verb(present,main,Subcat,can,_):- member(Subcat,['_none']).
is_verb(pastprt,main,Subcat,eaten,_):-member(Subcat,['_none','_np']).
is_verb(pastprt,main,Subcat,seen,_):-member(Subcat,['_none','_np']).
is_verb(pastprt,main,Subcat,heard,_):-member(Subcat,['_none','_np']).
is_verb(pastprt,main,Subcat,watched,_):-member(Subcat,['_none','_np']).

irreg-pl(fish).
irreg-past(see).
irreg-past(be).
irreg-pres(be).
en-pastprt(see).

