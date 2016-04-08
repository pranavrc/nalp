# nalp
A Prolog natural language parser built using Definite Clause Grammar.

## Tests

```
?- write('Enter your sentence?: '), read_atomics(Words), phrase(s(Inv, Voice, Stype, Vform, Agr, Tree),Words).
Enter your sentence?: what house did jack see
Words = [what, house, did, jack, see],
Inv = inv,
Vform = present,
Agr = '3s',
Tree = s(_G1240, np(subjective, q, '3s', pro(what)), s(_G1257,
np('3s', n(count, house)), s('_none', aux(did), np('3s', n
(proper, jack)), vp_intr(v(see))))) .

?- write('Enter your sentence?: '), read_atomics(Words), phrase(s(Inv, Voice, Stype, Vform, Agr, Tree),Words).
Enter your sentence?: who ate the fish
Words = [who, ate, the, fish],
Inv = noinv,
Voice = q,
Vform = past,
Agr = '3s',
Tree = s('_np', np(subjective, q, '3s', pro(who)), vp_tran(v
(ate), np('3s', art(the), n(mass, fish)))) .

?- write('Enter your sentence?: '), read_atomics(Words), phrase(s(Inv, Voice, Stype, Vform, Agr, Tree),Words).
Enter your sentence?: where did jack see the house
Words = [where, did, jack, see, the, house],
Inv = inv,
Vform = present,
Agr = '3s',
Tree = s(_G1255, pp(pp-word(where)), s('_np', aux(did), np('3s',
n(proper, jack)), vp_tran(v(see), np('3s', art(the), n(count,
house))))) .

?‐ write('Enter your sentence?: '), read_atomics(Words), phrase(s(Inv,Voice, Stype, Vform, Agr, Tree),Words).
Enter your sentence?: i can be eating
Words = [i, can, be, eating],Inv = noinv,
Stype = declarative,
Vform = present,
Agr = '1s',
Tree = s('_none', np(subjective, '1s', pro(i)), vp_modal(aux
(modal, can),
vp3(aux(be, be), vp_intr(v(eating))))) .
```
