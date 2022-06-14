Main: function (Sexprs) {
    return `${Sexprs.bq ().join ('')}`;
},

atom: function (a) {
    return `${a.bq ()}`;
},

List_dotted: function (klp, Sexprs1, kdot, Sexpr2, krp) {
    return `(${Sexprs1.bq ().join ('')} . ${Sexpr2.bq ()})`;
},

QuotedSexpr: function (sq, Sexpr) {
    return `'${Sexpr.bq ()}`;
}

string: function (dq1, cs, dq2) {
    return `"${cs.bq ().join ('')}"`;
},

number: function (ds) {
    return ds.bq ().join ('');
},

ws: function (c) {
    return c.bq ();
},

separator: function (c) {
    return c.bq ();
},

dq: function (c) {
    return c.bq ();
},

sq: function (c) {
    return c.bq ();
},

bq: function (c) {
    return c.bq ();
},

symbol: function (symchars) {
    return symchars.bq ().join ('');
},

symchar: function (c) {
    return c.bq ();
},

comment: function (ksemicolon, cs, nl) {
    return `;${cs.bq ().join ('')}${nl.bq ()}`;
},

nl: function (c) {
    return c.bq ();
},

space: function (c) {
    return c.bq ();
},

// bqscheme
Sexpr: function (x) {
    return x.bq ();
},

BQSexpr_bqsplice: function (kcommaat, BQSexpr) {
    return `,@${BQSexpr.bq ()}`;
},

BQSexpr_bqeval: function (kcomma, BQSexpr) {
    return `,${BQSexpr.bq ()}`;
},

BQSexpr_bqlist: function (BQSexpr) {
    return BQSexpr.bq ();
},

BQSexpr_atom: function (a) {
    return a.bq ();
},

BQList_dotted: function (klp, BQSexprs1, kdot, BQSexpr2, krp) {
    return `(${BQSexprs1.bq ().join ('')} . ${BQSexpr2.bq ()})`;
},
BQList_plain: function (klp, BQSexprs, krp) {
    return `(${BQSexprs.bq ().join ('')})`;
},
