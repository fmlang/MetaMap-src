/* c_morph.c - the C side of the Prolog interface to the morphology module.
*/

#include <stdio.h>
#include "sicstus/sicstus.h"
#include "im.h"
#include "dm.h"
#include "qp_morph_glue.h"

extern DmStruct *dm_variants(const char *term, dm_t cats, int *numV);

/* global data */
static struct {
    char *strCat;
    im_t imCat;
    dm_t dmCat;
}   catArray[] = {
	{ "adj",	IM_CAT_ADJ,	DM_CAT_ADJ   },
	{ "adv",	IM_CAT_ADV,	DM_CAT_ADV   },
	{ "aux",	(im_t) 0,	DM_CAT_AUX   },
	{ "compl",	(im_t) 0,	DM_CAT_COMPL },
	{ "conj",	(im_t) 0,	DM_CAT_CONJ  },
	{ "det",	(im_t) 0,	DM_CAT_DET   },
	{ "modal",	(im_t) 0,	DM_CAT_MODAL },
	{ "noun",	IM_CAT_NOUN,	DM_CAT_NOUN  },
	{ "prep",	(im_t) 0,	DM_CAT_PREP  },
	{ "pron",	(im_t) 0,	DM_CAT_PRON  },
	{ "verb",	IM_CAT_VERB,	DM_CAT_VERB  }
};

static int nCatArray = 11;

/* With SICStus Prolog's splfr, Per Mildner recommended that we remove
 * all function prototypes for foreign functions from our `C' code.
 * long int c_dm_variants();
 */

static int get_cats(SP_term_ref pCats, im_t *imCats, dm_t *dmCats);
static char *dm_get_cat(dm_t cat);

/* derivational variants */
long int
c_dm_variants(
	      const char *term,     /* +string */
	      SP_term_ref pCats,    /* +term */
	      SP_term_ref pVarList  /* -term */
	      )
{
    im_t temp;
    dm_t cats = 0;
    int n;
    int i;
    char const *cp;
    DmStruct *dms;
    SP_atom pNil = SP_atom_from_string("[]");

    if (!get_cats(pCats, &temp, &cats) || cats == 0)
	return(0);

    SP_put_atom(pVarList, pNil);
    if ((dms = dm_variants(term, cats, &n)) == (DmStruct *)NULL)
	return(1);
    for (i=0; i<n; i++)
    {
	SP_term_ref pCat = SP_new_term_ref();
	SP_atom pVar;
	SP_term_ref pTerm = SP_new_term_ref();
	SP_atom pAtom;

	pVar = SP_atom_from_string((dms+i)->dmVar);
	if ((cp = dm_get_cat((dms+i)->dmCat)) == (char *)NULL)
	    return(0);
	pAtom = SP_atom_from_string(cp);
	SP_put_atom(pCat, pAtom);
	SP_cons_functor(pTerm, pVar, 1, pCat);
	SP_cons_list(pVarList, pTerm, pVarList);
    }
    return(1);
}

/* reads the cat list */
static int
get_cats(
	 SP_term_ref pCats,
	 im_t *imCats,
	 dm_t *dmCats
	 )
{
    SP_term_ref head = SP_new_term_ref();
    SP_term_ref tail = SP_new_term_ref();
    SP_atom atom;
    char const *sCat;
    int i;

    *imCats = *dmCats = 0;
    SP_get_list(pCats, head, tail);
    SP_get_atom(head, &atom);
    sCat = SP_string_from_atom(atom);
    for (i=0; i<nCatArray; i++)
    {
	if (strcmp(sCat, catArray[i].strCat) == 0)
	{
	    *imCats |= catArray[i].imCat;
	    *dmCats |= catArray[i].dmCat;
	    break;
	}
    }

    while (SP_is_list(tail))
    {
	SP_get_list(tail, head, tail);
	SP_get_atom(head, &atom);
	sCat = SP_string_from_atom(atom);
	for (i=0; i<nCatArray; i++)
	{
	    if (strcmp(sCat, catArray[i].strCat) == 0)
	    {
		*imCats |= catArray[i].imCat;
		*dmCats |= catArray[i].dmCat;
		break;
	    }
	}
    }
    return(1);
}

/* maps a dm_t to a string */
static char *
dm_get_cat(
	   dm_t cat
	   )
{
    static char *cats[] = { "adj",  "adv",  "aux",   "compl",
			    "conj", "det",  "modal", "noun",
			    "prep", "pron", "verb",  "unknown"};

    switch (cat)
    {
	case DM_CAT_ADJ:	return(cats[0]);
	case DM_CAT_ADV:	return(cats[1]);
	case DM_CAT_AUX:	return(cats[2]);
	case DM_CAT_COMPL:	return(cats[3]);
	case DM_CAT_CONJ:	return(cats[4]);
	case DM_CAT_DET:	return(cats[5]);
	case DM_CAT_MODAL:	return(cats[6]);
	case DM_CAT_NOUN:	return(cats[7]);
	case DM_CAT_PREP:	return(cats[8]);
	case DM_CAT_PRON:	return(cats[9]);
	case DM_CAT_VERB:	return(cats[10]);
	default:		return(cats[11]);
    }
}
