#include <stdio.h>
#include <string.h>
#include <stdlib.h>
//#include <dmalloc.h>

#define PPREFIX	"++"

#define DEBUGLEVEL	2

#define PARSE_MAX_FUNC_ARGS	128

#define dprintf(lev, fmt, args...)   if(lev <= DEBUGLEVEL) printf(fmt, ## args)

char *handle_apply(FILE *fi, int level, int inif, int ineval, int *indexer, int *isamatrix, char *dname);

static FILE *fohead;
static FILE *fofunc;

char *get1stchar(char *istr) 
{
  int len= strlen(istr);
  int offset= strspn(istr, " \t\n\r");

  if (offset < len) {
    return(istr + offset);
  }

  return(NULL);
}

int get_id2(char *istr, char **first, char **second) {
  int len= strlen(istr);
  int soffset= strcspn(istr, ">") + 1;
  int eoffset;
  char *subscript= NULL;
  char *buffer= NULL;

  if (soffset >= len) return(-1);

  buffer= (char *) calloc(1024, sizeof(char));

  if (!buffer) return(-1);

  if (!strncmp(istr+1, "subscript=\"", 11)) {
    int broffset= strcspn(istr + 12, "\"");
    if (broffset < soffset - 1) {
      istr[12 + broffset]= 0;
      subscript= strdup(istr + 12);
      istr[12 + broffset]= '\"';
    }
  }

  eoffset= strcspn(istr + soffset, "<");
  if (eoffset + soffset < len) {
    *(istr + eoffset + soffset)= 0;
  }

  if (!subscript) {
    strcpy(buffer, istr + soffset);
  } else {
    sprintf(buffer, "%s_%s", istr + soffset, subscript);
    free(subscript);
  }

  if (!*first) {
    *first= buffer;
    dprintf(1, PPREFIX"first: %s\n", *first);
    return(0);
  } else {
    *second= buffer;
    dprintf(1, PPREFIX"second: %s\n", *second);
    return(1);
  }

  return(-1);
}

int get_mdim(char *istr, int *rows, int *cols) {
  int len= strlen(istr);
  char *roffset= strstr(istr, "rows=\"");
  char *coffset= strstr(istr, "cols=\"");
  char *rend, *cend;
  char tch;

  if (!roffset || !coffset) return(-1);

  rend= strchr(roffset + 6, '\"');
  cend= strchr(coffset + 6, '\"');

  if (!rend || !cend) return(-1);

  tch= *rend;
  *rend= 0;
  *rows= (int) strtoul(roffset + 6, (char **) NULL, 0);
  *rend= tch;
  tch= *cend;
  *cend= 0;
  *cols= (int) strtoul(coffset + 6, (char **) NULL, 0);
  *cend= tch;

  return(0);
}

#define APPLY_UNKN    0
#define APPLY_INDEXER 1
#define APPLY_EQUAL   2
#define APPLY_PLUS    3
#define APPLY_MINUS   4
#define APPLY_MULT    5
#define APPLY_DIV     6
#define APPLY_POW     7
#define APPLY_NEG     8
#define APPLY_AND     9
#define APPLY_GEQ     10
#define APPLY_LEQ     11
#define APPLY_LT      12
#define APPLY_GT      13
#define APPLY_EVAL    14
#define APPLY_TANH    15

#define MATRIX_TYPE_UNKNW	0
#define MATRIX_TYPE_SCALAR	1
#define MATRIX_TYPE_VAR		2
#define MATRIX_TYPE_FUNC	3

char *handle_matrix(FILE *fi, char *sdim, int level, char *vname)
{
  int rows, cols;
  int iret= get_mdim(sdim, &rows, &cols);
  int mtype= MATRIX_TYPE_UNKNW;
  char *istr, *buffer= (char *) malloc(1024);
  char *stret= NULL;
  char *idname= NULL;
  int i= 0, j= 0;
  int indexer= 0;
  int isamatrix= 0;

  dprintf(1, PPREFIX"ml:matrix(lev%d): %d %d %d\n", level, iret, rows, cols);

  if (!iret && rows && cols) {

    while (istr= fgets(buffer, 1024, fi)) {
      dprintf(2, "::%s", istr);
      if (istr= get1stchar(istr)) {
	if (!strncmp(istr, "<ml:id", 6)) {
	  dprintf(1, PPREFIX"ml:id(matrix): %s\n", istr);
	  get_id2(istr + 6, &idname, &idname);
	  mtype= MATRIX_TYPE_VAR;
	  if (!stret) {
	    stret= (char*) malloc(10240);
	    *stret= 0;
	  }
	  sprintf(stret + strlen(stret), "%s[%d][%d]= %s; ", vname, j, i, idname);
	  if (j+1 == rows) sprintf(stret + strlen(stret), "\n");
	} else if (!strncmp(istr, "<ml:apply", 9)) {
	  char *stret4= handle_apply(fi, level + 1, 0, 0, &indexer, &isamatrix, "dummy");
	  dprintf(1, PPREFIX"ml:apply(matrix): %s\n", stret4);
	  mtype= MATRIX_TYPE_FUNC;
	  if (!stret) {
	    stret= (char *) malloc(10240);
	    *stret= 0;
	  }
	  sprintf(stret + strlen(stret), "%s[%d][%d]= %s; ", vname, j, i, stret4);
	  if (j+1 == rows) sprintf(stret + strlen(stret), "\n");
	} else if (!strncmp(istr, "<ml:real", 7)) {
	  dprintf(1, PPREFIX"ml:real(matrix): %s\n", istr);
	  get_id2(istr + 7, &idname, &idname);
	  mtype= MATRIX_TYPE_SCALAR;
	  if (!stret) {
	    stret= (char *) malloc(10240);
	    *stret= 0;
	  }
	  sprintf(stret + strlen(stret), "%s[%d][%d]= %s; ", vname, j, i, idname);
	  if (j+1 == rows) sprintf(stret + strlen(stret), "\n");
	} else if (!strncmp(istr, "</ml:matrix", 11)) {
	  //	  dprintf(1, PPREFIX"i=%d j=%d\n", i, j);
	  break;
	}
	if (j < rows) j++;
	if (j == rows) {
	  j= 0;
	  i++;
	}
	dprintf(1, PPREFIX"i=%d j=%d\n", i, j);
      }
    }
  }

  if (i == cols) return(stret);

  printf(PPREFIX"MATRIX ERROR(%d): empty matrix\n", level);
  return(NULL);
}

char *handle_return(FILE *fi, int level, int infunc)
{
  char *istr, *buffer= (char *) malloc(1024);
  char *stret= NULL;
  char *idname= NULL;

  dprintf(1, PPREFIX"ml:return(lev%d):\n", level);

  while (istr= fgets(buffer, 1024, fi)) {
    dprintf(2, "::%s", istr);
    if (istr= get1stchar(istr)) {
      if (!strncmp(istr, "<ml:id", 6)) {
	dprintf(1, PPREFIX"ml:id(name): %s\n", istr);
	get_id2(istr + 6, &idname, &idname);
	if (!stret) {
	  stret= (char *) malloc(1024);
	  *stret= 0;
	}
	if (infunc)
	  sprintf(stret + strlen(stret), "return(%s);\n", idname);
	else
	  sprintf(stret + strlen(stret), "(%s)", idname);
	break;
      } 
    }
  }

  return(stret);
}

char *handle_parens(FILE *fi, int level)
{
  char *istr, *buffer= (char *) malloc(1024);
  char *stret= NULL;
  int  indexer= 0;
  int  isamatrix= 0;

  dprintf(1, PPREFIX"ml:parens(lev%d):\n", level);

  while (istr= fgets(buffer, 1024, fi)) {
    dprintf(2, "::%s", istr);
    if (istr= get1stchar(istr)) {
      if (!strncmp(istr, "<ml:apply", 9)) {
	char *stret2= handle_apply(fi, level + 1, 0, 0, &indexer, &isamatrix, "dummy");
	dprintf(1, PPREFIX"ml:apply-f(lev%d): ret %s\n", level, stret2);
	if (!stret) {
	  stret= (char *) malloc(1024);
	  *stret= 0;
	}
	sprintf(stret + strlen(stret), "(");
	strcat(stret, stret2);
      } else if (!strncmp(istr, "</ml:parens", 11)) {
	dprintf(1, PPREFIX"ml:parens close\n");
	strcat(stret, ")");
	break;
      }
    }
  }

  return(stret);
}

char *handle_function(FILE *fi, int level) 
{
  char *istr, *buffer= (char *) malloc(1024);
  char *fname= NULL;
  char **fargs= (char **) calloc(PARSE_MAX_FUNC_ARGS, sizeof(char*));
  char *stret= NULL;
  int   bvars= 0, nargs= 0, i;
  int   indexer= 0;
  int   isamatrix= 0;

  dprintf(1, PPREFIX"ml:function(lev%d):\n", level);

  while (istr= fgets(buffer, 1024, fi)) {
    dprintf(2, "::%s", istr);
    if (istr= get1stchar(istr)) {
      if (!fname && !strncmp(istr, "<ml:id", 6)) {
	dprintf(1, PPREFIX"ml:id(name): %s\n", istr);
	get_id2(istr + 6, &fname, &fname);
	break;
      }
    }
  }

  if (!istr) {
    free(buffer);
    return(NULL);
  }

  while (istr= fgets(buffer, 1024, fi)) {
    dprintf(2, "::%s", istr);
    if (istr= get1stchar(istr)) {
      if (!strncmp(istr, "<ml:id", 6) && nargs < PARSE_MAX_FUNC_ARGS) {
	dprintf(1, PPREFIX"ml:id: %s\n", istr);
	if (bvars) {
	  if (get_id2(istr + 6, &fargs[nargs], NULL) == 0) nargs++;
	  else 
	    dprintf(1, PPREFIX"FAULTY boundVar!\n");
	} else get_id2(istr + 6, &fname, NULL);
      } else if (!strncmp(istr, "<ml:apply", 9)) {
	char *stret2= handle_apply(fi, level + 1, 0, 0, &indexer, &isamatrix, "dummy");
	dprintf(1, PPREFIX"ml:apply-f(lev%d): ret %s\n", level, stret2);
	if (bvars) {
	  fargs[nargs++]= stret2;
	} else printf("NESTED F-APPLY ERROR\n");
      } else if (!strncmp(istr, "<ml:function", 12)) {
	char *stret2= handle_function(fi, level + 1);
	dprintf(1, PPREFIX"ml:function-f(lev%d): ret %s\n", level, stret2);
	if (bvars) {
	  fargs[nargs++]= stret2;
	} else printf("NESTED F-FUNCTION ERROR\n");
      } else if (!strncmp(istr, "<ml:boundVars", 13)) {
	dprintf(1, PPREFIX"ml:bVars:\n");
	bvars= 1;
      } else if (!strncmp(istr, "</ml:boundVars>", 11)) {
	if (fname) {
	  int slen= strlen(fname) + 2;
	  dprintf(1, PPREFIX"/Func %s\n", fname);
	  for(i= 0; i< nargs; i++) {
	    slen+= strlen(fargs[i]) + 2;
	  }
	  stret= (char *) malloc(slen + 512);
	  sprintf(stret, "%s(", fname);
	  for(i= 0; i< nargs; i++) {
	    if (i < nargs - 1) sprintf(buffer, "double %s, ", fargs[i]);
	    else sprintf(buffer, "double %s", fargs[i]);
	    strcat(stret, buffer);
	  }
	  sprintf(buffer, ")");
	  strcat(stret, buffer);
	  if (!level) printf("%s\n", stret);
	} else {
	  dprintf(1, PPREFIX"FAULTY FUNCTION\n");
	}
	free(buffer);
	for(i= 0; i< nargs; i++) {
	  free(fargs[i]);
	}
	return(stret);
      }
    }    
  }

  free(buffer);
  for(i= 0; i< nargs; i++) {
    free(fargs[i]);
  }
  return(NULL);
}

char *handle_sequence(FILE *fi, int level, int inif, int ineval) 
{
  char *istr, *buffer= (char *) malloc(1024);
  char **sarray= (char **) calloc(PARSE_MAX_FUNC_ARGS, sizeof(char*));
  char *stret= NULL;
  int   nargs= 0, i;
  int   indexer= 0;
  int   isamatrix= 0;

  dprintf(1, PPREFIX"handle_sequence(lev%d):\n", level);

  while (istr= fgets(buffer, 1024, fi)) {
    dprintf(2, "::%s", istr);
    if (istr= get1stchar(istr)) {
      if (!strncmp(istr, "<ml:id", 6) && nargs < PARSE_MAX_FUNC_ARGS) {
	dprintf(1, PPREFIX"ml:id: %s\n", istr);
	if (get_id2(istr + 6, &sarray[nargs], NULL) == 0) nargs++;
	else 
	  dprintf(1, PPREFIX"FAULTY boundVar!\n");
      } else if (!strncmp(istr, "<ml:real", 8) && nargs < PARSE_MAX_FUNC_ARGS) {
	dprintf(1, PPREFIX"ml:real: %s\n", istr);
	if (get_id2(istr + 6, &sarray[nargs], NULL) == 0) nargs++;
	else 
	  dprintf(1, PPREFIX"FAULTY seq!\n");
      } else if (!strncmp(istr, "<ml:apply", 9)) {
	char *stret2= handle_apply(fi, level + 1, inif, ineval, &indexer, &isamatrix, "dummy");
	dprintf(1, PPREFIX"ml:apply-f(lev%d): ret %s\n", level, stret2);
	sarray[nargs++]= stret2;
      } else if (!strncmp(istr, "<ml:function", 12)) {
	char *stret2= handle_function(fi, level + 1);
	dprintf(1, PPREFIX"ml:function-f(lev%d): ret %s\n", level, stret2);
	sarray[nargs++]= stret2;
      } else if (!strncmp(istr, "</ml:sequence>", 14)) {
	int slen= 2;
	for(i= 0; i< nargs; i++) {
	  slen+= strlen(sarray[i]) + 2;
	}
	stret= (char *) malloc(slen + 512);
	sprintf(stret, "(");
	for(i= 0; i< nargs; i++) {
	  if (i < nargs - 1) sprintf(buffer, "%s, ", sarray[i]);
	  else sprintf(buffer, "%s", sarray[i]);
	  strcat(stret, buffer);
	}
	sprintf(buffer, ")");
	strcat(stret, buffer);
	if (!level) printf("%s\n", stret);
	free(buffer);
	for(i= 0; i< nargs; i++) {
	  free(sarray[i]);
	}
	return(stret);
      }
    }    
  }

  free(buffer);
  for(i= 0; i< nargs; i++) {
    free(sarray[i]);
  }
  return(NULL);
}


char *handle_apply(FILE *fi, int level, int inif, int ineval, int *indexer, int *isamatrix, char *dname) 
{
  char *istr, *buffer;
  char *first= NULL;
  char *second= NULL;
  char *stret= NULL;
  int  action= APPLY_UNKN;
  int  sequence= 0;

  dprintf(1, PPREFIX"ml:apply(lev%d):\n", level);
  fflush(0);

  buffer= (char *) malloc(1024);
 
  if (!ineval) {
    while (istr= fgets(buffer, 1024, fi)) {
      dprintf(2, "::%s", istr);
      if (istr= get1stchar(istr)) {
	if (!strncmp(istr, "<ml:indexer", 11)) {
	  dprintf(1, PPREFIX"ml:indexer: %s\n", istr);
	  action= APPLY_INDEXER;
	  *indexer= 1;
	  break;
	} else if (!strncmp(istr, "<ml:greaterOrEqual", 19)) {
	  dprintf(1, PPREFIX"ml:greaterOrequal: %s\n", istr);
	  action= APPLY_GEQ;
	  break;
	} else if (!strncmp(istr, "<ml:lessThan", 12)) {
	  dprintf(1, PPREFIX"ml:lessThan: %s\n", istr);
	  action= APPLY_LT;
	  break;
	} else if (!strncmp(istr, "<ml:greaterThan", 15)) {
	  dprintf(1, PPREFIX"ml:greaterThan: %s\n", istr);
	  action= APPLY_GT;
	  break;
	} else if (!strncmp(istr, "<ml:lessOrEqual", 15)) {
	  dprintf(1, PPREFIX"ml:lessOrequal: %s\n", istr);
	  action= APPLY_LEQ;
	  break;
	} else if (!strncmp(istr, "<ml:greaterOrEqual", 15)) {
	  dprintf(1, PPREFIX"ml:greaterOrequal: %s\n", istr);
	  action= APPLY_GEQ;
	  break;
	} else if (!strncmp(istr, "<ml:equal", 9)) {
	  dprintf(1, PPREFIX"ml:equal: %s\n", istr);
	  action= APPLY_EQUAL;
	  break;
	} else if (!strncmp(istr, "<ml:plus", 8)) {
	  dprintf(1, PPREFIX"ml:plus: %s\n", istr);
	  action= APPLY_PLUS;
	  break;
	} else if (!strncmp(istr, "<ml:minus", 9)) {
	  dprintf(1, PPREFIX"ml:MINUS: %s\n", istr);
	  action= APPLY_MINUS;
	  break;
	} else if (!strncmp(istr, "<ml:div", 7)) {
	  dprintf(1, PPREFIX"ml:div: %s\n", istr);
	  action= APPLY_DIV;
	  break;
	} else if (!strncmp(istr, "<ml:mult", 8)) {
	  dprintf(1, PPREFIX"ml:mult: %s\n", istr);
	  action= APPLY_MULT;
	  break;
	} else if (!strncmp(istr, "<ml:and", 7)) {
	  dprintf(1, PPREFIX"ml:and: %s\n", istr);
	  action= APPLY_AND;
	  break;
	} else if (!strncmp(istr, "<ml:neg", 7)) {
	  dprintf(1, PPREFIX"ml:neg:\n");
	  action= APPLY_NEG;
	  break;
	} else if (!strncmp(istr, "<ml:pow", 7)) {
	  dprintf(1, PPREFIX"ml:pow: %s\n", istr);
	  action= APPLY_POW;
	  break;
	} else if (!strncmp(istr, "<ml:tanh", 8)) {
	  dprintf(1, PPREFIX"ml:tanh: %s\n", istr);
	  action= APPLY_TANH;
	  break;
	} else if (!strncmp(istr, "<ml:id", 6)) {
	  dprintf(1, PPREFIX"ml:id: %s\n", istr);
	  get_id2(istr + 6, &first, NULL);
	  action= APPLY_UNKN;
	  break;
	}
      }
    }

    if (!istr) {
      free(buffer);
      return(NULL);
    }
  } else { //ineval
    action= APPLY_EVAL;
  }

  while (istr= fgets(buffer, 1024, fi)) {
    dprintf(2, "::%s", istr);
    if (istr= get1stchar(istr)) {
      if (!strncmp(istr, "<ml:id", 6)) {
	dprintf(1, PPREFIX"ml:id: %s\n", istr);
	get_id2(istr + 6, &first, &second);
      } else if (!strncmp(istr, "<ml:real", 8)) {
	dprintf(1, PPREFIX"ml:real: %s\n", istr);
	get_id2(istr + 8, &first, &second);
      } else if (!strncmp(istr, "<ml:matrix", 10)) {
	char *smatr;
	if (!first && !dname)
	  smatr= handle_matrix(fi, istr + 10, level + 1, "DUMMY");
	else if (!first)
	  smatr= handle_matrix(fi, istr + 10, level + 1, dname);
	else
	  smatr= handle_matrix(fi, istr + 10, level + 1, first);
	dprintf(1, PPREFIX"ml:matrix:\n");
	second= smatr;
	*isamatrix= 1;
      } else if (!strncmp(istr, "<ml:parens", 10)) {
	char *stret4;
	dprintf(1, PPREFIX"ml:parens open\n");
	stret4= handle_parens(fi, level + 1);
	if (!first) first= stret4;
	else if (!second) second= stret4;
	else printf("PARENS ERROR\n");
      } else if (!strncmp(istr, "</ml:pow", 7)) {
	dprintf(1, PPREFIX"ml:pow close\n");
      } else if (!strncmp(istr, "<ml:apply", 9)) {
	int lindexer= 0;
	char *stret2= handle_apply(fi, level + 1, 0, ineval, &lindexer, isamatrix, "dummy");
	dprintf(1, PPREFIX"ml:apply2(lev%d): ret %s\n", level, stret2);
	if (!first) first= stret2;
	else if (!second) second= stret2;
	else printf("NESTED APPLY ERROR1: 1=\"%s\" 2=\"%s\"\n", first, second);
      } else if (!strncmp(istr, "<ml:sequence", 12)) {
	char *stret2= handle_sequence(fi, level + 1, 0, ineval);
	dprintf(1, PPREFIX"ml:sequence(lev%d): ret %s\n", level, stret2);
	if (!first) {
	  printf("SEQUENCE WITH NO ID\n");
	  first= strdup("UNKNOWN_ID");
	}
	if (!second) second= stret2;
	else printf("NESTED APPLY ERROR2: 2=\"%s\"\n", second);
	sequence= 1;
      } else if (!strncmp(istr, "<ml:function", 12)) {
	char *stret2= handle_function(fi, level + 1);
	dprintf(1, PPREFIX"ml:function2(lev%d): ret %s\n", level, stret2);
	if (!first) first= stret2;
	else if (!second) second= stret2;
	else printf("NESTED FUNCTION ERROR\n");
      } else if (!strncmp(istr, "</ml:apply>", 11)) {
	if (first && (second || action == APPLY_NEG)) {
	  if (second) {
	    dprintf(1, PPREFIX"/apply 1=%s 2=%s\n", first, second);
	    stret= (char *) malloc(strlen(first) + strlen(second) + 512);
	  } else {
	    dprintf(1, PPREFIX"/apply 1=%s\n", first);
	    stret= (char *) malloc(strlen(first) + 512);
	  }
	  switch (action) {
	  case APPLY_EQUAL: {
	    if (!inif) {
	      if (!sequence)
		sprintf(stret, "%s= %s", first, second);
	      else
		sprintf(stret, "%s(%s)", first, second);
	    } else
	      sprintf(stret, "%s == %s", first, second);
	  } break;
	  case APPLY_GEQ: {
	    sprintf(stret, "%s >= %s", first, second);
	  } break;
	  case APPLY_LEQ: {
	    sprintf(stret, "%s <= %s", first, second);
	  } break;
	  case APPLY_LT: {
	    sprintf(stret, "%s < %s", first, second);
	  } break;
	  case APPLY_GT: {
	    sprintf(stret, "%s > %s", first, second);
	  } break;
	  case APPLY_MINUS: {
	    sprintf(stret, "(%s - %s)", first, second);
	  } break;
	  case APPLY_PLUS: {
	    sprintf(stret, "(%s + %s)", first, second);
	  } break;
	  case APPLY_MULT: {
	    sprintf(stret, "%s*%s", first, second);
	  } break;
	  case APPLY_AND: {
	    sprintf(stret, "%s&&%s", first, second);
	  } break;
	  case APPLY_DIV: {
	    sprintf(stret, "%s/(%s)", first, second);
	  } break;
	  case APPLY_POW: {
	    sprintf(stret, "pow(%s, %s)", first, second);
	  } break;
	  case APPLY_TANH: {
	    sprintf(stret, "tanh(%s, %s)", first, second);
	  } break;
	  case APPLY_INDEXER: {
	    sprintf(stret, "%s\[%s]", first, second);
	  } break;
	  case APPLY_NEG: {
	    sprintf(stret, "-(%s)", first);
	  } break;
	  case APPLY_EVAL: {
	    if (!sequence)
	      sprintf(stret, "%s=%s", first, second);
	    else
	      sprintf(stret, "%s(%s)", first, second);
	  } break;
	  default: /* this must be a function */
	    sprintf(stret, "%s(%s)", first, second);	    
	  }
	  if (!level) printf("%s\n", stret);
	} else {
	  dprintf(1, PPREFIX"FAULTY APPLY lev%d act %d\n", level, action);
	}
	free(buffer);
	free(first);
	free(second);
	return(stret);
      }
    }    
  }

  free(buffer);
  free(first);
  free(second);
  return(NULL);
}

#define FUNC_NONE   0
#define FUNC_FUNC   1
#define FUNC_PROG   2

char *handle_define_eval(FILE *fi, int level, int eval, int *isafunc)
{
  char *istr, *buffer= (char *) malloc(1024);
  char *first= NULL;
  char *second= NULL;
  char *stret= NULL;
  int   inif= 0;
  int   ifthen= 0;
  int   napply= 0;
  char *accum= NULL;
  int   nIf[128];
  int   proglevel= 0;
  int   indexer= 0;
  int   isamatrix= 0;

  nIf[0]= 0;

  dprintf(1, PPREFIX"ml:define(lev%d):\n", level);

  while (istr= fgets(buffer, 1024, fi)) {
    dprintf(2, "::%s", istr);
    if (istr= get1stchar(istr)) {
      if (!strncmp(istr, "<ml:id", 6)) {
	dprintf(1, PPREFIX"ml:id: %s\n", istr);
	if (ifthen) {
	  if (first && accum) {
	    second= NULL;
	    get_id2(istr + 6, &first, &second);
	    sprintf(accum + strlen(accum), "%s= %s;\n", first, second);
	  } else {
	    dprintf(1, PPREFIX"DEFINE FLAW1: accum %x first %s\n", accum, (first) ? first : "NULL");
	  }
	} else {
	  get_id2(istr + 6, &first, &second);
	}
      } else if (!strncmp(istr, "<ml:real", 8)) {
	dprintf(1, PPREFIX"ml:real: %s\n", istr);
	if (ifthen) {
	  if (first && accum) {
	    second= NULL;
	    get_id2(istr + 8, &first, &second);
	    sprintf(accum + strlen(accum), "%s= %s;\n", first, second);
	  } else {
	    dprintf(1, PPREFIX"DEFINE FLAW2: accum %x first %s\n", accum, (first) ? first : "NULL");
	  }
	} else {
	  get_id2(istr + 8, &first, &second);
	}
      } else if (!strncmp(istr, "<ml:result", 10)) {
	get_id2(istr + 10, &first, &second);
      } else if (!strncmp(istr, "<ml:matrix", 10)) {
	if (!first)
	  printf("DEFINE ERROR(%d): matrix: no name \n", level);
	else {
	  char *smatr= handle_matrix(fi, istr + 10, level + 1, first);
	  dprintf(1, PPREFIX"ml:matrix:\n");
	  second= smatr;
	  isamatrix= 1;
	}
      } else if (!strncmp(istr, "<ml:apply", 9)) {
	char *stret3;

	if (!napply)
	  stret3= handle_apply(fi, level + 1, inif, eval, &indexer, &isamatrix, first);
	else
	  stret3= handle_apply(fi, level + 1, 0, eval, &indexer, &isamatrix, first);

	dprintf(1, PPREFIX"ml:apply3(lev%d): ret %s inif %d nif %d progl %d napply %d\n", level, stret3, inif, nIf[proglevel], proglevel, napply);

	if (inif) {
	  if (!nIf[proglevel]) {
	    printf("+++2");
	    if (!accum) {
	      accum= (char *) malloc(1024);
	      *accum= 0;
	    }
	    strcat(accum, "if (");
	    strcat(accum, stret3);
	    strcpy(accum + strlen(accum), ") {\n");
	    ifthen= 1;
	  } else if (!napply) {
	    if (accum) {
	      second= stret3;
	      sprintf(accum + strlen(accum), "else if (%s) {\n", second);
	    } else {
	      dprintf(1, PPREFIX"DEFINE FLAW4: accum %x first %s\n", accum, (first) ? first : "NULL");
	    }
	  } else {
	    if (accum) {
	      second= stret3;
	      if (*isafunc == FUNC_FUNC) {
		sprintf(accum + strlen(accum), "return(%s);\n", second);
	      } else {
		sprintf(accum + strlen(accum), "%s= %s;\n", first, second);
	      }
	    } else {
	      dprintf(1, PPREFIX"DEFINE FLAW4: accum %x first %s\n", accum, (first) ? first : "NULL");
	    }
	  }
	  (nIf[proglevel])++;
	} else {
	  if (!first) first= stret3;
	  else {
	    if (accum) {
	      if (nIf[proglevel] && *isafunc == FUNC_PROG)
		sprintf(accum + strlen(accum), " else {\n%s= %s;\n}\n", first, stret3);
	      else
		sprintf(accum + strlen(accum), "%s= %s", first, stret3);
	    } else
	      second= stret3;
	  }
	}
	napply++;
      } else if (!strncmp(istr, "<ml:function", 12)) {
	char *stret3= handle_function(fi, 1);
	dprintf(1, PPREFIX"ml:function3(lev%d): ret %s\n", level, stret3);
	if (!first) first= stret3;
	else if (!second) second= stret3;
	*isafunc= FUNC_FUNC;
      } else if (!strncmp(istr, "<ml:program", 11)) {
	if (*isafunc != FUNC_FUNC) *isafunc= FUNC_PROG;
	nIf[++proglevel]= 0;
	dprintf(1, PPREFIX"ml:program(lev%d): proglev\n", level, proglevel);
	/*	if (!accum)
	  printf("{\n");
	  else
	  strcat (accum, "{\n");*/
      } else if (!strncmp(istr, "</ml:program", 12)) {
	nIf[proglevel-1]+= nIf[proglevel];
	proglevel--;
	dprintf(1, PPREFIX"/ml:program(lev%d): proglev %d\n", level, proglevel);
	/*	if (!accum)
	  printf("}\n");
	else
	strcat (accum, "}\n");*/
      } else if (!strncmp(istr, "<ml:ifThen", 10)) {
	inif= 1;
	dprintf(1, PPREFIX"ml:if:\n");
      } else if (!strncmp(istr, "</ml:ifThen", 11)) {
	dprintf(1, PPREFIX"ml:if/:\n");
	if (!accum) {
	  accum= (char *) malloc(1024);
	  *accum= 0;
	}
	strcat(accum, "}\n");
	inif= 0;
	napply= 0;
      } else if (!strncmp(istr, "<ml:return", 9)) {
	char *stret5= handle_return(fi, level + 1, *isafunc == FUNC_FUNC);
	dprintf(1, PPREFIX"ml:return: \"%s\"\n", stret5);
	if (ifthen) {
	  if (first && accum) {
	    second= stret5;
	    if (*isafunc != FUNC_FUNC) {
	      sprintf(accum + strlen(accum), "%s= %s;\n", first, second);
	    } else {
	      sprintf(accum + strlen(accum), "%s;\n", second);
	    }
	  } else {
	    dprintf(1, PPREFIX"DEFINE FLAW1: accum %x first %s\n", accum, (first) ? first : "NULL");
	  }
	} else {
	  if (!first) first= stret5;
	  else if (!second) second= stret5;
	  else printf("RETURN ERROR\n");
	}
      } else if (!strncmp(istr, "<ml:localDefine", 15)) {
	char *stret3= handle_define_eval(fi, level + 1, 0, isafunc);
	dprintf(1, PPREFIX"ml:localDefine(lev%d): ret \"%s\"\n", level, stret3);
	if (ifthen) {
	  if (accum) {
	    sprintf(accum + strlen(accum), "%s;\n", stret3);
	  } else {
	    dprintf(1, PPREFIX"DEFINE FLAW2: accum %x first %s\n", accum, (first) ? first : "NULL");
	  }
	} else {
	  second= stret3;
	}
      } else if (
		 (!eval && (!strncmp(istr, "</ml:define>", 11) || !strncmp(istr, "</ml:localDefine", 16)))
		 ||
		 (eval && !strncmp(istr, "</ml:eval>", 9))
		 ) {
	if (!eval) {
	  static char *strdouble= "double";
	  static char *strequal= "=";
	  static char *strmatrix= "matrix<double, int>";
	  static char *strnewline= ";\n";
	  char *strtype= (isamatrix) ? (char *) strmatrix : (char *) strdouble;
	  char *sigeq= (isamatrix) ? (char *) strnewline : (char *) strequal;
	  int locd= (strncmp(istr, "</ml:localDefine", 16)) ? 0 : 1;
	  dprintf(1, PPREFIX"</ml:define>: accum \"%s\" first %s\n", accum, (first) ? first : "NULL");
	  if (accum) {
	    stret= accum;
	    if (!*isafunc)
	      sprintf(stret + strlen(stret), "%s\n", stret);
	    else {
	      char *ptr= strdup(stret);
	      if (*isafunc == FUNC_FUNC) {
		if (!nIf[proglevel]) {
		  sprintf(stret, "%s %s\n\{\nreturn(%s);\n}\n", strtype, first, ptr);
		} else {
		  sprintf(stret, "%s %s\n\{\n%s\n}\n", strtype, first, ptr);
		}
	      } else if (*isafunc == FUNC_PROG) {
		fprintf(fohead, "%s %s;\n", strtype, first);
		sprintf(stret, "%s", ptr);
	      } else {
		sprintf(stret + strlen(stret), "%s %s\n\{\n%s\n}\n", strtype, first, stret);
	      }
	      free(ptr);
	    }
	  } else {
	    if (first && second) {
	      dprintf(1, PPREFIX"/define 1=\"%s\" 2=\"%s\" type=%s sigeq=%s\n", first, second, strtype, sigeq);
	      stret= (char *) malloc(strlen(first) + strlen(second) + 32);
	      if (!*isafunc) {
		if (!indexer) {
		  fprintf(fohead, "%s %s;\n", strtype, first);
		  sprintf(stret, "%s%s %s", first, sigeq, second);
		} else {
		  sprintf(stret, "%s%s %s", first, sigeq, second);
		}
	      } else {
		if (*isafunc == FUNC_FUNC)
		  sprintf(stret, "%s %s\n{\nreturn(%s);\n}\n", strtype, first, second);
		else if (*isafunc == FUNC_PROG)
		  sprintf(stret, "%s%s %s", first, sigeq, second);
		else
		  sprintf(stret, "%s %s\n{\n%s\n}\n", strtype, first, second);
	      }
	      if (!level)
/*		strcat(stret, "\n");
	      else*/
		strcat(stret,";\n");
	    } else {
	      printf("FAULTY DEFINE: 1=\"%s\" 2=\"%s\"\n", first, second);
	    }
	  }
	} else {
	  if (first && second) {
	    dprintf(1, PPREFIX"/eval 1=%s 2=%s\n", first, second);
	    stret= (char *) malloc(strlen(first) + strlen(second) + 64);
	    sprintf(stret, "/*eval %d*/ // %s= %s\n", level, first, second);
	    /*	    if (level)
	      strcat(stret, "\n");
	      else
	      strcat(stret,";\n");*/
	  } else {
	    dprintf(1, PPREFIX"FAULTY EVAL: 1=\"%s\" 2=\"%s\"\n", first, second);
	  }
	}
	free(buffer);
	free(first);
	free(second);
	printf("++++++3: %s\n", stret);
	return(stret);
      }
    }    
  }

  free(buffer);
  free(first);
  free(second);
  return(NULL);
}

int main(int argc, char **argv)
{
  FILE *fi;
  char *buffer= (char *) malloc(1024);
  char *istr;
  int   indexer= 0;
  int   isamatrix= 0;
  int   isafunc;

  if (argc < 4) return(-1);

  fi= fopen(argv[1], "r");
  if (!fi) return(-1);
  fohead= fopen(argv[2], "w");
  if (!fohead) return(-2);
  fofunc= fopen(argv[3], "w");
  if (!fofunc) return(-3);

  if (!buffer) return(-4);

  printf("int main(int argc, char **argv) {\n");

  while (istr= fgets(buffer, 1024, fi)) {
    dprintf(2, "::%s", istr);
    if (istr= get1stchar(istr)) {
      if (!strncmp(istr, "<ml:define", 10)) {
	char *stret0;
	isafunc= FUNC_NONE;
	stret0= handle_define_eval(fi, 0, 0, &isafunc);
	if (stret0) {
	  if (isafunc == FUNC_FUNC)
	    fprintf(fofunc, "%s\n", stret0);
	  else
	    printf("%s\n", stret0);
	}
      } else if (!strncmp(istr, "<ml:eval", 8)) {
	char *stret0;
	stret0= handle_define_eval(fi, 0, 1, &isafunc);
	if (stret0) {
	  printf("%s", stret0);
	}
      } else if (!strncmp(istr, "<ml:apply", 9)) {
	char *stret0;
	printf("ml:apply0\n");
	stret0= handle_apply(fi, 0, 0, 0, &indexer, &isamatrix, "dummy");
	if (stret0)
	  printf("%s;\n", stret0);
      } else if (!strncmp(istr, "<ml:function", 12)) {
	char *stret0;
	printf("ml:function0\n");
	stret0= handle_function(fi, 0);
	if (stret0)
	  printf("%s\n", stret0);
      }
    }
  }
  printf("return(0);\n}");
}
