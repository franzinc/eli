/* Copyright (C) 1993, Franz Inc., Berkeley, CA.  All rights reserved. */

/* $Header: /repo/cvs.copy/eli/Attic/clmanaux.c,v 2.2 1993/07/23 07:28:04 layer Exp $ */

#include "clman.h"

struct Entry *table;
int table_byte_size;
int table_max_entries;

char *string_table;
int string_table_size;
int data_size;

hash(name, size)
    unsigned char *name;
{
    int counter;
    int i;
    unsigned char *p;

    if (*name != ':' &&
	(p = (unsigned char *)strchr(name, ':')) &&
	*(p++) != '\0')
    {
	size -= p - name;
	name = p;
    }
    counter = size;

    for (i = 0; i <= size; i++) {
	counter = ((counter << 3) ^ (counter >> 16)) + (040 | name[i]);
    }
    return (0xffff & ((counter >> 16) ^ counter));
}

puthashi(i)
{
    int max = table_max_entries - 1;
    int oldi = i, seen = 0;

    for (; table[i].name_index; i == max ? i = 0 : i++) {
	if (i == oldi) {
	    if (seen) {
		fprintf(stderr, "internal error: table full\n");
		exit(-1);
	    } else {
		seen++;
	    }
	}
    }
    return(i);
}

gethashi(name)
    char *name;
{
    int namesize = strlen(name);
    int i = hash(name, namesize) % table_max_entries;
    int max = table_max_entries - 1;
    int oldi = i, seen = 0;

    for (; table[i].name_index; i == max ? i = 0 : i++) {
	if (i == oldi) {
	    if (seen) {
		fprintf(stderr, "internal error: table full\n");
		exit(-1);
	    } else {
		seen++;
	    }
	}
	if (!strncmp(name, string_table + table[i].name_index, namesize))
	    break;
    }
    if (table[i].name_index) {
	return(i);
    } else {
	fprintf(stderr, "No CLMan entry for %s.\n", name);
	exit(-1);
    }
}
