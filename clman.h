/* Copyright (C) 1993, Franz Inc., Berkeley, CA.  All rights reserved. */

/* $Header: /repo/cvs.copy/eli/Attic/clman.h,v 2.1 1993/07/22 23:04:45 layer Exp $ */

#include <unistd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

struct Header {
    int entry_table_size;
    int string_table_size;
    int data_size;
};

struct File {
    char *name;
    long size;
    struct File *next;
};

struct Entry {
    int name_index;
    int data_index;
    short data_size;
    short ndefs;
};

#define SYMBOLS 3500

extern struct Entry *table;
extern int table_byte_size;
extern int table_max_entries;

extern char *string_table;
extern int string_table_size;
extern int data_size;
