/* Copyright (C) 1993, Franz Inc., Berkeley, CA.  All rights reserved. */

/* $Header: /repo/cvs.copy/eli/Attic/clman.c,v 2.5 1995/04/29 18:34:35 smh Exp $ */

#include "clman.h"
#include <netinet/in.h>

/*
 * Changes 11jul94 to hack binary data on different byte-order machines. -smh
 */

main(argc, argv)
    char **argv;
{
    /* usage: clman dbfile name
     *   with stdin being pairs of lines, the first being the key the
     *   second being the file containing the data.
     */
    struct Header h;
    int symindex;
    int fd, n;
    char *buf, *name;

    failed = 0;		/* the exit status is the number of entries found */

    if (argc != 3) {
	fprintf(stderr, "usage: %s dbfile name\n", argv[0]);
	exit(failed);
    }
    name = argv[2];

    if ((fd = open(argv[1], O_RDONLY)) < 0) {
	perror(argv[1]);
	fprintf(stderr, "couldn't open %s\n", argv[1]);
	exit(failed);
    }

    if (read(fd, &h, sizeof(h)) != sizeof(h)) {
	fprintf(stderr, "couldn't read header\n");
	exit(failed);
    }

    h.entry_table_size = ntohl(h.entry_table_size);
    h.string_table_size = ntohl(h.string_table_size);
    h.data_size = ntohl(h.data_size);

    table = (struct Entry *)malloc(h.entry_table_size);
    if (read(fd, table, h.entry_table_size) != h.entry_table_size) {
	fprintf(stderr, "couldn't read entry table\n");
	exit(failed);
    }
    table_max_entries = h.entry_table_size / sizeof(struct Entry);
    {
      int i;
      struct Entry *p;

      for (i=0, p=table; i<table_max_entries; i++, p++) {
	p->name_index = htonl(p->name_index);
	p->data_index = htonl(p->data_index);
	p->data_size  = htons(p->data_size);
	p->ndefs      = htons(p->ndefs);
      }
    }

    string_table = (char *)malloc(h.string_table_size);
    if (read(fd, string_table, h.string_table_size) != h.string_table_size) {
	fprintf(stderr, "couldn't read string table\n");
	exit(failed);
    }

    symindex = gethashi(name);

    buf = (char *)malloc(table[symindex].data_size);
    lseek(fd, table[symindex].data_index, SEEK_CUR);

    if ((n = read(fd, buf, table[symindex].data_size)) !=
	table[symindex].data_size) {
	fprintf(stderr, "couldn't read data\n");
	exit(failed);
    }
    write(1, buf, n);
    exit(table[symindex].ndefs);
}
