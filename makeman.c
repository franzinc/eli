/* Copyright (C) 1993, Franz Inc., Berkeley, CA.  All rights reserved. */

/* $Header: /repo/cvs.copy/eli/Attic/makeman.c,v 2.2 1993/07/27 20:12:30 layer Exp $ */

#include "clman.h"

char *symbols[SYMBOLS];
struct File *files[SYMBOLS];
int nsymbols = 0;

int file_max_size = 0;

char *sep = "\n===============================================================================\n\n";
int seplen;

main(argc, argv)
    char **argv;
{
    /* usage: makeman dbfile
     *   with stdin being pairs of lines, the first being the key the
     *   second being the file containing the data.
     */
    char buf[1024], *name;
    int buflen;
    struct File *file, *file_head;
    struct stat sbuf;
#define STATE_SYMBOL 1
#define STATE_FILE 2
    int state;
    int outfd;
    int c;
    extern char *optarg;
    extern int optind;
    char *dbfile;
    int errflg = 0;
    char *usage = "usage: makeman [-d <dir>] dbfile\n";
    char *mandir = "manual";

    while ((c = getopt(argc, argv, "d:")) != EOF) {
	switch (c) {
	case 'd':
	    mandir = optarg;
	    continue;
	case '?':
	    errflg++;
	    break;
	}
    }
    if (errflg || (optind != argc - 1)) {
	fprintf(stderr, usage);
	exit(1);
    } else {
	dbfile = argv[optind];
    }

    if ((outfd = open(dbfile, O_WRONLY|O_CREAT|O_TRUNC, 0666)) < 0) {
	perror(dbfile);
	fprintf(stderr, "couldn't open %s\n", dbfile);
	exit(1);
    }

    failed = -1;
    nsymbols = 0;
    seplen = strlen(sep);
    chdir(mandir);
    
    for (file_head = NULL, state = STATE_SYMBOL;;) {
	if (fgets(buf, sizeof (buf), stdin) == NULL) break;
	buflen = strlen(buf) - 1;
	if (buflen == 0) {
	    state = STATE_SYMBOL;
	    files[nsymbols] = file_head;
	    file_head = NULL;
	    nsymbols++;
	    if (nsymbols == SYMBOLS) {
		fprintf(stderr, "increase SYMBOLS (currently %d)\n",
			SYMBOLS);
		exit(1);
	    }
	    continue;
	}
	buf[buflen++] = '\0';

	switch (state) {
	case STATE_SYMBOL:
	    symbols[nsymbols] = (char *)malloc(buflen);
	    strncpy(symbols[nsymbols], buf, buflen);
	    state = STATE_FILE;
	    break;
	case STATE_FILE:
	    if (stat(buf, &sbuf) < 0) {
		perror(argv[0]);
		fprintf(stderr, "can't access file %s (%d)\n",
			buf, nsymbols);
		exit(1);
	    }
	    if (file_head == NULL) {
		file_head = file =
		    (struct File *)malloc(sizeof (struct File));
	    } else {
		file->next = (struct File *)malloc(sizeof (struct File));
		file = file->next;
	    }
	    file->name = (char *)malloc(buflen);
	    file->size = sbuf.st_size;
	    if (file->size > file_max_size)
		file_max_size = file->size;
	    strncpy(file->name, buf, buflen);
	    file->next = NULL;
	    break;
	}

    }
    if (state != STATE_SYMBOL) {
	fprintf(stderr, "Missing file after symbol.\n");
	exit(1);
    }

    make_data();
    write_file(dbfile, outfd);
    close(outfd);

    exit(0);
}

make_data()
{
    int i, ei;
    struct File *f, *fhead;
    int hashval;
    int symlen;
    int dsize;

    table_max_entries = (int)(1.3 * nsymbols);
    table_byte_size = table_max_entries * sizeof(struct Entry);
    table = (struct Entry *)malloc(table_byte_size);
    memset(table, 0, table_byte_size);

    /* nothing at index 0, so "struct Entry.name_index" of 0 means
     * "no entry in table"
     */
    string_table_size = 1;
    data_size = 1;

    for (i = 0; i < nsymbols; i++) {
	symlen = strlen(symbols[i]);
	hashval = hash(symbols[i], symlen) % table_max_entries;
	ei = puthashi(hashval);
	table[ei].name_index = string_table_size;
	table[ei].data_index = data_size;
	dsize = 0;
	for (fhead = f = files[i]; f != NULL; f = f->next) {
	    dsize += f->size;
	    table[ei].ndefs++;
	    if (f->next != NULL) dsize += seplen;
	}
	table[ei].data_size = dsize;
	data_size += dsize;
	string_table_size += symlen;
    }
}

write_file(name, fd)
    char *name;
{
    /*
     * file contains: header, entry table, string table, data.
     */
    struct Header h;
    int i, symlen, n, in;
    struct File *f, *fhead;
    char *file_buf;
    int zero = 0;
    int written_string_size = 1;
    int written_data_size = 1;

#if 0
    printf("nsymbols = %d, data_size = %d, table_byte_size = %d\n",
	   nsymbols, data_size, table_byte_size);
#endif

    h.entry_table_size = table_byte_size;
    h.string_table_size = string_table_size;
    h.data_size = data_size;

    if (write(fd, &h, sizeof(h)) != sizeof(h)) {
	perror(name);
	fprintf(stderr, "couldn't write header to %s\n", name);
	exit(1);
    }

    if (write(fd, table, table_byte_size) != table_byte_size) {
	perror(name);
	fprintf(stderr, "couldn't write table to %s\n", name);
	exit(1);
    }

    /* string table */
    if (write(fd, &zero, 1) != 1) {
	perror(name);
	fprintf(stderr, "couldn't write first byte of string table to %s\n",
		name);
	exit(1);
    }
    for (i = 0; i < nsymbols; i++) {
	symlen = strlen(symbols[i]);
	if (write(fd, symbols[i], symlen) != symlen) {
	    perror(name);
	    fprintf(stderr, "couldn't write string to %s\n", name);
	    exit(1);
	}
	written_string_size += symlen;
    }
    if (written_string_size != string_table_size) {
	fprintf(stderr, "written_string_size (%d) != string_table_size (%d)\n",
		written_string_size, string_table_size);
	exit(1);
    }

    /* data */
    if (write(fd, &zero, 1) != 1) {
	perror(name);
	fprintf(stderr, "couldn't write first byte of data to %s\n",
		name);
	exit(1);
    }
    file_buf = (char *)malloc(file_max_size);
    for (i = 0; i < nsymbols; i++) {
	for (fhead = f = files[i]; f != NULL; f = f->next) {
	    if ((in = open(f->name, O_RDONLY)) < 0) {
		perror(f->name);
		fprintf(stderr, "couldn't open %s\n", f->name);
		exit(1);
	    }
	    if ((n = read(in, file_buf, f->size)) != f->size) {
		perror(f->name);
		fprintf(stderr, "couldn't read %s\n", f->name);
		exit(1);
	    }
	    if (write(fd, file_buf, n) != n) {
		perror(name);
		fprintf(stderr, "couldn't write data to %s\n", name);
		exit(1);
	    }
	    written_data_size += n;
	    if (f->next != NULL) {
		if (write(fd, sep, seplen) != seplen) {
		    perror(name);
		    fprintf(stderr, "couldn't write sep to %s\n", name);
		    exit(1);
		}
		written_data_size += seplen;
	    }
	    close(in);
	}
    }
    if (written_data_size != data_size) {
	fprintf(stderr, "written_data_size (%d) != data_size (%d)\n",
		written_data_size, data_size);
	exit(1);
    }
}
