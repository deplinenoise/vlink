/* $VER: vlink t_rawbin.c V0.13 (02.11.10)
 *
 * This file is part of vlink, a portable linker for multiple
 * object formats.
 * Copyright (c) 1997-2010  Frank Wille
 *
 * vlink is freeware and part of the portable and retargetable ANSI C
 * compiler vbcc, copyright (c) 1995-2010 by Volker Barthelmann.
 * vlink may be freely redistributed as long as no modifications are
 * made and nothing is charged for it. Non-commercial usage is allowed
 * without any restrictions.
 * EVERY PRODUCT OR PROGRAM DERIVED DIRECTLY FROM MY SOURCE MAY NOT BE
 * SOLD COMMERCIALLY WITHOUT PERMISSION FROM THE AUTHOR.
 */


#include "config.h"
#if defined(RAWBIN1) || defined(RAWBIN2) || \
    defined(SREC19) || defined(SREC28) || defined(SREC37) || \
    defined(IHEX)
#define T_RAWBIN_C
#include "vlink.h"

#define MAXGAP 16  /* from MAXGAP bytes on, a new file will be created */
#define MAXSREC 32 /* max. number of bytes in an S1/S2/S3 record */
#define MAXIREC 32 /* max. number of bytes in an ihex record (actually 255) */


static unsigned long rawbin_headersize(struct GlobalVars *);
static int rawbin_identify(char *,uint8_t *,unsigned long,bool);
static void rawbin_readconv(struct GlobalVars *,struct LinkFile *);
static int rawbin_targetlink(struct GlobalVars *,struct LinkedSection *,
                              struct Section *);
static void rawbin_writeobject(struct GlobalVars *,FILE *);
static void rawbin_writeshared(struct GlobalVars *,FILE *);
static void rawbin_writeexec(struct GlobalVars *,FILE *,bool);
#ifdef RAWBIN1
static void rawbin_writesingle(struct GlobalVars *,FILE *);
#endif
#ifdef RAWBIN2
static void rawbin_writemultiple(struct GlobalVars *,FILE *);
#endif
#ifdef SREC19
static void srec19_write(struct GlobalVars *,FILE *);
#endif
#ifdef SREC28
static void srec28_write(struct GlobalVars *,FILE *);
#endif
#ifdef SREC37
static void srec37_write(struct GlobalVars *,FILE *);
#endif
#ifdef IHEX
static void ihex_write(struct GlobalVars *,FILE *);
#endif
#ifdef SHEX1
static void shex1_write(struct GlobalVars *,FILE *);
#endif

static const char defaultscript[] =
  "SECTIONS {\n"
  "  .text: { *(.text) *(CODE) *(text) }\n"
  "  .data: { *(.data) *(DATA) *(data) }\n"
  "  .bss: { *(.bss) *(BSS) *(bss) }\n"
  "}\n";


#ifdef RAWBIN1
struct FFFuncs fff_rawbin1 = {
  "rawbin1",
  defaultscript,
  NULL,
  rawbin_headersize,
  rawbin_identify,
  rawbin_readconv,
  NULL,
  rawbin_targetlink,
  NULL,
  NULL,
  NULL,
  NULL,NULL,NULL,
  rawbin_writeobject,
  rawbin_writeshared,
  rawbin_writesingle,
  NULL,NULL,
  0,
  0x8000,
  0,
  0,
  RTAB_UNDEF,0,
  -1, /* endianess undefined, only write */
  32
};
#endif

#ifdef RAWBIN2
struct FFFuncs fff_rawbin2 = {
  "rawbin2",
  defaultscript,
  NULL,
  rawbin_headersize,
  rawbin_identify,
  rawbin_readconv,
  NULL,
  rawbin_targetlink,
  NULL,
  NULL,
  NULL,
  NULL,NULL,NULL,
  rawbin_writeobject,
  rawbin_writeshared,
  rawbin_writemultiple,
  NULL,NULL,
  0,
  0x8000,
  0,
  0,
  RTAB_UNDEF,0,
  -1, /* endianess undefined, only write */
  32
};
#endif

#ifdef SREC19
struct FFFuncs fff_srec19 = {
  "srec19",
  defaultscript,
  NULL,
  rawbin_headersize,
  rawbin_identify,
  rawbin_readconv,
  NULL,
  rawbin_targetlink,
  NULL,
  NULL,
  NULL,
  NULL,NULL,NULL,
  rawbin_writeobject,
  rawbin_writeshared,
  srec19_write,
  NULL,NULL,
  0,
  0x8000,
  0,
  0,
  RTAB_UNDEF,0,
  -1, /* endianess undefined, only write */
  32
};
#endif

#ifdef SREC28
struct FFFuncs fff_srec28 = {
  "srec28",
  defaultscript,
  NULL,
  rawbin_headersize,
  rawbin_identify,
  rawbin_readconv,
  NULL,
  rawbin_targetlink,
  NULL,
  NULL,
  NULL,
  NULL,NULL,NULL,
  rawbin_writeobject,
  rawbin_writeshared,
  srec28_write,
  NULL,NULL,
  0,
  0x8000,
  0,
  0,
  RTAB_UNDEF,0,
  -1, /* endianess undefined, only write */
  32
};
#endif

#ifdef SREC37
struct FFFuncs fff_srec37 = {
  "srec37",
  defaultscript,
  NULL,
  rawbin_headersize,
  rawbin_identify,
  rawbin_readconv,
  NULL,
  rawbin_targetlink,
  NULL,
  NULL,
  NULL,
  NULL,NULL,NULL,
  rawbin_writeobject,
  rawbin_writeshared,
  srec37_write,
  NULL,NULL,
  0,
  0x8000,
  0,
  0,
  RTAB_UNDEF,0,
  -1, /* endianess undefined, only write */
  32
};
#endif

#ifdef IHEX
struct FFFuncs fff_ihex = {
  "ihex",
  defaultscript,
  NULL,
  rawbin_headersize,
  rawbin_identify,
  rawbin_readconv,
  NULL,
  rawbin_targetlink,
  NULL,
  NULL,
  NULL,
  NULL,NULL,NULL,
  rawbin_writeobject,
  rawbin_writeshared,
  ihex_write,
  NULL,NULL,
  0,
  0x8000,
  0,
  0,
  RTAB_UNDEF,0,
  -1, /* endianess undefined, only write */
  32
};
#endif

#ifdef SHEX1
struct FFFuncs fff_shex1 = {
  "oilhex",
  defaultscript,
  NULL,
  rawbin_headersize,
  rawbin_identify,
  rawbin_readconv,
  NULL,
  rawbin_targetlink,
  NULL,
  NULL,
  NULL,
  NULL,NULL,NULL,
  rawbin_writeobject,
  rawbin_writeshared,
  shex1_write,
  NULL,NULL,
  0,
  0x8000,
  0,
  0,
  RTAB_UNDEF,0,
  -1, /* endianess undefined, only write */
  32
};
#endif


/*****************************************************************/
/*                        Read Binary                            */
/*****************************************************************/


static unsigned long rawbin_headersize(struct GlobalVars *gv)
{
  return 0;  /* no header - it's pure binary! */
}


static int rawbin_identify(char *name,uint8_t *p,unsigned long plen,bool lib)
/* identify a binary file */
{
  return (ID_UNKNOWN);  /* binaries are only allowed to be written! */
}


static void rawbin_readconv(struct GlobalVars *gv,struct LinkFile *lf)
{
  ierror("rawbin_readconv(): Can't read raw-binaries");
}


static int rawbin_targetlink(struct GlobalVars *gv,struct LinkedSection *ls,
                             struct Section *s)
/* returns 1, if target requires the combination of the two sections, */
/* returns -1, if target doesn't want to combine them, */
/* returns 0, if target doesn't care - standard linking rules are used. */
{
  ierror("rawbin_targetlink(): Impossible to link raw-binaries");
  return (0);
}



/*****************************************************************/
/*                        Write Binary                           */
/*****************************************************************/

static struct LinkedSection *get_next_section(struct GlobalVars *gv)
/* returns pointer to next section with lowest base address */
{
  struct LinkedSection *ls = (struct LinkedSection *)gv->lnksec.first;
  struct LinkedSection *nextls,*minls = NULL;

  while (nextls = (struct LinkedSection *)ls->n.next) {
    if (minls) {
      if (ls->copybase < minls->copybase)
        minls = ls;
    }
    else
      minls = ls;
    ls = nextls;
  }
  if (minls) {
    remnode(&minls->n);
  }
  return (minls);
}


static void rawbin_writeexec(struct GlobalVars *gv,FILE *f,bool singlefile)
/* creates executable raw-binary files (with absolute addresses) */
{
  FILE *firstfile = f;
  bool firstsec = TRUE;
  unsigned long addr = ~0;
  struct LinkedSection *ls;

  while (ls = get_next_section(gv)) {
    if (ls->size == 0 || !(ls->flags & SF_ALLOC) || (ls->ld_flags & LSF_NOLOAD))
      continue;  /* ignore empty sections */

    if (firstsec) {
      if (gv->trace_file)
        fprintf(gv->trace_file,"Base address = 0x%08lx.\n",ls->copybase);
      firstsec = FALSE;
    }
    else {
      if (ls->copybase != addr) {
        if (ls->copybase-addr < MAXGAP || singlefile) {
          fwritegap(f,ls->copybase-addr);
        }
        else {  /* open a new file for this section */
          char *name;

          if (f != firstfile)
            fclose(f);
          name = alloc(strlen(gv->dest_name)+32);
          sprintf(name,"%s.%lx",gv->dest_name,ls->copybase);
          if (!(f = fopen(name,"wb"))) {
            error(29,name);
            break;
          }
          free(name);
        }
      }
    }

    /* resolve all relocations and write section contents */
    calc_relocs(gv,ls);
    fwritex(f,ls->data,ls->filesize);
    if (ls->filesize < ls->size)
      fwritegap(f,ls->size - ls->filesize);

    addr = ls->copybase + ls->size;
  }

  if (f && f!=firstfile)
    fclose(f);
}


static void rawbin_writeshared(struct GlobalVars *gv,FILE *f)
{
  error(30);  /* Target file format doesn't support shared objects */
}


static void rawbin_writeobject(struct GlobalVars *gv,FILE *f)
{
  error(62);  /* Target file format doesn't support relocatable objects */
}


#ifdef RAWBIN1
static void rawbin_writesingle(struct GlobalVars *gv,FILE *f)
/* creates a single raw-binary file, fill gaps between */
/* sections with zero */
{
  rawbin_writeexec(gv,f,TRUE);
}
#endif


#ifdef RAWBIN2
static void rawbin_writemultiple(struct GlobalVars *gv,FILE *f)
/* creates raw-binary which might get splitted over several */
/* files, because of different section base addresses */
{
  rawbin_writeexec(gv,f,FALSE);
}
#endif


#if defined(SREC19) || defined(SREC28) || defined(SREC37)
static void SRecOut(FILE *f,int stype,uint8_t *buf,int len)
{
  uint8_t chksum = 0xff-len-1;

  fprintf(f,"S%1d%02X",stype,(unsigned)(len+1));
  for (; len>0; len--) {
    fprintf(f,"%02X",(unsigned)*buf);
    chksum -= *buf++;
  }
  fprintf(f,"%02X\n",(unsigned)chksum);
}


static void srec_write(struct GlobalVars *gv,FILE *f,int addrsize)
{
  bool firstsec = TRUE;
  struct LinkedSection *ls;
  unsigned long len,addr;
  uint8_t *p,buf[MAXSREC+8];

  /* write header */
  buf[0] = buf[1] = 0;
  strncpy(&buf[2],gv->dest_name,MAXSREC+6);
  SRecOut(f,0,buf,(strlen(gv->dest_name)<(MAXSREC+6)) ?
          strlen(gv->dest_name)+2 : MAXSREC+8);

  while (ls = get_next_section(gv)) {
    if (ls->size == 0 || !(ls->flags & SF_ALLOC) || (ls->ld_flags & LSF_NOLOAD))
      continue;  /* ignore empty sections */

    if (firstsec) {
      if (gv->trace_file)
        fprintf(gv->trace_file,"Base address = 0x%08lx.\n",ls->copybase);
      firstsec = FALSE;
    }

    /* resolve all relocations and write section contents */
    calc_relocs(gv,ls);

    for (p=ls->data,addr=ls->copybase,len=ls->filesize; len>0; ) {
      int nbytes = (len>MAXSREC) ? MAXSREC : len;

      switch (addrsize) {
        case 2:
          write16be(buf,(uint16_t)addr);
          memcpy(buf+2,p,nbytes);
          SRecOut(f,1,buf,2+nbytes);
          break;
        case 3:
          buf[0] = (uint8_t)((addr>>16)&0xff);
          buf[1] = (uint8_t)((addr>>8)&0xff);
          buf[2] = (uint8_t)(addr&0xff);
          memcpy(buf+3,p,nbytes);
          SRecOut(f,2,buf,3+nbytes);
          break;
        case 4:
          write32be(buf,(uint32_t)addr);
          memcpy(buf+4,p,nbytes);
          SRecOut(f,3,buf,4+nbytes);
          break;
        default:
          ierror("srec_write(): Illegal SRec-type: %d",addrsize);
          nbytes = len;
          break;
      }
      p += nbytes;
      addr += nbytes;
      len -= nbytes;
    }
  }

  /* write trailer */
  memset(buf,0,4);
  SRecOut(f,11-addrsize,buf,addrsize);
}

#endif


#ifdef SREC19
static void srec19_write(struct GlobalVars *gv,FILE *f)
/* creates a Motorola S-Record file (S0,S1,S9), using 16-bit addresses */
{
  srec_write(gv,f,2);
}
#endif


#ifdef SREC28
static void srec28_write(struct GlobalVars *gv,FILE *f)
/* creates a Motorola S-Record file (S0,S2,S8), using 24-bit addresses */
{
  srec_write(gv,f,3);
}
#endif


#ifdef SREC37
static void srec37_write(struct GlobalVars *gv,FILE *f)
/* creates a Motorola S-Record file (S0,S1,S9), using 32-bit addresses */
{
  srec_write(gv,f,4);
}
#endif


#ifdef IHEX
static void IHexOut(FILE *f,unsigned long addr,uint8_t *buf,int len)
{
  static unsigned long hiaddr;
  uint8_t chksum;

  if (((addr&0xffff0000)>>16)!=hiaddr) {
    hiaddr = (addr&0xffff0000) >> 16;
    fprintf(f,":02000004%02X%02X%02X\n",
            (unsigned)(hiaddr>>8)&0xff,
            (unsigned)hiaddr&0xff,
            (unsigned)(-(2+4+(hiaddr>>8)+hiaddr))&0xff);
  }
  fprintf(f,":%02X%02X%02X00",
          (unsigned)len&0xff,(unsigned)(addr>>8)&0xff,(unsigned)addr&0xff);
  chksum = len + (addr>>8) + addr;
  for (; len>0; len--) {
    fprintf(f,"%02X",((unsigned)*buf)&0xff);
    chksum += *buf++;
  }
  fprintf(f,"%02X\n",(-chksum)&0xff);
}


static void ihex_write(struct GlobalVars *gv,FILE *f)
{
  bool firstsec = TRUE;
  struct LinkedSection *ls;
  unsigned long len,addr;
  uint8_t *p;

  while (ls = get_next_section(gv)) {
    if (ls->size == 0 || !(ls->flags & SF_ALLOC) || (ls->ld_flags & LSF_NOLOAD))
      continue;  /* ignore empty sections */

    if (firstsec) {
      if (gv->trace_file)
        fprintf(gv->trace_file,"Base address = 0x%08lx.\n",ls->copybase);
      firstsec = FALSE;
    }

    /* resolve all relocations and write section contents */
    calc_relocs(gv,ls);

    for (p=ls->data,addr=ls->copybase,len=ls->filesize; len>0; ) {
      int nbytes = (len>MAXIREC) ? MAXIREC : len;

      if ((((unsigned long)addr)&0xffff)+nbytes > 0xffff)
        nbytes = 0x10000 - (((unsigned long)addr) & 0xffff);
      IHexOut(f,addr,p,nbytes);
      p += nbytes;
      addr += nbytes;
      len -= nbytes;
    }
  }

  fprintf(f,":00000001FF\n");
}
#endif

#ifdef SHEX1
static void SHex1Out(FILE *f,unsigned long addr,uint8_t *buf,int len)
{
  int wordcnt=(len+3)/4;
  int i;

  fprintf(f,"%06lX %d",addr,wordcnt);

  for(i=0;i<len;i++){
    if((i&3)==0)
      fprintf(f," ");
    fprintf(f,"%02X",buf[i]);
  }
  for(i=0;i<wordcnt*4-len;i++)
    fprintf(f,"00");
  fprintf(f,"\n");
}


static void shex1_write(struct GlobalVars *gv,FILE *f)
{
  bool firstsec = TRUE;
  struct LinkedSection *ls;
  unsigned long len,addr;
  uint8_t *p;

  while (ls = get_next_section(gv)) {
    if (ls->size == 0 || !(ls->flags & SF_ALLOC) || (ls->ld_flags & LSF_NOLOAD))
      continue;  /* ignore empty sections */

    if (firstsec) {
      if (gv->trace_file)
        fprintf(gv->trace_file,"Base address = 0x%08lx.\n",ls->copybase);
      firstsec = FALSE;
    }

    /* resolve all relocations and write section contents */
    calc_relocs(gv,ls);

    for (p=ls->data,addr=ls->copybase,len=ls->filesize; len>0; ) {
      int nbytes = (len>32) ? 32 : len;

      SHex1Out(f,addr,p,nbytes);
      p += nbytes;
      addr += nbytes;
      len -= nbytes;
    }
  }
  fprintf(f,"000000 0\n");
}
#endif


#endif
