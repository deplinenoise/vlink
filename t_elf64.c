/* $VER: vlink t_elf64.c V0.7 (13.02.00)
 *
 * This file is part of vlink, a portable linker for multiple
 * object formats.
 * Copyright (c) 1997-2005  Frank Wille
 *
 * vlink is freeware and part of the portable and retargetable ANSI C
 * compiler vbcc, copyright (c) 1995-2005 by Volker Barthelmann.
 * vlink may be freely redistributed as long as no modifications are
 * made and nothing is charged for it. Non-commercial usage is allowed
 * without any restrictions.
 * EVERY PRODUCT OR PROGRAM DERIVED DIRECTLY FROM MY SOURCE MAY NOT BE
 * SOLD COMMERCIALLY WITHOUT PERMISSION FROM THE AUTHOR.
 */


#include "config.h"
#if defined(ELF64_ALPHA)
#error This code does not work!
#define T_ELF64_C
#include "vlink.h"
#include "elf64.h"
#if defined(ELF64_ALPHA)
#include "rel_elfalpha.h"
#endif

#define ELF_VER 1


#ifdef ELF64_ALPHA
static int alpha64_identify(char*,uint8_t *,unsigned long);
static void alpha64_readconv(struct GlobalVars *,struct LinkFile *);
static void alpha64_readELF(struct GlobalVars *,struct LinkFile *,uint8_t *);
static uint8_t alpha64_cmpsecflags(uint8_t,uint8_t);
static struct Section *alpha64_bssdefault(struct ObjectUnit *);
static int alpha64_targetlink(struct GlobalVars *,struct LinkedSection *,
                              struct Section *);
static struct Symbol *alpha64_lnksym(struct GlobalVars *,struct Section *,
                                     struct XReference *);
static void alpha64_setlnksym(struct GlobalVars *,struct Symbol *,
                              struct XReference *);
static void alpha64_relocs(struct GlobalVars *,uint8_t *,struct ObjectUnit *,
                           struct Elf64_Shdr *);
static void alpha64_writeobject(struct GlobalVars *,FILE *);
static void alpha64_writeshared(struct GlobalVars *,FILE *);
static void alpha64_writeexec(struct GlobalVars *,FILE *);
static uint8_t alpha64_getrel(uint8_t,char *,uint32_t);

struct FFFuncs fff_elf64alpha = {
  "elf64alpha",
  NULL,
  NULL,
  sizeof(struct Elf64_Ehdr),
  alpha64_identify,
  alpha64_readconv,
  alpha64_cmpsecflags,
  alpha64_bssdefault,
  alpha64_targetlink,
  alpha64_lnksym,
  alpha64_setlnksym,
  alpha64_writeobject,
  alpha64_writeshared,
  alpha64_writeexec,
  0x2000,
  2,
  0x7ff0,  /*@@@ I have no information */
  NULL,
  0,
  0,
  0 /* little endian */
};
#endif

/* small data sections */
static char *sdata = ".sdata";
static char *sbss = ".sbss";
static char *sdata2 = ".sdata2";
static char *sbss2 = ".sbss2";


static int elf64le_identify(struct FFFuncs *,char *,struct Elf64_Ehdr *,
                            unsigned long,unsigned char,unsigned char,
                            uint16_t,uint32_t);
static void elf64le_check_ar_type(struct FFFuncs *,char *,
                                  struct Elf64_Ehdr *,unsigned char,
                                  unsigned char,uint32_t,uint16_t,uint16_t,uint16_t);
static char *elf64le_shstrtab(struct LinkFile *,struct Elf64_Ehdr *);
static char *elf64le_strtab(struct LinkFile *,struct Elf64_Ehdr *,int);
static struct Elf64_Sym *elf64le_symtab(struct LinkFile *,
                                        struct Elf64_Ehdr *,int);
static struct Elf64_Shdr *elf64le_shdr(struct LinkFile *lf,
                                       struct Elf64_Ehdr *,uint16_t);
static void elf64le_section(uint8_t *,struct ObjectUnit *,
                            struct Elf64_Shdr *,int);
static void elf64le_symbols(struct GlobalVars *,uint8_t *,struct ObjectUnit *,
                            struct Elf64_Shdr *);
static void elf64le_reloc(struct GlobalVars *,struct Elf64_Ehdr *,
                          struct Section *,uint32_t,struct Elf64_Rela *,
                          bool,uint8_t);
static struct Section *elf64le_bssdefault(struct ObjectUnit *);
static int elf64_targetlink(struct GlobalVars *,struct LinkedSection *,
                            struct Section *);
static struct Symbol *elf64_lnksym(struct GlobalVars *,struct Section *,
                                   struct XReference *);
static void elf64_setlnksym(struct GlobalVars *,struct Symbol *,
                            struct XReference *);
static void elf64le_header(FILE *,uint16_t,uint16_t,uint32_t,uint32_t,uint32_t,
                           uint32_t,uint16_t,uint16_t,uint16_t);
static void elf64le_writeshdrs(FILE *,uint32_t,uint32_t);
static void elf64le_stdsymtab(struct GlobalVars *,uint8_t,uint8_t);
static void elf64le_addsymlist(struct GlobalVars *,struct SymTabList *,
                               uint8_t,uint8_t);
static void elf64le_makeshdrs(struct GlobalVars *);
static void elf64le_addrelocs(struct GlobalVars *,
                              uint8_t (*)(uint8_t,char *,uint32_t));
static void elf64le_makeshstrtab(void);
static void elf64le_makestrtab(void);
static void elf64le_makesymtab(uint32_t);
static struct ShdrNode *elf64le_addshdr(uint32_t,uint32_t,uint32_t,uint32_t,uint32_t,
                                        uint32_t,uint32_t,uint32_t,uint32_t,uint32_t);
static uint32_t elf64le_addsym(struct SymTabList *,char *,uint32_t,uint32_t,
                             uint8_t,uint8_t,uint16_t);
static uint32_t elf64le_findsym(struct SymTabList *,char *,uint16_t);
static void elf64le_addrela(uint32_t,int32_t,uint32_t,uint8_t);

static void elf64_initlists(void);
static struct ShdrNode *elf64_newshdr(void);
static uint32_t elf64_addshdrstr(char *);
static uint32_t elf64_addstr(char *);
static uint32_t elf64_addstrlist(struct StrTabList *,char *);
static uint8_t elf64_getinfo(struct Symbol *);
static uint8_t elf64_getbind(struct Symbol *);
static uint16_t elf64_getshndx(struct Symbol *,uint8_t);
static void elf64_writesections(struct GlobalVars *,FILE *);
static void elf64_writestrtab(FILE *,struct StrTabList *);
static void elf64_writesymtab(FILE *,struct SymTabList *);
static void elf64_writerelocs(struct GlobalVars *,FILE *);


static char ELFid[4] = {   /* identification for all ELF files */
  0x7f,'E','L','F'
};

static struct ar_info ai;  /* for scanning library archives */
static char *shstrtab;     /* section header string table */
static char *nullstr = "";

/* static data required for output file generation */
static struct list shdrlist;
static struct list phdrlist;
static struct list relalist;
static struct SymTabList symlist;
static struct StrTabList shstrlist;
static struct StrTabList stringlist;
static uint32_t shdrindex;
static uint32_t symtabidx,shstrtabidx,strtabidx; /* indexes of Shdr names */
static uint32_t elfoffset;    /* current ELF file offset */
static int secsyms;         /* offset to find section symbols by shndx */



/*****************************************************************/
/*                          Read ELF                             */
/*****************************************************************/


#ifdef ELF64_ALPHA

static int alpha64_identify(char *name,uint8_t *p,unsigned long plen)
/* identify ELF-Alpha-64Bit-LittleEndian */
{
  return (elf64le_identify(&fff_elf64alpha,name,(struct Elf64_Ehdr *)p,plen,
                           ELFCLASS64,ELFDATA2LSB,EM_ALPHA,ELF_VER));
}


static void alpha64_readconv(struct GlobalVars *gv,struct LinkFile *lf)
/* Read elf64alpha executable / object / shared obj. */
{
  if (lf->type == ID_LIBARCH) {
    if (ar_init(&ai,(char *)lf->data,lf->length,lf->filename)) {
      while (ar_extract(&ai)) {
        lf->objname = allocstring(ai.name);
        alpha64_readELF(gv,lf,ai.data);
      }
    }
    else
      ierror("alpha64_readconv(): archive %s corrupted since last access",
             lf->pathname);
  }
  else {
    lf->objname = lf->filename;
    alpha64_readELF(gv,lf,lf->data);
  }
}


static void alpha64_readELF(struct GlobalVars *gv,struct LinkFile *lf,
                            uint8_t *elf)
{
  struct ObjectUnit *u;
  struct Elf64_Ehdr *ehdr = (struct Elf64_Ehdr *)elf;
  struct Elf64_Shdr *shdr;
  uint16_t i,num_shdr;

  if (lf->type == ID_LIBARCH)  /* check ar-member for correct format */
    elf64le_check_ar_type(&fff_elf64alpha,lf->pathname,ehdr,ELFCLASS64,
                          ELFDATA2LSB,ELF_VER,EM_ALPHA,EM_NONE,EM_NONE);

  shstrtab = elf64le_shstrtab(lf,ehdr);
  u = create_objunit(lf,lf->objname);

  switch (read16le(ehdr->e_type)) {

    case ET_REL:  /* relocatable object file */
      if (read16le(ehdr->e_phnum) > 0)
        error(47,lf->pathname,lf->objname);  /* ignoring program hdr. tab */
      num_shdr = read16le(ehdr->e_shnum);

      /* create vlink sections */
      for (i=1; i<num_shdr; i++) {
        shdr = elf64le_shdr(lf,ehdr,i);
        switch (read32le(shdr->sh_type)) {
          case SHT_PROGBITS:
          case SHT_NOBITS:
            elf64le_section(elf,u,shdr,i);  /* create a new section */
          default:
            break;
        }
      }

      /* parse the other section headers */
      for (i=1; i<num_shdr; i++) {
        shdr = elf64le_shdr(lf,ehdr,i);
        switch (read32le(shdr->sh_type)) {
          case SHT_NULL:
          case SHT_STRTAB:
          case SHT_PROGBITS:
          case SHT_NOBITS:
            break;
          case SHT_SYMTAB:
            elf64le_symbols(gv,elf,u,shdr);  /* symbol definitions */
            break;
          case SHT_REL:
          case SHT_RELA:
            alpha64_relocs(gv,elf,u,shdr);  /* relocation information */
            break;
          case SHT_NOTE:
            ierror("alpha64_readELF(): %s: Section header type %d is "
                   "currently not supported",lf->pathname,
                   (int)read32le(shdr->sh_type));
            break;
          default:
            /* section header type not needed in relocatable objects */
            error(48,lf->pathname,read32le(shdr->sh_type),lf->objname);
            break;
        }
      }
      break;

    case ET_DYN:  /* shared object file */
      num_shdr = read16le(ehdr->e_shnum);

      /* create vlink sections */
      for (i=1; i<num_shdr; i++) {
        shdr = elf64le_shdr(lf,ehdr,i);
        switch (read32le(shdr->sh_type)) {
          case SHT_PROGBITS:
          case SHT_NOBITS:
            elf64le_section(elf,u,shdr,i);  /* create a new section */
          default:
            break;
        }
      }

      /* parse the other section headers */
      for (i=1; i<num_shdr; i++) {
        shdr = elf64le_shdr(lf,ehdr,i);
        switch (read32le(shdr->sh_type)) {
          case SHT_NULL:
          case SHT_STRTAB:
          case SHT_PROGBITS:
          case SHT_NOBITS:
          case SHT_HASH:
          case SHT_DYNAMIC:
          case SHT_DYNSYM:
            break;
          case SHT_SYMTAB:
            elf64le_symbols(gv,elf,u,shdr);  /* symbol definitions */
            break;
          case SHT_NOTE:
            ierror("alpha64_readELF(): %s: Section header type %d is "
                   "currently not supported",lf->pathname,
                   (int)read32le(shdr->sh_type));
            break;
          default:
            /* section header type not needed in shared objects */
            error(60,lf->pathname,read32le(shdr->sh_type),lf->objname);
            break;
        }
      }
      break;

    case ET_EXEC: /* executable file */
      /* @@@ */
      ierror("alpha64_readELF(): %s: Executables are currently "
             "not supported",lf->pathname);
      break;

    default:
      error(41,lf->pathname,lf->objname);  /* illegal fmt./file corrupted */
      break;
  }

  /* add new object unit to the appropriate list */
  add_objunit(gv,u,FALSE);
}


static uint8_t alpha64_cmpsecflags(uint8_t oldflags,uint8_t newflags)
/* compare and verify target-specific section flags, */
/* return 0xff if sections are incompatible, otherwise return new flags */
{
  return (oldflags|newflags);
}


static struct Section *alpha64_bssdefault(struct ObjectUnit *ou)
/* Create a default BSS section for elfppc64be */
{
  return (elf64le_bssdefault(ou));
}


static int alpha64_targetlink(struct GlobalVars *gv,struct LinkedSection *ls,
                              struct Section *s)
/* returns 1, if target requires the combination of the two sections, */
/* returns -1, if target don't want to combine them, */
/* returns 0, if target doesn't care - standard linking rules are used. */
{
  return (elf64_targetlink(gv,ls,s));
}


static struct Symbol *alpha64_lnksym(struct GlobalVars *gv,
                                     struct Section *sec,
                                     struct XReference *xref)
{
  return (elf64_lnksym(gv,sec,xref));
}


static void alpha64_setlnksym(struct GlobalVars *gv,struct Symbol *xdef,
                              struct XReference *xref)
{
  elf64_setlnksym(gv,xdef,xref);
}


/* @@@@ hier gehts weiter @@@@ */
static void alpha64_relocs(struct GlobalVars *gv,uint8_t *elf,
                           struct ObjectUnit *ou,struct Elf64_Shdr *shdr)
{
  char *sec_name = shstrtab + read32le(shdr->sh_name);
  struct LinkFile *lf = ou->lnkfile;
  bool rela = read32le(shdr->sh_type) == SHT_RELA;
  uint8_t rtype,*data = elf + read32le(shdr->sh_offset);
  unsigned long entsize = read32le(shdr->sh_entsize);
  int nrelocs = (int)(read32le(shdr->sh_size) / (uint32_t)entsize);
  uint32_t symndx = read32le(shdr->sh_link);

  struct Section *s;

  if ((data < lf->data) || 
      (data + read32le(shdr->sh_size) > lf->data + lf->length))
    error(51,lf->pathname,"ppc64be reloc",lf->objname);  /* illegal offset */
  if (!(s = find_sect_id(ou,read32le(shdr->sh_info))))
    /* a section with this index doesn't exist! */
    error(52,lf->pathname,sec_name,lf->objname,(int)read32le(shdr->sh_info));

  while (nrelocs--) {
    rtype = R_NONE;
    switch (ELF64_R_TYPE_LE(((struct Elf64_Rela *)data)->r_info)) {
      case R_NONE:
        break;  /* ignore */
      case R_PPC_ADDR32:
        rtype = R_ADDR32;
        break;
      case R_PPC_ADDR24:
        rtype = R_ADDR26;
        break;
      case R_PPC_ADDR16:
        rtype = R_ADDR16;
        break;
      case R_PPC_ADDR16_LO:
        rtype = R_ADDR16_LO;
        break;
      case R_PPC_ADDR16_HI:
        rtype = R_ADDR16_HI;
        break;
      case R_PPC_ADDR16_HA:
        rtype = R_ADDR16_HA;
        break;
      case R_PPC_ADDR14:
        rtype = R_ADDR14;
        break;
      case R_PPC_ADDR14_BRTAKEN:
        rtype = R_ADDR14_BRTAKEN;
        break;
      case R_PPC_ADDR14_BRNTAKEN:
        rtype = R_ADDR14_BRNTAKEN;
        break;
      case R_PPC_REL24:
        rtype = R_REL26;
        break;
      case R_PPC_REL14:
        rtype = R_REL14;
        break;
      case R_PPC_REL14_BRTAKEN:
        rtype = R_REL14_BRTAKEN;
        break;
      case R_PPC_REL14_BRNTAKEN:
        rtype = R_REL14_BRNTAKEN;
        break;
      case R_PPC_REL32:
        rtype = R_REL32;
        break;
      case R_PPC_SDAREL16:
        rtype = R_BASEREL16;
        break;
      case R_PPC_PASM_TOC16:
        rtype = R_BASEREL16;
        break;
      default:
        ierror("alpha64_relocs(): %s (%s): Reloc type %d in %s is currently "
               "not supported",lf->pathname,lf->objname,
               ELF64_R_TYPE_LE(((struct Elf64_Rela *)data)->r_info),
               sec_name);
        break;
    }

    if (rtype > R_NONE)
      elf64le_reloc(gv,(struct Elf64_Ehdr *)elf,s,symndx,
                    (struct Elf64_Rela *)data,rela,rtype);
    data += entsize;  /* next reloc */
  }

}

#endif /* ELF64_ALPHA */


static int elf64le_identify(struct FFFuncs *ff,char *name,
                            struct Elf64_Ehdr *p,unsigned long plen,
                            unsigned char class,unsigned char endian,
                            uint16_t machine,uint32_t ver)
/* check a possible ELF file against the requirements, then */
/* return its type (object, library, shared object) */
{
  bool arflag = FALSE;

  if (plen < sizeof(struct Elf64_Ehdr))
    return (ID_UNKNOWN);

  if (ar_init(&ai,(char *)p,plen,name)) {
    /* library archive detected, extract 1st archive member */
    arflag = TRUE;
    if (!(ar_extract(&ai))) {
      error(38,name);  /* Empty archive ignored */
      return (ID_LIBARCH);
    }
    p = (struct Elf64_Ehdr *)ai.data;
  }

  if (!strncmp(p->e_ident,ELFid,4)) {
    /* ELF identification found */
    if (p->e_ident[EI_CLASS]==class && p->e_ident[EI_DATA]==endian &&
        p->e_ident[EI_VERSION]==(unsigned char)ver &&
        read32le(p->e_version)==ver && read16le(p->e_machine)==machine) {
      switch (read16le(p->e_type)) {
        case ET_REL:
          return (arflag ? ID_LIBARCH : ID_OBJECT);
        case ET_EXEC:
          if (arflag) /* no executables in library archives */
            error(40,name,ff->tname);
          return (ID_EXECUTABLE);
        case ET_DYN:
          if (arflag) /* no shared objects in library archives */
            error(39,name,ff->tname);
          return (ID_SHAREDOBJ);
        default:
          error(41,name,ff->tname);  /* illegal fmt. / file corrupted */
          break;
      }
    }
  }

  return (ID_UNKNOWN);
}


static void elf64le_check_ar_type(struct FFFuncs *ff,char *name,
                                  struct Elf64_Ehdr *ehdr,unsigned char class,
                                  unsigned char endian,uint32_t ver,
                                  uint16_t mach1,uint16_t mach2,uint16_t mach3)
/* check all library archive members before conversion */
{
  uint16_t m = read16le(ehdr->e_machine);

  if (!strncmp(ehdr->e_ident,ELFid,4)) {
    /* ELF identification found */
    if (ehdr->e_ident[EI_CLASS]==class && ehdr->e_ident[EI_DATA]==endian &&
        ehdr->e_ident[EI_VERSION]==(unsigned char)ver &&
        read32le(ehdr->e_version)==ver && (m==mach1 || m==mach2 || m==mach3)) {
      switch (read16le(ehdr->e_type)) {
        case ET_REL:
          return;
        case ET_EXEC:  /* no executables in library archives */
          error(40,name,ff->tname);
          break;
        case ET_DYN:  /* no shared objects in library archives */
          error(39,name,ff->tname);
          break;
        default:  /* illegal fmt. / file corrupted */
          break;
      }
    }
  }
  error(41,name,ff->tname);
}


static char *elf64le_shstrtab(struct LinkFile *lf,struct Elf64_Ehdr *ehdr)
/* returns a pointer to the section header string table, if present, */
/* or NULL, otherwise */
{
  uint16_t i;
  struct Elf64_Shdr *shdr;
  char *stab;

  if (i = read16le(ehdr->e_shstrndx)) {
    shdr = elf64le_shdr(lf,ehdr,i);
    if (read32le(shdr->sh_type) != SHT_STRTAB)
      error(45,lf->pathname,lf->objname);  /* illegal type */
    stab = ((char *)ehdr) + read32le(shdr->sh_offset);
    if (((uint8_t *)stab < lf->data) || 
        ((uint8_t *)stab + read32le(shdr->sh_size) > lf->data + lf->length))
      error(46,lf->pathname,lf->objname);  /* illegal offset */
    else
      return (stab);
  }
  return (NULL);
}


static char *elf64le_strtab(struct LinkFile *lf,
                            struct Elf64_Ehdr *ehdr,int idx)
/* returns a pointer to the string table */
{
  static char *tabname = "string";
  struct Elf64_Shdr *shdr;
  char *stab;

  shdr = elf64le_shdr(lf,ehdr,idx);
  if (read32le(shdr->sh_type) != SHT_STRTAB)
     error(50,lf->pathname,tabname,lf->objname);  /* illegal type */
  stab = ((char *)ehdr) + read32le(shdr->sh_offset);
  if (((uint8_t *)stab < lf->data) || 
      ((uint8_t *)stab + read32le(shdr->sh_size) > lf->data + lf->length))
    error(51,lf->pathname,tabname,lf->objname);  /* illegal offset */
  return (stab);
}


static struct Elf64_Sym *elf64le_symtab(struct LinkFile *lf,
                                        struct Elf64_Ehdr *ehdr,int idx)
/* returns a pointer to the symbol table */
{
  static char *tabname = "symbol";
  struct Elf64_Shdr *shdr;
  struct Elf64_Sym *symtab;

  shdr = elf64le_shdr(lf,ehdr,idx);
  if (read32le(shdr->sh_type) != SHT_SYMTAB)
     error(50,lf->pathname,tabname,lf->objname);  /* illegal type */
  symtab = (struct Elf64_Sym *)((uint8_t *)ehdr + read32le(shdr->sh_offset));
  if (((uint8_t *)symtab < lf->data) || 
      ((uint8_t *)symtab + read32le(shdr->sh_size) > lf->data + lf->length))
    error(51,lf->pathname,tabname,lf->objname);  /* illegal offset */
  return (symtab);
}


static struct Elf64_Shdr *elf64le_shdr(struct LinkFile *lf,
                                       struct Elf64_Ehdr *ehdr,uint16_t idx)
/* return pointer to section header #idx */
{
  struct Elf64_Shdr *shdr;

  if (idx < read16le(ehdr->e_shnum)) {
    shdr = (struct Elf64_Shdr *)(((char *)ehdr) + ((read32le(ehdr->e_shoff) +
           (uint32_t)read16le(ehdr->e_shentsize) * (uint32_t)idx)));
    if (((uint8_t *)shdr < lf->data) ||
        (((uint8_t *)shdr)+read16le(ehdr->e_shentsize) > lf->data+lf->length))
      /* section header #x has illegal offset */
      error(44,lf->pathname,(int)idx,lf->objname);
    return (shdr);
  }
  else  /* Invalid ELF section header index */
    error(43,lf->pathname,(int)idx,lf->objname);
  return (NULL);  /* not reached, for compiler's sake */
}


static void elf64le_section(uint8_t *elf,struct ObjectUnit *ou,
                            struct Elf64_Shdr *shdr,int shndx)
/* create a new section */
{
  char *sec_name = shstrtab + read32le(shdr->sh_name);
  uint8_t *data = elf + read32le(shdr->sh_offset);
  unsigned long size = (unsigned long)read32le(shdr->sh_size);
  uint32_t f = read32le(shdr->sh_flags);
  struct LinkFile *lf = ou->lnkfile;
  uint8_t type=ST_DATA,flags=0;
  struct Section *s;

  if (read32le(shdr->sh_type) == SHT_NOBITS) {
    data = NULL;
    type = ST_UDATA;
    flags |= SF_UNINITIALIZED;
  }
  else {
    if (data+size > lf->data+lf->length)  /* illegal section offset */
      error(49,lf->pathname,sec_name,lf->objname);
  }
  s = create_section(ou,sec_name,data,size);

  s->id = shndx;  /* use section header index for identification */
  s->protection = SP_READ;
  if ((f & SHF_EXECINSTR) && data) {
    type = ST_CODE;
    s->protection |= SP_EXEC;
  }
  if (f & SHF_WRITE)
    s->protection |= SP_WRITE;
  if (!(f & SHF_ALLOC))
    flags |= SF_DISCARD;

  s->type = type;
  s->flags = flags;
  s->alignment = (uint8_t)shiftval(read32le(shdr->sh_addralign));
  addtail(&ou->sections,&s->n);  /* add Section to ObjectUnit */
}


static void elf64le_symbols(struct GlobalVars *gv,uint8_t *elf,
                            struct ObjectUnit *ou,struct Elf64_Shdr *shdr)
/* convert ELF symbol definitions into internal format */
{
  struct LinkFile *lf = ou->lnkfile;
  uint8_t *data = elf + read32le(shdr->sh_offset);
  unsigned long entsize = read32le(shdr->sh_entsize);
  int nsyms = (int)(read32le(shdr->sh_size) / (uint32_t)entsize);
  char *strtab = elf64le_strtab(lf,(struct Elf64_Ehdr *)elf,
                                read32le(shdr->sh_link));
  struct Section *sec,*firstsec;
  struct Elf64_Sym *elfsym;

  if ((data < lf->data) || 
      (data + read32le(shdr->sh_size) > lf->data + lf->length))
    error(51,lf->pathname,"symbol",lf->objname);  /* illegal offset */

  if (listempty(&ou->sections))  /* not a single section? */
    firstsec = elf64le_bssdefault(ou);
  else
    firstsec = (struct Section *)ou->sections.first;

  /* read ELF xdef symbols and convert to internal format */
  while (--nsyms > 0) {
    int shndx;
    char *symname;
    uint8_t type,objinfo,objbind;

    elfsym = (struct Elf64_Sym *)(data += entsize);
    symname = strtab + read32le(elfsym->st_name);
    switch (shndx = (int)read16le(elfsym->st_shndx)) {
      case SHN_UNDEF:
        sec = NULL;  /* ignore xrefs for now */
        break;
      /* assign a section for ABS and COMMON symbols, to prevent */
      /* accidental NULL-pointer references */
      case SHN_ABS:
        sec = firstsec;
        type = SYM_ABS;
        break;
      case SHN_COMMON:
        sec = firstsec;
        type = SYM_COMMON;
        break;
      /* reloc symbols have a definite section to which they belong */
      default:
        if (!(sec = find_sect_id(ou,shndx))) {
          /* a section with this index doesn't exist! */
          if (ELF64_ST_TYPE(*elfsym->st_info) != STT_SECTION)
            error(53,lf->pathname,symname,lf->objname,shndx);
        }
        type = SYM_RELOC;
        break;
    }

    if ((objinfo = ELF64_ST_TYPE(*elfsym->st_info)) == STT_SECTION)
      sec = NULL;  /* ignore section defines - will be reproduced */

    if (sec) {
      struct Symbol *sym;

      if (objinfo > STT_FILE) {
        /* illegal symbol type */
        error(54,lf->pathname,(int)objinfo,symname,lf->objname);
        objinfo = STT_NOTYPE;
      }
      switch (ELF64_ST_BIND(*elfsym->st_info)) {
        case STB_LOCAL:
          objbind = SYMB_LOCAL;
          break;
        case STB_GLOBAL:
          objbind = SYMB_GLOBAL;
          break;
        case STB_WEAK:
          objbind = SYMB_WEAK;
          break;
        default:  /* illegal binding type */
          error(55,lf->pathname,symname,ELF64_ST_BIND(*elfsym->st_info),
                lf->objname);
          objbind = SYMB_LOCAL;
          break;
      }

      /* add a new symbol definition */
      if (objbind == SYMB_LOCAL) {
        /* always define local symbols - multiple defintions allowed */
        addlocsymbol(gv,sec,symname,NULL,read32le(elfsym->st_value),type,0,
                     objinfo,read32le(elfsym->st_size));
      }
      else {
        if (sym = addsymbol(gv,sec,symname,NULL,read32le(elfsym->st_value),
                            type,0,objinfo,objbind,
                            read32le(elfsym->st_size))) {
          /* symbol is multiply defined! */
          struct LinkFile *lf2 = sym->relsect->obj->lnkfile;
          char buf[256];  /* @@@ dangerous... */

          if (lf2->type == ID_LIBARCH)
            sprintf(buf,"%s (%s)",lf2->filename,lf2->objname);
          else
            strcpy(buf,lf2->objname);
          error(56,lf->pathname,symname,lf->objname,buf);
        }
      }
    }
  }
}


static void elf64le_reloc(struct GlobalVars *gv,struct Elf64_Ehdr *elf,
                          struct Section *s,uint32_t symndx,
                          struct Elf64_Rela *rel,bool rela,uint8_t rtype)
/* Insert relocations, which are relative to a defined symbol, into */
/* the section's reloc-list. If the symbol is undefined, create an */
/* external reference on it, with the supplied relocation type. */
{
  struct LinkFile *lf = s->obj->lnkfile;
  struct Elf64_Shdr *symhdr = elf64le_shdr(lf,elf,symndx);
  struct Elf64_Sym *sym = elf64le_symtab(lf,elf,symndx) +
                          ELF64_R_SYM_LE(rel->r_info);
  uint32_t offs = read32le(rel->r_offset);
  uint32_t shndx = (uint32_t)read16le(sym->st_shndx);
  int32_t a;

  /* if addend isn't defined in Reloc, read it from the section data */
  if (rela)
    a = (int32_t)read32le(rel->r_addend);
  else
    a = readsection(s->data+offs,rtype);

  if (shndx == SHN_UNDEF || shndx == SHN_COMMON) {
    /* undefined or common symbol - create external reference */
    char *xrefname = elf64le_strtab(lf,elf,read32le(symhdr->sh_link)) +
                                    read32le(sym->st_name);

    addxref(gv,s,xrefname,offs,rtype,relocsize(rtype),a);
  }

  else if (ELF64_ST_TYPE(*sym->st_info) == STT_SECTION) {
    /* a normal relocation, with an offset relative to a section base */

    addreloc(s,find_sect_id(s->obj,shndx),0,offs,rtype,a);
  }

  else if (ELF64_ST_TYPE(*sym->st_info)<STT_SECTION && shndx<SHN_ABS) {
    /* relocations, which are relative to a known symbol */

    addreloc(s,find_sect_id(s->obj,shndx),0,offs,rtype,
             (int32_t)read32le(sym->st_value) + a);
  }

  else
    ierror("elf64le_reloc(): %s (%s): Only relocations which are relative "
           "to a section, function or object are supported "
           "(sym=%s, ST_TYPE=%d)",lf->pathname,lf->objname,
           elf64le_strtab(lf,elf,read32le(symhdr->sh_link)) +
                          read32le(sym->st_name),
           ELF64_ST_TYPE(*sym->st_info));
}


static struct Section *elf64le_bssdefault(struct ObjectUnit *ou)
/* Create a default BSS section for ELF64 LittleEndian */
{
  static char *bssname = ".bss";
  struct Section *s = create_section(ou,bssname,NULL,0);

  s->flags |= SF_UNINITIALIZED;
  s->type = ST_UDATA;
  s->protection = SP_READ | SP_WRITE;
  s->alignment = 4;  /* @@@ */
  return (s);
}


static int elf64_targetlink(struct GlobalVars *gv,struct LinkedSection *ls,
                            struct Section *s)
/* returns 1, if target requires the combination of the two sections, */
/* returns -1, if target don't want to combine them, */
/* returns 0, if target doesn't care - standard linking rules are used. */
{
  if (!gv->dest_object) {
    if ((!strncmp(ls->name,sdata,6) && !strncmp(s->name,sbss,5)
         && *(ls->name+6) == *(s->name+5)) ||
        (!strncmp(ls->name,sbss,5) && !strncmp(s->name,sdata,6)
         && *(ls->name+5) == *(s->name+6)))
      /* .sdata/.sbss, .sdata2/.sbss2, etc. are always combined */
      return (1);
  }
  return (0);
}


static struct Symbol *elf64_lnksym(struct GlobalVars *gv,
                                     struct Section *sec,
                                     struct XReference *xref)
{
  return (NULL);
}


static void elf64_setlnksym(struct GlobalVars *gv,struct Symbol *xdef,
                              struct XReference *xref)
{
}



/*****************************************************************/
/*                          Write ELF                            */
/*****************************************************************/


#ifdef ELF64_ALPHA

static void alpha64_writeshared(struct GlobalVars *gv,FILE *f)
/* creates a target-alpha64le shared object (which is pos. independant) */
{
  ierror("alpha64_writeshared(): Shared object generation has not "
         "yet been implemented");
}


static void alpha64_writeobject(struct GlobalVars *gv,FILE *f)
/* creates a target-elfppc64be relocatable object file */
{
  uint32_t sh_off,shstrndx,stabndx;

  elf64_initlists();
  elf64le_addsym(&symlist,nullstr,0,0,0,0,SHN_UNDEF);
  if (gv->discard_local < DISLOC_ALL)
    elf64le_stdsymtab(gv,STB_LOCAL,STT_FILE);

  elf64le_makeshdrs(gv);
  if (gv->discard_local < DISLOC_ALL)
    elf64le_stdsymtab(gv,STB_LOCAL,0);
  symlist.global_index = symlist.nextindex;
  if (gv->strip_symbols < STRIP_ALL) { /* although not recomm. for objects */
    elf64le_stdsymtab(gv,STB_WEAK,0);
    elf64le_stdsymtab(gv,STB_GLOBAL,0);
  }
  elf64le_addrelocs(gv,alpha64_getrel);

  shstrndx = shdrindex;
  elf64le_makeshstrtab();
  sh_off = elfoffset;
  stabndx = shdrindex;
  elfoffset += (shdrindex+2) * sizeof(struct Elf64_Shdr);
  elf64le_makesymtab(shdrindex+1);
  elf64le_makestrtab();

  elf64le_header(f,ET_REL,EM_PPC,0,0,sh_off,0,0,shdrindex,shstrndx);
  elf64_writesections(gv,f);
  elf64_writestrtab(f,&shstrlist);
  fwrite_align(f,2,ftell(f));
  elf64le_writeshdrs(f,elfoffset,stabndx);
  elf64_writesymtab(f,&symlist);
  elf64_writestrtab(f,&stringlist);
  fwrite_align(f,2,ftell(f));
  elf64_writerelocs(gv,f);
}


static void alpha64_writeexec(struct GlobalVars *gv,FILE *f)
/* creates a target-elfppc64be executable file (with absolute addresses) */
{
  ierror("alpha64_writeexec(): Executable file generation has not "
         "yet been implemented");
}


static uint8_t alpha64_getrel(uint8_t rtype,char *sname,uint32_t offs)
{
  switch (rtype) {
    case R_NONE:
      break;
    case R_ADDR32:
      return (R_PPC_ADDR32);
    case R_ADDR26:
      return (R_PPC_ADDR24);
    case R_ADDR16:
      return (R_PPC_ADDR16);
    case R_ADDR16_LO:
      return (R_PPC_ADDR16_LO);
    case R_ADDR16_HI:
      return (R_PPC_ADDR16_HI);
    case R_ADDR16_HA:
      return (R_PPC_ADDR16_HA);
    case R_ADDR14:
      return (R_PPC_ADDR14);
    case R_ADDR14_BRTAKEN:
      return (R_PPC_ADDR14_BRTAKEN);
    case R_ADDR14_BRNTAKEN:
      return (R_PPC_ADDR14_BRNTAKEN);
    case R_REL26:
      return (R_PPC_REL24);
    case R_REL14:
      return (R_PPC_REL14);
    case R_REL14_BRTAKEN:
      return (R_PPC_REL14_BRTAKEN);
    case R_REL14_BRNTAKEN:
      return (R_PPC_REL14_BRNTAKEN);
    case R_BASEREL16:
      return (R_PPC_SDAREL16);
    default:
      /* unsupported relocation type */
      error(32,fff_elf32ppcbe.tname,reloc_name[rtype],sname,offs);
      break;
  }
  return (R_NONE);
}

#endif /* ELF64_ALPHA */


static void elf64le_header(FILE *f,uint16_t type,uint16_t mach,uint32_t entry,
                           uint32_t phoff,uint32_t shoff,uint32_t flags,
                           uint16_t phnum,uint16_t shnum,uint16_t shstrndx)
/* write ELF header for 32-bit big endian */
{
  struct Elf64_Ehdr eh;

  memset(&eh,0,sizeof(struct Elf64_Ehdr));
  strncpy((char *)eh.e_ident,ELFid,4);
  eh.e_ident[EI_CLASS] = ELFCLASS32;
  eh.e_ident[EI_DATA] = ELFDATA2MSB;
  eh.e_ident[EI_VERSION] = ELF_VER;
  write16le(eh.e_type,type);
  write16le(eh.e_machine,mach);
  write32le(eh.e_version,ELF_VER);
  write32le(eh.e_entry,entry);
  write32le(eh.e_phoff,phoff);
  write32le(eh.e_shoff,shoff);
  write32le(eh.e_flags,flags);
  write16le(eh.e_ehsize,sizeof(struct Elf64_Ehdr));
  write16le(eh.e_phentsize,phnum ? sizeof(struct Elf64_Phdr):0);
  write16le(eh.e_phnum,phnum);
  write16le(eh.e_shentsize,shnum ? sizeof(struct Elf64_Shdr):0);
  write16le(eh.e_shnum,shnum);
  write16le(eh.e_shstrndx,shstrndx);
  fwritex(f,&eh,sizeof(struct Elf64_Ehdr));
}


static void elf64le_writeshdrs(FILE *f,uint32_t reloffset,uint32_t stabndx)
/* write all section headers */
{
  struct ShdrNode *shn;
  uint32_t type;

  while (shn = (struct ShdrNode *)remhead(&shdrlist)) {
    type = read32le(shn->s.sh_type);
    if (type == SHT_RELA || type == SHT_REL) {
      /* patch correct sh_offset and sh_link for reloc header */
      write32le(shn->s.sh_offset,read32le(shn->s.sh_offset)+reloffset);
      write32le(shn->s.sh_link,stabndx);
    }
    fwritex(f,&shn->s,sizeof(struct Elf64_Shdr));
  }
}


static void elf64le_stdsymtab(struct GlobalVars *gv,uint8_t bind,uint8_t type)
{
  elf64le_addsymlist(gv,&symlist,bind,type);
}


static void elf64le_addsymlist(struct GlobalVars *gv,struct SymTabList *sl,
                               uint8_t bind,uint8_t type)
/* add all symbols with specified bind and type to the ELF symbol list */
{
  struct LinkedSection *ls = (struct LinkedSection *)gv->lnksec.first;
  struct LinkedSection *nextls;
  struct Symbol *nextsym,*sym;

  while (nextls = (struct LinkedSection *)ls->n.next) {
    sym = (struct Symbol *)ls->symbols.first;

    while (nextsym = (struct Symbol *)sym->n.next) {
      uint8_t symtype = elf64_getinfo(sym);
      uint8_t symbind = elf64_getbind(sym);

      if (symbind == bind && (!type || (symtype == type))) {
        remnode(&sym->n);
        elf64le_addsym(sl,sym->name,sym->value,sym->size,
                       symbind,symtype,elf64_getshndx(sym,symtype));
        free(sym);
      }
      sym = nextsym;
    }
    ls = nextls;
  }
}


static void elf64le_makeshdrs(struct GlobalVars *gv)
/* generate all ELF section headers */
{
  struct LinkedSection *ls = (struct LinkedSection *)gv->lnksec.first;
  struct LinkedSection *nextls;
  struct ShdrNode *shn;
  struct SymbolNode *sym;
  uint32_t f;
  bool bss;

  /* offset, to find section-symbols by section header index */
  secsyms = (int)symlist.nextindex - (int)shdrindex;

  while (nextls = (struct LinkedSection *)ls->n.next) {
    ls->index = (int)shdrindex;
    bss = ls->type==ST_UDATA || (ls->flags&SF_UNINITIALIZED);
    f = (ls->flags & SF_DISCARD) ? 0 : SHF_ALLOC;
    if (ls->protection & SP_WRITE)
      f |= SHF_WRITE;
    if (ls->protection & SP_EXEC)
      f |= SHF_EXECINSTR;
    elf64le_addshdr(elf64_addshdrstr(ls->name),bss?SHT_NOBITS:SHT_PROGBITS,
                    f,0,elfoffset,ls->size,0,0,1<<(uint32_t)ls->alignment,0);
    if (!bss)
      elfoffset += ls->size;

    /* add section symbol (without name) */
    elf64le_addsym(&symlist,nullstr,0,0,STB_LOCAL,STT_SECTION,
                   (uint16_t)ls->index);
    ls = nextls;  /* next section */
  }
}


static void elf64le_addrelocs(struct GlobalVars *gv,
                              uint8_t (*getrel)(uint8_t,char *,uint32_t))
/* creates relocations for all sections */
{
  struct LinkedSection *ls = (struct LinkedSection *)gv->lnksec.first;
  struct LinkedSection *nextls;
  struct Reloc *rel,*nextrel;
  struct XReference *xref,*nextxref;
  uint32_t symidx,sroffs=0,roffs=0;

  while (nextls = (struct LinkedSection *)ls->n.next) {
    sroffs = roffs;

    /* relocations */
    rel = (struct Reloc *)ls->relocs.first;
    while (nextrel = (struct Reloc *)rel->n.next) {
      elf64le_addrela((uint32_t)rel->offset,rel->addend,
                      (uint32_t)(rel->relocsect.lnk->index + secsyms),
                      getrel(rel->type,ls->name,(uint32_t)rel->offset));
      roffs += gv->short_rel ?
               sizeof(struct Elf64_Rel) : sizeof(struct Elf64_Rela);
      rel = nextrel;
    }

    /* external references */
    xref = (struct XReference *)ls->xrefs.first;
    while (nextxref = (struct XReference *)xref->n.next) {
      /* undefined symbol */
      if (!(symidx = elf64le_findsym(&symlist,xref->name,SHN_UNDEF)))
        symidx = elf64le_addsym(&symlist,xref->name,0,0,STB_GLOBAL,
                                STT_NOTYPE,SHN_UNDEF);
      /* symbol's relocation */
      elf64le_addrela((uint32_t)xref->offset,xref->addend,symidx,
                      getrel(xref->type,ls->name,(uint32_t)xref->offset));
      roffs += gv->short_rel ?
               sizeof(struct Elf64_Rel) : sizeof(struct Elf64_Rela);
      xref = nextxref;
    }

    if (roffs != sroffs) {
      /* create ".rel(a).name" header */
      char *relname = (char *)alloc(strlen(ls->name)+6);

      sprintf(relname,".%s%s",gv->short_rel ? "rel":"rela",ls->name);
      elf64le_addshdr(elf64_addshdrstr(relname),
                      gv->short_rel ? SHT_REL:SHT_RELA,
                      0,0,sroffs,roffs-sroffs,0,(uint32_t)ls->index,4,
                      gv->short_rel ? sizeof(struct Elf64_Rel) : 
                                      sizeof(struct Elf64_Rela));
      /* sroffs is relative and will be corrected later */
      /* sh_link will be initialized, when .symtab exists */
    }
    ls = nextls;  /* next section */
  }
}


static void elf64le_makeshstrtab()
/* creates .shstrtab */
{
  elf64le_addshdr(shstrtabidx,SHT_STRTAB,0,0,elfoffset,
                  shstrlist.nextindex,0,0,1,0);
  elfoffset += shstrlist.nextindex;
  elfoffset += align(elfoffset,2);
}


static void elf64le_makestrtab()
/* creates .strtab */
{
  elf64le_addshdr(strtabidx,SHT_STRTAB,0,0,elfoffset,
                  stringlist.nextindex,0,0,1,0);
  elfoffset += stringlist.nextindex;
  elfoffset += align(elfoffset,2);
}


static void elf64le_makesymtab(uint32_t strtabindex)
/* creates .symtab */
{
  elf64le_addshdr(symtabidx,SHT_SYMTAB,0,0,elfoffset,
                  symlist.nextindex * sizeof(struct Elf64_Sym),
                  strtabindex,symlist.global_index,4,
                  sizeof(struct Elf64_Sym));
  elfoffset += symlist.nextindex * sizeof(struct Elf64_Sym);
}


static struct ShdrNode *elf64le_addshdr(uint32_t name,uint32_t type,uint32_t flags,
                                        uint32_t addr,uint32_t offset,
                                        uint32_t size,uint32_t link,uint32_t info,
                                        uint32_t align,uint32_t entsize)
{
  struct ShdrNode *shn = elf64_newshdr();

  write32le(shn->s.sh_name,name);
  write32le(shn->s.sh_type,type);
  write32le(shn->s.sh_flags,flags);
  write32le(shn->s.sh_addr,addr);
  write32le(shn->s.sh_offset,offset);
  write32le(shn->s.sh_size,size);
  write32le(shn->s.sh_link,link);
  write32le(shn->s.sh_info,info);
  write32le(shn->s.sh_addralign,align);
  write32le(shn->s.sh_entsize,entsize);
  return (shn);
}


static uint32_t elf64le_addsym(struct SymTabList *sl,char *name,uint32_t value,
                             uint32_t size,uint8_t bind,uint8_t type,uint16_t shndx)
/* add a new ELF symbol, return its symbol table index */
{
  struct SymbolNode **chain = &sl->hashtab[elf_hash(name) % SYMHTABSIZE];
  struct SymbolNode *sym;

  while (sym = *chain)
    chain = &sym->hashchain;
  /* new symbol table entry */
  *chain = sym = alloczero(sizeof(struct SymbolNode));
  sym->name = name;
  sym->index = sl->nextindex++;
  addtail(&sl->l,&sym->n);
  write32le(sym->s.st_name,elf64_addstr(name));
  write32le(sym->s.st_value,value);
  write32le(sym->s.st_size,size);
  *sym->s.st_info = ELF64_ST_INFO(bind,type);
  write16le(sym->s.st_shndx,shndx);
  return (sym->index);
}


static uint32_t elf64le_findsym(struct SymTabList *sl,char *name,uint16_t shndx)
/* find an ELF symbol by its name and shndx */
/* return its symbol table index, index=0 means 'not found' */
{
  struct SymbolNode **chain = &sl->hashtab[elf_hash(name) % SYMHTABSIZE];
  struct SymbolNode *sym;

  while (sym = *chain) {
    if (!strcmp(name,sym->name))
      if (read16le(sym->s.st_shndx) == shndx)
        return (sym->index);
    chain = &sym->hashchain;
  }
  return (0);
}


static void elf64le_addrela(uint32_t offs,int32_t addend,uint32_t sym,uint8_t type)
{
  struct RelaNode *rn = alloc(sizeof(struct RelaNode));

  write32le(rn->r.r_offset,offs);
  write32le(rn->r.r_addend,addend);
  ELF64_R_INFO_LE(rn->r.r_info,sym,(uint32_t)type);
  addtail(&relalist,&rn->n);
}


static void elf64_initlists()
/* initialize section header, program header, relocation, symbol, */
/* string and section header string lists */
{
  static struct StrTabNode *str_hashtab[STRHTABSIZE];
  static struct StrTabNode *shstr_hashtab[SHSTRHTABSIZE];
  static struct SymbolNode *sym_hashtab[SYMHTABSIZE];

  elfoffset = sizeof(struct Elf64_Ehdr);
  initlist(&shdrlist);
  initlist(&phdrlist);
  initlist(&relalist);
  shdrindex = 0;

  initlist(&symlist.l);
  symlist.hashtab = sym_hashtab;
  memset(sym_hashtab,0,SYMHTABSIZE*sizeof(struct SymbolNode *));
  symlist.nextindex = symlist.global_index = 0;

  initlist(&shstrlist.l);
  initlist(&stringlist.l);
  shstrlist.hashtab = shstr_hashtab;
  stringlist.hashtab = str_hashtab;
  shstrlist.nextindex = stringlist.nextindex = 0;
  stringlist.htabsize = STRHTABSIZE;
  shstrlist.htabsize = SHSTRHTABSIZE;
  memset(shstr_hashtab,0,SHSTRHTABSIZE*sizeof(struct StrTabNode *));
  memset(str_hashtab,0,STRHTABSIZE*sizeof(struct StrTabNode *));

  elf64_addshdrstr(nullstr);  /* first string is always "" */
  symtabidx = elf64_addshdrstr(".symtab");
  strtabidx = elf64_addshdrstr(".strtab");
  shstrtabidx = elf64_addshdrstr(".shstrtab");
  elf64_addstr(nullstr);
  elf64_newshdr();          /* first Shdr is always zero */
}


static struct ShdrNode *elf64_newshdr()
{
  struct ShdrNode *s = alloczero(sizeof(struct ShdrNode));

  addtail(&shdrlist,&s->n);
  ++shdrindex;
  return (s);
}


static uint32_t elf64_addshdrstr(char *s)
{
  return (elf64_addstrlist(&shstrlist,s));
}


static uint32_t elf64_addstr(char *s)
{
  return (elf64_addstrlist(&stringlist,s));
}


static uint32_t elf64_addstrlist(struct StrTabList *sl,char *s)
/* add a new string to an ELF string table and return its index */
{
  struct StrTabNode **chain = &sl->hashtab[elf_hash(s) % sl->htabsize];
  struct StrTabNode *sn;

  /* search string in hash table */
  while (sn = *chain) {
    if (!strcmp(s,sn->str))
      return (sn->index);  /* it's already in, return index */
    chain = &sn->hashchain;
  }

  /* new string table entry */
  *chain = sn = alloc(sizeof(struct StrTabNode));
  sn->hashchain = NULL;
  sn->str = s;
  sn->index = sl->nextindex;
  addtail(&sl->l,&sn->n);
  sl->nextindex += (uint32_t)strlen(s) + 1;
  return (sn->index);
}


static uint8_t elf64_getinfo(struct Symbol *sym)
{
  uint8_t type;

  switch (sym->info) {
    case SYMI_NOTYPE:
      type = STT_NOTYPE;  /* @@@ Is this allowed? */
      break;
    case SYMI_OBJECT:
      type = STT_OBJECT;
      break;
    case SYMI_FUNC:
      type = STT_FUNC;
      break;
    case SYMI_SECTION:
      ierror("elf64le_addsymlist(): STT_SECTION symbol detected");
      type = STT_SECTION;
      break;
    case SYMI_FILE:
      type = STT_FILE;
      break;
    default:
      ierror("elf64_getinfo(): Illegal symbol info: %d",(int)sym->info);
      break;
  }
  return (type);
}


static uint8_t elf64_getbind(struct Symbol *sym)
{
  uint8_t bind;

  switch (sym->bind) {
    case SYMB_LOCAL:
      bind = STB_LOCAL;
      break;
    case SYMB_GLOBAL:
      bind = STB_GLOBAL;
      break;
    case SYMB_WEAK:
      bind = STB_WEAK;
      break;
    default:
      ierror("elf64_getbind(): Illegal symbol binding: %d",(int)sym->bind);
      break;
  }
  return (bind);
}


static uint16_t elf64_getshndx(struct Symbol *sym,uint8_t symtype)
{
  uint16_t shndx;

  switch (sym->type) {
    case SYM_UNDEF:
      shndx = SHN_UNDEF;
      break;
    case SYM_ABS:
      shndx = SHN_ABS;
      break;
    case SYM_RELOC:
      if (symtype > STT_FUNC)
        ierror("elf64_getshndx(): %s is relocatable, but not a "
               "function or object (type %d)",sym->name,(int)symtype);
      shndx = (uint16_t)sym->relsect->lnksec->index;
      break;
    case SYM_COMMON:
      shndx = SHN_COMMON;
      break;
    default:
      ierror("elf64_getshndx(): Illegal symbol type: %d",(int)sym->type);
      break;
  }
  return (shndx);
}


static void elf64_writesections(struct GlobalVars *gv,FILE *f)
/* write all linked sections */
{
  struct LinkedSection *ls = (struct LinkedSection *)gv->lnksec.first;
  struct LinkedSection *nextls;

  while (nextls = (struct LinkedSection *)ls->n.next) {
    if (!(ls->flags & SF_UNINITIALIZED))
      fwritex(f,ls->data,ls->size);
    ls = nextls;
  }
}


static void elf64_writestrtab(FILE *f,struct StrTabList *sl)
{
  struct StrTabNode *stn;

  while (stn = (struct StrTabNode *)remhead(&sl->l))
    fwritex(f,stn->str,strlen(stn->str)+1);
}


static void elf64_writesymtab(FILE *f,struct SymTabList *sl)
{
  struct SymbolNode *sym;

  while (sym = (struct SymbolNode *)remhead(&sl->l))
    fwritex(f,&sym->s,sizeof(struct Elf64_Sym));
}


static void elf64_writerelocs(struct GlobalVars *gv,FILE *f)
{
  struct RelaNode *rn;

  while (rn = (struct RelaNode *)remhead(&relalist))
    fwritex(f,&rn->r,gv->short_rel ? sizeof(struct Elf64_Rel) : 
                                     sizeof(struct Elf64_Rela));
}


#endif
