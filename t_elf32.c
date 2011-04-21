/* $VER: vlink t_elf32.c V0.13 (25.11.10)
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
#ifdef ELF32
#define T_ELF32_C
#include "vlink.h"
#include "elf32.h"
#include "stabdefs.h"


static char ELFid[4] = {   /* identification for all ELF files */
  0x7f,'E','L','F'
};

/* table to determine number of buckets in .hash */
static const size_t elf_buckets[] =
{
  1, 3, 17, 37, 67, 97, 131, 197, 263, 521, 1031, 2053, 4099, 8209,
  16411, 32771, 0
};

/* static data required for output file generation */
static struct list shdrlist;
static struct list phdrlist;
static struct list relalist;
static struct Section *gotsec = NULL;
static struct Section *pltsec = NULL;
static struct Section *dynrelocs = NULL;
static struct Section *pltrelocs = NULL;
static struct Section *dynamic = NULL;
/* .hash table */
static struct SymbolNode **dyn_hash;
static size_t dyn_hash_entries;
/* list of dynamic symbols */
static struct list dynsym_list;
/* indexes of Shdr names */
static uint32_t symtabidx,shstrtabidx,strtabidx,stabdebugidx;
static struct ShdrNode *stabshdr = NULL;
static int secsyms;  /* offset to find section symbols by shndx */
static unsigned long file_hdr_gap;  /* gap between hdr and first segment */

/* global data for all ELF targets */
struct SymTabList elf32symlist = { {NULL,NULL,NULL},NULL,SYMHTABSIZE,0,0 };
struct SymTabList elf32dsymlist = { {NULL,NULL,NULL},NULL,DYNSYMHTABSIZE,0,0 };
struct StrTabList elf32stringlist = { {NULL,NULL,NULL},NULL,STRHTABSIZE,0 };
struct StrTabList elf32dstrlist = { {NULL,NULL,NULL},NULL,DYNSTRHTABSIZE,0 };
struct StrTabList elf32shstrlist = { {NULL,NULL,NULL},NULL,SHSTRHTABSIZE,0 };
struct list elf32stabcompunits;
uint32_t elf32shdridx;
unsigned long elf32offset;   /* current ELF file offset */
int8_t elf32_endianess = -1; /* used while creating output-file */

/* elf32 common linker symbols */
const char *elf32_symnames[] = {
  sdabase_name,sda2base_name,"__CTOR_LIST__","__DTOR_LIST__",
  gotbase_name+1,pltbase_name+1,dynamic_name+1
};

/* ELF section names */
const char note_name[] = ".note";
const char dyn_name[] = ".dynamic";
const char hash_name[] = ".hash";
const char dynsym_name[] = ".dynsym";
const char dynstr_name[] = ".dynstr";
const char *dynrel_name[2] = { ".rel.dyn",".rela.dyn" };
const char *pltrel_name[2] = { ".rel.plt",".rela.plt" };



/*****************************************************************/
/*                          Read ELF                             */
/*****************************************************************/


int elf32_identify(struct FFFuncs *ff,char *name,struct Elf32_Ehdr *p,
                   unsigned long plen,unsigned char class,
                   unsigned char endian,uint16_t machine,uint32_t ver)
/* check a possible ELF file against the requirements, then */
/* return its type (object, library, shared object) */
{
  bool arflag = FALSE;
  bool be = (endian == ELFDATA2MSB);
  struct ar_info ai;

  if (plen < sizeof(struct Elf32_Ehdr))
    return ID_UNKNOWN;

  if (ar_init(&ai,(char *)p,plen,name)) {
    /* library archive detected, extract 1st archive member */
    arflag = TRUE;
    if (!(ar_extract(&ai))) {
      error(38,name);  /* Empty archive ignored */
      return ID_IGNORE;
    }
    p = (struct Elf32_Ehdr *)ai.data;
  }

  if (!strncmp(p->e_ident,ELFid,4)) {
    /* ELF identification found */
    if (p->e_ident[EI_CLASS]==class && p->e_ident[EI_DATA]==endian &&
        p->e_ident[EI_VERSION]==(unsigned char)ver &&
        read32(be,p->e_version)==ver && read16(be,p->e_machine)==machine) {
      switch (read16(be,p->e_type)) {
        case ET_REL:
          return arflag ? ID_LIBARCH : ID_OBJECT;
        case ET_EXEC:
          if (arflag) /* no executables in library archives */
            error(40,name,ff->tname);
          return ID_EXECUTABLE;
        case ET_DYN:
          if (arflag) /* no shared objects in library archives */
            error(39,name,ff->tname);
          return ID_SHAREDOBJ;
        default:
          error(41,name,ff->tname);  /* illegal fmt. / file corrupted */
          break;
      }
    }
  }

  return ID_UNKNOWN;
}


void elf32_check_ar_type(struct FFFuncs *ff,const char *name,
                         struct Elf32_Ehdr *ehdr,unsigned char class,
                         unsigned char endian,uint32_t ver,int nmach,...)
/* check all library archive members before conversion */
{
  bool be = (endian == ELFDATA2MSB);
  uint16_t m = read16(be,ehdr->e_machine);
  va_list vl;

  va_start(vl,nmach);
  if (!strncmp(ehdr->e_ident,ELFid,4)) {
    /* ELF identification found */
    if (ehdr->e_ident[EI_CLASS]==class && ehdr->e_ident[EI_DATA]==endian &&
        ehdr->e_ident[EI_VERSION]==(unsigned char)ver &&
        read32(be,ehdr->e_version)==ver && nmach>0) {
      while (nmach--) {
        if (va_arg(vl,int) == m)
          m = 0;
      }
      if (m == 0) {
        switch (read16(be,ehdr->e_type)) {
          case ET_REL:
            goto check_ar_exit;
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
  }
  error(41,name,ff->tname);

check_ar_exit:
  va_end(vl);
}


char *elf32_shstrtab(struct LinkFile *lf,struct Elf32_Ehdr *ehdr)
/* returns a pointer to the section header string table, if present, */
/* or NULL, otherwise */
{
  bool be = (ehdr->e_ident[EI_DATA] == ELFDATA2MSB);
  uint16_t i;
  struct Elf32_Shdr *shdr;
  char *stab;

  if (i = read16(be,ehdr->e_shstrndx)) {
    shdr = elf32_shdr(lf,ehdr,i);
    if (read32(be,shdr->sh_type) != SHT_STRTAB)
      error(45,lf->pathname,lf->objname);  /* illegal type */
    stab = ((char *)ehdr) + read32(be,shdr->sh_offset);
    if (((uint8_t *)stab < lf->data) || 
        ((uint8_t *)stab + read32(be,shdr->sh_size) > lf->data + lf->length))
      error(46,lf->pathname,lf->objname);  /* illegal offset */
    else
      return stab;
  }
  return NULL;
}


char *elf32_strtab(struct LinkFile *lf,struct Elf32_Ehdr *ehdr,int idx)
/* returns a pointer to the string table */
{
  static char *tabname = "string";
  bool be = (ehdr->e_ident[EI_DATA] == ELFDATA2MSB);
  struct Elf32_Shdr *shdr;
  char *stab;

  shdr = elf32_shdr(lf,ehdr,idx);
  if (read32(be,shdr->sh_type) != SHT_STRTAB)
     error(50,lf->pathname,tabname,lf->objname);  /* illegal type */
  stab = ((char *)ehdr) + read32(be,shdr->sh_offset);
  if (((uint8_t *)stab < lf->data) || 
      ((uint8_t *)stab + read32(be,shdr->sh_size) > lf->data + lf->length))
    error(51,lf->pathname,tabname,lf->objname);  /* illegal offset */
  return stab;
}


struct Elf32_Sym *elf32_symtab(struct LinkFile *lf,struct Elf32_Ehdr *ehdr,
                               int idx)
/* returns a pointer to the symbol table */
{
  static char *tabname = "symbol";
  bool be = (ehdr->e_ident[EI_DATA] == ELFDATA2MSB);
  struct Elf32_Shdr *shdr;
  struct Elf32_Sym *symtab;
  uint32_t shtype;

  shdr = elf32_shdr(lf,ehdr,idx);
  shtype = read32(be,shdr->sh_type);
  if (shtype!=SHT_SYMTAB && shtype!=SHT_DYNSYM)
     error(50,lf->pathname,tabname,lf->objname);  /* illegal type */
  symtab = (struct Elf32_Sym *)((uint8_t *)ehdr + read32(be,shdr->sh_offset));
  if (((uint8_t *)symtab < lf->data) || 
      ((uint8_t *)symtab + read32(be,shdr->sh_size) > lf->data + lf->length))
    error(51,lf->pathname,tabname,lf->objname);  /* illegal offset */
  return symtab;
}


struct Elf32_Shdr *elf32_shdr(struct LinkFile *lf,struct Elf32_Ehdr *ehdr,
                              uint16_t idx)
/* return pointer to section header #idx */
{
  bool be = (ehdr->e_ident[EI_DATA] == ELFDATA2MSB);
  struct Elf32_Shdr *shdr;

  if (idx < read16(be,ehdr->e_shnum)) {
    shdr = (struct Elf32_Shdr *)(((char *)ehdr) + ((read32(be,ehdr->e_shoff) +
           (uint32_t)read16(be,ehdr->e_shentsize) * (uint32_t)idx)));
    if (((uint8_t *)shdr < lf->data) ||
        (((uint8_t *)shdr)+read16(be,ehdr->e_shentsize) > lf->data+lf->length))
      /* section header #x has illegal offset */
      error(44,lf->pathname,(int)idx,lf->objname);
    return shdr;
  }
  else  /* Invalid ELF section header index */
    error(43,lf->pathname,(int)idx,lf->objname);
  return NULL;  /* not reached, for compiler's sake */
}


void elf32_section(struct GlobalVars *gv,struct Elf32_Ehdr *ehdr,
                   struct ObjectUnit *ou,struct Elf32_Shdr *shdr,
                   int shndx,char *shstrtab)
/* create a new section */
{
  bool be = (ehdr->e_ident[EI_DATA] == ELFDATA2MSB);
  struct LinkFile *lf = ou->lnkfile;
  char *sec_name = shstrtab + read32(be,shdr->sh_name);
  uint8_t *data = (uint8_t *)ehdr + read32(be,shdr->sh_offset);
  unsigned long size = (unsigned long)read32(be,shdr->sh_size);
  uint32_t f = read32(be,shdr->sh_flags);
  uint8_t type=ST_DATA,flags=0,prot=SP_READ;
  struct Section *s;

  if (gv->strip_symbols>=STRIP_DEBUG &&
      (!strncmp(sec_name,".debug",6) ||
       !strncmp(sec_name,".line",5) ||
       !strncmp(sec_name,".stab",5)))
    return;   /* ignore debugging sections when -S or -s is given */

  if (read32(be,shdr->sh_type) == SHT_NOBITS) {
    data = NULL;
    type = ST_UDATA;
    flags |= SF_UNINITIALIZED;
  }
  else {
    if (data+size > lf->data+lf->length)  /* illegal section offset */
      error(49,lf->pathname,sec_name,lf->objname);
  }

  if ((f & SHF_EXECINSTR) && data) {
    type = ST_CODE;
    prot |= SP_EXEC;
  }
  if (f & SHF_WRITE)
    prot |= SP_WRITE;
  if (f & SHF_ALLOC)
    flags |= SF_ALLOC;
  if (!strncmp(sec_name,".gnu.linkonce",13))
    flags |= SF_LINKONCE;

  s = add_section(ou,sec_name,data,size,type,flags,prot,
                  (uint8_t)shiftcnt(read32(be,shdr->sh_addralign)),FALSE);

  s->link = read32(be,shdr->sh_link);  /* save link for later use */
  s->id = shndx;  /* use section header index for identification */
}


void elf32_symbols(struct GlobalVars *gv,struct Elf32_Ehdr *ehdr,
                   struct ObjectUnit *ou,struct Elf32_Shdr *shdr)
/* convert ELF symbol definitions into internal format */
{
  bool be = (ehdr->e_ident[EI_DATA] == ELFDATA2MSB);
  uint8_t flags = (read32(be,shdr->sh_type)==SHT_DYNSYM) ? SYMF_SHLIB : 0;
  struct LinkFile *lf = ou->lnkfile;
  uint8_t *data = (uint8_t *)ehdr + read32(be,shdr->sh_offset);
  unsigned long entsize = read32(be,shdr->sh_entsize);
  int nsyms = (int)(read32(be,shdr->sh_size) / (uint32_t)entsize);
  char *strtab = elf32_strtab(lf,ehdr,read32(be,shdr->sh_link));
  struct Section *sec;
  struct Elf32_Sym *elfsym;

  if ((data < lf->data) || 
      (data + read32(be,shdr->sh_size) > lf->data + lf->length))
    error(51,lf->pathname,"symbol",lf->objname);  /* illegal offset */

  /* read ELF xdef symbols and convert to internal format */
  while (--nsyms > 0) {
    int shndx;
    char *symname;
    uint8_t type=0;
    uint8_t objinfo,objbind;

    elfsym = (struct Elf32_Sym *)(data += entsize);
    symname = strtab + read32(be,elfsym->st_name);
    if (symname<(char *)lf->data || symname>(char *)lf->data+lf->length)
      error(127,lf->pathname,read32(be,elfsym->st_name),lf->objname);

    if (flags & SYMF_SHLIB) {
      /* don't import linker symbols from shared objects - we make our own */
      int i;

      for (i=0; i<(sizeof(elf32_symnames)/sizeof(elf32_symnames[0])); i++) {
        if (!strcmp(elf32_symnames[i],symname)) {
          symname = NULL;
          break;
        }
      }
      if (!symname)
        continue;
    }

    switch (shndx = (int)read16(be,elfsym->st_shndx)) {
      case SHN_UNDEF:
        sec = NULL;  /* ignore xrefs for now */
        break;
      /* assign a section for ABS and COMMON symbols */
      case SHN_ABS:
        sec = abs_section(ou);
        type = SYM_ABS;
        break;
      case SHN_COMMON:
        sec = common_section(gv,ou);
        type = SYM_COMMON;
        break;
      /* reloc symbols have a definite section to which they belong */
      default:
        if (read32(be,shdr->sh_type) == SHT_DYNSYM) {
          /* just put dynamic symbols into the first section -
             the section index might be wrong in .dynsym */
          sec = abs_section(ou);
        }
        else if (!(sec = find_sect_id(ou,shndx))) {
          /* a section with this index doesn't exist! */
          if (ELF32_ST_TYPE(*elfsym->st_info) != STT_SECTION)
            error(53,lf->pathname,symname,lf->objname,shndx);
        }
        type = SYM_RELOC;
        break;
    }

    if ((objinfo = ELF32_ST_TYPE(*elfsym->st_info)) == STT_SECTION)
      sec = NULL;  /* ignore section defines - will be reproduced */

    if (sec) {
      if (objinfo > STT_FILE) {
        /* illegal symbol type */
        error(54,lf->pathname,(int)objinfo,symname,lf->objname);
        objinfo = STT_NOTYPE;
      }
      switch (ELF32_ST_BIND(*elfsym->st_info)) {
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
          error(55,lf->pathname,symname,ELF32_ST_BIND(*elfsym->st_info),
                lf->objname);
          objbind = SYMB_LOCAL;
          break;
      }

      /* add a new symbol definition */
      if (objbind == SYMB_LOCAL) {
        addlocsymbol(gv,sec,symname,NULL,(int32_t)read32(be,elfsym->st_value),
                     type,flags,objinfo,read32(be,elfsym->st_size));
      }
      else {
        addsymbol(gv,sec,symname,NULL,(int32_t)read32(be,elfsym->st_value),
                  type,flags,objinfo,objbind,read32(be,elfsym->st_size),TRUE);
      }
    }
  }
}


static void elf32_dynrefs(struct GlobalVars *gv,struct Elf32_Ehdr *ehdr,
                          struct ObjectUnit *ou,struct Elf32_Shdr *shdr,
                          bool be,
                          uint8_t (*reloc_elf2vlink)(uint8_t,struct RelocInsert *))
/* Find all relocs in a shared object which refer to an undefined symbol. */
{
  uint8_t *data = (uint8_t *)ehdr + read32(be,shdr->sh_offset);
  unsigned long entsize = read32(be,shdr->sh_entsize);
  uint32_t symndx = read32(be,shdr->sh_link);
  int nrelocs = (int)(read32(be,shdr->sh_size) / (uint32_t)entsize);
  struct LinkFile *lf = ou->lnkfile;
  struct Section *sec;
  struct Reloc *r;

  if ((data < lf->data)
      || (data + read32(be,shdr->sh_size) > lf->data + lf->length)) {
    error(51,lf->pathname,"reloc",lf->objname);  /* illegal offset */
  }

  /* just put all xrefs into the first section of the shared object */
  sec = abs_section(ou);

  for (; nrelocs; nrelocs--,data+=entsize) {
    struct Elf32_Rela *elfrel = (struct Elf32_Rela *)data;
    struct Elf32_Shdr *symhdr = elf32_shdr(lf,ehdr,symndx);
    struct Elf32_Sym *sym = elf32_symtab(lf,ehdr,symndx) +
                            ELF32_R_SYM(read32(be,elfrel->r_info));
    uint32_t offs = read32(be,elfrel->r_offset);
    uint32_t shndx = (uint32_t)read16(be,sym->st_shndx);
    struct RelocInsert ri,*ri_ptr;
    uint8_t rtype;

    if (shndx == SHN_UNDEF || shndx == SHN_COMMON) {
      memset(&ri,0,sizeof(struct RelocInsert));
      rtype = reloc_elf2vlink(ELF32_R_TYPE(read32(be,elfrel->r_info)),&ri);
      if (rtype == R_NONE)
        continue;
      r = newreloc(gv,sec,elf32_strtab(lf,ehdr,read32(be,symhdr->sh_link))
                   + read32(be,sym->st_name),
                   NULL,0,read32(be,elfrel->r_offset),rtype,0);
      for (ri_ptr=&ri; ri_ptr; ri_ptr=ri_ptr->next)
        addreloc(sec,r,ri_ptr->bpos,ri_ptr->bsiz,ri_ptr->mask);

      /* referenced symbol is weak? */
      if (ELF32_ST_BIND(*sym->st_info)==STB_WEAK)
        r->flags |= RELF_WEAK;
    }
  }
}


static void elf32_reloc(struct GlobalVars *gv,struct Elf32_Ehdr *ehdr,
                        struct ObjectUnit *ou,struct Elf32_Shdr *shdr,
                        char *shstrtab,bool be,
                        uint8_t (*reloc_elf2vlink)(uint8_t,struct RelocInsert *))
/* Read ELF32 relocations, which are relative to a defined symbol, into
   the section's reloc-list. If the symbol is undefined, create an
   external reference on it, with the supplied relocation type. */
{
  uint8_t *data = (uint8_t *)ehdr + read32(be,shdr->sh_offset);
  bool is_rela = read32(be,shdr->sh_type) == SHT_RELA;
  unsigned long entsize = read32(be,shdr->sh_entsize);
  uint32_t symndx = read32(be,shdr->sh_link);
  int nrelocs = (int)(read32(be,shdr->sh_size) / (uint32_t)entsize);
  char *sec_name = shstrtab + read32(be,shdr->sh_name);
  struct LinkFile *lf = ou->lnkfile;
  struct Section *sec;

  if (gv->strip_symbols>=STRIP_DEBUG &&
      (!strncmp(sec_name,".rel.debug",10) ||
       !strncmp(sec_name,".rel.stab",9) ||
       !strncmp(sec_name,".rela.debug",11) ||
       !strncmp(sec_name,".rela.stab",10)))
    return;   /* ignore debugging sections when -S or -s is given */

  if ((data < lf->data) || 
      (data + read32(be,shdr->sh_size) > lf->data + lf->length)) {
    error(51,lf->pathname,"reloc",lf->objname);  /* illegal offset */
  }
  if (!(sec = find_sect_id(ou,read32(be,shdr->sh_info)))) {
    /* a section with this index doesn't exist! */
    error(52,lf->pathname,shstrtab + read32(be,shdr->sh_name),
          getobjname(ou),(int)read32(be,shdr->sh_info));
  }

  for (; nrelocs; nrelocs--,data+=entsize) {
    struct Elf32_Rela *elfrel = (struct Elf32_Rela *)data;
    struct Elf32_Shdr *symhdr = elf32_shdr(lf,ehdr,symndx);
    struct Elf32_Sym *sym = elf32_symtab(lf,ehdr,symndx) +
                            ELF32_R_SYM(read32(be,elfrel->r_info));
    uint32_t offs = read32(be,elfrel->r_offset);
    uint32_t shndx = (uint32_t)read16(be,sym->st_shndx);
    char *xrefname = NULL;
    struct Section *relsec=NULL;
    struct Reloc *r;
    struct RelocInsert ri,*ri_ptr;
    lword a;
    uint8_t rtype;

    memset(&ri,0,sizeof(struct RelocInsert));
    rtype = reloc_elf2vlink(ELF32_R_TYPE(read32(be,elfrel->r_info)),&ri);
    if (rtype == R_NONE)
      continue;

    /* if addend is not defined in Reloc, read it directly from the section */
    if (is_rela)
      a = (int32_t)read32(be,elfrel->r_addend);
    else
      a = (int32_t)readsection(gv,rtype,sec->data+offs,ri.bpos,ri.bsiz,ri.mask);

    if (shndx == SHN_UNDEF || shndx == SHN_COMMON) {
      /* undefined or common symbol - create external reference */
      xrefname = elf32_strtab(lf,ehdr,read32(be,symhdr->sh_link)) +
                              read32(be,sym->st_name);
      relsec = NULL;
    }
    else if (ELF32_ST_TYPE(*sym->st_info) == STT_SECTION) {
      /* a normal relocation, with an offset relative to a section base */
      relsec = find_sect_id(ou,shndx);
    }
    else if (ELF32_ST_TYPE(*sym->st_info)<STT_SECTION && shndx<SHN_ABS) {
      /* relocations, which are relative to a known symbol */
      relsec = find_sect_id(ou,shndx);
      a += (lword)read32(be,sym->st_value);
    }
    else
      ierror("elf32_reloc(): %s (%s): Only relocations which are relative "
             "to a section, function or object are supported "
             "(sym=%s, ST_TYPE=%d)",lf->pathname,lf->objname,
             elf32_strtab(lf,ehdr,read32(be,symhdr->sh_link)) +
                          read32(be,sym->st_name),
             ELF32_ST_TYPE(*sym->st_info));

    r = newreloc(gv,sec,xrefname,relsec,0,(unsigned long)offs,rtype,a);
    for (ri_ptr=&ri; ri_ptr; ri_ptr=ri_ptr->next)
      addreloc(sec,r,ri_ptr->bpos,ri_ptr->bsiz,ri_ptr->mask);

    /* referenced symbol is weak? */
    if (xrefname!=NULL && ELF32_ST_BIND(*sym->st_info)==STB_WEAK)
      r->flags |= RELF_WEAK;

    /* make sure that section data reflects this addend for other formats */
    if (is_rela)
      writesection(gv,sec->data+offs,r,a);
  }
}


static void elf32_stabs(struct GlobalVars *gv,struct LinkFile *lf,
                        struct Elf32_Ehdr *ehdr,struct ObjectUnit *ou)
/* find .stabstr belonging to .stab, convert everything (including
   .rela.stab) into internal format and delete *.stab* afterwards. */
{
  static const char *fn = "elf32_stabs";
  static const char *stabname = "stab";
  bool be = (ehdr->e_ident[EI_DATA] == ELFDATA2MSB);
  struct Section *stabsec;

  if (stabsec = find_sect_name(ou,".stab")) {
    char *strtab = elf32_strtab(lf,ehdr,stabsec->link);
    struct nlist32 *nlst = (struct nlist32 *)stabsec->data;
    long nlstlen = (long)stabsec->size;

    while (nlstlen >= sizeof(struct nlist32)) {
      /* next compilation unit: */
      /* read number of nlist records and size of string table */
      int cnt = (int)read16(be,&nlst->n_desc);
      uint32_t strtabsize = read32(be,&nlst->n_value);
      uint32_t funstart = 0;  /* start address of last function definition */
      struct Section *funsec = NULL;

      nlst++;
      nlstlen -= sizeof(struct nlist32);

      while (cnt--) {
        struct Reloc *r;
        struct Section *relsec;
        char *name;
        uint32_t val;

        if (nlstlen < sizeof(struct nlist32))
          error(118,lf->pathname,stabname,lf->objname);  /* malformatted */

        if (nlst->n_strx)
          name = strtab + read32(be,&nlst->n_strx);
        else
          name = NULL;

        switch (nlst->n_type & N_TYPE) {
          case N_TEXT:
          case N_DATA:
          case N_BSS:
            if (r = findreloc(stabsec,
                              (uint8_t *)&nlst->n_value - stabsec->data)) {
              if (r->rtype!=R_ABS || r->insert->bsiz!=32)
                ierror("%s: Bad .stab relocation",fn);
              relsec = r->relocsect.ptr;
              val = (uint32_t)r->addend;
              break;
            }
          default:
            relsec = NULL;
            val = read32(be,&nlst->n_value);
            break;
        }

        switch (nlst->n_type) {
          case N_FUN:
            if (nlst->n_strx) {
              if (relsec) {  /* function start is always relocatable */
                funsec = relsec;
                funstart = val;
              }
              else
                ierror("%s: N_FUN without relocatable address",fn);
            }
            else {  /* no name marks function end, still relative */
              relsec = funsec;
              val += funstart;
            }
            break;
          case N_SLINE:
            if (relsec == NULL) {
              relsec = funsec;
              val += funstart;
            }
            break;
        }

        addstabs(ou,relsec,name,nlst->n_type,nlst->n_other,
                 (int16_t)read16(be,&nlst->n_desc),val);
        nlst++;
        nlstlen -= sizeof(struct nlist32);
      }

      strtab += strtabsize;
    }
    if (nlstlen)
      error(118,lf->pathname,stabname,lf->objname);  /* ignoring junk */

    /* remove .stab from this object unit - will be recreated later */
    remnode(&stabsec->n);
  }
}


void elf32_parse(struct GlobalVars *gv,struct LinkFile *lf,
                 struct Elf32_Ehdr *ehdr,
                 uint8_t (*reloc_elf2vlink)(uint8_t,struct RelocInsert *))
/* parses a complete ELF file and converts into vlink-internal format */
{
  static const char *fn = "elf32_parse(): ";
  bool be = (ehdr->e_ident[EI_DATA] == ELFDATA2MSB);
  struct ObjectUnit *u;
  struct Elf32_Shdr *shdr;
  uint16_t i,num_shdr,dynstr_idx,dynsym_idx;
  char *shstrtab,*dynstrtab;
  struct Elf32_Dyn *dyn;

  shstrtab = elf32_shstrtab(lf,ehdr);
  u = create_objunit(gv,lf,lf->objname);

  switch (read16(be,ehdr->e_type)) {

    case ET_REL:  /* relocatable object file */
      if (read16(be,ehdr->e_phnum) > 0)
        error(47,lf->pathname,lf->objname);  /* ignoring program hdr. tab */
      num_shdr = read16(be,ehdr->e_shnum);

      /* create vlink sections */
      for (i=1; i<num_shdr; i++) {
        shdr = elf32_shdr(lf,ehdr,i);

        switch (read32(be,shdr->sh_type)) {
          case SHT_PROGBITS:
          case SHT_NOBITS:
          case SHT_NOTE:
            /* create a new section */
            elf32_section(gv,ehdr,u,shdr,i,shstrtab);
          default:
            break;
        }
      }

      /* parse the other section headers */
      for (i=1; i<num_shdr; i++) {
        shdr = elf32_shdr(lf,ehdr,i);

        switch (read32(be,shdr->sh_type)) {
          case SHT_NULL:
          case SHT_STRTAB:
          case SHT_NOTE:
          case SHT_PROGBITS:
          case SHT_NOBITS:
            break;
          case SHT_SYMTAB:
            elf32_symbols(gv,ehdr,u,shdr);  /* symbol definitions */
            break;
          case SHT_REL:
          case SHT_RELA:
            elf32_reloc(gv,ehdr,u,shdr,shstrtab,be,reloc_elf2vlink);
            break;
          default:
            /* section header type not needed in relocatable objects */
            error(48,lf->pathname,read32(be,shdr->sh_type),lf->objname);
            break;
        }
      }

      elf32_stabs(gv,lf,ehdr,u);  /* convert .stab into internal format */
      break;


    case ET_DYN:  /* shared object file */
      dynstrtab = NULL;
      dyn = NULL;
      dynstr_idx = dynsym_idx = 0;
      num_shdr = read16(be,ehdr->e_shnum);

      /* create vlink sections */
      for (i=1; i<num_shdr; i++) {
        shdr = elf32_shdr(lf,ehdr,i);

        switch (read32(be,shdr->sh_type)) {
          case SHT_DYNAMIC:
            /* remember pointer to .dynamic section contents */
            dyn = (struct Elf32_Dyn *)((uint8_t *)ehdr
                                       + read32(be,shdr->sh_offset));
            break;
          case SHT_DYNSYM:
            dynsym_idx = i;
            break;
          case SHT_PROGBITS:
          case SHT_NOBITS:
            /* create a new section */
            elf32_section(gv,ehdr,u,shdr,i,shstrtab);
          default:
            break;
        }
      }

      /* parse the other section headers */
      for (i=1; i<num_shdr; i++) {
        shdr = elf32_shdr(lf,ehdr,i);

        switch (read32(be,shdr->sh_type)) {
          case SHT_NULL:
          case SHT_STRTAB:
          case SHT_NOTE:
          case SHT_PROGBITS:
          case SHT_NOBITS:
          case SHT_HASH:
          case SHT_DYNAMIC:
          case SHT_SYMTAB:
            break;
          case SHT_DYNSYM:
            dynstr_idx = read32(be,shdr->sh_link);
            elf32_symbols(gv,ehdr,u,shdr);  /* symbol definitions */
            break;
          case SHT_REL:
          case SHT_RELA:
            if (fff[gv->dest_format]->flags & FFF_DYN_RESOLVE_ALL) {
              /* The dynamic link editor is limited, so we even have to
                 resolve references from the shared object at link time.
                 But only those which are linked to .dynsym. */
              if (read32(be,shdr->sh_link) == dynsym_idx)
                elf32_dynrefs(gv,ehdr,u,shdr,be,reloc_elf2vlink);
            }
            break;
          default:
            /* section header type not needed in shared objects */
            error(60,lf->pathname,read32(be,shdr->sh_type),lf->objname);
            break;
        }
      }

      if (dynstr_idx!=0 && dyn!=NULL) {
        /* set ObjectUnit's objname to the SONAME of the shared object */
        uint32_t tag;

        shdr = elf32_shdr(lf,ehdr,dynstr_idx);  /* .dynstr */
        while (tag = read32(be,dyn->d_tag)) {
          if (tag == DT_SONAME) {
            u->objname = (char *)ehdr + read32(be,shdr->sh_offset)
                         + read32(be,dyn->d_val);
            break;
          }
          dyn++;
        }
      }
      break;


    case ET_EXEC: /* executable file */
      /* @@@ */
      ierror("%s%s: Executables are currently not supported",fn,lf->pathname);
      break;


    default:
      error(41,lf->pathname,lf->objname);  /* illegal fmt./file corrupted */
      break;
  }

  /* add new object unit to the appropriate list */
  add_objunit(gv,u,FALSE);
}



/*****************************************************************/
/*                          Link ELF                             */
/*****************************************************************/


int elf32_targetlink(struct GlobalVars *gv,struct LinkedSection *ls,
                     struct Section *s)
/* returns 1, if target requires the combination of the two sections, */
/* returns -1, if target doesn't want to combine them, */
/* returns 0, if target doesn't care - standard linking rules are used. */
{
  return 0;
}


static struct Symbol *elf32_makelnksym(struct GlobalVars *gv,int idx)
{
  struct Symbol *sym = addlnksymbol(gv,elf32_symnames[idx],0,SYM_ABS,
                                    SYMF_LNKSYM,SYMI_OBJECT,SYMB_GLOBAL,0);

  sym->extra = idx;  /* for easy ident. in elf32_setlnksym */
  switch (idx) {
    case SDABASE:
    case SDA2BASE:
      sym->type = SYM_RELOC;
      sym->value = (lword)fff[gv->dest_format]->baseoff;
      break;
    case GLOBOFFSTAB:
      sym->value = fff[gv->dest_format]->gotoff;
      gv->got_base_name = elf32_symnames[idx];
      break;
    case PROCLINKTAB:
      gv->plt_base_name = elf32_symnames[idx];
      break;
  }
  return sym;
}


struct Symbol *elf32_lnksym(struct GlobalVars *gv,struct Section *sec,
                            struct Reloc *xref)
/* Check for common ELF32 linker symbols. */
{
  int i;

  if (!gv->dest_object) {
    for (i=0; i<(sizeof(elf32_symnames)/sizeof(elf32_symnames[0])); i++) {
      if (!strcmp(elf32_symnames[i],xref->xrefname))
        return elf32_makelnksym(gv,i);  /* new linker symbol created */
    }
  }
  return NULL;
}


void elf32_setlnksym(struct GlobalVars *gv,struct Symbol *xdef)
/* Initialize common ELF32 linker symbol structure during resolve_xref() */
{
  if (xdef->flags & SYMF_LNKSYM) {
    struct LinkedSection *ls;

    switch (xdef->extra) {
      case SDABASE:
        if (!(ls = find_lnksec(gv,sdata_name,ST_DATA,0,0,0)))
          if (!(ls = find_lnksec(gv,sbss_name,ST_UDATA,0,0,0)))
            ls = smalldata_section(gv);
        xdef->relsect = (struct Section *)ls->sections.first;
        break;
      case SDA2BASE:
        if (!(ls = find_lnksec(gv,sdata2_name,ST_DATA,0,0,0)))
          if (!(ls = find_lnksec(gv,sbss2_name,ST_UDATA,0,0,0)))
            ls = smalldata_section(gv);
        xdef->relsect = (struct Section *)ls->sections.first;
        break;
      case CTORS:
        if (ls = find_lnksec(gv,ctors_name,0,0,0,0)) {
          xdef->type = SYM_RELOC;
          xdef->relsect = (struct Section *)ls->sections.first;
        }
        break;
      case DTORS:
        if (ls = find_lnksec(gv,dtors_name,0,0,0,0)) {
          xdef->type = SYM_RELOC;
          xdef->relsect = (struct Section *)ls->sections.first;
        }
        break;
      case GLOBOFFSTAB:
        if (ls = find_lnksec(gv,got_name,0,0,0,0)) {
          xdef->type = SYM_RELOC;
          xdef->relsect = (struct Section *)ls->sections.first;
        }
        break;
      case PROCLINKTAB:
        if (ls = find_lnksec(gv,plt_name,0,0,0,0)) {
          xdef->type = SYM_RELOC;
          xdef->relsect = (struct Section *)ls->sections.first;
        }
        break;
      case DYNAMICSYM:
        if (ls = find_lnksec(gv,dyn_name,0,0,0,0)) {
          xdef->value = ls->base;
        }
        /* @@@ when .dynamic was not created _DYNAMIC stays NULL - ok? */
        break;
    }
    xdef->flags &= ~SYMF_LNKSYM;  /* do not init again */
  }
}


void elf32_initdynlink(struct GlobalVars *gv)
{
  struct ObjectUnit *ou = gv->dynobj;
  struct Symbol *sym;

  /* set endianess for output file */
  elf32_endianess = fff[gv->dest_format]->endianess;

  /* init dynamic symbol list */
  initlist(&dynsym_list);

  /* allocate .interp section for dynamically linked executables only */
  if (!gv->dest_sharedobj)
    add_section(ou,".interp",(uint8_t *)gv->interp_path,
                strlen(gv->interp_path)+1,ST_DATA,SF_ALLOC,SP_READ,0,TRUE);

  /* .hash, .dynsym, .dynstr and .dynamic are always present.
     Set them to an initial size. They will grow with dynamic symbols added. */
  add_section(ou,hash_name,NULL,0,ST_DATA,SF_ALLOC,SP_READ,2,TRUE);
  add_section(ou,dynsym_name,NULL,0,ST_DATA,SF_ALLOC,SP_READ,2,TRUE);
  add_section(ou,dynstr_name,NULL,0,ST_DATA,SF_ALLOC,SP_READ,0,TRUE);
  dynamic = add_section(ou,dyn_name,NULL,0,ST_DATA,SF_ALLOC,
                        SP_READ|SP_WRITE,2,TRUE);

  /* assign symbol _DYNAMIC the address of the .dynamic section */
  sym = elf32_makelnksym(gv,DYNAMICSYM);
  sym->flags |= SYMF_DYNEXPORT;
  elf32_adddynsym(sym);
}


struct Section *elf32_dyntable(struct GlobalVars *gv,
                               unsigned long initial_size,
                               unsigned long initial_offset,
                               uint8_t sectype,uint8_t secflags,
                               uint8_t secprot,int type)
/* return got/plt section, create new when missing */
{
  static const char fn[] = "elf32_dyntable():";
  static const char *secname[] = { NULL, got_name, plt_name };
  struct Section *sec,**secp;
  struct ObjectUnit *ou;
  int symidx = -1;

  switch (type) {
    case GOT_ENTRY:
      secp = &gotsec;
      symidx = GLOBOFFSTAB;
      break;
    case PLT_ENTRY:
      secp = &pltsec;
      symidx = PROCLINKTAB;
      break;
    default:
      ierror("%s wrong type: %d",fn,type);
      break;
  }
  if (sec = *secp)
    return sec;

  if (gv->dynobj == NULL)
    ierror("%s no dynobj",fn);

  /* Section does not exist - create it.
     The offset field is used for the next table entry offset. */
  sec = add_section(gv->dynobj,(char *)secname[type],NULL,initial_size,
                    sectype,secflags,secprot,2,TRUE);
  sec->offset = initial_offset;
  *secp = sec;

  /* create _GLOBAL_OFFSET_TABLE_ or _PROCEDURE_LINKAGE_TABLE_ linker symbol */
  if (symidx >= 0) {
    if (!findlnksymbol(gv,elf32_symnames[symidx]))
      elf32_makelnksym(gv,symidx);
  }

  return sec;
}


void elf32_adddynsym(struct Symbol *sym)
{
  if (sym->flags & SYMF_DYNIMPORT) {
    elf32_addsym(&elf32dsymlist,&elf32dstrlist,sym->name,0,0,
                 STB_GLOBAL,STT_NOTYPE,SHN_UNDEF);
  }
  else if (sym->flags & SYMF_DYNEXPORT) {
    struct DynSymNode *dsn = alloc(sizeof(struct DynSymNode));

    /* section index and value need to be fixed later for these entries! */
    dsn->idx = elf32_addsym(&elf32dsymlist,&elf32dstrlist,sym->name,0,
                            sym->size,elf32_getbind(sym),elf32_getinfo(sym),0);
    dsn->sym = sym;
    addtail(&dynsym_list,&dsn->n);
  }
  else
    ierror("elf32_adddynsym(): <%s> was not flagged as dynamic",sym->name);
}


static void elf32_dynreloc(struct ObjectUnit *ou,struct Reloc *r,int rela)
{
  const char *secname;
  struct Section **secp;
  uint8_t dynflag = SYMF_DYNIMPORT;

  switch (r->rtype) {
    case R_COPY:
      dynflag = SYMF_DYNEXPORT;
      /* fall through */
    case R_GLOBDAT:
    case R_LOADREL:
      secp = &dynrelocs;
      secname = dynrel_name[rela?1:0];
      r->flags |= RELF_DYN;
      break;
    case R_JMPSLOT:
      secp = &pltrelocs;
      secname = pltrel_name[rela?1:0];
      r->flags |= RELF_PLT;
      break;
    case R_ABS:
      return;
    default:
      ierror("elf32_dynreloc(): wrong rtype %s (%d)",
             reloc_name[r->rtype],(int)r->rtype);
      break;
  }

  /* make sure that dynamic relocation section exists */
  if (*secp == NULL)
    *secp = add_section(ou,secname,NULL,0,ST_DATA,SF_ALLOC,SP_READ,2,TRUE);

  /* increase size for new entry */
  (*secp)->size += rela ? 12 : 8;
  
  /* allocate referenced symbol in .dynsym and .dynstr */
  if (r->xrefname) {
    struct Symbol *sym = r->relocsect.symbol;

    if (!(sym->flags & SYMF_DYNLINK)) {
      /* add symbol to .dynsym symbol list, if not already present */
      sym->flags |= dynflag;
      elf32_adddynsym(sym);
    }
  }
}


struct Symbol *elf32_pltgotentry(struct GlobalVars *gv,struct Section *sec,
                                 DynArg a,uint8_t entrysymtype,
                                 unsigned long offsadd,unsigned long sizeadd,
                                 int etype)
/* Make a table entry for indirectly accessing a location from an external
   symbol defintion (GOT_ENTRY/PLT_ENTRY) or a local relocation (GOT_LOCAL).
   The entry has a size of offsadd bytes, while the table section sec will
   become sizeadd bytes larger per entry. */
{
  const char *fn = "elf32_pltgotentry():";
  char entryname[MAXLEN];
  struct Symbol *tabsym;
  struct Section *refsec;
  unsigned long refoffs;

  /* determine reference section and offset of ext. symbol or local reloc */
  if (etype == GOT_LOCAL) {
    refsec = a.rel->relocsect.ptr;
    refoffs = a.rel->offset;
  }
  else {
    refsec = a.sym->relsect;
    refoffs = (unsigned long)a.sym->value;
  }

  /* generate internal symbol name for this reference */
  snprintf(entryname,MAXLEN," %s@%lx@%lx",
           sec->name,(unsigned long)refsec,refoffs);

  /* create internal symbol, or return old one, when already present */
  tabsym = addsymbol(gv,sec,allocstring(entryname),NULL,(lword)sec->offset,
                     SYM_RELOC,0,entrysymtype,SYMB_LOCAL,offsadd,FALSE);

  /* tabsym is NULL when it was just created, otherwise we already got it */
  if (tabsym == NULL) {
    static uint8_t dyn_reloc_types[] = { R_NONE,R_GLOBDAT,R_JMPSLOT,R_COPY };
    struct Reloc *r;
    uint8_t rtype;

    tabsym = findlocsymbol(gv,sec->obj,entryname);
    if (tabsym == NULL) {
      ierror("%s %s-symbol refering to %s+%lx disappeared",
             fn,sec->name,refsec->name,refoffs);
    }

    /* create a relocation for the new entry */
    if (etype == GOT_LOCAL) {
      /* local symbol: GOT relocation can be resolved now */
      r = newreloc(gv,sec,NULL,refsec,0,sec->offset,R_ABS,(lword)refoffs);
    }
    else {
      /* we need a dynamic linker relocation at the entry's offset */
      r = newreloc(gv,sec,a.sym->name,NULL,0,sec->offset,
                   dyn_reloc_types[etype],0);
      r->relocsect.symbol = a.sym;  /* resolve with external symbol */
      /* Possible enhancement: Find out whether referenced symbol resides
         in an uninitialized section, without a relocation, then we don't
         need an R_COPY relocation either! */
    }
    addreloc(sec,r,0,32,-1);  /* size,mask only important for R_ABS */
    elf32_dynreloc(gv->dynobj,r,gv->reloctab_format==RTAB_ADDEND);

    /* increase offset and size counters of table-section */
    sec->offset += offsadd;
    sec->size += sizeadd;
  }

  return tabsym;
}


struct Symbol *elf32_bssentry(struct GlobalVars *gv,const char *secname,
                              struct Symbol *xdef)
/* Allocate space for a copy-object in a .bss or .sbss section and create
   a R_COPY relocation to the original symbol in the shared object. */
{
  struct Symbol *newxdef;

  if (gv->dynobj == NULL)
    ierror("elf32_bssentry(): no dynobj");
  newxdef = bss_entry(gv->dynobj,secname,xdef);

  if (newxdef) {
    /* entry in BSS was done, so we need a R_COPY relocation */
    struct Reloc *r;

    xdef = newxdef;
    r = newreloc(gv,xdef->relsect,xdef->name,NULL,0,0,R_COPY,0);
    r->relocsect.symbol = xdef;
    addreloc(xdef->relsect,r,0,32,-1);  /* mask/size irrelevant for R_COPY */
    elf32_dynreloc(xdef->relsect->obj,r,gv->reloctab_format==RTAB_ADDEND);
  }

  return xdef;
}


void elf32_dynamicentry(struct GlobalVars *gv,uint32_t tag,uint32_t val,
                        struct Section *relsec)
/* store another entry into the .dynamic section, make new relocation with
   relsec in value-field, when nonzero */
{
  if (dynamic) {
    bool be = elf32_endianess == _BIG_ENDIAN_;
    struct Elf32_Dyn dyn;
    unsigned long offs = dynamic->size;

    write32(be,dyn.d_tag,tag);
    write32(be,dyn.d_val,val);
    dynamic->data = re_alloc(dynamic->data,
                             dynamic->size+sizeof(struct Elf32_Dyn));
    memcpy(dynamic->data+offs,&dyn,sizeof(struct Elf32_Dyn));
    dynamic->size += sizeof(struct Elf32_Dyn);

    if (relsec) {
      /* we need a 32-bit R_ABS relocation for d_val */
      struct Reloc *r;
      int o = offsetof(struct Elf32_Dyn,d_val);

      r = newreloc(gv,dynamic,NULL,relsec,0,
                   offs+offsetof(struct Elf32_Dyn,d_val),R_ABS,(lword)val);
      r->flags |= RELF_INTERNAL;  /* just for calculating the address */
      addreloc(dynamic,r,0,32,-1);
    }
  }
  else
    ierror("elf32_dynamicentry(): .dynamic was never created");
}


static size_t elf32_num_buckets(size_t symcount)
/* determine optimal number of buckets in dynamic symbol hash table */
{
  int i;
  size_t best_num;

  for (i=0; elf_buckets[i]; i++) {
    best_num = elf_buckets[i];
    if (symcount < elf_buckets[i+1])
      break;
  }
  return best_num;
}


static void elf32_makehash(struct GlobalVars *gv)
/* Allocate and populate .hash section. */
{
  bool be = elf32_endianess == _BIG_ENDIAN_;
  size_t nsyms = elf32dsymlist.nextindex;
  size_t nbuckets = elf32_num_buckets(nsyms);
  struct Section *hashsec = find_sect_name(gv->dynobj,hash_name);

  if (hashsec) {
    struct SymbolNode *sn;
    uint32_t *hdata;

    /* .hash layout: nbuckets, nsyms, [buckets], [sym-indexes] */
    hashsec->size = (2 + nbuckets + nsyms) * sizeof(uint32_t);
    hashsec->data = alloczero(hashsec->size);
    hdata = (uint32_t *)hashsec->data;
    write32(be,&hdata[0],nbuckets);
    write32(be,&hdata[1],nsyms);
    for (sn=(struct SymbolNode *)elf32dsymlist.l.first;
         sn->n.next!=NULL; sn=(struct SymbolNode *)sn->n.next) {
      uint32_t *i = &hdata[2 + elf_hash(sn->name) % nbuckets];
      uint32_t j;

      while (j = read32(be,i))
        i = &hdata[2 + nbuckets + j];
      write32(be,i,sn->index);
    }      
  }
  else
    ierror("elf32_makehash(): no %s",hash_name);
}


static void elf32_putstrtab(uint8_t *p,struct StrTabList *sl)
/* write all StrTabList nodes sequentially into memory */
{
  struct StrTabNode *stn;

  while (stn = (struct StrTabNode *)remhead(&sl->l)) {
    const char *s;
    char c;

    s = stn->str;
    do {
      c = *s++;
      *p++ = c;
    } while (c);
  }
}


static void elf32_putsymtab(uint8_t *p,struct SymTabList *sl)
/* write all SymTabList nodes sequentially into memory */
{
  struct SymbolNode *sym;

  while (sym = (struct SymbolNode *)remhead(&sl->l)) {
    memcpy(p,&sym->s,sizeof(struct Elf32_Sym));
    p += sizeof(struct Elf32_Sym);
  }
}


void elf32_dyncreate(struct GlobalVars *gv,const char *pltgot_name)
/* generate .hash, populate .dynstr and .dynamic, allocate .dynsym,
   so that all sections have a valid size for the address calculation */
{
  const char *fn = "elf32_bssentry():";
  struct Section *dynstr,*dynsym,*pltgot;
  struct LibPath *lpn;

  if (gv->dynobj == NULL)
    ierror("%s no dynobj",fn);

  /* write SONAME and RPATHs */
  if (gv->soname && gv->dest_sharedobj)
    elf32_dynamicentry(gv,DT_SONAME,elf32_adddynstr(gv->soname),NULL);
  for (lpn=(struct LibPath *)gv->rpaths.first; lpn->n.next!=NULL;
       lpn=(struct LibPath *)lpn->n.next) {
    elf32_dynamicentry(gv,DT_RPATH,elf32_adddynstr(lpn->path),NULL);
  }

  /* generate .hash section */
  elf32_makehash(gv);

  /* allocate and populate .dynstr section */
  if (dynstr = find_sect_name(gv->dynobj,dynstr_name)) {
    dynstr->size = elf32dstrlist.nextindex;
    dynstr->data = alloc(dynstr->size);
    elf32_putstrtab(dynstr->data,&elf32dstrlist);
  }
  else
    ierror("%s %s missing",fn,dynstr_name);

  /* allocate .dynsym section - populate it later, when addresses are fixed */
  if (dynsym = find_sect_name(gv->dynobj,dynsym_name)) {
    dynsym->size = elf32dsymlist.nextindex * sizeof(struct Elf32_Sym);
    dynsym->data = alloc(dynsym->size);
  }
  else
    ierror("%s %s missing",fn,dynsym_name);

  /* finish .dynamic section */
  elf32_dynamicentry(gv,DT_HASH,0,find_sect_name(gv->dynobj,hash_name));
  elf32_dynamicentry(gv,DT_STRTAB,0,dynstr);
  elf32_dynamicentry(gv,DT_SYMTAB,0,dynsym);
  elf32_dynamicentry(gv,DT_STRSZ,dynstr->size,NULL);
  elf32_dynamicentry(gv,DT_SYMENT,sizeof(struct Elf32_Sym),NULL);
  elf32_dynamicentry(gv,DT_DEBUG,0,NULL); /* needed? */
  /* do we have a .plt or .got section (target dependant) */
  if (pltgot = find_sect_name(gv->dynobj,pltgot_name)) {
    elf32_dynamicentry(gv,DT_PLTGOT,0,pltgot);    
  }
  /* do we have .plt relocations? */
  if (pltrelocs) {
    elf32_dynamicentry(gv,DT_PLTRELSZ,pltrelocs->size,NULL);
    elf32_dynamicentry(gv,DT_PLTREL,
                       gv->reloctab_format==RTAB_ADDEND?DT_RELA:DT_REL,
                       NULL);
    elf32_dynamicentry(gv,DT_JMPREL,0,pltrelocs);
  }
  /* do we have any other dynamic relocations? */
  if (dynrelocs) {
    if (gv->reloctab_format == RTAB_ADDEND) {
      elf32_dynamicentry(gv,DT_RELA,0,dynrelocs);
      elf32_dynamicentry(gv,DT_RELASZ,dynrelocs->size,NULL);
      elf32_dynamicentry(gv,DT_RELAENT,sizeof(struct Elf32_Rela),NULL);
    }
    else {
      elf32_dynamicentry(gv,DT_REL,0,dynrelocs);
      elf32_dynamicentry(gv,DT_RELSZ,dynrelocs->size,NULL);
      elf32_dynamicentry(gv,DT_RELENT,sizeof(struct Elf32_Rel),NULL);
    }
  }
  /* end tag */
  elf32_dynamicentry(gv,DT_NULL,0,NULL);
}



/*****************************************************************/
/*                          Write ELF                            */
/*****************************************************************/


unsigned long elf32_headersize(struct GlobalVars *gv)
{
  int segcnt = 0;
  struct Phdr *p = gv->phdrlist;

  while (p) {
    if (p->flags & PHDR_USED)
      segcnt++;
    p = p->next;
  }
  return sizeof(struct Elf32_Ehdr) + segcnt * sizeof(struct Elf32_Phdr);
}


static struct ShdrNode *elf32_newshdr(void)
{
  struct ShdrNode *s = alloczero(sizeof(struct ShdrNode));

  addtail(&shdrlist,&s->n);
  ++elf32shdridx;
  return s;
}


static struct ShdrNode *elf32_addshdr(uint32_t name,uint32_t type,
                                      uint32_t flags,uint32_t addr,
                                      uint32_t offset,uint32_t size,
                                      uint32_t link,uint32_t info,
                                      uint32_t align,uint32_t entsize,bool be)
{
  struct ShdrNode *shn = elf32_newshdr();

  write32(be,shn->s.sh_name,name);
  write32(be,shn->s.sh_type,type);
  write32(be,shn->s.sh_flags,flags);
  write32(be,shn->s.sh_addr,addr);
  write32(be,shn->s.sh_offset,offset);
  write32(be,shn->s.sh_size,size);
  write32(be,shn->s.sh_link,link);
  write32(be,shn->s.sh_info,info);
  write32(be,shn->s.sh_addralign,align);
  write32(be,shn->s.sh_entsize,entsize);
  return shn;
}


static struct SymbolNode *elf32_findSymNode(struct SymTabList *sl,
                                            const char *name)
/* Find an ELF symbol node by its name.
   Return pointer to it, or NULL when not found. */
{
  struct SymbolNode **chain = &sl->hashtab[elf_hash(name) % sl->htabsize];
  struct SymbolNode *sym;

  while (sym = *chain) {
    if (!strcmp(name,sym->name))
      return sym;
    chain = &sym->hashchain;
  }
  return NULL;
}


static uint32_t elf32_findsym(struct SymTabList *sl,const char *name,
                              uint16_t shndx,bool be)
/* find an ELF symbol by its name and shndx */
/* return its symbol table index, index=0 means 'not found' */
{
  struct SymbolNode **chain = &sl->hashtab[elf_hash(name) % sl->htabsize];
  struct SymbolNode *sym;

  while (sym = *chain) {
    if (!strcmp(name,sym->name))
      if (read16(be,sym->s.st_shndx) == shndx)
        return sym->index;
    chain = &sym->hashchain;
  }
  return 0;
}


static uint32_t elf32_symidx(struct SymTabList *sl,struct StrTabList *strl,
                             const char *name,bool be)
/* return index of external symbol with name from SymTabList */
{
  uint32_t symidx;

  if (!(symidx = elf32_findsym(sl,name,SHN_COMMON,be)))
    if (!(symidx = elf32_findsym(sl,name,SHN_UNDEF,be)))
      symidx = elf32_addsym(sl,strl,name,0,0,
                            STB_GLOBAL,STT_NOTYPE,SHN_UNDEF);

  return symidx;
}


static int elf32_addrela(struct GlobalVars *gv,struct LinkedSection *ls,
                         struct Reloc *rel,bool be,
                         uint8_t (*reloc_vlink2elf)(struct Reloc *))
{
  struct RelaNode *rn = alloc(sizeof(struct RelaNode));
  uint32_t symidx;
  uint8_t rtype;

  if (rel->flags & RELF_INTERNAL)
    return 0;  /* internal relocations will never be exported */

  if (rel->xrefname) {
    symidx = elf32_symidx(&elf32symlist,&elf32stringlist,rel->xrefname,be);
  }
  else {
    if (rel->relocsect.lnk == NULL) {
      if (!(rel->flags & RELF_DYNLINK))
        return 0;  /* ignore, because it was resolved by a shared object */
      else
        ierror("elf32_addrela: Reloc type %d (%s) at %s+0x%lx (addend 0x%llx)"
               " is missing a relocsect.lnk",(int)rel->rtype,
               reloc_name[rel->rtype],ls->name,rel->offset,rel->addend);
    }
    symidx = (uint32_t)(rel->relocsect.lnk->index + secsyms);
  }

  if (!(rtype = reloc_vlink2elf(rel))) {
    struct RelocInsert *ri;

    if (ri = rel->insert)
      error(32,fff[gv->dest_format]->tname,reloc_name[rel->rtype],
            (int)ri->bpos,(int)ri->bsiz,ri->mask,ls->name,rel->offset);
    else
      ierror("elf32_addrela(): Reloc without insert-field");
  }

  write32(be,rn->r.r_offset,(uint32_t)(ls->base + rel->offset));
  write32(be,rn->r.r_addend,(uint32_t)rel->addend);
  write32(be,rn->r.r_info,ELF32_R_INFO(symidx,(uint32_t)rtype));
  addtail(&relalist,&rn->n);

  if (gv->reloctab_format != RTAB_ADDEND) {
    writesection(gv,ls->data+rel->offset,rel,rel->addend);
    return sizeof(struct Elf32_Rel);
  }
  writesection(gv,ls->data+rel->offset,rel,0);
  return sizeof(struct Elf32_Rela);
}


static uint32_t elf32_addstrlist(struct StrTabList *sl,const char *s)
/* add a new string to an ELF string table and return its index */
{
  struct StrTabNode **chain,*sn;

  if (sl->hashtab == NULL) {
    /* initialize an unused string list */
    initlist(&sl->l);
    sl->hashtab = alloczero(sl->htabsize * sizeof(struct StrTabNode *));
    elf32_addstrlist(sl,noname);
  }
  chain = &sl->hashtab[elf_hash(s) % sl->htabsize];

  /* search string in hash table */
  while (sn = *chain) {
    if (!strcmp(s,sn->str))
      return sn->index;  /* it's already in, return index */
    chain = &sn->hashchain;
  }

  /* new string table entry */
  *chain = sn = alloc(sizeof(struct StrTabNode));
  sn->hashchain = NULL;
  sn->str = s;
  sn->index = sl->nextindex;
  addtail(&sl->l,&sn->n);
  sl->nextindex += (uint32_t)strlen(s) + 1;

  return sn->index;
}


static uint32_t elf32_addshdrstr(const char *s)
{
  return elf32_addstrlist(&elf32shstrlist,s);
}


static uint32_t elf32_addstr(const char *s)
{
  return elf32_addstrlist(&elf32stringlist,s);
}


uint32_t elf32_adddynstr(const char *s)
{
  return elf32_addstrlist(&elf32dstrlist,s);
}


uint8_t elf32_getinfo(struct Symbol *sym)
{
  uint8_t type = STT_NOTYPE;

  switch (sym->info) {
    case SYMI_NOTYPE:
      break;  /* @@@ Is this allowed? */
    case SYMI_OBJECT:
      type = STT_OBJECT;
      break;
    case SYMI_FUNC:
      type = STT_FUNC;
      break;
    case SYMI_SECTION:
      ierror("elf32_getinfo(): STT_SECTION symbol detected");
      type = STT_SECTION;
      break;
    case SYMI_FILE:
      type = STT_FILE;
      break;
    default:
      ierror("elf32_getinfo(): Illegal symbol info: %d",(int)sym->info);
      break;
  }
  return type;
}


uint8_t elf32_getbind(struct Symbol *sym)
{
  uint8_t bind = STB_GLOBAL;

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
      ierror("elf32_getbind(): Illegal symbol binding: %d",(int)sym->bind);
      break;
  }
  return bind;
}


uint16_t elf32_getshndx(struct GlobalVars *gv,
                        struct Symbol *sym,uint8_t symtype)
{
  uint16_t shndx = SHN_UNDEF;

  switch (sym->type) {
    case SYM_INDIR:
      /* can't reproduce unsupported symbol type */
      error(33,fff[gv->dest_format]->tname,sym->name,sym_bind[sym->bind],
            sym_type[sym->type],sym_info[sym->info]);
    case SYM_UNDEF:
      shndx = SHN_UNDEF;
      break;
    case SYM_ABS:
      shndx = SHN_ABS;
      break;
    case SYM_RELOC:
      if (symtype > STT_FUNC)
        ierror("elf32_getshndx(): %s is relocatable, but not a "
               "function or object (type %d)",sym->name,(int)symtype);
      shndx = (uint16_t)sym->relsect->lnksec->index;
      break;
    case SYM_COMMON:
      shndx = SHN_COMMON;
      break;
    default:
      ierror("elf32_getshndx(): Illegal symbol type: %d",(int)sym->type);
      break;
  }
  return shndx;
}


void elf32_header(FILE *f,uint16_t type,uint16_t mach,uint32_t entry,
                  uint32_t phoff,uint32_t shoff,uint32_t flags,uint16_t phnum,
                  uint16_t shnum,uint16_t shstrndx,bool be)
/* write 32-bit ELF header */
{
  struct Elf32_Ehdr eh;

  memset(&eh,0,sizeof(struct Elf32_Ehdr));
  strncpy((char *)eh.e_ident,ELFid,4);
  eh.e_ident[EI_CLASS] = ELFCLASS32;
  eh.e_ident[EI_DATA] = be ? ELFDATA2MSB : ELFDATA2LSB;
  eh.e_ident[EI_VERSION] = ELF_VER;
  write16(be,eh.e_type,type);
  write16(be,eh.e_machine,mach);
  write32(be,eh.e_version,ELF_VER);
  write32(be,eh.e_entry,entry);
  write32(be,eh.e_phoff,phoff);
  write32(be,eh.e_shoff,shoff);
  write32(be,eh.e_flags,flags);
  write16(be,eh.e_ehsize,sizeof(struct Elf32_Ehdr));
  write16(be,eh.e_phentsize,phnum ? sizeof(struct Elf32_Phdr):0);
  write16(be,eh.e_phnum,phnum);
  write16(be,eh.e_shentsize,shnum ? sizeof(struct Elf32_Shdr):0);
  write16(be,eh.e_shnum,shnum);
  write16(be,eh.e_shstrndx,shstrndx);
  fwritex(f,&eh,sizeof(struct Elf32_Ehdr));
}


void elf32_writephdrs(struct GlobalVars *gv,FILE *f)
/* write 32-bit ELF Program Header (PHDR) */
{
  bool be = elf32_endianess == _BIG_ENDIAN_;
  long gapsize = file_hdr_gap;
  struct Elf32_Phdr phdr;
  struct Phdr *p;

  for (p=gv->phdrlist; p; p=p->next) {
    if (p->flags & PHDR_USED) {
      if (p->start!=ADDR_NONE && p->start_vma!=ADDR_NONE) {
        write32(be,phdr.p_type,p->type);
        write32(be,phdr.p_offset,p->offset);
        write32(be,phdr.p_vaddr,p->start_vma);
        write32(be,phdr.p_paddr,p->start);
        write32(be,phdr.p_filesz,p->file_end - p->start);
        write32(be,phdr.p_memsz,p->mem_end - p->start);
        write32(be,phdr.p_flags,p->flags & PHDR_PFMASK);
        write32(be,phdr.p_align,1L<<p->alignment);
        fwritex(f,&phdr,sizeof(struct Elf32_Phdr));
      }
      else
        gapsize += sizeof(struct Elf32_Phdr);
    }
  }
  fwritegap(f,gapsize);  /* gap at the end, for unused PHDRs */
}


void elf32_writeshdrs(struct GlobalVars *gv,FILE *f,uint32_t reloffset,
                      uint32_t stabndx)
/* write all section headers */
{
  const char *fn = "elf32_writeshdrs():";
  bool be = elf32_endianess == _BIG_ENDIAN_;
  struct LinkedSection *ls;
  struct ShdrNode *shn;
  uint32_t type;

  while (shn = (struct ShdrNode *)remhead(&shdrlist)) {
    type = read32(be,shn->s.sh_type);

    /* handle REL and RELA sections */
    if (type == SHT_RELA || type == SHT_REL) {
      if (read32(be,shn->s.sh_flags) & SHF_ALLOC) {
        /* allocated, so probably dynamic relocs: link to .dynsym */
        if (ls = find_lnksec(gv,dynsym_name,0,0,0,0))
          write32(be,shn->s.sh_link,(uint32_t)ls->index);
        else
          ierror("%s %s",fn,dynsym_name);

        if (read32(be,shn->s.sh_info) != 0) {
          /* link to .plt requested in info field */
          if (ls = find_lnksec(gv,plt_name,0,0,0,0))
            write32(be,shn->s.sh_info,(uint32_t)ls->index);
          else
            ierror("%s %s",fn,plt_name);
        }
      }
      else {
        /* patch correct sh_offset and sh_link for reloc header */
        write32(be,shn->s.sh_offset,read32(be,shn->s.sh_offset)+reloffset);
        write32(be,shn->s.sh_link,stabndx);
      }
    }

    /* handle HASH sections, which need a .dynsym link */
    else if (type == SHT_HASH) {
      if (ls = find_lnksec(gv,dynsym_name,0,0,0,0))
        write32(be,shn->s.sh_link,(uint32_t)ls->index);
      else
        ierror("%s %s",fn,dynsym_name);
    }

    /* handle DYNAMIC and DYNSYM sections */
    else if (type==SHT_DYNAMIC || type==SHT_DYNSYM) {
      /* write .dynstr link */
      if (ls = find_lnksec(gv,dynstr_name,0,0,0,0))
        write32(be,shn->s.sh_link,(uint32_t)ls->index);
      else
        ierror("%s %s",fn,dynstr_name);

      if (type == SHT_DYNSYM) {
        write32(be,shn->s.sh_info,1);  /* @@ FIXME! number of local symbols */
      }
    }

    fwritex(f,&shn->s,sizeof(struct Elf32_Shdr));
  }
}


void elf32_stdsymtab(struct GlobalVars *gv,uint8_t bind,uint8_t type)
{
  elf32_addsymlist(gv,&elf32symlist,bind,type);
}


void elf32_addsymlist(struct GlobalVars *gv,struct SymTabList *sl,
                      uint8_t bind,uint8_t type)
/* add all symbols with specified bind and type to the ELF symbol list */
{
  struct LinkedSection *ls = (struct LinkedSection *)gv->lnksec.first;
  struct LinkedSection *nextls;
  struct Symbol *nextsym,*sym;

  while (nextls = (struct LinkedSection *)ls->n.next) {
    sym = (struct Symbol *)ls->symbols.first;

    while (nextsym = (struct Symbol *)sym->n.next) {
      uint8_t symtype = elf32_getinfo(sym);
      uint8_t symbind = elf32_getbind(sym);

      if (symbind == bind && (!type || (symtype == type))) {
        if (!discard_symbol(gv,sym)) {
          remnode(&sym->n);
          elf32_addsym(sl,&elf32stringlist,sym->name,(uint32_t)sym->value,
                       sym->size,symbind,symtype,
                       elf32_getshndx(gv,sym,symtype));
        }
      }
      sym = nextsym;
    }
    ls = nextls;
  }
}


static uint16_t conv_perm_to_elf(uint8_t secperm)
/* converts vlink section permissions to ELF segment permissions */
{
  uint16_t elfperm = 0;

  if (secperm & SP_READ)
    elfperm |= PF_R;
  if (secperm & SP_WRITE)
    elfperm |= PF_W;
  if (secperm & SP_EXEC)
    elfperm |= PF_X;

  return elfperm;
}


uint32_t elf32_segmentcheck(struct GlobalVars *gv)
/* 1. checks the PT_LOAD segments for intermediate uninitialized sections,
      which will be turned into initialized PROGBITS data sections
   2. sets segment permissions from contained sections
   3. calculates bytes to insert at beginning of segment to meet
      page-alignment restrictions
   4. initializes PT_PHDR segment
   Returns number of segments in list */
{
  unsigned long headersize = fff[gv->dest_format]->headersize(gv);
  struct Phdr *p,*phdrs,*first=NULL;
  struct LinkedSection *ls,*bss_start,*seg_lastdat,*seg_lastsec,*prg_lastdat;
  unsigned long foffs;
  uint32_t segcnt;

  /* find PHDR segment */
  for (phdrs=NULL,p=gv->phdrlist; p; p=p->next) {
    if ((p->flags&PHDR_USED) && p->type==PT_PHDR) {
      phdrs = p;
      break;
    }
  }

  for (p=gv->phdrlist,prg_lastdat=NULL; p; p=p->next) {
    if (p->type==PT_LOAD && (p->flags&PHDR_USED) &&
        p->start!=ADDR_NONE && p->start_vma!=ADDR_NONE) {
      unsigned long amask = (1L << p->alignment) - 1;
      long gapsize;

      bss_start = seg_lastsec = NULL;
      p->flags &= ~PHDR_PFMASK;
      p->flags |= PF_R;  /* at least 'read' should be allowed */

      /* determine initial file offset for first segment */
      if (!first) {
        first = p;
        if (phdrs) {  /* header is included in first segment */
          p->flags |= PHDR_PHDRS | PHDR_FILEHDR;
          foffs = 0;
          file_hdr_gap = (unsigned long)p->start
                          - ((unsigned long)(p->start - headersize) & ~amask)
                          - headersize;
          p->start -= headersize + file_hdr_gap;
          if (p->start_vma)
            p->start_vma -= headersize + file_hdr_gap;
        }
        else
          foffs = headersize;  /* first segment starts after header */
      }

      for (ls=(struct LinkedSection *)gv->lnksec.first,
            seg_lastdat=NULL,seg_lastsec=NULL;
           ls->n.next!=NULL;
           ls=(struct LinkedSection *)ls->n.next) {

        if (ls->copybase>=(unsigned long)p->start && ls->size &&
            (ls->copybase+ls->size)<=(unsigned long)p->mem_end &&
            (ls->flags & SF_ALLOC)) {

          p->flags |= conv_perm_to_elf(ls->protection);
          if (ls->alignment > p->alignment)
            p->alignment = ls->alignment;

          if (ls->flags & SF_UNINITIALIZED) {
            if (!bss_start)
              bss_start = ls;
          }
          else {
            if (bss_start) {
              struct LinkedSection *bssls;

              for (bssls=bss_start; bssls!=ls;
                   bssls=(struct LinkedSection *)bssls->n.next) {
                bssls->flags &= ~SF_UNINITIALIZED;
                if (bssls->type == ST_UDATA)
                  bssls->type = ST_DATA;
                bssls->filesize = bssls->size;
              }
              /* Warning: Intermediate uninitialized sections in ELF
                 segment will be turned into initialized */
              error(82,p->name,bss_start->name,
                    ((struct LinkedSection *)ls->n.pred)->name);
              bss_start = NULL;
            }
          }

          if (seg_lastsec && !bss_start) {
            if (ls->copybase >= seg_lastsec->copybase+seg_lastsec->filesize) {
              seg_lastsec->gapsize = ls->copybase - (seg_lastsec->copybase +
                                                     seg_lastsec->filesize);
            }
            else if (ls->copybase > 0) {
              ierror("elf32_segmentcheck(): overlapping sections "
                     "%s(%lx-%lx) followed by %s(%lx)",
                     seg_lastsec->name,seg_lastsec->copybase,
                     seg_lastsec->copybase + seg_lastsec->filesize,
                     ls->copybase);
            }
          }
          seg_lastsec = ls;
          if (!bss_start || !seg_lastdat)
            prg_lastdat = seg_lastdat = ls;
        }
      }

      /* calculate alignment gap for segment */
      /* @@@ do we align on LMA or VMA? */
      gapsize = (p->start & amask) - (foffs & amask);
      p->alignment_gap = gapsize<0 ? gapsize+amask+1 : gapsize;

      if (seg_lastdat)
        p->file_end = seg_lastdat->copybase + seg_lastdat->filesize;

      foffs += p->alignment_gap;
      p->offset = foffs;
      foffs += p->file_end - p->start;
    }
  }

  if (phdrs!=NULL && first!=NULL) {
    /* init PHDR segment using the first LOAD seg. which directly follows */
    phdrs->flags &= ~PHDR_PFMASK;
    phdrs->flags |= PF_R | PF_X;
    phdrs->start = first->start + sizeof(struct Elf32_Ehdr);
    phdrs->start_vma = phdrs->start;
    phdrs->mem_end = phdrs->start + headersize - sizeof(struct Elf32_Ehdr);
    phdrs->file_end = phdrs->mem_end;
    phdrs->offset = sizeof(struct Elf32_Ehdr);
  }

  for (p=gv->phdrlist,segcnt=0; p; p=p->next) {
    if ((p->flags&PHDR_USED)
        && p->start!=ADDR_NONE && p->start_vma!=ADDR_NONE) {
      segcnt++;

      if (p->type!=PT_LOAD && p->type!=PT_PHDR && p->type!=PT_NULL) {
        /* set segment permissions for remaining segment types */
        p->flags &= ~PHDR_PFMASK;
        for (ls=seg_lastdat=(struct LinkedSection *)gv->lnksec.first;
             ls->n.next!=NULL; ls=(struct LinkedSection *)ls->n.next) {
          if (ls->copybase>=(unsigned long)p->start && ls->size &&
              (ls->copybase+ls->size)<=(unsigned long)p->mem_end &&
              (ls->flags & SF_ALLOC)) {
            p->flags |= conv_perm_to_elf(ls->protection);
            seg_lastdat = ls;
          }
        }
        p->file_end = seg_lastdat->copybase + seg_lastdat->filesize;
      }
    }
  }

  return segcnt;
}


static void add_section_shdr(struct LinkedSection *ls,bool bss,uint32_t f)
{
  struct ShdrNode *shn;
  uint32_t type = bss ? SHT_NOBITS : SHT_PROGBITS;
  uint32_t info = 0;
  uint32_t entsize = 0;

  if (!strncmp(ls->name,note_name,strlen(note_name)))
    type = SHT_NOTE;
  else if (!strncmp(ls->name,".rela",5)) {
    type = SHT_RELA;
    entsize = sizeof(struct Elf32_Rela);
  }
  else if (!strncmp(ls->name,".rel",4)) {
    type = SHT_REL;
    entsize = sizeof(struct Elf32_Rel);
  }
  else if (!strcmp(ls->name,hash_name)) {
    type = SHT_HASH;
    entsize = sizeof(uint32_t);
  }
  else if (!strcmp(ls->name,dynsym_name)) {
    type = SHT_DYNSYM;
    entsize = sizeof(struct Elf32_Sym);
  }
  else if (!strcmp(ls->name,dyn_name)) {
    type = SHT_DYNAMIC;
    entsize = sizeof(struct Elf32_Dyn);
  }
  else if (!strcmp(ls->name,dynstr_name))
    type = SHT_STRTAB;
  else if (!strncmp(ls->name,got_name,strlen(got_name)))
    entsize = sizeof(uint32_t);

  if (!strcmp(ls->name,pltrel_name[0]) || !strcmp(ls->name,pltrel_name[1]))
    info = ~0;  /* request .plt index in info field for .rel(a).plt */

  shn = elf32_addshdr(elf32_addshdrstr(ls->name),type,f,ls->base,
                      elf32offset,ls->size,0,info,1<<(uint32_t)ls->alignment,
                      entsize,elf32_endianess);


  if (stabdebugidx && !strcmp(ls->name,".stab"))
    stabshdr = shn;  /* patch sh_link field for .stabstr later */
}


void elf32_makeshdrs(struct GlobalVars *gv)
/* generate all ELF section headers */
{
  struct Phdr *p,*p2;
  struct LinkedSection *ls;
  unsigned long poffs;

  /* touch elf32symlist, to make sure it is initialized */
  elf32_addsym(&elf32symlist,&elf32stringlist,NULL,0,0,0,0,0);

  /* offset, to find section-symbols by section header index */
  secsyms = (int)elf32symlist.nextindex - (int)elf32shdridx;

  /* reset index */
  for (ls=(struct LinkedSection *)gv->lnksec.first;
       ls->n.next!=NULL; ls=(struct LinkedSection *)ls->n.next)
    ls->index = 0;

  for (p=gv->phdrlist; p; p=p->next) {
    if (p->type==PT_LOAD && (p->flags&PHDR_USED) &&
        p->start!=ADDR_NONE && p->start_vma!=ADDR_NONE) {
      /* check file offset for current segment */
      elf32offset += p->alignment_gap;
      poffs = p->offset;
      if (p->flags & (PHDR_PHDRS|PHDR_FILEHDR)) {
        poffs += fff[gv->dest_format]->headersize(gv) + file_hdr_gap;
        elf32offset += file_hdr_gap;
      }
      if (poffs != elf32offset) {
        ierror("elf32_makeshdrs(): PHDR \"%s\" offs %lu != %lu\n",
               p->name,poffs,elf32offset);
      }

      /* find sections which belong to this segment and set their index */
      for (ls=(struct LinkedSection *)gv->lnksec.first;
           ls->n.next!=NULL; ls=(struct LinkedSection *)ls->n.next) {
        if (ls->copybase>=(unsigned long)p->start &&
            (ls->copybase+ls->size)<=(unsigned long)p->mem_end &&
            (ls->flags & SF_ALLOC) && ls->index==0) {
          uint32_t f = SHF_ALLOC;
          bool bss = ls->type==ST_UDATA || (ls->flags&SF_UNINITIALIZED);

          ls->index = (int)elf32shdridx;
          f |= (ls->protection & SP_WRITE) ? SHF_WRITE : 0;
          f |= (ls->protection & SP_EXEC) ? SHF_EXECINSTR : 0;
          add_section_shdr(ls,bss,f);

          /* check if section included in other non-LOAD segments as well */
          for (p2=gv->phdrlist; p2; p2=p2->next) {
            if (p2->type!=PT_LOAD && (p2->flags&PHDR_USED) &&
                p2->start==(lword)ls->copybase)
              p2->offset = elf32offset;
          }

          /* update file offset */
          if (gv->dest_object) {
            if (!bss)
              elf32offset += ls->size;
          }
          else {
            elf32offset += ls->filesize + ls->gapsize;
          }

          /* add section symbol (without name) */
          elf32_addsym(&elf32symlist,&elf32stringlist,noname,
                       (uint32_t)ls->base,0,STB_LOCAL,STT_SECTION,
                       (uint16_t)ls->index);
        }
      }
    }
  }

  /* unallocated sections at last */
  for (ls=(struct LinkedSection *)gv->lnksec.first;
       ls->n.next!=NULL; ls=(struct LinkedSection *)ls->n.next) {
    if (ls->index == 0) {
      uint32_t f = 0;
      bool bss = ls->type==ST_UDATA || (ls->flags&SF_UNINITIALIZED);

      ls->index = (int)elf32shdridx;
      f |= (ls->protection & SP_WRITE) ? SHF_WRITE : 0;
      f |= (ls->protection & SP_EXEC) ? SHF_EXECINSTR : 0;
      add_section_shdr(ls,bss,f);
      /* update file offset */
      if (!bss)
        elf32offset += ls->size;
      /* add section symbol (without name) */
      elf32_addsym(&elf32symlist,&elf32stringlist,noname,(uint32_t)ls->base,0,
                   STB_LOCAL,STT_SECTION,(uint16_t)ls->index);
    }
  }
}


void elf32_addrelocs(struct GlobalVars *gv,
                     uint8_t (*reloc_vlink2elf)(struct Reloc *))
/* creates relocations for all sections */
{
  bool be = elf32_endianess == _BIG_ENDIAN_;
  struct LinkedSection *ls;
  struct Reloc *rel;
  uint32_t sroffs=0,roffs=0;

  for (ls=(struct LinkedSection *)gv->lnksec.first;
       ls->n.next!=NULL; ls=(struct LinkedSection *)ls->n.next) {
    sroffs = roffs;

    /* relocations */
    for (rel=(struct Reloc *)ls->relocs.first;
         rel->n.next!=NULL; rel=(struct Reloc *)rel->n.next) {
      roffs += elf32_addrela(gv,ls,rel,be,reloc_vlink2elf);
    }

    /* external references */
    for (rel=(struct Reloc *)ls->xrefs.first;
         rel->n.next!=NULL; rel=(struct Reloc *)rel->n.next) {
      roffs += elf32_addrela(gv,ls,rel,be,reloc_vlink2elf);
    }

    if (roffs != sroffs) {
      /* create ".rel(a).name" header */
      char *relname = (char *)alloc(strlen(ls->name)+6);

      sprintf(relname,".%s%s",
              gv->reloctab_format==RTAB_ADDEND ? "rela" : "rel",
              ls->name);
      elf32_addshdr(elf32_addshdrstr(relname),
                    gv->reloctab_format==RTAB_ADDEND ? SHT_RELA : SHT_REL,
                    0,0,sroffs,roffs-sroffs,0,(uint32_t)ls->index,4,
                    gv->reloctab_format==RTAB_ADDEND ?
                      sizeof(struct Elf32_Rela) : sizeof(struct Elf32_Rel),
                    be);
      /* sroffs is relative and will be corrected later */
      /* sh_link will be initialized, when .symtab exists */
    }
  }
}


void elf32_makeshstrtab(void)
/* creates .shstrtab */
{
  bool be = elf32_endianess == _BIG_ENDIAN_;

  elf32_addshdr(shstrtabidx,SHT_STRTAB,0,0,elf32offset,
                elf32shstrlist.nextindex,0,0,1,0,be);
  elf32offset += elf32shstrlist.nextindex;
  elf32offset += align(elf32offset,2);
}


void elf32_makestrtab(void)
/* creates .strtab */
{
  bool be = elf32_endianess == _BIG_ENDIAN_;

  elf32_addshdr(strtabidx,SHT_STRTAB,0,0,elf32offset,
                elf32stringlist.nextindex,0,0,1,0,be);
  elf32offset += elf32stringlist.nextindex;
  elf32offset += align(elf32offset,2);
}


void elf32_makestabstr(void)
/* create .stabstr */
{
  if (stabdebugidx) {
    bool be = elf32_endianess == _BIG_ENDIAN_;
    uint32_t size = 0;
    struct StabCompUnit *cu;

    for (cu=(struct StabCompUnit *)elf32stabcompunits.first;
         cu->n.next!=NULL; cu=(struct StabCompUnit *)cu->n.next)
      size += cu->strtab.nextindex;

    if (stabshdr) {
      /* patch sh_link field of .stab */
      write32(be,stabshdr->s.sh_link,elf32shdridx);
    }
    elf32_addshdr(stabdebugidx,SHT_STRTAB,0,0,elf32offset,size,0,0,1,0,be);
    elf32offset += size;
  }
}


void elf32_makesymtab(uint32_t strtabindex)
/* creates .symtab */
{
  bool be = elf32_endianess == _BIG_ENDIAN_;

  elf32_addshdr(symtabidx,SHT_SYMTAB,0,0,elf32offset,
                elf32symlist.nextindex * sizeof(struct Elf32_Sym),
                strtabindex,elf32symlist.global_index,4,
                sizeof(struct Elf32_Sym),be);
  elf32offset += elf32symlist.nextindex * sizeof(struct Elf32_Sym);
}


uint32_t elf32_addsym(struct SymTabList *sl,struct StrTabList *strl,
                      const char *name,uint32_t value,uint32_t size,
                      uint8_t bind,uint8_t type,uint16_t shndx)
/* add a new ELF symbol, return its symbol table index */
{
  bool be = elf32_endianess == _BIG_ENDIAN_;
  struct SymbolNode **chain,*sym;

  if (sl->hashtab == NULL) {
    /* initialize an unused symbol list */
    initlist(&sl->l);
    sl->hashtab = alloczero(sl->htabsize * sizeof(struct SymbolNode *));
    elf32_addsym(sl,strl,noname,0,0,0,0,SHN_UNDEF);
  }

  if (name == NULL)  /* do nothing, return first index */
    return 0;

  chain = &sl->hashtab[elf_hash(name) % sl->htabsize];

  while (sym = *chain)
    chain = &sym->hashchain;
  /* new symbol table entry */
  *chain = sym = alloczero(sizeof(struct SymbolNode));
  sym->name = name;
  sym->index = sl->nextindex++;
  addtail(&sl->l,&sym->n);
  write32(be,sym->s.st_name,elf32_addstrlist(strl,name));
  write32(be,sym->s.st_value,value);
  write32(be,sym->s.st_size,size);
  *sym->s.st_info = ELF32_ST_INFO(bind,type);
  write16(be,sym->s.st_shndx,shndx);
  return sym->index;
}


static size_t elf32_putdynreloc(struct GlobalVars *gv,struct LinkedSection *ls,
                                struct Reloc *rel,void *dst,
                                uint8_t (*reloc_vlink2elf)(struct Reloc *),
                                bool rela,bool be)
{
  const char *fn = "elf32_putdynreloc()";
  struct Elf32_Rela *rp = (struct Elf32_Rela *)dst;
  uint32_t symidx;
  uint8_t rtype;

  if (rel->xrefname) {
    struct SymbolNode *sn;

    sn = elf32_findSymNode(&elf32dsymlist,rel->xrefname);
    if (sn == NULL)
      ierror("%s no symbol <%s> in dyn.table",fn,rel->xrefname);
    symidx = sn->index;
  }
  else
    ierror("%s no symbol base",fn);

  if (!(rtype = reloc_vlink2elf(rel))) {
    struct RelocInsert *ri;

    if (ri = rel->insert)
      error(32,fff[gv->dest_format]->tname,reloc_name[rel->rtype],
            (int)ri->bpos,(int)ri->bsiz,ri->mask,ls->name,rel->offset);
    else
      ierror("%s Reloc without insert-field",fn);
  }

  write32(be,rp->r_offset,(uint32_t)(ls->base + rel->offset));
  write32(be,rp->r_info,ELF32_R_INFO(symidx,(uint32_t)rtype));

  if (rela) {
    write32(be,rp->r_addend,(uint32_t)rel->addend);
    writesection(gv,ls->data+rel->offset,rel,0);
    return sizeof(struct Elf32_Rela);
  }
  writesection(gv,ls->data+rel->offset,rel,rel->addend);
  return sizeof(struct Elf32_Rel);
}


static void elf32_makedynamic(struct GlobalVars *gv,
                              uint8_t (*reloc_vlink2elf)(struct Reloc *))
{
  if (gv->dynamic) {
    const char *fn = "elf32_makedynamic():";
    bool be = elf32_endianess == _BIG_ENDIAN_;
    int rela = gv->reloctab_format==RTAB_ADDEND ? 1 : 0;
    struct LinkedSection *ls;
    uint8_t *dynp,*pltp;

    if (ls = find_lnksec(gv,dynrel_name[rela],0,0,0,0))
      dynp = ls->data;
    else
      dynp = NULL;
    if (ls = find_lnksec(gv,pltrel_name[rela],0,0,0,0))
      pltp = ls->data;
    else
      pltp = NULL;

    /* write dynamic relocations */
    for (ls=(struct LinkedSection *)gv->lnksec.first;
         ls->n.next!=NULL; ls=(struct LinkedSection *)ls->n.next) {
      struct Reloc *rel = (struct Reloc *)ls->xrefs.first;
      struct Reloc *nextrel;

      while (nextrel = (struct Reloc *)rel->n.next) {
        if (rel->flags & RELF_DYN) {
          if (dynp == NULL)
            ierror("%s %s lost",fn,dynrel_name[rela]);
          dynp += elf32_putdynreloc(gv,ls,rel,dynp,reloc_vlink2elf,rela,be);
          remnode(&rel->n);
          /* free rel */
        }
        else if (rel->flags & RELF_PLT) {
          if (pltp == NULL)
            ierror("%s %s lost",fn,pltrel_name[rela]);
          pltp += elf32_putdynreloc(gv,ls,rel,pltp,reloc_vlink2elf,rela,be);
          remnode(&rel->n);
          /* free rel */
        }
        rel = nextrel;
      }
    }

    /* write dynamic symbols to .dynsym and fix their values */
    if (ls = find_lnksec(gv,dynsym_name,0,0,0,0)) {
      struct Elf32_Sym *tab = (struct Elf32_Sym *)ls->data;
      struct DynSymNode *dsn;

      elf32_putsymtab(ls->data,&elf32dsymlist);
      while (dsn = (struct DynSymNode *)remhead(&dynsym_list)) {
        uint8_t symtype = elf32_getinfo(dsn->sym);

        write32(be,tab[dsn->idx].st_value,dsn->sym->value);
        write16(be,tab[dsn->idx].st_shndx,elf32_getshndx(gv,dsn->sym,symtype));
        /* free dsn? */
      }
    }
    else
      ierror("%s %s lost",fn,dynsym_name);
  }
}


static struct StabCompUnit *newCompUnit(void)
{
  struct StabCompUnit *scu = alloczero(sizeof(struct StabCompUnit));

  initlist(&scu->stabs);
  scu->strtab.htabsize = STABHTABSIZE;
  return scu;
}


static void write_nlist(bool be,struct nlist32 *n,long strx,
                        uint8_t type,int8_t othr,int16_t desc,uint32_t val)
{
  write32(be,&n->n_strx,strx);
  n->n_type = type;
  n->n_other = othr;
  write16(be,&n->n_desc,desc);
  write32(be,&n->n_value,val);
}


void elf32_makestabs(struct GlobalVars *gv)
/* create .stab, .stabstr und .rela.stab sections from StabDebug records */
{
  static const char *fn = "elf32_makestabs";
  bool be = elf32_endianess == _BIG_ENDIAN_;

  stabdebugidx = 0;
  initlist(&elf32stabcompunits);

  if (gv->strip_symbols < STRIP_DEBUG) {
    struct StabCompUnit *cu = NULL;
    bool so_found = FALSE;
    struct LinkedSection *ls;
    struct ObjectUnit *obj;
    struct StabDebug *stab;
    unsigned long stabsize = 0;

    /* determine size of .stab section to generate */
    for (obj=(struct ObjectUnit *)gv->selobjects.first,stabsize=0;
         obj->n.next!=NULL; obj=(struct ObjectUnit *)obj->n.next) {

      while (stab=(struct StabDebug *)remhead(&obj->stabs)) {

        if (stab->n_type==N_SO && stab->name.ptr!=NULL) {
          if (*stab->name.ptr != '\0') {
            if (!so_found) {
              /* allocate and initialize new compilation unit */
              stabsize += sizeof(struct nlist32);  /* comp.unit header */
              cu = newCompUnit();
              addtail(&elf32stabcompunits,&cu->n);
              so_found = TRUE;
            }
          }
        }
        else
          so_found = FALSE;

        if (cu == NULL)
          ierror("%s: N_SO missing",fn);
        if (cu->entries >= 0x7fff)
          ierror("%s: too many stab-entries for compilation unit",fn);
        stabsize += sizeof(struct nlist32);  /* stab entry */
        cu->entries++;

        /* register symbol name in comp.unit's string table */
        if (stab->name.ptr)
          stab->name.idx = elf32_addstrlist(&cu->strtab,stab->name.ptr);
        else
          stab->name.idx = 0;

        if (so_found)
          cu->nameidx = stab->name.idx;  /* set comp.unit name */

        /* append stab entry to current comp.unit */
        addtail(&cu->stabs,&stab->n);
      }
    }

    if (stabsize) {
      /* create .stab section with contents for real */
      struct nlist32 *nlst = alloc(stabsize);

      if (ls = find_lnksec(gv,".stab",0,0,0,0)) {
        if (ls->size > 0)
          ierror("%s: .stab already in use",fn);
      }
      else
        ls = create_lnksect(gv,".stab",ST_DATA,0,SP_READ,2);
      ls->size = ls->filesize = stabsize;
      ls->data = (uint8_t *)nlst;

      /* allocate SHDR name for .stabstr */
      stabdebugidx = elf32_addshdrstr(".stabstr");

      /* transfer nodes from compilation units to section's data area */
      for (cu=(struct StabCompUnit *)elf32stabcompunits.first;
           cu->n.next!=NULL; cu=(struct StabCompUnit *)cu->n.next) {
        bool within_fun = FALSE;
        uint32_t fun_addr = 0;

        /* write compilation unit stab header containing number
           of stabs and length of string-table */
        write_nlist(be,nlst++,cu->nameidx,0,0,cu->entries,
                    cu->strtab.nextindex);

        /* write comp.unit's stabs */
        for (stab=(struct StabDebug *)cu->stabs.first;
             stab->n.next!=NULL; stab=(struct StabDebug *)stab->n.next) {

          switch (stab->n_type) {
            case N_SO:
              within_fun = FALSE;
              break;
            case N_FUN:
              if (stab->name.idx) {
                within_fun = TRUE;
                fun_addr = stab->n_value;
              }
              else {  /* function end address needs to be relative (size) */
                within_fun = FALSE;
                stab->n_value -= fun_addr;
                stab->relsect = NULL;
              }
              break;
            case N_SLINE:
              if (within_fun && stab->relsect) {
                /* convert address into a function-offset */
                stab->n_value -= fun_addr;
                stab->relsect = NULL;
              }
              break;
          }

          /* add relocation, if required */
          if (stab->relsect) {
            struct Reloc *r = alloczero(sizeof(struct Reloc));

            stab->n_value += stab->relsect->offset;
            r->relocsect.lnk = stab->relsect->lnksec;
            r->offset = (uint8_t *)&nlst->n_value - ls->data;
            r->addend = (lword)stab->n_value;
            r->rtype = R_ABS;
            addreloc(NULL,r,0,32,-1);
            addtail(&ls->relocs,&r->n);
          }

          /* write stab entry to section */
          write_nlist(be,nlst++,stab->name.idx,stab->n_type,
                      stab->n_othr,stab->n_desc,stab->n_value);
        }
      }
    }
  }
}


void elf32_initoutput(struct GlobalVars *gv,
                      uint32_t init_file_offset,int8_t output_endianess)
/* initialize section header, program header, relocation, symbol, */
/* string and section header string lists */
{
  elf32_endianess = output_endianess;
  elf32offset = init_file_offset;
  file_hdr_gap = 0;

  if (gv->phdrlist == NULL) {
    /* we need to provide at least one dummy PHDR, even for reloc-objects */
    struct Phdr *p = alloczero(sizeof(struct Phdr));

    p->type = PT_LOAD;
    p->flags = PF_X|PF_W|PF_R|PHDR_USED;
    p->mem_end = p->file_end = 0xffffffff;
    p->offset = fff[gv->dest_format]->headersize(gv);
    gv->phdrlist = p;
  }

  initlist(&shdrlist);
  initlist(&phdrlist);
  initlist(&relalist);
  elf32shdridx = 0;

  symtabidx = elf32_addshdrstr(".symtab");
  strtabidx = elf32_addshdrstr(".strtab");
  shstrtabidx = elf32_addshdrstr(".shstrtab");
  elf32_newshdr();          /* first Shdr is always zero */
}


void elf32_writesegments(struct GlobalVars *gv,FILE *f)
/* write all PT_LOAD segments, with alignment gaps, etc. */
{
  struct Phdr *p;
  struct LinkedSection *ls;

  for (p=gv->phdrlist; p; p=p->next) {
    if (p->type==PT_LOAD && (p->flags&PHDR_USED) &&
        p->start!=ADDR_NONE && p->start_vma!=ADDR_NONE) {
      /* write page-alignment gap */
      fwritegap(f,p->alignment_gap);

      /* write section contents */
      for (ls=(struct LinkedSection *)gv->lnksec.first;
           ls->n.next!=NULL; ls=(struct LinkedSection *)ls->n.next) {
        if (ls->copybase>=(unsigned long)p->start &&
            (ls->copybase+ls->size)<=(unsigned long)p->mem_end &&
            (ls->flags & SF_ALLOC)) {
          if (ls->filesize)
            fwritex(f,ls->data,ls->filesize);  /* section's contents */
          if (ls->gapsize)
            fwritegap(f,ls->gapsize);  /* inter-section alignment gap */
        }
      }
    }
  }

  /* unallocated sections at last */
  for (ls=(struct LinkedSection *)gv->lnksec.first;
       ls->n.next!=NULL; ls=(struct LinkedSection *)ls->n.next) {
    if (!(ls->flags & SF_ALLOC)) {
      if (!(ls->flags & SF_UNINITIALIZED))
        fwritex(f,ls->data,ls->size);
    }
  }
}


void elf32_writesections(struct GlobalVars *gv,FILE *f)
/* write all linked sections */
{
  struct LinkedSection *ls;

  /* write all allocated sections */
  for (ls=(struct LinkedSection *)gv->lnksec.first;
       ls->n.next!=NULL; ls=(struct LinkedSection *)ls->n.next) {
    if (ls->flags & SF_ALLOC) {
      if (!(ls->flags & SF_UNINITIALIZED))
        fwritex(f,ls->data,ls->size);
    }
  }
  /* unallocated sections at last */
  for (ls=(struct LinkedSection *)gv->lnksec.first;
       ls->n.next!=NULL; ls=(struct LinkedSection *)ls->n.next) {
    if (!(ls->flags & SF_ALLOC)) {
      if (!(ls->flags & SF_UNINITIALIZED))
        fwritex(f,ls->data,ls->size);
    }
  }
}


void elf32_writestrtab(FILE *f,struct StrTabList *sl)
{
  struct StrTabNode *stn;

  while (stn = (struct StrTabNode *)remhead(&sl->l))
    fwritex(f,stn->str,strlen(stn->str)+1);
}


void elf32_writestabstr(FILE *f)
{
  if (stabdebugidx) {
    struct StabCompUnit *cu;

    for (cu=(struct StabCompUnit *)elf32stabcompunits.first;
         cu->n.next!=NULL; cu=(struct StabCompUnit *)cu->n.next)
      elf32_writestrtab(f,&cu->strtab);
  }
}


void elf32_writesymtab(FILE *f,struct SymTabList *sl)
{
  struct SymbolNode *sym;

  while (sym = (struct SymbolNode *)remhead(&sl->l))
    fwritex(f,&sym->s,sizeof(struct Elf32_Sym));
}


void elf32_writerelocs(struct GlobalVars *gv,FILE *f)
{
  struct RelaNode *rn;

  while (rn = (struct RelaNode *)remhead(&relalist))
    fwritex(f,&rn->r,
            gv->reloctab_format==RTAB_ADDEND ?
              sizeof(struct Elf32_Rela) : sizeof(struct Elf32_Rel));
}


void elf32_writeobject(struct GlobalVars *gv,FILE *f,uint16_t m,int8_t endian,
                       uint8_t (*reloc_vlink2elf)(struct Reloc *))
/* creates an ELF32 relocatable object file */
{
  uint32_t sh_off,shstrndx,stabndx;

  elf32_initoutput(gv,sizeof(struct Elf32_Ehdr),endian);
  elf32_makestabs(gv);
  elf32_stdsymtab(gv,STB_LOCAL,STT_FILE);

  elf32_makeshdrs(gv);
  elf32_stdsymtab(gv,STB_LOCAL,0);
  elf32symlist.global_index = elf32symlist.nextindex;
  elf32_stdsymtab(gv,STB_WEAK,0);
  elf32_stdsymtab(gv,STB_GLOBAL,0);
  elf32_addrelocs(gv,reloc_vlink2elf);

  elf32_makestabstr();
  shstrndx = elf32shdridx;
  elf32_makeshstrtab();
  sh_off = elf32offset;
  stabndx = elf32shdridx;
  elf32offset += (elf32shdridx+2) * sizeof(struct Elf32_Shdr);
  elf32_makesymtab(elf32shdridx+1);
  elf32_makestrtab();

  elf32_header(f,ET_REL,m,0,0,sh_off,0,0,elf32shdridx,
               shstrndx,endian==_BIG_ENDIAN_);
  elf32_writesections(gv,f);
  elf32_writestabstr(f);
  elf32_writestrtab(f,&elf32shstrlist);
  fwrite_align(f,2,ftell(f));
  elf32_writeshdrs(gv,f,elf32offset,stabndx);
  elf32_writesymtab(f,&elf32symlist);
  elf32_writestrtab(f,&elf32stringlist);
  fwrite_align(f,2,ftell(f));
  elf32_writerelocs(gv,f);
}


void elf32_writeexec(struct GlobalVars *gv,FILE *f,uint16_t m,int8_t endian,
                     uint8_t (*reloc_vlink2elf)(struct Reloc *))
/* creates an ELF32 executable file (page-aligned with absolute addresses) */
{
  uint32_t sh_off,shstrndx,stabndx,phnum;
  struct LinkedSection *ls;

  elf32_initoutput(gv,elf32_headersize(gv),endian);
  elf32_makestabs(gv);
  phnum = elf32_segmentcheck(gv);
  elf32_stdsymtab(gv,STB_LOCAL,STT_FILE);

  elf32_makeshdrs(gv);
  elf32_makedynamic(gv,reloc_vlink2elf);

  elf32_stdsymtab(gv,STB_LOCAL,0);
  elf32symlist.global_index = elf32symlist.nextindex;
  elf32_stdsymtab(gv,STB_WEAK,0);
  elf32_stdsymtab(gv,STB_GLOBAL,0);

  if (gv->keep_relocs)
    elf32_addrelocs(gv,reloc_vlink2elf);
  for (ls=(struct LinkedSection *)gv->lnksec.first;
       ls->n.next!=NULL; ls=(struct LinkedSection *)ls->n.next) {
    calc_relocs(gv,ls);
  }

  elf32_makestabstr();
  shstrndx = elf32shdridx;
  elf32_makeshstrtab();
  sh_off = elf32offset;
  stabndx = elf32shdridx;
  elf32offset += (elf32shdridx+2) * sizeof(struct Elf32_Shdr);
  elf32_makesymtab(elf32shdridx+1);
  elf32_makestrtab();

  elf32_header(f,gv->dest_sharedobj?ET_DYN:ET_EXEC,m,
               (uint32_t)entry_address(gv),sizeof(struct Elf32_Ehdr),sh_off,0,
               phnum,elf32shdridx,shstrndx,endian==_BIG_ENDIAN_);
  elf32_writephdrs(gv,f);
  elf32_writesegments(gv,f);
  elf32_writestabstr(f);
  elf32_writestrtab(f,&elf32shstrlist);
  fwrite_align(f,2,ftell(f));
  elf32_writeshdrs(gv,f,elf32offset,stabndx);
  elf32_writesymtab(f,&elf32symlist);
  elf32_writestrtab(f,&elf32stringlist);
  if (gv->keep_relocs) {
    fwrite_align(f,2,ftell(f));
    elf32_writerelocs(gv,f);
  }
}

#endif /* ELF32 */
