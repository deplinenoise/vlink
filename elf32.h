/* $VER: vlink elf32.h V0.13 (02.11.10)
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


#include "elfcommon.h"


struct Elf32_Ehdr {
  unsigned char	e_ident[EI_NIDENT]; /* ELF "magic number" */
  unsigned char	e_type[2];          /* Identifies object file type */
  unsigned char	e_machine[2];       /* Specifies required architecture */
  unsigned char	e_version[4];       /* Identifies object file version */
  unsigned char	e_entry[4];         /* Entry point virtual address */
  unsigned char	e_phoff[4];         /* Program header table file offset */
  unsigned char	e_shoff[4];         /* Section header table file offset */
  unsigned char	e_flags[4];         /* Processor-specific flags */
  unsigned char	e_ehsize[2];        /* ELF header size in bytes */
  unsigned char	e_phentsize[2];     /* Program header table entry size */
  unsigned char	e_phnum[2];         /* Program header table entry count */
  unsigned char	e_shentsize[2];     /* Section header table entry size */
  unsigned char	e_shnum[2];         /* Section header table entry count */
  unsigned char	e_shstrndx[2];      /* Section header string table index */
};

struct Elf32_Phdr {
  unsigned char p_type[4];          /* Identifies program segment type */
  unsigned char p_offset[4];        /* Segment file offset */
  unsigned char p_vaddr[4];         /* Segment virtual address */
  unsigned char p_paddr[4];         /* Segment physical address */
  unsigned char p_filesz[4];        /* Segment size in file */
  unsigned char p_memsz[4];         /* Segment size in memory */
  unsigned char p_flags[4];         /* Segment flags */
  unsigned char p_align[4];         /* Segment alignment, file & memory */
};

struct Elf32_Shdr {
  unsigned char sh_name[4];         /* Section name, index in string tbl */
  unsigned char sh_type[4];         /* Type of section */
  unsigned char sh_flags[4];        /* Miscellaneous section attributes */
  unsigned char sh_addr[4];         /* Section virtual addr at execution */
  unsigned char sh_offset[4];       /* Section file offset */
  unsigned char sh_size[4];         /* Size of section in bytes */
  unsigned char sh_link[4];         /* Index of another section */
  unsigned char sh_info[4];         /* Additional section information */
  unsigned char sh_addralign[4];    /* Section alignment */
  unsigned char sh_entsize[4];      /* Entry size if section holds table */
};

struct Elf32_Sym {
  unsigned char st_name[4];         /* Symbol name, index in string tbl */
  unsigned char st_value[4];        /* Value of the symbol */
  unsigned char st_size[4];         /* Associated symbol size */
  unsigned char st_info[1];         /* Type and binding attributes */
  unsigned char st_other[1];        /* No defined meaning, 0 */
  unsigned char st_shndx[2];        /* Associated section index */
};
/* st_info */
#define ELF32_ST_BIND(i) ((i)>>4)
#define ELF32_ST_TYPE(i) ((i)&0xf)
#define ELF32_ST_INFO(b,t) (((b)<<4)+((t)&0xf))

struct Elf32_Note {
  unsigned char namesz[4];          /* Size of entry's owner string */
  unsigned char descsz[4];          /* Size of the note descriptor */
  unsigned char type[4];            /* Interpretation of the descriptor */
  char          name[1];            /* Start of the name+desc data */
};

struct Elf32_Rel {
  unsigned char r_offset[4];    /* Location at which to apply the action */
  unsigned char r_info[4];      /* index and type of relocation */
};

struct Elf32_Rela {
  unsigned char r_offset[4];    /* Location at which to apply the action */
  unsigned char r_info[4];      /* index and type of relocation */
  unsigned char r_addend[4];    /* Constant addend used to compute value */
};

/* r_info */
#define ELF32_R_SYM(i) ((i)>>8)
#define ELF32_R_TYPE(i) ((unsigned char)(i))
#define ELF32_R_INFO(s,t) (((s)<<8)+(unsigned char)(t))

struct Elf32_Dyn {
  unsigned char d_tag[4];           /* entry tag value */
  unsigned char d_val[4];
};


/* vlink specific - used to generate ELF32 output files */

#define STRHTABSIZE 0x10000
#define SHSTRHTABSIZE 0x100
#define DYNSTRHTABSIZE 0x1000
#define DYNSYMHTABSIZE 0x1000
#define STABHTABSIZE 0x1000

struct StrTabNode {
  struct node n;
  struct StrTabNode *hashchain;
  const char *str;
  uint32_t index;
};

struct StrTabList {
  struct list l;
  struct StrTabNode **hashtab;
  unsigned long htabsize;
  uint32_t nextindex;
};

struct SymbolNode {
  struct node n;
  struct SymbolNode *hashchain;
  const char *name;
  struct Elf32_Sym s;
  uint32_t index;
};

struct SymTabList {
  struct list l;
  struct SymbolNode **hashtab;
  unsigned long htabsize;
  uint32_t nextindex;
  uint32_t global_index;
};

struct DynSymNode {             /* links vlink symbol with .dynsym-symbol */
  struct node n;
  struct Symbol *sym;           /* pointer to vlink symbol */
  uint32_t idx;                 /* index of ELF symbol in .dynsym */
};

struct ShdrNode {
  struct node n;
  struct Elf32_Shdr s;
};

struct RelaNode {
  struct node n;
  struct Elf32_Rela r;
};


/* .stab compilation units */
struct StabCompUnit {
  struct node n;
  long nameidx;
  unsigned long entries;
  struct list stabs;
  struct StrTabList strtab;
};


/* for conversion from ELF reloc types to vlink internal format */
struct ELF2vlink {
  uint8_t rtype;
  uint16_t bpos;
  uint16_t bsiz;
  lword mask;
};


/* global data from t_elf32.c */
#ifndef T_ELF32_C
extern int8_t elf32_endianess;
extern struct SymTabList elf32symlist;
extern struct SymTabList elf32dsymlist;
extern struct StrTabList elf32shstrlist;
extern struct StrTabList elf32stringlist;
extern struct StrTabList elf32dstrlist;
extern uint32_t elf32shdridx;
extern uint32_t elf32offset;
extern const char *elf32_symnames[];
extern const char note_name[];
extern const char dyn_name[];
extern const char hash_name[];
extern const char dynsym_name[];
extern const char dynstr_name[];
extern const char *dynrel_name[2];
extern const char *pltrel_name[2];
#endif

/* Linker symbol IDs */
#define SDABASE         0   /* _SDA_BASE_ */
#define SDA2BASE        1   /* _SDA2_BASE_ */
#define CTORS           2   /* __CTOR_LIST__ */
#define DTORS           3   /* __DTOR_LIST__ */
#define GLOBOFFSTAB     4   /* _GLOBAL_OFFSET_TABLE_ */
#define PROCLINKTAB     5   /* _PROCEDURE_LINKAGE_TABLE_ */
#define DYNAMICSYM      6   /* _DYNAMIC */


/* Prototypes from t_elf32.c */

/* function for reading */
int elf32_identify(struct FFFuncs *,char *,struct Elf32_Ehdr *,
                   unsigned long,unsigned char,unsigned char,uint16_t,uint32_t);
void elf32_check_ar_type(struct FFFuncs *,const char *,struct Elf32_Ehdr *,
                         unsigned char,unsigned char,uint32_t,int,...);
char *elf32_shstrtab(struct LinkFile *,struct Elf32_Ehdr *);
char *elf32_strtab(struct LinkFile *,struct Elf32_Ehdr *,int);
struct Elf32_Sym *elf32_symtab(struct LinkFile *,struct Elf32_Ehdr *,int);
struct Elf32_Shdr *elf32_shdr(struct LinkFile *lf,struct Elf32_Ehdr *,uint16_t);
void elf32_section(struct GlobalVars *,struct Elf32_Ehdr *,
                   struct ObjectUnit *,struct Elf32_Shdr *,int,char *);
void elf32_symbols(struct GlobalVars *,struct Elf32_Ehdr *,
                   struct ObjectUnit *,struct Elf32_Shdr *);
void elf32_parse(struct GlobalVars *,struct LinkFile *,struct Elf32_Ehdr *,
                 uint8_t (*)(uint8_t,struct RelocInsert *));

/* functions for linking */
uint8_t elf32_cmpsecflags(uint8_t,uint8_t);
int elf32_targetlink(struct GlobalVars *,struct LinkedSection *,
                     struct Section *);
struct Symbol *elf32_lnksym(struct GlobalVars *,struct Section *,
                            struct Reloc *);
void elf32_setlnksym(struct GlobalVars *,struct Symbol *);
void elf32_initdynlink(struct GlobalVars *);
struct Section *elf32_dyntable(struct GlobalVars *,unsigned long,unsigned long,
                               uint8_t,uint8_t,uint8_t,int);
void elf32_adddynsym(struct Symbol *);
struct Symbol *elf32_pltgotentry(struct GlobalVars *,struct Section *,DynArg,
                                 uint8_t,unsigned long,unsigned long,int);
struct Symbol *elf32_bssentry(struct GlobalVars *,const char *,struct Symbol *);
void elf32_dynamicentry(struct GlobalVars *,uint32_t tag,uint32_t,
                        struct Section *);
void elf32_dyncreate(struct GlobalVars *,const char *);

/* functions for writing */
unsigned long elf32_headersize(struct GlobalVars *);
uint32_t elf32_adddynstr(const char *);
uint8_t elf32_getinfo(struct Symbol *);
uint8_t elf32_getbind(struct Symbol *);
uint16_t elf32_getshndx(struct GlobalVars *,struct Symbol *,uint8_t);
void elf32_header(FILE *,uint16_t,uint16_t,uint32_t,uint32_t,uint32_t,
                  uint32_t,uint16_t,uint16_t,uint16_t,bool);
void elf32_writephdrs(struct GlobalVars *,FILE *);
void elf32_writeshdrs(struct GlobalVars *,FILE *,uint32_t,uint32_t);
void elf32_stdsymtab(struct GlobalVars *,uint8_t,uint8_t);
void elf32_addsymlist(struct GlobalVars *,struct SymTabList *,uint8_t,uint8_t);
uint32_t elf32_segmentcheck(struct GlobalVars *);
void elf32_makeshdrs(struct GlobalVars *);
void elf32_addrelocs(struct GlobalVars *,uint8_t (*)(struct Reloc *));
void elf32_makeshstrtab(void);
void elf32_makestrtab(void);
void elf32_makestabstr(void);
void elf32_makesymtab(uint32_t);

uint32_t elf32_addsym(struct SymTabList *,struct StrTabList *,const char *,
                     uint32_t,uint32_t,uint8_t,uint8_t,uint16_t);
void elf32_makestabs(struct GlobalVars *);

void elf32_initoutput(struct GlobalVars *,uint32_t,int8_t);
void elf32_writesegments(struct GlobalVars *,FILE *);
void elf32_writesections(struct GlobalVars *,FILE *);
void elf32_writestrtab(FILE *,struct StrTabList *);
void elf32_writestabstr(FILE *);
void elf32_writesymtab(FILE *,struct SymTabList *);
void elf32_writerelocs(struct GlobalVars *,FILE *);

void elf32_writeobject(struct GlobalVars *,FILE *,uint16_t,int8_t,
                       uint8_t (*)(struct Reloc *));
void elf32_writeexec(struct GlobalVars *,FILE *,uint16_t,int8_t,
                     uint8_t (*)(struct Reloc *));
