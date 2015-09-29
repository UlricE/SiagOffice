#ifndef MW_UTILS_H
#define MW_UTILS_H

#ifndef MW_MAX
#define MW_MAX(a,b) ((a)>(b)?(a):(b))
#endif

#ifdef DEBUG
#define MW_TRACE(s) do {\
        FILE *f=fopen("TRACEME","a");\
        fprintf(f,"[%s,%d]: " ,__FILE__,__LINE__);\
        fprintf s;\
        fprintf(f,"\n");\
        fclose(f);\
        } while(0);
#else
#define MW_TRACE(s)
#endif

#undef _
#define _(p) MwTranslate(p)

enum MwStatusStates { MW_ABORT = 0, MW_DONE, MW_WAITING };

extern void MwMallocStats(void);
extern void MwMallocInit(void (*)(void), int);
extern void MwMallocExit(void);
extern void *MwMalloc(size_t);
extern void *MwCalloc(size_t, size_t);
extern void MwFree(void *);
extern void *MwRealloc(void *, size_t);
extern char *MwStrdup(const char *);
extern void MwChomp(char *);
extern void MwLoadDictionary(char *);
extern char *MwTranslate(char *);
extern void MwQuotecpy(char *, char *, char *);
extern pid_t MwSpawn(const char *);
extern void MwHelp(char *);
extern int MwFromCchar(char *);
extern void MwToCchar(char *, int);
extern int MwStrcmp(const char *, const char *);
extern int MwStrcasecmp(const char *, const char *);
extern int MwStrncasecmp(const char *, const char *, size_t);
extern int MwSnprintf(char *, size_t, const char *, ...);


#endif	/* MW_UTILS_H */
