#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
Check these declarations against the C/Fortran source code.
*/

/* .Fortran calls */
extern void F77_NAME(bt)(double *T, double *B, int *icode);
extern void F77_NAME(cndtd)(double *T, double *D, double *cdty, int *icode);
extern void F77_NAME(cpft)(double *T, double *cpf, int *icode);
extern void F77_NAME(cpgt)(double *T, double *cpg, int *icode);
extern void F77_NAME(cptd)(double *T, double *D, void *Cp, int *icode);
extern void F77_NAME(cptp)(double *T, double *p, void *Cp, int *icode);
extern void F77_NAME(ct)(double *T, double *C, int *icode);
extern void F77_NAME(cvft)(double *T, double *Cvf, int *icode);
extern void F77_NAME(cvgt)(double *T, double *Cvg, int *icode);
extern void F77_NAME(cvtd)(double *T, double *D, double *Cv, int *icode);
extern void F77_NAME(cvtp)(double *T, double *p, double *Cv, int *icode);
extern void F77_NAME(dddttd)(double *T, double *D, double *dDdT, int *icode);
extern void F77_NAME(dddttp)(double *T, double *p, double *dDdT, int *icode);
extern void F77_NAME(dfp)(double *p, double *Df, int *icode);
extern void F77_NAME(dfs)(double *s, double *Df, int *icode);
extern void F77_NAME(dft)(double *T, double *Df, int *icode);
extern void F77_NAME(dgp)(double *p, double *Dg, int *icode);
extern void F77_NAME(dgs)(double *s, double *Dg, int *icode);
extern void F77_NAME(dgt)(double *T, double *Dg, int *icode);
extern void F77_NAME(dhs)(double *h, double *s, double *D, int *icode);
extern void F77_NAME(dpddtd)(double *T, double *D, double *dpdD, int *icode);
extern void F77_NAME(dpddtp)(double *T, double *p, double *dpdD, int *icode);
extern void F77_NAME(dpdttd)(double *T, double *D, double *dpdT, int *icode);
extern void F77_NAME(dpdttp)(double *T, double *p, double *dpdD, int *icode);
extern void F77_NAME(dph)(double *p, double *h, double *D, int *icode);
extern void F77_NAME(dps)(double *p, double *s, double *D, int *icode);
extern void F77_NAME(dptctetab)(int *np, double *T, double *p, double *y);
extern void F77_NAME(dth)(double *T, double *h, double *D, int *icode);
extern void F77_NAME(dtp)(double *T, double *p, double *D, int *icode);
extern void F77_NAME(dtpctetab)(int *nT, double *p, double *T, double *y);
extern void F77_NAME(dts)(double *T, double *s, double *D, int *icode);
extern void F77_NAME(ftd)(double *T, double *D, double *f, int *icode);
extern void F77_NAME(ftp)(double *T, double *p, double *f, int *icode);
extern void F77_NAME(fugatp)(double *T, double *p, double *Fuga, int *icode);
extern void F77_NAME(gibbstp)(double *T, double *p, double *Gibbs, int *icode);
extern void F77_NAME(hft)(double *T, double *hf, int *icode);
extern void F77_NAME(hfttab)(int *n, double *T, double *hf);
extern void F77_NAME(hgt)(double *T, double *hg, int *icode);
extern void F77_NAME(hps)(double *p, double *s, double *h, int *icode);
extern void F77_NAME(hptctetab)(int *np, double *T, double *p, double *y);
extern void F77_NAME(htd)(double *T, double *D, double *h, int *icode);
extern void F77_NAME(htp)(double *T, double *p, double *h, int *icode);
extern void F77_NAME(htpctetab)(int *nT, double *p, double *T, double *y);
extern void F77_NAME(jtctd)(double *T, double *D, double *JTc, int *icode);
extern void F77_NAME(kapatd)(double *T, double *D, double *kapa, int *icode);
extern void F77_NAME(kvisctd)(double *T, double *D, double *KVscty, int *icode);
extern void F77_NAME(phi0dd)(double *D, double *fhizd, int *icode);
extern void F77_NAME(phi0ddd)(double *D, double *fhizd, int *icode);
extern void F77_NAME(phi0dt)(double *D, double *fhizdT, int *icode);
extern void F77_NAME(phi0td)(double *T, double *D, double *fhiztd, int *icode);
extern void F77_NAME(phi0ttd)(double *T, double *D, double *fhizttd, int *icode);
extern void F77_NAME(phi0tttd)(double *T, double *D, double *fhiztttd, int *icode);
extern void F77_NAME(phirddtd)(double *T, double *D, double *fhirdd, int *icode);
extern void F77_NAME(phirdtd)(double *T, double *D, double *fhird, int *icode);
extern void F77_NAME(phirdttd)(double *T, double *D, double *fhirdt, int *icode);
extern void F77_NAME(phirtd)(double *T, double *D, double *fhirtd, int *icode);
extern void F77_NAME(phirttd)(double *T, double *D, double *fhirt, int *icode);
extern void F77_NAME(phirtttd)(double *T, double *D, double *fhirtt, int *icode);
extern void F77_NAME(pmeltt)(double *T, double *pMelt, double *pMeltIh, double *pSubl, int *icode);
extern void F77_NAME(prandttd)(double *T, double *D, double *Prandt, int *icode);
extern void F77_NAME(psatd)(double *D, double *pSat1, double *pSat2, int *icode);
extern void F77_NAME(psats)(double *s, double *pSat, int *icode);
extern void F77_NAME(psatt)(double *T, double *pSat, int *icode);
extern void F77_NAME(psattab)(int *n, double *T, double *pSat);
extern void F77_NAME(ptd)(double *T, double *D, double *p, int *icode);
extern void F77_NAME(satdhsofp)(int *n, double *p, double *satDhs);
extern void F77_NAME(satdhsoft)(int *n, double *T, double *satDhs);
extern void F77_NAME(satvhsofp)(int *n, double *p, double *satvhs);
extern void F77_NAME(satvhsoft)(int *n, double *T, double *satvhs);
extern void F77_NAME(sft)(double *T, double *sf, int *icode);
extern void F77_NAME(sgt)(double *T, double *sg, int *icode);
extern void F77_NAME(sigmat)(double *T, double *Sigma, int *icode);
extern void F77_NAME(sph)(double *p, double *h, double *s, int *icode);
extern void F77_NAME(sptctetab)(int *np, double *T, double *p, double *y);
extern void F77_NAME(std)(double *T, double *D, double *s, int *icode);
extern void F77_NAME(stp)(double *T, double *p, double *s, int *icode);
extern void F77_NAME(stpctetab)(int *nT, double *p, double *T, double *y);
extern void F77_NAME(tdh)(double *D, double *h, double *T, int *icode);
extern void F77_NAME(tdp)(double *D, double *p, double *T, int *icode);
extern void F77_NAME(tds)(double *D, double *s, double *T, int *icode);
extern void F77_NAME(thrctd)(double *T, double *D, double *Thrc, int *icode);
extern void F77_NAME(ths)(double *h, double *s, double *T, int *icode);
extern void F77_NAME(tph)(double *p, double *h, double *T, int *icode);
extern void F77_NAME(tps)(double *p, double *s, double *T, int *icode);
extern void F77_NAME(tsatd)(double *D, double *Tsat1, double *Tsat2, int *icode);
extern void F77_NAME(tsatp)(double *p, double *Tsat, int *icode);
extern void F77_NAME(tsats)(double *s, double *Tsat, int *icode);
extern void F77_NAME(tsattab)(int *n, double *p, double *Tsat);
extern void F77_NAME(uft)(double *T, double *uf, int *icode);
extern void F77_NAME(ugt)(double *T, double *ug, int *icode);
extern void F77_NAME(utd)(double *T, double *D, double *u, int *icode);
extern void F77_NAME(utp)(double *T, double *p, double *u, int *icode);
extern void F77_NAME(visctd)(double *T, double *D, double *Vscty, int *icode);
extern void F77_NAME(vtp)(double *T, double *p, double *v, int *icode);
extern void F77_NAME(wft)(double *T, double *wf, int *icode);
extern void F77_NAME(wgt)(double *T, double *wg, int *icode);
extern void F77_NAME(wtd)(double *T, double *D, double *w, int *icode);
extern void F77_NAME(wtp)(double *T, double *p, double *w, int *icode);
extern void F77_NAME(ztd)(double *T, double *D, double *Z, int *icode);

static const R_FortranMethodDef FortranEntries[] = {
  {"bt",        (DL_FUNC) &F77_NAME(bt),        3},
  {"cndtd",     (DL_FUNC) &F77_NAME(cndtd),     4},
  {"cpft",      (DL_FUNC) &F77_NAME(cpft),      3},
  {"cpgt",      (DL_FUNC) &F77_NAME(cpgt),      3},
  {"cptd",      (DL_FUNC) &F77_NAME(cptd),      4},
  {"cptp",      (DL_FUNC) &F77_NAME(cptp),      4},
  {"ct",        (DL_FUNC) &F77_NAME(ct),        3},
  {"cvft",      (DL_FUNC) &F77_NAME(cvft),      3},
  {"cvgt",      (DL_FUNC) &F77_NAME(cvgt),      3},
  {"cvtd",      (DL_FUNC) &F77_NAME(cvtd),      4},
  {"cvtp",      (DL_FUNC) &F77_NAME(cvtp),      4},
  {"dddttd",    (DL_FUNC) &F77_NAME(dddttd),    4},
  {"dddttp",    (DL_FUNC) &F77_NAME(dddttp),    4},
  {"dfp",       (DL_FUNC) &F77_NAME(dfp),       3},
  {"dfs",       (DL_FUNC) &F77_NAME(dfs),       3},
  {"dft",       (DL_FUNC) &F77_NAME(dft),       3},
  {"dgp",       (DL_FUNC) &F77_NAME(dgp),       3},
  {"dgs",       (DL_FUNC) &F77_NAME(dgs),       3},
  {"dgt",       (DL_FUNC) &F77_NAME(dgt),       3},
  {"dhs",       (DL_FUNC) &F77_NAME(dhs),       4},
  {"dpddtd",    (DL_FUNC) &F77_NAME(dpddtd),    4},
  {"dpddtp",    (DL_FUNC) &F77_NAME(dpddtp),    4},
  {"dpdttd",    (DL_FUNC) &F77_NAME(dpdttd),    4},
  {"dpdttp",    (DL_FUNC) &F77_NAME(dpdttp),    4},
  {"dph",       (DL_FUNC) &F77_NAME(dph),       4},
  {"dps",       (DL_FUNC) &F77_NAME(dps),       4},
  {"dptctetab", (DL_FUNC) &F77_NAME(dptctetab), 4},
  {"dth",       (DL_FUNC) &F77_NAME(dth),       5},
  {"dtp",       (DL_FUNC) &F77_NAME(dtp),       4},
  {"dtpctetab", (DL_FUNC) &F77_NAME(dtpctetab), 4},
  {"dts",       (DL_FUNC) &F77_NAME(dts),       4},
  {"ftd",       (DL_FUNC) &F77_NAME(ftd),       4},
  {"ftp",       (DL_FUNC) &F77_NAME(ftp),       4},
  {"fugatp",    (DL_FUNC) &F77_NAME(fugatp),    4},
  {"gibbstp",   (DL_FUNC) &F77_NAME(gibbstp),   4},
  {"hft",       (DL_FUNC) &F77_NAME(hft),       3},
  {"hfttab",    (DL_FUNC) &F77_NAME(hfttab),    3},
  {"hgt",       (DL_FUNC) &F77_NAME(hgt),       3},
  {"hps",       (DL_FUNC) &F77_NAME(hps),       4},
  {"hptctetab", (DL_FUNC) &F77_NAME(hptctetab), 4},
  {"htd",       (DL_FUNC) &F77_NAME(htd),       4},
  {"htp",       (DL_FUNC) &F77_NAME(htp),       4},
  {"htpctetab", (DL_FUNC) &F77_NAME(htpctetab), 4},
  {"jtctd",     (DL_FUNC) &F77_NAME(jtctd),     4},
  {"kapatd",    (DL_FUNC) &F77_NAME(kapatd),    4},
  {"kvisctd",   (DL_FUNC) &F77_NAME(kvisctd),   4},
  {"phi0dd",    (DL_FUNC) &F77_NAME(phi0dd),    3},
  {"phi0ddd",   (DL_FUNC) &F77_NAME(phi0ddd),   3},
  {"phi0dt",    (DL_FUNC) &F77_NAME(phi0dt),    2},
  {"phi0td",    (DL_FUNC) &F77_NAME(phi0td),    4},
  {"phi0ttd",   (DL_FUNC) &F77_NAME(phi0ttd),   4},
  {"phi0tttd",  (DL_FUNC) &F77_NAME(phi0tttd),  4},
  {"phirddtd",  (DL_FUNC) &F77_NAME(phirddtd),  4},
  {"phirdtd",   (DL_FUNC) &F77_NAME(phirdtd),   4},
  {"phirdttd",  (DL_FUNC) &F77_NAME(phirdttd),  4},
  {"phirtd",    (DL_FUNC) &F77_NAME(phirtd),    4},
  {"phirttd",   (DL_FUNC) &F77_NAME(phirttd),   4},
  {"phirtttd",  (DL_FUNC) &F77_NAME(phirtttd),  4},
  {"pmeltt",    (DL_FUNC) &F77_NAME(pmeltt),    5},
  {"prandttd",  (DL_FUNC) &F77_NAME(prandttd),  4},
  {"psatd",     (DL_FUNC) &F77_NAME(psatd),     4},
  {"psats",     (DL_FUNC) &F77_NAME(psats),     3},
  {"psatt",     (DL_FUNC) &F77_NAME(psatt),     3},
  {"psattab",   (DL_FUNC) &F77_NAME(psattab),   3},
  {"ptd",       (DL_FUNC) &F77_NAME(ptd),       4},
  {"satdhsofp", (DL_FUNC) &F77_NAME(satdhsofp), 3},
  {"satdhsoft", (DL_FUNC) &F77_NAME(satdhsoft), 3},
  {"satvhsofp", (DL_FUNC) &F77_NAME(satvhsofp), 3},
  {"satvhsoft", (DL_FUNC) &F77_NAME(satvhsoft), 3},
  {"sft",       (DL_FUNC) &F77_NAME(sft),       3},
  {"sgt",       (DL_FUNC) &F77_NAME(sgt),       3},
  {"sigmat",    (DL_FUNC) &F77_NAME(sigmat),    3},
  {"sph",       (DL_FUNC) &F77_NAME(sph),       4},
  {"sptctetab", (DL_FUNC) &F77_NAME(sptctetab), 4},
  {"std",       (DL_FUNC) &F77_NAME(std),       4},
  {"stp",       (DL_FUNC) &F77_NAME(stp),       4},
  {"stpctetab", (DL_FUNC) &F77_NAME(stpctetab), 4},
  {"tdh",       (DL_FUNC) &F77_NAME(tdh),       4},
  {"tdp",       (DL_FUNC) &F77_NAME(tdp),       4},
  {"tds",       (DL_FUNC) &F77_NAME(tds),       4},
  {"thrctd",    (DL_FUNC) &F77_NAME(thrctd),    4},
  {"ths",       (DL_FUNC) &F77_NAME(ths),       4},
  {"tph",       (DL_FUNC) &F77_NAME(tph),       4},
  {"tps",       (DL_FUNC) &F77_NAME(tps),       4},
  {"tsatd",     (DL_FUNC) &F77_NAME(tsatd),     4},
  {"tsatp",     (DL_FUNC) &F77_NAME(tsatp),     3},
  {"tsats",     (DL_FUNC) &F77_NAME(tsats),     3},
  {"tsattab",   (DL_FUNC) &F77_NAME(tsattab),   3},
  {"uft",       (DL_FUNC) &F77_NAME(uft),       3},
  {"ugt",       (DL_FUNC) &F77_NAME(ugt),       3},
  {"utd",       (DL_FUNC) &F77_NAME(utd),       4},
  {"utp",       (DL_FUNC) &F77_NAME(utp),       4},
  {"visctd",    (DL_FUNC) &F77_NAME(visctd),    4},
  {"vtp",       (DL_FUNC) &F77_NAME(vtp),       4},
  {"wft",       (DL_FUNC) &F77_NAME(wft),       3},
  {"wgt",       (DL_FUNC) &F77_NAME(wgt),       3},
  {"wtd",       (DL_FUNC) &F77_NAME(wtd),       4},
  {"wtp",       (DL_FUNC) &F77_NAME(wtp),       4},
  {"ztd",       (DL_FUNC) &F77_NAME(ztd),       4},
  {NULL, NULL, 0}
};

void R_init_IAPWS95(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
