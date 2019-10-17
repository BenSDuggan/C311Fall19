#include <setjmp.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "fib5.h"

void *continuationr_fibr__m__subr2(void *fibr__m__subr1, void *k) {
continuation* _data = (continuation*)malloc(sizeof(continuation));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _fibr__m__subr2_continuation;
  _data->u._fibr__m__subr2._fibr__m__subr1 = fibr__m__subr1;
  _data->u._fibr__m__subr2._k = k;
  return (void *)_data;
}

void *continuationr_fibr__m__subr1(void *n, void *k) {
continuation* _data = (continuation*)malloc(sizeof(continuation));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _fibr__m__subr1_continuation;
  _data->u._fibr__m__subr1._n = n;
  _data->u._fibr__m__subr1._k = k;
  return (void *)_data;
}

void *continuationr_initr__m__k(void *jumpr__m__out) {
continuation* _data = (continuation*)malloc(sizeof(continuation));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _initr__m__k_continuation;
  _data->u._initr__m__k._jumpr__m__out = jumpr__m__out;
  return (void *)_data;
}

int main()
{
fibr__m__cpsr__m__n = (void *)(void *)5;
pc = &fibr__m__cps;
mount_tram();
printf("%d\n", (int)applyr__m__kr__m__v);}

void fibr__m__cps()
{
if((fibr__m__cpsr__m__n <= (void *)1)) {
  applyr__m__kr__m__v = (void *)(void *)1;
pc = &applyr__m__k;

} else {
  cc = (void *)continuationr_fibr__m__subr1(fibr__m__cpsr__m__n,cc);
fibr__m__cpsr__m__n = (void *)(void *)((int)fibr__m__cpsr__m__n - 1);
pc = &fibr__m__cps;

}
}

void applyr__m__k()
{
continuation* _c = (continuation*)cc;
switch (_c->tag) {
case _fibr__m__subr2_continuation: {
void *fibr__m__subr1 = _c->u._fibr__m__subr2._fibr__m__subr1;
void *k = _c->u._fibr__m__subr2._k;
cc = (void *)k;
applyr__m__kr__m__v = (void *)(void *)((int)fibr__m__subr1 + (int)applyr__m__kr__m__v);
pc = &applyr__m__k;
break; }
case _fibr__m__subr1_continuation: {
void *n = _c->u._fibr__m__subr1._n;
void *k = _c->u._fibr__m__subr1._k;
cc = (void *)continuationr_fibr__m__subr2(applyr__m__kr__m__v,k);
fibr__m__cpsr__m__n = (void *)(void *)((int)(void *)((int)n - 1) - 1);
pc = &fibr__m__cps;
break; }
case _initr__m__k_continuation: {
void *jumpr__m__out = _c->u._initr__m__k._jumpr__m__out;
_trstr *trstr = (_trstr *)jumpr__m__out;
longjmp(*trstr->jmpbuf, 1);
break; }
}
}

int mount_tram ()
{
srand (time (NULL));
jmp_buf jb;
_trstr trstr;
void *dismount;
int _status = setjmp(jb);
trstr.jmpbuf = &jb;
dismount = &trstr;
if(!_status) {
cc= (void *)continuationr_initr__m__k(dismount);
for(;;) {
pc();
}
}
return 0;
}
