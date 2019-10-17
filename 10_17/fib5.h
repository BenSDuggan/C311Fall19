struct continuation;
typedef struct continuation continuation;
struct continuation {
  enum {
    _fibr__m__subr2_continuation,
    _fibr__m__subr1_continuation,
    _initr__m__k_continuation
  } tag;
  union {
    struct { void *_fibr__m__subr1; void *_k; } _fibr__m__subr2;
    struct { void *_n; void *_k; } _fibr__m__subr1;
    struct { void *_jumpr__m__out; } _initr__m__k;
  } u;
};

void *continuationr_fibr__m__subr2(void *fibr__m__subr1, void *k);
void *continuationr_fibr__m__subr1(void *n, void *k);
void *continuationr_initr__m__k(void *jumpr__m__out);

void *fibr__m__cpsr__m__n, *applyr__m__kr__m__v, *cc;

void (*pc)();

void applyr__m__k();
void fibr__m__cps();
int main();
int mount_tram();

struct _trstr;
typedef struct _trstr _trstr;
struct _trstr {
  jmp_buf *jmpbuf;
  int value;
};

