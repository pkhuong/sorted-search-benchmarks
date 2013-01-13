#define RUNME /*
g++ -O3 -DPAPI -DPARALLEL -W -Wall -fpermissive tb-search.cpp -o parallel -L$HOME/lib/  -lpapi -ltcmalloc_minimal -lm
exit $?
TCMALLOC_LARGE_ALLOC_REPORT_THRESHOLD=$((20*1024*1024*1024)) LD_LIBRARY_PATH=$HOME/lib/ numactl -m 1 -N 1 ./parallel 27 0 1  > /dev/null

g++ -O3 -DCK_HT -DPAPI -DPARALLEL -W -Wall -fpermissive tb-search.cpp -o parallel -L$HOME/lib/ -I$HOME/ck/include -L$HOME/ck/src/ -lck -lpapi -ltcmalloc_minimal -lm
exit $?
TCMALLOC_LARGE_ALLOC_REPORT_THRESHOLD=$((20*1024*1024*1024)) LD_LIBRARY_PATH=$HOME/lib/:. ./parallel 29 -1 -1 64 > /dev/null

               */
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <math.h>
#include <set>
#include "sparsehash/dense_hash_set"
#include "cycle.h"

extern "C" {
#ifdef CK_HT
# include "ck_ht.h"
#endif
#ifdef PAPI
# include <papi.h>
#endif
}

#define likely(x) (__builtin_expect((x), 1))
#define unlikely(x) (__builtin_expect((x), 0))
#define ALWAYS_INLINE static inline __attribute__((always_inline)) 
#define AUTO __attribute__((cleanup(free_ptr)))

static void free_ptr (void * pptr)
{
        free(*(void**)pptr);
}

#define BARRIER do { __asm__ volatile("" ::: "memory"); } while (0)

#define BINARY(I) do {                                          \
                base = ((base)[I] <= key)?base+I:base;          \
                BARRIER;                                        \
        } while (0)

#ifdef PARALLEL_LOAD
#define TERNARY(I, J) do {                                      \
                unsigned * b1 = base+I, *b2 = base+J;           \
                unsigned mid1 = *b1, mid2 = *b2;                \
                BARRIER;                                        \
                base = (mid1 <= key)?b1:base;                   \
                base = (mid2 <= key)?b2:base;                   \
        } while(0)
#else
#define TERNARY(I, J) do {                                      \
                unsigned * b1 = base+I, *b2 = base+J;           \
                b1 = (*b2 <= key)?b2:b1;                        \
                BARRIER;                                        \
                base = (*b1 <= key)?b1:base;                    \
                BARRIER;                                        \
        } while(0)
#endif

#define QUATERNARY(I, J, K) do {                                        \
                unsigned * b1 = base+I, *b2 = base+J, *b3 = base+K;     \
                unsigned mid1 = *b1, mid2 = *b2, mid3 = *b3;            \
                BARRIER;                                                \
                base = (mid1 <= key)?b1:base;                           \
                base = (mid2 <= key)?b2:base;                           \
                base = (mid3 <= key)?b3:base;                           \
        } while(0)

typedef size_t (*search_fun)(unsigned, unsigned*);

#include "bsearch.inc"
#include "tsearch.inc"
#include "tbsearch.inc"
#include "qsearch.inc"
#include "osearch.inc"
#include "oqsearch.inc"

size_t dummy(unsigned key, unsigned * vector)
{
        (void)key;
        (void)vector;
        return -1UL;
}

static int shuffle_stride = -1;

unsigned * make_indices(unsigned key_low, unsigned key_high, size_t count)
{
        unsigned * indices;
        assert(!posix_memalign((void**)&indices, 1ul<<20,
                               count*sizeof(unsigned)));

        if (key_high < key_low) {
                unsigned tmp = key_high;
                key_high = key_low;
                key_low = tmp;
        }

        for (size_t i = 0; i < count; i++) {
                if (shuffle_stride == -1) {
                        unsigned rnd = 1.0*random()/RAND_MAX*(key_high-key_low);
                        indices[i] = key_low + rnd;
                } else {
                        indices[i] = key_low 
                                + ((i*shuffle_stride)%(key_high-key_low));
                }
        }

        return indices;
}

unsigned *
make_shuffled_indices(unsigned stride, unsigned count, unsigned repeat)
{
        unsigned * indices;
        assert(!posix_memalign((void**)&indices, 1ul<<20,
                               count*repeat*sizeof(unsigned)));
        if (shuffle_stride == -1) {
                for (unsigned i = 0; i < count; i++) {
                        unsigned max = i+1;
                        unsigned j = 1.0*max*random()/RAND_MAX;
                        indices[i] = indices[j];
                        indices[j] = stride*i;
                }
        } else {
                for (unsigned i = 0, j = 0; i < count;
                     i++, j = (j+stride)%count)
                        indices[i] = j;
        }
        for (unsigned i = 1, * dst=indices+count;
             i < repeat;
             i++, dst += count)
                memcpy(dst, indices, count*sizeof(unsigned));

        return indices;
}

unsigned * make_haystacks(unsigned key_low, unsigned stride,
                          size_t chunk_size, unsigned chunk_count)
{
        unsigned * needles;
        assert(!posix_memalign((void**)&needles, 1ul<<20,
                               chunk_size
                               *chunk_count
                               *sizeof(unsigned)));
        for (size_t i = 0, idx = 0; i < chunk_count; i++) {
                for (unsigned key = key_low, j = 0; j < chunk_size;
                     key += stride, j++)
                        needles[idx++] = key;
        }

        return needles;
}

#ifdef PAPI
static int event_set;
#endif
 
#include <stdio.h>

double * bench_present (unsigned chunk_size,
                        unsigned search_begin, unsigned search_end,
                        unsigned repeat, unsigned subrun,
                        search_fun search)
{
        srandom(12345);
        unsigned total_count = repeat*subrun;
        if (search_begin == -1U)
                search_begin = 0;
        if (search_end == -1U)
                search_end = chunk_size;
        AUTO unsigned * keys = make_indices(search_begin, search_end,
                                            total_count);
        AUTO unsigned * haystack = make_haystacks(0, 1, chunk_size, 1);
        double * times = calloc(repeat, sizeof(double));

#ifdef PAPI
        assert(PAPI_OK == PAPI_reset(event_set));
#endif
        size_t idx = 0;
        for (unsigned i = 0; i < repeat; i++) {
                ticks begin;
                if (subrun <= 1) {
                        unsigned key = keys[idx++];
                        begin = getticks();
                        size_t x = search(key, haystack);
#ifdef CHECK
                        if (search != dummy)
                                assert(haystack[x] == key);
#else
                        assert(likely(x != -2U));
#endif
                } else {
                        begin = getticks();
                        for (unsigned j = 0; j < subrun; j++) {
                                unsigned key = keys[idx++];
                                size_t x = search(key, haystack);
#ifdef CHECK
                                if (search != dummy)
                                        assert(haystack[x] == key);
#else
                                assert(likely(x != -2U));
#endif
                        }
                }
                ticks end = getticks();
                times[i] = elapsed(end, begin)/subrun;
        }

        return times;
}

static std::set<unsigned> set_haystack;

double * bench_stl (unsigned chunk_size,
                    unsigned search_begin, unsigned search_end,
                    unsigned repeat, unsigned subrun)
{
        srandom(12345);
        unsigned total_count = repeat*subrun;
        if (search_begin == -1U)
                search_begin = 0;
        if (search_end == -1U)
                search_end = chunk_size;
        AUTO unsigned * keys = make_indices(search_begin, search_end,
                                            total_count);
        std::set<unsigned> &haystack(set_haystack);
        haystack.clear();
        for (unsigned i = 0; i < chunk_size; i++)
                haystack.insert(haystack.end(), i);
        double * times = calloc(repeat, sizeof(double));

#ifdef PAPI
        assert(PAPI_OK == PAPI_reset(event_set));
#endif
        size_t idx = 0;
        std::set<unsigned>::iterator end = haystack.end();
        for (unsigned i = 0; i < repeat; i++) {
                ticks begin;
                if (subrun <= 1) {
                        unsigned key = keys[idx++];
                        begin = getticks();
                        std::set<unsigned>::iterator x
                                = haystack.lower_bound(key);
#ifdef CHECK
                        assert(*x == key);
#else
                        assert(likely(x != end));
#endif
                } else {
                        begin = getticks();
                        for (unsigned j = 0; j < subrun; j++) {
                                unsigned key = keys[idx++];
                                std::set<unsigned>::iterator x
                                        = haystack.lower_bound(key);
#ifdef CHECK
                                assert(*x == key);
#else
                                assert(likely(x != end));
#endif
                        }
                }
                ticks end = getticks();
                times[i] = elapsed(end, begin)/subrun;
        }

        return times;
}

struct hash_u32
{
        size_t tables[256][8];
        
        hash_u32()
        {
                for (unsigned i = 0; i < 256; i++) {
                        for (unsigned j = 0; j < 8; j++) {
                                size_t x = random(), y = random();
                                tables[i][j] = x^(y<<32);
                        }
                }
        }

        size_t operator() (unsigned xx) const
        {
                // size_t x = xx;
                // x = x*15028999435905310454ul+16708911996216745849ul;
                // return x%((1ul<<61)-1); // mersenne

                unsigned x0 = xx&0xff,
                        x1 = (xx>>8)&0xff,
                        x2 = (xx>>16)&0xff,
                        x3 = (xx>>24)&0xff;
                return tables[x0][0] ^ tables[x1][1]
                        ^ tables[x2][2] ^ tables[x3][3];
        }
};

typedef google::dense_hash_set<unsigned, hash_u32> hash_t;

static hash_t hash_haystack;

double * bench_hash (unsigned chunk_size,
                     unsigned search_begin, unsigned search_end,
                     unsigned repeat, unsigned subrun)
{
        srandom(12345);
        unsigned total_count = repeat*subrun;
        if (search_begin == -1U)
                search_begin = 0;
        if (search_end == -1U)
                search_end = chunk_size;
        AUTO unsigned * keys = make_indices(search_begin, search_end,
                                            total_count);
        hash_t &haystack(hash_haystack);
        haystack.set_empty_key(-1U);
        haystack.clear();
        haystack.resize(chunk_size);
        for (unsigned i = 0; i < chunk_size; i++)
                haystack.insert(i);
        double * times = calloc(repeat, sizeof(double));

#ifdef PAPI
        assert(PAPI_OK == PAPI_reset(event_set));
#endif
        size_t idx = 0;
        hash_t::iterator end = haystack.end();
        for (unsigned i = 0; i < repeat; i++) {
                ticks begin;
                if (subrun <= 1) {
                        unsigned key = keys[idx++];
                        begin = getticks();
                        hash_t::iterator x = haystack.find(key);
#ifdef CHECK
                        assert(*x == key);
#else
                        assert(likely(x != end));
#endif
                } else {
                        begin = getticks();
                        for (unsigned j = 0; j < subrun; j++) {
                                unsigned key = keys[idx++];
                                hash_t::iterator x = haystack.find(key);
#ifdef CHECK
                                assert(*x == key);
#else
                                assert(likely(x != end));
#endif
                        }
                }
                ticks end = getticks();
                times[i] = elapsed(end, begin)/subrun;
        }

        return times;
}

#ifdef CK_HT
static ck_ht ck_haystack;
static hash_u32 ck_hasher;

static void
ck_hash_fun (ck_ht_hash_t * h, const void * key,
             size_t len, uint64_t seed)
{
        (void)len;
        (void)seed;
        const uintptr_t * v = (const uintptr_t*)key;
        h->value = ck_hasher(*v);
}

static void * ht_malloc(size_t r)
{
	return malloc(r);
}

static void ht_free(void *p, size_t b, bool r)
{

	(void)b;
	(void)r;
	free(p);
}

double * bench_ck (unsigned chunk_size,
                   unsigned search_begin, unsigned search_end,
                   unsigned repeat, unsigned subrun)
{
        srandom(12345);
        unsigned total_count = repeat*subrun;
        if (search_begin == -1U)
                search_begin = 0;
        if (search_end == -1U)
                search_end = chunk_size;
        AUTO unsigned * keys = make_indices(search_begin, search_end,
                                            total_count);
        ck_ht * haystack = &ck_haystack;
        {
                static ck_malloc ck_allocator;
                ck_allocator.malloc = ht_malloc;
                ck_allocator.free   = ht_free;
                assert(ck_ht_init(haystack, CK_HT_MODE_DIRECT,
                                  ck_hash_fun, &ck_allocator,
                                  8, random()));
                ck_ht_grow_spmc(haystack, 2*chunk_size);
        }

        for (unsigned i = 0; i < chunk_size; i++) {
                ck_ht_entry_t entry;
                ck_ht_hash_t h;
                uintptr_t k = (uintptr_t)i|(1ul<<32);
                ck_ht_hash_direct(&h, haystack, k);
                ck_ht_entry_set_direct(&entry, k, k);
                assert(ck_ht_put_spmc(haystack, h, &entry));
        }
        double * times = calloc(repeat, sizeof(double));

#ifdef PAPI
        assert(PAPI_OK == PAPI_reset(event_set));
#endif
        size_t idx = 0;
        for (unsigned i = 0; i < repeat; i++) {
                ticks begin;
                if (subrun <= 1) {
                        unsigned key = keys[idx++];
                        uintptr_t frobbed_key = (uintptr_t)key|(1ul<<32);
                        ck_ht_entry_t entry;
                        int success;
                        begin = getticks();
                        {
                                ck_ht_hash_t h;
                                ck_ht_hash_direct(&h, haystack, frobbed_key);
                                ck_ht_entry_key_set_direct(&entry, frobbed_key);
                                success = ck_ht_get_spmc(haystack,
                                                         h, &entry);
                        }
#ifdef CHECK
                        assert(ck_ht_entry_value_direct(&entry)
                               == frobbed_key);
#else
                        assert(likely(success));
#endif
                } else {
                        begin = getticks();
                        for (unsigned j = 0; j < subrun; j++) {
                                unsigned key = keys[idx++];
                                uintptr_t frobbed_key = (uintptr_t)key|(1ul<<32);
                                ck_ht_entry_t entry;
                                int success;
                                begin = getticks();
                                {
                                        ck_ht_hash_t h;
                                        ck_ht_hash_direct(&h, haystack, frobbed_key);
                                        ck_ht_entry_key_set_direct(&entry, frobbed_key);
                                        success = ck_ht_get_spmc(haystack,
                                                                 h, &entry);
                                }
#ifdef CHECK
                                assert(ck_ht_entry_value_direct(&entry)
                                       == frobbed_key);
#else
                                assert(likely(success));
#endif
                        }
                }
                ticks end = getticks();
                times[i] = elapsed(end, begin)/subrun;
        }

        return times;
}
#endif

void print_doubles (const double * values, size_t count,
                    const char * prefix, unsigned size)
{
        double acc = 0;
        for (size_t i = 0; i < count; i++) {
                double v = values[i];
                acc += v;
                printf("%s\t%u\t%f\n", prefix, size, v);
        }

        //fprintf(stderr, "%s\t%f\n", prefix, acc/count);
}

#define ARG(N, VAR) do {                        \
        if (argc > (N))                         \
                VAR = atoi(argv[(N)]);          \
        } while (0)

search_fun baseline[] = {NULL, NULL, NULL, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy};

search_fun stl[] = {NULL, NULL, NULL, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy};

search_fun hash[] = {NULL, NULL, NULL, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy};

#ifdef CK_HT
search_fun ck_hash[] = {NULL, NULL, NULL, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy};
#else
search_fun ck_hash[] = {NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};
#endif

int main (int argc, char ** argv)
{
        unsigned chunk_size = 5;
        unsigned repeat = 100000;
        unsigned subrun = 1;
        unsigned search_begin = -1U;
        unsigned search_end = -1U;
        ARG(1, chunk_size);
        ARG(2, search_begin);
        ARG(3, search_end);
        ARG(4, shuffle_stride);
        ARG(5, repeat);
        ARG(6, subrun);

        assert(chunk_size <= 30);

        //printf("Search\tSize\tTicks\n");
#ifdef PARALLEL_LOAD
        search_fun*  tests[] = {baseline, binary, quat, ternary, tb, offset,
                                off_quat, stl, ck_hash, hash};
        const char * names[] = {"baseline", "bin", "quat", "ter", "tb", "ob",
                                "oq", "stl", "ck_ht", "goog"};
#else
        search_fun*  tests[] = {baseline, ternary};
        const char * names[] = {"baseline", "st"};
#endif

        double baseline_time = 0;
#ifdef PAPI
        struct { long long values[4]; } base, now;
        event_set = PAPI_NULL;
        assert(PAPI_VER_CURRENT == PAPI_library_init(PAPI_VER_CURRENT));
        assert(PAPI_OK == PAPI_create_eventset(&event_set));
        //assert(PAPI_OK == PAPI_add_event(event_set, PAPI_TOT_CYC));
        //assert(PAPI_OK == PAPI_add_event(event_set, PAPI_RES_STL));
        //assert(PAPI_OK == PAPI_add_event(event_set, PAPI_BR_MSP));
        assert(PAPI_OK == PAPI_add_event(event_set, PAPI_L1_LDM));
        assert(PAPI_OK == PAPI_add_event(event_set, PAPI_L2_LDM));
        assert(PAPI_OK == PAPI_add_event(event_set, PAPI_L3_TCM));
        assert(PAPI_OK == PAPI_add_event(event_set, PAPI_TLB_DM));
        assert(PAPI_OK == PAPI_start(event_set));
        bzero(&base, sizeof(base));
#endif

        for (unsigned i = 0; i < sizeof(tests)/sizeof(tests[0]); i++) {
                set_haystack.clear();
#ifdef CK_HT
                if (ck_haystack.m)
                        ck_ht_destroy(&ck_haystack);
#endif
                if (tests[i][chunk_size] == NULL) continue;
                AUTO double * times = NULL;
#ifdef PAPI
                bzero(&now, sizeof(now));
#endif
                unsigned begin = search_begin, end = search_end;
                if ((begin != -1U) && (begin >= 1ul<<chunk_size))
                        begin = (1ul<<chunk_size)-1;
                if ((end != -1U) && (end >= 1ul<<chunk_size))
                        end = (1ul<<chunk_size);
                
                /* indice range is from end */
                if (0) {
                        if (begin != -1U)
                                begin = (1u<<chunk_size)-begin;
                        if (end != -1U)
                                end = (1u<<chunk_size)-end;
                        unsigned tmp = end;
                        end = begin;
                        begin = tmp;
                }
                if (tests[i] == stl) {
                        if (chunk_size > 27) continue;
                        times = bench_stl(1u<<chunk_size,
                                          begin, end,
                                          repeat, subrun);
                } else if (tests[i] == hash) {
                        if (chunk_size > 29) continue;
                        times = bench_hash(1u<<chunk_size,
                                           begin, end,
                                           repeat, subrun);
                } else if (tests[i] == ck_hash) {
                        if (chunk_size > 27) continue;
#ifdef CK_HT
                        times = bench_ck(1u<<chunk_size,
                                         begin, end,
                                         repeat, subrun);
#endif
                } else {
                        times = bench_present(1u<<chunk_size,
                                              begin, end,
                                              repeat, subrun,
                                              tests[i][chunk_size]);
                }
#ifdef PAPI
                assert(PAPI_OK == PAPI_accum(event_set, &now.values[0]));
#endif
                if (i == 0) {
                        baseline_time = HUGE_VAL;
                        for (unsigned j = 0; j < repeat; j++)
                                baseline_time = fmin(baseline_time, times[j]);
#ifdef PAPI
                        base = now;
#if 0
                        fprintf(stderr, "Baseline: %f %lli %lli %lli %lli\n",
                                baseline_time,
                                base.values[0], base.values[1], 
                                base.values[2], base.values[3]);
#endif
#else
#if 0
                        fprintf(stderr, "Baseline: %f\n",
                                baseline_time);
#endif
#endif
                } else {
                        for (unsigned j = 0; j < repeat; j++)
                                times[j] -= baseline_time;
                        print_doubles(times, repeat, names[i], 1u<<chunk_size);
#ifdef PAPI
                        fprintf(stderr, "%s\t%u", names[i], 1u<<chunk_size);
                        for (unsigned i = 0;
                             i < sizeof(now.values)/sizeof(now.values[0]);
                             i++) {
                                double x = 1.0*(now.values[i]-base.values[i])
                                        /(repeat*subrun);
                                if (x < 0) x = 0;
                                fprintf(stderr, "\t%.2f", x);
                        }
                        fprintf(stderr, "\n");
#endif
                }
        }

#ifdef PAPI
        assert(PAPI_OK == PAPI_stop(event_set, &now.values[0]));
        assert(PAPI_OK == PAPI_cleanup_eventset(event_set));
        assert(PAPI_OK == PAPI_destroy_eventset(&event_set));
#endif

        return 0;
}
