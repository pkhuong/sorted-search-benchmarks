#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <math.h>
#include <set>
#include "cycle.h"
#ifdef PAPI
extern "C" {
# include <papi.h>
}
#endif

#define likely(x) (__builtin_expect((x), 1))
#define unlikely(x) (__builtin_expect((x), 0))
#define ALWAYS_INLINE static inline __attribute__((always_inline)) 
#define AUTO __attribute__((cleanup(free_ptr)))

static void free_ptr (void * pptr)
{
        free(*(void**)pptr);
}

#define BINARY(I) (base = (((volatile unsigned*)base)[I] <= key)?base+I:base)
#ifdef PARALLEL_LOAD
#define TERNARY(I, J) do {                                      \
                unsigned * b1 = base+I, *b2 = base+J;           \
                unsigned mid1 = *b1, mid2 = *b2;                \
                base = (mid1 <= key)?b1:base;                   \
                base = (mid2 <= key)?b2:base;                   \
        } while(0)
#else
#define TERNARY(I, J) do {                                      \
                volatile unsigned * b1 = base+I, *b2 = base+J;  \
                b1 = (*b2 <= key)?b2:b1;                        \
                base = (*b1 <= key)?(unsigned*)b1:base;         \
        } while(0)
#endif

#define QUATERNARY(I, J, K) do {                                        \
                unsigned * b1 = base+I, *b2 = base+J, *b3 = base+K;     \
                unsigned mid1 = *b1, mid2 = *b2, mid3 = *b3;            \
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

unsigned * make_indices(unsigned key_low, unsigned key_high, size_t count)
{
        unsigned * indices;
        assert(!posix_memalign((void**)&indices, 1ul<<20,
                               count*sizeof(unsigned)));

        for (size_t i = 0; i < count; i++) {
                unsigned rnd = 1.0*random()/RAND_MAX*(key_high-key_low);
                indices[i] = key_low + rnd;
        }

        return indices;
}

unsigned *
make_shuffled_indices(unsigned stride, unsigned count, unsigned repeat)
{
        unsigned * indices;
        assert(!posix_memalign((void**)&indices, 1ul<<20,
                               count*repeat*sizeof(unsigned)));
        for (unsigned i = 0; i < count; i++) {
                unsigned max = i+1;
                unsigned j = 1.0*max*random()/RAND_MAX;
                indices[i] = indices[j];
                indices[j] = stride*i;
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
        std::set<unsigned> haystack;
        for (unsigned i = 0; i < chunk_size; i++)
                haystack.insert(i);
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
                        std::set<unsigned>::iterator x
                                = haystack.lower_bound(key);
#ifdef CHECK
                        if (search != dummy)
                                assert(*x == key);
#else
                        assert(likely(x != haystack.end()));
#endif
                } else {
                        begin = getticks();
                        for (unsigned j = 0; j < subrun; j++) {
                                unsigned key = keys[idx++];
                                std::set<unsigned>::iterator x
                                        = haystack.lower_bound(key);
#ifdef CHECK
                                if (search != dummy)
                                        assert(*x == key);
#else
                                assert(likely(x != haystack.end()));
#endif
                        }
                }
                ticks end = getticks();
                times[i] = elapsed(end, begin)/subrun;
        }

        return times;
}

void print_doubles (const double * values, size_t count,
                    const char * prefix, unsigned size)
{
        double acc = 0;
        for (size_t i = 0; i < count; i++) {
                double v = values[i];
                acc += v;
                printf("%s\t%u\t%f\n", prefix, size, v);
        }

        fprintf(stderr, "%s   \t%f\n", prefix, acc/count);
}

#define ARG(N, VAR) do {                        \
        if (argc > (N))                         \
                VAR = atoi(argv[(N)]);          \
        } while (0)

search_fun baseline[] = {NULL, NULL, NULL, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy, dummy};


int main (int argc, char ** argv)
{
        unsigned chunk_size = 5;
        unsigned repeat = 10000;
        unsigned subrun = 1;
        unsigned search_begin = -1U;
        unsigned search_end = -1U;
        ARG(1, chunk_size);
        ARG(2, search_begin);
        ARG(3, search_end);
        ARG(4, repeat);
        ARG(5, subrun);

        assert(chunk_size <= 30);

        printf("Search\tSize\tTicks\n");
        search_fun*  tests[] = {baseline, binary, quat, ternary, tb, offset,
                                off_quat};
        const char * names[] = {"baseline", "bin", "quat", "ter", "tb", "ob",
                                "oq"};

        double baseline_time = 0;
#ifdef PAPI
        struct { long long values[4]; } base, now;
        event_set = PAPI_NULL;
        assert(PAPI_VER_CURRENT == PAPI_library_init(PAPI_VER_CURRENT));
        assert(PAPI_OK == PAPI_create_eventset(&event_set));
        assert(PAPI_OK == PAPI_add_event(event_set, PAPI_TOT_CYC));
        //assert(PAPI_OK == PAPI_add_event(event_set, PAPI_RES_STL));
        //assert(PAPI_OK == PAPI_add_event(event_set, PAPI_BR_MSP));
        assert(PAPI_OK == PAPI_add_event(event_set, PAPI_L1_LDM));
        assert(PAPI_OK == PAPI_add_event(event_set, PAPI_L2_LDM));
        assert(PAPI_OK == PAPI_add_event(event_set, PAPI_L3_TCM));
        //assert(PAPI_OK == PAPI_add_event(event_set, PAPI_TLB_DM));
        assert(PAPI_OK == PAPI_start(event_set));
        bzero(&base, sizeof(base));
#endif
        for (unsigned i = 0; i < sizeof(tests)/sizeof(tests[0]); i++) {
                if (tests[i][chunk_size] == NULL) continue;
                AUTO double * times = NULL;
#ifdef PAPI
                bzero(&now, sizeof(now));
#endif
                unsigned begin = search_begin, end = search_end;
                /* indice range is from end */
                {
                        if (begin != -1U)
                                begin = (1u<<chunk_size)-begin;
                        if (end != -1U)
                                end = (1u<<chunk_size)-end;
                        unsigned tmp = end;
                        end = begin;
                        begin = tmp;
                }
                times = bench_present(1u<<chunk_size,
                                      begin, end,
                                      repeat, subrun,
                                      tests[i][chunk_size]);
#ifdef PAPI
                assert(PAPI_OK == PAPI_accum(event_set, &now.values[0]));
#endif
                if (i == 0) {
                        baseline_time = HUGE_VAL;
                        for (unsigned j = 0; j < repeat; j++)
                                baseline_time = fmin(baseline_time, times[j]);
#ifdef PAPI
                        base = now;
                        fprintf(stderr, "Baseline: %f %lli %lli %lli %lli\n",
                                baseline_time,
                                base.values[0], base.values[1], 
                                base.values[2], base.values[3]);
#else
                        fprintf(stderr, "Baseline: %f\n",
                                baseline_time);
#endif
                } else {
                        for (unsigned j = 0; j < repeat; j++)
                                times[j] -= baseline_time;
                        print_doubles(times, repeat, names[i], 1u<<chunk_size);
#ifdef PAPI
                        fprintf(stderr, "%s", names[i]);
                        for (unsigned i = 0;
                             i < sizeof(now.values)/sizeof(now.values[0]);
                             i++) {
                                double x = 1.0*(now.values[i]-base.values[i])
                                        /(repeat*subrun);
                                if (x < 0) x = 0;
                                fprintf(stderr, " %.2f", x);
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
