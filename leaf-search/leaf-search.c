#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <math.h>
#include "cycle.h"
#ifdef PAPI
# include <papi.h>
#endif

#define likely(x) (__builtin_expect((x), 1))
#define unlikely(x) (__builtin_expect((x), 0))
#define ALWAYS_INLINE static inline __attribute__((always_inline)) 
#define AUTO __attribute__((cleanup(free_ptr)))

static void free_ptr (void * pptr)
{
        free(*(void**)pptr);
}

ALWAYS_INLINE size_t
linear_search (unsigned key, unsigned * vector, size_t size)
{
        for (unsigned i = size; i --> 0;) {
                unsigned v = vector[i];
                if (v <= key) return i;
        }

        return -1;
}

ALWAYS_INLINE size_t
fwd_search (unsigned key, unsigned * vector, size_t size)
{
        for (size_t i = 0; i < size; i++) {
                unsigned v = vector[i];
                if (v >= key)
                        return (v > key)?i-1:i;
        }

        return size-1;
}

ALWAYS_INLINE unsigned lb (unsigned long x)
{
        if (x <= 1) return 0;
        return (8*sizeof(unsigned long))-__builtin_clzl(x-1);
}

ALWAYS_INLINE size_t
binary_search (unsigned key, unsigned * vector, size_t size)
{
        unsigned * low = vector;

        /* if (vector[0] > key) return -1; */

        for (unsigned i = lb(size); i != 0; i--) {
                size /= 2;
                unsigned mid = low[size];
                if (mid <= key)
                        low += size;
        }
        
        return (*low > key)? -1: low - vector;
}

#define SEARCH(N)                                               \
size_t lsearch_##N(unsigned key, unsigned * vector)             \
{                                                               \
        return linear_search(key, vector, N);                   \
}                                                               \
                                                                \
size_t fsearch_##N(unsigned key, unsigned * vector)             \
{                                                               \
        return fwd_search(key, vector, N);                      \
}                                                               \
                                                                \
size_t bsearch_##N(unsigned key, unsigned * vector)             \
{                                                               \
        return binary_search(key, vector, N);                   \
}

SEARCH(1)
SEARCH(2)
SEARCH(4)
SEARCH(8)
SEARCH(16)
SEARCH(32)
SEARCH(64)

typedef int v4si __attribute__ ((vector_size (16)));
typedef float v4sf __attribute__ ((vector_size (16)));

#define PCMP(KEYS, VALS) (__builtin_ia32_movmskps                       \
                          ((v4sf)__builtin_ia32_pcmpgtd128((KEYS),      \
                                                           (VALS))))

#define TEST(I, VALS) do {                      \
                int mask = PCMP(keys, VALS)^0xf;    \
                if (mask)                       \
                        return 4*(I)+__builtin_ctz(mask);       \
        } while (0)

#define TEST2(I, V1, V2, BIT) do {                              \
                unsigned mask                                   \
                        = PCMP(keys, V1)                        \
                        | PCMP(keys,V2)<<4;                     \
                mask = (mask^0xff) | (BIT)<<7;                  \
                if (mask)                                       \
                        return 4*(I)+__builtin_ctz(mask);       \
        } while (0)

#define TEST4(I, V1, V2, V3, V4, BIT) do {                      \
                unsigned mask1                                  \
                        = PCMP(keys, V1)                        \
                        | PCMP(keys, V2)<<4;                    \
                unsigned mask2                                  \
                        = PCMP(keys, V3)                        \
                        | PCMP(keys, V4)<<4;                    \
                unsigned mask = mask1|mask2<<8;                 \
                mask = (mask^0xffff) | (BIT)<<15;               \
                if (mask)                                       \
                        return 4*(I)+__builtin_ctz(mask);       \
        } while (0)

size_t
vsearch_4 (unsigned key, unsigned * vector)
{
        if (vector[0] > key) return -1;
        v4si keys = {key, key, key, key};
        v4si vals = *(v4si*)vector;
        int mask = (PCMP(keys, vals)^0xf)|(1<<3);
        return __builtin_ctz(mask);
}

size_t
vsearch_8 (unsigned key, unsigned * vector)
{
        if (vector[0] > key) return -1;
        v4si keys = {key, key, key, key};
        v4si * vals = (v4si*)vector;
        v4si v0 = vals[0], v1 = vals[1];
        /* TEST(0, v0); */
        /* TEST(1, v1); */
        TEST2(0, v0, v1, 1);
}

size_t
vsearch_16 (unsigned key, unsigned * vector)
{
        if (vector[0] > key) return -1;
        v4si keys = {key, key, key, key};
        v4si * vals = (v4si*)vector;
        v4si v0 = vals[0], v1 = vals[1], v2 = vals[2], v3 = vals[3];
        /* TEST(0, v0); */
        /* TEST(1, v1); */
        /* TEST(2, v2); */
        /* TEST(3, v3); */
        /* return 15; */
        /* TEST2(0, v0, v1, 0); */
        /* TEST2(2, v2, v3, 1); */
        TEST4(0, v0, v1, v2, v3, 1);
}

size_t
vsearch_32 (unsigned key, unsigned * vector)
{
        if (vector[0] > key) return -1;
        v4si keys = {key, key, key, key};
        v4si * vals = (v4si*)vector;
        
        {
                v4si v0 = vals[0], v1 = vals[1],
                     v2 = vals[2], v3 = vals[3];
                /* TEST2(0, v0, v1, 0); */
                /* TEST2(2, v2, v3, 0); */
                TEST4(0, v0, v1, v2, v3, 0);
        }

        {
                v4si v0 = vals[4], v1 = vals[5],
                     v2 = vals[6], v3 = vals[7];
                /* TEST2(4, v0, v1, 0); */
                /* TEST2(6, v2, v3, 1); */
                TEST4(4, v0, v1, v2, v3, 1);
        }
}

size_t
vsearch_64 (unsigned key, unsigned * vector)
{
        if (vector[0] > key) return -1;
        v4si keys = {key, key, key, key};
        v4si * vals = (v4si*)vector;
        for (unsigned i = 0; i < 12; i+=4, vals += 4) {
                v4si v0 = vals[0], v1 = vals[1],
                     v2 = vals[2], v3 = vals[3];
                /* TEST2(i, v0, v1, 0); */
                /* TEST2(i+2, v2, v3, 0); */
                TEST4(i, v0, v1, v2, v3, 0);
        }
        {
                v4si v0 = vals[0], v1 = vals[1],
                     v2 = vals[2], v3 = vals[3];
                /* TEST2(12, v0, v1, 0); */
                /* TEST2(14, v2, v3, 1); */
                TEST4(12, v0, v1, v2, v3, 1);
        }
}

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
                          unsigned chunk_size, unsigned chunk_count)
{
        unsigned * needles;
        assert(!posix_memalign((void**)&needles, 1ul<<20,
                               (size_t)chunk_size
                               *chunk_count
                               *sizeof(unsigned)));
        for (size_t i = 0, idx = 0; i < chunk_count; i++) {
                for (unsigned key = key_low, j = 0; j < chunk_size;
                     key += stride, j++)
                        needles[idx++] = key;
        }

        return needles;
}

typedef size_t (*search_fun)(unsigned, unsigned*);
#ifdef PAPI
static int event_set;
#endif

double * bench_present (unsigned chunk_size,
                        unsigned search_begin, unsigned search_end,
                        unsigned repeat, unsigned subrun, unsigned skip,
                        search_fun search)
{
        srandom(12345);
        unsigned total_count = repeat*(subrun+skip);
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
                times[i] = elapsed(end, begin);
                if (skip) {
                        for (unsigned j = 0; j < skip; j++) {
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
        }

        return times;
}

double * bench_present_no_cache (unsigned chunk_size,
                                 unsigned search_begin, unsigned search_end,
                                 unsigned repeat, unsigned subrun,
                                 unsigned skip,
                                 search_fun search)
{
        srandom(12345);
        unsigned total_count = repeat*(subrun+skip);
        if (search_begin == -1U)
                search_begin = 0;
        if (search_end == -1U)
                search_end = chunk_size;
        AUTO unsigned * haystack = NULL;
        AUTO unsigned * keys = NULL;
        AUTO unsigned * offsets = NULL;
        {
                size_t total_size = 1ul<<30;
                size_t chunk_bytes = chunk_size*sizeof(unsigned);
                haystack = make_haystacks(0, 1, chunk_size,
                                          (1ul<<30)/chunk_bytes+1);
                keys = make_indices(search_begin, search_end,
                                    total_count);

                size_t stride = chunk_bytes*(((1ul<<12)/chunk_bytes)+1);
                size_t page_count = total_size/stride;
                size_t recycle = (repeat*subrun+page_count-1)/page_count;
                offsets = make_shuffled_indices(stride/sizeof(unsigned),
                                                page_count,
                                                recycle);
        }
        double * times = calloc(repeat, sizeof(double));

#ifdef PAPI
        assert(PAPI_OK == PAPI_reset(event_set));
#endif
        size_t idx = 0;
        for (unsigned i = 0; i < repeat; i++) {
                ticks begin;
                if (subrun <= 1) {
                        unsigned offset = offsets[idx];
                        unsigned key = keys[idx++];
                        begin = getticks();
                        size_t x = search(key, haystack+offset);
#ifdef CHECK
                        if (search != dummy)
                                assert(haystack[x+offset] == key);
#else
                        assert(x != -2U);
#endif
                } else {
                        begin = getticks();
                        for (unsigned j = 0; j < subrun; j++) {
                                unsigned offset = offsets[idx];
                                unsigned key = keys[idx++];
                                size_t x = search(key, haystack+offset);
#ifdef CHECK
                                if (search != dummy)
                                        assert(haystack[x+offset] == key);
#else
                                assert(x != -2U);
#endif
                        }
                }
                ticks end = getticks();
                times[i] = elapsed(end, begin);
                if (skip) {
                        for (unsigned j = 0; j < skip; j++) {
                                unsigned offset = offsets[idx];
                                unsigned key = keys[idx++];
                                size_t x = search(key, haystack+offset);
#ifdef CHECK
                                if (search != dummy)
                                        assert(haystack[x+offset] == key);
#else
                                assert(x != -2U);
#endif
                        }
                }
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

search_fun linear[] = 
{
        lsearch_1,
        lsearch_2,
        lsearch_4,
        lsearch_8,
        lsearch_16,
        lsearch_32,
        lsearch_64
};

search_fun forward[] = 
{
        fsearch_1,
        fsearch_2,
        fsearch_4,
        fsearch_8,
        fsearch_16,
        fsearch_32,
        fsearch_64
};

search_fun vector[] = 
{
        NULL,
        NULL,
        vsearch_4,
        vsearch_8,
        vsearch_16,
        vsearch_32,
        vsearch_64
};


search_fun binary[] = 
{
        bsearch_1,
        bsearch_2,
        bsearch_4,
        bsearch_8,
        bsearch_16,
        bsearch_32,
        bsearch_64
};

search_fun baseline[] = 
{
        dummy,
        dummy,
        dummy,
        dummy,
        dummy,
        dummy,
        dummy
};

#define ARG(N, VAR) do {                        \
        if (argc > (N))                         \
                VAR = atoi(argv[(N)]);          \
        } while (0)

int main (int argc, char ** argv)
{
        unsigned chunk_size = 5;
        unsigned repeat = 10000;
        unsigned subrun = 1;
        unsigned search_begin = -1U;
        unsigned search_end = -1U;
        int in_cache = 1;
        unsigned skip = 0;
        ARG(1, chunk_size);
        ARG(2, in_cache);
        ARG(3, search_begin);
        ARG(4, search_end);
        ARG(5, repeat);
        ARG(6, subrun);
        ARG(7, skip);

        assert(chunk_size <= 6);

        printf("Search\tSize\tTicks\n");
        search_fun*  tests[] = {baseline, forward, linear, vector, binary};
        const char * names[] = {"baseline", "lin", "inv", "vec", "bin"};

        double baseline_time = 0;
#ifdef PAPI
        struct { long long values[4]; } base, now;
        event_set = PAPI_NULL;
        assert(PAPI_VER_CURRENT == PAPI_library_init(PAPI_VER_CURRENT));
        assert(PAPI_OK == PAPI_create_eventset(&event_set));
        assert(PAPI_OK == PAPI_add_event(event_set, PAPI_TOT_CYC));
        assert(PAPI_OK == PAPI_add_event(event_set, PAPI_RES_STL));
        assert(PAPI_OK == PAPI_add_event(event_set, PAPI_BR_MSP));
        assert(PAPI_OK == PAPI_add_event(event_set, PAPI_L2_LDM));
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
                if ((tests[i] == linear) || (tests[i] == binary)) {
                        if (begin != -1U)
                                begin = (1u<<chunk_size)-begin;
                        if (end != -1U)
                                end = (1u<<chunk_size)-end;
                        unsigned tmp = end;
                        end = begin;
                        begin = tmp;
                }
                if (in_cache)
                        times = bench_present(1u<<chunk_size,
                                              begin, end,
                                              repeat, subrun, skip,
                                              tests[i][chunk_size]);
                else    times = bench_present_no_cache
                                (1u<<chunk_size,
                                 begin, end,
                                 repeat, subrun, skip,
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
