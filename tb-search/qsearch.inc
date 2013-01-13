size_t q_3 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(2u, 4u, 6u);        /*          8 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t q_4 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(4u, 8u, 12u);       /*         16 -> 4          */
	QUATERNARY(1u, 2u, 3u);        /*          4 -> 1          */
	return base-vector;
}

size_t q_5 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(8u, 16u, 24u);      /*         32 -> 8          */
	QUATERNARY(2u, 4u, 6u);        /*          8 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t q_6 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(16u, 32u, 48u);     /*         64 -> 16         */
	QUATERNARY(4u, 8u, 12u);       /*         16 -> 4          */
	QUATERNARY(1u, 2u, 3u);        /*          4 -> 1          */
	return base-vector;
}

size_t q_7 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(32u, 64u, 96u);     /*        128 -> 32         */
	QUATERNARY(8u, 16u, 24u);      /*         32 -> 8          */
	QUATERNARY(2u, 4u, 6u);        /*          8 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t q_8 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(64u, 128u, 192u);   /*        256 -> 64         */
	QUATERNARY(16u, 32u, 48u);     /*         64 -> 16         */
	QUATERNARY(4u, 8u, 12u);       /*         16 -> 4          */
	QUATERNARY(1u, 2u, 3u);        /*          4 -> 1          */
	return base-vector;
}

size_t q_9 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(128u, 256u, 384u);  /*        512 -> 128        */
	QUATERNARY(32u, 64u, 96u);     /*        128 -> 32         */
	QUATERNARY(8u, 16u, 24u);      /*         32 -> 8          */
	QUATERNARY(2u, 4u, 6u);        /*          8 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t q_10 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(256u, 512u, 768u);  /*       1024 -> 256        */
	QUATERNARY(64u, 128u, 192u);   /*        256 -> 64         */
	QUATERNARY(16u, 32u, 48u);     /*         64 -> 16         */
	QUATERNARY(4u, 8u, 12u);       /*         16 -> 4          */
	QUATERNARY(1u, 2u, 3u);        /*          4 -> 1          */
	return base-vector;
}

size_t q_11 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(512u, 1024u, 1536u); /*       2048 -> 512        */
	QUATERNARY(128u, 256u, 384u);  /*        512 -> 128        */
	QUATERNARY(32u, 64u, 96u);     /*        128 -> 32         */
	QUATERNARY(8u, 16u, 24u);      /*         32 -> 8          */
	QUATERNARY(2u, 4u, 6u);        /*          8 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t q_12 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(1024u, 2048u, 3072u); /*       4096 -> 1024       */
	QUATERNARY(256u, 512u, 768u);  /*       1024 -> 256        */
	QUATERNARY(64u, 128u, 192u);   /*        256 -> 64         */
	QUATERNARY(16u, 32u, 48u);     /*         64 -> 16         */
	QUATERNARY(4u, 8u, 12u);       /*         16 -> 4          */
	QUATERNARY(1u, 2u, 3u);        /*          4 -> 1          */
	return base-vector;
}

size_t q_13 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(2048u, 4096u, 6144u); /*       8192 -> 2048       */
	QUATERNARY(512u, 1024u, 1536u); /*       2048 -> 512        */
	QUATERNARY(128u, 256u, 384u);  /*        512 -> 128        */
	QUATERNARY(32u, 64u, 96u);     /*        128 -> 32         */
	QUATERNARY(8u, 16u, 24u);      /*         32 -> 8          */
	QUATERNARY(2u, 4u, 6u);        /*          8 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t q_14 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(4096u, 8192u, 12288u); /*      16384 -> 4096       */
	QUATERNARY(1024u, 2048u, 3072u); /*       4096 -> 1024       */
	QUATERNARY(256u, 512u, 768u);  /*       1024 -> 256        */
	QUATERNARY(64u, 128u, 192u);   /*        256 -> 64         */
	QUATERNARY(16u, 32u, 48u);     /*         64 -> 16         */
	QUATERNARY(4u, 8u, 12u);       /*         16 -> 4          */
	QUATERNARY(1u, 2u, 3u);        /*          4 -> 1          */
	return base-vector;
}

size_t q_15 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(8192u, 16384u, 24576u); /*      32768 -> 8192       */
	QUATERNARY(2048u, 4096u, 6144u); /*       8192 -> 2048       */
	QUATERNARY(512u, 1024u, 1536u); /*       2048 -> 512        */
	QUATERNARY(128u, 256u, 384u);  /*        512 -> 128        */
	QUATERNARY(32u, 64u, 96u);     /*        128 -> 32         */
	QUATERNARY(8u, 16u, 24u);      /*         32 -> 8          */
	QUATERNARY(2u, 4u, 6u);        /*          8 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t q_16 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(16384u, 32768u, 49152u); /*      65536 -> 16384      */
	QUATERNARY(4096u, 8192u, 12288u); /*      16384 -> 4096       */
	QUATERNARY(1024u, 2048u, 3072u); /*       4096 -> 1024       */
	QUATERNARY(256u, 512u, 768u);  /*       1024 -> 256        */
	QUATERNARY(64u, 128u, 192u);   /*        256 -> 64         */
	QUATERNARY(16u, 32u, 48u);     /*         64 -> 16         */
	QUATERNARY(4u, 8u, 12u);       /*         16 -> 4          */
	QUATERNARY(1u, 2u, 3u);        /*          4 -> 1          */
	return base-vector;
}

size_t q_17 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(32768u, 65536u, 98304u); /*     131072 -> 32768      */
	QUATERNARY(8192u, 16384u, 24576u); /*      32768 -> 8192       */
	QUATERNARY(2048u, 4096u, 6144u); /*       8192 -> 2048       */
	QUATERNARY(512u, 1024u, 1536u); /*       2048 -> 512        */
	QUATERNARY(128u, 256u, 384u);  /*        512 -> 128        */
	QUATERNARY(32u, 64u, 96u);     /*        128 -> 32         */
	QUATERNARY(8u, 16u, 24u);      /*         32 -> 8          */
	QUATERNARY(2u, 4u, 6u);        /*          8 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t q_18 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(65536u, 131072u, 196608u); /*     262144 -> 65536      */
	QUATERNARY(16384u, 32768u, 49152u); /*      65536 -> 16384      */
	QUATERNARY(4096u, 8192u, 12288u); /*      16384 -> 4096       */
	QUATERNARY(1024u, 2048u, 3072u); /*       4096 -> 1024       */
	QUATERNARY(256u, 512u, 768u);  /*       1024 -> 256        */
	QUATERNARY(64u, 128u, 192u);   /*        256 -> 64         */
	QUATERNARY(16u, 32u, 48u);     /*         64 -> 16         */
	QUATERNARY(4u, 8u, 12u);       /*         16 -> 4          */
	QUATERNARY(1u, 2u, 3u);        /*          4 -> 1          */
	return base-vector;
}

size_t q_19 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(131072u, 262144u, 393216u); /*     524288 -> 131072     */
	QUATERNARY(32768u, 65536u, 98304u); /*     131072 -> 32768      */
	QUATERNARY(8192u, 16384u, 24576u); /*      32768 -> 8192       */
	QUATERNARY(2048u, 4096u, 6144u); /*       8192 -> 2048       */
	QUATERNARY(512u, 1024u, 1536u); /*       2048 -> 512        */
	QUATERNARY(128u, 256u, 384u);  /*        512 -> 128        */
	QUATERNARY(32u, 64u, 96u);     /*        128 -> 32         */
	QUATERNARY(8u, 16u, 24u);      /*         32 -> 8          */
	QUATERNARY(2u, 4u, 6u);        /*          8 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t q_20 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(262144u, 524288u, 786432u); /*    1048576 -> 262144     */
	QUATERNARY(65536u, 131072u, 196608u); /*     262144 -> 65536      */
	QUATERNARY(16384u, 32768u, 49152u); /*      65536 -> 16384      */
	QUATERNARY(4096u, 8192u, 12288u); /*      16384 -> 4096       */
	QUATERNARY(1024u, 2048u, 3072u); /*       4096 -> 1024       */
	QUATERNARY(256u, 512u, 768u);  /*       1024 -> 256        */
	QUATERNARY(64u, 128u, 192u);   /*        256 -> 64         */
	QUATERNARY(16u, 32u, 48u);     /*         64 -> 16         */
	QUATERNARY(4u, 8u, 12u);       /*         16 -> 4          */
	QUATERNARY(1u, 2u, 3u);        /*          4 -> 1          */
	return base-vector;
}

size_t q_21 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(524288u, 1048576u, 1572864u); /*    2097152 -> 524288     */
	QUATERNARY(131072u, 262144u, 393216u); /*     524288 -> 131072     */
	QUATERNARY(32768u, 65536u, 98304u); /*     131072 -> 32768      */
	QUATERNARY(8192u, 16384u, 24576u); /*      32768 -> 8192       */
	QUATERNARY(2048u, 4096u, 6144u); /*       8192 -> 2048       */
	QUATERNARY(512u, 1024u, 1536u); /*       2048 -> 512        */
	QUATERNARY(128u, 256u, 384u);  /*        512 -> 128        */
	QUATERNARY(32u, 64u, 96u);     /*        128 -> 32         */
	QUATERNARY(8u, 16u, 24u);      /*         32 -> 8          */
	QUATERNARY(2u, 4u, 6u);        /*          8 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t q_22 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(1048576u, 2097152u, 3145728u); /*    4194304 -> 1048576    */
	QUATERNARY(262144u, 524288u, 786432u); /*    1048576 -> 262144     */
	QUATERNARY(65536u, 131072u, 196608u); /*     262144 -> 65536      */
	QUATERNARY(16384u, 32768u, 49152u); /*      65536 -> 16384      */
	QUATERNARY(4096u, 8192u, 12288u); /*      16384 -> 4096       */
	QUATERNARY(1024u, 2048u, 3072u); /*       4096 -> 1024       */
	QUATERNARY(256u, 512u, 768u);  /*       1024 -> 256        */
	QUATERNARY(64u, 128u, 192u);   /*        256 -> 64         */
	QUATERNARY(16u, 32u, 48u);     /*         64 -> 16         */
	QUATERNARY(4u, 8u, 12u);       /*         16 -> 4          */
	QUATERNARY(1u, 2u, 3u);        /*          4 -> 1          */
	return base-vector;
}

size_t q_23 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(2097152u, 4194304u, 6291456u); /*    8388608 -> 2097152    */
	QUATERNARY(524288u, 1048576u, 1572864u); /*    2097152 -> 524288     */
	QUATERNARY(131072u, 262144u, 393216u); /*     524288 -> 131072     */
	QUATERNARY(32768u, 65536u, 98304u); /*     131072 -> 32768      */
	QUATERNARY(8192u, 16384u, 24576u); /*      32768 -> 8192       */
	QUATERNARY(2048u, 4096u, 6144u); /*       8192 -> 2048       */
	QUATERNARY(512u, 1024u, 1536u); /*       2048 -> 512        */
	QUATERNARY(128u, 256u, 384u);  /*        512 -> 128        */
	QUATERNARY(32u, 64u, 96u);     /*        128 -> 32         */
	QUATERNARY(8u, 16u, 24u);      /*         32 -> 8          */
	QUATERNARY(2u, 4u, 6u);        /*          8 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t q_24 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(4194304u, 8388608u, 12582912u); /*   16777216 -> 4194304    */
	QUATERNARY(1048576u, 2097152u, 3145728u); /*    4194304 -> 1048576    */
	QUATERNARY(262144u, 524288u, 786432u); /*    1048576 -> 262144     */
	QUATERNARY(65536u, 131072u, 196608u); /*     262144 -> 65536      */
	QUATERNARY(16384u, 32768u, 49152u); /*      65536 -> 16384      */
	QUATERNARY(4096u, 8192u, 12288u); /*      16384 -> 4096       */
	QUATERNARY(1024u, 2048u, 3072u); /*       4096 -> 1024       */
	QUATERNARY(256u, 512u, 768u);  /*       1024 -> 256        */
	QUATERNARY(64u, 128u, 192u);   /*        256 -> 64         */
	QUATERNARY(16u, 32u, 48u);     /*         64 -> 16         */
	QUATERNARY(4u, 8u, 12u);       /*         16 -> 4          */
	QUATERNARY(1u, 2u, 3u);        /*          4 -> 1          */
	return base-vector;
}

size_t q_25 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(8388608u, 16777216u, 25165824u); /*   33554432 -> 8388608    */
	QUATERNARY(2097152u, 4194304u, 6291456u); /*    8388608 -> 2097152    */
	QUATERNARY(524288u, 1048576u, 1572864u); /*    2097152 -> 524288     */
	QUATERNARY(131072u, 262144u, 393216u); /*     524288 -> 131072     */
	QUATERNARY(32768u, 65536u, 98304u); /*     131072 -> 32768      */
	QUATERNARY(8192u, 16384u, 24576u); /*      32768 -> 8192       */
	QUATERNARY(2048u, 4096u, 6144u); /*       8192 -> 2048       */
	QUATERNARY(512u, 1024u, 1536u); /*       2048 -> 512        */
	QUATERNARY(128u, 256u, 384u);  /*        512 -> 128        */
	QUATERNARY(32u, 64u, 96u);     /*        128 -> 32         */
	QUATERNARY(8u, 16u, 24u);      /*         32 -> 8          */
	QUATERNARY(2u, 4u, 6u);        /*          8 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t q_26 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(16777216u, 33554432u, 50331648u); /*   67108864 -> 16777216   */
	QUATERNARY(4194304u, 8388608u, 12582912u); /*   16777216 -> 4194304    */
	QUATERNARY(1048576u, 2097152u, 3145728u); /*    4194304 -> 1048576    */
	QUATERNARY(262144u, 524288u, 786432u); /*    1048576 -> 262144     */
	QUATERNARY(65536u, 131072u, 196608u); /*     262144 -> 65536      */
	QUATERNARY(16384u, 32768u, 49152u); /*      65536 -> 16384      */
	QUATERNARY(4096u, 8192u, 12288u); /*      16384 -> 4096       */
	QUATERNARY(1024u, 2048u, 3072u); /*       4096 -> 1024       */
	QUATERNARY(256u, 512u, 768u);  /*       1024 -> 256        */
	QUATERNARY(64u, 128u, 192u);   /*        256 -> 64         */
	QUATERNARY(16u, 32u, 48u);     /*         64 -> 16         */
	QUATERNARY(4u, 8u, 12u);       /*         16 -> 4          */
	QUATERNARY(1u, 2u, 3u);        /*          4 -> 1          */
	return base-vector;
}

size_t q_27 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(33554432u, 67108864u, 100663296u); /*  134217728 -> 33554432   */
	QUATERNARY(8388608u, 16777216u, 25165824u); /*   33554432 -> 8388608    */
	QUATERNARY(2097152u, 4194304u, 6291456u); /*    8388608 -> 2097152    */
	QUATERNARY(524288u, 1048576u, 1572864u); /*    2097152 -> 524288     */
	QUATERNARY(131072u, 262144u, 393216u); /*     524288 -> 131072     */
	QUATERNARY(32768u, 65536u, 98304u); /*     131072 -> 32768      */
	QUATERNARY(8192u, 16384u, 24576u); /*      32768 -> 8192       */
	QUATERNARY(2048u, 4096u, 6144u); /*       8192 -> 2048       */
	QUATERNARY(512u, 1024u, 1536u); /*       2048 -> 512        */
	QUATERNARY(128u, 256u, 384u);  /*        512 -> 128        */
	QUATERNARY(32u, 64u, 96u);     /*        128 -> 32         */
	QUATERNARY(8u, 16u, 24u);      /*         32 -> 8          */
	QUATERNARY(2u, 4u, 6u);        /*          8 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t q_28 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(67108864u, 134217728u, 201326592u); /*  268435456 -> 67108864   */
	QUATERNARY(16777216u, 33554432u, 50331648u); /*   67108864 -> 16777216   */
	QUATERNARY(4194304u, 8388608u, 12582912u); /*   16777216 -> 4194304    */
	QUATERNARY(1048576u, 2097152u, 3145728u); /*    4194304 -> 1048576    */
	QUATERNARY(262144u, 524288u, 786432u); /*    1048576 -> 262144     */
	QUATERNARY(65536u, 131072u, 196608u); /*     262144 -> 65536      */
	QUATERNARY(16384u, 32768u, 49152u); /*      65536 -> 16384      */
	QUATERNARY(4096u, 8192u, 12288u); /*      16384 -> 4096       */
	QUATERNARY(1024u, 2048u, 3072u); /*       4096 -> 1024       */
	QUATERNARY(256u, 512u, 768u);  /*       1024 -> 256        */
	QUATERNARY(64u, 128u, 192u);   /*        256 -> 64         */
	QUATERNARY(16u, 32u, 48u);     /*         64 -> 16         */
	QUATERNARY(4u, 8u, 12u);       /*         16 -> 4          */
	QUATERNARY(1u, 2u, 3u);        /*          4 -> 1          */
	return base-vector;
}

size_t q_29 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(134217728u, 268435456u, 402653184u); /*  536870912 -> 134217728  */
	QUATERNARY(33554432u, 67108864u, 100663296u); /*  134217728 -> 33554432   */
	QUATERNARY(8388608u, 16777216u, 25165824u); /*   33554432 -> 8388608    */
	QUATERNARY(2097152u, 4194304u, 6291456u); /*    8388608 -> 2097152    */
	QUATERNARY(524288u, 1048576u, 1572864u); /*    2097152 -> 524288     */
	QUATERNARY(131072u, 262144u, 393216u); /*     524288 -> 131072     */
	QUATERNARY(32768u, 65536u, 98304u); /*     131072 -> 32768      */
	QUATERNARY(8192u, 16384u, 24576u); /*      32768 -> 8192       */
	QUATERNARY(2048u, 4096u, 6144u); /*       8192 -> 2048       */
	QUATERNARY(512u, 1024u, 1536u); /*       2048 -> 512        */
	QUATERNARY(128u, 256u, 384u);  /*        512 -> 128        */
	QUATERNARY(32u, 64u, 96u);     /*        128 -> 32         */
	QUATERNARY(8u, 16u, 24u);      /*         32 -> 8          */
	QUATERNARY(2u, 4u, 6u);        /*          8 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t q_30 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(268435456u, 536870912u, 805306368u); /* 1073741824 -> 268435456  */
	QUATERNARY(67108864u, 134217728u, 201326592u); /*  268435456 -> 67108864   */
	QUATERNARY(16777216u, 33554432u, 50331648u); /*   67108864 -> 16777216   */
	QUATERNARY(4194304u, 8388608u, 12582912u); /*   16777216 -> 4194304    */
	QUATERNARY(1048576u, 2097152u, 3145728u); /*    4194304 -> 1048576    */
	QUATERNARY(262144u, 524288u, 786432u); /*    1048576 -> 262144     */
	QUATERNARY(65536u, 131072u, 196608u); /*     262144 -> 65536      */
	QUATERNARY(16384u, 32768u, 49152u); /*      65536 -> 16384      */
	QUATERNARY(4096u, 8192u, 12288u); /*      16384 -> 4096       */
	QUATERNARY(1024u, 2048u, 3072u); /*       4096 -> 1024       */
	QUATERNARY(256u, 512u, 768u);  /*       1024 -> 256        */
	QUATERNARY(64u, 128u, 192u);   /*        256 -> 64         */
	QUATERNARY(16u, 32u, 48u);     /*         64 -> 16         */
	QUATERNARY(4u, 8u, 12u);       /*         16 -> 4          */
	QUATERNARY(1u, 2u, 3u);        /*          4 -> 1          */
	return base-vector;
}

search_fun 
quat[] = {NULL, NULL, NULL, q_3, q_4, q_5, q_6, q_7, q_8, q_9, q_10, q_11, q_12, q_13, q_14, q_15, q_16, q_17, q_18, q_19, q_20, q_21, q_22, q_23, q_24, q_25, q_26, q_27, q_28, q_29, q_30};
