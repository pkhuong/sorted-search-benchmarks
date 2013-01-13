size_t o_3 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(3u);                    /*          8 -> 5          */
	BINARY(2u);                    /*          5 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_4 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(7u);                    /*         16 -> 9          */
	BINARY(4u);                    /*          9 -> 5          */
	BINARY(2u);                    /*          5 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_5 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(15u);                   /*         32 -> 17         */
	BINARY(8u);                    /*         17 -> 9          */
	BINARY(4u);                    /*          9 -> 5          */
	BINARY(2u);                    /*          5 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_6 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(31u);                   /*         64 -> 33         */
	BINARY(16u);                   /*         33 -> 17         */
	BINARY(8u);                    /*         17 -> 9          */
	BINARY(4u);                    /*          9 -> 5          */
	BINARY(2u);                    /*          5 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_7 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(62u);                   /*        128 -> 66         */
	BINARY(32u);                   /*         66 -> 34         */
	BINARY(16u);                   /*         34 -> 18         */
	BINARY(8u);                    /*         18 -> 10         */
	BINARY(4u);                    /*         10 -> 6          */
	BINARY(2u);                    /*          6 -> 4          */
	BINARY(1u);                    /*          4 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_8 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(125u);                  /*        256 -> 131        */
	BINARY(64u);                   /*        131 -> 67         */
	BINARY(32u);                   /*         67 -> 35         */
	BINARY(17u);                   /*         35 -> 18         */
	BINARY(8u);                    /*         18 -> 10         */
	BINARY(4u);                    /*         10 -> 6          */
	BINARY(2u);                    /*          6 -> 4          */
	BINARY(1u);                    /*          4 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_9 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(251u);                  /*        512 -> 261        */
	BINARY(128u);                  /*        261 -> 133        */
	BINARY(65u);                   /*        133 -> 68         */
	BINARY(33u);                   /*         68 -> 35         */
	BINARY(17u);                   /*         35 -> 18         */
	BINARY(8u);                    /*         18 -> 10         */
	BINARY(4u);                    /*         10 -> 6          */
	BINARY(2u);                    /*          6 -> 4          */
	BINARY(1u);                    /*          4 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_10 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(503u);                  /*       1024 -> 521        */
	BINARY(256u);                  /*        521 -> 265        */
	BINARY(130u);                  /*        265 -> 135        */
	BINARY(66u);                   /*        135 -> 69         */
	BINARY(33u);                   /*         69 -> 36         */
	BINARY(17u);                   /*         36 -> 19         */
	BINARY(9u);                    /*         19 -> 10         */
	BINARY(4u);                    /*         10 -> 6          */
	BINARY(2u);                    /*          6 -> 4          */
	BINARY(1u);                    /*          4 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_11 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(1007u);                 /*       2048 -> 1041       */
	BINARY(512u);                  /*       1041 -> 529        */
	BINARY(260u);                  /*        529 -> 269        */
	BINARY(132u);                  /*        269 -> 137        */
	BINARY(67u);                   /*        137 -> 70         */
	BINARY(34u);                   /*         70 -> 36         */
	BINARY(17u);                   /*         36 -> 19         */
	BINARY(9u);                    /*         19 -> 10         */
	BINARY(4u);                    /*         10 -> 6          */
	BINARY(2u);                    /*          6 -> 4          */
	BINARY(1u);                    /*          4 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_12 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(2015u);                 /*       4096 -> 2081       */
	BINARY(1023u);                 /*       2081 -> 1058       */
	BINARY(520u);                  /*       1058 -> 538        */
	BINARY(264u);                  /*        538 -> 274        */
	BINARY(134u);                  /*        274 -> 140        */
	BINARY(68u);                   /*        140 -> 72         */
	BINARY(35u);                   /*         72 -> 37         */
	BINARY(18u);                   /*         37 -> 19         */
	BINARY(9u);                    /*         19 -> 10         */
	BINARY(4u);                    /*         10 -> 6          */
	BINARY(2u);                    /*          6 -> 4          */
	BINARY(1u);                    /*          4 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_13 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(4030u);                 /*       8192 -> 4162       */
	BINARY(2047u);                 /*       4162 -> 2115       */
	BINARY(1040u);                 /*       2115 -> 1075       */
	BINARY(528u);                  /*       1075 -> 547        */
	BINARY(269u);                  /*        547 -> 278        */
	BINARY(136u);                  /*        278 -> 142        */
	BINARY(69u);                   /*        142 -> 73         */
	BINARY(35u);                   /*         73 -> 38         */
	BINARY(18u);                   /*         38 -> 20         */
	BINARY(9u);                    /*         20 -> 11         */
	BINARY(5u);                    /*         11 -> 6          */
	BINARY(2u);                    /*          6 -> 4          */
	BINARY(1u);                    /*          4 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_14 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(8061u);                 /*      16384 -> 8323       */
	BINARY(4095u);                 /*       8323 -> 4228       */
	BINARY(2080u);                 /*       4228 -> 2148       */
	BINARY(1056u);                 /*       2148 -> 1092       */
	BINARY(537u);                  /*       1092 -> 555        */
	BINARY(273u);                  /*        555 -> 282        */
	BINARY(138u);                  /*        282 -> 144        */
	BINARY(70u);                   /*        144 -> 74         */
	BINARY(36u);                   /*         74 -> 38         */
	BINARY(18u);                   /*         38 -> 20         */
	BINARY(9u);                    /*         20 -> 11         */
	BINARY(5u);                    /*         11 -> 6          */
	BINARY(2u);                    /*          6 -> 4          */
	BINARY(1u);                    /*          4 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_15 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(16123u);                /*      32768 -> 16645      */
	BINARY(8190u);                 /*      16645 -> 8455       */
	BINARY(4160u);                 /*       8455 -> 4295       */
	BINARY(2113u);                 /*       4295 -> 2182       */
	BINARY(1073u);                 /*       2182 -> 1109       */
	BINARY(545u);                  /*       1109 -> 564        */
	BINARY(277u);                  /*        564 -> 287        */
	BINARY(141u);                  /*        287 -> 146        */
	BINARY(71u);                   /*        146 -> 75         */
	BINARY(36u);                   /*         75 -> 39         */
	BINARY(19u);                   /*         39 -> 20         */
	BINARY(9u);                    /*         20 -> 11         */
	BINARY(5u);                    /*         11 -> 6          */
	BINARY(2u);                    /*          6 -> 4          */
	BINARY(1u);                    /*          4 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_16 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(32247u);                /*      65536 -> 33289      */
	BINARY(16380u);                /*      33289 -> 16909      */
	BINARY(8320u);                 /*      16909 -> 8589       */
	BINARY(4226u);                 /*       8589 -> 4363       */
	BINARY(2146u);                 /*       4363 -> 2217       */
	BINARY(1090u);                 /*       2217 -> 1127       */
	BINARY(554u);                  /*       1127 -> 573        */
	BINARY(281u);                  /*        573 -> 292        */
	BINARY(143u);                  /*        292 -> 149        */
	BINARY(73u);                   /*        149 -> 76         */
	BINARY(37u);                   /*         76 -> 39         */
	BINARY(19u);                   /*         39 -> 20         */
	BINARY(9u);                    /*         20 -> 11         */
	BINARY(5u);                    /*         11 -> 6          */
	BINARY(2u);                    /*          6 -> 4          */
	BINARY(1u);                    /*          4 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_17 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(64495u);                /*     131072 -> 66577      */
	BINARY(32760u);                /*      66577 -> 33817      */
	BINARY(16640u);                /*      33817 -> 17177      */
	BINARY(8452u);                 /*      17177 -> 8725       */
	BINARY(4293u);                 /*       8725 -> 4432       */
	BINARY(2180u);                 /*       4432 -> 2252       */
	BINARY(1108u);                 /*       2252 -> 1144       */
	BINARY(562u);                  /*       1144 -> 582        */
	BINARY(286u);                  /*        582 -> 296        */
	BINARY(145u);                  /*        296 -> 151        */
	BINARY(74u);                   /*        151 -> 77         */
	BINARY(37u);                   /*         77 -> 40         */
	BINARY(19u);                   /*         40 -> 21         */
	BINARY(10u);                   /*         21 -> 11         */
	BINARY(5u);                    /*         11 -> 6          */
	BINARY(2u);                    /*          6 -> 4          */
	BINARY(1u);                    /*          4 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_18 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(128991u);               /*     262144 -> 133153     */
	BINARY(65519u);                /*     133153 -> 67634      */
	BINARY(33280u);                /*      67634 -> 34354      */
	BINARY(16904u);                /*      34354 -> 17450      */
	BINARY(8586u);                 /*      17450 -> 8864       */
	BINARY(4361u);                 /*       8864 -> 4503       */
	BINARY(2215u);                 /*       4503 -> 2288       */
	BINARY(1125u);                 /*       2288 -> 1163       */
	BINARY(572u);                  /*       1163 -> 591        */
	BINARY(290u);                  /*        591 -> 301        */
	BINARY(148u);                  /*        301 -> 153        */
	BINARY(75u);                   /*        153 -> 78         */
	BINARY(38u);                   /*         78 -> 40         */
	BINARY(19u);                   /*         40 -> 21         */
	BINARY(10u);                   /*         21 -> 11         */
	BINARY(5u);                    /*         11 -> 6          */
	BINARY(2u);                    /*          6 -> 4          */
	BINARY(1u);                    /*          4 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_19 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(257982u);               /*     524288 -> 266306     */
	BINARY(131039u);               /*     266306 -> 135267     */
	BINARY(66559u);                /*     135267 -> 68708      */
	BINARY(33808u);                /*      68708 -> 34900      */
	BINARY(17173u);                /*      34900 -> 17727      */
	BINARY(8722u);                 /*      17727 -> 9005       */
	BINARY(4431u);                 /*       9005 -> 4574       */
	BINARY(2250u);                 /*       4574 -> 2324       */
	BINARY(1143u);                 /*       2324 -> 1181       */
	BINARY(581u);                  /*       1181 -> 600        */
	BINARY(295u);                  /*        600 -> 305        */
	BINARY(150u);                  /*        305 -> 155        */
	BINARY(76u);                   /*        155 -> 79         */
	BINARY(38u);                   /*         79 -> 41         */
	BINARY(20u);                   /*         41 -> 21         */
	BINARY(10u);                   /*         21 -> 11         */
	BINARY(5u);                    /*         11 -> 6          */
	BINARY(2u);                    /*          6 -> 4          */
	BINARY(1u);                    /*          4 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_20 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(515965u);               /*    1048576 -> 532611     */
	BINARY(262078u);               /*     532611 -> 270533     */
	BINARY(133119u);               /*     270533 -> 137414     */
	BINARY(67616u);                /*     137414 -> 69798      */
	BINARY(34345u);                /*      69798 -> 35453      */
	BINARY(17445u);                /*      35453 -> 18008      */
	BINARY(8861u);                 /*      18008 -> 9147       */
	BINARY(4500u);                 /*       9147 -> 4647       */
	BINARY(2286u);                 /*       4647 -> 2361       */
	BINARY(1161u);                 /*       2361 -> 1200       */
	BINARY(590u);                  /*       1200 -> 610        */
	BINARY(300u);                  /*        610 -> 310        */
	BINARY(152u);                  /*        310 -> 158        */
	BINARY(77u);                   /*        158 -> 81         */
	BINARY(39u);                   /*         81 -> 42         */
	BINARY(20u);                   /*         42 -> 22         */
	BINARY(10u);                   /*         22 -> 12         */
	BINARY(5u);                    /*         12 -> 7          */
	BINARY(3u);                    /*          7 -> 4          */
	BINARY(1u);                    /*          4 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_21 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(1031931u);              /*    2097152 -> 1065221    */
	BINARY(524156u);               /*    1065221 -> 541065     */
	BINARY(266238u);               /*     541065 -> 274827     */
	BINARY(135232u);               /*     274827 -> 139595     */
	BINARY(68689u);                /*     139595 -> 70906      */
	BINARY(34890u);                /*      70906 -> 36016      */
	BINARY(17722u);                /*      36016 -> 18294      */
	BINARY(9001u);                 /*      18294 -> 9293       */
	BINARY(4572u);                 /*       9293 -> 4721       */
	BINARY(2323u);                 /*       4721 -> 2398       */
	BINARY(1179u);                 /*       2398 -> 1219       */
	BINARY(599u);                  /*       1219 -> 620        */
	BINARY(305u);                  /*        620 -> 315        */
	BINARY(155u);                  /*        315 -> 160        */
	BINARY(78u);                   /*        160 -> 82         */
	BINARY(40u);                   /*         82 -> 42         */
	BINARY(20u);                   /*         42 -> 22         */
	BINARY(10u);                   /*         22 -> 12         */
	BINARY(5u);                    /*         12 -> 7          */
	BINARY(3u);                    /*          7 -> 4          */
	BINARY(1u);                    /*          4 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_22 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(2063863u);              /*    4194304 -> 2130441    */
	BINARY(1048312u);              /*    2130441 -> 1082129    */
	BINARY(532476u);               /*    1082129 -> 549653     */
	BINARY(270464u);               /*     549653 -> 279189     */
	BINARY(137378u);               /*     279189 -> 141811     */
	BINARY(69780u);                /*     141811 -> 72031      */
	BINARY(35443u);                /*      72031 -> 36588      */
	BINARY(18003u);                /*      36588 -> 18585      */
	BINARY(9145u);                 /*      18585 -> 9440       */
	BINARY(4645u);                 /*       9440 -> 4795       */
	BINARY(2359u);                 /*       4795 -> 2436       */
	BINARY(1198u);                 /*       2436 -> 1238       */
	BINARY(609u);                  /*       1238 -> 629        */
	BINARY(309u);                  /*        629 -> 320        */
	BINARY(157u);                  /*        320 -> 163        */
	BINARY(80u);                   /*        163 -> 83         */
	BINARY(40u);                   /*         83 -> 43         */
	BINARY(21u);                   /*         43 -> 22         */
	BINARY(10u);                   /*         22 -> 12         */
	BINARY(5u);                    /*         12 -> 7          */
	BINARY(3u);                    /*          7 -> 4          */
	BINARY(1u);                    /*          4 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_23 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(4127727u);              /*    8388608 -> 4260881    */
	BINARY(2096623u);              /*    4260881 -> 2164258    */
	BINARY(1064952u);              /*    2164258 -> 1099306    */
	BINARY(540928u);               /*    1099306 -> 558378     */
	BINARY(274757u);               /*     558378 -> 283621     */
	BINARY(139559u);               /*     283621 -> 144062     */
	BINARY(70887u);                /*     144062 -> 73175      */
	BINARY(36006u);                /*      73175 -> 37169      */
	BINARY(18289u);                /*      37169 -> 18880      */
	BINARY(9290u);                 /*      18880 -> 9590       */
	BINARY(4718u);                 /*       9590 -> 4872       */
	BINARY(2397u);                 /*       4872 -> 2475       */
	BINARY(1217u);                 /*       2475 -> 1258       */
	BINARY(619u);                  /*       1258 -> 639        */
	BINARY(314u);                  /*        639 -> 325        */
	BINARY(159u);                  /*        325 -> 166        */
	BINARY(81u);                   /*        166 -> 85         */
	BINARY(41u);                   /*         85 -> 44         */
	BINARY(21u);                   /*         44 -> 23         */
	BINARY(11u);                   /*         23 -> 12         */
	BINARY(5u);                    /*         12 -> 7          */
	BINARY(3u);                    /*          7 -> 4          */
	BINARY(1u);                    /*          4 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_24 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(8255455u);              /*   16777216 -> 8521761    */
	BINARY(4193247u);              /*    8521761 -> 4328514    */
	BINARY(2129903u);              /*    4328514 -> 2198611    */
	BINARY(1081856u);              /*    2198611 -> 1116755    */
	BINARY(549514u);               /*    1116755 -> 567241     */
	BINARY(279118u);               /*     567241 -> 288123     */
	BINARY(141774u);               /*     288123 -> 146349     */
	BINARY(72013u);                /*     146349 -> 74336      */
	BINARY(36578u);                /*      74336 -> 37758      */
	BINARY(18579u);                /*      37758 -> 19179      */
	BINARY(9437u);                 /*      19179 -> 9742       */
	BINARY(4793u);                 /*       9742 -> 4949       */
	BINARY(2435u);                 /*       4949 -> 2514       */
	BINARY(1237u);                 /*       2514 -> 1277       */
	BINARY(628u);                  /*       1277 -> 649        */
	BINARY(319u);                  /*        649 -> 330        */
	BINARY(162u);                  /*        330 -> 168        */
	BINARY(82u);                   /*        168 -> 86         */
	BINARY(42u);                   /*         86 -> 44         */
	BINARY(21u);                   /*         44 -> 23         */
	BINARY(11u);                   /*         23 -> 12         */
	BINARY(5u);                    /*         12 -> 7          */
	BINARY(3u);                    /*          7 -> 4          */
	BINARY(1u);                    /*          4 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_25 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(16510910u);             /*   33554432 -> 17043522   */
	BINARY(8386494u);              /*   17043522 -> 8657028    */
	BINARY(4259807u);              /*    8657028 -> 4397221    */
	BINARY(2163711u);              /*    4397221 -> 2233510    */
	BINARY(1099028u);              /*    2233510 -> 1134482    */
	BINARY(558237u);               /*    1134482 -> 576245     */
	BINARY(283549u);               /*     576245 -> 292696     */
	BINARY(144025u);               /*     292696 -> 148671     */
	BINARY(73155u);                /*     148671 -> 75516      */
	BINARY(37158u);                /*      75516 -> 38358      */
	BINARY(18874u);                /*      38358 -> 19484      */
	BINARY(9587u);                 /*      19484 -> 9897       */
	BINARY(4869u);                 /*       9897 -> 5028       */
	BINARY(2474u);                 /*       5028 -> 2554       */
	BINARY(1256u);                 /*       2554 -> 1298       */
	BINARY(638u);                  /*       1298 -> 660        */
	BINARY(324u);                  /*        660 -> 336        */
	BINARY(165u);                  /*        336 -> 171        */
	BINARY(84u);                   /*        171 -> 87         */
	BINARY(42u);                   /*         87 -> 45         */
	BINARY(22u);                   /*         45 -> 23         */
	BINARY(11u);                   /*         23 -> 12         */
	BINARY(5u);                    /*         12 -> 7          */
	BINARY(3u);                    /*          7 -> 4          */
	BINARY(1u);                    /*          4 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_26 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(33021821u);             /*   67108864 -> 34087043   */
	BINARY(16772989u);             /*   34087043 -> 17314054   */
	BINARY(8519613u);              /*   17314054 -> 8794441    */
	BINARY(4327423u);              /*    8794441 -> 4467018    */
	BINARY(2198056u);              /*    4467018 -> 2268962    */
	BINARY(1116473u);              /*    2268962 -> 1152489    */
	BINARY(567097u);               /*    1152489 -> 585392     */
	BINARY(288050u);               /*     585392 -> 297342     */
	BINARY(146311u);               /*     297342 -> 151031     */
	BINARY(74316u);                /*     151031 -> 76715      */
	BINARY(37748u);                /*      76715 -> 38967      */
	BINARY(19174u);                /*      38967 -> 19793      */
	BINARY(9739u);                 /*      19793 -> 10054      */
	BINARY(4947u);                 /*      10054 -> 5107       */
	BINARY(2512u);                 /*       5107 -> 2595       */
	BINARY(1276u);                 /*       2595 -> 1319       */
	BINARY(649u);                  /*       1319 -> 670        */
	BINARY(329u);                  /*        670 -> 341        */
	BINARY(167u);                  /*        341 -> 174        */
	BINARY(85u);                   /*        174 -> 89         */
	BINARY(43u);                   /*         89 -> 46         */
	BINARY(22u);                   /*         46 -> 24         */
	BINARY(11u);                   /*         24 -> 13         */
	BINARY(6u);                    /*         13 -> 7          */
	BINARY(3u);                    /*          7 -> 4          */
	BINARY(1u);                    /*          4 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_27 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(66043643u);             /*  134217728 -> 68174085   */
	BINARY(33545978u);             /*   68174085 -> 34628107   */
	BINARY(17039227u);             /*   34628107 -> 17588880   */
	BINARY(8654845u);              /*   17588880 -> 8934035    */
	BINARY(4396112u);              /*    8934035 -> 4537923    */
	BINARY(2232946u);              /*    4537923 -> 2304977    */
	BINARY(1134195u);              /*    2304977 -> 1170782    */
	BINARY(576099u);               /*    1170782 -> 594683     */
	BINARY(292621u);               /*     594683 -> 302062     */
	BINARY(148633u);               /*     302062 -> 153429     */
	BINARY(75496u);                /*     153429 -> 77933      */
	BINARY(38347u);                /*      77933 -> 39586      */
	BINARY(19478u);                /*      39586 -> 20108      */
	BINARY(9894u);                 /*      20108 -> 10214      */
	BINARY(5025u);                 /*      10214 -> 5189       */
	BINARY(2553u);                 /*       5189 -> 2636       */
	BINARY(1297u);                 /*       2636 -> 1339       */
	BINARY(658u);                  /*       1339 -> 681        */
	BINARY(335u);                  /*        681 -> 346        */
	BINARY(170u);                  /*        346 -> 176        */
	BINARY(86u);                   /*        176 -> 90         */
	BINARY(44u);                   /*         90 -> 46         */
	BINARY(22u);                   /*         46 -> 24         */
	BINARY(11u);                   /*         24 -> 13         */
	BINARY(6u);                    /*         13 -> 7          */
	BINARY(3u);                    /*          7 -> 4          */
	BINARY(1u);                    /*          4 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_28 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(132087287u);            /*  268435456 -> 136348169  */
	BINARY(67091956u);             /*  136348169 -> 69256213   */
	BINARY(34078454u);             /*   69256213 -> 35177759   */
	BINARY(17309690u);             /*   35177759 -> 17868069   */
	BINARY(8792224u);              /*   17868069 -> 9075845    */
	BINARY(4465891u);              /*    9075845 -> 4609954    */
	BINARY(2268390u);              /*    4609954 -> 2341564    */
	BINARY(1152198u);              /*    2341564 -> 1189366    */
	BINARY(585243u);               /*    1189366 -> 604123     */
	BINARY(297266u);               /*     604123 -> 306857     */
	BINARY(150993u);               /*     306857 -> 155864     */
	BINARY(76694u);                /*     155864 -> 79170      */
	BINARY(38956u);                /*      79170 -> 40214      */
	BINARY(19787u);                /*      40214 -> 20427      */
	BINARY(10051u);                /*      20427 -> 10376      */
	BINARY(5105u);                 /*      10376 -> 5271       */
	BINARY(2593u);                 /*       5271 -> 2678       */
	BINARY(1317u);                 /*       2678 -> 1361       */
	BINARY(669u);                  /*       1361 -> 692        */
	BINARY(340u);                  /*        692 -> 352        */
	BINARY(173u);                  /*        352 -> 179        */
	BINARY(88u);                   /*        179 -> 91         */
	BINARY(44u);                   /*         91 -> 47         */
	BINARY(23u);                   /*         47 -> 24         */
	BINARY(11u);                   /*         24 -> 13         */
	BINARY(6u);                    /*         13 -> 7          */
	BINARY(3u);                    /*          7 -> 4          */
	BINARY(1u);                    /*          4 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_29 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(264174575u);            /*  536870912 -> 272696337  */
	BINARY(134183911u);            /*  272696337 -> 138512426  */
	BINARY(68156908u);             /*  138512426 -> 70355518   */
	BINARY(34619381u);             /*   70355518 -> 35736137   */
	BINARY(17584448u);             /*   35736137 -> 18151689   */
	BINARY(8931783u);              /*   18151689 -> 9219906    */
	BINARY(4536779u);              /*    9219906 -> 4683127    */
	BINARY(2304395u);              /*    4683127 -> 2378732    */
	BINARY(1170487u);              /*    2378732 -> 1208245    */
	BINARY(594533u);               /*    1208245 -> 613712     */
	BINARY(301985u);               /*     613712 -> 311727     */
	BINARY(153389u);               /*     311727 -> 158338     */
	BINARY(77912u);                /*     158338 -> 80426      */
	BINARY(39574u);                /*      80426 -> 40852      */
	BINARY(20101u);                /*      40852 -> 20751      */
	BINARY(10210u);                /*      20751 -> 10541      */
	BINARY(5186u);                 /*      10541 -> 5355       */
	BINARY(2635u);                 /*       5355 -> 2720       */
	BINARY(1338u);                 /*       2720 -> 1382       */
	BINARY(680u);                  /*       1382 -> 702        */
	BINARY(345u);                  /*        702 -> 357        */
	BINARY(175u);                  /*        357 -> 182        */
	BINARY(89u);                   /*        182 -> 93         */
	BINARY(45u);                   /*         93 -> 48         */
	BINARY(23u);                   /*         48 -> 25         */
	BINARY(12u);                   /*         25 -> 13         */
	BINARY(6u);                    /*         13 -> 7          */
	BINARY(3u);                    /*          7 -> 4          */
	BINARY(1u);                    /*          4 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t o_30 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	BINARY(528349151u);            /* 1073741824 -> 545392673  */
	BINARY(268367823u);            /*  545392673 -> 277024850  */
	BINARY(136313815u);            /*  277024850 -> 140711035  */
	BINARY(69238763u);             /*  140711035 -> 71472272   */
	BINARY(35168895u);             /*   71472272 -> 36303377   */
	BINARY(17863566u);             /*   36303377 -> 18439811   */
	BINARY(9073557u);              /*   18439811 -> 9366254    */
	BINARY(4608791u);              /*    9366254 -> 4757463    */
	BINARY(2340973u);              /*    4757463 -> 2416490    */
	BINARY(1189066u);              /*    2416490 -> 1227424    */
	BINARY(603970u);               /*    1227424 -> 623454     */
	BINARY(306778u);               /*     623454 -> 316676     */
	BINARY(155824u);               /*     316676 -> 160852     */
	BINARY(79149u);                /*     160852 -> 81703      */
	BINARY(40203u);                /*      81703 -> 41500      */
	BINARY(20420u);                /*      41500 -> 21080      */
	BINARY(10372u);                /*      21080 -> 10708      */
	BINARY(5269u);                 /*      10708 -> 5439       */
	BINARY(2676u);                 /*       5439 -> 2763       */
	BINARY(1359u);                 /*       2763 -> 1404       */
	BINARY(690u);                  /*       1404 -> 714        */
	BINARY(351u);                  /*        714 -> 363        */
	BINARY(178u);                  /*        363 -> 185        */
	BINARY(91u);                   /*        185 -> 94         */
	BINARY(46u);                   /*         94 -> 48         */
	BINARY(23u);                   /*         48 -> 25         */
	BINARY(12u);                   /*         25 -> 13         */
	BINARY(6u);                    /*         13 -> 7          */
	BINARY(3u);                    /*          7 -> 4          */
	BINARY(1u);                    /*          4 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

search_fun 
offset[] = {NULL, NULL, NULL, o_3, o_4, o_5, o_6, o_7, o_8, o_9, o_10, o_11, o_12, o_13, o_14, o_15, o_16, o_17, o_18, o_19, o_20, o_21, o_22, o_23, o_24, o_25, o_26, o_27, o_28, o_29, o_30};
