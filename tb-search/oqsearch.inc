size_t oq_3 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(3u, 6u, 5u);        /*          8 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t oq_4 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(5u, 10u, 11u);      /*         16 -> 5          */
	QUATERNARY(2u, 4u, 3u);        /*          5 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t oq_5 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(9u, 18u, 23u);      /*         32 -> 9          */
	QUATERNARY(3u, 6u, 6u);        /*          9 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t oq_6 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(17u, 34u, 47u);     /*         64 -> 17         */
	QUATERNARY(5u, 10u, 12u);      /*         17 -> 5          */
	QUATERNARY(2u, 4u, 3u);        /*          5 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t oq_7 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(33u, 66u, 95u);     /*        128 -> 33         */
	QUATERNARY(9u, 18u, 24u);      /*         33 -> 9          */
	QUATERNARY(3u, 6u, 6u);        /*          9 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t oq_8 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(66u, 132u, 190u);   /*        256 -> 66         */
	QUATERNARY(17u, 34u, 49u);     /*         66 -> 17         */
	QUATERNARY(5u, 10u, 12u);      /*         17 -> 5          */
	QUATERNARY(2u, 4u, 3u);        /*          5 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t oq_9 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(131u, 262u, 381u);  /*        512 -> 131        */
	QUATERNARY(34u, 68u, 97u);     /*        131 -> 34         */
	QUATERNARY(9u, 18u, 25u);      /*         34 -> 9          */
	QUATERNARY(3u, 6u, 6u);        /*          9 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t oq_10 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(261u, 522u, 763u);  /*       1024 -> 261        */
	QUATERNARY(67u, 134u, 194u);   /*        261 -> 67         */
	QUATERNARY(18u, 36u, 49u);     /*         67 -> 18         */
	QUATERNARY(5u, 10u, 13u);      /*         18 -> 5          */
	QUATERNARY(2u, 4u, 3u);        /*          5 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t oq_11 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(521u, 1042u, 1527u); /*       2048 -> 521        */
	QUATERNARY(133u, 266u, 388u);  /*        521 -> 133        */
	QUATERNARY(34u, 68u, 99u);     /*        133 -> 34         */
	QUATERNARY(9u, 18u, 25u);      /*         34 -> 9          */
	QUATERNARY(3u, 6u, 6u);        /*          9 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t oq_12 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(1041u, 2082u, 3055u); /*       4096 -> 1041       */
	QUATERNARY(265u, 530u, 776u);  /*       1041 -> 265        */
	QUATERNARY(68u, 136u, 197u);   /*        265 -> 68         */
	QUATERNARY(18u, 36u, 50u);     /*         68 -> 18         */
	QUATERNARY(5u, 10u, 13u);      /*         18 -> 5          */
	QUATERNARY(2u, 4u, 3u);        /*          5 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t oq_13 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(2081u, 4162u, 6111u); /*       8192 -> 2081       */
	QUATERNARY(529u, 1058u, 1552u); /*       2081 -> 529        */
	QUATERNARY(135u, 270u, 394u);  /*        529 -> 135        */
	QUATERNARY(35u, 70u, 100u);    /*        135 -> 35         */
	QUATERNARY(9u, 18u, 26u);      /*         35 -> 9          */
	QUATERNARY(3u, 6u, 6u);        /*          9 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t oq_14 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(4162u, 8324u, 12222u); /*      16384 -> 4162       */
	QUATERNARY(1058u, 2116u, 3104u); /*       4162 -> 1058       */
	QUATERNARY(269u, 538u, 789u);  /*       1058 -> 269        */
	QUATERNARY(69u, 138u, 200u);   /*        269 -> 69         */
	QUATERNARY(18u, 36u, 51u);     /*         69 -> 18         */
	QUATERNARY(5u, 10u, 13u);      /*         18 -> 5          */
	QUATERNARY(2u, 4u, 3u);        /*          5 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t oq_15 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(8323u, 16646u, 24445u); /*      32768 -> 8323       */
	QUATERNARY(2114u, 4228u, 6209u); /*       8323 -> 2114       */
	QUATERNARY(537u, 1074u, 1577u); /*       2114 -> 537        */
	QUATERNARY(137u, 274u, 400u);  /*        537 -> 137        */
	QUATERNARY(35u, 70u, 102u);    /*        137 -> 35         */
	QUATERNARY(9u, 18u, 26u);      /*         35 -> 9          */
	QUATERNARY(3u, 6u, 6u);        /*          9 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t oq_16 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(16645u, 33290u, 48891u); /*      65536 -> 16645      */
	QUATERNARY(4228u, 8456u, 12417u); /*      16645 -> 4228       */
	QUATERNARY(1074u, 2148u, 3154u); /*       4228 -> 1074       */
	QUATERNARY(273u, 546u, 801u);  /*       1074 -> 273        */
	QUATERNARY(70u, 140u, 203u);   /*        273 -> 70         */
	QUATERNARY(18u, 36u, 52u);     /*         70 -> 18         */
	QUATERNARY(5u, 10u, 13u);      /*         18 -> 5          */
	QUATERNARY(2u, 4u, 3u);        /*          5 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t oq_17 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(33289u, 66578u, 97783u); /*     131072 -> 33289      */
	QUATERNARY(8455u, 16910u, 24834u); /*      33289 -> 8455       */
	QUATERNARY(2148u, 4296u, 6307u); /*       8455 -> 2148       */
	QUATERNARY(546u, 1092u, 1602u); /*       2148 -> 546        */
	QUATERNARY(139u, 278u, 407u);  /*        546 -> 139        */
	QUATERNARY(36u, 72u, 103u);    /*        139 -> 36         */
	QUATERNARY(10u, 20u, 26u);     /*         36 -> 10         */
	QUATERNARY(3u, 6u, 7u);        /*         10 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t oq_18 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(66577u, 133154u, 195567u); /*     262144 -> 66577      */
	QUATERNARY(16909u, 33818u, 49668u); /*      66577 -> 16909      */
	QUATERNARY(4295u, 8590u, 12614u); /*      16909 -> 4295       */
	QUATERNARY(1091u, 2182u, 3204u); /*       4295 -> 1091       */
	QUATERNARY(278u, 556u, 813u);  /*       1091 -> 278        */
	QUATERNARY(71u, 142u, 207u);   /*        278 -> 71         */
	QUATERNARY(19u, 38u, 52u);     /*         71 -> 19         */
	QUATERNARY(5u, 10u, 14u);      /*         19 -> 5          */
	QUATERNARY(2u, 4u, 3u);        /*          5 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t oq_19 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(133153u, 266306u, 391135u); /*     524288 -> 133153     */
	QUATERNARY(33817u, 67634u, 99336u); /*     133153 -> 33817      */
	QUATERNARY(8589u, 17178u, 25228u); /*      33817 -> 8589       */
	QUATERNARY(2182u, 4364u, 6407u); /*       8589 -> 2182       */
	QUATERNARY(555u, 1110u, 1627u); /*       2182 -> 555        */
	QUATERNARY(141u, 282u, 414u);  /*        555 -> 141        */
	QUATERNARY(36u, 72u, 105u);    /*        141 -> 36         */
	QUATERNARY(10u, 20u, 26u);     /*         36 -> 10         */
	QUATERNARY(3u, 6u, 7u);        /*         10 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t oq_20 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(266306u, 532612u, 782270u); /*    1048576 -> 266306     */
	QUATERNARY(67634u, 135268u, 198672u); /*     266306 -> 67634      */
	QUATERNARY(17177u, 34354u, 50457u); /*      67634 -> 17177      */
	QUATERNARY(4363u, 8726u, 12814u); /*      17177 -> 4363       */
	QUATERNARY(1109u, 2218u, 3254u); /*       4363 -> 1109       */
	QUATERNARY(282u, 564u, 827u);  /*       1109 -> 282        */
	QUATERNARY(72u, 144u, 210u);   /*        282 -> 72         */
	QUATERNARY(19u, 38u, 53u);     /*         72 -> 19         */
	QUATERNARY(5u, 10u, 14u);      /*         19 -> 5          */
	QUATERNARY(2u, 4u, 3u);        /*          5 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t oq_21 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(532611u, 1065222u, 1564541u); /*    2097152 -> 532611     */
	QUATERNARY(135267u, 270534u, 397344u); /*     532611 -> 135267     */
	QUATERNARY(34354u, 68708u, 100913u); /*     135267 -> 34354      */
	QUATERNARY(8725u, 17450u, 25629u); /*      34354 -> 8725       */
	QUATERNARY(2216u, 4432u, 6509u); /*       8725 -> 2216       */
	QUATERNARY(563u, 1126u, 1653u); /*       2216 -> 563        */
	QUATERNARY(143u, 286u, 420u);  /*        563 -> 143        */
	QUATERNARY(37u, 74u, 106u);    /*        143 -> 37         */
	QUATERNARY(10u, 20u, 27u);     /*         37 -> 10         */
	QUATERNARY(3u, 6u, 7u);        /*         10 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t oq_22 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(1065221u, 2130442u, 3129083u); /*    4194304 -> 1065221    */
	QUATERNARY(270533u, 541066u, 794688u); /*    1065221 -> 270533     */
	QUATERNARY(68707u, 137414u, 201826u); /*     270533 -> 68707      */
	QUATERNARY(17450u, 34900u, 51257u); /*      68707 -> 17450      */
	QUATERNARY(4432u, 8864u, 13018u); /*      17450 -> 4432       */
	QUATERNARY(1126u, 2252u, 3306u); /*       4432 -> 1126       */
	QUATERNARY(286u, 572u, 840u);  /*       1126 -> 286        */
	QUATERNARY(73u, 146u, 213u);   /*        286 -> 73         */
	QUATERNARY(19u, 38u, 54u);     /*         73 -> 19         */
	QUATERNARY(5u, 10u, 14u);      /*         19 -> 5          */
	QUATERNARY(2u, 4u, 3u);        /*          5 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t oq_23 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(2130441u, 4260882u, 6258167u); /*    8388608 -> 2130441    */
	QUATERNARY(541065u, 1082130u, 1589376u); /*    2130441 -> 541065     */
	QUATERNARY(137414u, 274828u, 403651u); /*     541065 -> 137414     */
	QUATERNARY(34899u, 69798u, 102515u); /*     137414 -> 34899      */
	QUATERNARY(8864u, 17728u, 26035u); /*      34899 -> 8864       */
	QUATERNARY(2252u, 4504u, 6612u); /*       8864 -> 2252       */
	QUATERNARY(572u, 1144u, 1680u); /*       2252 -> 572        */
	QUATERNARY(146u, 292u, 426u);  /*        572 -> 146        */
	QUATERNARY(38u, 76u, 108u);    /*        146 -> 38         */
	QUATERNARY(10u, 20u, 28u);     /*         38 -> 10         */
	QUATERNARY(3u, 6u, 7u);        /*         10 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t oq_24 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(4260881u, 8521762u, 12516335u); /*   16777216 -> 4260881    */
	QUATERNARY(1082129u, 2164258u, 3178752u); /*    4260881 -> 1082129    */
	QUATERNARY(274827u, 549654u, 807302u); /*    1082129 -> 274827     */
	QUATERNARY(69798u, 139596u, 205029u); /*     274827 -> 69798      */
	QUATERNARY(17727u, 35454u, 52071u); /*      69798 -> 17727      */
	QUATERNARY(4503u, 9006u, 13224u); /*      17727 -> 4503       */
	QUATERNARY(1144u, 2288u, 3359u); /*       4503 -> 1144       */
	QUATERNARY(291u, 582u, 853u);  /*       1144 -> 291        */
	QUATERNARY(74u, 148u, 217u);   /*        291 -> 74         */
	QUATERNARY(19u, 38u, 55u);     /*         74 -> 19         */
	QUATERNARY(5u, 10u, 14u);      /*         19 -> 5          */
	QUATERNARY(2u, 4u, 3u);        /*          5 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t oq_25 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(8521761u, 17043522u, 25032671u); /*   33554432 -> 8521761    */
	QUATERNARY(2164257u, 4328514u, 6357504u); /*    8521761 -> 2164257    */
	QUATERNARY(549653u, 1099306u, 1614604u); /*    2164257 -> 549653     */
	QUATERNARY(139595u, 279190u, 410058u); /*     549653 -> 139595     */
	QUATERNARY(35453u, 70906u, 104142u); /*     139595 -> 35453      */
	QUATERNARY(9004u, 18008u, 26449u); /*      35453 -> 9004       */
	QUATERNARY(2287u, 4574u, 6717u); /*       9004 -> 2287       */
	QUATERNARY(581u, 1162u, 1706u); /*       2287 -> 581        */
	QUATERNARY(148u, 296u, 433u);  /*        581 -> 148        */
	QUATERNARY(38u, 76u, 110u);    /*        148 -> 38         */
	QUATERNARY(10u, 20u, 28u);     /*         38 -> 10         */
	QUATERNARY(3u, 6u, 7u);        /*         10 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t oq_26 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(17043522u, 34087044u, 50065342u); /*   67108864 -> 17043522   */
	QUATERNARY(4328514u, 8657028u, 12715008u); /*   17043522 -> 4328514    */
	QUATERNARY(1099306u, 2198612u, 3229208u); /*    4328514 -> 1099306    */
	QUATERNARY(279189u, 558378u, 820117u); /*    1099306 -> 279189     */
	QUATERNARY(70906u, 141812u, 208283u); /*     279189 -> 70906      */
	QUATERNARY(18008u, 36016u, 52898u); /*      70906 -> 18008      */
	QUATERNARY(4574u, 9148u, 13434u); /*      18008 -> 4574       */
	QUATERNARY(1162u, 2324u, 3412u); /*       4574 -> 1162       */
	QUATERNARY(296u, 592u, 866u);  /*       1162 -> 296        */
	QUATERNARY(76u, 152u, 220u);   /*        296 -> 76         */
	QUATERNARY(20u, 40u, 56u);     /*         76 -> 20         */
	QUATERNARY(6u, 12u, 14u);      /*         20 -> 6          */
	QUATERNARY(2u, 4u, 4u);        /*          6 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t oq_27 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(34087043u, 68174086u, 100130685u); /*  134217728 -> 34087043   */
	QUATERNARY(8657027u, 17314054u, 25430016u); /*   34087043 -> 8657027    */
	QUATERNARY(2198611u, 4397222u, 6458416u); /*    8657027 -> 2198611    */
	QUATERNARY(558378u, 1116756u, 1640233u); /*    2198611 -> 558378     */
	QUATERNARY(141811u, 283622u, 416567u); /*     558378 -> 141811     */
	QUATERNARY(36016u, 72032u, 105795u); /*     141811 -> 36016      */
	QUATERNARY(9147u, 18294u, 26869u); /*      36016 -> 9147       */
	QUATERNARY(2324u, 4648u, 6823u); /*       9147 -> 2324       */
	QUATERNARY(591u, 1182u, 1733u); /*       2324 -> 591        */
	QUATERNARY(151u, 302u, 440u);  /*        591 -> 151        */
	QUATERNARY(39u, 78u, 112u);    /*        151 -> 39         */
	QUATERNARY(10u, 20u, 29u);     /*         39 -> 10         */
	QUATERNARY(3u, 6u, 7u);        /*         10 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t oq_28 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(68174085u, 136348170u, 200261371u); /*  268435456 -> 68174085   */
	QUATERNARY(17314054u, 34628108u, 50860031u); /*   68174085 -> 17314054   */
	QUATERNARY(4397221u, 8794442u, 12916833u); /*   17314054 -> 4397221    */
	QUATERNARY(1116755u, 2233510u, 3280466u); /*    4397221 -> 1116755    */
	QUATERNARY(283621u, 567242u, 833134u); /*    1116755 -> 283621     */
	QUATERNARY(72031u, 144062u, 211590u); /*     283621 -> 72031      */
	QUATERNARY(18294u, 36588u, 53737u); /*      72031 -> 18294      */
	QUATERNARY(4647u, 9294u, 13647u); /*      18294 -> 4647       */
	QUATERNARY(1181u, 2362u, 3466u); /*       4647 -> 1181       */
	QUATERNARY(300u, 600u, 881u);  /*       1181 -> 300        */
	QUATERNARY(77u, 154u, 223u);   /*        300 -> 77         */
	QUATERNARY(20u, 40u, 57u);     /*         77 -> 20         */
	QUATERNARY(6u, 12u, 14u);      /*         20 -> 6          */
	QUATERNARY(2u, 4u, 4u);        /*          6 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

size_t oq_29 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(136348169u, 272696338u, 400522743u); /*  536870912 -> 136348169  */
	QUATERNARY(34628107u, 69256214u, 101720062u); /*  136348169 -> 34628107   */
	QUATERNARY(8794440u, 17588880u, 25833667u); /*   34628107 -> 8794440    */
	QUATERNARY(2233509u, 4467018u, 6560931u); /*    8794440 -> 2233509    */
	QUATERNARY(567241u, 1134482u, 1666268u); /*    2233509 -> 567241     */
	QUATERNARY(144062u, 288124u, 423179u); /*     567241 -> 144062     */
	QUATERNARY(36588u, 73176u, 107474u); /*     144062 -> 36588      */
	QUATERNARY(9293u, 18586u, 27295u); /*      36588 -> 9293       */
	QUATERNARY(2361u, 4722u, 6932u); /*       9293 -> 2361       */
	QUATERNARY(600u, 1200u, 1761u); /*       2361 -> 600        */
	QUATERNARY(153u, 306u, 447u);  /*        600 -> 153        */
	QUATERNARY(39u, 78u, 114u);    /*        153 -> 39         */
	QUATERNARY(10u, 20u, 29u);     /*         39 -> 10         */
	QUATERNARY(3u, 6u, 7u);        /*         10 -> 3          */
	TERNARY(1u, 2u);               /*          3 -> 1          */
	return base-vector;
}

size_t oq_30 (unsigned key, unsigned * vector)
{
	unsigned * base = vector;
	QUATERNARY(272696337u, 545392674u, 801045487u); /* 1073741824 -> 272696337  */
	QUATERNARY(69256213u, 138512426u, 203440124u); /*  272696337 -> 69256213   */
	QUATERNARY(17588880u, 35177760u, 51667333u); /*   69256213 -> 17588880   */
	QUATERNARY(4467018u, 8934036u, 13121862u); /*   17588880 -> 4467018    */
	QUATERNARY(1134481u, 2268962u, 3332537u); /*    4467018 -> 1134481    */
	QUATERNARY(288123u, 576246u, 846358u); /*    1134481 -> 288123     */
	QUATERNARY(73175u, 146350u, 214948u); /*     288123 -> 73175      */
	QUATERNARY(18585u, 37170u, 54590u); /*      73175 -> 18585      */
	QUATERNARY(4720u, 9440u, 13865u); /*      18585 -> 4720       */
	QUATERNARY(1199u, 2398u, 3521u); /*       4720 -> 1199       */
	QUATERNARY(305u, 610u, 894u);  /*       1199 -> 305        */
	QUATERNARY(78u, 156u, 227u);   /*        305 -> 78         */
	QUATERNARY(20u, 40u, 58u);     /*         78 -> 20         */
	QUATERNARY(6u, 12u, 14u);      /*         20 -> 6          */
	QUATERNARY(2u, 4u, 4u);        /*          6 -> 2          */
	BINARY(1u);                    /*          2 -> 1          */
	return base-vector;
}

search_fun 
off_quat[] = {NULL, NULL, NULL, oq_3, oq_4, oq_5, oq_6, oq_7, oq_8, oq_9, oq_10, oq_11, oq_12, oq_13, oq_14, oq_15, oq_16, oq_17, oq_18, oq_19, oq_20, oq_21, oq_22, oq_23, oq_24, oq_25, oq_26, oq_27, oq_28, oq_29, oq_30};
