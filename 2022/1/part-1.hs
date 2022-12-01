maxCal :: [[Int]] -> Int
maxCal a = foldl (max') 0 $ map (sum) a
	   where max' a b | a > b     = a
                          | otherwise = b

input :: [[Int]]
input = [[2494,8013,1055,5425,9104,10665],[10642,10677,10300,7374,9085,8508,7569,6214],[5300,5960,8252,5852,4308,9991,6367],[1361,5356,6114,3379,5996,3822,3785,1622,1143,6009,2211,4951,3093,6395],[4892,6648,3022,6063,5436,2174,4264,2004,6444,2926,2651,2749],[17121,17879,11056,15642],[3926,6751,2643,7882,4006,1351,4912,2735,2985,4015],[3043,6923,10352,9998,2470,7346,1077,3024],[21632,34735],[8525,1982,9285,4188,7017,6666,6538],[7019,2696,2707,3639,3674,3642,4837,3804],[3570,8191,6054,8727,7323,3522],[1442,5794,3774,2398,5615,5786,1832,2234,7906,4799],[12978,17621,9788,9237],[3006,2799,5627,2478,3159,6430,3843,1469,2006,4131,2269,6201,3593,1423],[30560],[5281,3850,2894,4056,4341,4117,8216,2700,1327,2379],[1597,7217,1003,2555,4820,2881,4051,4550,2457,6518,5480,3044],[9465,4325,5593,5606,9317,1026,3707,9456,7801],[6217,7814,3159,3866,1035,4302,1498,1160,1510,4650],[13306,15884,9699,11206],[24775,21119,12643],[12737,10893,3450,10732,8670,7597],[35714],[3380,3513,6559,6891,7767,5311,7086,5345,3498,3091,2516],[11565,12877,13416,10568,16033],[1380,4192,5815,6230,2434,2443,3682,5568,6134,3316,5878,4493,3857,2110],[4341,2469,5606,4268,3525,5555,5771,2284,1921,3279,1901,5991,2868,4866,2310],[5969,18172,1931],[3598,2837,1105,7300,11781,10686],[3698,4698,3891,4481,4089,3511,4949,2148,4017,2529,3572,1912,1837,1937,2688],[6485,7072,7964,13114,5864,5280],[12413,7196,12856,18069],[1367,2238,3089,3189,5022,3877,5238,3566,1081,1525,1958,5796,2498,1517,1481],[25527,17474,4705],[6722,2844,3856,1780,4518,5679,4775,6067,2695,5979,2488,1515,5071],[7155,4147,4406,4169,2156,7110,6837,4673,3599,2672,2768,3195],[8040,5248,6440,1830,2281,4938,6116,6842,5743,5788,5703],[2527,3362,5630,4976,1989,5157,4951,3382,4525,4057,4077,3717,4035,4907,1795],[1069,7242,2438,7224,8058,6215,7770,5958,2445,1702,3558],[6075,4164,5797,1051,3104,2966,3653,3933,1546,5111,4811,4145,3532,2289,1468],[9540],[1830,6422,7523,2131,3072,2873,1423,7560,4640,4476],[10022,15927,14403,2312],[3056,5371,4501,6465,4761,2050,4187,1179,3746,6089,1365,6495,4844,4984],[1812,2671,1805,4200,1986,5327,5548,1872,4364,3147,5447,5882,2602,5501,2446],[9725,8080,9941,10636,6852,5378,4137],[25228,22215,10199],[15394,28799],[3620,1758,4663,2715,5642,6209,5361,4582,2165,7485,7611],[6910,6782,2177,2763,6877,3922,1355,2400,6139,5362,5737,3766,3535],[6587,6770,3729,3316,4161,2828,3886,7483,2746,5923],[1647,6201,1200,5810,4493,5373,1856,5507,1833,4277,2897,4473,4433,4352],[3807,18028,7426,18476],[6493,1157,3125,7736,1208,3243,7737,1953,1970,3245,1600],[12034,12959,9334],[8432,3424,4506,5758,10840,11056],[7284,4013,4217,7307,6367,1977,3617,2165,2969,6595,6651,2446],[1083,5636,1785,2553,4634,3790,7465,2970,2725,4267,2861],[3145,7872,5049,3368,4009,2348,6877,6956,2359,8064,4386],[4764,6236,4923,4395,2974,3985,2725,1831,2755,4068,2383,2138,2998,4346],[6673,2519,2754,5734,4796,3389,6931,5771,5936,2328,5684,3171,4608],[15166,11415,9051,3437,10405],[1590,7370,7003,5030,3852,6353,7054,3765,2925,6968],[9678,2846,8939,10270,7196,11860,4221],[4447,5407,1459,1693,3774,3978,5240,2331,1903,4421,1152,5084,1969,2365,4526],[5249,5631,3701,6419,6003,6884,2315,5745,1341,6370,5430,6442,3156],[2112,3970,1000,1981,3528,3490,1510,1629,5413,4277,6347,6134,1855],[8848,3187,9361,16338,5644],[8477,6983,8052,4156,8204,3216,4797,3024,3499,6006],[7326,14394,7169,17160],[3282,5582,6657,5435,2096,3038,5747,2545,1927,6331,6785,1199,1794],[1471,4089,8406,3811,7060,1164,1042,5968,8721],[6191,17967,6680,15071],[1811,2960,4046,4196,5271,1982,2324,8928,4517],[12525,5174,5986,14713,15516],[4412,5282,6035,5882,5488,5797,1101,2763,6159,3886,5358,5802,4033,3113],[9534,4195,9134,4093,2988,7170,10552,1724],[1768,8793,5606,8771,5365,9465,6618,3528],[2206,4623,4108,3138,5818,4907,2036,1646,5703,3097,3475,1167,2515,5095,4964],[3633,6926,5402,4602,8429,9575,7807,3691,1060],[1142,10612,6215,9965,6377,2889],[3544,3870,1373,1166,2068,5111,1958,4383,1868,5305,3620,5304,3126,2972],[36719],[6297,1347,1423,7325,1798,7102,2585,7973,7084,6963,2423],[23242,10969,22162],[13407],[3392,4473,1448,4470,2740,2249,1154,2212,3078,3817,6471,3506,3395,1906],[4808,1923,8332,8636,2919,5259,7210,10191],[7183,4475,5652,6992,7492,7906,7275,6680,5776],[2752,9069,11736,9659,11337,1120,9412],[3199,3488,13447,10265,6076,13516],[1651,3970,7077,6774,6915,4838,10860],[5737,6268,1718,10759,3572,5494],[24537,10330],[10316,2956,5555,6248,5263,1249,5783,5774],[3525,2066,2616,7191,3926,1532,5964,5512,1695,2512,6491],[2975,1916,10644,5182,5339,8680,3275,10289],[4593,4274,4044,4904,1989,4851,1555,3703,5231,2427,3096,4461,5178,5241,1141],[2331,1706,5359,5149,3273,4602,3067,6598,6665,3244,2654],[2959,3036,1202,4882,1152,1667,4291,2393,3774,5366,1414,3985,4260,2775,3831],[2474,1259,2835,6583,6997,6502,5069,6551,3077,5282,2469,7110],[8600,5500,15337,13901,1000],[18310,10484],[6280,8351,4405,5826,1032,6646,1367,3758,7046,2308],[4888,8942,6060,2012,9154,6142,1923,7135,3387],[9170,5125,1487,6729,8642,10296,7469],[1373,9559,6106,2405,7721,5877,5946,6080],[4164,2747,4312,5824,2960,2506,6274,5887,4796,3492,4477,2933,1811,4957],[8728,9090,7028,8869,3362,3784,7482,8101,3665],[5341,8591,3108,9080,6248,6100],[7454,28472],[7151,4060,9436,9008,1333,6366,8098,7046,1869],[34017,11550],[4232,6470,3007,4974,6384,5389,2818,3389,2533,6309,2500,2012,2348,3439],[2004,2786,6125,2455,3910,2408,1037,2979,3755,3838,2639,5705,4765,3057],[2257,2999,6940,2994,5671,1779,2765,5705,2980,4426,6194,4142,5594],[31017,14466],[5123,4500,5594,2181,3229,4797,3006,5755,3988,3184,2501,5516,5401],[23717,19963,23101],[22339,24909,1460],[4560,2544,3862,4631,2156,7098,3945,1190,1009,1850,2418,2600],[3012,1803,1248,1391,3975,4870,6010,4507,5351,2986,6310,4678,4341,4420],[18246,20268,12620],[2308,9249,3234,11647,5717,10514,10348],[6993,14784,22089],[2456,7956,4644,11788,10286,10176,6059],[7692,10510,11255],[6235,2861,1136],[11444],[11241,2893,3679,12208],[8913,9505,3537,15868],[4555,9248,18262],[12005,1419,5608,3454],[14026,3426,9822,6157,1034],[3063,1146,10723,1798,10569,3361,8119,5926],[11894,8410,3507,8343,15469],[16610,3588,11982,2953],[3112,4710,4522,2766,4143,3462,1736,2128,2889,4421,2772,1153,4622,3897,4013],[3483,1728,3257,4736,11723,2254,7438],[4465,5761,1625,5566,1391,5720,2158,5331,1482,1734,4974,1276,2704,1386,5426],[18780,10740],[5245,1769,7853],[2539,6400,4704,3929,6819,4483,1535,1689,6851,2644,1799,6513,4817],[6631,13675,2737,15282,10426],[4102,2574,4427,2824,1083,4480,3898,2459,4123,4502,2174,4804,3109,3121,3408],[9018,2604,9256,6568,4487,4133,3395,9266,2025],[3920,1556,6177,5349,6296,13639],[4313,1156,8342,7415,4000,11858,4709],[8780,5173,6162,6344,1941],[1301,4685,1841,2222,3305,3965,6551,7114,5399,3747,1839,6706],[7853,11264],[14768,14746,5840,1731],[4166,1442,2971,8507,1859,5344],[2874,1080,4159,2491,3860,6050,6523,1136,1478,6370,6541,3607,1908],[4652,5783,4407,5261,1017,3165,3589,1771,1700,1308,2280,5742],[4598,4281,2933,5351,2897,3983,6840,4827,3726,7356,5221,4955],[5184,9444,4616,2549,4979,4577],[4998,1639,1189,2612,1072,2130,1044,3009,4132,2712,1223,5394,4698,3702,2456],[5523,6843,6182,4741,6514,5068,4521,2424,6076,7273,3125,6378],[5675,5075,4462,2155,2621,6955,6893,5037,1935,6837,2225,5190],[23738,30667],[3332,4507,4454,3274,6417,5739,1855,5078,2040,2472,6068,5696,1449],[12606,17528],[1761,8980,8137,12561,1838],[23685,5576,2070],[2014,2573,1478,3902,6247,5167,3393,3723,6336,4675,6430,5310,1484,4109],[9842,3374,4250,6939,3975,5106,4823],[27517,29161],[3861,2633,15717],[4392,3840,3178,4148,1419,4242,5498,3890,6033,4700,6624,4499],[7872,1829,9261,3780,3815,11309,5207],[10646,3597,8139,4802,6588,8367,9006],[3268,4153,4070,3310,1132,6237,3285,4461,1418,3315,5840,3931,4168,4059],[12562,15842,4084,5075,4652],[13950,9685,24437],[13322,17454,3563,6272],[3171,4566,2828,2367,3947,3408,4149,3323,2995,5217,2740,5365,1857,3393,5257],[25535,4255,4792],[2725,7987,4898,4443,6620,7232,5562,8511,8551,6817],[4986,1999,9609,4028,7978,7833],[7122,3544,1386,6190,2119,3479,3638,2645,4651,6909,3419,6260],[4884,24630,16892],[1404,3127,3142,5332,1979,1038,4098,3841,3620,3092,5325,3685,5385,3439],[25512,4994,1339],[65852],[5886,2370,2424,7331,4765,8599,3997,4213,1185,4739],[3268,6752,3238,8925,4044,9369,4423,6024],[7615,7582,1946,6493,6969,2369,2498,4231,6898,2301],[13338,15159,1007,1579,4532],[3111,4477,2628,2009,1361,3171,5895,5000,3213,5536],[3384,6723,5558,3445,7104,4590,2005,1142,4239,2804,2136,5493],[8745,8221,2669,3171,5469,2077,7506,1388,6350],[1347,1620,2690,4070,4859,6068,4170,4634,2749,3346,3541,3841,3558,5879,1725],[1630,8268,4370,1609,6617,5419,4991,3865,5722,4530],[5677,17141,16522,13806],[18659,25364,19945],[27851,31094],[4535,5908,4391,1243,1037,4963,1570,5836,3720,3066,2995,4234,3073,5882],[37830],[8609,8530,6527,2145,8647,7720,5906,2524,8666],[1143,1080,4034,2107,2539,4458,5898,2709,4941,1011,1232,3069,1527,2769,5071],[2092,6346,6041,8472,10954,2509,3632],[23978,1578,23954],[3027,2598,1818,5892,2519,5083,3793,5849,1488,3357,1350,6813,4486],[2282,6271,2548,6333,7978,4451,5808,5716,7485,6615,2823],[63025],[5701,2013,5327,3534,4144,1980,5576,5075,5456,3808,2466,5444,4918,6048,4177],[3240,1730,5306,7215,2278,4619,3812,4873,4453],[1467,2152,6616,3839,3422,3110,4447,6771,2967,4994,6764,5722,3680],[58287],[6434,1137,15936,15651,14632],[1074,5120,1340,4497,4947,2195,1243,5123,2076,4464,2615,3780,5693,1208,2180],[14965,10684,2049,10628,9532],[24592],[4560,4156,3363,1751,4886,5216,2888,1327,1285,6123,2701,4501,6007],[6445,1701,3886,3597,3854,2593,3249,3424,1703,3036,1670,5705,5781,1953],[11411,7632,1419,5620,7953,7273,10715],[12735,1860,6909,9353],[6189,10627,9335,5646,5375,6886,3897,5716],[8653,12542,6082,15244],[4819,1613,1332,3125,1897,3675,3688,3393,3794,1146],[2603,6838,6037,5540,2335,6837,4425,3437,5965,3449,4752,5173,1971],[25800,14272,12870],[4840,6051,2429,2372,2533,4184,4051,5449,1321,2738,4450,4473,4618,2936,5818],[9966,9565,4598,6548,6832,1749],[6333,7723,7084,7305,2609,8143,10400,7867],[2652,6421,6630,4124,8490,8618,6380,2636,8045],[1382,6541,2533,3419,3661,4779,5466,6163,5536,4844,2600,1126,4297],[4541,10317,5900,4278,2540,4385,3048,3079],[7542,4938,1913,7746,7472,7621,1307,2142],[12358,6813,15347,14448,4584],[8653,14358,1689,3466,13581],[18460,15717,11921],[5002,1450,1233,5553,2723,3811,4765,1001,4513,5847,4403],[20065,10696],[8007,1090,2875,4585,7707,7823,2009,8498],[9586,8143,8021,4245,7291,3936,11899],[26024],[14163,9586,12371,2127,4436],[2613,6450,3236,7142,6570,6405,2444,3243,6166,7090,3461,6534],[3176,4477,4562,3589,5280,5548,3708,4632,3836,4171,5284,3667,3313,4312],[12262,7518,13586,14339,2951],[30207],[3979,2163,7147,3726,4358,7572,3345,1296,2755,4608],[5723,3754,2684],[19384,3722],[2024,8540,6504,12081,4812,3647,2731],[18117,13193,13899,19853],[3533,4991,2034,3914,5920,2455,2660,3175,1410,1127,2272,3689,5804,1311,1121]]
main :: IO()
main = print $ maxCal input
