;==============================================================================
; MOD player - data conversions
;==============================================================================

cpu 386

%include "mod/structs/global.inc"
%include "debug/log.inc"

segment modplayer public use16 class=CODE align=16
segment modplayer


;------------------------------------------------------------------------------
; Convert MOD period to note.
;------------------------------------------------------------------------------
; -> CX - Period
; <- BX - Note (always an 8-bit value, but zero extended)
;------------------------------------------------------------------------------

global mod_convert_period_to_note
mod_convert_period_to_note:
	mov bx, notetab

.loop_notetab:
	cmp cs:[bx], cx
	jbe .use_note
	add bx, 2
	cmp bx, notetab_end
	jb .loop_notetab

.use_note:
	sub bx, notetab
	shr bx, 1

	retn


;------------------------------------------------------------------------------
; Adjust note with finetune value.
;------------------------------------------------------------------------------
; -> BH - Note
;    AH - Finetune (-8 - 7)
; <- CX - Period * 16
;------------------------------------------------------------------------------

global mod_note_finetune
mod_note_finetune:
	push ax
	push ebx
	push es

	mov cx, modplayer_data
	mov es, cx

	mov al, PERTAB_LEN
	cmp bh, al			; Limit note to highest note
	jb .use_note
	mov bh, PERTAB_LEN - 1

.use_note:
	imul ah
	movzx bx, bh
	add bx, ax
	add bx, bx
	mov cx, es:[pertab + bx]

	pop es
	pop ebx
	pop ax
	retn


;------------------------------------------------------------------------------
; Round down period to nearest seminote.
;------------------------------------------------------------------------------
; -> CX - Period * 16
;    AH - Finetune (-8 - 7)
; <- CX - Period * 16 rounded down to nearest semitone
;------------------------------------------------------------------------------

global mod_period_floor_seminote
mod_period_floor_seminote:
	push ax
	push ebx
	push dx
	push es

	mov bx, modplayer_data
	mov es, bx

	mov dx, cx			; DX: period * 16
	mov al, PERTAB_LEN
	movzx cx, al
	dec cx				; CX: period table length - 1
	imul ah
	movsx ebx, ax			; EBX: index into finetuned period table

.find_period:
	cmp dx, es:[pertab + ebx * 2]
	jae .found_period
	inc ebx
	loop .find_period, cx

.found_period:
	mov cx, es:[pertab + ebx * 2]

	pop es
	pop dx
	pop ebx
	pop ax
	retn


;==============================================================================
; Data area
;==============================================================================

		;--------------------------------------------------------------
		; Period to note conversion table

		alignb 2
notetab		dw 3424, 3232, 3048, 2880, 2712, 2560, 2416, 2280, 2152, 2032, 1920, 1812,
		dw 1712, 1616, 1524, 1440, 1356, 1280, 1208, 1140, 1076, 1016, 960, 906,
		dw 856, 808, 762, 720, 678, 640, 604, 570, 538, 508, 480, 453,
		dw 428, 404, 381, 360, 339, 320, 302, 285, 269, 254, 240, 226,
		dw 214, 202, 190, 180, 170, 160, 151, 143, 135, 127, 120, 113,
		dw 107, 101, 95, 90, 85, 80, 75, 71, 67, 63, 60, 56,
		dw 53, 50, 47, 45, 42, 40, 37, 35, 33, 31, 30, 28
notetab_end:


segment modplayer_data public use16 class=DATA align=16
segment modplayer_data

		;--------------------------------------------------------------
		; Period * 16 tables
		;--------------------------------------------------------------
		; This is all but trivial... Periods are such a central
		; component of a MOD file and playroutine there is no way around
		; this Amiga-specific thing. Everything is alright as long as
		; one stays with ProTracker MODs only. Soon however, PCs started
		; catching up and trackers with 8+ channels became common. PCs
		; never had the limitations of the Amiga and could mix a much
		; wider range; trackers quickly started to support additional
		; octaves above/below the ProTracker range. With higher octaves,
		; precision decreases rapidly and it gets impossible to apply
		; any finetune. Therefore this player multiplies period by 16
		; internally so it can use more accurate values for higher
		; octaves, especially with finetune. A slight detune will still
		; remain, any attempt to fix this would introduce inaccuracy or
		; out-of-tune between sounds 2 octaves apart.

		alignb 2

		; Tuning -8
		dw 58048, 54784, 51712, 48768, 46080, 43392, 40960, 38656, 36480, 34432, 32512, 30720
		dw 29024, 27392, 25856, 24384, 23040, 21696, 20480, 19328, 18240, 17216, 16256, 15360
		dw 14512, 13696, 12928, 12192, 11520, 10848, 10240, 9664, 9120, 8608, 8128, 7680
		dw 7248, 6848, 6464, 6096, 5760, 5424, 5120, 4832, 4560, 4304, 4064, 3840
		dw 3616, 3424, 3232, 3040, 2880, 2720, 2560, 2416, 2288, 2160, 2032, 1920
		dw 1808, 1712, 1616, 1520, 1440, 1360, 1280, 1208, 1144, 1080, 1016, 960
		dw 904, 856, 808, 760, 720, 680, 640, 604, 572, 540, 508, 480

		; Tuning -7
		dw 57600, 54400, 51328, 48448, 45760, 43200, 40704, 38464, 36288, 34240, 32320, 30528
		dw 28800, 27200, 25664, 24224, 22880, 21600, 20352, 19232, 18144, 17120, 16160, 15264
		dw 14400, 13600, 12832, 12112, 11440, 10800, 10176, 9616, 9072, 8560, 8080, 7632
		dw 7200, 6800, 6416, 6064, 5712, 5392, 5088, 4800, 4544, 4288, 4048, 3808
		dw 3600, 3392, 3200, 3024, 2864, 2704, 2544, 2400, 2272, 2144, 2016, 1904
		dw 1800, 1696, 1600, 1512, 1432, 1352, 1272, 1200, 1136, 1072, 1008, 952
		dw 900, 848, 800, 756, 716, 676, 636, 600, 568, 536, 504, 476

		; Tuning -6
		dw 57216, 54016, 50944, 48128, 45376, 42880, 40448, 38208, 36032, 34048, 32128, 30336
		dw 28608, 27008, 25472, 24064, 22688, 21440, 20224, 19104, 18016, 17024, 16064, 15168
		dw 14304, 13504, 12736, 12032, 11344, 10720, 10112, 9552, 9008, 8512, 8032, 7584
		dw 7152, 6752, 6368, 6016, 5680, 5360, 5056, 4768, 4512, 4256, 4016, 3792
		dw 3568, 3376, 3184, 3008, 2832, 2672, 2528, 2384, 2256, 2128, 2000, 1888
		dw 1784, 1688, 1592, 1504, 1416, 1336, 1264, 1192, 1128, 1064, 1000, 944
		dw 892, 844, 796, 752, 708, 668, 632, 596, 564, 532, 500, 472

		; Tuning -5
		dw 56768, 53632, 50624, 47744, 45056, 42560, 40192, 37888, 35776, 33792, 31872, 30080
		dw 28384, 26816, 25312, 23872, 22528, 21280, 20096, 18944, 17888, 16896, 15936, 15040
		dw 14192, 13408, 12656, 11936, 11264, 10640, 10048, 9472, 8944, 8448, 7968, 7520
		dw 7104, 6704, 6320, 5968, 5632, 5312, 5024, 4736, 4480, 4224, 3984, 3760
		dw 3552, 3344, 3168, 2992, 2816, 2656, 2512, 2368, 2240, 2112, 2000, 1888
		dw 1776, 1672, 1584, 1496, 1408, 1328, 1256, 1184, 1120, 1056, 1000, 944
		dw 888, 836, 792, 748, 704, 664, 628, 592, 560, 528, 500, 472

		; Tuning -4
		dw 56384, 53248, 50240, 47424, 44736, 42240, 39872, 37632, 35520, 33536, 31616, 29888
		dw 28192, 26624, 25120, 23712, 22368, 21120, 19936, 18816, 17760, 16768, 15808, 14944
		dw 14096, 13312, 12560, 11856, 11184, 10560, 9968, 9408, 8880, 8384, 7904, 7472
		dw 7056, 6656, 6272, 5920, 5600, 5280, 4992, 4704, 4448, 4192, 3952, 3728
		dw 3520, 3328, 3136, 2960, 2800, 2640, 2496, 2352, 2224, 2096, 1968, 1872
		dw 1760, 1664, 1568, 1480, 1400, 1320, 1248, 1176, 1112, 1048, 984, 936
		dw 880, 832, 784, 740, 700, 660, 624, 588, 556, 524, 492, 468

		; Tuning -3
		dw 56000, 52864, 49856, 47104, 44416, 41920, 39616, 37376, 35264, 33280, 31424, 29632
		dw 28000, 26432, 24928, 23552, 22208, 20960, 19808, 18688, 17632, 16640, 15712, 14816
		dw 14000, 13216, 12464, 11776, 11104, 10480, 9904, 9344, 8816, 8320, 7856, 7408
		dw 6992, 6608, 6240, 5888, 5552, 5248, 4944, 4672, 4416, 4160, 3920, 3712
		dw 3504, 3296, 3120, 2944, 2784, 2624, 2480, 2336, 2208, 2080, 1968, 1856
		dw 1752, 1648, 1560, 1472, 1392, 1312, 1240, 1168, 1104, 1040, 984, 928
		dw 876, 824, 780, 736, 696, 656, 620, 584, 552, 520, 492, 464

		; Tuning -2
		dw 55552, 52480, 49536, 46720, 44096, 41664, 39296, 37120, 35008, 33024, 31168, 29440
		dw 27776, 26240, 24768, 23360, 22048, 20832, 19648, 18560, 17504, 16512, 15584, 14720
		dw 13888, 13120, 12384, 11680, 11024, 10416, 9824, 9280, 8752, 8256, 7792, 7360
		dw 6944, 6560, 6192, 5840, 5520, 5200, 4912, 4640, 4384, 4128, 3904, 3680
		dw 3472, 3280, 3088, 2928, 2752, 2608, 2464, 2320, 2192, 2064, 1952, 1840
		dw 1736, 1640, 1544, 1464, 1376, 1304, 1232, 1160, 1096, 1032, 976, 920
		dw 868, 820, 772, 732, 688, 652, 616, 580, 548, 516, 488, 460

		; Tuning -1
		dw 55168, 52096, 49152, 46400, 43776, 41344, 39040, 36800, 34752, 32832, 30976, 29248
		dw 27584, 26048, 24576, 23200, 21888, 20672, 19520, 18400, 17376, 16416, 15488, 14624
		dw 13792, 13024, 12288, 11600, 10944, 10336, 9760, 9200, 8688, 8208, 7744, 7312
		dw 6896, 6512, 6144, 5808, 5472, 5168, 4880, 4608, 4352, 4096, 3872, 3648
		dw 3456, 3248, 3072, 2896, 2736, 2576, 2432, 2304, 2176, 2048, 1936, 1824
		dw 1728, 1624, 1536, 1448, 1368, 1288, 1216, 1152, 1088, 1024, 968, 912
		dw 864, 812, 768, 724, 684, 644, 608, 576, 544, 512, 484, 456

		;--------------------------------------------------------------
		; Tuning 0
		;--------------------------------------------------------------
		; Extend beyond ProTracker range by 2 octaves
pertab		dw 54784, 51712, 48768, 46080, 43392, 40960, 38656, 36480, 34432, 32512, 30720, 28992
		dw 27392, 25856, 24384, 23040, 21696, 20480, 19328, 18240, 17216, 16256, 15360, 14496
		;--------------------------------------------------------------
		; ProTracker range
		dw 13696, 12928, 12192, 11520, 10848, 10240, 9664, 9120, 8608, 8128, 7680, 7248
		dw 6848, 6464, 6096, 5760, 5424, 5120, 4832, 4560, 4304, 4064, 3840, 3616
		dw 3424, 3232, 3040, 2880, 2720, 2560, 2416, 2288, 2160, 2032, 1920, 1808
		;--------------------------------------------------------------
		; Extend above ProTracker range by 2 octaves
		dw 1712, 1616, 1520, 1440, 1360, 1280, 1208, 1144, 1080, 1016, 960, 904
		dw 856, 808, 760, 720, 680, 640, 604, 572, 540, 508, 480, 452
		;--------------------------------------------------------------
PERTAB_LEN	EQU ($ - pertab) / 2

		; Tuning 1
		dw 54400, 51328, 48448, 45760, 43136, 40768, 38464, 36288, 34240, 32320, 30528, 28800
		dw 27200, 25664, 24224, 22880, 21568, 20384, 19232, 18144, 17120, 16160, 15264, 14400
		dw 13600, 12832, 12112, 11440, 10784, 10192, 9616, 9072, 8560, 8080, 7632, 7200
		dw 6800, 6416, 6064, 5712, 5392, 5088, 4800, 4544, 4288, 4048, 3824, 3600
		dw 3408, 3216, 3024, 2864, 2704, 2544, 2400, 2272, 2144, 2016, 1904, 1808
		dw 1704, 1608, 1512, 1432, 1352, 1272, 1200, 1136, 1072, 1008, 952, 904
		dw 852, 804, 756, 716, 676, 636, 600, 568, 536, 504, 476, 452

		; Tuning 2
		dw 54016, 50944, 48128, 45376, 42880, 40448, 38208, 36032, 34048, 32128, 30336, 28608
		dw 27008, 25472, 24064, 22688, 21440, 20224, 19104, 18016, 17024, 16064, 15168, 14304
		dw 13504, 12736, 12032, 11344, 10720, 10112, 9552, 9008, 8512, 8032, 7584, 7152
		dw 6752, 6368, 6016, 5680, 5360, 5056, 4768, 4512, 4256, 4016, 3792, 3584
		dw 3376, 3184, 3008, 2832, 2672, 2528, 2384, 2256, 2128, 2000, 1888, 1792
		dw 1688, 1592, 1504, 1416, 1336, 1264, 1192, 1128, 1064, 1000, 944, 896
		dw 844, 796, 752, 708, 668, 632, 596, 564, 532, 500, 472, 448

		; Tuning 3
		dw 53632, 50624, 47744, 45056, 42560, 40192, 37888, 35776, 33792, 31872, 30080, 28416
		dw 26816, 25312, 23872, 22528, 21280, 20096, 18944, 17888, 16896, 15936, 15040, 14208
		dw 13408, 12656, 11936, 11264, 10640, 10048, 9472, 8944, 8448, 7968, 7520, 7104
		dw 6704, 6320, 5968, 5632, 5312, 5024, 4736, 4480, 4224, 3984, 3760, 3552
		dw 3344, 3168, 2992, 2816, 2656, 2512, 2368, 2240, 2112, 2000, 1888, 1776
		dw 1672, 1584, 1496, 1408, 1328, 1256, 1184, 1120, 1056, 1000, 944, 888
		dw 836, 792, 748, 704, 664, 628, 592, 560, 528, 500, 472, 444

		; Tuning 4
		dw 53248, 50240, 47424, 44736, 42240, 39872, 37632, 35520, 33536, 31680, 29888, 28224
		dw 26624, 25120, 23712, 22368, 21120, 19936, 18816, 17760, 16768, 15840, 14944, 14112
		dw 13312, 12560, 11856, 11184, 10560, 9968, 9408, 8880, 8384, 7920, 7472, 7056
		dw 6656, 6272, 5920, 5600, 5280, 4992, 4704, 4448, 4192, 3952, 3728, 3520
		dw 3328, 3136, 2960, 2800, 2640, 2496, 2352, 2224, 2096, 1984, 1872, 1760
		dw 1664, 1568, 1480, 1400, 1320, 1248, 1176, 1112, 1048, 992, 936, 880
		dw 832, 784, 740, 700, 660, 624, 588, 556, 524, 496, 468, 440

		; Tuning 5
		dw 52864, 49856, 47104, 44416, 41920, 39616, 37376, 35264, 33280, 31424, 29632, 27968
		dw 26432, 24928, 23552, 22208, 20960, 19808, 18688, 17632, 16640, 15712, 14816, 13984
		dw 13216, 12464, 11776, 11104, 10480, 9904, 9344, 8816, 8320, 7856, 7408, 6992
		dw 6608, 6240, 5888, 5552, 5248, 4944, 4672, 4416, 4160, 3920, 3712, 3504
		dw 3296, 3120, 2944, 2784, 2624, 2480, 2336, 2208, 2080, 1968, 1856, 1744
		dw 1648, 1560, 1472, 1392, 1312, 1240, 1168, 1104, 1040, 984, 928, 872
		dw 824, 780, 736, 696, 656, 620, 584, 552, 520, 492, 464, 436

		; Tuning 6
		dw 52480, 49536, 46720, 44096, 41664, 39296, 37120, 35008, 33024, 31168, 29440, 27776
		dw 26240, 24768, 23360, 22048, 20832, 19648, 18560, 17504, 16512, 15584, 14720, 13888
		dw 13120, 12384, 11680, 11024, 10416, 9824, 9280, 8752, 8256, 7792, 7360, 6944
		dw 6560, 6192, 5840, 5520, 5200, 4912, 4640, 4384, 4128, 3904, 3680, 3472
		dw 3280, 3088, 2928, 2752, 2608, 2464, 2320, 2192, 2064, 1952, 1840, 1744
		dw 1640, 1544, 1464, 1376, 1304, 1232, 1160, 1096, 1032, 976, 920, 872
		dw 820, 772, 732, 688, 652, 616, 580, 548, 516, 488, 460, 436

		; Tuning 7
		dw 52096, 49152, 46400, 43776, 41344, 39040, 36800, 34752, 32832, 30976, 29248, 27584
		dw 26048, 24576, 23200, 21888, 20672, 19520, 18400, 17376, 16416, 15488, 14624, 13792
		dw 13024, 12288, 11600, 10944, 10336, 9760, 9200, 8688, 8208, 7744, 7312, 6896
		dw 6512, 6144, 5808, 5472, 5168, 4880, 4608, 4352, 4096, 3872, 3648, 3456
		dw 3264, 3072, 2896, 2736, 2576, 2432, 2304, 2176, 2048, 1936, 1824, 1728
		dw 1632, 1536, 1448, 1368, 1288, 1216, 1152, 1088, 1024, 968, 912, 864
		dw 816, 768, 724, 684, 644, 608, 576, 544, 512, 484, 456, 432
