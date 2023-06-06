		; Digits font rasterized to bitmap
		; https://www.dafont.com/digits.font

		align 2

font_digits:
.header		db 32			; First glyph's character code
		db 126			; Last glyph's character code
		db 11			; Font height in pixels
		db 4			; Space character width in pixels
		db 1			; Letter spacing (adjust to your liking)
		db 0, 0, 0		; Reserved for future use

		; Index of glyph definition for each character code within the
		; given range. If the offset is 0, the glyph is not present
		; (render a space instead).

		dw .glyph_032 - .header
		dw .glyph_033 - .header
		dw .glyph_034 - .header
		dw .glyph_035 - .header
		dw .glyph_036 - .header
		dw .glyph_037 - .header
		dw .glyph_038 - .header
		dw .glyph_039 - .header
		dw .glyph_040 - .header
		dw .glyph_041 - .header
		dw .glyph_042 - .header
		dw .glyph_043 - .header
		dw .glyph_044 - .header
		dw .glyph_045 - .header
		dw .glyph_046 - .header
		dw .glyph_047 - .header
		dw .glyph_048 - .header
		dw .glyph_049 - .header
		dw .glyph_050 - .header
		dw .glyph_051 - .header
		dw .glyph_052 - .header
		dw .glyph_053 - .header
		dw .glyph_054 - .header
		dw .glyph_055 - .header
		dw .glyph_056 - .header
		dw .glyph_057 - .header
		dw .glyph_058 - .header
		dw .glyph_059 - .header
		dw .glyph_060 - .header
		dw .glyph_061 - .header
		dw .glyph_062 - .header
		dw .glyph_063 - .header
		dw .glyph_064 - .header
		dw .glyph_065 - .header
		dw .glyph_066 - .header
		dw .glyph_067 - .header
		dw .glyph_068 - .header
		dw .glyph_069 - .header
		dw .glyph_070 - .header
		dw .glyph_071 - .header
		dw .glyph_072 - .header
		dw .glyph_073 - .header
		dw .glyph_074 - .header
		dw .glyph_075 - .header
		dw .glyph_076 - .header
		dw .glyph_077 - .header
		dw .glyph_078 - .header
		dw .glyph_079 - .header
		dw .glyph_080 - .header
		dw .glyph_081 - .header
		dw .glyph_082 - .header
		dw .glyph_083 - .header
		dw .glyph_084 - .header
		dw .glyph_085 - .header
		dw .glyph_086 - .header
		dw .glyph_087 - .header
		dw .glyph_088 - .header
		dw .glyph_089 - .header
		dw .glyph_090 - .header
		dw .glyph_091 - .header
		dw .glyph_092 - .header
		dw .glyph_093 - .header
		dw .glyph_094 - .header
		dw .glyph_095 - .header
		dw .glyph_096 - .header
		dw .glyph_097 - .header
		dw .glyph_098 - .header
		dw .glyph_099 - .header
		dw .glyph_100 - .header
		dw .glyph_101 - .header
		dw .glyph_102 - .header
		dw .glyph_103 - .header
		dw .glyph_104 - .header
		dw .glyph_105 - .header
		dw .glyph_106 - .header
		dw .glyph_107 - .header
		dw .glyph_108 - .header
		dw .glyph_109 - .header
		dw .glyph_110 - .header
		dw .glyph_111 - .header
		dw .glyph_112 - .header
		dw .glyph_113 - .header
		dw .glyph_114 - .header
		dw .glyph_115 - .header
		dw .glyph_116 - .header
		dw .glyph_117 - .header
		dw .glyph_118 - .header
		dw .glyph_119 - .header
		dw .glyph_120 - .header
		dw .glyph_121 - .header
		dw .glyph_122 - .header
		dw .glyph_123 - .header
		dw .glyph_124 - .header
		dw .glyph_125 - .header
		dw .glyph_126 - .header

		; Character glyph definitions
		; First 4 bytes in each glyph are:
		; [0]: Number of bytes per row
		; [1]: Character width in pixels (bitmap packed to upper bits)
		; [2]: Character height in pixels (0 for empty characters)
		; [3]: Number of rows in pixels to skip above character
		; Followed by [0] * [2] bytes of character bitmap data.

.glyph_032	db 1, 4, 0, 9

.glyph_033	db 1, 2, 8, 1
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 00000000b
		db 11000000b

.glyph_034	db 1, 3, 2, 1
		db 10100000b
		db 10100000b

.glyph_035	db 1, 5, 8, 1
		db 01010000b
		db 01010000b
		db 11111000b
		db 01010000b
		db 01010000b
		db 11111000b
		db 01010000b
		db 01010000b

.glyph_036	db 1, 6, 10, 0
		db 00110000b
		db 01111000b
		db 11001100b
		db 11000000b
		db 01111000b
		db 00001100b
		db 11001100b
		db 11001100b
		db 01111000b
		db 00110000b

.glyph_037	db 1, 6, 8, 1
		db 01000100b
		db 10100100b
		db 10101000b
		db 01010000b
		db 00101000b
		db 01010100b
		db 10010100b
		db 10001000b

.glyph_038	db 1, 6, 10, 0
		db 00110000b
		db 01111000b
		db 11001100b
		db 11000000b
		db 00110000b
		db 11000000b
		db 11001100b
		db 11001100b
		db 01111000b
		db 00110000b

.glyph_039	db 1, 1, 2, 1
		db 10000000b
		db 10000000b

.glyph_040	db 1, 3, 8, 1
		db 01100000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 01100000b

.glyph_041	db 1, 3, 8, 1
		db 11000000b
		db 01100000b
		db 01100000b
		db 01100000b
		db 01100000b
		db 01100000b
		db 01100000b
		db 11000000b

.glyph_042	db 1, 3, 3, 1
		db 10100000b
		db 01000000b
		db 10100000b

.glyph_043	db 1, 5, 5, 3
		db 00100000b
		db 00100000b
		db 11111000b
		db 00100000b
		db 00100000b

.glyph_044	db 1, 2, 2, 8
		db 11000000b
		db 01000000b

.glyph_045	db 1, 5, 1, 5
		db 11111000b

.glyph_046	db 1, 2, 1, 8
		db 11000000b

.glyph_047	db 1, 5, 8, 1
		db 00011000b
		db 00011000b
		db 00110000b
		db 00110000b
		db 01100000b
		db 01100000b
		db 11000000b
		db 11000000b

.glyph_048	db 1, 6, 8, 1
		db 01111000b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 01111000b

.glyph_049	db 1, 3, 8, 1
		db 01100000b
		db 11100000b
		db 01100000b
		db 01100000b
		db 01100000b
		db 01100000b
		db 01100000b
		db 01100000b

.glyph_050	db 1, 6, 8, 1
		db 01111000b
		db 11001100b
		db 11001100b
		db 00011000b
		db 00110000b
		db 01100000b
		db 11001100b
		db 11111100b

.glyph_051	db 1, 6, 8, 1
		db 01111000b
		db 11001100b
		db 00001100b
		db 00111000b
		db 00001100b
		db 11001100b
		db 11001100b
		db 01111000b

.glyph_052	db 1, 6, 8, 1
		db 00111100b
		db 01101100b
		db 11001100b
		db 11001100b
		db 11111100b
		db 00001100b
		db 00001100b
		db 00001100b

.glyph_053	db 1, 6, 8, 1
		db 11111100b
		db 11000000b
		db 11000000b
		db 11111000b
		db 00001100b
		db 00001100b
		db 11001100b
		db 11111000b

.glyph_054	db 1, 6, 8, 1
		db 01111000b
		db 11001100b
		db 11000000b
		db 11111000b
		db 11001100b
		db 11001100b
		db 11001100b
		db 01111000b

.glyph_055	db 1, 6, 8, 1
		db 11111100b
		db 00001100b
		db 00001100b
		db 00011000b
		db 00110000b
		db 00110000b
		db 00110000b
		db 00110000b

.glyph_056	db 1, 6, 8, 1
		db 01111000b
		db 11001100b
		db 11001100b
		db 01111000b
		db 11001100b
		db 11001100b
		db 11001100b
		db 01111000b

.glyph_057	db 1, 6, 8, 1
		db 01111000b
		db 11001100b
		db 11001100b
		db 01111100b
		db 00001100b
		db 11001100b
		db 11001100b
		db 01111000b

.glyph_058	db 1, 2, 3, 6
		db 11000000b
		db 00000000b
		db 11000000b

.glyph_059	db 1, 2, 4, 6
		db 11000000b
		db 00000000b
		db 11000000b
		db 01000000b

.glyph_060	db 1, 3, 5, 4
		db 00100000b
		db 01000000b
		db 10000000b
		db 01000000b
		db 00100000b

.glyph_061	db 1, 4, 3, 5
		db 11110000b
		db 00000000b
		db 11110000b

.glyph_062	db 1, 3, 5, 4
		db 10000000b
		db 01000000b
		db 00100000b
		db 01000000b
		db 10000000b

.glyph_063	db 1, 6, 8, 1
		db 01111000b
		db 11001100b
		db 11001100b
		db 11001100b
		db 00011100b
		db 00110000b
		db 00000000b
		db 00110000b

.glyph_064	db 1, 7, 8, 1
		db 01111100b
		db 10000010b
		db 10111010b
		db 10101010b
		db 10111010b
		db 10001110b
		db 10000000b
		db 01111100b

.glyph_065	db 1, 6, 8, 1
		db 00110000b
		db 01001000b
		db 11001100b
		db 11001100b
		db 11111100b
		db 11001100b
		db 11001100b
		db 11001100b

.glyph_066	db 1, 6, 8, 1
		db 11111000b
		db 11001100b
		db 11001100b
		db 11111000b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11111000b

.glyph_067	db 1, 6, 8, 1
		db 01111000b
		db 11001100b
		db 11001100b
		db 11000000b
		db 11000000b
		db 11001100b
		db 11001100b
		db 01111000b

.glyph_068	db 1, 6, 8, 1
		db 11111000b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11111000b

.glyph_069	db 1, 6, 8, 1
		db 11111100b
		db 11000000b
		db 11000000b
		db 11111000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11111100b

.glyph_070	db 1, 6, 8, 1
		db 11111100b
		db 11000000b
		db 11000000b
		db 11111000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b

.glyph_071	db 1, 6, 8, 1
		db 01111000b
		db 11001100b
		db 11001100b
		db 11000000b
		db 11000000b
		db 11011100b
		db 11001100b
		db 01111100b

.glyph_072	db 1, 6, 8, 1
		db 11001100b
		db 11001100b
		db 11001100b
		db 11111100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b

.glyph_073	db 1, 2, 8, 1
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b

.glyph_074	db 1, 6, 8, 1
		db 00001100b
		db 00001100b
		db 00001100b
		db 00001100b
		db 00001100b
		db 11001100b
		db 11001100b
		db 01111000b

.glyph_075	db 1, 6, 8, 1
		db 11001100b
		db 11001100b
		db 11011000b
		db 11110000b
		db 11011000b
		db 11001100b
		db 11001100b
		db 11001100b

.glyph_076	db 1, 6, 8, 1
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11111100b

.glyph_077	db 1, 7, 8, 1
		db 11000110b
		db 11101110b
		db 11010110b
		db 11000110b
		db 11000110b
		db 11000110b
		db 11000110b
		db 11000110b

.glyph_078	db 1, 6, 8, 1
		db 11001100b
		db 11001100b
		db 11101100b
		db 11011100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b

.glyph_079	db 1, 6, 8, 1
		db 01111000b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 01111000b

.glyph_080	db 1, 6, 8, 1
		db 11111000b
		db 11001100b
		db 11001100b
		db 11111000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b

.glyph_081	db 1, 6, 8, 1
		db 01111000b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11011100b
		db 01111100b

.glyph_082	db 1, 6, 8, 1
		db 11111000b
		db 11001100b
		db 11001100b
		db 11111000b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b

.glyph_083	db 1, 6, 8, 1
		db 01111000b
		db 11001100b
		db 11000000b
		db 01111000b
		db 00001100b
		db 11001100b
		db 11001100b
		db 01111000b

.glyph_084	db 1, 6, 8, 1
		db 11111100b
		db 00110000b
		db 00110000b
		db 00110000b
		db 00110000b
		db 00110000b
		db 00110000b
		db 00110000b

.glyph_085	db 1, 6, 8, 1
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 01111000b

.glyph_086	db 1, 6, 8, 1
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 01001000b
		db 01111000b
		db 00110000b

.glyph_087	db 1, 7, 8, 1
		db 11000110b
		db 11000110b
		db 11000110b
		db 11000110b
		db 11000110b
		db 11010110b
		db 11101110b
		db 11000110b

.glyph_088	db 1, 6, 8, 1
		db 11001100b
		db 11001100b
		db 11001100b
		db 01111000b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b

.glyph_089	db 1, 6, 8, 1
		db 11001100b
		db 11001100b
		db 11001100b
		db 01111000b
		db 00110000b
		db 00110000b
		db 00110000b
		db 00110000b

.glyph_090	db 1, 6, 8, 1
		db 11111100b
		db 00001100b
		db 00001100b
		db 00010000b
		db 00100000b
		db 11000000b
		db 11000000b
		db 11111100b

.glyph_091	db 1, 2, 8, 1
		db 11000000b
		db 10000000b
		db 10000000b
		db 10000000b
		db 10000000b
		db 10000000b
		db 10000000b
		db 11000000b

.glyph_092	db 1, 5, 8, 1
		db 11000000b
		db 11000000b
		db 01100000b
		db 01100000b
		db 00110000b
		db 00110000b
		db 00011000b
		db 00011000b

.glyph_093	db 1, 2, 8, 1
		db 11000000b
		db 01000000b
		db 01000000b
		db 01000000b
		db 01000000b
		db 01000000b
		db 01000000b
		db 11000000b

.glyph_094	db 1, 5, 3, 1
		db 00100000b
		db 01010000b
		db 10001000b

.glyph_095	db 1, 4, 1, 8
		db 11110000b

.glyph_096	db 1, 2, 2, 1
		db 10000000b
		db 01000000b

.glyph_097	db 1, 6, 5, 4
		db 01111100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 01111100b

.glyph_098	db 1, 6, 8, 1
		db 11000000b
		db 11000000b
		db 11000000b
		db 11111000b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11111000b

.glyph_099	db 1, 6, 5, 4
		db 01111000b
		db 11001100b
		db 11000000b
		db 11001100b
		db 01111000b

.glyph_100	db 1, 6, 8, 1
		db 00001100b
		db 00001100b
		db 00001100b
		db 01111100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 01111100b

.glyph_101	db 1, 6, 5, 4
		db 01111000b
		db 11001100b
		db 11111100b
		db 11000000b
		db 01111100b

.glyph_102	db 1, 6, 8, 1
		db 01111000b
		db 11001100b
		db 11001100b
		db 11000000b
		db 11111100b
		db 11000000b
		db 11000000b
		db 11000000b

.glyph_103	db 1, 6, 7, 4
		db 01111100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 01111100b
		db 00001100b
		db 11111000b

.glyph_104	db 1, 6, 8, 1
		db 11000000b
		db 11000000b
		db 11000000b
		db 11111000b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b

.glyph_105	db 1, 2, 7, 2
		db 11000000b
		db 00000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b

.glyph_106	db 1, 2, 9, 2
		db 11000000b
		db 00000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b

.glyph_107	db 1, 6, 8, 1
		db 11000000b
		db 11000000b
		db 11000000b
		db 11001100b
		db 11001100b
		db 11110000b
		db 11001100b
		db 11001100b

.glyph_108	db 1, 2, 8, 1
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b

.glyph_109	db 1, 7, 5, 4
		db 11111100b
		db 11010110b
		db 11010110b
		db 11010110b
		db 11010110b

.glyph_110	db 1, 6, 5, 4
		db 11111000b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b

.glyph_111	db 1, 6, 5, 4
		db 01111000b
		db 11001100b
		db 11001100b
		db 11001100b
		db 01111000b

.glyph_112	db 1, 6, 7, 4
		db 11111000b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11111000b
		db 11000000b
		db 11000000b

.glyph_113	db 1, 6, 7, 4
		db 01111100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 01111100b
		db 00001100b
		db 00001100b

.glyph_114	db 1, 6, 5, 4
		db 11111000b
		db 11001100b
		db 11001100b
		db 11000000b
		db 11000000b

.glyph_115	db 1, 6, 5, 4
		db 11111100b
		db 11000000b
		db 11111100b
		db 00001100b
		db 11111100b

.glyph_116	db 1, 4, 7, 2
		db 01100000b
		db 01100000b
		db 11110000b
		db 01100000b
		db 01100000b
		db 01100000b
		db 01100000b

.glyph_117	db 1, 6, 5, 4
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 01111100b

.glyph_118	db 1, 6, 5, 4
		db 11001100b
		db 11001100b
		db 11001100b
		db 01111000b
		db 00110000b

.glyph_119	db 1, 7, 5, 4
		db 11010110b
		db 11010110b
		db 11010110b
		db 11010110b
		db 01111110b

.glyph_120	db 1, 6, 5, 4
		db 11001100b
		db 11001100b
		db 01111000b
		db 11001100b
		db 11001100b

.glyph_121	db 1, 6, 7, 4
		db 11001100b
		db 11001100b
		db 11001100b
		db 11001100b
		db 11111100b
		db 00001100b
		db 11111000b

.glyph_122	db 1, 6, 5, 4
		db 11111100b
		db 00001100b
		db 00110000b
		db 11000000b
		db 11111100b

.glyph_123	db 1, 3, 8, 1
		db 01100000b
		db 11000000b
		db 11000000b
		db 01100000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 01100000b

.glyph_124	db 1, 2, 8, 1
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b
		db 11000000b

.glyph_125	db 1, 3, 8, 1
		db 11000000b
		db 01100000b
		db 01100000b
		db 11000000b
		db 01100000b
		db 01100000b
		db 01100000b
		db 11000000b

.glyph_126	db 1, 4, 2, 4
		db 01010000b
		db 10100000b
