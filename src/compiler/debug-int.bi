''debug_int.bi
''debug internal
#if __FB_DEBUG__

#include once "reg.bi"


declare sub ppDumpTree _
	( _
		byval optimize as integer = FALSE _
	)

declare sub ppLookup _
	( _
	)

declare sub regDump2( byval this_ as REGCLASS ptr )


'' #if (__FB_DEBUG__ <> 0) orelse defined(__GAS64_DEBUG__)
declare function emitDumpRegName( byval dtype as integer, byval reg as integer ) as string

#endif