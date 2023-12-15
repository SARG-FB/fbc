''debug_int.bi
''debug internal

#if (__FB_DEBUG__ <> 0) orelse defined(__GAS64_DEBUG__)
declare function vregDumpToStr( byval v as IRVREG ptr ) as string
declare sub vregDump( byval v as IRVREG ptr )
#endif

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

declare function emitDumpRegName( byval dtype as integer, byval reg as integer ) as string

declare sub hDumpFreeIntRegs( )
declare sub hDump _
	( _
		byval op as integer, _
		byval v1 as IRVREG ptr, _
		byval v2 as IRVREG ptr, _
		byval vr as IRVREG ptr, _
		byval wrapline as integer = FALSE _
	)
declare function tacvregDump( byval tacvreg as IRTACVREG ptr ) as string
declare sub tacDump( byval tac as IRTAC ptr )

#endif