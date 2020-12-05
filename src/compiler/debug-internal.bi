
#if __FB_DEBUG__
#include once "fbint.bi"
#include once "symb.bi"
#include once "reg.bi"

declare sub regDump2( byval this_ as REGCLASS ptr )


declare function hGetRegName( byval dtype as integer, byval reg as integer ) as zstring ptr
declare function hGetNamespacePrefix( byval sym as FBSYMBOL ptr ) as string
declare sub hemitudt2( byval sym as FBSYMBOL ptr )
declare function hHasDtor( byval sym as FBSYMBOL ptr ) as integer
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

declare sub astDtorListDump( )

declare function astDumpInline( byval n as ASTNODE ptr ) as string
declare sub astDumpSmall( byval n as ASTNODE ptr, byref prefix as string = "" )

declare function typeDumpToStr _
	( _
		byval dtype as integer, _
		byval subtype as FBSYMBOL ptr, _
		byval verbose as boolean = false _
	) as string
'' For debugging, e.g. use like this:
''  symbTrace(a), "(replacing this)"
''  symbTrace(b), "(with this)"
#define symbTrace( s ) print __FUNCTION__ + "(" & __LINE__ & "): "; symbDumpToStr( s )

declare function symbDumpToStr _
	( _
		byval s as FBSYMBOL ptr, _
		byval verbose as boolean = false _
	) as string

declare sub symbDump( byval s as FBSYMBOL ptr )
declare sub symbDumpNamespace( byval ns as FBSYMBOL ptr )
declare sub symbDumpChain( byval chain_ as FBSYMCHAIN ptr )

'' FBARRAY: 6 pointer/integer fields + the dimTB with 3 integer fields per dimension
#define symbDescriptorHasRoomFor( sym, dimensions ) (symbGetLen( sym ) = env.pointersize * (((dimensions) * 3) + 6))

declare function symbDumpPrettyToStr( byval sym as FBSYMBOL ptr ) as string

declare function emitDumpRegName( byval dtype as integer, byval reg as integer ) as string

declare function vregDumpToStr( byval v as IRVREG ptr ) as string
declare sub vregDump( byval v as IRVREG ptr )

declare function vregdumpfull( byval v as IRVREG ptr ) as string
declare function vregpretty( byval v as IRVREG ptr ) as string
declare function hdumpProcHeader(byval proc as FBSYMBOL ptr) as string
#endif
