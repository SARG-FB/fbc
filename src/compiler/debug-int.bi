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
declare sub astDtorListDump( )


declare sub astDumpTree _
	( _
		byval n as ASTNODE ptr, _
		byval col as integer = 0 _
	)

declare sub astDumpList _
	( _
		byval n as ASTNODE ptr, _
		byval col as integer = 0 _
	)

declare function astDumpInline( byval n as ASTNODE ptr ) as string
declare sub astDumpSmall( byval n as ASTNODE ptr, byref prefix as string = "" )

declare sub dbg_astOutput _
	( _
		byref s as string, _
		byval col as integer, _
		byval just as integer, _
		byval depth as integer = -1 _
	)

declare function hHasDtor( byval sym as FBSYMBOL ptr ) as integer
declare function hAstNodeClassToStr _
	( _
		byval c as AST_NODECLASS _
	) as string
	
declare function hAstNodeToStr _
	( _
		byval n as ASTNODE ptr _
	) as string
	
#endif

